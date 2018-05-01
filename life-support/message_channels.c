/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Message Channel Support */

#include "std.h"

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "spy.h"


#define SetSubtypeData(mc,p) \
  { \
	mc->subtypeData0 = (((uint64_t) p) >> 32) & 0xFFFFFFFFL; \
	mc->subtypeData1 = ((uint64_t) p) & 0xFFFFFFFFL; \
  }

#define SubtypeData(mc) \
  (PtrV)(((uint64_t)mc->subtypeData0 << 32) | mc->subtypeData1)


/* Initialize the command message channel and all other message channel structures */

void InitializeMessageChannels (VLMConfig* config)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbMessageChannel));
  register EmbMessageChannel *p = (EmbMessageChannel*) HostPointer (cp);
  register EmbCommandChannel *cc;

	p->type = EmbMessageChannelType;
	p->unit = 0;
	EmbCommAreaPtr->command_channel = cp;
	p->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
	EmbCommAreaPtr->channel_table = cp;

	p->subtype = EmbMessageChannelCommandSubtype;
	p->hostToGuestQueue = NullEmbPtr;			/* Should never be used ... */
	p->hostToGuestSupplyQueue = NullEmbPtr;		/* ... */

	cc = malloc (sizeof (EmbCommandChannel));
	if (NULL == cc) vpunt (NULL, "Couldn't allocate master command message channel");
	SetSubtypeData (p, cc);

	cc->header.nextActiveChannel = NULL;
	cc->header.commArea = EmbCommAreaPtr;
	cc->header.messageChannel = p;

	p->guestToHostQueue = CreateQueue (CommandQueueSize, sizeof (EmbPtr));
	cc->guestToHostQueue = (EmbQueue*) HostPointer (p->guestToHostQueue);
	cc->guestToHostQueue->signal = InstallSignalHandler ((ProcPtrV) &ExecuteGuestCommands, 
														 (PtrV) cc, FALSE);
	
	p->guestToHostReturnQueue = CreateQueue (CommandQueueSize, sizeof (EmbPtr));
	cc->guestToHostReturnQueue = (EmbQueue*) HostPointer (p->guestToHostReturnQueue);
}


/* Poll all active message channels -- Our caller is holding Life Support's signal lock */

void PollMessageChannels ()
{
  register EmbMessageChannel* messageChannel;
  register EmbMessageSubtypeData* subtypeData;
  register EmbQueue* guestToHostQueue;

	if (NullEmbPtr == EmbCommAreaPtr->command_channel)
		return;

	for (messageChannel = (EmbMessageChannel*) HostPointer (EmbCommAreaPtr->command_channel);
		 messageChannel != NULL;
		 messageChannel = subtypeData->header.nextActiveChannel)
	  {
		subtypeData = (EmbMessageSubtypeData*) SubtypeData (messageChannel);
		guestToHostQueue = (EmbQueue*) HostPointer (messageChannel->guestToHostQueue);
		if (messageChannel->guestToHostImpulse && (guestToHostQueue->signal != -1))
			EmbCommAreaPtr->guest_to_host_signals |= 1 << guestToHostQueue->signal;
	  }
}


/* Threads a newly activated message channel into the list of active channels */

void ThreadActiveMessageChannel (EmbMessageChannel* theChannel)
{
  register EmbMessageChannel* messageChannel;
  register EmbMessageSubtypeData* subtypeData;

	((EmbMessageSubtypeData*) SubtypeData (theChannel))->header.nextActiveChannel = NULL;

	for (messageChannel = (EmbMessageChannel*) HostPointer (EmbCommAreaPtr->command_channel);
		 messageChannel != NULL;
		 messageChannel = subtypeData->header.nextActiveChannel)
	  {
		subtypeData = (EmbMessageSubtypeData*) SubtypeData (messageChannel);
		if (NULL == subtypeData->header.nextActiveChannel)
		  {
			subtypeData->header.nextActiveChannel = theChannel;
			return;
		  }
	  }
}


/* Remove an existing message channel from the list of active channels */

void UnthreadMessageChannel (EmbMessageChannel* theChannel)
{
  register EmbMessageChannel* messageChannel;
  register EmbMessageSubtypeData* subtypeData;
  EmbMessageSubtypeData* theSubtypeData;

	for (messageChannel = (EmbMessageChannel*) HostPointer (EmbCommAreaPtr->command_channel);
		 messageChannel != NULL;
		 messageChannel = subtypeData->header.nextActiveChannel)
	  {
		subtypeData = (EmbMessageSubtypeData*) SubtypeData (messageChannel);
		if (theChannel == subtypeData->header.nextActiveChannel)
		  {
			theSubtypeData = (EmbMessageSubtypeData*) SubtypeData (theChannel);
			subtypeData->header.nextActiveChannel = theSubtypeData->header.nextActiveChannel;
			theSubtypeData->header.nextActiveChannel = NULL;
			return;
		  }
	  }
}


/* Execute incoming commands from the VLM */

void ExecuteGuestCommands (EmbCommandChannel *commandChannel)
{
  register EmbQueue *commandQueue = commandChannel->guestToHostQueue;
  register EmbQueue *resultsQueue = commandChannel->guestToHostReturnQueue;
  EmbPtr commandPtr;
  register EmbCommandBuffer *command;
  EmbCommandStartMBINBuffer *startMBINCommand;
  EmbMessageChannel *mbinChannel;
  EmbMBINChannel *mbinSubChannel;

	while (EmbQueueFilled (commandQueue))
	  {
		if (0 == EmbQueueSpace (resultsQueue))
		  {
			SignalLater (commandQueue->signal);
			return;
		  }

		commandPtr = EmbQueueTakeWord (commandQueue);
		if (commandPtr)
		  {
			command = (EmbCommandBuffer*) HostPointer (commandPtr);

			switch (command->header.opcode)
			{
			  case EmbCommandBufferStartMBIN:
				/* Activate an MBIN channel that was created by the VLM */
				startMBINCommand = (EmbCommandStartMBINBuffer*) command;
				mbinChannel = (EmbMessageChannel*) HostPointer (startMBINCommand->mbinChannel);
				mbinSubChannel = malloc (sizeof (EmbMBINChannel));
				if (NULL == mbinSubChannel)
					command->resultCode = ENOMEM;
				else
				  {
					mbinSubChannel->header.commArea = EmbCommAreaPtr;
					mbinSubChannel->header.messageChannel = mbinChannel;
					mbinSubChannel->guestToHostQueue
					  = (EmbQueue*) HostPointer (mbinChannel->guestToHostQueue);
					mbinSubChannel->guestToHostReturnQueue
					  = (EmbQueue*) HostPointer (mbinChannel->guestToHostReturnQueue);
					mbinSubChannel->hostToGuestQueue
					  = (EmbQueue*) HostPointer (mbinChannel->hostToGuestQueue);
					mbinSubChannel->hostToGuestSupplyQueue
					  = (EmbQueue*) HostPointer (mbinChannel->hostToGuestSupplyQueue);
					SetSubtypeData (mbinChannel, mbinSubChannel);
					ThreadActiveMessageChannel ((EmbMessageChannel*) mbinChannel);
					activeMBINChannel = mbinSubChannel;
					mbinSubChannel->guestToHostQueue->signal
					  = InstallSignalHandler((ProcPtrV)&SendMBINBuffers, (PtrV)mbinSubChannel,
											 FALSE);
					command->resultCode = ESUCCESS;
				  }
				break;

			  default:
				/* Unrecognized opcode */
				command->resultCode = EINVAL;
			}

			EmbQueuePutWord (resultsQueue, commandPtr);
		  }
	  }
}


/* Reset a message channel */

void ResetMessageChannel (EmbChannel* channel)
{
  register EmbMessageChannel* messageChannel = (EmbMessageChannel*) channel;
  register EmbMessageSubtypeData* subtypeData;
  boolean allocatedByVLM;

	allocatedByVLM = GuestPointer (channel) > EmbCommAreaPtr->host_buffer_start + EmbCommAreaPtr->host_buffer_size;

	messageChannel->guestToHostImpulse = EmbMessageImpulseNone;
	messageChannel->hostToGuestImpulse = EmbMessageImpulseNone;
	subtypeData = (EmbMessageSubtypeData*) SubtypeData (messageChannel);
	subtypeData->header.nextActiveChannel = NULL;

	switch (messageChannel->subtype)
	{
	  case EmbMessageChannelCommandSubtype:
		ResetIncomingQueue ((EmbQueue*) HostPointer (messageChannel->guestToHostQueue));
		ResetOutgoingQueue ((EmbQueue*) HostPointer (messageChannel->guestToHostReturnQueue));
		break;

	  case EmbMessageChannelMBINSubtype:
		ResetIncomingQueue ((EmbQueue*) HostPointer (messageChannel->guestToHostQueue));
		ResetOutgoingQueue ((EmbQueue*) HostPointer (messageChannel->guestToHostReturnQueue));
		ResetIncomingQueue ((EmbQueue*) HostPointer (messageChannel->hostToGuestSupplyQueue));
		ResetOutgoingQueue ((EmbQueue*) HostPointer (messageChannel->hostToGuestQueue));
		if (allocatedByVLM && (activeMBINChannel == (EmbMBINChannel*) subtypeData))
			activeMBINChannel = NULL;
		break;

	  default:
		break;
	}

	if (allocatedByVLM && subtypeData)
	  {
		free (subtypeData);
		SetSubtypeData (messageChannel, NULL);
	  }
}


/* Cleanup the message channels */

void TerminateMessageChannels ()
{
	/* Command and MBIN message channels don't have any state subject to clean up other than
	   their signal handlers which have already been addressed by TerminateSignalHandlers */
}
