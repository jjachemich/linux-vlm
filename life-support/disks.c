/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Disk Life Support */

#include "std.h"

#include <fcntl.h>
#include <sys/stat.h>
#ifdef OS_OSF
#include <sys/mode.h>
#endif
#include <sys/uio.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "ivoryrep.h"
#include "memory.h"

#ifndef S_DEFFILEMODE
#define S_DEFFILEMODE	(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)
#endif

#define	DiskPageSize	8192


#define SetHostState(dc,p) \
  { \
	dc->hostState0 = (((uint64_t) p) >> 32) & 0xFFFFFFFFL; \
	dc->hostState1 = ((uint64_t) p) & 0xFFFFFFFFL; \
  }

#define HostState(dc) \
  (DiskChannelState*)(((uint64_t)dc->hostState0 << 32) | dc->hostState1)


/* Attach a disk partition (nee, file) to the disk channel supplied by Lisp and add said
   channel to the list of active embedded channels.  Lisp will have already setup the
   disk channel's unit number, read-only flag, and FIFO queues */

void AttachDiskChannel (AttachDiskChannelRequest* pRequest)
{
  register AttachDiskChannelRequest *request = pRequest;
  register EmbDiskChannel *diskChannel = (EmbDiskChannel*) HostPointer (request->diskChannel);
  register DiskChannelState *diskState;
  struct stat fileStatus;
  LispObj filenameHeader;
  size_t filenameSize;
  char *filename;
  int openFlags;
  int lblocksize = 2048 ;

	request->result = ESUCCESS;					/* Presume success */
	request->errorMsg = NullEmbPtr;				/* Can't return error messages (yet) */

	diskState = (DiskChannelState*) malloc (sizeof (DiskChannelState));
	if (NULL == diskState)
	  {
		verror ("AttachDiskChannel", "Couldn't allocate disk channel status structure");
		request->result = ENOMEM;
		return;
	  }
	SetHostState (diskChannel, diskState);

	diskState->fd = -1;		/* Needed before linking into channel list */
	diskState->command_queue_ptr = (EmbQueue*) HostPointer (diskChannel->command_queue);
	diskState->status_queue_ptr = (EmbQueue*) HostPointer (diskChannel->status_queue);
	diskState->error_pending = FALSE;
	if ((request->blockSize == 0) ||
	    (request->blockSize > 4096) ||
	    (request->blockSize < 0))
		diskChannel->blocksize = lblocksize * 4 ;
	else
		diskChannel->blocksize = request->blockSize * 4 ;
	diskState->blocksize = diskChannel->blocksize ;

	if (Type_String != *MapVirtualAddressTag ((Integer) 
						  ((Integer*)&request->filename - MapVirtualAddressData (0))))
	  {
		verror ("AttachDiskChannel", "Disk partition filename is not a simple string");
		request->result = EINVAL;
		return;
	  }

	filenameHeader = VirtualMemoryRead (request->filename);
	if (Type_HeaderI != (LispObjTag (filenameHeader) & 0x3F))
	  {
		verror ("AttachDiskChannel", "Disk partition filename is not a simple string");
		request->result = EINVAL;
		return;
	  }


	if ((LispObjData (filenameHeader) & ~Array_LengthMask) != 0x50000000L)
	  {
		verror ("AttachDiskChannel", "Disk partition filename is not a simple string");
		request->result = EINVAL;
		return;
	  }

	filenameSize = LispObjData (filenameHeader) & Array_LengthMask;
	filename = (char*) malloc (filenameSize + 1);
	if (NULL == filename)
	  {
		verror ("AttachDiskChannel", 
			    "Couldn't allocate space for local copy of disk partition filename");
		request->result = ENOMEM;
		return;
	  }

	memcpy (filename, MapVirtualAddressData (request->filename + 1), filenameSize);
	filename[filenameSize] = 0;
	diskState->filename = filename ;

	if (diskChannel->flags.read_only)
		openFlags = O_RDONLY;
	else
		openFlags = O_RDWR;

	if (CreateIfNotFound == request->ifNotFoundAction)
		openFlags |= O_CREAT;

	printf("AttachDiskChannel open '%s' with blocks of %u bytes (%d words)\n",
	       filename,
	       diskChannel->blocksize,
	       diskChannel->blocksize / 4 );
	diskState->fd = open (filename, openFlags, S_DEFFILEMODE);
	if (-1 == diskState->fd)
	  {
		verror ("AttachDiskChannel", "Unable to open disk partition %s", filename);
		request->result = errno;
		return;
	  }

	if (fstat (diskState->fd, &fileStatus))
	  {
		verror ("AttachDiskChannel", "Unable to determine size of disk partition %s", 
			    filename);
		request->result = errno;
		close (diskState->fd);
		return;
	  }

	if (request->minimumLength > 0)
		if (request->minimumLength > fileStatus.st_size)
		  {
			if (ftruncate (diskState->fd, (off_t) request->minimumLength))
			  {
				verror ("AttachDiskChannel",
					    "Unable to set size of disk partition %s to %d bytes", 
						filename, request->minimumLength);
				request->result = errno;
				close (diskState->fd);
				return;
			  }
			fileStatus.st_size = request->minimumLength;
		  }

	diskChannel->number_of_pages = fileStatus.st_size / diskChannel->blocksize ;

	diskChannel->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
	EmbCommAreaPtr->channel_table = GuestPointer (diskChannel);

	diskState->command_queue_ptr->signal = InstallSignalHandler ((ProcPtrV) &DiskLife, 
																 (PtrV) diskChannel, FALSE);

	return;
}


/* Grow the file (nee, disk partition) attached to the given disk channel so that it's,
   at least the requested number of bytes in length */

void GrowDiskPartition (GrowDiskPartitionRequest* pRequest)
{
  register GrowDiskPartitionRequest *request = pRequest;
  register EmbDiskChannel *diskChannel = (EmbDiskChannel*) HostPointer (request->diskChannel);
  register DiskChannelState *diskState = HostState (diskChannel);
  struct stat fileStatus;

	request->result = ESUCCESS;					/* Presume success */
	request->errorMsg = NullEmbPtr;				/* Can't return error messages (yet) */

	if (-1 == diskState->fd)
	  {
		verror ("GrowDiskPartition", "There is no disk partition attached to channel #%d",
			    diskChannel->unit);
		request->result = EINVAL;
		return;

	  }
	if (fstat (diskState->fd, &fileStatus))
	  {
		verror ("GrowDiskPartition", 
			    "Unable to determine size of disk partition attached to channel #%d",
			    diskChannel->unit);
		request->result = errno;
		return;
	  }

	if (request->newLength > fileStatus.st_size)
	  {
		if (ftruncate (diskState->fd, (off_t) request->newLength))
		  {
			verror ("GrowDiskPartition",
					"Unable to set size of disk partition attached to channel #%d to %d bytes",
					diskChannel->unit, request->newLength);
			request->result = errno;
			return;
		  }
		fileStatus.st_size = request->newLength;
	  }

	diskChannel->number_of_pages = fileStatus.st_size / diskChannel->blocksize ;

	return;
}


/* Detach the given disk channel from its disk partition (nee, file) and remove it from
   the linked list of active embedded channels */

void DetachDiskChannel (EmbPtr diskChannelPtr)
{
  register EmbDiskChannel *diskChannel = (EmbDiskChannel*) HostPointer (diskChannelPtr);
  register DiskChannelState *diskState = HostState (diskChannel);
  register EmbPtr channelPtr;
  register EmbPtr prevChannelPtr;


	RemoveSignalHandler (diskState->command_queue_ptr->signal);
	diskState->command_queue_ptr->signal = -1;

	if (diskState->fd != -1)
	  {
		  printf("DetachDiskChannel close '%s' (%d)\n", diskState->filename, diskChannel->unit);
		close (diskState->fd);
		diskState->fd = -1;
	  }

	free(diskState->filename);
	prevChannelPtr = NullEmbPtr;
    channelPtr = EmbCommAreaPtr->channel_table;

	while (channelPtr != NullEmbPtr)
	  {
		if (diskChannelPtr == channelPtr)
		  {
			if (NullEmbPtr == prevChannelPtr)
				EmbCommAreaPtr->channel_table = diskChannel->next;
			else
				((EmbChannel*)HostPointer(prevChannelPtr))->next = diskChannel->next;
			break;
		  }
		prevChannelPtr = channelPtr;
		channelPtr = ((EmbChannel*)HostPointer(channelPtr))->next;
	  }
}


/* The actual guts of disk life support -- Process the individual read/write requests */

void DiskLife (EmbDiskChannel* diskChannel)
{
  DiskChannelState* diskState = HostState (diskChannel);
  register EmbQueue* commandQueue = diskState->command_queue_ptr;
  register EmbQueue* statusQueue = diskState->status_queue_ptr;
  EmbDiskQueueElement* command;
  EmbWord commandPtr;

	while (EmbQueueFilled (commandQueue))
	  {
		if (EmbCommAreaPtr->inhibitDisk || (0 == EmbQueueSpace	(statusQueue)))
		  {
			/* Can't do I/O now -- Ask to be invoked again on the next "clock tick" */
			SignalLater (commandQueue->signal);
			return;
		  }

		commandPtr = EmbQueueTakeWord (commandQueue);
		if (commandPtr)
		  {
			command = (EmbDiskQueueElement*) HostPointer (commandPtr);

			switch (command->op.cmd)
			{
			  case WriteCmd:
				/* Write one or more pages to disk */
				if (diskChannel->flags.read_only)
				  {
					command->status = LostStatus;
					command->error_code = EROFS;
					break;
				  }
				/* Fall through to the ReadCmd case */

			  case ReadCmd:
				/* Read one or more pages from disk -- The WriteCmd case shares this code */
				if (diskState->error_pending)
					command->status = AbortStatus;
				else if (-1 == diskState->fd)
				  {
					command->status = LostStatus;
					command->error_code = ENXIO;
				  }
				else
				  {
					command->error_code = DoDiskIO (diskChannel, diskState, command);
					if (command->error_code)
					  {
						command->status = LostStatus;
						diskState->error_pending = TRUE;		/* Flush until reset */
					  }
					else
						command->status = WonStatus;
				  }
				break;

			  case ResetCmd:
				/* Reset the channel after an error */
				diskState->error_pending = FALSE;
				command->status = WonStatus;
				break;

			  case InitializeCmd:
				/* Initialize the channel -- Would reset meters if we had any */
				diskState->error_pending = FALSE;
				command->status = WonStatus;
				break;

			  default:
				command->status = LostStatus;
				command->error_code = ENXIO;
			}

			EmbQueuePutWord (statusQueue, commandPtr);
		  }
	  }
}


/* Perform a single I/O operation -- Splits the command into ... */

int DoDiskIO (EmbDiskChannel* diskChannel, DiskChannelState* diskState,
					 EmbDiskQueueElement* command)
{
  EmbAddressPair *addressPair;
  ssize_t nBytes, actualBytes;
  off_t startingOffset;
  int nAddresses, nVectors, i;
  ssize_t btoread, obtoread ;

	if ((command->page < 0) || (command->page + command->count > diskChannel->number_of_pages))
		return (EINVAL);

	startingOffset = (off_t) command->page * diskChannel->blocksize;
	if (-1 == lseek (diskState->fd, startingOffset, SEEK_SET))
		return (errno);

	nAddresses = command->n_addresses;
	addressPair = &command->addresses[0];
	obtoread = 0;

	while (nAddresses > 0)
	  {
		nVectors = (nAddresses > NIOVectors) ? NIOVectors : nAddresses;
		nBytes = 0;

		for (i = 0; i < nVectors; i++, addressPair++, nAddresses--)
		  {
			diskState->iovs[i].iov_base = (caddr_t) HostPointer (addressPair->address);
			btoread = addressPair->n_words * sizeof (EmbWord);
			obtoread = obtoread + btoread ;
//			btoread = (btoread * diskChannel->blocksize) / 8192 ;
			diskState->iovs[i].iov_len = btoread ;
			nBytes += diskState->iovs[i].iov_len;
		  }

		switch (command->op.cmd)
		{
		  case ReadCmd:
			actualBytes = readv (diskState->fd, diskState->iovs, nVectors);
#ifdef DEBUG_DISK 
			printf ( "disk%d: read (%zu) %zu bytes (%zu blocks) at offset %zu to (%p)\n",
				 diskChannel->unit,
				 obtoread,
				 actualBytes,
				 (actualBytes / diskState->blocksize),
				 startingOffset,
				 diskState->iovs[0].iov_base );
#endif
			break;

		  case WriteCmd:
			actualBytes = writev (diskState->fd, diskState->iovs, nVectors);
#ifdef DEBUG_DISK
			printf ( "disk%d: written (%zu) %zu bytes (%zu blocks) at offset %zu\n",
				 diskChannel->unit,
				 nBytes, 
				 actualBytes,
				 (actualBytes / diskState->blocksize),
				 startingOffset );
#endif
			break;

		  default:								/* Shouldn't get here ... */
			return (EINVAL);
		}

		if (-1 == actualBytes)
			return (errno);

		else if (actualBytes != nBytes)
			return (EINTR);
	  }
	
	return (0);
}


/* Reset a disk channel */

void ResetDiskChannel (EmbChannel* channel)
{
  register EmbDiskChannel* diskChannel = (EmbDiskChannel*) channel;
  register DiskChannelState* diskState = HostState (diskChannel);

	ResetIncomingQueue (diskState->command_queue_ptr);
	ResetOutgoingQueue (diskState->status_queue_ptr);
	diskState->error_pending = FALSE;

	if (GuestPointer (diskChannel) > EmbCommAreaPtr->host_buffer_start +
									 EmbCommAreaPtr->host_buffer_size)
	  {
		/* If Lisp created the disk channel, we must close the attached file now before 
		   Lisp discards the disk channel as Lisp won't first ask to detach it */
		if (diskState->fd != -1)
		  {
			close (diskState->fd);
			diskState->fd = -1;
		  }
	  }
}


/* Cleanup a single disk channel --
      The thread which runs this channel has already been killed by RemoveAllSignalHandlers */

void TerminateDiskChannel (EmbDiskChannel* diskChannel)
{
  register DiskChannelState* diskState = HostState (diskChannel);

	if (diskState->fd != -1)
	  {
		close (diskState->fd);
		diskState->fd = -1;
	  }
}


/* Cleanup the disk channels */

void TerminateDiskChannels ()
{
  EmbDiskChannel* diskChannel;
  EmbPtr channel;

	for (channel = EmbCommAreaPtr->channel_table; channel != NullEmbPtr;
		 channel = diskChannel->next)
	  {
		diskChannel = (EmbDiskChannel*) HostPointer (channel);
		if (EmbDiskChannelType == diskChannel->type)
			TerminateDiskChannel (diskChannel);
	  }
}
