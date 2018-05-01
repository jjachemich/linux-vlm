/* -*- Mode: C -*- */

/* Life Support periodic activities */ 

#include "std.h"

#include "life_types.h"
#include "embed.h"
#include "life_prototypes.h"
#include "utilities.h"

#define OneSecond        1000000000L
#define OneQuarterSecond  250000000L
#define OneEighthSecond   125000000L
//#define OneSixteenthSecond 62500000L
#define OneSixteenthSecond  7500000L
#define OneSixteethSecond  16666667L
//#define OneSixteethSecond   8333333L


/* Returns TRUE if the VLM is running either in the IFEP or Lisp */
boolean VLMIsRunning (EmbCommArea* ep)
{ 
	return ((ep->spy_status == 0) && (ep->fep.status != HaltedFEPStatus));
}

/* Returns TRUE if the VLM is running Lisp */
boolean VLMIsRunningLisp (EmbCommArea* ep)
{
	return (VLMIsRunning (ep) && (ep->fep.status == IdleFEPStatus));
}

/* Updates the VLM guest status to reflect its current true status */
void UpdateVLMStatus ()
{
  register EmbCommArea *ep = EmbCommAreaPtr;

	switch (ep->guestStatus)
	{
	  case NonexistentGuestStatus:
	  case BrokenGuestStatus:
		/* VLM stays missing or broken until someone fixes it */
		break;

	  case UninitializedGuestStatus:
	  case InitializingGuestStatus:
		ep->guestStatus =   VLMIsRunningLisp (ep) ? RunningGuestStatus
						  : VLMIsRunning (ep) ? InitializedGuestStatus
						  : ep->guestStatus;
		break;

	  case InitializedGuestStatus:
	  case StartedGuestStatus:
		/* If VLM's no longer running the IFEP, assume it's in the BootROM or DevicePROM */
		ep->guestStatus =   VLMIsRunningLisp (ep) ? RunningGuestStatus
						  : VLMIsRunning(ep) ? ep->guestStatus 
						  : InitializingGuestStatus;
		break;

	  case CrashedGuestStatus:
	  case RunningGuestStatus:
		ep->guestStatus =   VLMIsRunningLisp (ep) ? RunningGuestStatus
						  : VLMIsRunning(ep) ? CrashedGuestStatus
						  : InitializingGuestStatus;
		break;
	}

	UpdateColdLoadNames ();
}


/* Reset the communications area */

static void ResetCommArea (boolean fullReset)
{
  register EmbChannel* channel;
  register EmbPtr channelP;
	
	for (channelP = EmbCommAreaPtr->channel_table; channelP != NullEmbPtr; 
		 channelP = channel->next)
	  {
		channel = HostPointer (channelP);
		switch (channel->type)
		{ 
		  case EmbDiskChannelType:
			if (fullReset)
				ResetDiskChannel (channel);
			break;
		  case EmbConsoleChannelType:
			if (fullReset)
				ResetConsoleChannel (channel);
			break;
		  case EmbNetworkChannelType:
			if (fullReset)
				ResetNetworkChannel (channel);
			break;
#ifdef UNIMPLEMENTED
		  case EmbRPCChannelType:
			if (fullReset)
				ResetRPCChannel (channel);
			break;
#endif
#ifdef UNIMPLEMENTED
		  case EmbSCSIChannelType:
			ResetSCSIChannel (channel);
			break;
#endif
		  case EmbColdLoadChannelType:
			ResetColdLoadChannel (channel);
			break;
#ifdef UNIMPLEMENTED
		  case EmbHostFileChannelType:
			ResetHostFileChannel (channel);
			break;
#endif
		  case EmbMessageChannelType:
			if (fullReset)
				ResetMessageChannel (channel);
			break;
		}
	  }

	if (fullReset)
	  {
		/* Reset host buffer memory when requested by Lisp */
	  }
}


/* Process a reset request from the VLM */

void ProcessResetRequest ()
{
	switch (EmbCommAreaPtr->reset_request)
	{
	  case ReadNVRAMResetRequest:
		/* Read permanent settings maintained by the host for the VLM -- NYI */
		break;

	  case WriteNVRAMResetRequest:
		/* Write permanent settings maintained by the host for the VLM -- NYI */
		break;
	
	  case AreYouThereResetRequest:
		/* Ivory simply wants to know if it's embedded */
		break;
	
	  case FEPResetRequest:
		/* The IFEP has started running and needs a partial reset of the communications area */
		ResetCommArea (FALSE);
		break;
	
	  case LispResetRequest:
		/* Lisp has started running and needs a complete reset of the communications area */
		ResetCommArea (TRUE);
		EmbCommAreaPtr->resetRequestCount++;	/* A true reset */
		EmbCommAreaPtr->restart_applications = 1;	/* Restart applications on full reset */
		break;
	
	  default:
		/* Silently ignore any requests which are either not supported or not recognized */
		break;
	}

	EmbCommAreaPtr->reset_request = NoResetRequest;
}


/* Top level function of the polling thread -- Does all its work under protection of Life 
   Support's global signal lock as the main thrust of polling is to periodically wakeup 
   all signal handlers in case a "hardware interrupt" from the VLM was lost. */

void IvoryLifePolling (pthread_addr_t argument)
{
  pthread_t self = pthread_self ();
  struct timespec pollingSleep;

  pollingSleep.tv_sec = 0;
  pollingSleep.tv_nsec = 0;
  
  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);
  
  while (TRUE)
    {
      begin_MUTEX_LOCKED (signalLock);

      EmbCommAreaPtr->pollTime += pollingSleep.tv_nsec;
      
      PollMessageChannels ();
      
      if (EmbCommAreaPtr->reset_request != NoResetRequest)
	ProcessResetRequest ();
      
      else if (EmbCommAreaPtr->pollTime > OneQuarterSecond)
	//      if (EmbCommAreaPtr->pollTime > OneEighthSecond);   // OneQuarterSecond)
      {
	EmbCommAreaPtr->pollTime = 0;
	EmbCommAreaPtr->guest_to_host_signals |= EmbCommAreaPtr->live_guest_to_host_signals;
	if (pthread_cond_broadcast (&EmbCommAreaPtr->signalSignal))
	  vpunt (NULL, "Unable to send Life Support signal signal in thread %lx", self);
      }
      
      else if (EmbCommAreaPtr->reawaken)
	//      if (EmbCommAreaPtr->reawaken)
	{
	  EmbCommAreaPtr->guest_to_host_signals |= EmbCommAreaPtr->reawaken;
	  EmbCommAreaPtr->reawaken = 0;
	  if (pthread_cond_broadcast (&EmbCommAreaPtr->signalSignal))
	    vpunt (NULL, "Unable to send Life Support signal signal in thread %lx", self);
	}
      
      end_MUTEX_LOCKED (signalLock);
      
      if (EmbCommAreaPtr->clock_interval > 0)
	{
	  EmbCommAreaPtr->pollClockTime -= pollingSleep.tv_nsec;
	  if (EmbCommAreaPtr->pollClockTime <= 0)
	    {
	      EmbSendSignal (EmbCommAreaPtr->clock_signal);
	      EmbCommAreaPtr->pollClockTime = 1000 * EmbCommAreaPtr->clock_interval;
	    }
	  if (EmbCommAreaPtr->pollClockTime > OneQuarterSecond)
	    pollingSleep.tv_nsec = OneQuarterSecond;
	  else
	    pollingSleep.tv_nsec = EmbCommAreaPtr->pollClockTime;
	}
      
      else
	pollingSleep.tv_nsec = OneSixteenthSecond;
      
      UpdateVLMStatus ();
      
      if (0) {
	printf("sleep; interval %d, time %ld, %ld\n",
	       EmbCommAreaPtr->clock_interval,
	       pollingSleep.tv_sec, pollingSleep.tv_nsec);
      }
      
      pollingSleep.tv_sec = 0;
      pollingSleep.tv_nsec = OneEighthSecond;
      
      if (pthread_delay_np (&pollingSleep))
	vpunt (NULL, "Unable to sleep in thread %lx", self);
    }
  
  pthread_cleanup_pop (TRUE);
}


/* Top level function of the interval timer thread -- Sleeps for the interval requested
   by the emulator thread and then sends a clock interrupt.  As the emulator (nee, Lisp) 
   will frequently need to reprogram the timer while we're waiting, we actually wait for 
   a signal with a timeout.  The emulator raises the signal to indicate that it's 
   reprogramming the interval timer.  Should the wait timeout, we send the clock interrupt. */

void IntervalTimerDriver (pthread_addr_t argument)
{
  pthread_t self = pthread_self ();
  struct timespec expirationTime, expirationInterval /*, newExpirationTime */ ;
  //  struct timeval tmv ; struct timezone tz ;
  int result;

  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);
  
  WaitUntilInitializationComplete ();
  
  EmbCommAreaPtr->clockTime = -1;
  
  begin_MUTEX_LOCKED (clockLock);
  
  while (TRUE)
    {
      if (EmbCommAreaPtr->clockTime >= 0)
	{
	  expirationInterval.tv_sec = 0;
	  expirationInterval.tv_nsec = 1000 * EmbCommAreaPtr->clockTime;
	  while (expirationInterval.tv_nsec >= OneSecond)
	    {
	      expirationInterval.tv_sec++;
	      expirationInterval.tv_nsec -= OneSecond;
	    }
	  if (pthread_get_expiration_np (&expirationInterval, &expirationTime) < 0)
	    vpunt (NULL, "Unable to compute interval timer expiration time");
	  result = pthread_cond_timedwait (&EmbCommAreaPtr->clockSignal, 
					   &EmbCommAreaPtr->clockLock,
					   &expirationTime);
	}
      
      else
	/* Wait indefinitely for someone to program the interval timer */
	result = pthread_cond_wait (&EmbCommAreaPtr->clockSignal, 
				    &EmbCommAreaPtr->clockLock);
      
      if (result == ETIMEDOUT)
	{
	  EmbSendSignal (EmbCommAreaPtr->clock_signal);
	  EmbCommAreaPtr->clockTime = -1;
	}
    }
  
  end_MUTEX_LOCKED (clockLock);
  
  pthread_cleanup_pop (TRUE);
}

/* Reprogram the interval timer -- Called from the emulator thread in response to writing a 
   value to the WaitForNextEvent coprocessor register.  We simply set the relative time for
   the next interrupt and signal the interval timer thread.  The interval timer thread will
   wakeup, note that there's a new timeout, and go back to sleep with the new timeout. */

void SetIntervalTimer (Integer relativeTimeout)
{
  begin_MUTEX_LOCKED (clockLock);

  EmbCommAreaPtr->clockTime = relativeTimeout;
  if (pthread_cond_broadcast (&EmbCommAreaPtr->clockSignal) < 0)
    vpunt (NULL, "Unable to send Life Support clock signal in thread %lx", pthread_self ());
  
  end_MUTEX_LOCKED (clockLock);
  
  pthread_yield ();
}
