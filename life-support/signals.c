/* -*- Mode: C; Tab-Width: 4 -*- */

/* Functions which handle signals between the VLM and host system */

#include "std.h"

#include "life_types.h"
#include "embed.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "ivoryrep.h"


/* Initialize the data structures used for signal handling */

void InitializeSignalHandlers ()
{
  int	i;

	EmbCommAreaPtr->guest_to_host_signals = 0;
	EmbCommAreaPtr->live_guest_to_host_signals = 0;
	EmbCommAreaPtr->host_to_guest_signals = 0;
	EmbCommAreaPtr->live_host_to_guest_signals = 0;
	EmbCommAreaPtr->reawaken = 0;
	EmbCommAreaPtr->useSignalLocks = FALSE;

	for (i = 0; i < NSignals; i++)
	  {
		EmbCommAreaPtr->signalHandler[i].handlerThreadSetup = FALSE;
		EmbCommAreaPtr->signalHandler[i].signal = 0;
		EmbCommAreaPtr->signalHandler[i].handlerFunction = NULL;
		EmbCommAreaPtr->signalHandler[i].handlerArgument = NULL;
	  }
}


/* Install a signal handler -- Handlers are implemented as threads */

SignalNumber InstallSignalHandler (ProcPtrV signalHandler, PtrV signalArgument, bool inputP)
{
	int	 i;
	SignalMask signal;

	if (EmbCommAreaPtr->useSignalLocks)
		if (pthread_mutex_lock (&EmbCommAreaPtr->signalLock))
			vpunt (NULL, "Unable to lock the Life Support signal lock in thread %lx",
				   pthread_self ());

	for (i = 0; i < NSignals; i++)
	  {
		signal = 1 << i;
		if ((EmbCommAreaPtr->live_guest_to_host_signals & signal) == 0)
		  {
			EmbCommAreaPtr->live_guest_to_host_signals |= signal;
			EmbCommAreaPtr->signalHandler[i].signal = signal;
			EmbCommAreaPtr->signalHandler[i].handlerFunction = signalHandler;
			EmbCommAreaPtr->signalHandler[i].handlerArgument = signalArgument;
			if (EmbCommAreaPtr->signalHandler[i].handlerThreadSetup)
			  {
#ifdef USING_REALTIME_KERNEL
				  int policy, priority;

				  policy = pthread_attr_getsched ((inputP) ? EmbCommAreaPtr->inputThreadAttrs
								  : EmbCommAreaPtr->outputThreadAttrs);
				  priority = pthread_attr_getprio ((inputP) ? EmbCommAreaPtr->inputThreadAttrs
								   : EmbCommAreaPtr->outputThreadAttrs);
				  if (pthread_setscheduler (EmbCommAreaPtr->signalHandler[i].handlerThread,
							    policy, priority))
					  vpunt (NULL,
						 "Unable to set scheduler policy/priority of thread %lx to %d/%d",
						 EmbCommAreaPtr->signalHandler[i].handlerThread, policy, priority);
#endif
			  }
			else
			{
				if (pthread_create (&EmbCommAreaPtr->signalHandler[i].handlerThread,
						    (inputP) ? &EmbCommAreaPtr->inputThreadAttrs
						    : &EmbCommAreaPtr->outputThreadAttrs,
						    (pthread_startroutine_t)&SignalHandlerTopLevel,
						    &EmbCommAreaPtr->signalHandler[i]))
					vpunt (NULL, "Unable to create thread to handle signal %d for %lx (%lx)",
					       i, signalHandler, signalArgument);
				EmbCommAreaPtr->signalHandler[i].handlerThreadSetup = TRUE;
			}
			break;
		  }
	  }
	
	if (EmbCommAreaPtr->useSignalLocks)
		if (pthread_mutex_unlock (&EmbCommAreaPtr->signalLock))
			vpunt (NULL, "Unable to unlock the Life Support signal lock in thread %lx",
				   pthread_self ());

	if (i < NSignals)
		return (i);
	else
		vpunt (NULL, "Signal table overflow");
	return 0 ;
}


/* Called by the emulator to inform us that the VLM has sent us an interrupt */

void SendInterruptToLifeSupport ()
{
	if (pthread_cond_broadcast (&EmbCommAreaPtr->signalSignal))
		vpunt (NULL, "Unable to send Life Support an interrupt from the VLM");
}


/* Called by the emulator to wait until life support detects an event which requires
   a response by the emulator.  This mechanism is used in Genera's idle process to
   reduce the amount of host CPU time consumed by the VLM */

void WaitForLifeSupport ()
{
   struct timespec delta, abstime;
   int result;

   delta.tv_sec = 1;
   delta.tv_nsec = 0;

	/* The emulator recognizes interrupts from Life Support only after branch and jump
	   instructions (i.e., instructions which change the PC).  If we allowed the emulator
	   to wait for Life Support while there are pending signals, we could hang.
	   EXPLAIN FEP MODE ...
	   (Consider the case of the interval timer firing before the Idle process actually
	   waits for it.  The clock signal would be pending but might not actually interrupt
	   the emulator if we're in the straight-line code in the Idle process before it
	   waits for Life Support.) */

	if (EmbCommAreaPtr->host_to_guest_signals
		  && ((processor->control >> 30) & TrapMode_FEP) != TrapMode_FEP)
		SendInterruptToEmulator ();

    else
	  {
		pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_mutex_unlock,
							  (void*)&EmbCommAreaPtr->wakeupLock);
		if (pthread_mutex_lock (&EmbCommAreaPtr->wakeupLock))
			vpunt (NULL, "Unable to lock the VLM wakeup lock in thread %lx", pthread_self ());
	
		if (pthread_get_expiration_np(&delta, &abstime))
			vpunt (NULL, "Unable to get absolute time");

		/* 8.12 changed cond_wait to cond_timedwait because sometimes got here with signals */
		if ((result = pthread_cond_timedwait (&EmbCommAreaPtr->wakeupSignal,
						      &EmbCommAreaPtr->wakeupLock, &abstime)))
			if (!(result == ETIMEDOUT || result == EINTR))
				vpunt (NULL, "Unable to wait for a VLM wakeup signal in thread %lx",
					   pthread_self ());
	
		if (pthread_mutex_unlock (&EmbCommAreaPtr->wakeupLock))
			vpunt (NULL, "Unable to unlock the VLM wakeup lock in thread %lx",
				   pthread_self ());
		pthread_cleanup_pop (FALSE);

#if defined(ARCH_ALPHA)
		processor->previousrcpp = 0;				/* Force microsecond clock to be reset */
#endif
	  }
}


/* Send a signal to the VLM -- The emulator provides us with a function to send it an
   interrupt just as we provide it a function to send us an interrupt.  The emulator's
   function ensures that the interrupt is delivered to the VLM at an appropriate time */

void EmbSendSignal (SignalNumber signal)
{
  //hack
  if (signal == 0) {
    //printf("EmbSendSignal(0)\n"); 
  //  while (1);
  //  if (EmbCommAreaPtr->clock_interval > 0) return;
    //return;
 }
  
	if (pthread_mutex_lock (&EmbCommAreaPtr->wakeupLock))
		vpunt (NULL, "Unable to lock the VLM wakeup lock in thread %lx", pthread_self ());
	
	if ((signal > -1) && (signal < NSignals))
	  {
		EmbCommAreaPtr->host_to_guest_signals |= (1 << signal);
		SendInterruptToEmulator ();
	  }

	if (pthread_cond_broadcast (&EmbCommAreaPtr->wakeupSignal))
		vpunt (NULL, "Unable to wakeup the VLM from Life Support");

	if (pthread_mutex_unlock (&EmbCommAreaPtr->wakeupLock))
		vpunt (NULL, "Unable to unlock the VLM wakeup lock in thread %lx",
			   pthread_self ());
}


/* Called by a signal handler when it can't handle the signal now and wishes to try later --
   The polling thread checks for said handlers each "clock tick" */

void SignalLater (SignalNumber signal)
{
  pthread_t self = pthread_self ();

	if (pthread_mutex_lock (&EmbCommAreaPtr->signalLock))
		vpunt (NULL, "Unable to lock the Life Support signal lock in thread %lx", self);

	EmbCommAreaPtr->reawaken |= (SignalMask)(1 << signal);

	if (pthread_mutex_unlock (&EmbCommAreaPtr->signalLock))
		vpunt (NULL, "Unable to unlock the Life Support signal lock in thread %lx", self);
}


/* Installed when removing a signal handler to render its thread benign */

void NullSignalHandler (PtrV ignore)
{
}


/* Remove a signal handler, including its thread */

void RemoveSignalHandler (SignalNumber signal)
{
  SignalMask mask = 1 << signal;

	if ((signal < 0) || (signal >= NSignals))
		return;

	if (EmbCommAreaPtr->useSignalLocks)
		if (pthread_mutex_lock (&EmbCommAreaPtr->signalLock))
			vpunt (NULL, "Unable to lock the Life Support signal lock in thread %lx",
				   pthread_self ());

	EmbCommAreaPtr->live_guest_to_host_signals &= ~mask;
	EmbCommAreaPtr->reawaken &= ~mask;

	/* Leave a last interrupt pending so that the signal handler thread can notice
	   that it's been cancelled and clean up gracefully */
	EmbCommAreaPtr->guest_to_host_signals |= mask;

	if (EmbCommAreaPtr->signalHandler[signal].handlerThreadSetup)
	  {
		EmbCommAreaPtr->signalHandler[signal].handlerFunction = &NullSignalHandler;
		EmbCommAreaPtr->signalHandler[signal].handlerArgument = NULL;
	  }

	if (EmbCommAreaPtr->useSignalLocks)
		if (pthread_mutex_unlock (&EmbCommAreaPtr->signalLock))
			vpunt (NULL, "Unable to unlock the Life Support signal lock in thread %lx",
				   pthread_self ());
}


/* Remove all signal handlers, including their threads, on exit */

void TerminateSignalHandlers ()
{
  int	i;
  void* exit_value;

	for (i = 0; i < NSignals; i++)
		if (EmbCommAreaPtr->signalHandler[i].handlerThreadSetup)
		  {
			pthread_cancel (EmbCommAreaPtr->signalHandler[i].handlerThread);
			pthread_join (EmbCommAreaPtr->signalHandler[i].handlerThread, &exit_value);
			EmbCommAreaPtr->signalHandler[i].handlerThreadSetup = FALSE;
		  }
}


/* Top level function for a signal handler thread -- We wait for our signal to be present
   and call the real handler function */

void SignalHandlerTopLevel (pthread_addr_t argument)
{
  SignalHandler *signalHandler = (SignalHandler*) argument;
  pthread_t self = signalHandler->handlerThread;

	pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);

	begin_MUTEX_LOCKED (signalLock);

	while (TRUE)
	  {
		if (EmbCommAreaPtr->guest_to_host_signals & signalHandler->signal)
		  {
			EmbCommAreaPtr->guest_to_host_signals &= ~signalHandler->signal;
			pthread_testcancel ();
			if (pthread_mutex_unlock (&EmbCommAreaPtr->signalLock))
				vpunt (NULL, "Unable to unlock the Life Support signal lock in thread %lx",
					   self);
			(*(signalHandler->handlerFunction)) (signalHandler->handlerArgument);
			if (pthread_mutex_lock (&EmbCommAreaPtr->signalLock))
				vpunt (NULL, "Unable to lock the Life Support signal lock in thread %lx",
					   self);
		  }

		else if (pthread_cond_wait (&EmbCommAreaPtr->signalSignal,
									&EmbCommAreaPtr->signalLock))
			vpunt (NULL, "Unable to wait for the Life Support signal signal in thread %lx",
				   self);
	  }

	end_MUTEX_LOCKED (signalLock);

	pthread_cleanup_pop (TRUE);
}
