/* -*- Mode: C; Tab-Width: 4 -*- */

/* Function prototypes for all entrypoints in VLM Life Support */

#include <pthread.h>
#include <netinet/in.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "world_tools.h"


/* When executing code which may call a function that is a thread cancellation point
   (e.g., nanosleep, read, write) while under the protection of a mutex (i.e., lock),
   we must establish a cleanup handler that unlocks the mutex to prevent the possibility
   of a deadlock during application shutdown. */

#define begin_MUTEX_LOCKED(lock) \
	pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_mutex_unlock, \
						  (void*)&EmbCommAreaPtr->lock); \
	if (pthread_mutex_lock (&EmbCommAreaPtr->lock)) \
		vpunt (NULL, "Unable to lock the Life Support " #lock " in thread %lx",  \
			   pthread_self ());

#define end_MUTEX_LOCKED(lock) \
	if (pthread_mutex_unlock (&EmbCommAreaPtr->lock)) \
		vpunt (NULL, "Unable to unlock the Life Support " #lock " in thread %lx", \
			   pthread_self ()); \
	pthread_cleanup_pop (FALSE);

/* Life Support initialization holds onto the signal lock (mutex) until it's safe for the
   various threads which comprise Life Support to run free.  Consequently, each thread
   first locks and unlocks the signal lock to synchronize with Life Support initialization. */

#define WaitUntilInitializationComplete() \
	if (pthread_mutex_lock (&EmbCommAreaPtr->signalLock)) \
		vpunt (NULL, "Unable to lock the Life Support signal lock in thread %lx", \
			   pthread_self ()); \
	if (pthread_mutex_unlock (&EmbCommAreaPtr->signalLock)) \
		vpunt (NULL, "Unable to unlock the Life Support signal lock in thread %lx", \
			   pthread_self ());


/*** initialization.c ***/

EmbPtr	EmbCommAreaAlloc (size_t nBytes);
int	InitializeLifeSupport (VLMConfig* config);
EmbPtr	MakeEmbString (char* aString);
void	TerminateLifeSupport (void);

void		ParseVersionNumber (char* versionString, int* majorVersion, int* minorVersion, int* majorRevision, int* minorRevision);
void		SetupThreadAttrs (char* class, int priorityBoost, pthread_attr_t* threadAttrs,
								  bool* threadAttrsSetup);


/*** cold_load.c ***/

int	InitializeColdLoadChannel (VLMConfig* config);
void	ResetColdLoadChannel (EmbChannel* channel);
void	TerminateColdLoadChannel (void);
void	UpdateColdLoadNames (void);
int check_display(XParams *params,
		  NetworkInterface *nwi) ;
int check_keyboard(XParams *params,
		   boolean noWaiting);


/* Internal functions are prototyped in the source file */


/*** console.h ***/

boolean	ConsoleInputAvailableP (void);
void	DoConsoleIO (EmbConsoleChannel* consoleChannel, EmbConsoleBuffer* command);
void	InitializeConsoleChannel (VLMConfig* config);
void	ResetConsoleChannel (EmbChannel* channel);
void	TerminateConsoleChannel (void);

void		AdvanceOpeningState (EmbConsoleChannel* pConsoleChannel);
void		CloseDisplay (EmbConsoleChannel* chanel);
void		ConsoleDriver (EmbConsoleChannel* consoleChannel, EmbQueue* pRequestQueue, 
							   EmbQueue* pReplyQueue);
void		ConsoleInput (EmbConsoleChannel* consoleChannel);
int		ConsoleInputWait (EmbConsoleChannel* pConsoleChannel, 
								  EmbConsoleBuffer* pCommand);
void		ConsoleOutput (EmbConsoleChannel* consoleChannel);
int		ConsoleRead (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand);
int		ConsoleWrite (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand);
void		DisableRunLights (EmbConsoleChannel* consoleChannel);
void		DrawRunLights (pthread_addr_t argument);
void		EnableRunLights (EmbConsoleChannel* pConsoleChannel, 
								 EmbConsoleBuffer* pCommand);
int		OpenDisplay (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand);
int		ProcessConnectionRequest (EmbConsoleChannel* pConsoleChannel,
										  EmbConsoleBuffer* pCommand);


/*** disks.c ***/

void	AttachDiskChannel (AttachDiskChannelRequest* pRequest);
void	GrowDiskPartition (GrowDiskPartitionRequest* pRequest);
void	DetachDiskChannel (EmbPtr diskChannelPtr);
void	ResetDiskChannel (EmbChannel* channel);
void	TerminateDiskChannels (void);

int		DoDiskIO (EmbDiskChannel* diskChannel, DiskChannelState* diskState, 
						  EmbDiskQueueElement* command);
void		DiskLife (EmbDiskChannel* diskChannel);
void		TerminateDiskChannel (EmbDiskChannel* diskChannel);

/*** unixcrypt.c ***/

void UnixCrypt (UnixCryptRequest *pRequest);

/*** message_channels.c ***/

void	InitializeMessageChannels (VLMConfig* config);
void	PollMessageChannels (void);
void	ResetMessageChannel (EmbChannel* channel);
void	TerminateMessageChannels (void);
void	UnthreadMessageChannel (EmbMessageChannel* theChannel);

void		ExecuteGuestCommands (EmbCommandChannel* commandChannel);
void		ThreadActiveMessageChannel (EmbMessageChannel* theChannel);


/*** network.c ***/

void	InitializeNetworkChannels (VLMConfig* config);
void	ResetNetworkChannel (EmbChannel* channel);
void	TerminateNetworkChannels (void);

void		InitializeNetChannel (NetworkInterface* interface, int netUnit
#ifdef OS_OSF
				      , struct in_addr* localHostAddress
#else
#ifndef USE_LIBPCAP
				      , int ipSocket, struct ifconf* ifc
#endif
#endif
                                      );
void		NetworkChannel (pthread_addr_t argument);
void		NetworkChannelTransmitter (EmbNetChannel* pNetChannel);
#ifdef OS_OSF
void		TerminateNetChannel (EmbNetChannel* netChannel);
#else
void		TerminateNetChannel (EmbNetChannel* netChannel, int ipSocket);
#endif


/*** polling.c ***/

void	IntervalTimerDriver (pthread_addr_t argument);
void	IvoryLifePolling (pthread_addr_t argument);
void	SetIntervalTimer (Integer relativeTimeout);

void		ProcessResetRequest (void);
void		UpdateVLMStatus (void);
boolean	VLMIsRunning (EmbCommArea* ep);
boolean	VLMIsRunningLisp (EmbCommArea* ep);


/*** queues.c ***/

/* All other entrypoints are defined in embed.h */

EmbPtr	CreateQueue (int nElements, int elementSize);
void	ResetIncomingQueue (EmbQueue* q);
void	ResetOutgoingQueue (EmbQueue* q);


/*** signals.c ***/

/* InstallSignalHandler, EmbSendSignal, SignalLater, and RemoveSignalHandler
   are defined in embed.h */

void	InitializeSignalHandlers (void);
void	TerminateSignalHandlers (void);

void		NullSignalHandler (PtrV ignore);
void		SignalHandlerTopLevel (pthread_addr_t argument);

/* The prototypes for SendInterruptToLifeSupport and WaitForLifeSupport are in ivoryrep.h */
