/* -*- Mode: C; Tab-Width: 4 -*- */

/* This file defines the various common data structures, including the
   communication area, the channels, and the configuration file.
   For convenience, the private parts of these structures are in this
   file too, but they shouldn't be accessed outside of Ivory life support
   and they especially should not be depended upon by the guest.
 */

#ifndef _EMBED_
#define _EMBED_

#include <sys/types.h>
#include <pthread.h>
#include <sys/uio.h>
#include "pfilt_wrapper.h"

#include "life_types.h"


/*** Signal Handlers ***/

#define NSignals 32

typedef struct
  {
	pthread_t	handlerThread;		/* Thread which runs this signal handler */
	bool		handlerThreadSetup;	/* TRUE => the above thread has been created */
	SignalMask	signal;				/* Identifies the signal under its care */
	ProcPtrV	handlerFunction;	/* The function which actually handles the signal ... */
	PtrV		handlerArgument;	/* ... and its argument */
  }		SignalHandler;

void			EmbSendSignal (SignalNumber signal);
SignalNumber	InstallSignalHandler(ProcPtrV singalHandler, PtrV signalArgument, bool inputP);
void			RemoveSignalHandler (SignalNumber signal);
void			SignalLater (SignalNumber signal);


/*** Communication Area ***/

typedef struct
  {
	/* Overall Information */
	EmbWord	identifier;			/* char[4] 'EMBD' verifies this is the communication area */
	EmbWord	version;			/* 1 identifies the communication architecture version */
	EmbWord	system_type;		/* SystemTypeXXX identifies the host & guest systems jointly */

	EmbWord	number_of_slots;	/* Number of guest-visible 32-bit words in this structure */
  	EmbWord	comm_memory_size;	/* Total number of 32-bit words in communication memory */

	/* Version Numbers of various software entities, in case anyone cares */
	struct
	  {								/* Version number of this program ... */
#if BYTE_ORDER == LITTLE_ENDIAN
		EmbWord	minor :16;
		EmbWord	major :16;
#else
		EmbWord	major :16;
		EmbWord	minor :16;
#endif
	  }		generaVersion;
	struct
	  {								/* Version number of the OSF/1 operating system ... */
#if BYTE_ORDER == LITTLE_ENDIAN
		EmbWord	minorRevision :8;
		EmbWord	majorRevision :8;
		EmbWord	minorRelease :8;
		EmbWord	majorRelease :7;
		EmbWord	testReleaseP :1;
#else
		EmbWord	testReleaseP :1;
		EmbWord	majorRelease :7;
		EmbWord	minorRelease :8;
		EmbWord	majorRevision :8;
		EmbWord	minorRevision :8;
#endif
	  }		osfVersion;
	EmbWord	guest_major_version;
	EmbWord	guest_minor_version;
	EmbWord	fep_major_version;
	EmbWord	fep_minor_version;

	/* Memory Allocation */
	EmbPtr	guest_buffer_start;	/* Portion of communication memory allocated by guest */
	EmbWord	guest_buffer_size;	/* Number of 32-bit words in that */
	EmbPtr	host_buffer_start;	/* Portion of communication memory allocated by host */
	EmbWord	host_buffer_size;	/* Number of 32-bit words in that */
	EmbPtr	fep_buffer_start;	/* Portion of communication memory used by IFEP */
	EmbWord	fep_buffer_size;	/* Number of 32-bit words in that */

	/* Signals */
	SignalMask	guest_to_host_signals;		/* 1 if signal bit wants attention */
	SignalMask	live_guest_to_host_signals;	/* 1 if signal bit is in use */
	SignalMask	host_to_guest_signals;		/* 1 if signal bit wants attention */
	SignalMask	live_host_to_guest_signals;	/* 1 if signal bit is in use */

	/* Channels */
	EmbPtr	channel_table;		/* Head of threaded list of all channels */
	EmbPtr	consoleChannel;		/* Main console (X window) channel */
	EmbPtr	cold_load_channel;	/* Cold load stream channel */
	EmbPtr	command_channel;	/* Command message channel */

	/* Memory Configuration */
	EmbWord	virtualMemorySize;		/* Size of emulator virtual memory in words */
	EmbWord	worldImageSize;			/* Size of world load in words */
	EmbPtr	bad_memory_map;			/* MacIvory: Pointer to map of holes in Ivory memory */
	EmbWord	bad_memory_map_size;	/* MacIvory: Number of 64-bit entries in that map */

	/* The remaining guest visible slots aren't in their proper logical position in
	   this structure as they were added after the initial release */

	SignalNumber	clock_signal;	/* Send this signal to simulate a clock interrupt */
	EmbWord			clock_interval;	/* Approximate clock interrupt interval in microseconds */
	EmbWord	run_lights;				/* Right-justified bit mask of run lights */
	EmbWord	reset_request;			/* Set by guest to request reset, cleared by host */
	EmbWord	board_serial_number;	/* Guest board serial number */
	EmbWord	board_major_version;	/* Guest board major revision level */
	EmbWord	board_minor_version;	/* Guest board minor revision level */
	EmbWord	spy_command;			/* Debugging */
	EmbWord	spy_status;				/* Debugging */
	EmbWord	stop_request;			/* Set to force guest to halt */
	struct 
	  {
#if BYTE_ORDER == LITTLE_ENDIAN
		uEmbWord	status:8;			/* Guest status as determined by its FEP (see below) */
		uEmbWord	cursor:1;			/* FEP twiddles this bit to blink its cursor */
		uEmbWord	busy:1;				/* 1 => FEP is executing a command */
		uEmbWord	error:1;			/* 1 => last FEP command ended in an error */
		uEmbWord	lisp_is_loaded:1;	/* 1 => the FEP has determined that Lisp is loaded */
		uEmbWord	:20;
#else
		uEmbWord	:20;
		uEmbWord	lisp_is_loaded:1;	/* 1 => the FEP has determined that Lisp is loaded */
		uEmbWord	error:1;			/* 1 => last FEP command ended in an error */
		uEmbWord	busy:1;				/* 1 => FEP is executing a command */
		uEmbWord	cursor:1;			/* FEP twiddles this bit to blink its cursor */
		uEmbWord	status:8;			/* Guest status as determined by its FEP (see below) */
#endif
	  }		fep;
	EmbWord	restart_applications;	/* Set by guest to request host restart its applications */
	EmbWord	signal_interrupt_vector;	/* (Unused) Tells guest more precisely how to interrupt host */
	EmbWord	base_register;			/* MacIvory base register value */
	EmbWord	hostVersion2;			/* Unused */
	EmbWord hostVersion3;			/* Unused */
	struct
	  {
#if BYTE_ORDER == LITTLE_ENDIAN
		EmbWord	data:16;			/* Contents of NVRAM maintained by host for MacIvory */
		EmbWord :16;
#else
		EmbWord :16;
		EmbWord	data:16;			/* Contents of NVRAM maintained by host for MacIvory */
#endif
	  }		MacIvory_NVRAM_settings;
	EmbPtr	worldPathname;			/* Pathname of the world loaded by the VLM command */
	EmbPtr		unixLoginName;		/* Login name of user running the emulator ... */
	uEmbWord	unixUID;			/* ... User ID ... */
	uEmbWord	unixGID;			/* ... Group ID */
	EmbPtr          unixCwd;                /* current working directory */
	EmbPtr          UnixHomeDir;             /* User's home directory */

	/* Remainder of structure is Life Support globals, not known to the Ivory */

	/* Padding to allow for convenient expansion of shared portion later */
	EmbWord	pad0,					/* Special padding slot used by INIT code */
			pad1[13];

	/* To add slots to the shared portion of the communications area, add the slots before 
	   the pad0 field and decrease the size of the pad1 field by the number of slots added.
	   As long as spare slots remain, you won't have to recompile all of the host software;
	   just those files which you modify to use the new slots.

	   However, if you do use up all the remaining pad slots, instead of leaving no padding,
	   leave a minimum of 16 pad slots (one in pad0, 15 in pad1).  You must also recompile 
	   all of the host software. 

	   Be sure that any changes you make to the shared portion of the communications area
	   are reflected in the sources of all host systems (MacIvory, Solstice, VLM) as well 
	   as by appropriate patches in Lisp.
	*/

	EmbWord	guestStatus;			/* A GuestStatus code */

	pthread_attr_t	pollThreadAttrs;		/* Attributes used to create polling threads */
	bool			pollThreadAttrsSetup;	/* TRUE => Above attributes have been created */

	pthread_attr_t	outputThreadAttrs;		/* Attributes used to create output threads */
	bool			outputThreadAttrsSetup;	/* TRUE => Above attributes have been created */

	pthread_attr_t	inputThreadAttrs;		/* Attributes used to create input threads */
	bool			inputThreadAttrsSetup;	/* TRUE => Above attributes have been created */

	bool			useSignalLocks;				/* TRUE => Manipulate signals under the lock */
	SignalHandler	signalHandler[NSignals];	/* The guest-to-host signal handlers */
	SignalMask		reawaken;					/* Signals to try again on next clock tick */
	pthread_mutex_t	signalLock;					/* Used to control access to signals */
	bool			signalLockSetup;			/* TRUE => the above mutex has been created */
	pthread_cond_t	signalSignal;				/* Used to wakeup signal handlers */
	bool			signalSignalSetup;			/* TRUE => the above has been created */

	pthread_t		pollingThread;		/* Life Support polling loop runs in this thread */
	bool			pollingThreadSetup;	/* TRUE => Life Support polling thread was created */
	long			pollTime;			/* Nanoseconds since we last polled all signals */
	long			pollClockTime;		/* Nanoseconds until next periodic clock interrupt */

	pthread_mutex_t	clockLock;			/* Used to implement an interval timer */
	bool			clockLockSetup;		/* TRUE => the above mutex has been created */
	pthread_cond_t	clockSignal;		/* Used to implement an interval timer */
	bool			clockSignalSetup;	/* TRUE => the above has been created */
	pthread_t		clockThread;		/* The thread which actually implements the timer */
	bool			clockThreadSetup;	/* TRUE => Above thread has been created */
	long			clockTime;			/* Microseconds until next interval timer interrupt */

	EmbWord	resetRequestCount;			/* # of times that Lisp (not IFEP) requested a reset */
	EmbWord	restartApplicationsCount;	/* # of times we've been requested to restart */

	bool	inhibitDisk;			/* TRUE => don't process any disk I/O requests */
	EmbWord	debugLevel;				/* Controls level of debugging output */

	caddr_t	slaveTrigger;			/* Address of location monitored by VLM to simulate
									   a high-priority sequence break */

	pthread_mutex_t	XLock;			/* Used to control access to the X library */
	bool			XLockSetup;		/* TRUE => the above mutex has been created */

	pthread_mutex_t	wakeupLock;			/* Used by the VLM to wait for activity */
	bool			wakeupLockSetup;	/* TRUE => the above mutex has been created */
	pthread_cond_t	wakeupSignal;		/* Used by the VLM to wait for activity */
	bool			wakeupSignalSetup;	/* TRUE => the above has been created */

	/* Add new slots at the end of this structure as multiple programs use this data */
  }		EmbCommArea;

/*** Static variable containing the address of the communication area ***/
extern EmbCommArea	*EmbCommAreaPtr;


/* Values for EmbCommArea.system_type */
enum system_type
  {
	SystemTypeUX400G = 01003,		/* SGI box with Merlin board in it */
	SystemTypeXL400 = 01004,		/* Standalone Merlin-I system */
	SystemTypeMacIvory1 = 01006,	/* 2-card NuBus Ivory Macintosh coprocessor (240ns) */
	SystemTypeMacIvory2 = 01006,	/* 2-card NuBus Ivory Macintosh coprocessor (140ns) */
	SystemTypeUX400S = 01007,		/* Sun-3/Sun-4 running SunOS with Merlin board in it */
	SystemTypeXL1200 = 01011,		/* Standalone Merlin-II system */
	SystemTypeUX1200S = 01012,		/* Sun-3/Sun-4 running SunOS with Merlin-II board in it */
	SystemTypeUX1200G = 01013,		/* SGI box with Merlin-II board in it */
	SystemTypeMacIvory3 = 01014,	/* 1-card NuBus Ivory Macintosh coprocessor (65ns) */
	SystemTypeNXP1000 = 01015,		/* Standalone system based on the Domino board design */
	SystemTypeVLM = 01016			/* Virtual Lisp Machine */
  };

/* Values for EmbCommArea.reset_request */
enum ResetRequest
  {
	ReadNVRAMResetRequest = -6,		/* Read NVRAM maintained by host */
	WriteNVRAMResetRequest,			/* Write NVRAM maintained by host */
	AreYouThereResetRequest,		/* Ivory just wants to know if we're alive */
	BootResetRequest,				/* Boot ROM is starting -- Prepare to load Device PROM */
	DevicePROMResetRequest,			/* Load the next available chunk of the Device PROM */
	FEPResetRequest,				/* Reset request issued by IFEP doesn't clear everything */
	NoResetRequest,					/* Normal state */
	LispResetRequest				/* Reset request issued by Lisp proper clears everything */
  };

/* Values for EmbCommArea.guest_status */
enum GuestStatus
  {
	NonexistentGuestStatus = -2,	/* No guest hardware present */
	BrokenGuestStatus = -1,			/* Host or guest initialization failed */
	UninitializedGuestStatus = 0,	/* Communication area okay; guest not initialized yet */
	InitializingGuestStatus,		/* Guest being initialized; running BootROM/DevicePROM */
	InitializedGuestStatus,			/* IFEP running; Hello command not yet issued */
	StartedGuestStatus,				/* IFEP running; Hello command issued; Lisp hasn't run */
	CrashedGuestStatus,				/* IFEP running; Lisp had been running */
	RunningGuestStatus				/* Lisp running */
  };

/* Values for EmbCommArea.fep.status */
enum FEPStatus
  {
	HaltedFEPStatus = 0xFF,			/* FEP has stopped: 0xFF == -1 as unsigned 8-bit value */
	RunningFEPStatus = 0,			/* FEP is running */
	IdleFEPStatus = 1				/* FEP is idle (i.e., Lisp is running) */
  };


/*** Pointers ***/

/* Null pointer of EmbPtr type */
#define NullEmbPtr (EmbPtr)(-1)

/* Convert a guest pointer (EmbPtr) to a host pointer */
/* Assumes EmbCommAreaPtr is an accessible variable */
#define HostPointer(p) (PtrV)(&((EmbWord*)EmbCommAreaPtr)[p])

/* Convert a host pointer to a guest pointer (EmbPtr) */
/* Assumes EmbCommAreaPtr is an accessible variable */
#define GuestPointer(p) (EmbPtr)((EmbWord*)p - (EmbWord*)EmbCommAreaPtr)


/*** Strings ***/

typedef struct
  {
	EmbWord	length;					/* Number of characters in the string */
	EmbWord	string;					/* The string starts here and is stored in host order */
  }		EmbString;


/*** Queues ***/

typedef struct
  {
	EmbWord	element_size;		/* Number of bytes per queue element */
	EmbWord	queue_size;			/* Number of elements in the queue */
	EmbWord	put_index;			/* Index of next element to be written */
	EmbWord	take_index;			/* Index of next element to be read */
	SignalNumber	signal;		/* Send this signal on empty => non-empty transition;
								   -1 if no signal used */
	EmbWord	first_element[1];	/* Array of elements starts here */
  }		EmbQueue;

void	EmbQueuePut (EmbQueue* q, PtrV element);		/* Put element into queue */
bool	EmbQueueTake (EmbQueue* q, PtrV element);		/* Take element from queue */
int		EmbQueueSpace (EmbQueue* q);					/* Number of free elements */
int		EmbQueueFilled (EmbQueue* q);					/* Number of non-free elements */
void	EmbQueuePutWord (EmbQueue* q, EmbWord element);	/* Put element into word queue */
EmbWord	EmbQueueTakeWord (EmbQueue* q);					/* Take element from word queue */
void	EmbQueuePutByte (EmbQueue* q, byte element);	/* Put element into byte queue */
byte	EmbQueueTakeByte (EmbQueue* q);					/* Take element from byte queue */
int		EmbQueuePutWords (EmbQueue* q, EmbWord* elements, int count);	/* Multi-word */
int		EmbQueueTakeWords (EmbQueue* q, EmbWord* elements, int count);
int		EmbQueuePutBytes (EmbQueue* q, byte* elements, int count);		/* Multi-byte */
int		EmbQueueTakeBytes (EmbQueue* q, byte* elements, int count);


/*** Channels ***/

typedef struct
  {
	EmbWord	type;				/* Code number of type of channel */
	EmbWord	unit;				/* Distinguishes multiple channels of same type */
	EmbPtr	next;				/* Next channel in list of all channels */
	/* Remainder of structure depends on the channel type */
  }		EmbChannel;

/* Values for EmbChannel.type */
enum EmbChannelType
  {
	EmbDiskChannelType = 1,		/* Disk drive/partition */
	EmbConsoleChannelType,		/* Display, keyboard, and mouse */
	EmbNetworkChannelType,		/* Packet Network */
	EmbRPCChannelType,			/* Remote Procedure Call */
	EmbSCSIChannelType,			/* IFEP SCSI I/O */
	EmbColdLoadChannelType,		/* Cold load stream */
	EmbHostFileChannelType,		/* IFEP access to host file system */
	EmbMessageChannelType		/* High bandwidth message channel for I/O */
  };


/*** Disk Channels ***/

#define	NIOVectors	32

typedef struct
  {
	EmbWord		type;				/* EmbDiskChannelType */
	EmbWord		unit;				/* FEP unit number */
	EmbPtr		next;				/* Link to next channel */
	EmbWord		number_of_pages;	/* Size of disk in Ivory pages */
		/* -- More disk characteristics fields may be added later */
	EmbPtr		command_queue;		/* Commands from the guest */
	EmbPtr		status_queue;		/* Results from the host using guest's command buffers */

	struct							/* A word of flags ... */
	  {
#if BYTE_ORDER == LITTLE_ENDIAN
		uEmbWord	host_byte_order:1;	/* TRUE => byte swap data for this unit if needed */
		uEmbWord	read_only:1;		/* TRUE => this unit is read only */
		uEmbWord	:30;
#else
		uEmbWord	:30;
		uEmbWord	read_only:1;		/* TRUE => this unit is read only */
		uEmbWord	host_byte_order:1;	/* TRUE => byte swap data for this unit if needed */
#endif
	  }		flags;
	EmbWord		hostState0;			/* Pointer to host specific disk channel data ... */
	EmbWord		hostState1;			/* ... split into two words to avoid unaligned accesses */
	EmbWord         blocksize ;                     /* block size in bytes */
} 		EmbDiskChannel;

/* Internal data describing the disk channel */

typedef struct
  {
	EmbQueue	*command_queue_ptr;	/* For faster access to the queues ... */
	EmbQueue	*status_queue_ptr;	/* ... */
	bool		error_pending;		/* TRUE => channel is in error and needs to be reset */
	int		fd;			/* File descriptor of the partition or -1 if not open */
	int             blocksize ;             /* size of the blocks to read/write (in Bytes) */
	char*           filename ;              /* the filename */
	struct iovec	iovs[NIOVectors];	/* Describes buffers for current transaction */
  }		DiskChannelState;

/* Data structure passed by Lisp via the AttachDiskChannel coprocessor register */

typedef struct
  {
	EmbPtr		diskChannel;		/* Embedded pointer to disk channel setup by Lisp */
	EmbWord		filename;		/* Name of file to attach to channel (a DTP-STRING) */
	EmbWord		ifNotFoundAction;	/* What to do if the file doesn't exist */
	EmbWord		minimumLength;		/* File must be at least this many bytes */
	EmbWord		result;			/* Set to zero if attached, non-zero if an error occured */
	EmbPtr		errorMsg;		/* Set to embedded pointer of an error message, if any */
	EmbWord         blockSize ;             /* size of the blocks to be used (in words) */
  }		AttachDiskChannelRequest;

enum IfNotFoundActions
  {
	CreateIfNotFound,
	ErrorIfNotFound
  };

/* Data structure passed by Lisp via the GrowDiskPartition coprocessor register */

typedef struct
  {
	EmbPtr		diskChannel;		/* Embedded pointer to disk channel */
	EmbWord		newLength;		/* File must be grown to, at least, this many bytes */
	EmbWord		result;			/* Set to zero if attached, non-zero if an error occured */
	EmbPtr		errorMsg;		/* Set to embedded pointer of an error message, if any */
  }		GrowDiskPartitionRequest;

/* The queues contains pointers to EmbDiskQueueElement structures which describe a
   single disk I/O transaction */

enum EmbDiskCmd
  {
	ReadCmd = 1, 					/* Read pages from disk */
	WriteCmd,					/* Write pages onto the disk */
	ResetCmd,					/* Reset the channel, including any pending aborts */
	InitializeCmd					/* Initiailze the channel, including resetting meters */
  };

enum EmbDiskStatus
  {
	WonStatus = 1,					/* This operation completed successfully */
	LostStatus,					/* This operation failed */
	AbortStatus					/* This operation wasn't tried due to a pending abort */
  };

/* Data structure passed by Lisp via the UnixCrypt coprocessor register */

typedef struct
{
	EmbWord          cryptPassword ;                /* the password to crypt (a DTP-STRING) */
	EmbWord          cryptSalt ;                    /* the salt string (DTP-STRING) */
	EmbWord          cryptResult ;                  /* set to zero, if no errors, errno else */
	EmbPtr           cryptString ;                  /* resulting string when no errors (embedded string) */
} UnixCryptRequest ;

typedef struct
  {
#if BYTE_ORDER == LITTLE_ENDIAN
	uEmbWord	cmd:3;				/* An EmbDiskCmd code */
	uEmbWord	tagged:1;			/* 0 => 32-bit data, 1 => 40-bit data (NYI) */
	uEmbWord	buffered:1;			/* 0 => to main memory (NYI), 1 => to guest buffer */
	uEmbWord	:3;
	uEmbWord	suppress_error_recovery:1;	/* Don't attempt any error recovery (NYI) */
	uEmbWord	:7;
	uEmbWord	:16;
#else
	uEmbWord	:16;
	uEmbWord	:7;
	uEmbWord	suppress_error_recovery:1;	/* Don't attempt any error recovery (NYI) */
	uEmbWord	:3;
	uEmbWord	buffered:1;			/* 0 => to main memory (NYI), 1 => to guest buffer */
	uEmbWord	tagged:1;			/* 0 => 32-bit data, 1 => 40-bit data (NYI) */
	uEmbWord	cmd:3;				/* An EmbDiskCmd code */
#endif
 } EmbDiskOperation;

typedef struct
  {
	EmbPtr	address;				/* Address of buffer */
	EmbWord	n_words;				/* Number of words at that address */
  }		EmbAddressPair;

typedef struct
  {
	EmbWord		id;					/* Meaningful only to guest */
	EmbWord		sync;				/* Execute commands with EQ sync values in order given */
	EmbDiskOperation	op;			/* The command */
	EmbWord		page;				/* Starting disk address in pages */  
	EmbWord		count;				/* Length of transfer in pages */
	EmbWord		n_addresses;		/* Number of memory address pairs */
	EmbWord		status;				/* An EmbDiskStatus code */
	EmbWord		error_code;			/* If status == LostStatus, this is an OSErr */
	EmbAddressPair	addresses[1];	/* Describes the guest buffers for this transaction */
  }		EmbDiskQueueElement;


/*** Console Channel ***/

/* The data portion of an EmbConsoleCommandEnableRunLights buffer is described by this 
   structure which is also included in the channel itself */

typedef struct {
	EmbWord	windowID;				/* Identifiers window where the run lights will appear */
	EmbWord	nLights;				/* Number of run lights to be drawn */
	EmbWord	lightWidth;				/* Width in pixels of an individual run light */
	EmbWord	lightHeight;			/* Height in pixels ... */
	EmbWord	firstLightX;			/* Horizontal position of first run light in the window */
	EmbWord	firstLightY;			/* Verital position ... */
	EmbWord	lightXSpacing;			/* Horizontal spacing between run lights in pixels */
	EmbWord	lightYSpacing;			/* Vertical spacing ... */
	EmbWord	lightForeground;		/* Foreground color used to draw the run lights */
	EmbWord	lightBackground;		/* Background color ...*/
	EmbWord	lightPlaneMask;			/* Plane mask ...*/
  }		EmbConsoleRunLights;

/* The actual channel */

typedef struct {
	EmbWord	type;					/* EmbConsoleChannelType */
	EmbWord	unit;					/* 0 (not used) */
	EmbPtr	next;					/* Link to next channel */
	EmbPtr	outputRequestQueue;		/* Commands and data to send appear in this queue */
	EmbPtr	outputReplyQueue;		/* Our response to the commands are placed in this queue */
	EmbPtr	inputRequestQueue;		/* Requests to read data appear in this queue */
	EmbPtr	inputReplyQueue;		/* The data is placed in this queue */
	EmbWord	hostAddress;			/* IP address of host where the console will appear */
	EmbWord displayNumber;			/* Display number on the host; -1 for the default */
	EmbWord screenNumber;			/* Screen number on the host; -1 for the default */
    EmbWord initialState;			/* Console's initial state (a WindowInitialState) */
	EmbPtr	geometry;				/* String specifying the console's geometry if non-NULL */
	EmbPtr	foregroundColor;		/* String naming the foregound color if non-NULL */
	EmbPtr	backgroundColor;		/* String naming the background color if non-NULL */
	EmbPtr	borderColor;			/* String naming the border color if non-NULL */
	EmbPtr	borderWidth;			/* Width of border in pixels if greater than zero */
	EmbWord	inputAvailableP;		/* Non-zero if input is available without blocking */
	/* The remaining fields are not visible to Ivory */
	EmbQueue	*outputRequestQ;		/* For faster access to the queues ... */
	EmbQueue	*outputReplyQ;			/* ... */
	EmbQueue	*inputRequestQ;			/* ... */
	EmbQueue	*inputReplyQ;			/* ... */
	pthread_t	drawRunLights;			/* Thread used to draw the run lights */
	bool		drawRunLightsSetup;		/* TRUE => The above thread exists */
	char		*hostName;				/* Name of the host where the console will appear */
	void		*display;				/* X display object of the console */
	int			fd;						/* File descriptor from display object for I/O */
	int			openingState;			/* Open display substate (an OpeningState) */
	int			nextPixmapFormat;		/* Index of next pixmap format to return to Lisp */
	int			nextRoot;				/* Index of next root screen ... */
	int			nextRootDepth;			/* Index of said root's next depth ... */
	int			nextRootDepthVisual;	/* Index of said root's depth's next visual ...*/
	void		*rlDisplay;				/* Display used to draw the run lights */
	EmbConsoleRunLights	runLights;		/* Describes where and how the run lights are drawn */
	void		*rlGC;					/* Graphic context for the run lights */
	EmbWord		lastRunLights;			/* Value of run lights when we last drew them */
  }		EmbConsoleChannel;

/* State of the opening display finite-state machine -- See console.c for more information */

enum OpeningState
  {
	OpeningStateNone,					/* Opening dialogue is complete */
	OpeningStatePrefix,					/* Return the connection setup prefix */
	OpeningStateHeader,					/* Return the connection setup header */
	OpeningStateVendor,					/* Return the vendor's identification string */
	OpeningStatePixmapFormat,			/* Return the next available pixmap format */
	OpeningStateRoot,					/* Return the next available root window's header */
	OpeningStateRootDepth,				/* Return the next available depth of a root window */
	OpeningStateRootDepthVisual			/* Return the next available visual of a depth */
  };

/* Each command and reply is described by this structure */

typedef struct {
	EmbWord	opcode;					/* An EmbConsoleCommand */
	EmbWord	id;						/* Unique identifier of this command */
	EmbWord	result;					/* Standard Unix error code indicating success/failure */
	EmbWord	data[1];				/* First byte of data (if any) appears here */
  }		EmbConsoleBuffer;

enum EmbConsoleCommand
  {
	EmbConsoleCommandOpenDisplay = 1,	/* Open the display */
	EmbConsoleCommandCloseDisplay,		/* Close the display */
	EmbConsoleCommandNoOp,				/* Do nothing: Used for output synchronization */
	EmbConsoleCommandWrite,				/* Write data to the server */
	EmbConsoleCommandRead,				/* Read a specific number of bytes from the server */
	EmbConsoleCommandInputWait,			/* Wait until there's input available */
	EmbConsoleCommandEnableRunLights,	/* Enable drawing run lights */
	EmbConsoleCommandDisableRunLights	/* Disable drawing run lights */
  };

/* The data portion of an EmbConsoleCommandOpenDisplay buffer is described by this structure */

typedef struct {
	EmbWord	lastRequestNumber;		/* Sequence number of last request used during open */
  }		EmbConsoleOpenDisplay;

/* The data portion of EmbConsoleCommandWrite and EmbConsoleCommandRead buffers are described
   by this structure */

typedef struct {
	EmbWord	address;				/* Ivory address of the buffer */
	EmbWord	offset;					/* Offset within buffer of first byte to transfer */
	EmbWord	nBytes;					/* Number of bytes to transfer */
  }		EmbConsoleDataTransfer;

/* The data portion of an EmbConsoleCommandInputWait buffer is described by this structure */

typedef struct {
	EmbWord	timeout;				/* Milliseconds to wait for input to be available */
	EmbWord availableP;				/* Set non-zero if input is available; zero if timeout */
  }		EmbConsoleInputWait;


/*** Cold Load Stream Channel ***/

#define  ColdLoadCommandHistorySize  1024
//#define  ColdLoadCommandHistorySize  4096
#define ColdLoadProgressStringSize   256
//#define	 ColdLoadProgressStringSize   512

typedef struct {
  EmbWord	type;			/* EmbColdLoadChannelType */
  EmbWord	unit;			/* 0 (not used) */
  EmbPtr	next;			/* link to next channel */
  EmbPtr	keyboard_input_queue;	/* words containing keystrokes */
  EmbPtr	display_output_queue;	/* words containing display commands */
  EmbWord	display_width;		/* width in pixels of window */
  EmbWord	display_height;		/* height in pixels of window */
  EmbWord	character_width;	/* width in pixels of a character */
  EmbWord	line_height;		/* height in pixels of a line */
  struct
  {					/* FEP/Lisp progress note ... */
    EmbWord	numerator;		/* ... same meaning as for a real note */
    EmbWord	denominator;		/* ... " */
    EmbWord	string_total_size;	/* ... maximum size allowed for note's text */
    EmbWord	string_length;		/* ... non-zero => there's a note to display */
    char	string[ColdLoadProgressStringSize];	/* ... the actual text is put here */
  }		progress_note;
  /* The remaining fields are not visible to Ivory */
  pthread_t	coldLoadInput;		/* The thread that processes cold load input */
  bool		coldLoadInputSetup;	/* The thread has been created */
  int		fd;			/* File descriptor of the display if not -1 */
  bool	is_selected;			/* non-zero when cold load is selected */
  EmbWord	command_history_top;	/* last used element of command history */
  EmbWord	command_history_wrapped;/* whether/not history has wrapped around */
  EmbWord	command_history[ColdLoadCommandHistorySize];	/* the history */
} EmbColdLoadChannel;


/*** Network Channels ***/

#define MaxEmbNetPacketSize 1516

#ifdef OS_LINUX
/* ARP table entry a VLM IP address -- 
   Added to the host's ARP by InitializeLifeSupport and removed by TerminateLifeSupport */
typedef struct EmbNetARPReq
  {
	struct EmbNetARPReq	*next;
	struct arpreq		arp;
  }		EmbNetARPReq;
#endif

typedef struct
  {
	EmbWord	type;					/* EmbNetworkChannelType */
	EmbWord	unit;					/* Network interface number */
	EmbPtr	next;					/* Next channel in list of all channels */
	EmbWord	status;					/* Status bits (see below) */
	EmbPtr	guestToHostQueue;		/* Guest to host outgoing packet queue */
	EmbPtr	guestToHostReturnQueue;	/* Host to guest freed outgoing packets queue */
	EmbPtr	hostToGuestSupplyQueue;	/* Guest to host incoming free packets queue */
	EmbPtr  hostToGuestQueue;		/* Host to guest incoming packets queue */
	EmbWord	name0;					/* Network interface name -- 1st 4 characters */
	EmbWord	name1;					/* Network interface name -- 2nd 4 characters */
	EmbWord	hardwareAddressHigh;	/* Interface hardware address -- high order 32 bits */
	EmbWord	hardwareAddressLow;		/* Interface hardware address -- low order 16 bits */
	EmbWord	hostPrimaryProtocol;	/* Host's primary network protocol's Ethernet type */
	EmbWord	hostPrimaryAddress;		/* Host's network address on this interface */
	EmbWord	guestPrimaryProtocol;	/* Guest's primary network protocol's Ethernet type */
	EmbWord	guestPrimaryAddress;	/* Guest's network address on this interface */
	/* Meters */
	EmbWord	nTransmitFailures;		/* Counter of unsuccessful packet transmissions */
	EmbWord nReceiveFailures;		/* Counter of unsuccessful reads of incoming packets */
	EmbWord	nFalseReceiverWakeups;	/* Counter of read timeouts */
	EmbWord nReceivedPacketsLost;	/* Counter of packets received but guest wouldn't take */
	EmbWord	unusedMeters[4];
	EmbPtr	addressString;			/* String describing this interface's network address */
	/* The remaining fields are not visible to Ivory */
	EmbQueue	*guestToHostQ;			/* For faster access to the queues ... */
	EmbQueue	*guestToHostReturnQ;	/* ... */
	EmbQueue	*hostToGuestSupplyQ;	/* ... */
	EmbQueue	*hostToGuestQ;			/* ... */
#ifdef USE_LIBPCAP
    pcap_t* pcap;
#else
#ifdef OS_LINUX
	int		fd;		/* File descriptor of our filter or -1 if not open */
	unsigned int    net_broken ;    /* non-zero if machine don't handle mac's correctly */
	struct sockaddr_ll	sll;		/* Contains information needed to write packets */
	EmbNetARPReq	*arpReq;	/* List of ARP entries associated with this channel */
#endif
#endif
	EmbNetFilter filter;				/* Our packet filter psuedo-code */
	pthread_t	receiverThread;			/* Packet receiver runs in this thread */
	boolean		receiverThreadSetup;	/* TRUE => Above thread has been created */
	int			alignmentPad;			/* Ensure that the buffer is 32-bit word-aligned */
	byte		receiveBuffer[MaxEmbNetPacketSize];	/* Buffer for incoming packets */
  }		EmbNetChannel;

/* Flags defined in the channel's status slot */

#define EmbNetStatusHostReady	1	/* Alpha side of Ethernet channel is ready */
#define EmbNetStatusGuestReady	2	/* VLM side of Ethernet channel is ready */

/* Queues contain pointers to the buffers with the following structure */

typedef struct
  {
	EmbWord	nBytes;					/* Number of bytes in packet */
	EmbWord	data[1];				/* The data */
  }		EmbNetPacket;


/*** High Bandwidth Message Channel for I/O ***/

typedef struct
  {
	EmbWord	type;					/* EmbMessageChannelType */
	EmbWord	unit;					/* Guest's unique ID for this channel (if any) */
	EmbPtr	next;					/* Next channel in list of all channels */
	EmbWord	subtype;				/* Specific type of I/O (serial, X window, etc.) */
	EmbPtr	guestToHostQueue;		/* Guest to host outgoing commands/data */
	EmbPtr	guestToHostReturnQueue;	/* ... return queue for the above buffers */
	EmbWord	guestToHostImpulse;		/* Non-zero => Guest is requesting immediate action */
	EmbPtr	hostToGuestQueue;		/* Host to guest incoming status/data */
	EmbPtr	hostToGuestSupplyQueue;	/* ... supply queue for the above buffers */
	EmbWord	hostToGuestImpulse;		/* Non-zero => Host is requesting immediate action */
	uEmbWord	subtypeData0;		/* Pointer to subtype specific data structure ... */
	uEmbWord	subtypeData1;		/* ... split into two words to avoid unaligned accesses */
 }		EmbMessageChannel;

enum EmbMessageChannelSubtype
  {
	EmbMessageChannelSerialSubtype = 1,		/* Asynchronous serial I/O */
	EmbMessageChannelCommandSubtype, 		/* General operations (substitute for RPC) */
	EmbMessageChannelMBINSubtype			/* Minima ROM MBIN protocol */
  };

enum EmbMessageImpulse
  {
	EmbMessageImpulseNone = 0
  };

/* All subchannel data structures must include the following information */

#define EmbMessageSubtypeDataHeader \
  struct \
   { \
	EmbMessageChannel*	nextActiveChannel;	/* Pointer to next active message channel */ \
	EmbCommArea*		commArea;			/* Backpointers for use by the signal handler */ \
	EmbMessageChannel*	messageChannel;		/* ... */ \
   }	header

typedef struct
  {
    EmbMessageSubtypeDataHeader;
  }		EmbMessageSubtypeData;

/* Queues contain pointers to the following command/data/status blocks */

#if BYTE_ORDER == LITTLE_ENDIAN 
#define EmbMessageBufferHeader \
  struct \
    { \
	  EmbWord	length:24;			/* Amount of data in buffer in bytes */ \
	  EmbWord	opcode:8;			/* Subtype specific opcode */ \
    }	header
#else
#define EmbMessageBufferHeader \
  struct \
    { \
	  EmbWord	opcode:8;			/* Subtype specific opcode */ \
	  EmbWord	length:24;			/* Amount of data in buffer in bytes */ \
    }	header
#endif

typedef struct
  {
	EmbMessageBufferHeader;
	EmbWord		data[1];			/* Data bytes in host order */
  }		EmbMessageBuffer;


/* General Operations (Command) */

typedef struct
  {
	EmbMessageSubtypeDataHeader;
	EmbQueue*		guestToHostQueue;		/* More convenient form of queue pointers */
	EmbQueue*		guestToHostReturnQueue;	/* ... */
  }		EmbCommandChannel;

enum EmbCommandBufferOpcode
  {
	EmbCommandBufferStartMBIN = 1	/* Activate an MBIN message channel */
  };

#define EmbCommandBufferHeader \
  EmbMessageBufferHeader; \
  EmbWord		resultCode			/* Non-zero if the command wasn't executed successfully */

typedef struct
  {
	EmbCommandBufferHeader;
	EmbWord		operands[1];		/* Operands for the command as supplied by the guest */
  }		EmbCommandBuffer;

typedef struct
  {
	EmbCommandBufferHeader;
	EmbPtr		mbinChannel;		/* Pointer to MBIN message channel to be made active */
  }		EmbCommandStartMBINBuffer;


/* Minima ROM MBIN protocol -- Unlike other message channels, the buffers in this channel 
   conform to the remote memory protocol defined by the Remote Debugger (aka Minima Debugger),
   spy.c, and the various Minima ROMs */

typedef struct
  {
	EmbMessageSubtypeDataHeader;
	EmbQueue*		guestToHostQueue;		/* More convenient form of queue pointers */
	EmbQueue*		guestToHostReturnQueue;	/* ... */
	EmbQueue*		hostToGuestQueue;		/* ... */
	EmbQueue*		hostToGuestSupplyQueue;	/* ... */
  }		EmbMBINChannel;

enum EmbMBINImpulse
  {
	EmbMBINImpulseShutdown = 1
  };

#endif
