/* Configuration parameters for the VLM:
      Defaults can be overridden by either the user's .VLM file or command line arguments */

#ifndef _VLM_CONFIG_
#define _VLM_CONFIG_

#include <sys/types.h>
#include <limits.h>
#include <sys/param.h>
#include "pfilt_wrapper.h"

#include "life_types.h"


/* X window configuration parameters */

typedef struct
  {
	char	*xpHostName;			/* Name of host where screen will appear; NULL for local */
	long	xpHostAddress;			/* Protocol address of the above */
	int		xpDisplay;				/* Display number on the host; -1 for default */
	int		xpScreen;				/* Screen number on the host; -1 for default */
	int		xpInitialState;			/* Window's initial state (a WindowInitialState) */
	char	*xpGeometry;			/* X geometry specification for window */
	char	*xpForegroundColor;		/* Name of foreground color */
	char	*xpBackgroundColor;		/* Name of background color */
	char	*xpBorderColor;			/* Name of border color */
	int		xpBorderWidth;			/* Width of border in pixels */
  }		XParams;


/* Configuration data for a single network interface */

typedef struct NetworkInterface
  {
	  boolean			present;		/* TRUE => this interface is available */
	  char			device[_POSIX_PATH_MAX+1];	/* Optional interface or packet filter name */
	  unsigned short	myProtocol;			/* Primary Ethernet protocol */
	  struct in_addr	myAddress;			/* Primary protocol address */
	  struct in_addr        myHostAddress;                  /* Hosts primary address */
	  boolean haveMac;                                      /* true, if MAC set */
	  struct mac_addr {
		  unsigned char bytes[6];
	  } myMac;                                              /* MAC address, if given */
#ifdef GENERA
	char			myOptions[_POSIX_PATH_MAX+1];	/* Primary network options */
    struct NetworkInterface*	anotherAddress;		/* Secondary address for this interface */
#endif
  }		NetworkInterface;

#define MaxNetworkInterfaces 8
#define ETHERTYPE_CHAOS 0x0804


/* We'll place the communications area immediately after the BootComm and BootData areas
   in "VMA=PMA" space.  Those two area occupy 128 (#x80) words starting at #xFFFE0000.
   Thus, the communications area will begin at location #xFFFE0080 and can be no more 
   than #x1FF80 (130944) words. */
#define EmbCommAreaAddress 0xFFFE0080L
#define DefaultEmbCommAreaSize 0x1FF80
#define DefaultHostBufferSpace 15000
#define DefaultGuestBufferSpace 100000

#define DefaultVLMConfigFilePathname "/var/lib/symbolics/.VLM"

#define DefaultVLMDebuggerPathname "/var/lib/symbolics/VLM_debugger"

#define DefaultGeneraWorldPathname "/var/lib/symbolics/Genera-8-5.vlod"
#define DefaultMinimaWorldPathname "/var/lib/symbolics/Minima.mlod"
#define DefaultWorldSearchPath "/var/lib/symbolics:/usr/lib/symbolics"

#define DefaultVirtualMemory "200"
#define MinimumVirtualMemory 125


/* Channel Queue sizes -- Eventually, these could be parameters in the configuration file */

#define DiskQueueSize			32
#define ConsoleInputQueueSize		50
#define ConsoleOutputQueueSize		50
#define NetworkReceiverQueueSize	100  /* was 20, prevents losing so many packets */
#define NetworkTransmitterQueueSize     60   /* was 20 */
#define RPCHostToGuestQueueSize		15
#define RPCGuestToHostQueueSize		30
#define SCSIQueueSize			10
#define ColdLoadInputQueueSize		50 /* was 100: jj? */
#define ColdLoadOutputQueueSize		50  /* was 50: jj? */
#define HostFileCommandQueueSize 	5
#define HostFileReplyQueueSize 		5
#define CommandQueueSize		5


/* Instruction tracing configuration data */

typedef struct
  {
    boolean		traceP;			/* TRUE => Enable instruction tracing */
    boolean		tracePOST;		/* TRUE => Trace the POST test */
    int			bufferSize;		/* Size of circular trace buffer */
    unsigned int	startPC;		/* PC where tracing will start if non-zero */
    unsigned int	stopPC;			/* PC where tracing will stop if non-zero */
    char		*outputFile;		/* Name of file to record full trace */
}  TraceConfig;


/* Main configuration data structure */

typedef struct
  {
	boolean		enableSpy;							/* TRUE => Enable remote memory spy */
	TraceConfig	tracing;							/* Controls instruction tracing */
	size_t		commAreaSize;						/* Size of communications area in words */
	size_t		hostBufferSpace;					/* Words reserved for host buffers */
	size_t		guestBufferSpace;					/* Words reserved for guest buffers */
	char		vlmDebuggerPath[_POSIX_PATH_MAX+1];	/* Pathname of VLM debugger to be loaded */
	char		worldPath[_POSIX_PATH_MAX+1];		/* Pathname of world load to be loaded */
#ifdef GENERA
	char*		worldSearchPath;					/* -> Directories to search for worlds */
	boolean		enableIDS;							/* TRUE => allow incremental disk saves */
	size_t		virtualMemory;						/* Size of emulated virtual memory */
#endif
	XParams		coldLoadXParams;					/* X parameters for cold load window */
	XParams		generaXParams;						/* X Parameters for the main screen */
	struct in_addr	diagnosticIPAddress;			/* IP address of our diagnostic server */
	NetworkInterface	interfaces[MaxNetworkInterfaces];	/* Network interfaces [8] */
	boolean		testFunction;                       /* TRUE => run TESTFCN instead of FIB for POST */
	/* Other parameters? */
  }		VLMConfig;

#endif
