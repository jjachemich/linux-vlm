/* -*- Mode:C -*- */

#include "std.h"
#include <sys/param.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <utmp.h>

#ifdef _C_EMULATOR_
#include "emulator.h"
#include "ivory.h"
#else
#include "aistat.h"
#include "aihead.h"
#include "ivoryrep.h"
#endif
#include "memory.h"
#include "spy.h"

#include "life_types.h"
#include "embed.h"
#include "life_prototypes.h"
#include "utilities.h"

/* BEGIN KLUDGE! */
#define MemoryPageNumber(vma) ((vma) >> MemoryPage_AddressShift)
#define CreatedP(vma) VMExists(VMAttributeTable[MemoryPageNumber(vma)])
/* END KLUDGE! */

#define PageSize 0x100

#define REMOTE_MEMORY_PACKET_DATA 1284
#define MEGABYTES 256

typedef enum {
  rm_discard,
  rm_noop,
  rm_ack,
  rm_write,
  rm_read,
  rm_system_startup,
  rm_trap,
  rm_boot,
  rm_create_pages,
  rm_mbin,
  rm_stop
  } remote_memory_opcode;

typedef enum {
  rm_physical,
  rm_virtual,
  rm_register,
  rm_coprocessor
  } remote_memory_type;

struct rm_pkt {
  unsigned char rm_pad[2];
  unsigned char rm_id[4];
  unsigned char rm_operand[3];
  int rm_opcode:8;
  unsigned char data[REMOTE_MEMORY_PACKET_DATA];
};

#define REMOTE_MEMORY_PACKET_HEADER 10

struct rm_aligned_pkt {
  unsigned char rm_id[4];
  unsigned char rm_operand[3];
  int rm_opcode:8;
  unsigned char data[REMOTE_MEMORY_PACKET_DATA];
};

#define REMOTE_MEMORY_ALIGNED_PACKET_HEADER 8

static int spy = -1;
static int send_trap = 0;
static pthread_key_t mainThread;

static pthread_mutex_t spyLock;
static boolean spyLockSetup = FALSE;
static pthread_t spyThread;
static boolean spyThreadSetup = FALSE;
EmbMBINChannel *activeMBINChannel = NULL;
static struct {unsigned int id; boolean acked;} MBINHistory[16];

/* Divine a port number to use.  Try to use 2900 + xxx where xxx is 128.81.41.xxx,
   if we're coming in from SLIP or something we're on a different subnet, so we fudge. */

static int divine_port_number(unsigned long diagnosticAddress)
{
  unsigned long port;

  port = htonl (diagnosticAddress) - 0x80512900; /* 128.81.41.00 */
  if (port < 0) port = -port;
  if (port > 256) port = port % 256 + 256; else port = port % 256;
  port += 2900;
  return port;
}

static void bind_a_port(int spy,struct sockaddr_in * sin,int len)
{
  if (bind(spy, (struct sockaddr*)sin, len)) {
    verror ("spy", NULL);
    sin->sin_port = htons(ntohs(sin->sin_port) + 1);
    bind_a_port(spy,sin,len);
  }
  else vwarn (NULL, "Spy started on port %d.", ntohs(sin->sin_port));
}

/* static void signal_handler (int x) */
/* { */
/*   HaltMachine(); */
/* } */

static int spy_transmit (struct rm_pkt *pkt, int rm_length, struct sockaddr_in *sin)
{
  int result = 0;
  pthread_cleanup_push((pthread_cleanuproutine_t)pthread_mutex_unlock, (void*)&spyLock);
  if (pthread_mutex_lock (&spyLock)) 
    vpunt ("spy", "Unable to lock the spy lock in thread %x", pthread_self ());
  if (sendto(spy, &pkt->rm_pad[0], rm_length, 0, (struct sockaddr*)sin, sizeof(struct sockaddr_in)) < 0)
    {
      verror ("spy", NULL);
      result = 1;
    }
  if (pthread_mutex_unlock (&spyLock))
    vpunt ("spy", "Unable to unlock the spy lock in thread %x", pthread_self ());
  pthread_cleanup_pop(FALSE);
  return(result);
}

static unsigned int read_long (unsigned char *bytes)
{
  return (bytes[0] | (bytes[1]<<8) | (bytes[2]<<16) | (bytes[3]<<24));
}

static void write_long (unsigned char *bytes, unsigned int data)
{
  bytes[0] = data;
  bytes[1] = data>>8;
  bytes[2] = data>>16;
  bytes[3] = data>>24;
}

static struct sockaddr_in trap_sin;
static boolean trap_sinValid = FALSE;
static struct sockaddr_in mbin_sin;
static boolean mbin_sinValid = FALSE;
static jmp_buf trap_environment;

/* Catch SEGV's when poking at memory */
static void segv_handler (int number)
{
  _longjmp(trap_environment, -1);
}

static void SpyTopLevel (pthread_addr_t argument)
{
  pthread_t self = pthread_self ();

  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);
  if (pthread_mutex_lock (&spyLock))
    vpunt ("spy", "Unable to lock the spy lock in thread %x", self);
  if (pthread_mutex_unlock (&spyLock)) 
    vpunt ("spy", "Unable to unlock the spy lock in thread %x", self);

  RemoteMemorySpyLoop ();			/* Returns iff the spy port has closed */

  pthread_cleanup_pop (TRUE);
}

#define SuspendVLM() if(Runningp()){forciblyHalted = TRUE; HaltMachine(); while (Runningp());}
#define ResumeVLM(resumeP) if(forciblyHalted){forciblyHalted = FALSE; StartMachine(resumeP);}

void RemoteMemorySpyLoop ()
{
  struct rm_pkt pkt, reply;
  LispObj buffer[PageSize];
  LispObj *bufferp;
  unsigned char *p;
  unsigned int vma, operand;
  int nwords, nchunks, i, pkt_length, reply_length, sinlen;
  int booted = 0;
  struct sockaddr_in pkt_source;
  struct pollfd pollSpy;
  boolean forciblyHalted = FALSE;

  pollSpy.fd = spy;
  pollSpy.events = POLLIN;

  while (1)
    {
      pthread_testcancel();

      if (!Runningp()) {
	if (send_trap) {
	  PushOneFakeFrame();
	  PushOneFakeFrame();
	  /* this does "remote system halted" */
	  if (trap_sinValid) {
	    reply.rm_opcode = rm_trap;
	    spy_transmit(&reply, REMOTE_MEMORY_PACKET_HEADER, &trap_sin);
	  }
	  send_trap = 0;
	}
      }

      if (0 == (i = poll(&pollSpy, 1, 1000)))
	continue;
      else if (i < 0)
	vpunt ("spy", "Waiting for a packet from the remote debugger");
      else if (pollSpy.revents & (POLLHUP | POLLNVAL))
	/* Spy port has vanished -- Assume that the emulator is shutting down ... */
	break;

      sinlen = sizeof(struct sockaddr_in);
      if ((pkt_length = recvfrom(spy, &pkt.rm_pad[0], REMOTE_MEMORY_PACKET_HEADER + REMOTE_MEMORY_PACKET_DATA, 0, (struct sockaddr*)&pkt_source, (socklen_t *)&sinlen)) < 0)
	vpunt ("spy", "Reading packet from remote debugger");

      reply.rm_operand[0] = 0;
      reply.rm_operand[1] = 0;
      reply.rm_operand[2] = 0;
      reply_length = REMOTE_MEMORY_PACKET_HEADER;
      reply.rm_opcode = rm_ack;
      memcpy(&reply.rm_id[0], &pkt.rm_id[0], 4);
      
      operand = read_long(&pkt.rm_operand[0]) & 0xffffff;
      
      switch (pkt.rm_opcode)
      {
	case rm_boot:
	  SuspendVLM ();
	  spy_transmit(&reply, reply_length, &pkt_source);
	  InitializeIvoryProcessor(MapVirtualAddressData (0), MapVirtualAddressTag (0));
	  booted = 1;
	  ResumeVLM (TRUE);
	  break;
	  
	case rm_create_pages:
	  SuspendVLM ();
	  vma = read_long(&pkt.data[0]);
	  nwords = read_long(&pkt.data[4]);
	  EnsureVirtualAddressRange(vma, nwords, FALSE);
	  spy_transmit(&reply, reply_length, &pkt_source);
	  ResumeVLM (TRUE);
	  break;
	  
	case rm_noop:
	  spy_transmit(&reply, reply_length, &pkt_source);
	  break;
	  
	case rm_read:
	  SuspendVLM ();
	  vma = read_long(&pkt.data[0]);
	  nwords = operand & 0x3ff;
	  nchunks = (nwords+3)/4;
	  switch ((operand>>10) & 3)
	  {
	    case rm_physical:
#ifdef _C_EMULATOR_
	      goto READ_WRITE_MEMORY_ERROR;
#else
	      /* Use physical addresses to read uncached data */
	      {
		void (*old_segv_handler) () = signal(SIGSEGV, segv_handler);
		if (_setjmp(trap_environment)) {
		  signal(SIGSEGV, old_segv_handler);
		  goto READ_WRITE_MEMORY_ERROR;
		}
		for (i = 0; i < nwords; i++) {
		  if (!CreatedP(vma+i)) goto READ_WRITE_MEMORY_ERROR;	/* KLUDGE! */
		  buffer[i] = (VirtualMemoryReadUncached(vma + i));
		}
                signal(SIGSEGV, old_segv_handler);
              }
	      break;
#endif
	      
	    case rm_virtual:
	      {
		void (*old_segv_handler) () = signal(SIGSEGV, segv_handler);
		if (_setjmp(trap_environment)) {
		  signal(SIGSEGV, old_segv_handler);
		  goto READ_WRITE_MEMORY_ERROR;
		}
		for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		  if (ReadVirtualMemory (vma+i, &buffer[i])) goto read_error;
#else
		  if (!CreatedP(vma+i)) goto READ_WRITE_MEMORY_ERROR;	/* KLUDGE! */
		  buffer[i] = (VirtualMemoryRead(vma + i));
#endif
		}
                signal(SIGSEGV, old_segv_handler);
              }
	      break;
	      
	    case rm_register:
	      for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		ReadInternalRegister (vma+i, &buffer[i]);  
#else
		buffer[i] = ReadInternalRegister(vma + i);
#endif
	      }
	      break;

	    case rm_coprocessor:
	      for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		vwarn ("spy", "Read of coprocessor register %d failed.", (vma + i));
#else
		buffer[i] = CoprocessorRead(vma + i);
#endif
	      }
	      break;
	  }
	  for (i = 0, bufferp = &buffer[0], p = &reply.data[0];
	       i < nchunks;
	       i++, bufferp += 4, p += 20)
	    {
	      p[0] = LispObjTag(bufferp[0]);
	      p[1] = LispObjTag(bufferp[1]);
	      p[2] = LispObjTag(bufferp[2]);
	      p[3] = LispObjTag(bufferp[3]);
	      write_long(&p[4], LispObjData(bufferp[0]));
	      write_long(&p[8], LispObjData(bufferp[1]));
	      write_long(&p[12], LispObjData(bufferp[2]));
	      write_long(&p[16], LispObjData(bufferp[3]));
	    }
	  reply_length += nchunks*20;
	  spy_transmit(&reply, reply_length, &pkt_source);
	  ResumeVLM (TRUE);
	  break;
	  
READ_WRITE_MEMORY_ERROR:
	  reply.rm_operand[0] = 1;
	  spy_transmit(&reply, reply_length, &pkt_source);
	  ResumeVLM (rm_read == pkt.rm_opcode);
	  break;
	  
	case rm_stop:
	  forciblyHalted = FALSE;
	  HaltMachine();
	  spy_transmit(&reply, reply_length, &pkt_source);
	  break;
	  
	case rm_system_startup:
	  spy_transmit(&reply, reply_length, &pkt_source);
	  memcpy(&trap_sin, &pkt_source, sizeof(struct sockaddr_in));
	  trap_sinValid = TRUE;
	  memcpy (&mbin_sin, &pkt_source, sizeof(struct sockaddr_in));
	  mbin_sinValid = TRUE;
	  if (!IvoryProcessorSystemStartup (booted))
	    vwarn ("spy", "Bad start routine.");
	  send_trap = 1;
	  break;

	case rm_write:
	  SuspendVLM ();
	  vma = read_long(&pkt.data[0]);
	  nwords = operand & 0x3ff;
	  nchunks = (nwords+3)/4;
	  for (i = 0, bufferp = &buffer[0], p = &pkt.data[4];
	       i < nchunks;
	       i++, bufferp += 4, p += 20)
	    {
	      LispObjTag(bufferp[0]) = p[0];
	      LispObjTag(bufferp[1]) = p[1];
	      LispObjTag(bufferp[2]) = p[2];
	      LispObjTag(bufferp[3]) = p[3];
	      LispObjData(bufferp[0]) = read_long(&p[4]);
	      LispObjData(bufferp[1]) = read_long(&p[8]);
	      LispObjData(bufferp[2]) = read_long(&p[12]);
	      LispObjData(bufferp[3]) = read_long(&p[16]);
	    }
	  switch ((operand>>10) & 3)
	  {
	    case rm_physical:
	      goto READ_WRITE_MEMORY_ERROR;
	      
	    case rm_virtual:
              {
                void (*old_segv_handler) () = signal(SIGSEGV, segv_handler);
                if (_setjmp(trap_environment)) {
		  signal(SIGSEGV, old_segv_handler);
		  goto READ_WRITE_MEMORY_ERROR;
		}
		for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		  if (WriteVirtualMemory (vma+i, &buffer[i])) goto read_error;
#else
		  if (!CreatedP(vma+i)) goto READ_WRITE_MEMORY_ERROR;	/* KLUDGE! */
		  VirtualMemoryWrite(vma + i, buffer[i]);
#endif
		}
		signal(SIGSEGV, old_segv_handler);
	      }
	      break;
	      
	    case rm_register:
	      for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		if (!WriteInternalRegister(vma + i,&buffer[i]))
		  vwarn ("spy", "Write of internal register %d failed", vma + i);
#else
		if (WriteInternalRegister(vma + i,buffer[i])==-1)
		  vwarn ("spy", "Write of internal register %d failed", vma + i);
#endif
	      }
	      break;

	    case rm_coprocessor:
	      for (i = 0; i < nwords; i++) {
#ifdef _C_EMULATOR_
		vwarn ("spy", "Write of coprocessor register %d failed.", (vma + i));
#else
		if (!CoprocessorWrite(vma + i, buffer[i]))
		  vwarn ("spy", "Write of coprocessor register %d failed.", (vma + i));
#endif
	      }
	      break;
	  };
	  spy_transmit(&reply, reply_length, &pkt_source);
	  ResumeVLM (FALSE);
	  break;
	  
	case rm_mbin:
	  {
	    EmbPtr bufferEmbPtr;
	    struct rm_aligned_pkt *buffer;
	    unsigned int id;
	    int historyID;
	    memcpy (&mbin_sin, &pkt_source, sizeof(struct sockaddr_in));
	    mbin_sinValid = TRUE;
	    if (activeMBINChannel) {
	      id = read_long(&pkt.rm_id[0]);
	      historyID = id & 0xF;
	      if (MBINHistory[historyID].id == id) {
		/* ... */
		if (MBINHistory[historyID].acked) 
		  spy_transmit(&reply, reply_length, &pkt_source);
	      }
	      else if (EmbQueueFilled (activeMBINChannel->hostToGuestSupplyQueue)
		       && EmbQueueSpace (activeMBINChannel->hostToGuestQueue)) {
		/* ... */
		bufferEmbPtr = EmbQueueTakeWord (activeMBINChannel->hostToGuestSupplyQueue);
		if (bufferEmbPtr && (bufferEmbPtr != NullEmbPtr)) {
		  buffer = (struct rm_aligned_pkt*) HostPointer (bufferEmbPtr);
		  memcpy (&buffer->rm_id[0],&pkt.rm_id[0],REMOTE_MEMORY_ALIGNED_PACKET_HEADER);
		  memcpy (&buffer->data[0], &pkt.data[0], operand);
		  EmbQueuePutWord (activeMBINChannel->hostToGuestQueue, bufferEmbPtr);
		  MBINHistory[historyID].id = id;
		  MBINHistory[historyID].acked = FALSE;
	        }
	      }
	    }
	  }
	  break;

	case rm_discard:
	  break;
      }
    }
}


/* External Interfaces */

void InitializeSpy (boolean sendTrapP, unsigned long diagnosticAddress)
{
  struct sockaddr_in sin;
  int ipport_remote_memory;

  if (pthread_key_create (&mainThread, NULL))
    vpunt (NULL, "Unable to establish per-thread data.");
  pthread_setspecific (mainThread, (void*) TRUE);

  atexit (&TerminateSpy);

  ipport_remote_memory = divine_port_number(diagnosticAddress);

  if ((spy = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    vpunt ("spy", "Unable to create spy socket");
  sin.sin_family =  AF_INET;
  sin.sin_addr.s_addr = INADDR_ANY;
  sin.sin_port = htons(ipport_remote_memory);
  bind_a_port(spy,&sin,sizeof(sin));
  send_trap = sendTrapP;

  memset ((char*)&MBINHistory[0], 0, sizeof (MBINHistory));

  if (pthread_mutex_init (&spyLock, NULL))
    vpunt ("spy", "Unable to create the spy lock");
  spyLockSetup = TRUE;
  
  if (pthread_create (&spyThread, 
		      &EmbCommAreaPtr->pollThreadAttrs, (pthread_startroutine_t)&SpyTopLevel,
		      NULL))
    vpunt ("spy", "Unable to create the spy thread");
  spyThreadSetup = TRUE;

  return;
}

void ReleaseSpyLock ()
{
  if (pthread_mutex_unlock (&spyLock)) 
    vpunt ("spy", "Unable to unlock the spy lock in thread %x", pthread_self ());
}

void	SendMBINBuffers (EmbMBINChannel* mbinChannel)
{
  register EmbQueue *gthQ = mbinChannel->guestToHostQueue;
  register EmbQueue *gthrQ = mbinChannel->guestToHostReturnQueue;
  EmbPtr bufferPtr;
  struct rm_aligned_pkt *buffer;
  struct rm_pkt pkt;
  unsigned int nBytes, id;
  int historyID;

  if (mbinChannel->header.messageChannel->guestToHostImpulse)
    switch (mbinChannel->header.messageChannel->guestToHostImpulse)
    {
      case EmbMBINImpulseShutdown:
        activeMBINChannel = NULL;
	ResetIncomingQueue (gthQ);
	ResetOutgoingQueue (gthrQ);
	ResetIncomingQueue (mbinChannel->hostToGuestSupplyQueue);
	ResetOutgoingQueue (mbinChannel->hostToGuestQueue);
	mbinChannel->header.messageChannel->guestToHostImpulse = EmbMessageImpulseNone;
	UnthreadMessageChannel (mbinChannel->header.messageChannel);
	free (mbinChannel);
	return;
      default:
	mbinChannel->header.messageChannel->guestToHostImpulse = EmbMessageImpulseNone;
        break;
    }

  while (EmbQueueFilled (gthQ)) {
    if (0 == EmbQueueSpace (gthrQ)) {
      SignalLater (gthQ->signal);
      return;
    }
    bufferPtr = EmbQueueTakeWord (gthQ);
    if (bufferPtr && (bufferPtr != NullEmbPtr) && mbin_sinValid) {
      buffer = (struct rm_aligned_pkt*) HostPointer (bufferPtr);
      nBytes = read_long(&buffer->rm_operand[0]) & 0xFFFFFF;
      memcpy (&pkt.rm_id[0], &buffer->rm_id[0], REMOTE_MEMORY_ALIGNED_PACKET_HEADER);
      memcpy (&pkt.data[0], &buffer->data[0], nBytes);
      if (rm_ack == buffer->rm_opcode) {
	id = read_long(&buffer->rm_id[0]);
	historyID = id & 0xF;
	MBINHistory[historyID].id = id;
	MBINHistory[historyID].acked = TRUE;
      }
      spy_transmit(&pkt, REMOTE_MEMORY_PACKET_HEADER + nBytes, &mbin_sin);
    }
    EmbQueuePutWord (gthrQ, bufferPtr);
  }
}

void TerminateSpy ()
{
  struct timespec killSleep;
  void *exit_code;

  if (NULL == pthread_getspecific (mainThread)) return;

  if (spyThreadSetup) {
    pthread_cancel (spyThread);
    killSleep.tv_sec = 1;
    killSleep.tv_nsec = 250000000;
    pthread_delay_np (&killSleep);
    pthread_join (spyThread, &exit_code);
    spyThreadSetup = FALSE;
  }
  if (spyLockSetup) {
    pthread_mutex_destroy (&spyLock);
    spyLockSetup = FALSE;
  }
  if (spy != -1) {
    close(spy);
    spy = -1;
  }
}
