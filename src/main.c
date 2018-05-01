/* -*- Mode: C; Tab-Width: 4 -*- */

#include "std.h"

#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "world_tools.h"
#include "utilities.h"
#include "SystemComm.h"

#ifdef _C_EMULATOR_
#include "emulator.h"
#include "memory.h"
#else
#include "ivoryrep.h"
#endif
#include "spy.h"

#ifdef OS_OSF
#include <machine/fpu.h>
#else
#include <fenv.h>
#endif

#define MBToWords(MB) ((MB * 1024 * 1024) + 4)/5
#define WordsToMB(words) ((5 * words) + (1024 * 1024) - 1)/(1024 * 1024)

Boolean Trace = FALSE;
Boolean EnableIDS = FALSE;
Boolean TestFunction = FALSE;
static pthread_key_t mainThread;

static void MaybeTerminateVLM (int signal)
{
#ifdef OS_LINUX
  char *answer = NULL;
  size_t answerSize = 0, *answerSize_p = &answerSize;
  ssize_t nRead;
#else
  char answer[BUFSIZ];
#endif

  if (NULL == pthread_getspecific (mainThread))
    return;

  if (EmbCommAreaPtr->guestStatus > StartedGuestStatus)
    {
      if (RunningGuestStatus == EmbCommAreaPtr->guestStatus)
        fprintf (stderr, "\nLisp is running!\n\n");
      else
        fprintf (stderr, "\nLisp was running!\n\n");

      fprintf (stderr, "If you exit, the current state of Lisp will be lost.\n");
      fprintf (stderr, "All information in its memory image (e.g., any modified editor\n");
      fprintf (stderr, "buffers) will be irretrievably lost.  Further, Lisp will abandon\n");
      fprintf (stderr, "any tasks it is performing for its clients.\n\n");

      fprintf (stderr, "Do you still wish to exit?  (yes or no) ");
      fflush (stderr);

      while (TRUE)
        {
#ifdef OS_LINUX
          nRead = getline (&answer, answerSize_p, stdin);
          if (nRead < 0)
            vpunt (NULL, "Unexpected EOF on standard input");
          answer[nRead - 1] = '\0';
#else
          if (NULL == gets (answer))
            vpunt (NULL, "Unexpected EOF on standard input");
#endif
          if (0 == strcmp (answer, "yes"))
            break;
          else if (0 == strcmp (answer, "no"))
            return;
          else
            {
              fprintf (stderr, "Please answer 'yes' or 'no'.  ");
              fflush (stderr);
            }
        }
    }

  TerminateTracing ();
  TerminateSpy ();
  TerminateLifeSupport ();

  _exit (EXIT_SUCCESS);
}


int main (int argc, char** argv)
{
  VLMConfig config;
  struct sigaction sigAction;
  Integer worldImageSize, worldImageMB;
  char* message;
  int reason;

  BuildConfiguration (&config, argc, argv);
#ifdef GENERA
  EnableIDS = config.enableIDS;
#endif

  TestFunction = config.testFunction;
  Trace = config.tracing.tracePOST;
  InitializeIvoryProcessor (MapVirtualAddressData (0), MapVirtualAddressTag (0));

  Trace = config.tracing.traceP;
  if (Trace) InitializeTracing (config.tracing.bufferSize, config.tracing.startPC,
                                config.tracing.stopPC, config.tracing.outputFile);

  if (InitializeLifeSupport (&config) < 0) exit (-1);

#if defined(OS_OSF)
  ieee_set_fp_control(IEEE_TRAP_ENABLE_INV +
                      IEEE_TRAP_ENABLE_DZE +
                      IEEE_TRAP_ENABLE_OVF +
                      IEEE_TRAP_ENABLE_UNF +
                      IEEE_TRAP_ENABLE_INE);

#elif defined(OS_LINUX)
#ifdef FE_NOMASK_ENV
  fesetenv (FE_NOMASK_ENV);
#else
  feenableexcept (FE_INEXACT | FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW | FE_INVALID);
#endif

#elif defined(OS_DARWIN)
  /* TBD: -- Need an equivalent to: fesetenv (FE_NOMASK_ENV) */
#endif

  if (pthread_key_create (&mainThread, NULL))
    vpunt (NULL, "Unable to establish per-thread data.");

  pthread_setspecific (mainThread, (void*) TRUE);

  sigAction.sa_handler = (sa_handler_t)MaybeTerminateVLM;
  sigemptyset (&sigAction.sa_mask);
  sigAction.sa_flags = 0;
  if (sigaction (SIGINT, &sigAction, NULL))
    vpunt (NULL, "Unable to establish SIGINT handler.");
  if (sigaction (SIGTERM, &sigAction, NULL))
    vpunt (NULL, "Unable to establish SIGTERM handler.");
  if (sigaction (SIGHUP, &sigAction, NULL))
    vpunt (NULL, "Unable to establish SIGHUP handler.");
  if (sigaction (SIGQUIT, &sigAction, NULL))
    vpunt (NULL, "Unable to establish SIGQUIT handler.");

#ifdef IVERIFY
  EnsureVirtualAddressRange (0xF8000000L, 0x00100000L, FALSE);
#else
  worldImageSize = LoadWorld (&config);

#ifdef GENERA
  LoadVLMDebugger (&config);

  worldImageMB = WordsToMB (worldImageSize);
  if (worldImageMB > config.virtualMemory)
    vpunt (NULL, "World file %s won't fit within the requested virtual memory (%dMB)",
           config.worldPath, config.virtualMemory);
  if ((2 * worldImageMB) > config.virtualMemory)
    vwarn (NULL, "Only %dMB of virtual memory unused after loading world file %s\n",
           (config.virtualMemory - worldImageMB), config.worldPath);

  VirtualMemoryWrite (SystemCommSlotAddress (enableSysoutAtColdBoot),
                      EnableIDS ? processor->taddress : processor->niladdress);

  EmbCommAreaPtr->virtualMemorySize = MBToWords (config.virtualMemory);
  EmbCommAreaPtr->worldImageSize = worldImageSize;
#endif
#endif

  if (config.enableSpy) InitializeSpy (TRUE, config.diagnosticIPAddress.s_addr);

#ifdef AUTOSTART
  if (!IvoryProcessorSystemStartup (TRUE))
    vpunt (NULL, "Unable to start the VLM.");
#endif

  if (config.enableSpy) ReleaseSpyLock ();

  while (config.enableSpy ? TRUE : Runningp())
    {
      reason = InstructionSequencer ();
      if (reason)     
        {
          switch (reason)
            {
            case HaltReason_IllInstn:
              message = "Unimplemented instruction";
              break;
    
            case HaltReason_Halted:
              message = NULL;
              break;
              
            case HaltReason_SpyCalled:
              message = NULL;
              break;
              
            case HaltReason_FatalStackOverflow:
              message = "Stack overflow while not in emulator mode";
              break;
              
            case HaltReason_IllegalTrapVector:
              message = "Illegal trap vector contents";
              break;
              
            default:      
              message = "Halted for unknown reason";
            }
          if (message != NULL)
            vwarn (NULL, "%s at PC %08x (%s)", message, processor->epc >> 1,
                   (processor->epc & 1) ? "Odd" : "Even");
        }
#ifndef IVERIFY
      if (HaltReason_Halted == reason)
        break;
#endif
    }

  exit (EXIT_SUCCESS);
}
