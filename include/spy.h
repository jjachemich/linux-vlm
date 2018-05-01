/* -*- Mode: C; Tab-Width: 4 -*- */

/* Function prototypes for the external interface to the Remote Debugger spy */

#ifndef _SPY_
#define _SPY_

#include "life_types.h"
#include "embed.h"

extern EmbMBINChannel	*activeMBINChannel;

void	InitializeSpy (boolean sendTrapP, unsigned long diagnosticAddress);
void	ReleaseSpyLock (void);
void	SendMBINBuffers (EmbMBINChannel* mbinChannel);
void	TerminateSpy (void);

void		RemoteMemorySpyLoop (void);

#endif
