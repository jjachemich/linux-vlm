/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Console Life Support -- EXPLANATION */
#define _GNU_SOURCE

#include "config.h"
#include "std.h"

#include <netdb.h>
#include <X11/Xlibint.h>
#include <X11/Xproto.h>
// jj
#if HAVE_X11_XLIB_XCB_H
#include <X11/Xlib-xcb.h>
#else
#error need Xlib-xcb.h to compile
#endif
#if HAVE_DLFCN_H
#include <dlfcn.h>
#else
#error need dlfcn.h to compile
#endif
// jj
#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "memory.h"
#include "utilities.h"


/* Create the console channel */

void InitializeConsoleChannel (VLMConfig* config)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbConsoleChannel));
  register EmbConsoleChannel *p = (EmbConsoleChannel*) HostPointer (cp);
	
	p->type = EmbConsoleChannelType;
	p->unit = 0;
	p->next = EmbCommAreaPtr->channel_table;	/* Thread into list of all channels */
	EmbCommAreaPtr->channel_table = cp;
	EmbCommAreaPtr->consoleChannel = cp;		/* Make it easy to find */

	p->outputRequestQueue = CreateQueue (ConsoleOutputQueueSize, sizeof (EmbPtr));
	p->outputRequestQ = (EmbQueue*) HostPointer (p->outputRequestQueue);
	p->outputRequestQ->signal = InstallSignalHandler((ProcPtrV)&ConsoleOutput, (PtrV)p, FALSE);
	p->outputReplyQueue = CreateQueue (ConsoleOutputQueueSize, sizeof (EmbPtr));
	p->outputReplyQ = (EmbQueue*) HostPointer (p->outputReplyQueue);

	p->inputRequestQueue = CreateQueue (ConsoleInputQueueSize, sizeof (EmbPtr));
	p->inputRequestQ = (EmbQueue*) HostPointer (p->inputRequestQueue);
	p->inputRequestQ->signal = InstallSignalHandler ((ProcPtrV) &ConsoleInput, (PtrV) p, TRUE);
	p->inputReplyQueue = CreateQueue (ConsoleInputQueueSize, sizeof (EmbPtr));
	p->inputReplyQ = (EmbQueue*) HostPointer (p->inputReplyQueue);

	p->hostName = config->generaXParams.xpHostName;
	p->hostAddress = htonl (config->generaXParams.xpHostAddress);
	p->displayNumber = config->generaXParams.xpDisplay;
	p->screenNumber = config->generaXParams.xpScreen;
    p->initialState = config->generaXParams.xpInitialState;
	p->geometry = MakeEmbString (config->generaXParams.xpGeometry);
	p->foregroundColor = MakeEmbString (config->generaXParams.xpForegroundColor);
	p->backgroundColor = MakeEmbString (config->generaXParams.xpBackgroundColor);
	p->borderColor = MakeEmbString (config->generaXParams.xpBorderColor);
	p->borderWidth = config->generaXParams.xpBorderWidth;

	p->display = NULL;
	p->openingState = OpeningStateNone;
	p->rlDisplay = NULL;

	if (pthread_create (&p->drawRunLights, &EmbCommAreaPtr->pollThreadAttrs,
					    (pthread_startroutine_t) &DrawRunLights, p))
		vpunt (NULL, "Unable to create the console channel polling thread");
	p->drawRunLightsSetup = TRUE;
}


/* Do console I/O -- Available as a coprocessor call */

void DoConsoleIO (EmbConsoleChannel* consoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleBuffer* command = pCommand;

	switch (command->opcode)
	{
	  case EmbConsoleCommandOpenDisplay:
		command->result = OpenDisplay (consoleChannel, command);
		break;

	  case EmbConsoleCommandCloseDisplay:
		CloseDisplay (consoleChannel);
		command->result = ESUCCESS;
		break;

	  case EmbConsoleCommandNoOp:
		command->result = ESUCCESS;
		break;

	  case EmbConsoleCommandWrite:
		if (OpeningStatePrefix == consoleChannel->openingState)
			command->result = ESUCCESS;
		else
			command->result = ConsoleWrite (consoleChannel, command);
		break;

	  case EmbConsoleCommandRead:
		if (consoleChannel->openingState != OpeningStateNone)
			command->result = ProcessConnectionRequest (consoleChannel, command);
		else
			command->result = ConsoleRead (consoleChannel, command);
		break;

	  case EmbConsoleCommandInputWait:
		if (consoleChannel->openingState != OpeningStateNone)
		  {
			((EmbConsoleInputWait*)&command->data[0])->availableP = TRUE;
			command->result = ESUCCESS;
		  }
		else
			command->result = ConsoleInputWait (consoleChannel, command);
		break;

	  case EmbConsoleCommandEnableRunLights:
		EnableRunLights (consoleChannel, command);
		command->result = ESUCCESS;
		break;

	  case EmbConsoleCommandDisableRunLights:
		DisableRunLights (consoleChannel);
		command->result = ESUCCESS;
		break;
	}
}


/* Process requests from the VLM */

void ConsoleDriver (EmbConsoleChannel* consoleChannel,
						   EmbQueue* pRequestQueue, EmbQueue* pReplyQueue)
{
  register EmbQueue* requestQueue = pRequestQueue;
  register EmbQueue* replyQueue = pReplyQueue;
  register EmbConsoleBuffer* command;
  EmbPtr commandPtr;

	while (EmbQueueFilled (requestQueue))
	  {
		if (0 == EmbQueueSpace (replyQueue))
		  {
			/* Can't do I/O now -- Ask to be invoked again on the next "clock tick" */
			SignalLater (requestQueue->signal);
			return;
		  }

		commandPtr = EmbQueueTakeWord (requestQueue);
		if (commandPtr)
		  {
			command = (EmbConsoleBuffer*) HostPointer (commandPtr);
			DoConsoleIO (consoleChannel, command);
			EmbQueuePutWord (replyQueue, commandPtr);
		  }
	  }
}


/* Actual signal handlers for the output and input request queues */

void ConsoleOutput (EmbConsoleChannel* consoleChannel)
{
	ConsoleDriver (consoleChannel, 
				   consoleChannel->outputRequestQ, consoleChannel->outputReplyQ);
}

void ConsoleInput (EmbConsoleChannel* consoleChannel)
{
	ConsoleDriver (consoleChannel, consoleChannel->inputRequestQ, consoleChannel->inputReplyQ);
}


/* Open the display */

int OpenDisplay (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  register EmbConsoleOpenDisplay* openDisplay = (EmbConsoleOpenDisplay*) &command->data[0];
  char displayName[BUFSIZ];
  int result;

	if (consoleChannel->display != NULL)
		return (EBUSY);

	BuildXDisplayName (displayName,
			   consoleChannel->hostName,
			   consoleChannel->displayNumber,
			   consoleChannel->screenNumber);

	begin_MUTEX_LOCKED (XLock);

	consoleChannel->display = XOpenDisplay (displayName);

	if (consoleChannel->display)
	  {
		consoleChannel->fd = XConnectionNumber ((Display*) consoleChannel->display);
		consoleChannel->openingState = OpeningStatePrefix;
		openDisplay->lastRequestNumber = ((struct _XDisplay*)consoleChannel->display)->request;
		result = ESUCCESS;
	  }

	else
	  {
		result = errno;
		switch (result)
		{
		  case ESUCCESS:
			result = ECONNREFUSED;
			break;
		  case EWOULDBLOCK:
			result = ENXIO;
			break;
		}
	  }

	end_MUTEX_LOCKED (XLock);

	return (result);
}


/* Process the individual parts of a connection request -- When opening a display, Lisp 
   will send a connection setup request.  Because X doesn't permit said request to be
   issued twice, we intercept it and return the data that it would have returned as derived
   from internal data structures.  A finite-state machine is used to determine what
   piece of data will be returned next */

int ProcessConnectionRequest (EmbConsoleChannel* pConsoleChannel, 
									 EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  EmbConsoleDataTransfer* dataTransfer = (EmbConsoleDataTransfer*) &command->data[0];
  register struct _XDisplay* display = (struct _XDisplay*) consoleChannel->display;
  char* data;
  xConnSetupPrefix setupPrefix;
  xConnSetup setup;
  xPixmapFormat pixmapFormat;
  ScreenFormat* screenFormat;
  xWindowRoot windowRoot;
  Screen* screen;
  xDepth pDepth;
  Depth* depth;
  xVisualType visualType;
  Visual* visual;

	data = (char*) MapVirtualAddressData (dataTransfer->address);
	data += dataTransfer->offset;

	switch (consoleChannel->openingState)
	{
	  case OpeningStatePrefix:
		setupPrefix.success = TRUE;
		setupPrefix.lengthReason = 0;
		setupPrefix.majorVersion = display->proto_major_version;
		setupPrefix.minorVersion = display->proto_minor_version;
		setupPrefix.length = 0;				/* Genera ignores it */
		memcpy (data, &setupPrefix, sizeof (xConnSetupPrefix));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStateHeader:
		setup.release = display->release;
		setup.ridBase = display->resource_base;
		setup.ridMask = display->resource_mask;
		setup.motionBufferSize = display->motion_buffer;
		setup.nbytesVendor = strlen (display->vendor);
		setup.maxRequestSize = display->max_request_size;
		setup.numRoots = display->nscreens;
		setup.numFormats = display->nformats;
		setup.imageByteOrder = display->byte_order;
		setup.bitmapBitOrder = display->bitmap_bit_order;
		setup.bitmapScanlineUnit = display->bitmap_unit;
		setup.bitmapScanlinePad = display->bitmap_pad;
		setup.minKeyCode = display->min_keycode;
		setup.maxKeyCode = display->max_keycode;
		memcpy (data, &setup, sizeof (xConnSetup));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStateVendor:
		memcpy (data, display->vendor, strlen (display->vendor));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStatePixmapFormat:
		screenFormat = &display->pixmap_format[consoleChannel->nextPixmapFormat];
		pixmapFormat.depth = screenFormat->depth;
		pixmapFormat.bitsPerPixel = screenFormat->bits_per_pixel;
		pixmapFormat.scanLinePad = screenFormat->scanline_pad;
		memcpy (data, &pixmapFormat, sizeof (xPixmapFormat));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStateRoot:
		screen = &display->screens[consoleChannel->nextRoot];
		windowRoot.windowId = screen->root;
		windowRoot.defaultColormap = screen->cmap;
		windowRoot.whitePixel = screen->white_pixel;
		windowRoot.blackPixel = screen->black_pixel;
		windowRoot.currentInputMask = screen->root_input_mask;
		windowRoot.pixWidth = screen->width;
		windowRoot.pixHeight = screen->height;
		windowRoot.mmWidth = screen->mwidth;
		windowRoot.mmHeight = screen->mheight;
		windowRoot.minInstalledMaps = screen->min_maps;
		windowRoot.maxInstalledMaps = screen->max_maps;
		windowRoot.rootVisualID = screen->root_visual->visualid;
		windowRoot.backingStore = screen->backing_store;
		windowRoot.saveUnders = screen->save_unders;
		windowRoot.rootDepth = screen->root_depth;
		windowRoot.nDepths = screen->ndepths;
		memcpy (data, &windowRoot, sizeof (xWindowRoot));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStateRootDepth:
		screen = &display->screens[consoleChannel->nextRoot];
		depth = &screen->depths[consoleChannel->nextRootDepth];
		pDepth.depth = depth->depth;
		pDepth.nVisuals = depth->nvisuals;
		memcpy (data, &pDepth, sizeof (xDepth));
		AdvanceOpeningState (consoleChannel);
		break;

	  case OpeningStateRootDepthVisual:
		screen = &display->screens[consoleChannel->nextRoot];
		depth = &screen->depths[consoleChannel->nextRootDepth];
		visual = &depth->visuals[consoleChannel->nextRootDepthVisual];
		visualType.visualID = visual->visualid;
		visualType.class = visual->class;
		visualType.bitsPerRGB = visual->bits_per_rgb;
		visualType.colormapEntries = visual->map_entries;
		visualType.redMask = visual->red_mask;
		visualType.greenMask = visual->green_mask;
		visualType.blueMask = visual->blue_mask;
		memcpy (data, &visualType, sizeof (xVisualType));
		AdvanceOpeningState (consoleChannel);
		break;
	}

	return (ESUCCESS);
}


/* Advance to the next appropriate state of connection request processing */

void AdvanceOpeningState (EmbConsoleChannel* pConsoleChannel)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register struct _XDisplay* display = (struct _XDisplay*) consoleChannel->display;
  Screen* screen;
  Depth* depth;

	switch (consoleChannel->openingState)
	{
	  case OpeningStatePrefix:
		consoleChannel->openingState = OpeningStateHeader;
		break;

	  case OpeningStateHeader:
		consoleChannel->openingState = OpeningStateVendor;
		break;

	  case OpeningStateVendor:
		if (display->nformats > 0)
		  {
			consoleChannel->openingState = OpeningStatePixmapFormat;
			consoleChannel->nextPixmapFormat = 0;
		  }
		else if (display->nscreens > 0)
		  {
			consoleChannel->openingState = OpeningStateRoot;
			consoleChannel->nextRoot = 0;
		  }
		else
			consoleChannel->openingState = OpeningStateNone;
		break;

	  case OpeningStatePixmapFormat:
		consoleChannel->nextPixmapFormat++;
		if (consoleChannel->nextPixmapFormat >= display->nformats) {
			if (display->nscreens > 0)
			  {
				consoleChannel->openingState = OpeningStateRoot;
				consoleChannel->nextRoot = 0;
			  }
			else
				consoleChannel->openingState = OpeningStateNone;
		}
		break;

	  case OpeningStateRoot:
		screen = &display->screens[consoleChannel->nextRoot];
		if (screen->ndepths > 0)
		  {
			consoleChannel->openingState = OpeningStateRootDepth;
			consoleChannel->nextRootDepth = 0;
		  }
		else
		  {
			consoleChannel->nextRoot++;
			if (consoleChannel->nextRoot >= display->nscreens)
				consoleChannel->openingState = OpeningStateNone;
		  }
		break;

	  case OpeningStateRootDepth:
		screen = &display->screens[consoleChannel->nextRoot];
		depth = &screen->depths[consoleChannel->nextRootDepth];
		if (depth->nvisuals > 0)
		  {
			consoleChannel->openingState = OpeningStateRootDepthVisual;
			consoleChannel->nextRootDepthVisual = 0;
		  }
		else
		  {
			consoleChannel->nextRootDepth++;
			if (consoleChannel->nextRootDepth >= screen->ndepths)
			  {
				consoleChannel->nextRoot++;
				if (consoleChannel->nextRoot >= display->nscreens)
					consoleChannel->openingState = OpeningStateNone;
			  }
		  }
		break;

	  case OpeningStateRootDepthVisual:
		screen = &display->screens[consoleChannel->nextRoot];
		depth = &screen->depths[consoleChannel->nextRootDepth];
		consoleChannel->nextRootDepthVisual++;
		if (consoleChannel->nextRootDepthVisual >= depth->nvisuals)
		  {
			consoleChannel->nextRootDepth++;
			if (consoleChannel->nextRootDepth >= screen->ndepths)
			  {
				consoleChannel->nextRoot++;
				if (consoleChannel->nextRoot >= display->nscreens)
					consoleChannel->openingState = OpeningStateNone;
					else
					consoleChannel->openingState = OpeningStateRoot;
				}
			else
			  {
			consoleChannel->openingState = OpeningStateRootDepth;
				}
		  }
		break;
	}
}


/* Write data to the server */

int ConsoleWrite (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  register EmbConsoleDataTransfer* dataTransfer = (EmbConsoleDataTransfer*) &command->data[0];
  struct pollfd pollDisplay;
  char* data;
  ssize_t nBytes, actualBytes;
  int result;

	data = (char*) MapVirtualAddressData (dataTransfer->address);
	data += dataTransfer->offset;
	nBytes = dataTransfer->nBytes;

	result = EWOULDBLOCK;
	pollDisplay.fd = consoleChannel->fd;
	pollDisplay.events = POLLOUT;

	while (EWOULDBLOCK == result)
	  {
		pthread_testcancel ();

		pollDisplay.revents = 0;
		poll (&pollDisplay, 1, 1000);

		if (pollDisplay.revents & POLLOUT)
		  {
			actualBytes = write (consoleChannel->fd, data, nBytes);
			if (actualBytes == nBytes)
				result = ESUCCESS;
			else
			  {
				/* Might be a partial write */
				result = (actualBytes < 0) ? errno : EWOULDBLOCK;
				nBytes -= (actualBytes < 0) ? 0 : actualBytes;
				data += (actualBytes < 0) ? 0 : actualBytes;
			  }
		  }

		else if (pollDisplay.revents & POLLNVAL)
			result = EBADF;

		else if (pollDisplay.revents & POLLHUP)
			result = ENXIO;

		else if (pollDisplay.revents & POLLERR)
			result = EIO;
	  }

	return (result);
}


/* Read data from the server */

int ConsoleRead (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  register EmbConsoleDataTransfer* dataTransfer = (EmbConsoleDataTransfer*) &command->data[0];
  struct pollfd pollDisplay;
  char* data;
  ssize_t nBytes, actualBytes;
  int result;

	data = (char*) MapVirtualAddressData (dataTransfer->address);
	data += dataTransfer->offset;
	nBytes = dataTransfer->nBytes;

	result = EWOULDBLOCK;
	pollDisplay.fd = consoleChannel->fd;
	pollDisplay.events = POLLIN;

	while (EWOULDBLOCK == result)
	  {
		pthread_testcancel ();

		pollDisplay.revents = 0;
		poll (&pollDisplay, 1, 1000);

		if (pollDisplay.revents & POLLIN)
		  {
			actualBytes = read (consoleChannel->fd, data, nBytes);
			if (actualBytes == nBytes)
				result = ESUCCESS;
			else if ((0 == actualBytes) && (EWOULDBLOCK != errno))
				result = ENOSPC;				/* End-of-File */
			else
			  {
				/* Might be a partial read */
				result = (actualBytes < 0) ? errno : EWOULDBLOCK;
				nBytes -= (actualBytes < 0) ? 0 : actualBytes;
				data += (actualBytes < 0) ? 0 : actualBytes;
			  }
		  }

		else if (pollDisplay.revents & POLLNVAL)
			result = EBADF;

		else if (pollDisplay.revents & POLLHUP)
			result = ENXIO;

		else if (pollDisplay.revents & POLLERR)
			result = EIO;
	  }

	return (result);
}


/* Check if input is available in response to a CoprocessorRead by the VLM */

boolean ConsoleInputAvailableP ()
{
  EmbConsoleChannel* consoleChannel = (EmbConsoleChannel*) HostPointer (EmbCommAreaPtr->consoleChannel);
  struct pollfd pollDisplay;

	if (NULL == consoleChannel->display)
		return (FALSE);

	else if (consoleChannel->openingState != OpeningStateNone)
		return (TRUE);
	
	pollDisplay.fd = consoleChannel->fd;
	pollDisplay.events = POLLIN;
	pollDisplay.revents = 0;
	poll (&pollDisplay, 1, 0);
	return ((pollDisplay.revents & POLLIN) != 0);
}


/* Wait until data is available from the server */

int ConsoleInputWait (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  register EmbConsoleInputWait* inputWait = (EmbConsoleInputWait*) &command->data[0];
  struct pollfd pollDisplay;
  int result;

	pollDisplay.fd = consoleChannel->fd;
	pollDisplay.events = POLLIN;
	pollDisplay.revents = 0;

	result = poll (&pollDisplay, 1, inputWait->timeout);

	if (0 == result)
	  {
		result = ESUCCESS;
		inputWait->availableP = FALSE;
	  }

	else if (pollDisplay.revents & POLLIN)
	  {
		result = ESUCCESS;
		inputWait->availableP = TRUE;
	  }

	else if (pollDisplay.revents & POLLNVAL)
		result = EBADF;

	else if (pollDisplay.revents & POLLHUP)
		result = ENXIO;

	else if (pollDisplay.revents & POLLERR)
		result = EIO;

	return (result);
}


/* Close the display if open */
// jj
extern enum xcbvals { XcbUnknown, XcbLoaded, XcbNotLoaded } haveXcb ;
// jj
void CloseDisplay (EmbConsoleChannel* consoleChannel)
{
  static void *handle ;
  static void (*xcbflush)(xcb_connection_t *conn) ;
  static void (*xcbdisconnect)(xcb_connection_t *conn);
  static xcb_connection_t* (*xgetxcbconnection)(Display *dpy);

	DisableRunLights (consoleChannel);

	if (consoleChannel->display != NULL)
	  {
		begin_MUTEX_LOCKED (XLock);
		// jj
		handle = dlopen("libX11-xcb.so.1", RTLD_LAZY );
//		fprintf(stderr,"handle =%p\n", handle);
		if (haveXcb == XcbLoaded) {
		  xcb_connection_t *c ;

		  if (xgetxcbconnection == NULL) 
			  xgetxcbconnection = dlsym( handle, "XGetXCBConnection");
		  if (xcbflush == NULL) xcbflush = dlsym( RTLD_NEXT, "xcb_flush");
		  if (xcbdisconnect == NULL) 
			  xcbdisconnect = dlsym( RTLD_NEXT, "xcb_disconnect");

//		  fprintf(stderr,"xgetxcbconnection = %p\n",xgetxcbconnection);
		  c = xgetxcbconnection(consoleChannel->display);
		  xcbflush(c);
		  xcbdisconnect(c);
		} else 	XCloseDisplay ((Display*) consoleChannel->display);
		// jj
		consoleChannel->display = NULL;
		end_MUTEX_LOCKED (XLock);
	  }
}


/* Enable drawing of run lights */

void EnableRunLights (EmbConsoleChannel* pConsoleChannel, EmbConsoleBuffer* pCommand)
{
  register EmbConsoleChannel* consoleChannel = pConsoleChannel;
  register EmbConsoleBuffer* command = pCommand;
  EmbConsoleRunLights* runLights = (EmbConsoleRunLights*) &command->data[0];
  char displayName[BUFSIZ];
  XGCValues gcValues;

	BuildXDisplayName (displayName, consoleChannel->hostName, consoleChannel->displayNumber,
					   consoleChannel->screenNumber);

	begin_MUTEX_LOCKED (XLock);

	consoleChannel->rlDisplay = XOpenDisplay (displayName);

	if (NULL != consoleChannel->rlDisplay)
	  {
		consoleChannel->rlGC = malloc (sizeof (GC));

		if (NULL != consoleChannel->rlGC)
		  {
			memcpy (&consoleChannel->runLights, runLights, sizeof (EmbConsoleRunLights));

			gcValues.foreground = consoleChannel->runLights.lightForeground;
			gcValues.background = consoleChannel->runLights.lightBackground;
			gcValues.plane_mask = consoleChannel->runLights.lightPlaneMask;
			*(GC*) consoleChannel->rlGC = XCreateGC (consoleChannel->rlDisplay, 
													 consoleChannel->runLights.windowID,
													 (GCForeground | GCBackground | GCPlaneMask),
													 &gcValues);
		  }
	  }

	end_MUTEX_LOCKED (XLock);

	consoleChannel->lastRunLights = 0;
}


/* Periodically update the run lights, if enabled */

#define OneOneHundrethSecond 10000000L

void DrawRunLights (pthread_addr_t argument)
{
  register EmbConsoleChannel* consoleChannel = (EmbConsoleChannel*) argument;
  pthread_t self = pthread_self ();
  struct timespec drlSleep;
  int changed, i, bit, x;

  //	pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);

	WaitUntilInitializationComplete ();

	drlSleep.tv_sec = 0;
	drlSleep.tv_nsec = 10 * OneOneHundrethSecond;		/* 10 Hz */

	while (TRUE)
	  {
	    begin_MUTEX_LOCKED (XLock);
	    if (consoleChannel->rlDisplay != NULL)
		  {
			changed = consoleChannel->lastRunLights ^ EmbCommAreaPtr->run_lights;
			consoleChannel->lastRunLights = EmbCommAreaPtr->run_lights;
			x = consoleChannel->runLights.firstLightX;
		
			for (i = 0, bit = 1, x = consoleChannel->runLights.firstLightX;
				 i < consoleChannel->runLights.nLights; 
				 i++, bit = bit << 1, x += consoleChannel->runLights.lightXSpacing)
				if (changed & bit) {
					if (consoleChannel->lastRunLights & bit)
						XFillRectangle (consoleChannel->rlDisplay,
									    consoleChannel->runLights.windowID,
										*(GC*) consoleChannel->rlGC, x, 
										consoleChannel->runLights.firstLightY, 
										consoleChannel->runLights.lightWidth,
										consoleChannel->runLights.lightHeight);
					else
						XClearArea (consoleChannel->rlDisplay, 
								    consoleChannel->runLights.windowID, x,
									consoleChannel->runLights.firstLightY,
									consoleChannel->runLights.lightWidth, 
									consoleChannel->runLights.lightHeight, FALSE);
				}
			XFlush (consoleChannel->rlDisplay);

		  }
	    end_MUTEX_LOCKED (XLock);

	    if (pthread_delay_np (&drlSleep))
	      vpunt (NULL, "Unable to sleep in thread %lx", self);
	  }

	//	pthread_cleanup_pop (TRUE);
}


/* Disable drawing of run lights */

void DisableRunLights (EmbConsoleChannel* consoleChannel)
{
	begin_MUTEX_LOCKED (XLock);

	if (consoleChannel->rlGC != NULL)
	  {
		free (consoleChannel->rlGC);
		consoleChannel->rlGC = NULL;
	  }

	if (consoleChannel->rlDisplay != NULL)
	  {
		XCloseDisplay ((Display*) consoleChannel->rlDisplay);
		consoleChannel->rlDisplay = NULL;
	  }

	end_MUTEX_LOCKED (XLock);
}


/* Reset the console channel */

void ResetConsoleChannel (EmbChannel* channel)
{
  register EmbConsoleChannel* consoleChannel = (EmbConsoleChannel*) channel;

	ResetIncomingQueue (consoleChannel->outputRequestQ);
	ResetOutgoingQueue (consoleChannel->outputReplyQ);
	ResetIncomingQueue (consoleChannel->inputRequestQ);
	ResetOutgoingQueue (consoleChannel->inputReplyQ);
	CloseDisplay (consoleChannel);
}


/* Cleanup the console channel */

void TerminateConsoleChannel (void)
{
  void *exit_value;
  register EmbConsoleChannel* consoleChannel;

	if (NullEmbPtr == EmbCommAreaPtr->consoleChannel)
		return;
	else
		consoleChannel = (EmbConsoleChannel*) HostPointer (EmbCommAreaPtr->consoleChannel);

	if (consoleChannel->drawRunLightsSetup)
	  {
		pthread_cancel (consoleChannel->drawRunLights);
		pthread_join (consoleChannel->drawRunLights, &exit_value);
		consoleChannel->drawRunLightsSetup = FALSE;
	  }

	CloseDisplay (consoleChannel);
}
