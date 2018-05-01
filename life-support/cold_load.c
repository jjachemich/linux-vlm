/* -*- Mode: C; -*- */

/* VLM's Cold Load Window implementation */
// jj
#define _GNU_SOURCE
// jj
#include "config.h"
#include "std.h"

#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
// jj
#include <X11/X.h>
#if HAVE_X11_XLIB_XCB_H
#include <X11/Xlib-xcb.h>
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
#include "utilities.h"
#include "symbolics_characters.h"
#include "genera-icon-32.xbm"
#include "genera-cptfont.xbm"

int manage_run_lights = 0;
int run_lights_state;


#include <X11/Xproto.h>
#include <X11/Xutil.h>
#include "cold_load_keymappings.h"

#define RUN_LIGHT_Y_SPACE 3	/* Pixels to leave for run bars in cold-load window */
#define RUN_LIGHT_Y_OFFSET (RUN_LIGHT_Y_SPACE-1)

/* These are closely related to pixels_per_run_light as computed in alloc_screen_array,
   but any values smaller than the below (as tend to be computed) look crappy. */
#define RUN_LIGHT_WIDTH 32
#define RUN_LIGHT_SPACING 40

#define DISK_RUN_LIGHT 2
#define PROCESS_RUN_LIGHT 3
#define NETWORK_RUN_LIGHT 5
#define NETBOOT_PROGRESS_BAR 7

static EmbColdLoadChannel *cold_channel = NULL;
static EmbQueue *keyboard_queue = NULL, *display_queue = NULL;

static Display *display = NULL;
static KeySym *orig_meta, *orig_hyper ;
static int ks_p_kc_meta, ks_p_kc_hyper ;
static Screen *screen;
static Visual *visual;
static Window window, icon_window, root;
static Colormap colormap;
static GC gc, icon_gc, icon_gc_s, icon_gc_c, icon_gc_t;
static Pixmap icon_bitmap = 0, cptfont_bitmap = 0;
static XModifierKeymap *originalModmap = NULL;
static int icon_width = 32, icon_height = 36;
static int char_width, char_height, width = 0, height = 0;
static int loff, toff, roff, boff;
static int lmarg = 3, tmarg = 22, rmarg = 3, bmarg = 3;
static int current_x = 0, current_y = 0;
static int cursor_visible = 0, cursor_frozen = 0, cursor_state = 0, light_state = 0;
static int visibility = 0, icon_visibility = 0;
static int run_light_y, run_light_first_x, run_label_y;
static int progress_bar_first_x, progress_bar_width, run_label_width, run_label_height;
static int progress_bar_numerator_state = 0, progress_bar_denominator_state = 0;
static int progress_bar_length_state = 0, progress_label_length;
static char *progress_label = NULL;
static int meta_mask = 0, super_mask = 0, hyper_mask = 0, isol3_mask = 0, isol3_code = 0;
/* static jmp_buf x_io_error; */
static int did_show = 0;

typedef struct {
  int length;
  char *chars;
} line;

static line *screen_array = NULL;
static enum KeyboardType keyboardType = Unknown;
static coldmapentry *skMap = NULL;
static short *fkMap = NULL;
static int removeNumLockModifier = 0;
// jj
enum xcbvals { XcbUnknown, XcbLoaded, XcbNotLoaded } haveXcb = XcbUnknown;

/* Internal function prototypes -- Here to avoid including X headers everywhere */

static void   alloc_screen_array (int new_width_pixels, int new_height_pixels);
static void   ColdLoadInput (pthread_addr_t argument);
static void   ColdLoadOutput (void* ignored);
/* static int    ColdXErrorHandler (Display *display, XErrorEvent *error); */
static void   close_display (void);
/* static void   close_display_child_hook (void); */
/* static void   close_run_lights_display (void); */
static int    do_modifier (XModifierKeymap **modmapp, int *changedp, 
			   KeyCode code1, KeyCode code2, KeyCode code3);
static int    find_modifier (XModifierKeymap *modmap, KeyCode code);
static int    find_unused_modifier (XModifierKeymap **modmapp);
static void   get_keyboard_modifier_codes (int for_real_p,
					   KeyCode *control_l_code, KeyCode *control_r_code,
					   KeyCode *meta_l_code, KeyCode *meta_r_code,
					   KeyCode *alt_l_code, KeyCode *super_code,
					   KeyCode *hyper_code);
static void   handle_input (void);
/* static void   handle_output (void); */
static void   handle_output_command (uEmbWord command);
static void   hide_cursor (void);
/* static int    initialize_cold (XParams *cl_params, boolean noWaiting); */
static void   manage_cold_load_output (void);
static int    manage_x_input (XParams *params);
static int    mask_to_modifier (int mask);
static int    open_cold_load_display (XParams *params, boolean noWaiting);
/* static void   open_run_lights_display (XParams *params, Window window_id, int nlights, */
/* 				       unsigned int width, unsigned int height,  */
/* 				       unsigned int x, unsigned int y, */
/* 				       unsigned int dx, unsigned int dy, */
/* 				       unsigned int foreground, unsigned int background, */
/* 				       unsigned int plane_mask); */
static int    open_display (XParams *params, boolean noWaiting);
static void   redisplay_line (int y, int x, int xlim);
static void   redisplay_screen_array (int minx, int miny, int maxx, int maxy);
static void   replay_command_history (void);
static void   reset_light_state (int screen_cleared_p);
static void   SetColdLoadNames (void);
/* static void   SetColdXErrorHandler (void); */
static void   SetupColdLoadNameStrings (VLMConfig* config);
static int    setup_modifier_mapping (void);
static int    setup_x_io_error_handler (void);
static void   show_cursor_internal (int new_state);
static void   show_icon (void);
static void   show_lights (int force);
static void   stop_cold_x (void);
/* static void   update_cold_load_blinkers (void); */
/* static void   update_cold_load_run_lights (void); */
static void   make_map (int for_real_p);

#define show_cursor() show_cursor_internal (EmbCommAreaPtr->fep.cursor)


static XModifierKeymap *remove_modifier ( Display *display, KeySym keysym, XModifierKeymap *map ) {
	XModifierKeymap *newmap ;
	int code = XKeysymToKeycode (display, keysym );
	int i,k=0, found=0, mpm = map->max_keypermod ;
	for (i=0;i<8;i++) {
		for(k=0;k<mpm;k++)
			if (map->modifiermap[i*mpm+k] == code) { found = 1; break; }
		if (found) break ;
	}
	if (found) {
		newmap = XDeleteModifiermapEntry ( map, code, i );
		if (XSetModifierMapping ( display, newmap ) != MappingSuccess)
			vwarn ("cold load","Unable to install X modifier keymap");
		return newmap ;
	}
	return NULL ;
}

static void make_map (int for_real_p)
{
	KeySym meta_keysym[] = { XK_Meta_L, XK_Alt_L };
	KeySym hyper_keysym[] = { XK_Hyper_R, XK_Menu } ;
	XModifierKeymap *newmap=NULL;
	KeyCode control_l_code, control_r_code, meta_l_code, meta_r_code,
		alt_l_code, super_code, hyper_code;

	get_keyboard_modifier_codes(for_real_p,
				    &control_l_code,
				    &control_r_code,
				    &meta_l_code, 
				    &meta_r_code,
				    &alt_l_code, 
				    &super_code,
				    &hyper_code);

	// always put Meta on Alt (if we have an alt key)
	if (XKeysymToKeycode(display,XK_Alt_L)) {
		orig_meta = XGetKeyboardMapping ( display,
						  XKeysymToKeycode(display,
								   XK_Alt_L),
						  1,
						  &ks_p_kc_meta );
	}
	// if no Hyper key available, put it on Menu (if there is one)
	if (XKeysymToKeycode(display,XK_Menu)) {
		orig_hyper = XGetKeyboardMapping ( display,
						   XKeysymToKeycode(display,
								    XK_Menu),
						   1,
						   &ks_p_kc_hyper );
		XChangeKeyboardMapping(display,
				       XKeysymToKeycode(display,XK_Menu),
				       sizeof(hyper_keysym)/sizeof(KeySym),
				       hyper_keysym,
				       1);
		if (for_real_p) vwarn("cold load",
				      "Your Hyper(_R) key now is on (right) Menu");
	}
	// always put Meta on Alt
	if (XKeysymToKeycode(display,XK_Alt_L)) {
		XChangeKeyboardMapping(display,
				       XKeysymToKeycode(display,XK_Alt_L),
				       sizeof(meta_keysym)/sizeof(KeySym),
				       meta_keysym,
				       1);
		if (for_real_p) vwarn("cold load",
				      "Your Meta key now is on ALT");
	}
	// remove non-functional Hyper_L key
	newmap = remove_modifier ( display, XK_Hyper_L, 
				   newmap ? 
				   newmap : 
				   XGetModifierMapping(display) );
	if (for_real_p) 
		vwarn("cold load",
		      "NumLock will not be functional in genera");
}

static int open_cold_load_display (XParams *params, boolean noWaiting)
{
	open_display(params, noWaiting);
	if (display != NULL) {
		replay_command_history();
		return(XConnectionNumber(display));
	} else
		return(-1);
}


static int manage_x_input (XParams *params)
{
  while (display != NULL && XPending(display)) {
    handle_input();
  }
  return(display == NULL ? -1 : XConnectionNumber(display));
}


static void manage_cold_load_output ()
{
  uEmbWord command;
  
  begin_MUTEX_LOCKED (XLock);
  hide_cursor();
  end_MUTEX_LOCKED (XLock);

/* again: */
  while (EmbQueueFilled(display_queue)) {
    command = (uEmbWord)EmbQueueTakeWord(display_queue);
    cold_channel->command_history[cold_channel->command_history_top++] = command;
    if (cold_channel->command_history_top == ColdLoadCommandHistorySize) {
      cold_channel->command_history_top = 0;
      cold_channel->command_history_wrapped = TRUE;
    }
    begin_MUTEX_LOCKED (XLock);
    handle_output_command(command);
    end_MUTEX_LOCKED (XLock);
  }
  /* pthread_yield(); */
  /* pthread_testcancel(); */
  /* goto again; */

  begin_MUTEX_LOCKED (XLock);
  show_cursor ();
  end_MUTEX_LOCKED (XLock);

  return ;
}

/* static void update_cold_load_blinkers () */
/* { */
/*   show_cursor(); */
/*   XFlush(display); */
/* } */

/* static void update_cold_load_run_lights () */
/* { */
/*   show_lights(0); */
/*   XFlush(display); */
/* } */

static int setup_x_io_error_handler ()
{
  /* //  return(_setjmp(x_io_error)); */
  return 0 ;
}


static void stop_cold_x ()
{
  begin_MUTEX_LOCKED (XLock);
  
  //  fprintf ( stderr, "stop_cold_x ... " );
  if (display) XFlush(display);
  end_MUTEX_LOCKED (XLock);
  //  fprintf ( stderr, "closing dpy ... " );
  close_display();
  //  fprintf ( stderr, "closed\n" );

}

static char *in_addr_to_string(struct in_addr *addr)
{
	struct in_addr swap_addr;
	swap_addr.s_addr = ntohl(addr->s_addr);
	return inet_ntoa(swap_addr);
}

int check_display(XParams *params,
		  NetworkInterface *nwi)
{
 char display_name[BUFSIZ];
 int i ;

  BuildXDisplayName(display_name, 
		    params->xpHostName,
		    params->xpDisplay,
		    params->xpScreen);

  if (!(display = XOpenDisplay(display_name))) 
  {
	  if (errno == EAGAIN) {
		  vwarn("cold load init",
			"unable to open display %d on %s",
			params->xpDisplay,
			params->xpHostName ? : "localhost");
		  for (i=0;i<MaxNetworkInterfaces;i++)
			  if (nwi[i].present &&
			      nwi[i].myProtocol == ETH_P_IP)
				  break ;
		  vwarn("cold load init",
			"you should probably issue \"xhost +%s\" on %s",
			in_addr_to_string(&nwi[i].myAddress),
			params->xpHostName ? : "localhost");
	  } else
		  vwarn("cold load init",
			"Unable to open display %s - %m",
			XDisplayName(display_name));
	  return -1;
  }
  else XCloseDisplay(display);
  return 0;
}

int check_keyboard (XParams *params,
		    boolean noWaiting)
{
  char display_name[BUFSIZ];
  struct timespec openSleep;

  BuildXDisplayName(display_name, 
		    params->xpHostName,
		    params->xpDisplay,
		    params->xpScreen);

  if (!(display = XOpenDisplay(display_name))) 
  {
	  if (noWaiting) return -1;
	  else {
		  verror ("cold load init", NULL);
		  vwarn ("cold load init", "Waiting for X server (%m)... ");
		  while (display == NULL) {
			  openSleep.tv_sec = 5;
			  openSleep.tv_nsec = 0;
			  if (pthread_delay_np(&openSleep)) 
				  vpunt (NULL,
					 "Unable to sleep in thread %lx",
					 pthread_self());
			  display = XOpenDisplay(display_name);
		  }
		  fprintf (stderr, "Done.\n");
	  }
  }
  screen = XDefaultScreenOfDisplay(display);
  visual = XDefaultVisualOfScreen(screen);
  root = XRootWindowOfScreen(screen);
  colormap = XDefaultColormapOfScreen(screen);

  originalModmap = XGetModifierMapping(display);
  make_map(0);
  if (setup_modifier_mapping() < 0) {
	  close_display ();
  	  return -1 ;
  }
  close_display();
  return 0;

}

static int open_display (XParams *params, boolean noWaiting)
{
  XWMHints wmhints;
  XSizeHints sizehints;
  XColor color;
  XSetWindowAttributes attributes;
  XGCValues gcv;
  XFontStruct *fontinfo;
  char display_name[BUFSIZ];
  int screen_no, border_width, w_x, w_y, w_w, w_h, g_flags;
  struct timespec openSleep;

  BuildXDisplayName(display_name, 
		    params->xpHostName,
		    params->xpDisplay,
		    params->xpScreen);

  display=XOpenDisplay(display_name);
  if (display == NULL) {
	  if (noWaiting) return -1;
	  else {
		  verror ("cold load init", NULL);
		  vwarn ("cold load init", "Waiting for X server... ");
		  while (display == NULL) {
			  openSleep.tv_sec = 5;
			  openSleep.tv_nsec = 0;
			  if (pthread_delay_np(&openSleep)) 
				  vpunt (NULL,
					 "Unable to sleep in thread %lx",
					 pthread_self());
			  display = XOpenDisplay(display_name);
		  }
		  fprintf (stderr, "Done.\n");
	  }
  }
  screen_no = XDefaultScreen(display);
  screen = XDefaultScreenOfDisplay(display);
  visual = XDefaultVisualOfScreen(screen);
  root = XRootWindowOfScreen(screen);
  colormap = XDefaultColormapOfScreen(screen);

  originalModmap = XGetModifierMapping(display);
  make_map (1);
  setup_modifier_mapping();

  fontinfo = XLoadQueryFont(display, "genera-cptfont");
  if (fontinfo)
  {
	  gcv.font = fontinfo->fid;
	  XFreeFontInfo(NULL, fontinfo, 0);
  }
  else
	  gcv.font = 0;
  char_width = 8;
  char_height = 12;
  roff = rmarg - 0;
  toff = tmarg + 10;
  loff = lmarg + 0;
  boff = bmarg + 2;
  
  border_width = params->xpBorderWidth<0 ? 2 : params->xpBorderWidth;
#ifdef REALARGUMENTPARSING
  if (params->xpGeometry)
	  g_flags = XGeometry(display,
			      screen_no,
			      params->xpGeometry,
			      "800x400+0+0",
			      border_width,
			      char_width,
			      char_height,
			      roff+loff,
			      toff+boff,
			      &w_x,
			      &w_y,
			      &w_w,
			      &w_h);
  else
  { 
	  g_flags = 0;
	  w_x = 0;
	  w_y = 0;
	  w_w = 800;
	  w_h = 400; }
#else
  if (params->xpGeometry)
	  g_flags = XGeometry(display,
			      screen_no,
			      params->xpGeometry,
			      "800x800+100+100",
			      border_width,
			      char_width,
			      char_height,
			      roff+loff,
			      toff+boff,
			      &w_x,
			      &w_y,
			      &w_w,
			      &w_h);
  else
    { 
	    g_flags = 0;
	    w_x = 100;
	    w_y = 100;
	    w_w = 800;
	    w_h = 800; 
    }
#endif

  if ((params->xpForegroundColor != NULL) &&
      XAllocNamedColor(display,
		       colormap,
		       params->xpForegroundColor,
		       &color,
		       &color))
	  gcv.foreground = color.pixel;
  else
	  gcv.foreground = XBlackPixelOfScreen(screen);

  if ((params->xpBackgroundColor != NULL) &&
      XAllocNamedColor(display,
		       colormap,
		       params->xpBackgroundColor,
		       &color,
		       &color))
	  gcv.background = color.pixel;
  else
    gcv.background = XWhitePixelOfScreen(screen);

  if ((params->xpBorderColor != NULL) &&
      XAllocNamedColor(display,
		       colormap,
		       params->xpBorderColor,
		       &color,
		       &color))
	  attributes.border_pixel = color.pixel;
  else
	  attributes.border_pixel = XBlackPixelOfScreen(screen);

  attributes.background_pixel = gcv.background;
  attributes.event_mask = KeyPressMask|ExposureMask|StructureNotifyMask|
	  FocusChangeMask|VisibilityChangeMask;
  attributes.colormap = colormap;
  window = XCreateWindow(display,
			 root,
			 w_x,
			 w_y,
			 w_w,
			 w_h,
			 border_width,
			 CopyFromParent,
			 InputOutput,
			 visual,
			 CWBackPixel|CWBorderPixel|CWEventMask|CWColormap,
			 &attributes);
  icon_window = XCreateWindow(display,
			      root,
			      w_x,
			      w_y,
			      icon_width,
			      icon_height,
			      0,
			      CopyFromParent,
			      InputOutput,
			      visual,
			      CWBackPixel|CWEventMask|CWColormap,
			      &attributes);

  gc = XCreateGC(display,
		 window,
		 GCForeground|GCBackground|(gcv.font ? GCFont : 0),
		 &gcv);
  icon_gc = XCreateGC(display,
		      icon_window,
		      GCForeground|GCBackground,
		      &gcv);
  
  if (!gcv.font)
	  cptfont_bitmap = XCreateBitmapFromData(display,
						 root,
						 GENERA_CPTFONT_bits,
						 GENERA_CPTFONT_width,
						 GENERA_CPTFONT_height);

  if (XCellsOfScreen(screen) < 16) {
	  icon_bitmap = XCreateBitmapFromData(display,
					      icon_window,
					      GeneraIcon32_bits,
					      GeneraIcon32_width,
					      GeneraIcon32_height);
	  icon_gc_s = icon_gc_c = icon_gc_t = NULL;
  } else {
	  icon_bitmap = 0;
	  color.red = 0;
	  color.green = 65535;
	  color.blue = 0;
	  if (XAllocColor(display, colormap, &color)) {
		  gcv.foreground = color.pixel;
		  icon_gc_s = XCreateGC(display,
					icon_window,
					GCForeground,
					&gcv);
	  } else
		  icon_gc_s = icon_gc;
	  color.red = 65535;
	  color.green = 0;
	  color.blue = 0;
	  if (XAllocColor(display, colormap, &color)) {
		  gcv.foreground = color.pixel;
		  icon_gc_c = XCreateGC(display,
					icon_window,
					GCForeground,
					&gcv);
	  } else
		  icon_gc_c = icon_gc;
	  color.red = 65535;
	  color.green = 0;
	  color.blue = 65535;
	  if (XAllocColor(display, colormap, &color)) {
		  gcv.foreground = color.pixel;
		  icon_gc_t = XCreateGC(display,
					icon_window,
					GCForeground,
					&gcv);
	  } else
		  icon_gc_t = icon_gc;
  }

  SetColdLoadNames ();
  wmhints.flags = InputHint|StateHint|IconWindowHint;
  wmhints.input = True;
  wmhints.initial_state = (params->xpInitialState == Iconic) ? 
	  IconicState :
	  NormalState;
  wmhints.icon_window = icon_window;
  XSetWMHints(display, window, &wmhints);
  sizehints.flags = ((g_flags & XValue) ? USPosition : PPosition)
    		    | ((g_flags & WidthValue) ? 
		       USSize :
		       PSize);
  sizehints.x = w_x;		/* These are for pre-ICCCM window managers */
  sizehints.y = w_y;
  sizehints.width = w_w;
  sizehints.height = w_h;
  XSetNormalHints(display, window, &sizehints);
  XMapWindow(display, window);
  XFlush(display);
  
  alloc_screen_array(w_w, w_h);
  return 0;
}


static void close_display ()
{
	begin_MUTEX_LOCKED (XLock);
	if (display != NULL)
	{
		if (originalModmap != NULL) {
			if (XSetModifierMapping(display,
						originalModmap) != 
			    MappingSuccess)
				vwarn("cold load",
				      "Unable to restore original X modifier keymap");
			XFreeModifiermap(originalModmap);
			if (orig_meta) {
				XChangeKeyboardMapping(display,
						       XKeysymToKeycode(display,
									XK_Alt_L),
						       ks_p_kc_meta,
						       orig_meta,
						       1);
				XFree (orig_meta);
			}
			if (orig_hyper) {
				XChangeKeyboardMapping(display,
						       XKeysymToKeycode(display,
									XK_Menu),
						       ks_p_kc_hyper,
						       orig_hyper,
						       1);
				XFree (orig_hyper);
			}
			originalModmap = NULL;
		}
		XCloseDisplay(display);
		display = NULL;
	}
	end_MUTEX_LOCKED (XLock);
}


/* static void close_display_child_hook () */
/* { */
/*   if (display != NULL) */
/*     close(XConnectionNumber(display)); */
/* } */


static void handle_input ()
{
	XEvent event;
	KeySym keysym;
	int key = -1, bits = 0;
	coldmapentry *mapp;
	static int first_keypress = 1 ;
	static char buffer[3] ;
	static int last_w = 0, last_h = 0;
	
	XNextEvent(display, &event);
	  
//
// jj
//
	XFlush(display);
//
//
//
	begin_MUTEX_LOCKED (XLock);
	  
	switch (event.type)
	{
		
	case ConfigureNotify:
		//    printf ("ConfN %d x %d\n",event.xconfigure.width, event.xconfigure.height);
		if (event.xconfigure.window == window) {
			if ((last_w != event.xconfigure.width) ||
			    (last_h != event.xconfigure.height)) {
				alloc_screen_array(event.xconfigure.width,
						   event.xconfigure.height);
				last_w = event.xconfigure.width ;
				last_h = event.xconfigure.height ;
			}
			else if (event.xconfigure.window == icon_window)
			{
				icon_width = event.xconfigure.width;
				icon_height = event.xconfigure.height;
			}
		}
		break;
		  
	  case Expose:
		  //    printf ("Expose\n");
		  if (event.xexpose.window == window &&
		      event.xexpose.count == 0)
		  {
			  if (event.xexpose.y < tmarg) show_lights(1);
			  hide_cursor();
			  redisplay_screen_array((event.xexpose.x-lmarg)/char_width,
						 (event.xexpose.y-tmarg)/char_height,
						 (event.xexpose.x-lmarg+event.xexpose.width-1)/char_width+1,
						 (event.xexpose.y-tmarg+event.xexpose.height-1)/char_height+1);
			  reset_light_state(True);
			  show_lights(1);
			  show_cursor();
		  }
		  else if (event.xexpose.window == icon_window)
			  show_icon();
		  break;
		  
	case KeyPress:
		//    printf ("XKp\n");
		if (first_keypress)
		{
			/* Disable FEP timer */
			first_keypress = 0;
			alarm(0);
		}
		keysym = XLookupKeysym(&event.xkey, 0);
		if (IsModifierKey(keysym) ||
		    (XK_Multi_key == keysym) ||
		    (XK_KP_F4 == keysym))
			break;
		if (event.xkey.state & ControlMask) bits |= 1;
		if (event.xkey.state & meta_mask) bits |= 2;
		if (event.xkey.state & super_mask) bits |= 4;
		if (event.xkey.state & hyper_mask) bits |= 8;
		if (event.xkey.state & isol3_mask) bits |= 16;
		//
		if ((XK_F1 <= keysym) && (keysym <= XK_F23))
			key = fkMap[2*(keysym-XK_F1) +
				    ((event.xkey.state & ShiftMask) ? 1 : 0)];
		//
		// if ISO_Level3_Shift char: use X translation
		//
		else if ((event.xkey.state & isol3_mask) &&
			 (XK_space <= keysym) && (keysym <= XK_ssharp)) {
			XLookupString ( &event.xkey,
					buffer,
					sizeof(buffer),
					&keysym,
					NULL );
			key = buffer[0] ;
		}
		//
		// handle shift & lock
		//
		else if ((XK_a <= keysym) && (keysym <= XK_z))
		{
			key = (keysym - XK_a) + 65;
			if ((bits == 0)
			    ? ((event.xkey.state
				& (ShiftMask | LockMask)) == 0)
			    : (event.xkey.state & ShiftMask))
				key = key + 32;

		}
		//
		// special cases
		//
		else
		{
			if (event.xkey.state & ShiftMask) {
				if (XK_KP_Enter == keysym)
					/* Special case Shift-Keypad-Enter: Generate Return instead of End */
					key = SK_Return;
				else
					keysym = XLookupKeysym(&event.xkey, 1);
			}
			if ((XK_space <= keysym) && (keysym <= XK_asciitilde))
				key = keysym;
			else if (key == -1)
			{
				for (mapp = skMap; mapp->code != -1; mapp++)
				{
					if (keysym == mapp->keysym)
					{
						key = mapp->code;
						break;
					}
				}
			}
		}
		if (key == -1)
			XBell(display, 0);
		else {
			EmbQueuePutWord (keyboard_queue, (clsoInputChar<<24) |
					 ((uEmbWord)bits<<12) | (uEmbWord)key);
			if ((key == SK_Function) && (bits & 9) == 9)  
				EmbCommAreaPtr->stop_request = TRUE;
		}
		//    printf("keysym = %d, key = %d, bits = %d\n", keysym, key, bits);
		break;
		
	case MappingNotify:
//		  printf ("MapN\n");
		XRefreshKeyboardMapping(&event.xmapping);
		if (event.xmapping.request == MappingModifier)
			if (setup_modifier_mapping() < 0) {
//			  	  vwarn ("cold load init", "in MappingNotify");
				exit (-1) ; 
			}
		break;
		
	case VisibilityNotify:
		//    printf ("VisN\n");
		if (event.xvisibility.window == window)
			visibility = (event.xvisibility.state != 
				      VisibilityFullyObscured);
		else if (event.xvisibility.window == icon_window)
			icon_visibility = (event.xvisibility.state != 
					   VisibilityFullyObscured);
		break;
		
	case FocusIn:
		//    printf ("XFI ");
		//    printf ("mode = %d, detail =%d\n", event.xfocus.mode, event.xfocus.detail);
		/* if (event.xfocus.mode == NotifyNormal && event.xfocus.detail == NotifyPointer) */
		/*   break ; */
		cursor_frozen = 0;
		show_cursor();
		break;
		
	case FocusOut:
		//    printf ("XFO\n");
		show_cursor_internal(1);
		cursor_frozen = 1;
		break;
	}
	end_MUTEX_LOCKED (XLock);
}

static void alloc_screen_array (int new_width_pixels, int new_height_pixels)
{
  line *old_screen_array = screen_array;
  int old_width = width;
  int old_height = height;
  int y = 0;
  int new_width, new_height;	/* in lines */
  int pixels_per_run_light;
  
  new_width = (new_width_pixels - (roff+loff))/char_width;
  new_height = (new_height_pixels -
		(toff+char_height+RUN_LIGHT_Y_SPACE+boff))/char_height;
  
  if ((new_width == old_width) && (new_height == old_height)) return;
  
  screen_array = (line *)malloc(new_height * sizeof(line));
  while (y < new_height)
  {
	  screen_array[y].length = 0;
	  screen_array[y].chars = (char *)malloc(new_width);
	  memset(screen_array[y].chars, ' ', new_width);
	  if (y < old_height)
	  {
		  screen_array[y].length = old_screen_array[y].length <
			  new_width
			  ? old_screen_array[y].length : new_width;
		  memcpy(screen_array[y].chars,
			 old_screen_array[y].chars,
			 screen_array[y].length);
	  }
	  y++;
  }
  if (old_screen_array != NULL)
  {
	  for (y=0; y<old_height; y++)
		  free(old_screen_array[y].chars);
	  free(old_screen_array);
  }
  cold_channel->character_width = 1;
  cold_channel->line_height = 1;
  cold_channel->display_width = new_width;
  cold_channel->display_height = new_height;
  run_light_y = new_height_pixels - RUN_LIGHT_Y_OFFSET;
  run_label_y = new_height_pixels - RUN_LIGHT_Y_SPACE;
  run_label_height = char_height;
  pixels_per_run_light = (new_width_pixels - (roff+loff)) / 32;
  run_light_first_x = (pixels_per_run_light * 8) + loff;
  run_label_width = new_width_pixels - run_light_first_x - roff;
  progress_bar_first_x = (pixels_per_run_light * 22) + loff;
  progress_bar_width = new_width_pixels - loff - progress_bar_first_x - roff;
  reset_light_state(True);
  EmbQueuePutWord(keyboard_queue, clsoSetSize<<24);
  width = new_width;
  height = new_height;
}


static void redisplay_line (int y, int x, int xlim)
{
	if (!cptfont_bitmap) {
		XDrawImageString(display, window, gc,
				 x*char_width+loff, y*char_height+toff,
				 &screen_array[y].chars[x], xlim-x);
	}
	else
	{
		int cx, wx, wy = y*char_height+tmarg;
		
		for (cx = x, wx = x*char_width+lmarg;
		     cx<xlim;
		     cx++, wx+=char_width)
			XCopyPlane(display, cptfont_bitmap, window, gc,
				   (char_width-1)*screen_array[y].chars[cx], 0,
				   (char_width-1), char_height, wx, wy, 1);
	}
}


static void redisplay_screen_array (int minx, int miny, int maxx, int maxy)
{
  int y;
  int this_minx = ((0 < minx) ? minx : 0);
  int this_miny = ((0 < miny) ? miny : 0);
  int this_maxy = ((height < maxy) ? height : maxy);
  
  for (y = this_miny; y<this_maxy; y++)
  {
	  int this_maxx = ((screen_array[y].length < maxx) ? screen_array[y].length : maxx);
	  
	  if (this_minx < this_maxx)
		  redisplay_line(y, this_minx, this_maxx);
  }
}


static void show_cursor_internal (int new_state)
{
	if (visibility && !cursor_frozen)
	{
		if (cursor_visible && (cursor_state != new_state))
			hide_cursor();
		if (!cursor_visible)
		{
			cursor_state = EmbCommAreaPtr->fep.cursor;  
			if (cursor_state)
				XFillRectangle(display,
					       window,
					       gc,
					       current_x*char_width+lmarg, 
					       current_y*char_height+tmarg,
					       char_width-1,
					       char_height-1);
			XDrawRectangle(display,
				       window,
				       gc,
				       current_x*char_width+lmarg, 
				       current_y*char_height+tmarg,
				       char_width-1,
				       char_height-1);
			cursor_visible = 1;
		}
	}
}


static void hide_cursor ()
{
  if (cursor_visible)
  {
	  XClearArea(display,
		     window,
		     current_x*char_width+lmarg,
		     current_y*char_height+tmarg,
		     char_width,
		     char_height,
		     False);
	  redisplay_screen_array(current_x,
				 current_y,
				 current_x+1,
				 current_y+1);
	  cursor_visible = 0;
  }
}


static void show_icon ()
{
  XPoint tri[3];
  int xoff = icon_width > 32 ? (icon_width-32)/2 : 0;
  
  if (icon_bitmap)
	  XCopyPlane(display,
		     icon_bitmap,
		     icon_window,
		     icon_gc,
		     0,
		     0,
		     32,
		     32,
		     xoff,
		     0,
		     1);
  else {
	  XFillRectangle(display,
			 icon_window,
			 icon_gc_s,
			 xoff+10,
			 3,
			 9,
			 9);
	  XFillArc(display,
		   icon_window,
		   icon_gc_c,
		   xoff+15,
		   9,
		   14,
		   14,
		   0,
		   360*64);
	  tri[0].x = xoff+3; tri[0].y = 29;
	  tri[1].x = xoff+10; tri[1].y = 15;
	  tri[2].x = xoff+17; tri[2].y = 29;
	  XFillPolygon(display,
		       icon_window,
		       icon_gc_t,
		       tri,
		       3,
		       Convex,
		       CoordModeOrigin);
  }
}

static void show_lights (int force)
{
	int i, bit;
	int changed = light_state ^ EmbCommAreaPtr->run_lights;
	EmbColdLoadChannel *cls;
	int pb_length, pb_length_change;

	//  if (! display) return ;
	light_state = EmbCommAreaPtr->run_lights;
	if (visibility) {
		/* Update run bars in cold-load window */
		if (force || changed)
			for (i = run_light_first_x, bit = 1;
			     bit < 32;
			     i += RUN_LIGHT_SPACING, bit = bit << 1)
				if (force || (changed & bit)) {
					if (light_state & bit)
						XFillRectangle(display,
							       window,
							       gc,
							       i,
							       run_light_y,
							       RUN_LIGHT_WIDTH,
							       1);
					else
						XClearArea(display,
							   window,
							   i,
							   run_light_y,
							   RUN_LIGHT_WIDTH,
							   1,
							   False);
				}
		/* Update progress bar */
		cls = HostPointer(EmbCommAreaPtr->cold_load_channel);
		if (cls != NULL) {
			if (cls->progress_note.string_length == 0) {
				if (progress_label != NULL) {
					/* Clear progress label */
					XClearArea( display,
						    window,
						    run_light_first_x,
						    run_label_y - 
						    run_label_height + 1,
						    run_label_width,
						    run_label_height,
						    False );
					free(progress_label);
					progress_label = NULL;
				}
				if (progress_bar_length_state != 0) {
					/* Clear progress bar */
					XClearArea(display,
						   window,
						   progress_bar_first_x,
						   run_light_y,
						   progress_bar_width,
						   1,
						   False);
					progress_bar_numerator_state =
						progress_bar_denominator_state =
						progress_bar_length_state = 0;
				}
			} else {
				/* Update progress label */
				if (progress_label == NULL) {
					/* Draw run bar labels */
					XDrawString(display, 
						    window,
						    gc,
						    run_light_first_x +
						    (PROCESS_RUN_LIGHT *
						     RUN_LIGHT_SPACING),
						    run_label_y,
						    "Run",
						    3);
					XDrawString(display,
						    window,
						    gc,
						    run_light_first_x +
						    (DISK_RUN_LIGHT *
						     RUN_LIGHT_SPACING),
						    run_label_y,
						    "Disk",
						    4);
					XDrawString(display,
						    window,
						    gc,
						    run_light_first_x +
						    (NETWORK_RUN_LIGHT *
						     RUN_LIGHT_SPACING),
						    run_label_y,
						    "Net",
						    3);
					/* Allocate memory for progress label cache */
					progress_label =
						(char *)calloc(cls->progress_note.string_total_size,
							       sizeof(char));
					progress_label_length = 0;
				}
				if (progress_label_length !=
				    cls->progress_note.string_length ||
				    strcmp(progress_label,
					   cls->progress_note.string)) {
					/* Recache progress label */
					progress_label_length =
						cls->progress_note.string_length;
					strncpy(progress_label,
						cls->progress_note.string,
						progress_label_length);
					/* Draw new label
					 * Erase old label first so no overwrite */
					XClearArea( display,
						    window,
						    progress_bar_first_x,
						    run_label_y -
						    run_label_height + 1,
						    progress_bar_width,
						    run_label_height,
						    False );
					XDrawString(display,
						    window,
						    gc,
						    progress_bar_first_x,
						    run_label_y,
						    progress_label,
						    progress_label_length);
				}
				if (cls->progress_note.denominator > 0) {
					/* Update progress bar */
					if (progress_bar_numerator_state !=
					    cls->progress_note.numerator ||
					    progress_bar_denominator_state !=
					    cls->progress_note.denominator) {
						progress_bar_numerator_state =
							cls->progress_note.numerator;
						progress_bar_denominator_state =
							cls->progress_note.denominator;
						pb_length =
							(progress_bar_numerator_state *
							 progress_bar_width) /
							progress_bar_denominator_state;
						pb_length_change =
							pb_length -
							progress_bar_length_state;
						if (pb_length_change < 0) {
							/* Shorten the progress bar */
							XClearArea(display,
								   window,
								   progress_bar_first_x + pb_length,
								   run_light_y - pb_length_change,
								   1,
								   1,
								   False);
							progress_bar_length_state =
								pb_length;
						} else if (pb_length_change > 0) {
							/* Lengthen the progress bar */
							XFillRectangle(display,
								       window,
								       gc,
								       progress_bar_first_x +
								       progress_bar_length_state,
								       run_light_y,
								       pb_length_change,
								       1);
							progress_bar_length_state = pb_length;
						}
					}
				}
			}
		}
	}
	/* if (icon_visibility) { */
	/*   /\* Update run bars in icon *\/ */
	/*   if (force || changed) */
	/*     for (i = 2, bit = 1; bit < 32; i += 6, bit = bit << 1) */
	/* 	if (force || (changed & bit)) */
	/* 	  if (light_state & bit) */
	/* 	    XFillRectangle(display, icon_window, icon_gc, i, 32, 4, 4); */
	/* 	  else */
	/* 	    XClearArea(display, icon_window, i, 32, 4, 4, False); */
	/* } */
}


static void reset_light_state (int screen_cleared_p)
{
	if (screen_cleared_p == True) {
		progress_bar_numerator_state =
			progress_bar_denominator_state =
			progress_bar_length_state = 0;
		light_state = 0;
	}
	if (progress_label != NULL) {
		free(progress_label);
		progress_label = NULL;
	}
}


static void replay_command_history ()
{
	int i, have_pos = FALSE;
	
	if (cold_channel->command_history_wrapped)
		i = cold_channel->command_history_top+1;
	else
		i = 0;
	for (; i != cold_channel->command_history_top; i++) {
		/* Watch for history wraparound */
		if (i == ColdLoadCommandHistorySize)
			i = 0;
		
		/* Don't do any output until we know where to put it */
		if (!have_pos && 
		    ((cold_channel->command_history[i]>>24) & 0xff) ==
		    clsoSetCursorpos)
			have_pos = TRUE;

		/* Do output */
		if (have_pos)
			handle_output_command(cold_channel->command_history[i]);
	}
	
	reset_light_state(False);
	show_lights(1);
}

static void handle_output_command (uEmbWord command)
{
	int operator;
	int x, y;
	char c;
	XWMHints wmhints;
  
	operator = (command>>24) & 0xff;
	switch (operator) {
	case clsoDrawChar:
	case clsoLozengedChar:
		if ((current_y < height) && (current_x < width)) {
			if (screen_array[current_y].length <= current_x) {
				for (x = screen_array[current_y].length;
				     x < current_x;
				     x++) 
					screen_array[current_y].chars[x] = ' ';
				screen_array[current_y].length = current_x + 1;
			}
			c = (char)(command & 0xff);
			screen_array[current_y].chars[current_x] = c;
			redisplay_line(current_y, current_x, current_x+1);
		}
		current_x++;
		break;
	case clsoSetCursorpos:
		current_x = command & 0xfff;
		current_y = (command>>12) & 0xfff;
		break;
	case clsoClearRestOfWindow:
		for (y = current_y+1; y<height; y++)
			screen_array[y].length = 0;
		XClearArea(display,
			   window,
			   lmarg,
			   (current_y+1)*char_height+tmarg,
			   width*char_width,
			   (height - (current_y+1))*char_height,
			   False);
		reset_light_state(True);
		// fall through
	case clsoClearRestOfLine:
		if (current_x < screen_array[current_y].length) {
			screen_array[current_y].length = current_x;
			XClearArea(display,
				   window,
				   current_x*char_width+lmarg,
				   current_y*char_height+tmarg,
				   (width - current_x)*char_width,
				   char_height,
				   False);
		}
		break;
	case clsoBeep:
		XBell(display, 0);
		break;
	case clsoSelect:
#ifndef JJ_WAS_CLEAR_SCREEN			/* Rejected by Hornig */
		/* Clear screen */
		for (y = 0; y<height; y++)
			screen_array[y].length = 0;
		XClearArea(display,
			   window,
			   lmarg,
			   tmarg,
			   width*char_width,
			   height*char_height+tmarg,
			   False);
		
		/* Clear history */
		cold_channel->command_history_top = 0;
		cold_channel->command_history_wrapped = FALSE;
#endif
		
		/* Expose display, ring bell */
		XWithdrawWindow ( display, window, XDefaultScreen(display) );
		wmhints.flags = InputHint|StateHint|IconWindowHint;
		wmhints.input = True;
		wmhints.initial_state = NormalState;
		wmhints.icon_window = icon_window;
		XSetWMHints(display, window, &wmhints);
		XMapRaised( display, window );
		XBell(display, 0);
		XFlush (display);
		break;
	case clsoDeselect:
		XIconifyWindow ( display,window, XDefaultScreen(display) );
		break;
	}
}


static void get_keyboard_modifier_codes (int for_real_p,
					 KeyCode *control_l_code,
					 KeyCode *control_r_code,
					 KeyCode *meta_l_code,
					 KeyCode *meta_r_code,
					 KeyCode *alt_l_code,
					 KeyCode *super_code,
					 KeyCode *hyper_code)
{
  KeyCode keycode1, keycode2;

  *control_l_code = XKeysymToKeycode(display, XK_Control_L);
  *control_r_code = XKeysymToKeycode(display, XK_Control_R);
  *meta_l_code = XKeysymToKeycode(display, XK_Meta_L);
  *meta_r_code = XKeysymToKeycode(display, XK_Meta_R);
  *alt_l_code = XKeysymToKeycode(display, XK_Alt_L);

  keycode1 = XKeysymToKeycode(display, XK_ISO_Left_Tab);	/* Linux X server */
  keycode2 = XKeysymToKeycode(display, XK_adiaeresis);	/* ä */
  
  //  printf("keycode1 %d, keycode2 %d\n", keycode1, keycode2);

  if (keycode1 != 0 && keycode2 != 0) {
	  if (for_real_p) {
		  if (! did_show ) vwarn("cold load",
					 "presuming a German keyboard");
		  did_show = 1;
	  } ;
//    printf("German Keyboard\n");
	  keyboardType = German;
	  isol3_code = XKeysymToKeycode(display,XK_ISO_Level3_Shift);
	  skMap = (coldmapentry*)&coldmapGerman;
	  fkMap = (short*)&fkmapGerman;
	  if (keycode1 != 0) {
		  skMap->keysym = XK_Num_Lock;				/* Linux X server */
		  /* Linux assigns a modifier mapping to the Num_Lock keysym but, on the
		     Apple Pro keyboard, that keysym maps to the key labelled "clear" so
		     it's safe to remove the modifier mapping to make room for Super/Hyper */
		  removeNumLockModifier = TRUE;
	  } else
		  /* ---*** TODO: Find out what KeySym is labelled CLEAR */
		  skMap->keysym = 0;					/* Apple X11 */
	  *super_code = XKeysymToKeycode(display, XK_Super_L);
	  *hyper_code = XKeysymToKeycode(display, XK_Hyper_R);
	  return ;
  }

  if (keycode1 != 0 && keycode2 == 0) {
	  if (for_real_p) {
		  if (!did_show) vwarn("cold load",
				       "presuming a US keyboard");
		  did_show = 1;
	  }
//    printf("US Keyboard\n");
	  keyboardType = Us;
	  isol3_code = XKeysymToKeycode(display,XK_ISO_Level3_Shift);
	  skMap = (coldmapentry*)&coldmapUs ;
	  fkMap = (short*)&fkmapUs;
	  if (keycode1 != 0) {
		  skMap->keysym = XK_Num_Lock;	/* Linux X server */
		  /* Linux assigns a modifier mapping to the Num_Lock keysym but, on the
		     Apple Pro keyboard, that keysym maps to the key labelled "clear" so
		     it's safe to remove the modifier mapping to make room for Super/Hyper */
		  removeNumLockModifier = TRUE;
	  } else
		  /* ---*** TODO: Find out what KeySym is labelled CLEAR */
		  skMap->keysym = 0;					/* Apple X11 */
	  *super_code = XKeysymToKeycode(display, XK_Super_L);
	  *hyper_code = XKeysymToKeycode(display, XK_Hyper_R);
	  return ;
  }

  keycode2 = XKeysymToKeycode(display, XK_Aring);		/* Apple X11 */

  if (keycode1 != 0 || keycode2 != 0) {
	  if (!did_show) {
		  vwarn("cold load",
			"presuming an Apple Pro keyboard");
		  did_show = 1;
	  };
	  keyboardType = Apple_Pro;
	  skMap = (coldmapentry*)&coldmapApple;
	  fkMap = (short*)&fkmapApple;
	  if (keycode1 != 0) {
		  skMap->keysym = XK_Num_Lock;				/* Linux X server */
		  /* Linux assigns a modifier mapping to the Num_Lock keysym but, on the
		     Apple Pro keyboard, that keysym maps to the key labelled "clear" so
		     it's safe to remove the modifier mapping to make room for Super/Hyper */
		  removeNumLockModifier = TRUE;
	  } else
		  /* ---*** TODO: Find out what KeySym is labelled CLEAR */
		  skMap->keysym = 0;					/* Apple X11 */
	  *super_code = XKeysymToKeycode(display, XK_Down);
	  *hyper_code = XKeysymToKeycode(display, XK_Left);
  }

  else {
	  /* Assume it's a DEC keyboard */

	  /* Special knowledge -- DEC's LK401-AA has two Multi-Key keys labelled Compose Character.
	     The call to XKeysymToKeycode returns the code for the righthand Compose Character.
	     The lefthand Compose Character's key code is four greater than the righthand key. */

	  keycode1 = XKeysymToKeycode(display, XK_Multi_key);
	  keycode2 = XKeysymToKeycode(display, XK_space);
	  *super_code = keycode1 + 4;
	  *hyper_code = keycode1;
	  
	  /* OSF 4.0 with CDE makes shift+Space be the Multi-Key code, don't get confused */

//printf("dec keyboard\n");
//printf("keycode1 %d, keycode2 %d\n", keycode1, keycode2);

	  if (keycode1 == keycode2)
		  *hyper_code = 0;

	  /* If XK_Multi_key's code is 0, then we must have the PC-style DEC keyboard. */

	  if (*hyper_code == 0) {
		  if (!did_show) vwarn("cold load",
				       "presuming a DEC PC keyboard");
		  did_show = 1;
		  keyboardType = DEC_PC;
		  skMap = (coldmapentry*)&coldmapDECPC;
		  fkMap = (short*)&fkmapDECPC;
		  *super_code = XKeysymToKeycode(display, XK_Down);
		  *hyper_code = XKeysymToKeycode(display, XK_Left);
	  }

	  else {
		  if (!did_show) vwarn("cold load",
				       "presuming a DEC LK401 keyboard");
		  did_show = 1;
		  keyboardType = DEC_LK401;
		  skMap = (coldmapentry*)&coldmapDECLK;
		  fkMap = (short*)&fkmapDECLK;
	  }
  }

  if ((*meta_l_code == 0) &&
      (*meta_r_code == 0) &&
      *alt_l_code)
	  *meta_l_code = *alt_l_code;

// hack
  *control_r_code = *control_l_code;
  *super_code = XKeysymToKeycode(display, XK_Control_R);
  
  return;
}


static int find_modifier (XModifierKeymap *modmap, KeyCode code)
{
	int modifier, i;
	
	if (code == 0) return -1;
	for (modifier = 0; modifier < 8; modifier++)
		for (i = 0; i < modmap->max_keypermod; i++)
			if (modmap->modifiermap[i+modifier*modmap->max_keypermod] == code)
				return modifier;
	return -1;
}


static int find_unused_modifier (XModifierKeymap **modmapp)
{
	int modifier, i;
	KeyCode num_lock_code;

	for (modifier = 0; modifier < 8; modifier++)
	{
		for (i = 0; i < (*modmapp)->max_keypermod; i++)
			if ((*modmapp)->modifiermap[i+modifier*(*modmapp)->max_keypermod] != 0)
				goto next_modifier;
		return modifier;
	next_modifier:
		continue;
	}

	if (removeNumLockModifier) {
		num_lock_code = XKeysymToKeycode(display, XK_Num_Lock);
		for (modifier = 0; modifier < 8; modifier++) {
			for (i = 0; i < (*modmapp)->max_keypermod; i++) {
				if ((*modmapp)->modifiermap[i+modifier*(*modmapp)->max_keypermod] == num_lock_code) {
					*modmapp =
						XDeleteModifiermapEntry(*modmapp,
									num_lock_code,
									modifier);
					vwarn("cold load",
					      "NumLock modifier removed");
					return modifier;
				}
			}
		}
	}
	
	return -1;
}


static int do_modifier (XModifierKeymap **modmapp, int *changedp, 
			KeyCode code1, KeyCode code2, KeyCode code3)
{
  int mod = -1;

  mod = find_modifier(*modmapp, code1);
  if (mod == -1)
	  mod = find_modifier(*modmapp, code2);
  if (mod == -1)
	  mod = find_modifier(*modmapp, code3);
  //  fprintf(stderr,"found %02x %02x %02x -> %d %02x\n", code1,code2,code3,mod,1<<mod);
  if (mod != -1)
	  return 1<<mod;
  if ((code1 == 0) && (code2 == 0) && (code3 == 0))
	  return 0;
  mod = find_unused_modifier(modmapp);
  if (mod == -1)
	  return 0;
  if (code1 != 0)
  {
	  *modmapp = XInsertModifiermapEntry(*modmapp, code1, mod);
	  *changedp = 1;
  }
  if (code2 != 0)
  {
	  *modmapp = XInsertModifiermapEntry(*modmapp, code2, mod);
	  *changedp = 1;
  }
  if (code3 != 0)
  {
	  *modmapp = XInsertModifiermapEntry(*modmapp, code3, mod);
	  *changedp = 1;
  }
  return 1<<mod;
}


static int mask_to_modifier (int mask)
{
	int i = -1;
	while (mask) {
		i++;
		mask >>= 1;
	}
	return i;
}


static int setup_modifier_mapping ()
{
  XModifierKeymap *modmap;
  KeyCode control_l_code,  control_r_code, meta_l_code, meta_r_code, 
          alt_l_code, super_code, hyper_code;
  int changed = 0;

  get_keyboard_modifier_codes(1,
			      &control_l_code,
			      &control_r_code,
			      &meta_l_code, 
			      &meta_r_code,
			      &alt_l_code,
			      &super_code,
			      &hyper_code);
  
//#if 0
  XGrabServer(display);
  modmap = XGetModifierMapping(display);
  do_modifier(&modmap, &changed, control_l_code, control_r_code, 0);
  meta_mask = do_modifier(&modmap, &changed,  meta_l_code, meta_r_code, 0);
  if (meta_mask == 0)
	  vwarn ("cold load init",
		 "Unable to allocate a modifier for the Meta key.");
  super_mask = do_modifier(&modmap, &changed, super_code, 0, 0);
  if (super_mask == 0)
	  vwarn ("cold load init",
		 "Unable to allocate a modifier for the Super key.");
  hyper_mask = do_modifier(&modmap, &changed, hyper_code, 0, 0);
  if (hyper_mask == 0)
	  vwarn ("cold load init",
		 "Unable to allocate a modifier for the Hyper key.");
  else if (hyper_mask == super_mask) {
	  modmap = XDeleteModifiermapEntry(modmap,
					   hyper_code,
					   mask_to_modifier(super_mask));
	  hyper_mask = do_modifier(&modmap, &changed, super_code, 0, 0);
	  if (hyper_mask == 0)
		  vwarn ("cold load init",
			 "Unable to allocate a modifier for the Hyper key.");
	  else
		  modmap = XDeleteModifiermapEntry(modmap,
						   super_code,
						   mask_to_modifier(hyper_mask));
	  changed = TRUE;
  }
  if (keyboardType == German) {
	  isol3_mask = do_modifier(&modmap, &changed, isol3_code, 0, 0);
	  if (isol3_mask == 0)
		  vwarn ("cold load init",
			 "Unable to allocate a modifier for the ISO_Level3_Shift key. Use xmodmap");
  }

  if (changed)
	  if (XSetModifierMapping(display, modmap) != MappingSuccess)
		  vwarn("Cold load", "Unable to set X modifier keymap");
  XUngrabServer(display);
  XFreeModifiermap(modmap);
  return 0;
//#endif
}



/* Error Handler */

/* static XErrorHandler XErrorDefaultHandler = NULL; */

/* static void SetColdXErrorHandler () */
/* { */
/* 	/\* Set error handler *\/ */
/* 	if (XErrorDefaultHandler == NULL) */
/* 		XErrorDefaultHandler =  */
/* 			XSetErrorHandler ((XErrorHandler)&ColdXErrorHandler); */
/* } */


/* static int ColdXErrorHandler (Display *display, XErrorEvent *error) */
/* { */
/* 	fprintf ( stderr, "Xerror : %d\n", error->request_code); */
/* 	if (error->request_code != X_KillClient) */
/* 		return ((*XErrorDefaultHandler)(display, error)); */
/* } */



/* static int initialize_cold (XParams *cl_params, boolean noWaiting) */
/* { */
/*   int x_fd; */

/*   begin_MUTEX_LOCKED (XLock); */

/*   x_fd = open_cold_load_display (cl_params, noWaiting); */

/*   end_MUTEX_LOCKED (XLock); */

/*   return (x_fd); */
/* } */


/* The output driver for the Cold Load window */

static void ColdLoadOutput (void* ignored)
{
	if (cold_channel->fd > 0) manage_cold_load_output ();
}


/* The input driver for the Cold Load window */

#define POLLWAIT_IN_MS 40

static void ColdLoadInput (pthread_addr_t argument)
{
	pthread_t self = pthread_self ();
	VLMConfig *config = (VLMConfig*) argument;
	struct pollfd xpoll;
	

	pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach,
			      (void*)self);

	WaitUntilInitializationComplete ();

	if (-1 == cold_channel->fd) {
		begin_MUTEX_LOCKED (XLock);
		cold_channel->fd = open_cold_load_display (&config->coldLoadXParams,
							   FALSE);
		end_MUTEX_LOCKED (XLock);
		setup_x_io_error_handler ();
	}
	
	while (TRUE) {

		pthread_testcancel ();
		xpoll.fd = cold_channel->fd;
		xpoll.events = POLLIN;
		
		/* Handle pending X input and errors */
		poll (&xpoll, 1, POLLWAIT_IN_MS);
		
		if (xpoll.revents) {
			/* If can read from x_fd, there're events pending */
			cold_channel->fd =
				manage_x_input (&config->coldLoadXParams);
		}
		begin_MUTEX_LOCKED (XLock);
		if (cold_channel->fd) XFlush (display);
		//    if (cold_channel->fd) update_cold_load_run_lights ();
		end_MUTEX_LOCKED (XLock);
	}
	pthread_cleanup_pop (TRUE);
}



static char *ColdLoadWindowName = NULL,
	    *ColdLoadIconName = NULL,
	    *DebuggerWindowName = NULL,
	    *DebuggerIconName = NULL;

static enum GuestStatus lastGuestStatus = NonexistentGuestStatus;

static char *concatenate_string(char *string1, char *string2)
{
	int total_size = strlen(string1) + strlen(string2) + 1;
	char *new_string = malloc(total_size);
	if (0 == new_string)
		vpunt (NULL, "No room for concatenated string.");
	strcpy(new_string, string1);
	return(strcat(new_string, string2));
}
     

static void SetupColdLoadNameStrings (VLMConfig* config)
{
	NetworkInterface *interface;
	struct hostent *theHost;
	struct in_addr theAddress;
	char *longHostName, *shortHostName, buffer[128], *pp, *aName;

	interface = &config->interfaces[0];
	while (!interface->present) interface++;

	switch (interface->myProtocol) {
		
	case ETHERTYPE_IP:
		theAddress.s_addr = htonl (interface->myAddress.s_addr);
		if (NULL ==
		    (theHost = gethostbyaddr ((char*)&theAddress.s_addr,
					      sizeof (struct in_addr),
					      AF_INET))) {
			sprintf (buffer, "INTERNET|%s", inet_ntoa(theAddress));
			longHostName = shortHostName = strdup (buffer);
		}
		else {
			longHostName = strdup (theHost->h_name);
			pp = strchr (longHostName, '.');
			if (pp) *pp = 0;
			shortHostName = longHostName;
			while (*theHost->h_aliases) {
				aName = strdup (*theHost->h_aliases);
				pp = strchr (aName, '.');
				if (pp) *pp = 0;
				if (strlen (aName) < strlen (shortHostName))
					shortHostName = aName; 
				theHost->h_aliases++;
			}
		}
		break;
		
	case ETHERTYPE_CHAOS:
		sprintf (buffer,
			 "CHAOS|%o",
			 htonl (interface->myAddress.s_addr));
		longHostName = shortHostName = strdup (buffer);
		break;
		
	default:
		longHostName = shortHostName = "";
		break;
	}

	ColdLoadIconName = concatenate_string (shortHostName,
					       " Cold Load");
	ColdLoadWindowName = concatenate_string (longHostName,
						 " Cold Load Stream");
	DebuggerWindowName = concatenate_string (longHostName,
						 " VLM Debugger");
	DebuggerIconName = concatenate_string (shortHostName,
					       " Debugger");
}

static void SetColdLoadNames ()
{
	if (display != NULL && window != 0) {
		if (RunningGuestStatus == EmbCommAreaPtr->guestStatus) {
			XStoreName (display, window, ColdLoadWindowName);
			XSetIconName (display, window, ColdLoadIconName);
		}
		else {
			XStoreName (display, window, DebuggerWindowName);
			XSetIconName (display, window, DebuggerIconName);
		}
	}
}

void UpdateColdLoadNames ()
{
	if (EmbCommAreaPtr->guestStatus != lastGuestStatus) {
		begin_MUTEX_LOCKED (XLock);
		SetColdLoadNames ();
		end_MUTEX_LOCKED (XLock);
		lastGuestStatus = EmbCommAreaPtr->guestStatus;
	}
}

static pthread_t plightsThread ;

static void* lightsThread ( void *ignore) {
  // update the cold_load run lights periodically
  //  1 s  in ns = 1 000 000 000
  // 40 ms in ns = 0 000 040 000
  // 1000 ms in ns = 0 000 100 000
  //#define LIGHT_SLEEP 40000               /* 25 times a second */
#define LIGHT_SLEEP 100000               /* 10 times a second */

	struct timespec lightSleep = { 0, LIGHT_SLEEP } ;

	WaitUntilInitializationComplete ();
	
	while (display)
	{
		pthread_delay_np ( &lightSleep );
		pthread_testcancel ();
		if (display && visibility) {
			begin_MUTEX_LOCKED (XLock);
			show_lights ( 0 );
			end_MUTEX_LOCKED (XLock);
		}
	}
	//  pthread_cleanup_pop (TRUE);
	return NULL;
}



/* Create the Cold Load Stream's channel */

int InitializeColdLoadChannel (VLMConfig* config)
{ 
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbColdLoadChannel));
  register EmbColdLoadChannel *p = (EmbColdLoadChannel*) HostPointer (cp);
  
  p->type = EmbColdLoadChannelType;
  p->unit = 0;
  p->next = EmbCommAreaPtr->channel_table;	/* Thread into list of all channels */
  EmbCommAreaPtr->channel_table = cp;
  EmbCommAreaPtr->cold_load_channel = cp;	/* Make it easy to find */
  cold_channel = p;

  p->keyboard_input_queue = CreateQueue (ColdLoadInputQueueSize, sizeof (EmbPtr));
  keyboard_queue = (EmbQueue*) HostPointer (p->keyboard_input_queue);
  
  p->display_output_queue = CreateQueue (ColdLoadOutputQueueSize, sizeof (EmbPtr));
  display_queue = (EmbQueue*) HostPointer (p->display_output_queue);
  display_queue->signal = InstallSignalHandler ((ProcPtrV)&ColdLoadOutput, NULL, FALSE);

  p->progress_note.string_total_size = ColdLoadProgressStringSize;
  p->progress_note.string_length = 0;
  
  SetupColdLoadNameStrings (config);

  begin_MUTEX_LOCKED (XLock);

  p->fd = open_cold_load_display (&config->coldLoadXParams, TRUE);

  end_MUTEX_LOCKED (XLock);

  if (-1 == p->fd) {
    verror ("cold load init", NULL);
    vwarn ("cold load init", "Will wait for X server but cold load may not function properly.");
  }
   setup_x_io_error_handler ();

  if (pthread_create (&p->coldLoadInput, &EmbCommAreaPtr->inputThreadAttrs,
		      (pthread_startroutine_t) &ColdLoadInput, (pthread_addr_t) config))
    vpunt (NULL, "Unable to create the cold load window's input thread");
  if (pthread_create (&plightsThread, NULL,
  		      (pthread_startroutine_t) &lightsThread, NULL))
    vpunt (NULL, "Unable to create the cold load window's runlight thread");
  p->coldLoadInputSetup = TRUE;
  return 0 ;
}


/* Reset the Cold Load Stream's channel */

void ResetColdLoadChannel (EmbChannel* channel)
{
  register EmbColdLoadChannel* coldLoadChannel = (EmbColdLoadChannel*) channel;

  ResetIncomingQueue ((EmbQueue*) HostPointer (coldLoadChannel->display_output_queue));
  ResetOutgoingQueue ((EmbQueue*) HostPointer (coldLoadChannel->keyboard_input_queue));
  coldLoadChannel->progress_note.string_length = 0;	/* Flush any progress note */
  coldLoadChannel->is_selected = FALSE;
  coldLoadChannel->command_history_top = 0;		/* Flush the cold load's history */
  coldLoadChannel->command_history_wrapped = FALSE;
}


/* Cleanup the Cold Load Stream's channel */

void TerminateColdLoadChannel () {
  //  void *exit_value;
  
  visibility = 0 ; // to stop output for run_lights et al

  if (cold_channel && cold_channel->coldLoadInputSetup) {
    pthread_cancel (cold_channel->coldLoadInput);
    pthread_cancel (plightsThread);
    pthread_join (cold_channel->coldLoadInput, NULL);
    cold_channel->coldLoadInputSetup = FALSE;
//    fprintf ( stderr, "after join input\n");
    pthread_join (plightsThread, NULL);
  }
  stop_cold_x ();
}

void __attribute__ ((constructor)) cold_load_init(void);
void cold_load_init( void ) {

  XInitThreads() ;

  if (haveXcb == XcbUnknown) {
    if (dlsym(RTLD_NEXT,"xcb_disconnect")) {
      haveXcb = XcbLoaded ;
      vwarn("cold load init","using Xlib with xcb");
    }  else {
      haveXcb = XcbNotLoaded ;
      vwarn("cold load init","using Xlib without xcb");
    }
  }
}
// jj
