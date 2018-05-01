#ifndef _COLD_LOAD_KEYMAPPINGS_
#define COLD_LOAD_KEYMAPPINGS_

#define XK_MISCELLANY
#define XK_XKB_KEYS
#define XK_LATIN1
#include <X11/keysymdef.h>
#include <X11/DECkeysym.h>

/* Keyboard mappings for the Cold Load window for various keyboard types */

enum KeyboardType {
  Unknown,
  DEC_LK401,
  DEC_PC,
  Apple_Pro,
  German,
  Us
 };

typedef struct {
  short code;
  KeySym keysym;
} coldmapentry;

typedef struct {
  KeySym code;
  KeySym keysym;
} isol3mapentry;


/* DEC LK401 keyboard */

static coldmapentry coldmapDECLK[] = {
	{ SK_Rubout, XK_Delete },
	{ SK_Help, XK_Help },
	{ SK_Backspace, XK_BackSpace },
	{ SK_Backspace, DXK_Remove },
	{ SK_Tab, XK_Tab },
	{ SK_Line, XK_Linefeed },
	{ SK_Return, XK_Return },
	{ SK_Escape, XK_Escape },
	{ SK_Complete, XK_Find },
	{ SK_Line, XK_Insert },
	{ SK_Select, XK_Select },
	{ SK_Scroll, XK_Next },
	{ SK_End, XK_Menu },
	{ SK_End, XK_KP_Enter },
	{ SK_Refresh, XK_KP_F1 },
	{ SK_Page, XK_KP_F2 },
	{ -1, -1} };

static short fkmapDECLK[] = {
  SK_Function, SK_Function,			/* F1 */
  SK_Escape, SK_Escape,				/* F2 */
  /* F3-F5 are intercepted by DEC's window manager and, therefore, aren't usable in Genera */
  -1, -1,					/* F3 */
  -1, -1,					/* F4 */
  -1, -1,					/* F5 */
  SK_Network, SK_Network,			/* F6 */
  /* F7-F11 are intercepted by DEC's window manager and, therefore, aren't usable in Genera */
  -1, -1,					/* F7 */
  -1, -1,					/* F8 */
  -1, -1,					/* F9 */
  -1, -1,					/* F10 */
  -1, -1,					/* F11 */
  SK_Square, SK_Square,				/* F12 */
  SK_Circle, SK_Circle,				/* F13 */
  SK_Triangle, SK_Triangle,			/* F14 */
  SK_Help, SK_Help,				/* F15 (Help) */
  SK_End, SK_End,				/* F16 (Menu or Do) */
  SK_Clear_Input, SK_Clear_Input,		/* F17 */
  SK_Suspend, SK_Suspend,			/* F18 */
  SK_Resume, SK_Resume,				/* F19 */
  SK_Abort, SK_Abort,				/* F20 */
  /* F21-F23 do not exist on DEC keyboards */
  -1, -1,					/* F21 */
  -1, -1,					/* F22 */
  -1, -1,					/* F23 */
  };


/* DEC PC-style keyboard */

static coldmapentry coldmapDECPC[] = {
	{ SK_Rubout, XK_Delete },
	{ SK_Help, XK_Insert },
	{ SK_Backspace, XK_BackSpace },
	{ SK_Backspace, DXK_Remove },
	{ SK_Tab, XK_Tab },
	{ SK_Line, XK_Linefeed },
	{ SK_Line, XK_Right },
	{ SK_Return, XK_Return },
	{ SK_Escape, XK_Escape },
	{ SK_Complete, XK_Find },
	{ SK_Complete, XK_Home },
	{ SK_Line, XK_Right },
	{ SK_Select, XK_Select },
	{ SK_Scroll, XK_Next },
	{ SK_End, XK_Menu },
	{ SK_End, XK_KP_Enter },
	{ SK_Refresh, XK_KP_F1 },
	{ SK_Page, XK_KP_F2 },
	{ SK_Page, XK_Page_Up },
	{ SK_Suspend, XK_KP_Divide },
	{ SK_Resume, XK_KP_Multiply },
	{ SK_Abort, XK_KP_Subtract },
	{ -1, -1} };
     
static short fkmapDECPC[] = {
  SK_Select, SK_Select,				/* F1 */
  SK_Function, SK_Function,			/* F2 */
  SK_Network, SK_Network,			/* F3 */
  SK_Escape, SK_Escape,                		/* F4 */
  SK_Refresh, SK_Refresh,			/* F5 */  
  SK_Square, SK_Square,				/* F6 */
  SK_Circle, SK_Circle,				/* F7 */
  SK_Triangle, SK_Triangle,			/* F8 */
  SK_Clear_Input, SK_Clear_Input,		/* F9 */
  -1, -1,					/* F10 */
  SK_Help, SK_Help,                             /* F11 */
  SK_Backspace, SK_Backspace,                   /* F12 */
  -1, -1,					/* F13 */
  -1, -1,					/* F14 */
  SK_Help, SK_Help,				/* F15 */
  SK_End, SK_End,				/* F16 */
  SK_Clear_Input, SK_Clear_Input,		/* F17 */
  SK_Suspend, SK_Suspend,			/* F18 */
  SK_Resume, SK_Resume,				/* F19 */
  SK_Abort, SK_Abort,				/* F20 */
  -1, -1,					/* F21 */
  -1, -1,					/* F22 */
  -1, -1,					/* F23 */
  };


/* Apple Keyboard */

/* No mappings for -- SK_Square, SK_Circle, SK_Triangle, SK_Clear_Input */
static coldmapentry coldmapApple[] = {
	{ SK_Clear_Input, 0 },
	{ SK_Rubout, XK_Delete },
	{ SK_Help, XK_Insert },
	{ SK_Backspace, XK_BackSpace },
	{ SK_Tab, XK_Tab },
	{ SK_Line, XK_Right },
	{ SK_Return, XK_Return },
	{ SK_Escape, XK_Escape },
	{ SK_Complete, XK_Home },
	{ SK_End, XK_End },
	{ SK_Scroll, XK_KP_Enter },
	{ SK_Page, XK_Prior },
	{ SK_Suspend, XK_KP_Equal },
	{ SK_Resume, XK_KP_Divide },
	{ SK_Abort, XK_KP_Multiply },
	{ -1, -1} };
     
static short fkmapApple[] = {
  SK_Select, SK_Select,				/* F1 */
  SK_Function, SK_Function,			/* F2 */
  SK_Network, SK_Network,			/* F3 */
  SK_Refresh, SK_Refresh,              		/* F4 */
  -1, -1,					/* F5 */
  -1, -1,					/* F6 */
  -1, -1,					/* F7 */
  -1, -1,					/* F8 */
  -1, -1,					/* F9 */
  -1, -1,					/* F10 */
  -1, -1,					/* F11 */
  -1, -1,					/* F12 */
  -1, -1,					/* F13 */
  -1, -1,					/* F14 */
  -1, -1,					/* F15 */
  -1, -1,					/* F16 */
  -1, -1,					/* F17 */
  -1, -1,					/* F18 */
  -1, -1,					/* F19 */
  -1, -1,					/* F20 */
  -1, -1,					/* F21 */
  -1, -1,					/* F22 */
  -1, -1,					/* F23 */
  };

/* German Keyboard */

static coldmapentry coldmapGerman[] = {
	{ XK_KP_Begin, XK_ISO_Level3_Shift },
  //  SK_Clear_Input, XK_F10,				/* Depends on X server */
	{ SK_Rubout, XK_Delete },
  //  SK_Help, XK_Insert,
	{ SK_Backspace, XK_BackSpace },
	{ SK_Tab, XK_Tab },
  //  SK_Line, XK_Right,
	{ SK_Return, XK_Return },
	{ SK_Escape, XK_Escape },
  //  SK_Complete, XK_Home,
	{ SK_End, XK_End },
  //  SK_Scroll, XK_KP_Enter,
	{ SK_Page, XK_Prior },
  //  SK_Suspend, XK_KP_Equal,
  //  SK_Resume, XK_KP_Divide,
  //  SK_Abort, XK_KP_Multiply,
	{ -1, -1} };
     
static short fkmapGerman[] = {
  SK_Select, SK_Square,				/* F1 */
  SK_Network, SK_Circle,			/* F2 */
  SK_Function, SK_Triangle,			/* F3 */
  SK_Suspend, SK_Suspend,                       /* F4 */
  SK_Resume, SK_Resume,				/* F5 */
  SK_Abort, SK_Abort,				/* F6 */
  SK_Help, SK_Help,				/* F7 */
  SK_Refresh, SK_Refresh,			/* F8 */
  SK_Scroll, SK_Page,				/* F9 */
  SK_Clear_Input, SK_Clear_Input,		/* F10 */
  SK_Complete, SK_End,  			/* F11 */
  SK_Help, SK_Help,				/* F12 */
  -1, -1,					/* F13 */
  -1, -1,					/* F14 */
  -1, -1,					/* F15 */
  -1, -1,					/* F16 */
  -1, -1,					/* F17 */
  -1, -1,					/* F18 */
  -1, -1,					/* F19 */
  -1, -1,					/* F20 */
  -1, -1,					/* F21 */
  -1, -1,					/* F22 */
  -1, -1,					/* F23 */
  };

/* US Keyboard */

static coldmapentry coldmapUs[] = {
	{ XK_KP_Begin, XK_ISO_Level3_Shift },
  //  SK_Clear_Input, XK_F10,				/* Depends on X server */
	{ SK_Rubout, XK_Delete },
  //  SK_Help, XK_Insert,
	{ SK_Backspace, XK_BackSpace },
	{ SK_Tab, XK_Tab },
  //  SK_Line, XK_Right,
	{ SK_Return, XK_Return },
	{ SK_Escape, XK_Escape },
  //  SK_Complete, XK_Home,
	{ SK_End, XK_End },
  //  SK_Scroll, XK_KP_Enter,
	{ SK_Page, XK_Prior },
  //  SK_Suspend, XK_KP_Equal,
  //  SK_Resume, XK_KP_Divide,
  //  SK_Abort, XK_KP_Multiply,
	{ -1, -1} };
     
static short fkmapUs[] = {
  SK_Select, SK_Square,				/* F1 */
  SK_Network, SK_Circle,			/* F2 */
  SK_Function, SK_Triangle,			/* F3 */
  SK_Suspend, SK_Suspend,                       /* F4 */
  SK_Resume, SK_Resume,				/* F5 */
  SK_Abort, SK_Abort,				/* F6 */
  SK_Help, SK_Help,				/* F7 */
  SK_Refresh, SK_Refresh,			/* F8 */
  SK_Scroll, SK_Page,				/* F9 */
  SK_Clear_Input, SK_Clear_Input,		/* F10 */
  SK_Complete, SK_End,  			/* F11 */
  SK_Help, SK_Help,				/* F12 */
  -1, -1,					/* F13 */
  -1, -1,					/* F14 */
  -1, -1,					/* F15 */
  -1, -1,					/* F16 */
  -1, -1,					/* F17 */
  -1, -1,					/* F18 */
  -1, -1,					/* F19 */
  -1, -1,					/* F20 */
  -1, -1,					/* F21 */
  -1, -1,					/* F22 */
  -1, -1,					/* F23 */
  };

/* static isol3mapentry isol3Map[] = { */
/* 	{ XK_at , XK_q },                                /\* @ : q *\/ */
/* 	{ XK_bracketleft , XK_8 },                       /\* [ : 8 *\/ */
/* 	{ XK_bracketright , XK_9 },                      /\* ] : 9 *\/ */
/* 	{ XK_braceleft, XK_7 },                          /\* { : 7 *\/ */
/* 	{ XK_braceright, XK_0 },                         /\* } : 0 *\/   */
/* 	{ XK_backslash, XK_ssharp },                     /\* \ : ß *\/ */
/* 	{ XK_asciitilde, XK_plus },                      /\* ~ : + *\/ */
/* 	{ XK_bar, XK_less },                             /\* | : < *\/ */
/* 	{ -1, -1 } */
/* }; */
#endif
