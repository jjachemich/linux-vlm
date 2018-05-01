/* Miscellaneous utility routines */

#include "std.h"

#include <stdarg.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <linux/unistd.h>

#include <X11/Xlib.h>
#include <X11/Xresource.h>

#include "VLM_configuration.h"
#include "life_types.h"
#include "ivoryrep.h"
#include "chaos.h"

#if defined(GENERA)
static char* CommandName = "genera";
#define CommandClass "Genera"

#elif defined(MINIMA)
static char* CommandName = "minima";
#define CommandClass "Minima"

#elif defined(IVERIFY)
static char* CommandName = "iverify";
#define CommandClass "IVerify"
#endif


/* Internal function prototypes */

static boolean	GetOption (XrmDatabase options, char* name, char* class, char* value);
static boolean	GetXOption (XrmDatabase options, char* windowName, char* windowClass, 
						    char* name, char* class, char* value);
static void		GetDefaultConfiguration (VLMConfig* config, XrmDatabase* options);
static void		InterpretNetworkOptions (VLMConfig* config, XrmDatabase options);
static void		InterpretOptions (VLMConfig* config, XrmDatabase options);
static void		InterpretXOptions (XrmDatabase options, XParams* xParams, 
								   char* windowEnglishName, char* windowName,
								   char* windowClass);
static void		MaybeReadConfigurationFile (VLMConfig* config, XrmDatabase* options, 
										    char* pathname);
static char*	MergeSearchPaths (char* newSearchPath, char* oldSearchPath);
static int		PrintMessage (char* section, char* format, va_list arguments);
static void		ProcessCommandArguments (VLMConfig* config, XrmDatabase* options, 
										 int argc, char** argv);
static boolean	VerifyHostName (char* name, char** hostName, unsigned long* hostAddress,
								boolean rejectLocalHost);


/* Guts of the following message printing functions */

static int PrintMessage (char* section, char* format, va_list arguments)
{
  char name[128];

	if (NULL == section)
		sprintf (name, "%s: ", CommandName);
	else
		sprintf (name, "%s (%s): ", CommandName, section);

	fprintf (stderr, "%s", name);

	if (format != NULL)
	  {
		vfprintf (stderr, format, arguments);
		fprintf (stderr, "\n");
	  }

	return (strlen (name));
}


/* Print an error message and terminate the VLM */

void vpunt (char* section, char* format, ...)
{
  va_list ap;
  char *errmsg;
  int prefixLength;

	va_start (ap, format);

	prefixLength = PrintMessage (section, format, ap);

	if (errno)
	  {
		errmsg = strerror (errno);
		if (NULL == format)
			fprintf (stderr, "%s\n", errmsg);
		else
			fprintf (stderr, "%*s%s\n", prefixLength, "", errmsg);
	  }

	va_end (ap);

//    MaybePrintTrace ();

//while (1);
	exit (EXIT_FAILURE);
}


/* Print an error message */

void verror (char* section, char* format, ...)
{
  va_list ap;
  char *errmsg;
  int prefixLength;

	va_start (ap, format);

	prefixLength = PrintMessage (section, format, ap);

	if (errno)
	  {
		errmsg = strerror (errno);
		if (NULL == format)
			fprintf (stderr, "%s\n", errmsg);
		else
			fprintf (stderr, "%*s%s\n", prefixLength, "", errmsg);
	  }

	va_end (ap);
}


/* Print a warning */

void vwarn (char* section, char* format, ...)
{
  va_list ap;

	va_start (ap, format);

	PrintMessage (section, format, ap);

	va_end (ap);
}


/* Change the command name used by vpunt and vwarn */

void SetCommandName (char* newCommandName)
{
#if defined(OS_OSF) || defined(__FreeBSD__)
	CommandName = strdup (newCommandName);
#else
	CommandName = strndup (newCommandName, 32);
#endif
}


/* Creates an X display name string in the supplied buffer */

void BuildXDisplayName (char* displayName, char* hostName, int display, int screen)
{
	sprintf (displayName, "%s", hostName == NULL ? "" : hostName);

	if (display != -1 || screen != -1)
	  {
		sprintf (displayName, "%s:", displayName);
		if (display != -1)
			sprintf (displayName, "%s%d", displayName, display);
		if (screen != -1)
			sprintf (displayName, "%s.%d", displayName, screen);
	  }
}



/* Determine the VLM configuration */

void BuildConfiguration (VLMConfig* config, int argc, char** argv)
{
  XrmDatabase options = NULL;
  char *homeDir, workingDir[_POSIX_PATH_MAX+1], configFile[_POSIX_PATH_MAX+1];

	XrmInitialize ();

	GetDefaultConfiguration (config, &options);

	MaybeReadConfigurationFile (config, &options, DefaultVLMConfigFilePathname);

	if ((homeDir = getenv ("HOME")) != NULL)
	  {
		sprintf (configFile, "%s/.VLM", homeDir);
		MaybeReadConfigurationFile (config, &options, configFile);
	  }

	if (getcwd (workingDir, sizeof (workingDir)))
	  {
		sprintf (configFile, "%s/.VLM", workingDir);
		MaybeReadConfigurationFile (config, &options, configFile);
	  }

	ProcessCommandArguments (config, &options, argc, argv);

	InterpretOptions (config, options);
}


/* Fill in the default options for the VLM */

static void GetDefaultConfiguration (VLMConfig* config, XrmDatabase* options)
{
  char *display, *worldSearchPath;
  int i;

	XrmPutStringResource (options, "*spy", "no");

	XrmPutStringResource (options, "*trace", "no");
	XrmPutStringResource (options, "*tracePOST", "no");

    XrmPutStringResource (options, "*testfunction", "no");

	config->commAreaSize = DefaultEmbCommAreaSize;
	config->hostBufferSpace = DefaultHostBufferSpace;
	config->guestBufferSpace = DefaultGuestBufferSpace;

	XrmPutStringResource (options, "*debugger", DefaultVLMDebuggerPathname);

	for (i = 0; i < MaxNetworkInterfaces; i++)
	  {
		config->interfaces[i].present = FALSE;
	  }

    XrmPutStringResource (options, "genera.world", DefaultGeneraWorldPathname);
    XrmPutStringResource (options, "minima.world", DefaultMinimaWorldPathname);

	if ((worldSearchPath = getenv ("WORLDPATH")) != NULL)
		XrmPutStringResource (options, "genera.worldSearchPath", 
							  MergeSearchPaths (worldSearchPath, DefaultWorldSearchPath));
	else
		XrmPutStringResource (options, "genera.worldSearchPath", DefaultWorldSearchPath);

	XrmPutStringResource (options, "genera.enableIDS", "no");
	XrmPutStringResource (options, "genera.virtualMemory", DefaultVirtualMemory);

	if ((display = getenv ("DISPLAY")) != NULL)
		XrmPutStringResource (options, "*display", display);
	else
		XrmPutStringResource (options, "*display", ":0.0");

	XrmPutStringResource (options, "*coldLoad.iconic", "yes");

}


/* Read a .VLM file in the specified directory if it exists */

static void MaybeReadConfigurationFile (VLMConfig* config, XrmDatabase* options,
									    char* pathname)
{
  XrmDatabase fileOptions;
  char newSearchPath[_POSIX_ARG_MAX], oldSearchPath[_POSIX_ARG_MAX], *mergedSearchPath,
	   searchPathOption[128];
  int fd;

	fd = open (pathname, O_RDONLY);
	if (-1 == fd) {
		if (ENOENT == errno)
		  {
			errno = ESUCCESS;
			return;
		  }
		else
			vpunt (NULL, "Unable to verify existence of configuration file %s", pathname);
	}
	close (fd);

	fileOptions = XrmGetFileDatabase (pathname);
	if (NULL == fileOptions)
		vpunt (NULL, "Unable to parse configuration file %s", pathname);

#ifdef GENERA
	if (GetOption (fileOptions, "worldSearchPath", "WorldSearchPath", newSearchPath))
	  {
		GetOption (*options, "worldSearchPath", "WorldSearchPath", oldSearchPath);
		mergedSearchPath = MergeSearchPaths (newSearchPath, oldSearchPath);
		sprintf (searchPathOption, "%s.worldSearchPath", CommandName);
		XrmPutStringResource (&fileOptions, searchPathOption, mergedSearchPath);
	  }
#endif

	XrmMergeDatabases (fileOptions, options);
}


/* The command line arguments to a VLM command */

#if defined(GENERA)
#define BaseOptions 33

#elif defined(MINIMA)
#define BaseOptions 5

#elif defined(IVERIFY)
#define BaseOptions 3
#endif

#ifdef TRACING
#define TracingOptions 2
#else
#define TracingOptions 0
#endif

#define OptionsTableSize BaseOptions+TracingOptions

static XrmOptionDescRec OptionsTable[OptionsTableSize] = {
	{"-spy",					".spy",						XrmoptionSepArg,	NULL},
	{"-diagnostic",				".diagnosticHost",			XrmoptionSepArg,	NULL},
	{"-testfunction",           ".testfunction",            XrmoptionSepArg,    NULL},
#ifndef IVERIFY
	{"-world",					".world",					XrmoptionSepArg,	NULL},
	{"-network",				".network",					XrmoptionSepArg,	NULL},
#endif
#ifdef GENERA
	{"-debugger",				".debugger",				XrmoptionSepArg,	NULL},
	{"-ids",					".enableIDS",				XrmoptionSepArg,	NULL},
	{"-vm",						".virtualMemory",			XrmoptionSepArg,	NULL},
	{"-display",				".main.display",			XrmoptionSepArg,	NULL},
	{"-geometry",				".main.geometry",			XrmoptionSepArg,	NULL},
	{"-iconic",					".main.iconic",				XrmoptionSepArg,	NULL},
	{"-foreground",				".main.foreground",			XrmoptionSepArg,	NULL},
	{"-fg",						".main.foreground",			XrmoptionSepArg,	NULL},
	{"-background",				".main.background",			XrmoptionSepArg,	NULL},
	{"-bg",						".main.background",			XrmoptionSepArg,	NULL},
	{"-bordercolor",			".main.borderColor",		XrmoptionSepArg,	NULL},
	{"-bd",						".main.borderColor",		XrmoptionSepArg,	NULL},
	{"-borderwidth",			".main.borderWidth",		XrmoptionSepArg,	NULL},
	{"-bw",						".main.borderWidth",		XrmoptionSepArg,	NULL},
	{"-coldloaddisplay",		".coldLoad.display",		XrmoptionSepArg,	NULL},
	{"-cld",					".coldLoad.display",		XrmoptionSepArg,	NULL},
	{"-coldloadgeometry",		".coldLoad.geometry",		XrmoptionSepArg,	NULL},
	{"-clg",					".coldLoad.geometry",		XrmoptionSepArg,	NULL},
	{"-coldloadiconic",			".coldLoad.iconic",			XrmoptionSepArg,	NULL},
	{"-cli",					".coldLoad.iconic",			XrmoptionSepArg,	NULL},
	{"-coldloadforeground",		".coldLoad.foreground",		XrmoptionSepArg,	NULL},
	{"-clfg",					".coldLoad.foreground",		XrmoptionSepArg,	NULL},
	{"-coldloadbackground",		".coldLoad.background",		XrmoptionSepArg,	NULL},
	{"-clbg",					".coldLoad.background",		XrmoptionSepArg,	NULL},
	{"-coldloadbordercolor",	".coldLoad.borderColor",	XrmoptionSepArg,	NULL},
	{"-clbd",					".coldLoad.borderColor",	XrmoptionSepArg,	NULL},
	{"-coldloadborderwidth",	".coldLoad.borderWidth",	XrmoptionSepArg,	NULL},
	{"-clbw",					".coldLoad.borderWidth",	XrmoptionSepArg,	NULL},
#endif
#ifdef TRACING
	{"-trace",					".trace",					XrmoptionSepArg,	NULL},
	{"-tracePOST",				".tracePOST",				XrmoptionSepArg,	NULL},
#endif
};


/* Parse the command line arguments */

static void ProcessCommandArguments (VLMConfig* config, XrmDatabase* options, 
									 int argc, char** argv)
{
  char oldSearchPath[_POSIX_ARG_MAX], *mergedSearchPath, searchPathOption[128];
  int argLength;

	XrmParseCommand (options, OptionsTable, OptionsTableSize, CommandName, &argc, argv);

	/* Any leftover arguments must be "-searchpath PATH" */

	while (argc > 1)
	  {
		argv++;
		argc--;

#ifdef GENERA
		argLength = strlen (*argv);
		if (0 == strncmp (*argv, "-searchpath", (argLength < 7) ? 7 : argLength))
		  {
			if (argc > 1)
			  {
				argv++;
				argc--;
				GetOption (*options, "worldSearchPath", "WorldSearchPath", oldSearchPath);
				mergedSearchPath = MergeSearchPaths (*argv, oldSearchPath);
				sprintf (searchPathOption, "%s.worldSearchPath", CommandName);
				XrmPutStringResource (options, searchPathOption, mergedSearchPath);
			  }
			else
				vpunt (NULL, "A list of directory pathnames must follow -searchpath");
		  }

		else
#endif
			vpunt (NULL, "Unrecognized option %s", *argv);
	  }
}


/* Convert the options found above from strings into our internal representations */

static void InterpretOptions (VLMConfig* config, XrmDatabase options)
{
  NetworkInterface *interface;
  char value[_POSIX_ARG_MAX], *hostName, *end;
  unsigned long hostAddress, datum;
  int i;

	GetOption (options, "spy", "Spy", value);
	if (0 == strcmp (value, "yes"))
		config->enableSpy = TRUE;
	else if (0 == strcmp (value, "no"))
		config->enableSpy = FALSE;
	else
		vpunt (NULL, "Value of spy parameter, %s, is invalid", value);

	GetOption (options, "testfunction", "TestFunction", value);
	if (0 == strcmp (value, "yes"))
		config->testFunction = TRUE;
	else if (0 == strcmp (value, "no"))
		config->testFunction = FALSE;
	else
		vpunt (NULL, "Value of testfunction parameter, %s, is invalid", value);

#ifndef TRACING
	config->tracing.traceP = FALSE;
	config->tracing.tracePOST = FALSE;
#endif

#ifdef TRACING
	config->tracing.bufferSize = 25000;
	config->tracing.startPC = 0;
	config->tracing.stopPC = 0;
	config->tracing.outputFile = NULL;

	GetOption (options, "trace", "Trace", value);

	if (0 == strcmp (value, "yes"))
		config->tracing.traceP = TRUE;

	else if (0 == strcmp (value, "no"))
		config->tracing.traceP = FALSE;

	else
	  {
		config->tracing.traceP = TRUE;
		start = value;
		datum = strtoul (start, &end, 10);
		if (start != end)
			config->tracing.bufferSize = datum;
		else
		  {
			if (*end == '[')
			  {
				end2 = strrchr (start, ']');
				if (end2)
				  {
					*end2 = '\0';
#if defined(OS_OSF) || defined(__FreeBSD__)
					config->tracing.outputFile = strdup (start + 1);
#else
					config->tracing.outputFile = strndup (start + 1, _POSIX_PATH_MAX + 1);
#endif
					*end2 = ']';
					end = end2 + 1;
				  }
				else
					vpunt (NULL, "Value of trace parameter, %s, is invalid", value);
			  }
		  }
		if (*end)
		  {
			if (*end == ',')
			  {
				start = end + 1;
				datum = strtoul (start, &end, 0);
				if (start != end)
					config->tracing.startPC = datum;
				if (*end)
				  {
					if (*end == ',')
					  {
						start = end + 1;
						datum = strtoul (start, &end, 0);
						if (start != end)
							config->tracing.stopPC = datum;
						if (*end)
							vpunt (NULL, "Value of trace parameter, %s, is invalid", value);
					  }
					else
						vpunt (NULL, "Value of trace parameter, %s, is invalid", value);
				  }
			  }
			else
				vpunt (NULL, "Value of trace parameter, %s, is invalid", value);
		  }
	  }

	GetOption (options, "tracePOST", "TracePOST", value);

	if (0 == strcmp (value, "yes"))
		config->tracing.tracePOST = TRUE;

	else if (0 == strcmp (value, "no"))
		config->tracing.tracePOST = FALSE;

	else
		vpunt (NULL, "Value of tracePOST parameter, %s, is invalid", value);
#endif

#ifndef IVERIFY
	GetOption (options, "world", "World", value);
	strcpy (config->worldPath, value);

	InterpretNetworkOptions (config, options);

#ifdef GENERA
	GetOption (options, "debugger", "Debugger", value);
	strcpy (config->vlmDebuggerPath, value);

	GetOption (options, "enableIDS", "EnableIDS", value);
	if (0 == strcmp (value, "yes"))
		config->enableIDS = TRUE;
	else if (0 == strcmp (value, "no"))
		config->enableIDS = FALSE;
	else
		vpunt (NULL, "Value of enable IDS parameter, %s, is invalid", value);

	GetOption (options, "virtualMemory", "VirtualMemory", value);
	datum = strtoul (value, &end, 10);
	if (*end)
		vpunt (NULL, "Value of virtual memory size parameter, %s, is invalid", value);
	if (datum < MinimumVirtualMemory)
		vpunt (NULL, "Minimum virtual memory size is %d megabytes", MinimumVirtualMemory);
	config->virtualMemory = datum;

	GetOption (options, "worldSearchPath", "WorldSearchPath", value);
	config->worldSearchPath = strdup (value);

	InterpretXOptions (options, &config->generaXParams, "main X console", "main", "Main");
	InterpretXOptions (options, &config->coldLoadXParams, "cold load", "coldLoad", "ColdLoad");
#endif
#endif

#ifndef MINIMA
	if (config->enableSpy)
#endif
	  {
		if (GetOption (options, "diagnosticHost", "DiagnosticHost", value))
		  {
			if (VerifyHostName (value, &hostName, &hostAddress, FALSE))
				memcpy ((char*)&config->diagnosticIPAddress.s_addr, (char*)&hostAddress,
						sizeof (config->diagnosticIPAddress.s_addr));
			else
				vpunt (NULL, "Unknown diagnostic host %s", value);
		  }
		else
#ifdef MINIMA
			vpunt (NULL, "You must specify a diagnostic host.");
#else
		  {
			config->diagnosticIPAddress.s_addr = 0;
			for (i = 0; 
				 (i < MaxNetworkInterfaces) && (0 == config->diagnosticIPAddress.s_addr);
				 i++)
			  {
				interface = &config->interfaces[i];
				while ((interface != NULL) && interface->present)
				  {
					if (ETHERTYPE_IP == interface->myProtocol)
					  {
						config->diagnosticIPAddress.s_addr = htonl (interface->myAddress.s_addr);
						break;
					  }
#ifdef GENERA
					interface = interface->anotherAddress;
#else
					interface = NULL;
#endif
				  }
			  }

			if (0 == config->diagnosticIPAddress.s_addr)
				vpunt (NULL, "You must specify a diagnostic host to use the spy.");
		  }
#endif
	  }
}


/* Convert the network interfaces specification into one or more interface definitions */

static void InterpretNetworkOptions (VLMConfig* config, XrmDatabase options)
{
	NetworkInterface *mainInterface, *interface, *ointerface = NULL;
  char buffer[_POSIX_ARG_MAX], *value, *deviceName, *hostName,
	   *commaPosition, *colonPosition, *semicolonPosition, *end;
  unsigned long hostAddress;
  int i;
 
	if (!GetOption (options, "network", "Network", buffer))
		vpunt (NULL, "At least one network interface must be defined");

	value = &buffer[0];

	while ((value != NULL) && *value)
	  {
		commaPosition = strchr (value, ',');
		if (commaPosition != NULL)
			*commaPosition = 0;

		colonPosition = strchr (value, ':');
		semicolonPosition = strchr (value, ';');
	
		if ((colonPosition != NULL) && (semicolonPosition != NULL) && 
									   (semicolonPosition < colonPosition))
			vpunt (NULL, "Invalid syntax in specification of network interface: %s", value);
	
		if (colonPosition != NULL)
		  {
			*colonPosition = 0;
			deviceName = strdup (value);
			value = colonPosition + 1;
		  }
		else
			deviceName = "";
	
		interface = NULL;
		for (i = 0; i < MaxNetworkInterfaces; i++)
			if (config->interfaces[i].present)
				if (0 == strcmp (deviceName, config->interfaces[i].device))
				  {
					mainInterface = &config->interfaces[i];
					interface = mainInterface;
#ifndef GENERA
					vpunt (NULL, "Only one network address per interface is supported");
#else
					if (!interface->haveMac) {
						while (interface->anotherAddress != NULL)
							interface = interface->anotherAddress;
						ointerface = interface;
						interface->anotherAddress = malloc (sizeof (NetworkInterface));
						if (NULL == interface->anotherAddress)
							vpunt (NULL,
							       "Unable to allocate space for an additional network address");
						memset(interface->anotherAddress,
						       0, 
						       sizeof(NetworkInterface));
						interface = interface->anotherAddress;
					}
#endif
					break;
				  }
				else;
			else
			  {
				interface = mainInterface = &config->interfaces[i];
				break;
			  }

		if (NULL == interface)
		  {
			if (commaPosition != NULL) *commaPosition = ',';
			if (colonPosition != NULL) *colonPosition = ':';
			if (semicolonPosition != NULL) *semicolonPosition = ';';
			vpunt (NULL, "Too many distinct network interfaces in %s", buffer);
		  }

		strcpy (interface->device, deviceName);

		if (semicolonPosition != NULL)
			*semicolonPosition = 0;
	
		if ((0 == strncasecmp (value, "MAC|", strlen ("MAC|"))))
		  {
			  struct mac_addr mac;
			  char c;

			value += strlen ("MAC|");
			if (6 != sscanf(value, 
					"%hhx:%hhx:%hhx:%hhx:%hhx:%hhx%c",
					&mac.bytes[0],
					&mac.bytes[1],
					&mac.bytes[2],
					&mac.bytes[3],
					&mac.bytes[4],
					&mac.bytes[5],
					&c))
				vpunt(NULL,
				      "failed to parse MAC address %s",
				      value);
			if (ointerface) {
				free(interface);
				interface = ointerface;
			}
			interface->haveMac = TRUE;
			interface->myMac = mac;
		  }	
		else if ((0 == strncmp (value, "CHAOS|", strlen ("CHAOS|"))) || 
			 (0 == strncmp (value, "chaos|", strlen ("chaos|"))))
		{
			value += strlen ("CHAOS|");
			interface->myProtocol = ETH_P_CHAOS;
			hostAddress = strtoul (value, &end, 8);
			if (*end)
			  {
				if (colonPosition != NULL) *colonPosition = ':';
				if (semicolonPosition != NULL) *semicolonPosition = ';';
				vpunt (NULL, "Invalid chaos address in specification of network interface: %s",
					   value);
			  }
			else
				interface->myAddress.s_addr = ntohl (hostAddress);
		  }
	
		else if ((0 == strncmp (value, "INTERNET|", strlen ("INTERNET|"))) || 
				 (0 == strncmp (value, "internet|", strlen ("internet|"))))
		  {
			value += strlen ("INTERNET|");
			interface->myProtocol = ETHERTYPE_IP;
			hostAddress = ntohl (inet_addr (value));
			if (hostAddress == ntohl (-1))
			  {
				if (colonPosition != NULL) *colonPosition = ':';
				if (semicolonPosition != NULL) *semicolonPosition = ';';
				vpunt (NULL,
					   "Invalid Internet address in specification of network interface: %s",
					   value);
			  }
			else
				interface->myAddress.s_addr = hostAddress;
		  }

		else
		  {
			interface->myProtocol = ETHERTYPE_IP;
			if (VerifyHostName (value, &hostName, &hostAddress, TRUE))
			  {
				memcpy ((char*)&interface->myAddress.s_addr, (char*)&hostAddress,
						sizeof (interface->myAddress.s_addr));
				interface->myAddress.s_addr = ntohl (interface->myAddress.s_addr);
			  }
			else
			  {
				if (colonPosition != NULL) *colonPosition = ':';
				if (semicolonPosition != NULL) *semicolonPosition = ';';
				vpunt (NULL, "Unknown host in specification of network interface: %s", value);
			  }
		  }
	
#ifdef GENERA
		interface->myHostAddress.s_addr = 0;
		if (semicolonPosition != NULL) {
			char *hp = NULL;
			strcpy (interface->myOptions, semicolonPosition + 1);
			if ((hp = strcasestr(interface->myOptions,"host="))) {
				hp += strlen("host=");
				switch(interface->myProtocol) {
				case ETH_P_CHAOS:
					hostAddress = strtoul(hp, &end, 8);
					if (hostAddress)
						interface->myHostAddress.s_addr = hostAddress;
					else
						vpunt("network options",
						      "%s: could not parse host address \"%s\"",
						      interface->device,
						      hp);
					break;
				case ETH_P_IP:
					hostAddress = ntohl (inet_addr (hp));
					if (hostAddress != ntohl (-1))
						interface->myHostAddress.s_addr = hostAddress;
					else
						vpunt("network options",
						      "%s: could not parse host address \"%s\"",
						      interface->device,
						      hp);

					break;
				}
			}
		}
		else
			interface->myOptions[0] = 0;
		interface->anotherAddress = FALSE;
#endif
	
		interface->present = TRUE;

		value = (commaPosition != NULL) ? commaPosition + 1 : NULL;
	  }
}


/* Convert the options for an X window into our internal representation */

static void InterpretXOptions (XrmDatabase options, XParams* xParams, char* windowEnglishName,
							   char* windowName, char* windowClass)
{
  char value[_POSIX_ARG_MAX], *hostName, *colonPosition, *start, *end;
  unsigned long hostAddress, datum;

	GetXOption (options, windowName, windowClass, "display", "Display", value);

	colonPosition = strchr (value, ':');

	if (colonPosition != NULL)
	  {
		*colonPosition = 0;
		if (VerifyHostName (value, &hostName, &hostAddress, FALSE))
		  {
			xParams->xpHostName = hostName;
			xParams->xpHostAddress = hostAddress;
		  }
		else
			vpunt (NULL,
			       "Unknown host %s specified for display of %s",
			       value,
			       windowEnglishName);
		*colonPosition = ':';
		start = colonPosition + 1;
		datum = strtoul (start, &end, 10);
		if (start != end)
			xParams->xpDisplay = datum;
		if (*end)
		  {
			if (*end == '.')
			  {
				start = end + 1;
				datum = strtoul (start, &end, 0);
				if (start != end)
					xParams->xpScreen = datum;
				if (*end)
					vpunt (NULL,
					       "Invalid display specification %s for %s",
					       value,
					       windowEnglishName);
			  }
			else
				vpunt (NULL,
				       "Invalid display specification %s for %s",
				       value,
				       windowEnglishName);
		  }
		else
			xParams->xpScreen = -1;
	  }

	else
	  {
		if (VerifyHostName (value, &hostName, &hostAddress, FALSE))
		  {
			xParams->xpHostName = hostName;
			xParams->xpHostAddress = hostAddress;
		  }
		else
			vpunt (NULL,
			       "Unknown host %s specified for display of %s",
			       value,
			       windowEnglishName);
		xParams->xpDisplay = -1;
		xParams->xpScreen = -1;
	  }

	if (GetXOption (options, windowName, windowClass, "iconic", "Iconic", value))
	  {
		if (0 == strcmp (value, "yes"))
			xParams->xpInitialState = Iconic;
		else if (0 == strcmp (value, "no"))
			xParams->xpInitialState = Normal;
		else
			vpunt (NULL, "Invalid value, %s, for iconic state of %s", value,
				   windowEnglishName);
	  }
	else
		xParams->xpInitialState = Unspecified;

	if (GetXOption (options, windowName, windowClass, "geometry", "Geometry", value))
		xParams->xpGeometry = strdup (value);
	else
		xParams->xpGeometry = NULL;

	if (GetXOption (options, windowName, windowClass, "foreground", "Foreground", value))
		xParams->xpForegroundColor = strdup (value);
	else
		xParams->xpForegroundColor = NULL;

	if (GetXOption (options, windowName, windowClass, "background", "Background", value))
		xParams->xpBackgroundColor = strdup (value);
	else
		xParams->xpBackgroundColor = "white";

	if (GetXOption (options, windowName, windowClass, "borderColor", "BorderColor", value))
		xParams->xpBorderColor = strdup (value);
	else
		xParams->xpBorderColor = NULL;

	if (GetXOption (options, windowName, windowClass, "borderWidth", "BorderWidth", value))
	  {
		datum = strtoul (value, &end, 10);
		if (*end)
			vpunt (NULL, "Invalid value, %s, for border width of %s", value,
				   windowEnglishName);
		else
			xParams->xpBorderWidth = datum;
	  }
	else
		xParams->xpBorderWidth = -1;
}


/* Merge two world search paths */

static char* MergeSearchPaths (char* newSearchPath, char* oldSearchPath)
{
	newSearchPath = strdup (newSearchPath);

	if (0 == strncmp (newSearchPath, "+:", 2))
		newSearchPath = strcat (strdup (&newSearchPath[1]), oldSearchPath);

	if (0 == strncmp (newSearchPath + strlen (newSearchPath) - 2, ":+", 2))
	  {
		newSearchPath[strlen(newSearchPath)-1] = 0;
		newSearchPath = strcat (newSearchPath, oldSearchPath);
	  }

	return (newSearchPath);
}


/* Get the value of an option from the database */

static boolean GetOption (XrmDatabase options, char* name, char* class, char* value)
{
  char optionName[128], optionClass[128], *valueClass;
  XrmValue dbValue;

	sprintf (optionName, "%s.%s", CommandName, name);
	sprintf (optionClass, "%s.%s", CommandClass, class);

	if (XrmGetResource (options, optionName, optionClass, &valueClass, &dbValue))
	  {
		strncpy (value, dbValue.addr, dbValue.size);
		return (TRUE);					  
	  }

	else
		return (FALSE);
}


/* Get the value of an option for an X window from the database */

static boolean GetXOption (XrmDatabase options, char* windowName, char* windowClass,
						   char* name, char* class, char* value)
{
  char optionName[128], optionClass[128];

	sprintf (optionName, "%s.%s", windowName, name);
	sprintf (optionClass, "%s.%s", windowClass, class);

	return (GetOption (options, optionName, optionClass, value));
}


/* Convert a putative host name into an official name and address */

static boolean VerifyHostName (char* name, char** hostName, unsigned long* hostAddress,
							   boolean rejectLocalHost)
{
  struct hostent *hp;

	if (*name == '\0' || !strcmp (name, "unix") || !strcmp (name, "localhost"))
	  {
		if (rejectLocalHost)
			return (FALSE);
		if (NULL == (hp = gethostbyname ("localhost")))
			vpunt (NULL, "Unable to determine local host network address");
		*hostAddress = *(unsigned long*) hp->h_addr;
		*hostName = (*name == '\0') ? NULL : strdup ("localhost");
	  }

	else if ((hp = gethostbyname (name)) != NULL)
	  {
		*hostAddress = *(unsigned long*) hp->h_addr;
		*hostName = strdup (hp->h_name);
	  }

	else if ((*hostAddress = ntohl (inet_addr (name))) == ntohl (-1))
	  {
		if (EWOULDBLOCK == errno) errno = ESUCCESS;
		return (FALSE);
	  }

	else
		/* Here iff name is a valid Internet address */
		*hostName = strdup (name);

	return (TRUE);
}


#ifndef OS_OSF

/* Time-related thread "primitives" that were part of OSF and used througout the emulator */

/* Convert an interval to an absolute time */

#define NSECS_IN_USEC 1000
#define NSECS_IN_SEC (1000 * 1000 * 1000)

int pthread_get_expiration_np (const struct timespec *delta, struct timespec *abstime)
{
  int status;
  //  struct timeval now;
  //  struct timezone obsolete;

  struct timespec now ;

  //  	status = gettimeofday (&now, &obsolete);
  status = clock_gettime ( CLOCK_REALTIME, &now );

  if (status == 0)
    {
      abstime->tv_sec = now.tv_sec + delta->tv_sec;
      //      abstime->tv_nsec = (NSECS_IN_USEC * now.tv_usec) + delta->tv_nsec;
      abstime->tv_nsec = now.tv_nsec + delta->tv_nsec;
      while (abstime->tv_nsec > NSECS_IN_SEC)
	{
	  abstime->tv_sec  += 1;
	  abstime->tv_nsec -= NSECS_IN_SEC;
	}
    }
  
  return (status);
}


/* Put the current thread to sleep for the specified interval */

int pthread_delay_np (const struct timespec *ointerval)
{
  int status;
  struct timespec interval, rinterval;

	interval.tv_sec  = ointerval->tv_sec;
	interval.tv_nsec = ointerval->tv_nsec;

	pthread_testcancel ();

	while ((status = nanosleep (&interval, &rinterval)))
	  {
		if (errno != EINTR) break;
		interval.tv_sec  = rinterval.tv_sec;
		interval.tv_nsec = rinterval.tv_nsec;
		pthread_testcancel ();
	}

	return (status);
}

#endif
