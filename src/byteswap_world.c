/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM World File Tools */

#include "std.h"

#include "world_tools.h"
#include "utilities.h"

#define CommandName "byteswap_world"

Boolean Trace = FALSE;
Boolean EnableIDS = FALSE;
Boolean TestFunction = FALSE;

int main (int argc, char** argv)
{
  char *searchPath = strdup (DefaultWorldSearchPath);
  char *worldPath;
  size_t argLength;
  int i;
  boolean sawWorld = FALSE;

	SetCommandName (CommandName);

	for (i = 1; i < argc; i++)
	  {
		argLength = strlen (argv[i]);
		if (0 == strncmp (argv[i], "-searchpath", (argLength < 7) ? 7 : argLength)) {
			if (i < argc - 1)
				searchPath = argv[++i];
			else
				vpunt (NULL, "A search path must follow %s", argv[i]);
		}
	  }

	for (i = 1; i < argc; i++)
	  {
		argLength = strlen (argv[i]);
		if (0 == strncmp (argv[i], "-searchpath", (argLength < 7) ? 7 : argLength))
			i++;
		else if (0 == strncmp (argv[i], "-", 1))
			vpunt (NULL, "Unrecognized option: %s", argv[i]);
		else
		  {
			sawWorld = TRUE;
			worldPath = argv[i];
			if (NULL == strchr (worldPath, '/'))
				worldPath = strncat (strdup ("./"), argv[i], argLength);
			ByteSwapWorld (worldPath, searchPath);
		  }
	  }

	if (!sawWorld)
		vpunt (NULL, "Usage: %s worlds {-searchpath PATH}", CommandName);

	exit (EXIT_SUCCESS);
}
