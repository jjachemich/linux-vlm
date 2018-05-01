/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Unix crypt Support */

#include "std.h"
#include <crypt.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "ivoryrep.h"
#include "memory.h"

/* the maximum size of the returned crypt result string */
/* is 123 chractars (acc. to Drepper) excluding */
/* the terminating 0 byte for the string */
#define MAX_CRYPT_RESULT_SIZE 124

static EmbPtr EmbCryptResult = NullEmbPtr ;
static char cryptResult[MAX_CRYPT_RESULT_SIZE];

static void allocateResult ( void ) {
	if (EmbCryptResult == NullEmbPtr) {
		memset ( cryptResult, (char) 1, MAX_CRYPT_RESULT_SIZE - 1);
		cryptResult[MAX_CRYPT_RESULT_SIZE -1] = (char) 0 ;
		EmbCryptResult = MakeEmbString ( cryptResult );
	}
}

static char *getLispSimpleString (EmbWord *LispString, char *name)
{
	LispObj Lstring ;
	char *string ;
	size_t stringsize ;

	if (Type_String !=
	    *MapVirtualAddressTag ((Integer)((Integer *)LispString -
					     MapVirtualAddressData (0))))
	  {
		  verror ("UnixCrypt", "%s is not a simple string", name);
		  return NULL ;
	  }

 	Lstring = VirtualMemoryRead (*LispString);
	if (Type_HeaderI != (LispObjTag (Lstring) & 0x3F))
	{
		verror ("UnixCrypt", "%s is not a simple string", name);
		return NULL ;
	}
	
	if ((LispObjData (Lstring) & ~Array_LengthMask) != 0x50000000L)
	{
		verror ("UnixCrypt", "%s is not a simple string", name);
		return NULL;
	}

	stringsize = LispObjData (Lstring) & Array_LengthMask;
	string = (char*) malloc ((size_t)stringsize + 1);
	if (NULL == string)
	  {
		  verror ("UnixCrypt",
			  "Couldn't allocate space for local copy of %s", name);
		  return NULL ;
	  }

	memcpy (string,
		MapVirtualAddressData (*LispString + 1),
		(size_t)stringsize);
	string[stringsize] = 0;
	return string ;
}

/* crypt a password using a given a salt string */
void UnixCrypt (UnixCryptRequest *pRequest)
{
	UnixCryptRequest *request = pRequest ;
	EmbString *embString;
	char *password, *salt, *result ;
	int stringsize = MAX_CRYPT_RESULT_SIZE - 1 ;

	allocateResult ();
	embString = (EmbString*) HostPointer (EmbCryptResult);
	strcpy ( (char *)&embString->string, "*1" );
	embString->length = strlen("*1");
	request->cryptString = EmbCryptResult ;
	request->cryptResult = ESUCCESS ;
	password = getLispSimpleString ( &request->cryptPassword, "password" );
	if (password == NULL) {
		request->cryptResult = EINVAL;
		return;
	}
	salt = getLispSimpleString ( &request->cryptSalt, "salt" );
	if (salt == NULL) {
		free ( password );
		request->cryptResult = EINVAL;
		return;
	}
	errno = 0;
	result = crypt( password, salt );
	if (errno == 2 || salt[0] != '$') {
		vwarn("UnixCrypt",
		      "*WARNING* DES encryption requested. Computing it anyway");
		errno = 0;
		result = crypt( password, salt );
	}
	if (errno) {
		verror("UnixCrypt", "crypt error");
		request->cryptResult = errno;
		free(password);
		free(salt);
		return;
	}
	if (strlen(result) > stringsize) {
		verror("UnixCrypt", 
		       "result string too short (%d) for crypt result (%d)", 
		       stringsize,
		       strlen(result));
		request->cryptResult = ENOMEM ;
		free(password);
		free(salt);
		return;
	}
	strcpy ( (char *)&embString->string, result );
	embString->length = strlen (result );
	request->cryptString = EmbCryptResult ;
	free(password);
	free(salt);
	return ;
}
