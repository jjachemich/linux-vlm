/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1991, 1993    *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */
/*
 * HISTORY
 */
#ifndef lint
static char *rcsid = "@(#)$RCSfile: pfopen.c,v $ $Revision: 1.1.1.1 $ (DEC) $Date: 2003/12/03 16:57:16 $";
#endif

#include <sys/socket.h>
#include <sys/time.h>	/* for timeval struct in pfilt.h */
#include <sys/file.h>
#include <sys/errno.h>
#include <net/if.h>
#include <net/pfilt.h>
#include <stdio.h>

#define	PFPREFIX	"/dev/pf/pfilt"		/* prefix for device names */
#define	PFMAXMINORS	256			/* 8-bit minor device field */
extern int errno;

/*
 * pfopen(ifname, flags): to support access to the Ethernet Packet Filter.
 * (using kernel options PACKETFILTER, pseudo-device packetfilter)
 *
 * ifname is a ptr to the Ethernet device name ("ln0", "xna1", "pf0", etc.)
 *	or NULL for default
 * flags are passed to the open() system call.
 *
 * return value:
 *	special device file descriptor on success
 *	-1 on failure with errno set to indicate the error
 *
 */
pfopen(ifname, flags)
char *ifname;			/* "ln0", "pf0", etc. or NULL */
int flags;
{
	int i;			/* loop counter */
	int fd;			/* file descriptor */
	char tryname[128];	/* device name: "/dev/pf/pfiltnn" */
	static int setif();

	if (ifname && (ifname[0] == 0))
	    ifname = NULL;	/* change empty string to NULL string */

	/* find next available device under the /dev/pf directory */
	for (i = 0; i < PFMAXMINORS; i++) {
		sprintf(tryname, "%s%d", PFPREFIX, i);
		fd = open(tryname, flags, 0);
		if (fd < 0) {
			switch (errno) {
			case EBUSY:	/* device in use */
				continue;	/* try the next entry */
			case ENOENT:	/* ran out of filenames */
			case ENXIO:	/* no more configured in kernel */
			default:	/* something else went wrong */
				return(-1);
			}
		}
		/* open succeeded, set the interface name */
		return(setif(fd, ifname));
	}
	return(-1);	/* didn't find an openable device */
}

static int setif(fd, ifname)
int fd;
char *ifname;
{
	if (ifname == NULL)	/* use default */
	    return(fd);

	if (ioctl(fd, EIOCSETIF, ifname) < 0) {
		close(fd);
		return(-1);
	}
	/* return the file descriptor */
	return(fd);
}
