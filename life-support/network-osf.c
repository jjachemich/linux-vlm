/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support for OSF (Tru64 UNIX) */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <poll.h>

#include <netdb.h>
#include <netinet/in.h>
#include "pfilt_wrapper.h"
#include <netinet/in_systm.h>
#define _NO_BITFIELDS
#include <netinet/ip.h>
#include <arpa/inet.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "FEPComm.h"


/* Create the network channels */

void InitializeNetworkChannels (VLMConfig* config)
{
  struct hostent* localHostEntry;
  struct in_addr localHostAddress;
  char localHost[MAXHOSTNAMELEN];
  int i;

	if (gethostname (localHost, sizeof (localHost)))
		vpunt (NULL, "Unable to determine local host name");

	if (NULL == (localHostEntry = gethostbyname (localHost)))
		vpunt (NULL, "Unable to determine local host network address");

	memcpy ((char*)&localHostAddress.s_addr, localHostEntry->h_addr,
		    sizeof (localHostAddress.s_addr));

	for (i = 0; i < MaxNetworkInterfaces; i++)
		if (config->interfaces[i].present)
			InitializeNetChannel (&config->interfaces[i], i, &localHostAddress);

#ifdef MINIMA
	WriteFEPCommSlot (localIPAddress0, 0, Type_Fixnum);
	WriteFEPCommSlot (diagnosticIPAddress, htonl (config->diagnosticIPAddress.s_addr), Type_Fixnum);
	WriteFEPCommSlot (localIPAddress1, 0, Type_Fixnum);
	WriteFEPCommSlot (localIPSubnetMask0, 0, Type_Fixnum);
	WriteFEPCommSlot (localIPSubnetMask1, 0, Type_Fixnum);
	WriteFEPCommSlot (gatewayIPAddress0, 0, Type_Fixnum);
	WriteFEPCommSlot (gatewayIPAddress1, 0, Type_Fixnum);
	WriteFEPCommSlot (loadServerIPAddress, 0, Type_Fixnum);
#endif
}


/* Create a single network channel */

static void InitializeNetChannel (NetworkInterface* interface, int unitNumber,
								  struct in_addr* localHostAddress)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbNetChannel));
  register EmbNetChannel* p = (EmbNetChannel*) HostPointer (cp);
  struct ifreq hardwareInterface;
  struct endevp deviceParms;
  struct timeval timeout;
  u_short ioctlBits, *fp;
  int x;
  u_short etherTypeOffset = offsetof (struct ether_header, ether_type) / sizeof (u_short);
  u_short ipAddressOffset = (offsetof (struct ip, ip_dst) + sizeof (struct ether_header)) / sizeof (u_short);
#ifdef GENERA
  struct in_addr guestAddress;
  char addressAsString[_POSIX_ARG_MAX];
  boolean firstInterface;
#endif

	p->type = EmbNetworkChannelType;
	p->unit = unitNumber;
	p->fd = -1;									/* Needed before linking into channel list */
	p->receiverThreadSetup = FALSE;				/* .. */
	p->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
	EmbCommAreaPtr->channel_table = cp;

/*#define NOROOT*/
#ifndef NOROOT
	p->fd = pfopen ((0 == interface->device[0]) ? NULL : interface->device, O_RDWR);
	if (-1 == p->fd)
		vpunt (NULL, "Unable to open VLM network interface #%d using %s", unitNumber,
			   (0 == interface->device[0]) ? "the default channel" : interface->device);

	if (-1 == ioctl (p->fd, EIOCIFNAME, &hardwareInterface))
		vpunt (NULL,
			   "Unable to determine hardware interface name for VLM network interface #%d",
			   unitNumber);
#endif
	p->name0 = p->name1 = 0;
	memcpy ((char*)&p->name0, hardwareInterface.ifr_name, 2* sizeof (EmbWord));

	p->status = 0;
	p->hostPrimaryProtocol = ETHERTYPE_IP;
	p->hostPrimaryAddress = ntohl (localHostAddress->s_addr);
	p->guestPrimaryAddress = ntohl (interface->myAddress.s_addr);
	p->guestPrimaryAddress = interface->myAddress.s_addr;

#ifndef NOROOT
	if (-1 == ioctl (p->fd, EIOCDEVP, &deviceParms))
		vpunt (NULL,
			   "Unable to determine hardware interface address for VLM network interface #%d",
			   unitNumber);
#endif
	p->hardwareAddressHigh = p->hardwareAddressLow = 0;
	memcpy ((char*)&p->hardwareAddressHigh, deviceParms.end_addr, deviceParms.end_addr_len);
	
#ifndef NOROOT
	x = 1;
	if (-1 == ioctl (p->fd, EIOCALLOWPROMISC, &x))
		vpunt (NULL, "Unable to set ALLOWPROMISC for VLM network interface #%d", unitNumber);

	x = 1;
	if (-1 == ioctl (p->fd, EIOCALLOWCOPYALL, &x))
		vpunt (NULL, "Unable to set ALLOWCOPYALL for VLM network interface #%d", unitNumber);

	ioctlBits = ENHOLDSIG | ENNONEXCL | ENCOPYALL;
	if (-1 == ioctl (p->fd, EIOCMBIS, &ioctlBits))
		vpunt (NULL, "Unable to set attributes for VLM network interface #%d", unitNumber);

	ioctlBits = ENBATCH | ENTSTAMP | ENPROMISC | ENBPFHDR;
	if (-1 == ioctl (p->fd, EIOCMBIC, &ioctlBits))
		vpunt (NULL, "Unable to clear attributes for VLM network interface #%d", unitNumber);

	timeout.tv_sec = timeout.tv_usec = 0;		/* Wait indefinitely for packets */
	if (-1 == ioctl (p->fd, EIOCSRTIMEOUT, &timeout))
		vpunt (NULL, "Unable to set packet timeout for VLM network interface #%d", unitNumber);

#if 0
	x = deviceParms.end_MTU;					/* TEMPORARY workaround to DEC bug */
	x = (x < MaxEmbNetPacketSize) ? x : MaxEmbNetPacketSize;
	if (-1 == ioctl (p->fd, EIOCTRUNCATE, &x))
		vpunt (NULL, "Unable to set maximum packet size for VLM network interface #%d",
			   unitNumber);
#endif

	x = -1;										/* -1 => Get maximum allowable queue size */
	if (-1 == ioctl (p->fd, EIOCMAXBACKLOG, &x))
		vpunt (NULL, "Unable to determine maximum queue size for VLM network interface #%d", 
			   unitNumber);
	if (-1 == ioctl (p->fd, EIOCSETW, &x))
		vpunt (NULL, "Unable to set queue size for VLM network interface #%d", unitNumber);
#endif

	p->filter.enf_Priority = 255;				/* Maximum priority */
p->filter.enf_Priority = 10;

	/* A packet filter which will reject IP packets destined for the host */
	fp = &p->filter.enf_Filter[0];
#if 0
	*fp++ = ENF_PUSHWORD + etherTypeOffset;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (ETHERTYPE_IP);
	*fp++ = ENF_NEQ;							/* TRUE if not an IP packet */
	*fp++ = ENF_PUSHWORD + ipAddressOffset;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress >> 16);
	*fp++ = ENF_NEQ;							/* TRUE if top of addresses don't match */
	*fp++ = ENF_PUSHWORD + ipAddressOffset + 1;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress);
	*fp++ = ENF_NEQ;							/* TRUE if bottom of addresses don't match */
	*fp++ = ENF_OR;								/* TRUE if addresses don't match */
	*fp++ = ENF_OR;								/* TRUE if not IP or addresses don't match */
#else

	/* not ip-packet or (( not to-host and not from-host ) | to-us) */

	/* not IP packet */
	*fp++ = ENF_PUSHWORD + etherTypeOffset;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (ETHERTYPE_IP);
	*fp++ = ENF_NEQ;							/* TRUE if not an IP packet */

	/* not to host */
	*fp++ = ENF_PUSHWORD + ipAddressOffset;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress >> 16);
	*fp++ = ENF_NEQ;							/* TRUE if top of addresses don't match */
	*fp++ = ENF_PUSHWORD + ipAddressOffset + 1;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress);
	*fp++ = ENF_NEQ;							/* TRUE if bottom of addresses don't match */
	*fp++ = ENF_OR;								/* TRUE if addresses don't match */

	/* not from host */
	*fp++ = ENF_PUSHWORD + ipAddressOffset - 2;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress >> 16);
	*fp++ = ENF_NEQ;
	*fp++ = ENF_PUSHWORD + ipAddressOffset - 2 + 1;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->hostPrimaryAddress);
	*fp++ = ENF_NEQ;
	*fp++ = ENF_OR;

	*fp++ = ENF_AND;

	/* to us */
	*fp++ = ENF_PUSHWORD + ipAddressOffset;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->guestPrimaryAddress >> 16);
	*fp++ = ENF_EQ;
	*fp++ = ENF_PUSHWORD + ipAddressOffset + 1;
	*fp++ = ENF_PUSHLIT;
	*fp++ = htons (p->guestPrimaryAddress);
	*fp++ = ENF_EQ;
	*fp++ = ENF_AND;

	*fp++ = ENF_OR;

	*fp++ = ENF_OR;
#endif

	p->filter.enf_FilterLen = fp - &p->filter.enf_Filter[0];

	{
	  struct in_addr a;
	  a.s_addr = htonl(p->hostPrimaryAddress);
	  printf("p->hostPrimaryAddress %08x\n", p->hostPrimaryAddress);
	  printf("hostPrimaryAddress %s\n", inet_ntoa(a));
	}

#ifndef NOROOT
	if (-1 == ioctl (p->fd, EIOCSETF, &p->filter))
		vpunt (NULL, "Unable to set packet filter program for VLM network interface #%d",
			   unitNumber);
#endif

	p->nTransmitFailures = p->nReceiveFailures = 0;

	p->guestToHostQueue = CreateQueue (NetworkTransmitterQueueSize, sizeof (EmbPtr));
	p->guestToHostQ = (EmbQueue*) HostPointer (p->guestToHostQueue);
	p->guestToHostQ->signal = InstallSignalHandler ((ProcPtrV) &NetworkChannelTransmitter,
												    (PtrV) p, FALSE);

	p->guestToHostReturnQueue = CreateQueue (NetworkTransmitterQueueSize, sizeof (EmbPtr));
	p->guestToHostReturnQ = (EmbQueue*) HostPointer (p->guestToHostReturnQueue);

	p->hostToGuestSupplyQueue = CreateQueue (NetworkReceiverQueueSize, sizeof (EmbPtr));
	p->hostToGuestSupplyQ = (EmbQueue*) HostPointer (p->hostToGuestSupplyQueue);

	p->hostToGuestQueue = CreateQueue (NetworkReceiverQueueSize, sizeof (EmbPtr));
	p->hostToGuestQ = (EmbQueue*) HostPointer (p->hostToGuestQueue);

#ifdef GENERA
	for (interface = interface, firstInterface = TRUE; interface != NULL;
		 interface = interface->anotherAddress, firstInterface = FALSE)
	  {
		if (firstInterface)
			addressAsString[0] = 0;
		else
			sprintf (addressAsString, "%s,", addressAsString);
		if (interface->device[0])
			sprintf (addressAsString, "%s%s:", addressAsString, interface->device);
		switch (interface->myProtocol)
		{
		  case ETHERTYPE_IP:
			guestAddress.s_addr = htonl (interface->myAddress.s_addr);
			sprintf (addressAsString, "%sINTERNET|%s", addressAsString, 
					 inet_ntoa (guestAddress));
			break;
		  case ETHERTYPE_CHAOS:
			sprintf (addressAsString, "%sCHAOS|%o", addressAsString,
					 htonl (interface->myAddress.s_addr));
			break;
		}
		if (interface->myOptions[0])
			sprintf (addressAsString, "%s;%s", addressAsString, interface->myOptions);
	  }
	p->addressString = MakeEmbString (addressAsString);
printf("%s\n", addressAsString);
#endif

	if (pthread_create (&p->receiverThread, &EmbCommAreaPtr->inputThreadAttrs,
					    (pthread_startroutine_t) &NetworkChannelReceiver, (pthread_addr_t) p))
		vpunt (NULL,
			   "Unable to create thread to receive packets for VLM network interface #%d",
			   unitNumber);
	p->receiverThreadSetup = TRUE;

	p->status |= EmbNetStatusHostReady;
}


/* Reset a network channel */

void ResetNetworkChannel (EmbChannel* channel)
{
  register EmbNetChannel* netChannel = (EmbNetChannel*) channel;

	ioctl (netChannel->fd, EIOCFLUSH, 0);

	ResetIncomingQueue (netChannel->guestToHostQ);
	ResetOutgoingQueue (netChannel->guestToHostReturnQ);

	ResetIncomingQueue (netChannel->hostToGuestSupplyQ);
	ResetOutgoingQueue (netChannel->hostToGuestQ);
}

#if 1
static void show_packet(char *who, unsigned char *pkt)
{
  unsigned char *p;
  char *prot;
  unsigned short sp, dp;
  p = pkt;
  if (p[12] == 0x08 && p[13] == 0) {
    switch (p[14+9/*20-11*/]) {
    case 1: prot = "icmp"; break;
    case 6: prot = "tcp"; break;
    case 17: prot = "udp"; break;
    default: prot = "?";
    }
    sp = (p[14+20+0] << 8) | p[14+20+1];
    dp = (p[14+20+2] << 8) | p[14+20+3];
    printf("%s %u.%u.%u.%u:%u -> %u.%u.%u.%u:%u %s\n",
	   who,
	   p[14+20-8], p[14+20-7], p[14+20-6], p[14+20-5], sp,
	   p[14+20-4], p[14+20-3], p[14+20-2], p[14+20-1], dp,
	   prot);
  }
  if (p[12] == 0x08 && p[13] == 6) {
    printf(".");
    fflush(stdout);
  }
}
#endif

/* Network Channel transmitter */

static void NetworkChannelTransmitter (EmbNetChannel* pNetChannel)
{
  register EmbNetChannel* netChannel = pNetChannel;
  register EmbQueue* transmitQueue = netChannel->guestToHostQ;
  register EmbQueue* returnQueue = netChannel->guestToHostReturnQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t nBytes, actualBytes;

if (0) printf("NetworkChannelTransmitter()\n");
	while (EmbQueueFilled (transmitQueue))
	  {
		if (0 == EmbQueueSpace (returnQueue))
		  {
			/* Can't do I/O now because we can't return the buffer -- Try again later */
			SignalLater (transmitQueue->signal);
			return;
		  }

		netPacketPtr = EmbQueueTakeWord (transmitQueue);
		if (NULL == netPacketPtr) netPacketPtr = NullEmbPtr;

		if (netPacketPtr != NullEmbPtr)
		  {
			if (netChannel->status & EmbNetStatusHostReady)
			  {
				netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
				nBytes = (ssize_t) netPacket->nBytes;
#ifndef NOROOT
show_packet("tx", (unsigned char *)&netPacket->data[0]);
				actualBytes = write (netChannel->fd, &netPacket->data[0], nBytes);
#else
				actualBytes = 0;
#endif
				if (actualBytes != nBytes)
				  {
					netChannel->nTransmitFailures++;
				  }
			  }

			EmbQueuePutWord (returnQueue, netPacketPtr);
		  }
	  }
}


/* Network Channel receiver thread -- Can it be written to not copy??? */

#define OneMillisecond 1000000L

static void NetworkChannelReceiver (pthread_addr_t argument)
{
  pthread_t self = pthread_self ();
  register EmbNetChannel* netChannel = (EmbNetChannel*) argument;
  register EmbQueue* supplyQueue = netChannel->hostToGuestSupplyQ;
  register EmbQueue* receiveQueue = netChannel->hostToGuestQ;
  struct pollfd pollReceiver;
  struct timespec receiverPause;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t actualBytes;

	pthread_cleanup_push (pthread_detach, &self);

	WaitUntilInitializationComplete ();

	pollReceiver.fd = netChannel->fd;
	pollReceiver.events = POLLNORM;

	while (TRUE)
	  {
		pthread_testcancel ();
		
		pollReceiver.revents = 0;
		poll (&pollReceiver, 1, 1000);

		if (0 == (pollReceiver.revents & POLLNORM))
			continue;

		actualBytes = read (netChannel->fd, &netChannel->receiveBuffer, MaxEmbNetPacketSize);

		if (actualBytes < 0)
			netChannel->nReceiveFailures++;

		else if (0 == actualBytes)
			netChannel->nFalseReceiverWakeups++;

		else if (!(netChannel->status & EmbNetStatusGuestReady))
			;

		else if ((0 == EmbQueueSpace (supplyQueue)) || (0 == EmbQueueSpace (receiveQueue)))
			netChannel->nReceivedPacketsLost++;

		else
		  {
show_packet("rx", (unsigned char *)&netChannel->receiveBuffer);

			while (0 == (netPacketPtr = EmbQueueTakeWord (supplyQueue)))
			  {
				receiverPause.tv_sec = 0;
				receiverPause.tv_nsec = OneMillisecond;
				if (pthread_delay_np (&receiverPause))
					vpunt (NULL, "Unable to sleep in thread %lx", self);
			  }
			netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
			netPacket->nBytes = (EmbWord) actualBytes;
			memcpy (&netPacket->data[0], &netChannel->receiveBuffer[0], actualBytes);
			EmbQueuePutWord (receiveQueue, netPacketPtr);
		  }
	  }

	pthread_cleanup_pop (TRUE);
}


/* Cleanup a single network channel */

static void TerminateNetChannel (EmbNetChannel* netChannel)
{
	void *exit_value;

	if (netChannel->receiverThreadSetup)
	  {
		pthread_cancel (netChannel->receiverThread);
		pthread_join (netChannel->receiverThread, &exit_value);
		netChannel->receiverThreadSetup = FALSE;
	  }

	if (netChannel->fd != -1)
	  {
		close (netChannel->fd);
		netChannel->fd = -1;
	  }
}


/* Cleanup the network channels */

void TerminateNetworkChannels ()
{
  EmbNetChannel* netChannel;
  EmbPtr channel;

	for (channel = EmbCommAreaPtr->channel_table; channel != NullEmbPtr;
		 channel = netChannel->next)
	  {
		netChannel = (EmbNetChannel*) HostPointer (channel);
		if (EmbNetworkChannelType == netChannel->type)
			TerminateNetChannel (netChannel);
	  }
}
