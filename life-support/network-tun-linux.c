/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support for Linux - /dev/tun */

#ifdef USE_TUN

#include <stdio.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/un.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include "pfilt_wrapper.h"
//#include <linux/if.h>
#include <linux/if_tun.h>

#include <netinet/ip.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "FEPComm.h"

static EmbNetChannel* pInputChannel;
void InitializeNetChannel (NetworkInterface* interface, int unitNumber, 
				  int ipSocket, struct ifconf* ifc);

/* Create the network channels */

void InitializeNetworkChannels (VLMConfig* config)
{
  struct ifconf ifc;
  int ipSocket, savedLen, i;
  bool tryAgain;

  printf("InitializeNetworkChannels()\n");

  ipSocket = socket (PF_INET, SOCK_STREAM, 0);
  if (ipSocket == -1)
    vpunt (NULL, "Unable to open IP socket to gather network interface information");

  ifc.ifc_len = 32 * sizeof (struct ifreq);
  ifc.ifc_buf = NULL;
  tryAgain = TRUE;

  while (tryAgain)
    {
      ifc.ifc_buf = realloc (ifc.ifc_buf, ifc.ifc_len);
      if (ifc.ifc_buf == NULL)
	vpunt (NULL, "Unable to obtain space to read IP addresses of network interfaces");
      savedLen = ifc.ifc_len;
      if (ioctl (ipSocket, SIOCGIFCONF, &ifc) < 0)
	vpunt (NULL, "Unable to obtain IP addresses assigned to network interfaces");
      if (ifc.ifc_len == savedLen)
	ifc.ifc_len = 2 * ifc.ifc_len;
      else
	tryAgain = FALSE;
    }

  ifc.ifc_len = ifc.ifc_len / sizeof (struct ifreq);

  printf("MaxNetworkInterfaces %d\n", MaxNetworkInterfaces);

  printf("0 myAddress %08x\n", config->interfaces[0].myAddress.s_addr);

  for (i = 0; i < MaxNetworkInterfaces; i++) {
    if (config->interfaces[i].present) {
      printf("init %d\n", i);
      printf("config %p\n", config);
      printf("interface %p\n", &config->interfaces[i]);
      InitializeNetChannel(&config->interfaces[i], i, ipSocket, &ifc);
    }
  }

  close (ipSocket);

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
void NetworkChannelReceiver (pthread_addr_t argument);

/* Create a single network channel */

void InitializeNetChannel (NetworkInterface* interface, int unitNumber, 
				  int ipSocket, struct ifconf* ifc)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbNetChannel));
  register EmbNetChannel* p = (EmbNetChannel*) HostPointer (cp);
  struct ifreq ifr;
  struct if_nameindex *saved_ifs, *ifs;
  int interfaceIndex, i, err;
  NetworkInterface* pInterface;
#ifdef GENERA
  struct in_addr guestAddress;
  char addressAsString[_POSIX_ARG_MAX];
  boolean firstInterface;
#endif

  pInputChannel = p;

  p->type = EmbNetworkChannelType;
  p->unit = unitNumber;
  p->fd = -1; /* Needed before linking into channel list */
  p->receiverThreadSetup = FALSE;				/* .. */
  p->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
  EmbCommAreaPtr->channel_table = cp;

  if (interface->device[0])
    {
      /*
       * Verify that the requested device is running and
       * is an Ethernet interface
       */
      p->name0 = p->name1 = 0;
      memcpy ((char*)&p->name0, interface->device, 2 * sizeof (EmbWord));

      printf("device %s\n", interface->device);

      strncpy (ifr.ifr_name, interface->device, IFNAMSIZ);

      p->hardwareAddressHigh = p->hardwareAddressLow = 0;
      memcpy ((char*)&p->hardwareAddressHigh, ifr.ifr_hwaddr.sa_data,
	      2 * sizeof (EmbWord));

      printf("hw address %#x %#x\n",
	     p->hardwareAddressHigh,
	     p->hardwareAddressLow);
    }
  else
    {
      /* No interface specified: Use the first available Ethernet interface */
      interfaceIndex = -1;
      ifs = saved_ifs = if_nameindex();

      while (ifs->if_index != 0 && ifs->if_name != NULL)
	{
	  strncpy(ifr.ifr_name, ifs->if_name, IFNAMSIZ);
	  if (ioctl (ipSocket, SIOCGIFFLAGS, &ifr) < 0)
	    vpunt (NULL, "Unable to determine attributes of network device %s",
		   ifr.ifr_name);
	  if ((ifr.ifr_flags & (IFF_UP | IFF_RUNNING | IFF_LOOPBACK))
	      == (IFF_UP | IFF_RUNNING))
	    {
	      if (ioctl (ipSocket, SIOCGIFHWADDR, &ifr) < 0)
		vpunt (NULL, "Unable to determine hardware address for network device %s",
		       ifr.ifr_name);
	      if (ifr.ifr_hwaddr.sa_family == ARPHRD_ETHER)
		{
		  interfaceIndex = ifs->if_index;
		  strncpy (interface->device, ifs->if_name, IFNAMSIZ);
		  p->name0 = p->name1 = 0;
		  memcpy ((char*)&p->name0, ifs->if_name, 2 * sizeof (EmbWord));
		  p->hardwareAddressHigh = p->hardwareAddressLow = 0;
		  memcpy ((char*)&p->hardwareAddressHigh, ifr.ifr_hwaddr.sa_data,
			  2 * sizeof (EmbWord));
		  break;
					
		}
	    }
	  ifs++;
	}

      if_freenameindex (saved_ifs);

      if (interfaceIndex < 0)
	vpunt (NULL, "Unable to find an Ethernet interface to attach"
	       " to VLM network interface #%d",
	       unitNumber);
    }


  /* Get IP address of interface */

  p->hostPrimaryProtocol = -1;

  for (i = 0; i < ifc->ifc_len; i++) 
    {
      if (strncmp (interface->device, ifc->ifc_req[i].ifr_name, IFNAMSIZ) == 0)
	{
	  p->hostPrimaryProtocol = ETHERTYPE_IP;
#ifdef ARCH_X86_64
	  p->hostPrimaryAddress
	    = ntohl(((struct sockaddr_in *)&ifc->ifc_req[i].ifr_addr)->sin_addr.s_addr);
#else
	  p->hostPrimaryAddress
	    = ((struct sockaddr_in *)&ifc->ifc_req[i].ifr_addr)->sin_addr.s_addr;
#endif
	  break;
	}
    }

  if (p->hostPrimaryProtocol == -1)
    vpunt (NULL, "Unable to determine IP address assigned to network device %s",
	   interface->device);


  /* Open tunnel interface*/

  printf("hostPrimaryAddress %#x\n", p->hostPrimaryAddress);
  printf("guestPrimaryAddress %#x\n", p->guestPrimaryAddress);

  p->fd = open("/dev/net/tun", O_RDWR);
  if (p->fd < 0)
    vpunt (NULL, "Unable to /dev/net/tun for VLM network interface #%d",
	   unitNumber);

  //-----
  memset(&ifr, 0, sizeof(ifr));

  /* Flags: IFF_TUN   - TUN device (no Ethernet headers)                     
   *        IFF_TAP   - TAP device                                           
   *                                                                         
   *        IFF_NO_PI - Do not provide packet information                    
   */
  //  ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
  ifr.ifr_flags = IFF_TAP | IFF_NO_PI;

  strcpy(ifr.ifr_name, "tun0");

  err = ioctl(p->fd, TUNSETIFF, (void *) &ifr);
  if (err < 0) {
    vpunt (NULL, "Can't TUNSETIFF for VLM network interface #%d",
	   unitNumber);
  }

  //  strcpy(tun_name, ifr.ifr_name);
  if (system("/sbin/ifconfig tun0 10.0.0.1 dstaddr 10.0.0.2 netmask 255.255.255.0") < 0)
	  vpunt(NULL,"Can't ifconfig tun0");

  /*
   * Create entries in the host's ARP table for each IP address assigned
   * to this channel
   */

  p->arpReq = NULL;

#ifdef GENERA
  for (pInterface = interface;
       pInterface != NULL;
       pInterface = pInterface->anotherAddress)
#else
    pInterface = interface;
#endif
  {
    if (pInterface->myProtocol == ETHERTYPE_IP)
      {
	EmbPtr arpReqPtr = EmbCommAreaAlloc (sizeof (EmbNetARPReq));
	register EmbNetARPReq* pARP = (EmbNetARPReq*) HostPointer (arpReqPtr);
	pARP->next = p->arpReq;
	p->arpReq = pARP;

	pARP->arp.arp_pa.sa_family = AF_INET;
	((struct sockaddr_in *)&pARP->arp.arp_pa)->sin_addr.s_addr
	  = htonl (pInterface->myAddress.s_addr);

	/* Only supported interface type */
	pARP->arp.arp_ha.sa_family = ARPHRD_ETHER;
	memcpy (pARP->arp.arp_ha.sa_data, &p->hardwareAddressHigh,
		2 * sizeof (EmbWord));

	pARP->arp.arp_flags = ATF_COM | ATF_PERM /* | ATF_PUBL */ ;
	memcpy (pARP->arp.arp_dev, interface->device,
		sizeof (pARP->arp.arp_dev));

	/* Only first interface structure has the device */
#ifndef NOROOT
	if (ioctl (ipSocket, SIOCSARP, &pARP->arp) < 0)
	  vpunt (NULL, "Unable to establish ARP mappings for VLM network interface #%d",
		 unitNumber);
#endif
      }
  }

	
  /* Finish initialization */

  p->status = 0;
  p->guestPrimaryProtocol = interface->myProtocol;
#ifdef ARCH_X86_64
  p->guestPrimaryAddress = interface->myAddress.s_addr;
#else
  p->guestPrimaryAddress = htonl (interface->myAddress.s_addr);
#endif

#if BYTE_ORDER == BIG_ENDIAN
  bswap32_block (&p->hardwareAddressHigh, 2 * sizeof (EmbWord));
#endif

  p->nTransmitFailures = p->nReceiveFailures = 0;

  p->guestToHostQueue = CreateQueue(NetworkTransmitterQueueSize, sizeof(EmbPtr));
  p->guestToHostQ = (EmbQueue*) HostPointer (p->guestToHostQueue);
  p->guestToHostQ->signal =
    InstallSignalHandler((ProcPtrV)&NetworkChannelTransmitter,
			 (PtrV) p, FALSE);

  p->guestToHostReturnQueue =
    CreateQueue(NetworkTransmitterQueueSize, sizeof(EmbPtr));
  p->guestToHostReturnQ = (EmbQueue*) HostPointer(p->guestToHostReturnQueue);

  p->hostToGuestSupplyQueue =
    CreateQueue(NetworkReceiverQueueSize, sizeof(EmbPtr));
  p->hostToGuestSupplyQ = (EmbQueue*) HostPointer(p->hostToGuestSupplyQueue);

  p->hostToGuestQueue = CreateQueue(NetworkReceiverQueueSize, sizeof(EmbPtr));
  p->hostToGuestQ = (EmbQueue*) HostPointer(p->hostToGuestQueue);

#ifdef GENERA
  for (pInterface = interface, firstInterface = TRUE; pInterface != NULL;
       pInterface = pInterface->anotherAddress, firstInterface = FALSE)
    {
      if (firstInterface)
	addressAsString[0] = 0;
      else
	sprintf (addressAsString, "%s,", addressAsString);

      if (pInterface->device[0])
	sprintf (addressAsString, "%s%s:", addressAsString, pInterface->device);

      switch (pInterface->myProtocol)
	{
	case ETHERTYPE_IP:
	  guestAddress.s_addr = htonl (pInterface->myAddress.s_addr);
	  sprintf (addressAsString, "%sINTERNET|%s", addressAsString, 
		   inet_ntoa (guestAddress));
	  break;
	case ETHERTYPE_CHAOS:
	  sprintf (addressAsString, "%sCHAOS|%o", addressAsString,
		   htonl (pInterface->myAddress.s_addr));
	  break;
	}

      if (pInterface->myOptions[0])
	sprintf (addressAsString, "%s;%s",
		 addressAsString, pInterface->myOptions);
    }

//printf("addressAsString %s\n", addressAsString);

  p->addressString = MakeEmbString (addressAsString);
#endif

  if (pthread_create (&p->receiverThread, &EmbCommAreaPtr->inputThreadAttrs,
		      (pthread_startroutine_t) &NetworkChannelReceiver,
		      (pthread_addr_t) p))
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

  ResetIncomingQueue (netChannel->guestToHostQ);
  ResetOutgoingQueue (netChannel->guestToHostReturnQ);

  ResetIncomingQueue (netChannel->hostToGuestSupplyQ);
  ResetOutgoingQueue (netChannel->hostToGuestQ);
}


static char last_packet[1560];

static int
new_packet(char *packet, int size)
{
  if (memcmp(last_packet, packet, size) == 0)
    return 0;

  memcpy(last_packet, packet, size);

  return 1;
}

static void
recv_packet(char *packet, int size)
{
  register EmbNetChannel* netChannel = pInputChannel;
  register EmbQueue* supplyQueue = netChannel->hostToGuestSupplyQ;
  register EmbQueue* receiveQueue = netChannel->hostToGuestQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
                                                                                
  netPacketPtr = EmbQueueTakeWord (supplyQueue);
  netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
  netPacket->nBytes = (EmbWord)size;
  memcpy (&netPacket->data[0], packet, size);
#if BYTE_ORDER == BIG_ENDIAN
  bswap32_block (&netPacket->data, size);
#endif
  EmbQueuePutWord (receiveQueue, netPacketPtr);
}
                                                                                
void
answer_arp(char *pkt, int size)
{
  char tmp[10];
  int i;
 
  pkt[21] = 2;
  memcpy(tmp, &pkt[22], 10);
  memcpy(&pkt[22], &pkt[32], 10);
 
  for (i = 0; i < 6; i++)
    tmp[i] = i;
 
  memcpy(&pkt[32], tmp, 10);
 
  printf("answering arp\n");
 
  recv_packet(pkt, size);
}

void
dump_packet(char *who, unsigned char *pkt, int size)
{
  int i, offset = 0;
  unsigned char *p;
  unsigned short ptype;
  int prot;
                                                                                
#if 0
  p = pkt;
  for (i = 0; i < 8; i++) {
    printf("%04x: %02x %02x %02x %02x %02x %02x %02x %02x\n",
	   offset, p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]);
    offset += 8;
    p += 8;
  }
#endif
  p = pkt;
  ptype = (p[12]<<8)|p[13];
                                                                                
  switch (ptype) {
  case 0x0806:
#if 0
    printf("arp\n");
    op = (p[20]<<8)|p[21];
    if (op == 1) printf("request ");
    if (op == 2) printf("response ");
    printf("\n");
    p += 22;
    printf("arp: sender %02x:%02x:%02x:%02x:%02x:%02x %u.%u.%u.%u\n",
	   p[0], p[1], p[2], p[3], p[4], p[5],
	   p[6], p[7], p[8], p[9]);
    p += 10;
    printf("     target %02x:%02x:%02x:%02x:%02x:%02x %u.%u.%u.%u",
	   p[0], p[1], p[2], p[3], p[4], p[5],
	   p[6], p[7], p[8], p[9]);
    printf("\n");
    //    answer_arp((char *)pkt, size);
#endif
    break;
  case 0x0800:
    printf("%s ip: ", who);
    p += 14;
    prot = p[9];
    printf("%u.%u.%u.%u ", p[12], p[13], p[14], p[15]);
    printf("%u.%u.%u.%u ", p[16], p[17], p[18], p[19]);
    p += 20;
    switch (prot) {
    case 17:
      printf("udp; %u %u", (p[0]<<8)|p[1], (p[2]<<8)|p[3]);
    }
    printf("\n");
    break;
  default:
    printf("%s ", who);
    for (i = 0; i < 8; i++) {
      printf("%04x: %02x %02x %02x %02x %02x %02x %02x %02x\n",
	     offset,
	     p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]);
      offset += 8;
      p += 8;
    }
    break;
  }
}

/* Network Channel transmitter */

void NetworkChannelTransmitter (EmbNetChannel* pNetChannel)
{
  register EmbNetChannel* netChannel = pNetChannel;
  register EmbQueue* transmitQueue = netChannel->guestToHostQ;
  register EmbQueue* returnQueue = netChannel->guestToHostReturnQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t nBytes, actualBytes;


  while (EmbQueueFilled (transmitQueue))
    {
      if (0 == EmbQueueSpace (returnQueue))
	{
	  /*
	   * Can't do I/O now because we can't return the buffer --
	   * Try again later
	   */
	  SignalLater (transmitQueue->signal);
	  return;
	}

      netPacketPtr = EmbQueueTakeWord (transmitQueue);
      if (NULL == (void*)(uint64_t)netPacketPtr) netPacketPtr = NullEmbPtr;

      if (netPacketPtr != NullEmbPtr)
	{
	  if (/*netChannel->status & EmbNetStatusHostReady*/1)
	    {
	      netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
	      nBytes = (ssize_t) netPacket->nBytes;
#if BYTE_ORDER == BIG_ENDIAN
	      bswap32_block(&netPacket->data, nBytes);
#endif

	      memcpy(netChannel->sll.sll_addr,
		     ((struct ethhdr*)netPacket->data)->h_dest,
		     ETH_ALEN);

#if 0
	      /* */
	      pptr = (char *)netPacket->data;
	      proto = (pptr[12] << 8) | pptr[13];

	      nBytes -= 14;

	      if (proto == 0x800)
		actualBytes = write(netChannel->fd, pptr + 14, nBytes);
	      else
		actualBytes = nBytes;
#else
	      actualBytes = write(netChannel->fd, (char *)netPacket->data,
				  nBytes);
#endif

	      if (actualBytes != nBytes)
		{
		  printf("tx error\n");
		  netChannel->nTransmitFailures++;
		}
#if 1
	      if (new_packet((char *)new_packet, nBytes) || 1) {
		dump_packet("tx", (unsigned char *)&netPacket->data[0], nBytes);
	      }
#endif
	    }

	  EmbQueuePutWord (returnQueue, netPacketPtr);
	}
    }
}


/* Network Channel receiver thread -- Can it be written to not copy??? */

#define OneMillisecond 1000000L

void NetworkChannelReceiver (pthread_addr_t argument)
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


  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);

  WaitUntilInitializationComplete ();

  pollReceiver.fd = netChannel->fd;
  pollReceiver.events = POLLIN;

  while (TRUE)
    {
      pthread_testcancel ();
		
      pollReceiver.revents = 0;
      poll (&pollReceiver, 1, 1000);

      if (0 == (pollReceiver.revents & POLLIN))
	continue;

#if 0
      actualBytes = read(netChannel->fd,
			 ((char *)netChannel->receiveBuffer) + 14,
			 MaxEmbNetPacketSize);

      dump_packet("rx", (u_char *)&netChannel->receiveBuffer, actualBytes+14);
#else
      actualBytes = read(netChannel->fd, netChannel->receiveBuffer,
			 MaxEmbNetPacketSize);
      
      {
	u_char *pptr = (unsigned char *)netChannel->receiveBuffer;
	u_short proto = (pptr[12] << 8) | pptr[13];

	if (proto != 0x800 && proto != 0x806)
	  continue;
      }

      dump_packet("rx", (u_char *)&netChannel->receiveBuffer, actualBytes);
#endif

      if (actualBytes < 0)
	netChannel->nReceiveFailures++;

      else if (0 == actualBytes)
	netChannel->nFalseReceiverWakeups++;

#if 0
      else if (!(netChannel->status & EmbNetStatusGuestReady))
	;
#endif

      else if ((0 == EmbQueueSpace (supplyQueue)) || (0 == EmbQueueSpace (receiveQueue)))
	netChannel->nReceivedPacketsLost++;

      else
	{
#if 0
	  actualBytes += 14;
#endif
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
#if BYTE_ORDER == BIG_ENDIAN
	  bswap32_block (&netPacket->data, actualBytes);
#endif
	  EmbQueuePutWord (receiveQueue, netPacketPtr);
	}
    }

  pthread_cleanup_pop (TRUE);
}


/* Cleanup a single network channel */

void TerminateNetChannel (EmbNetChannel* netChannel, int ipSocket)
{
  /* EmbNetARPReq *embARPReq; */
  void *exit_value;

  if (netChannel->receiverThreadSetup)
    {
      pthread_cancel (netChannel->receiverThread);
      pthread_join (netChannel->receiverThread, &exit_value);
      netChannel->receiverThreadSetup = FALSE;
    }

#ifndef NOROOT
  /* for (embARPReq = netChannel->arpReq;  embARPReq != NULL; embARPReq->next) */
  /*   ioctl (ipSocket, SIOCDARP, &embARPReq->arp); */
#endif

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
  int ipSocket;

  ipSocket = socket (PF_INET, SOCK_STREAM, 0);

  for (channel = EmbCommAreaPtr->channel_table; channel != NullEmbPtr;
       channel = netChannel->next)
    {
      netChannel = (EmbNetChannel*) HostPointer (channel);
      if (EmbNetworkChannelType == netChannel->type)
	TerminateNetChannel (netChannel, ipSocket);
    }

  if (ipSocket > -1)
    close (ipSocket);
}

#endif /* USE_TUN */
