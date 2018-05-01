/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support for libpcap */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include "pfilt_wrapper.h"

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "FEPComm.h"

static EmbNetChannel* pInputChannel;

/* Create the network channels */

void InitializeNetworkChannels (VLMConfig* config)
{
  int ipSocket, savedLen, i;
  bool tryAgain;

  printf("InitializeNetworkChannels()\n");

  for (i = 0; i < MaxNetworkInterfaces; i++) {
    if (config->interfaces[i].present) {
      InitializeNetChannel (&config->interfaces[i], i);
    }
  }

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

static void InitializeNetChannel (NetworkInterface* interface, int unitNumber)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbNetChannel));
  register EmbNetChannel* p = (EmbNetChannel*) HostPointer (cp);

  NetworkInterface* pInterface;

#ifdef GENERA
  struct in_addr guestAddress;
  char addressAsString[_POSIX_ARG_MAX];
  boolean firstInterface;
#endif

  pInputChannel = p;

  p->type = EmbNetworkChannelType;
  p->unit = unitNumber;
  p->pcap = 0;
  p->receiverThreadSetup = FALSE;				/* .. */
  p->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
  EmbCommAreaPtr->channel_table = cp;

  if (!interface->device[0]) {
    vpunt (NULL, "Missing ethernet interface name for network interface #%d", unitNumber);
  }

  /* Open a packet socket and bind it to the interface */

  {
    char errbuf[1024]; /* the libpcap manual page does not specify how large this should be */
    p->pcap = pcap_open_live (interface->device, 1560, 1, 0, errbuf);
    if (!p->pcap) {
      vpunt (NULL, "Cannot open packet capturing device for network interface #%d: %s", unitNumber, errbuf);
    }
  }
  
  /* Create and attach the filter program */

  if (pcap_compile (p->pcap,
                    &(p->filter),
                    "ether host 23:42:23:42:00:00",
                    1,
                    htonl(0xFFFFFF00)) == -1) {
    vpunt (NULL, "Error creating filter for network interface #%d: %s", unitNumber, pcap_geterr (p->pcap));
  }

  /* Finish initialization */

  p->status = 0;

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
  for (pInterface = interface, firstInterface = TRUE; pInterface != NULL;
       pInterface = pInterface->anotherAddress, firstInterface = FALSE) {
    if (firstInterface)
      addressAsString[0] = 0;
    else
      sprintf (addressAsString, "%s,", addressAsString);
    if (pInterface->device[0])
      sprintf (addressAsString, "%s%s:", addressAsString, pInterface->device);
    switch (pInterface->myProtocol) {
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
      sprintf (addressAsString, "%s;%s", addressAsString, pInterface->myOptions);
  }
  printf("Initialize network interface #%d as \"%s\"\n", unitNumber, addressAsString);
  p->addressString = MakeEmbString (addressAsString);
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

#ifdef OS_OSF
  ioctl (netChannel->fd, EIOCFLUSH, 0); 	/* Flush incoming packets */
#endif

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
  unsigned char *p, *pp;
  unsigned short ptype;
  int op, prot;
                                                                                
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

static void
NetworkChannelTransmitter(EmbNetChannel* pNetChannel)
{
#if 0
  register EmbNetChannel* netChannel = pNetChannel;
  register EmbQueue* transmitQueue = netChannel->guestToHostQ;
  register EmbQueue* returnQueue = netChannel->guestToHostReturnQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t nBytes, actualBytes;

  while (EmbQueueFilled (transmitQueue)) {
    if (0 == EmbQueueSpace (returnQueue)) {
      /* Can't do I/O now because we can't return the buffer -- Try again later */
      SignalLater (transmitQueue->signal);
      return;
    }

    netPacketPtr = EmbQueueTakeWord (transmitQueue);
    if (NULL == (void*)(uint64_t)netPacketPtr) netPacketPtr = NullEmbPtr;

    if (netPacketPtr != NullEmbPtr) {
      if (/*netChannel->status & EmbNetStatusHostReady*/1) {
        netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
        nBytes = (ssize_t) netPacket->nBytes;
#if BYTE_ORDER == BIG_ENDIAN
        bswap32_block (&netPacket->data, nBytes);
#endif

        memcpy (netChannel->sll.sll_addr, ((struct ethhdr*)netPacket->data)->h_dest,
                ETH_ALEN);
#if 0
        actualBytes = sendto (netChannel->fd, &netPacket->data[0],
                              nBytes, /*MSG_CONFIRM*/0,
                              (struct sockaddr*)&netChannel->sll,
                              sizeof (netChannel->sll));
#else
        actualBytes = sendto (netChannel->fd, &netPacket->data[0],
                              nBytes, /*MSG_CONFIRM*/0,
                              NULL,
                              sizeof (netChannel->sll));
#endif
        if (actualBytes != nBytes) {
          printf("tx error\n");
          netChannel->nTransmitFailures++;
        }
#if 1
        if (new_packet((char *)new_packet, nBytes) || 1) {
          if (0) printf("NetworkChannelTransmitter() %p %d\n", netPacket, nBytes);
          if (0) printf("%02x:%02x:%02x:%02x:%02x:%02x ",
                        netChannel->sll.sll_addr[0], netChannel->sll.sll_addr[1], 
                        netChannel->sll.sll_addr[2], netChannel->sll.sll_addr[3], 
                        netChannel->sll.sll_addr[4], netChannel->sll.sll_addr[5]);
          dump_packet("tx", (unsigned char *)&netPacket->data[0], nBytes);
        }
#endif
      }

      EmbQueuePutWord (returnQueue, netPacketPtr);
    }
  }
#endif
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
  struct sockaddr sll;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t actualBytes;
  socklen_t sllLen;

  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);

  WaitUntilInitializationComplete ();

  pollReceiver.fd = netChannel->fd;
  pollReceiver.events = POLLIN;

  while (TRUE) {
    pthread_testcancel ();
		
    pollReceiver.revents = 0;
    poll (&pollReceiver, 1, 1000);

    if (0 == (pollReceiver.revents & POLLIN))
      continue;

    sllLen = sizeof (sll);
    actualBytes = recvfrom (netChannel->fd, &netChannel->receiveBuffer,
                            MaxEmbNetPacketSize, MSG_TRUNC,
                            &sll, &sllLen);
    dump_packet("rx", (unsigned char*)&netChannel->receiveBuffer, actualBytes);

    if (actualBytes < 0)
      netChannel->nReceiveFailures++;

    else if (0 == actualBytes)
      netChannel->nFalseReceiverWakeups++;

    //		else if (!(netChannel->status & EmbNetStatusGuestReady))
    //			;

    else if ((0 == EmbQueueSpace (supplyQueue)) || (0 == EmbQueueSpace (receiveQueue)))
      netChannel->nReceivedPacketsLost++;

    else {
      while (0 == (netPacketPtr = EmbQueueTakeWord (supplyQueue))) {
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

static void TerminateNetChannel (EmbNetChannel* netChannel, int ipSocket)
{
#if 0
  EmbNetARPReq *embARPReq;
  void *exit_value;

  if (netChannel->receiverThreadSetup) {
    pthread_cancel (netChannel->receiverThread);
    pthread_join (netChannel->receiverThread, &exit_value);
    netChannel->receiverThreadSetup = FALSE;
  }

#ifndef NOROOT
  for (embARPReq = netChannel->arpReq;  embARPReq != NULL; embARPReq->next)
    ioctl (ipSocket, SIOCDARP, &embARPReq->arp);
#endif

  if (netChannel->fd != -1) {
    close (netChannel->fd);
    netChannel->fd = -1;
  }
#endif
}

/* Cleanup the network channels */

void TerminateNetworkChannels ()
{
  EmbNetChannel* netChannel;
  EmbPtr channel;
  int ipSocket;

  ipSocket = socket (PF_INET, SOCK_STREAM, 0);

  for (channel = EmbCommAreaPtr->channel_table;
       channel != NullEmbPtr;
       channel = netChannel->next) {
    netChannel = (EmbNetChannel*) HostPointer (channel);
    if (EmbNetworkChannelType == netChannel->type)
      TerminateNetChannel (netChannel, ipSocket);
  }

  if (ipSocket > -1)
    close (ipSocket);
}
