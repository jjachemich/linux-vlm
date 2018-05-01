///* -*- Mode: C -*- */

/* VLM Network Life Support for Linux - /dev/tap */

#ifdef USE_TAP

#include <stdio.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <sys/param.h>

#include <netinet/in.h>
#include "pfilt_wrapper.h"
#include <linux/if_tun.h>

#include <netinet/ip.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>

#include <X11/Xlib.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "FEPComm.h"
#include "chaos.h"

#define ENV_VLM_TAP "VLM_TAP"
#define DEFAULT_INTERFACE "tap0"
#define FAKE_CHAOS_HOST_ADDRESS 255
#define FAKE_INET_HOST_ADDRESS 1
/* #define DEBUG_NETWORK 1 */
/* #define DEBUG_CHAOS 0 */
/* #define DEBUG_IP 0 */
/* #define DEBUG_ICMP 0 */
/* #define DEBUG_ARP 1 */
 
static EmbNetChannel* pInputChannel;
void NetworkChannelReceiver (pthread_addr_t argument);

/* Create the network channels */

/* static void get_mac_for ( char *ifname, uint8_t *mac ) { */
/* 	int sockfd ; */
/* 	struct ifreq ifr ; */
  
/* 	if ((sockfd = socket (AF_INET,SOCK_DGRAM,0)) < 0) { */
/* 		perror("sock for get_mac_for"); */
/* 		exit(-1); */
/* 	} */

/* 	memset ( &ifr, 0, sizeof(ifr)) ; */
/* 	strcpy(ifr.ifr_name,ifname); */
/* 	if (ioctl(sockfd,SIOCGIFHWADDR,&ifr) < 0) { */
/* 		perror("SIOCGIFHWADDR for get_mac_for"); */
/* 		exit (-1) ; */
/* 	} ; */

/* 	memcpy(mac,ifr.ifr_hwaddr.sa_data,ETH_ALEN); */

/* 	close(sockfd); */
/* } */

static void gen_random_mac ( uint8_t *mac ) {
	static int initialized = 0 ;
	int i ;

	if (! initialized) {
		srandom(time(0));
		initialized = 1;
	}
	for ( i=0 ; i < ETH_ALEN ; i++) {
		do
			mac[i] = random() ;
		while ((mac[i] == 0) || (mac[i] == 0xff)) ;
	}
	/*
	 * make it a locally administerd address
	 */
	mac[0] &= 0xFC ;
	mac[0] |= 0x02 ;
}

static char *makeAddressString ( VLMConfig *cfg ) {
  int i, needcomma = 0 ;
  NetworkInterface *nip ;
  struct in_addr ina ;
  static char result[_POSIX_ARG_MAX] ;
  char interim[10], *p = result ;

  *p = 0 ;
  for ( i = 0; i < 7; i++ ) {
    nip = &(cfg->interfaces[i]) ;
    while ( nip != NULL ) {
      if ( nip->present ) {
	if (needcomma) p = stpcpy( p, "," ) ;
	if ( ! strcmp (nip->device,""))
	  p = stpcpy ( p, DEFAULT_INTERFACE ) ;
	else
	  p = stpcpy( p, nip->device );
	p = stpcpy( p , ":" );
	if (nip->haveMac) {
		char macstring[18];

		p = stpcpy(p, "MAC|");
		sprintf(macstring, "%02x:%02x:%02x:%02x:%02x:%02x",
			nip->myMac.bytes[0],
			nip->myMac.bytes[1],
			nip->myMac.bytes[2],
			nip->myMac.bytes[3],
			nip->myMac.bytes[4],
			nip->myMac.bytes[5]);
		p = stpcpy(p,macstring);
		p = stpcpy(p, ",");
		if ( ! strcmp (nip->device,""))
			p = stpcpy ( p, DEFAULT_INTERFACE ) ;
		else
			p = stpcpy( p, nip->device );
		p = stpcpy( p , ":" );
	} 
	if (nip->myProtocol == ETH_P_IP) {
	  p = stpcpy( p, "INTERNET|" );
	  ina.s_addr = ntohl(nip->myAddress.s_addr);
	  p = stpcpy( p, inet_ntoa(ina) );
	} else if (nip->myProtocol == ETHERTYPE_CHAOS) {
	  p = stpcpy( p, "CHAOS|");
	  sprintf( interim,"%o", ntohl( nip->myAddress.s_addr ));
	  p = stpcpy( p, interim );
	}
	if ( nip->myOptions ) {
	  if ( strcmp( nip->myOptions, "") ) {
	    p = stpcpy( p, ";" );
	    p = stpcpy( p, nip->myOptions );
	  }
	}
	needcomma = 1;
	nip = nip->anotherAddress ;
      } else nip = NULL ;
    }
  }
  return result ;
}

static void send_ip_arp_req ( int to_fd,
			      uint8_t *my_mac, 
			      uint32_t my_ip, 
			      uint32_t remote_ip) {
	
	struct arp_ip_packet ap;
	uint32_t mip = ntohl(my_ip), rip = ntohl(remote_ip);
	
#if 0
	char buf1[16],buf2[16];
	
	vwarn("send ip arp","who has %s, tell %s",
	      inet_ntop(AF_INET, &rip, buf1, sizeof(buf1)),
	      inet_ntop(AF_INET, &mip, buf2, sizeof(buf2)));
#endif

	memset((void *)&ap,0,sizeof(ap));
	memset(ap.eth.eth_dst,0xff,ETH_ALEN);
	memcpy(ap.eth.eth_src,my_mac,ETH_ALEN);
	ap.eth.eth_prot = htons(ETH_P_ARP);
	ap.arp.ar_hrd = htons(ARPHRD_ETHER);
	ap.arp.ar_pro = htons(ETH_P_IP);
	ap.arp.ar_hln = ETH_ALEN ;
	ap.arp.ar_pln = 4 ;
	ap.arp.ar_op = htons(ARPOP_REQUEST);
	memcpy ( ap.src_mac, my_mac, ETH_ALEN ) ;
	memset ( ap.dst_mac, 0, ETH_ALEN );
	ap.src_ip.s_addr = mip ;
	ap.dst_ip.s_addr = rip ;

	if ( write(to_fd,
		   (char *)&ap,
		   sizeof(struct arp_ip_packet)) != 
	     sizeof(struct arp_ip_packet) ) {
		perror("sending ip arp request");
		exit ( -1 );
	}
}

static void send_chaos_arp_req ( int to_fd,
				 uint8_t *my_mac, 
				 uint16_t my_chaos, 
				 uint16_t remote_chaos) {

	/*
	 * send gratuitous arp packet to check mac and chaos addresses
	 */
	struct arp_chaos_packet ap;

	memset((void *)&ap,0,sizeof(ap));
	memset(ap.eth.eth_dst,0xff,ETH_ALEN);
	memcpy(ap.eth.eth_src,my_mac,ETH_ALEN);
	ap.eth.eth_prot = htons(ETH_P_ARP);
	ap.arp.ar_hrd = htons(ARPHRD_ETHER);
	ap.arp.ar_pro = htons(ETH_P_CHAOS);
	ap.arp.ar_hln = ETH_ALEN ;
	ap.arp.ar_pln = 2 ;
	ap.arp.ar_op = htons(ARPOP_REQUEST);
	memcpy ( ap.src_mac, my_mac, ETH_ALEN ) ;
	memset ( ap.dst_mac, 0xff, ETH_ALEN );
	ap.src_chaos = my_chaos ;
	ap.dst_chaos = my_chaos ;

	if (write(to_fd,
		  (char *)&ap,
		  sizeof(struct arp_chaos_packet) ) != 
	    sizeof(struct arp_chaos_packet)) {
		perror("sending chaos arp request");
		exit ( -1 );
	}
	memset((void *)&ap,0,sizeof(ap));
	memset(ap.eth.eth_dst,0xff,ETH_ALEN);
	memcpy(ap.eth.eth_src,my_mac,ETH_ALEN);
	ap.eth.eth_prot = htons(ETH_P_ARP);
	ap.arp.ar_hrd = htons(ARPHRD_ETHER);
	ap.arp.ar_pro = htons(ETH_P_CHAOS);
	ap.arp.ar_hln = ETH_ALEN ;
	ap.arp.ar_pln = 2 ;
	ap.arp.ar_op = htons(ARPOP_REPLY);
	memcpy ( ap.src_mac, my_mac, ETH_ALEN ) ;
	memset ( ap.dst_mac, 0xff, ETH_ALEN );
	ap.src_chaos = my_chaos ;
	ap.dst_chaos = my_chaos ;
	if (write(to_fd,
		  (char *)&ap,
		  sizeof(struct arp_chaos_packet) ) != 
	    sizeof(struct arp_chaos_packet)) {
		perror("sending chaos arp reply");
		exit ( -1 );
	}

}

void InitializeNetworkChannels ( VLMConfig* config ) {
  EmbPtr cp ;
  register EmbNetChannel* p ;
  int chan_no ;
  uint32_t iaddr ;
  EmbPtr addressString = MakeEmbString ( makeAddressString ( config )) ;
  struct ifreq ifr;
  uint8_t real_mac[ETH_ALEN] ;
  char ifn[8][16] ;
  char xhost_cmd[7+INET_ADDRSTRLEN] ;
  char buf[INET_ADDRSTRLEN] ;

  //  dump_vlm_config ( config );
  //
  // construct list of EmbNetChannels first
  //
  for (chan_no = 0; chan_no < 8; chan_no++ ) {
    struct NetworkInterface *cip = &config->interfaces[chan_no];

    if ( !cip->present ) break ;
    // allocate an embNetChannel
    cp = EmbCommAreaAlloc ( sizeof(EmbNetChannel) );
    p = (EmbNetChannel *) HostPointer(cp);
    p->type = EmbNetworkChannelType;
    p->unit = chan_no ;
    p->fd = -1 ;
    p->receiverThreadSetup = 0;
    p->next = EmbCommAreaPtr->channel_table ;
    EmbCommAreaPtr->channel_table = cp ;
    p->hostPrimaryProtocol = cip->myProtocol ;
    p->guestPrimaryProtocol = cip->myProtocol ;
    switch (p->hostPrimaryProtocol) {

    case ETH_P_CHAOS:
	    p->guestPrimaryAddress = cip->myAddress.s_addr >> 16;
	    if (cip->myHostAddress.s_addr)
		    p->hostPrimaryAddress =
			    cip->myHostAddress.s_addr;
	    else
		    p->hostPrimaryAddress = 
			    ((p->guestPrimaryAddress << 8 & 0xFF00) | 
			     FAKE_CHAOS_HOST_ADDRESS ) &0xFFFF ;
	    break ;

    case ETH_P_IP:
	    p->guestPrimaryAddress = cip->myAddress.s_addr ;
	    if (cip->myHostAddress.s_addr)
		    p->hostPrimaryAddress =
			    cip->myHostAddress.s_addr;
	    else
		    p->hostPrimaryAddress = 
			    (cip->myAddress.s_addr & 
			     0xffffff00) |
			    FAKE_INET_HOST_ADDRESS ;
	    break ;
    }
  }
  //
  // now fill channels in list order, so VLM uses the same channel numbering
  //
  cp = EmbCommAreaPtr->channel_table ;
  p = (EmbNetChannel *) HostPointer(cp);
#if DEBUG_NETWORK
  fprintf(stderr,"net config address string = %s\n", makeAddressString(config));
#endif  

  for (chan_no = 0;
       p->type == EmbNetworkChannelType;
       chan_no++) {
	  struct NetworkInterface *cip = &config->interfaces[chan_no];

	  if ( !cip->present ) break ;
	  strncpy(ifn[p->unit],
		  cip->device,
		  sizeof(ifn[p->unit]));
	  if (!strcmp(ifn[p->unit],"")) {
		  vwarn ("network",
			 "You did not specify a tun/tap interface for network channel %d:",
			 p->unit);
      vwarn ("network",
	     "You should either give a network spec on the command line, like" );
      vwarn ("network",
	     "\tgenera ... -network \"tap3:INTERNET|a.b.c.d;gateway=e.f.g.h\"" );
      vwarn ("network",
	     "\tor else you could set VLM_TAP=\"tap6\" in the environment." );
      if (getenv(ENV_VLM_TAP)) {
	      vwarn ("network", "\tUsing the value of VLM_TAP=\"%s\" for channel %d.",
		     getenv(ENV_VLM_TAP), p->unit );
	      strncpy(ifn[p->unit],
		      getenv(ENV_VLM_TAP),
		      sizeof(ifn[p->unit]));
      }
      else {
	      vwarn ("network",
		     "\tUsing built-in default \"%s\" for channel %d.", 
		     DEFAULT_INTERFACE,
		     p->unit );
	      strncpy(ifn[p->unit],
		      DEFAULT_INTERFACE,
		      sizeof(ifn[p->unit]));
      }
	  }
	  strncpy((char *)&(p->name0),ifn[p->unit],
		  MIN(sizeof(ifn[p->unit]), sizeof(p->name0)));
	  if (cip->haveMac) {
		  int i;
		  
		  for(i=0; i<ETH_ALEN; i++)
			  real_mac[i] =
				  cip->myMac.bytes[i] ;
		  if ((real_mac[0] & 0x02) == 0) {
			  real_mac[0] |= 0x02; /* make it a LAA mac */
			  vwarn("net_init",
				"setting LAA bit in mac address for %s results\
 in %02x:%02x:%02x:%02x:%02x:%02x",
				ifn[p->unit],
				real_mac[0],
				real_mac[1],
				real_mac[2],
				real_mac[3],
				real_mac[4],
				real_mac[5]);
		  }
	  } else
		  gen_random_mac ( real_mac );
	  p->hardwareAddressLow = p->hardwareAddressHigh = 0 ;
	  memcpy(&(p->hardwareAddressHigh),real_mac,ETH_ALEN);

	  // open tapx: device
	  if ((p->fd = open("/dev/net/tun", O_RDWR )) < 0) {
		  perror("open /dev/net/tun");
		  exit ( -1 ) ;
	  }

	  memset ( &ifr, 0, sizeof(ifr) );
	  ifr.ifr_flags = IFF_TAP | IFF_NO_PI ;
	  strcpy(ifr.ifr_name, (char *)(&(p->name0))) ;
	  if ( ioctl ( p->fd, TUNSETIFF, &ifr ) < 0 ) {
		  perror("TUNSETIFF");
		  exit(-1);
	  }
    
	  p->arpReq = NULL ;
	  p->status = 0;

	  switch (p->hostPrimaryProtocol) {

	  case ETH_P_IP: {
		  Display *dpy;

		  iaddr = htonl(config->interfaces[p->unit].myAddress.s_addr);
		  sprintf(xhost_cmd,
			  "xhost +%s",
			  inet_ntop(AF_INET,
				    &iaddr,
				    buf,
				    sizeof(buf)));
		  if (((dpy=XOpenDisplay(NULL)) == NULL) &&
		      (errno == EAGAIN)) {
			  vwarn("net init",
				"if you don't get a display or you're getting the ominous");
			  vwarn("net init",
				"'No protocol specified' error"
				", you may want to execute");
			  vwarn("net init","%s",xhost_cmd);
			  vwarn("net init",
				"on your X server to allow X11 access for genera");
		  }
		  if (dpy) XCloseDisplay(dpy);
#if DEBUG_NETWORK
		  iaddr = ntohl(p->hostPrimaryAddress);
		  fprintf(stderr,"ch%d %s: hostPrimaryAddress  = IP %s", 
			  chan_no, 
			  ifn[p->unit],
			  inet_ntop(AF_INET, &iaddr, buf, sizeof(buf)));
		  if (!config->interfaces[p->unit].myHostAddress.s_addr)
			  fprintf(stderr,", (host 1 on guest subnet)\n");
		  else
			  fprintf(stderr, " (given)\n");
		  iaddr = ntohl(config->interfaces[p->unit].myAddress.s_addr);
		  fprintf(stderr,"ch%d %s: guestPrimaryAddress = IP %s\n", 
			  chan_no, 
			  ifn[p->unit], 
			  inet_ntop(AF_INET, &iaddr, buf, sizeof(buf)));
#endif // DEBUG_NETWORK

		  break ;
	  }

    case ETH_P_CHAOS:
#if DEBUG_NETWORK
	    fprintf(stderr,"ch%d %s: hostPrimaryAddress  = CHAOS #o%o",
		    chan_no,
		    ifn[p->unit],
		    p->hostPrimaryAddress);
	    if (!config->interfaces[p->unit].myHostAddress.s_addr)
		    fprintf(stderr,", (#o377 on guest subnet)\n");
	    else
		    fprintf(stderr, " (given)\n");
	    fprintf(stderr,"ch%d %s: guestPrimaryAddress = CHAOS #o%o\n",
		    chan_no,
		    ifn[p->unit],
		    htons(p->guestPrimaryAddress));
#endif // DEBUG_NETWORK
	    break;

    } 
#if DEBUG_NETWORK
	  fprintf(stderr,"ch%d %s: guestMac = %02x:%02x:%02x:%02x:%02x:%02x\n",
		  chan_no,
		  ifn[p->unit],
		  real_mac[0], real_mac[1], real_mac[2],
		  real_mac[3], real_mac[4], real_mac[5] );
#endif // DEBUG_NETWORK
	  
	  p->unit = chan_no ;
	  p->nReceiveFailures = 0 ;
	  p->nTransmitFailures = 0 ;
	  p->addressString = addressString ;
	  p->net_broken = 0 ;
	  // Queues
	  p->guestToHostQueue =
		  CreateQueue(NetworkTransmitterQueueSize, sizeof(EmbPtr));
	  p->guestToHostQ = (EmbQueue*) HostPointer (p->guestToHostQueue);
	  p->guestToHostQ->signal =
		  InstallSignalHandler((ProcPtrV)&NetworkChannelTransmitter,
				       (PtrV) p, FALSE);
	  
	  p->guestToHostReturnQueue =
		  CreateQueue(NetworkTransmitterQueueSize, sizeof(EmbPtr));
	  p->guestToHostReturnQ = 
		  (EmbQueue*) HostPointer(p->guestToHostReturnQueue);
    
	  p->hostToGuestSupplyQueue =
		  CreateQueue(NetworkReceiverQueueSize, sizeof(EmbPtr));
	  p->hostToGuestSupplyQ =
		  (EmbQueue*) HostPointer(p->hostToGuestSupplyQueue);
    
	  p->hostToGuestQueue =
		  CreateQueue(NetworkReceiverQueueSize, sizeof(EmbPtr));
	  p->hostToGuestQ =
		  (EmbQueue*) HostPointer(p->hostToGuestQueue);
	  // receiver Thread
	  if (pthread_create (&p->receiverThread,
			      &EmbCommAreaPtr->inputThreadAttrs,
			      (pthread_startroutine_t) &NetworkChannelReceiver,
			      (pthread_addr_t) p)) {
		  perror ( "create receiver thread" );
		  exit (-1);
	  }
	  p->receiverThreadSetup = TRUE;
	  p->status |= EmbNetStatusHostReady;
	  
	  switch (p->hostPrimaryProtocol) {
		  
	  case ETH_P_IP:
		  send_ip_arp_req ( p->fd, 
				    (uint8_t *)&p->hardwareAddressHigh, 
				    p->guestPrimaryAddress, 
				    p->hostPrimaryAddress);
		  break ;

	  case ETH_P_CHAOS:
		  send_chaos_arp_req ( p->fd,
		  		       (uint8_t *)&p->hardwareAddressHigh,
		  		       htons(p->guestPrimaryAddress),
		  		       htons(p->hostPrimaryAddress));
		  break ;
	  }

	  if ((cp = p->next) == -1) break ;
	  p = (EmbNetChannel *) HostPointer(cp);
  }
}

/* Reset a network channel */

void ResetNetworkChannel (EmbChannel* channel)
{
  register EmbNetChannel* netChannel = (EmbNetChannel*) channel;

  ResetIncomingQueue (netChannel->guestToHostQ);
  ResetOutgoingQueue (netChannel->guestToHostReturnQ);

  ResetIncomingQueue (netChannel->hostToGuestSupplyQ);
  ResetOutgoingQueue (netChannel->hostToGuestQ);

  switch (netChannel->hostPrimaryProtocol) {

  case ETH_P_IP:
	  send_ip_arp_req ( netChannel->fd, 
			    (uint8_t *)&netChannel->hardwareAddressHigh, 
			    netChannel->guestPrimaryAddress, 
			    netChannel->hostPrimaryAddress);
	  break ;
	  
  case ETH_P_CHAOS:
	  send_chaos_arp_req ( netChannel->fd,
	  		       (uint8_t *)&netChannel->hardwareAddressHigh,
	  		       htons(netChannel->guestPrimaryAddress),
	  		       htons(netChannel->hostPrimaryAddress));
	  break ;
  }
  netChannel->net_broken = 0 ;
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
 
  for (i = 0; i < ETH_ALEN; i++)
	  tmp[i] = i;
  
  memcpy(&pkt[32], tmp, 10);
 
  printf("answering arp\n");
  
  recv_packet(pkt, size);
}

#if DEBUG_NETWORK
#if DEBUG_CHAOS
static char *chaos_opcode ( uint16_t opc ) {
  static char other[20] ;
  switch(opc) {
  case CHAOS_OP_RFC: return "RFC";
  case CHAOS_OP_OPN: return "OPN";
  case CHAOS_OP_CLS: return "CLS";
  case CHAOS_OP_FWD: return "FWD";
  case CHAOS_OP_ANS: return "ANS";
  case CHAOS_OP_SNS: return "SNS";
  case CHAOS_OP_STS: return "STS";
  case CHAOS_OP_RUT: return "RUT";
  case CHAOS_OP_LOS: return "LOS";
  case CHAOS_OP_LSN: return "LSN";
  case CHAOS_OP_MNT: return "MNT";
  case CHAOS_OP_EOF: return "EOF";
  case CHAOS_OP_UNC: return "UNC";
  case CHAOS_OP_BRD: return "BRD";
  case CHAOS_OP_DAT: return "DAT";
  default: 
	  sprintf( other, "OP 0x%02x ?", opc );
	  return other ;
  }
  return "???";
}
#endif // DEBUG_CHAOS
#endif // DEBUG_NETWORK

#if DEBUG_NETWORK
static void dump_packet (char *who, unsigned char *pkt, int size) {
  struct eth_header *peth = (struct eth_header *)pkt ;
#if DEBUG_CHAOS
  struct eth_chaos_packet *pchaos = (struct eth_chaos_packet *)pkt ;
#endif
#if DEBUG_ARP
  struct arp_chaos_packet *parp =(struct arp_chaos_packet *)pkt ;
  struct arp_ip_packet *parpi = (struct arp_ip_packet *)pkt ;
#endif
#if DEBUG_IP
  struct eth_ip_packet *pip = (struct eth_ip_packet *)pkt ;
#endif
  char proto[20], subproto[20], src[20], dst[20] ;

  sprintf(proto,"0x%04x",ntohs(peth->eth_prot));
  strcpy(subproto,"");
  strcpy(src,"");
  strcpy(dst,"");

  switch (ntohs(peth->eth_prot)) {

  case ETH_P_ARP:
#if DEBUG_ARP
	  switch (ntohs(parp->arp.ar_op)) {
	  case ARPOP_REQUEST: strcpy(proto,"arp req"); break;
	  case ARPOP_REPLY: strcpy(proto,"arp rpl"); break;
	  default: sprintf(proto,"arp %d",ntohs(parp->arp.ar_op));
	  }
	  switch (ntohs(parp->arp.ar_pro)) {
	  case ETH_P_IP: 
		  strcpy(subproto,"ip");
		  strcpy(src,inet_ntoa(parpi->src_ip));
		  strcpy(dst,inet_ntoa(parpi->dst_ip));
		  break ;
	  case ETH_P_CHAOS:
		  strcpy(subproto,"chaos");
		  sprintf(src,"#o%o", parp->src_chaos);
		  sprintf(dst,"#o%o", parp->dst_chaos);
		  break ;
	  }
#else
	  return ;
#endif // DEBUG_ARP
	  break;

  case ETH_P_IP:
#if DEBUG_IP
	  sprintf(proto,"ip");
	  switch (pip->ip.ip_p) {

	  case IPPROTO_ICMP:
#if DEBUG_ICMP
		  strcpy(subproto,"icmp"); break ;
#else
		  return ;
#endif // DEBUG_ICMP
	  case IPPROTO_IGMP: strcpy(subproto,"igmp");  break;
	  case IPPROTO_UDP: strcpy(subproto,"udp");  break;
	  case IPPROTO_TCP: strcpy(subproto,"tcp"); break;
	  default: sprintf(subproto,"0x%04x",pip->ip.ip_p); break;
	  }
	  strcpy(src, inet_ntoa(pip->ip.ip_src));
	  strcpy(dst, inet_ntoa(pip->ip.ip_dst));
#else // no DEBUG_IP
	  return ;
#endif // DEBUG IP
	  break;

  case ETH_P_CHAOS:
#if DEBUG_CHAOS
	  sprintf(proto,"chaos");
	  strcpy(subproto,chaos_opcode(pchaos->chaos.hd.opcode ));
	  sprintf(src,"#o%o",
		  (pchaos->chaos.hd.source_subnet << 8) | 
		  pchaos->chaos.hd.source_host );
	  sprintf(dst,"#o%o",
		  (pchaos->chaos.hd.dest_subnet << 8) |
		  pchaos->chaos.hd.dest_host );
#else
	  return;
#endif // DEBUG_CHAOS
	  break;

  case ETH_P_IPV6:
	  strcpy(proto,"IPV6:VLM does");
	  strcpy(subproto,"not");
	  strcpy(src,"speak");
	  strcpy(dst,"IPV6 (yet)");
	  break ;
	  
  default:
	  sprintf(proto,"0x%04x",ntohs(peth->eth_prot));
	  strcpy(subproto,"");
	  strcpy(src,"");
	  strcpy(dst,"");
	  
  }
  printf("%s %d %s (%s) %s -> %s\n", who, size, proto, subproto, src, dst);
}
#endif // DEBUG_NETWORK

/* Network Channel transmitter */

void NetworkChannelTransmitter (EmbNetChannel* pNetChannel)
{
  register EmbNetChannel* netChannel = pNetChannel;
  register EmbQueue* transmitQueue = netChannel->guestToHostQ;
  register EmbQueue* returnQueue = netChannel->guestToHostReturnQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t nBytes, actualBytes;
  unsigned char altbuf[ETH_ZLEN];

#if DEBUG_NETWORK
  char tx[22] ;


  sprintf(tx,"tx%d:%s",pNetChannel->unit,(char *)&(pNetChannel->name0));
#endif // DEBUG_NETWORK

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
			  //
			  // JJ : force eth src to be our address
			  //
			  if (memcmp((char *)netPacket->data + ETH_ALEN,
				     (char *)&pNetChannel->hardwareAddressHigh,
				     ETH_ALEN)) {
				  if (! pNetChannel->net_broken) {
					  vwarn("net tx",
						"ch%d: trying to send from wrong mac address. fixing it for now."
						,pNetChannel->unit);
					  vwarn("net tx",
						"for a longer term solution, you should patch your Genera system.");
					  pNetChannel->net_broken = 1 ;
				  }
				  memcpy((char *)netPacket->data + ETH_ALEN,
					 (char *)&pNetChannel->hardwareAddressHigh
					 ,ETH_ALEN);
			  }
			  /*
			   * if this is an arp request, swap bytes in src chaos addr
			   */
			  /* struct arp_chaos_packet *arpp = (struct arp_chaos_packet *)netPacket->data ; */
			  /* if (ntohs(arpp->eth.eth_prot) == ETH_P_ARP) { */
			  /* 	  if (ntohs(arpp->arp.ar_op) == ARPOP_REQUEST) */
			  /* 	  	  arpp->src_chaos = (arpp->src_chaos >> 8) | */
			  /* 	  		  (arpp->src_chaos << 8); */
			  /* 	  if (ntohs(arpp->arp.ar_op) == ARPOP_REPLY) */
			  /* 	  	  arpp->src_chaos = (arpp->src_chaos >> 8) | */
			  /* 	  		  (arpp->src_chaos << 8); */
			  /* } */
			  if (pNetChannel->net_broken) {
				  //
				  // if this is an arp reply, put our address there too
				  //
				  struct arp_chaos_packet *arpp = (struct arp_chaos_packet *)netPacket->data ;
				  if (ntohs(arpp->eth.eth_prot) == ETH_P_ARP) {
					  vwarn("net tx",
						"ch%d : fixing arp.",
						pNetChannel->unit);
					  memcpy(arpp->src_mac,
						 (char *)&(pNetChannel->hardwareAddressHigh),
						 ETH_ALEN);
				  }
				  
			  }
			  //
			  // if dst == src, set dst to host hw
			  //
			  if (!memcmp((char *)netPacket->data, 
				      (char *)netPacket->data + ETH_ALEN, 
				      ETH_ALEN)) {
				  vwarn("net tx",
					"ch%d: send to my own mac - making it a broadcast",
					pNetChannel->unit);
				  memset((char *)netPacket->data,0xff,ETH_ALEN);
				  pNetChannel->net_broken = 1 ;
			  }
			  //
			  // JJ
			  //
			  // if packet size < ETH_ZLEN bytes, pad it
			  //
			  if(nBytes < ETH_ZLEN) {
				  memset((void *)&altbuf,0,sizeof(altbuf));
				  memcpy((void *)&altbuf,(void *)netPacket->data,nBytes);
				  nBytes = ETH_ZLEN ;
				  actualBytes = write(netChannel->fd, altbuf, nBytes);
			  } else
				  actualBytes = write(netChannel->fd, (char *)netPacket->data,
						      nBytes);
			  if (actualBytes != nBytes)
			  {
				  printf("tx error\n");
				  netChannel->nTransmitFailures++;
			  }
#if DEBUG_NETWORK
			  dump_packet(tx, (unsigned char *)&netPacket->data[0], nBytes);
#endif // DEBUG_NETWORK
		  }
		  
		  EmbQueuePutWord (returnQueue, netPacketPtr);
	  }
  }
}
 
/* static void NetworkChannelTransmitter (EmbNetChannel* pNetChannel) */
/* { */
/*   register EmbNetChannel* netChannel = pNetChannel; */
/*   register EmbQueue* transmitQueue = netChannel->guestToHostQ; */
/*   register EmbQueue* returnQueue = netChannel->guestToHostReturnQ; */
/*   EmbPtr netPacketPtr; */
/*   EmbNetPacket* netPacket; */
/*   ssize_t nBytes, actualBytes; */


/*   while (EmbQueueFilled (transmitQueue)) */
/*     { */
/*       if (0 == EmbQueueSpace (returnQueue)) */
/* 	{ */
/* 	  /\* */
/* 	   * Can't do I/O now because we can't return the buffer -- */
/* 	   * Try again later */
/* 	   *\/ */
/* 	  SignalLater (transmitQueue->signal); */
/* 	  return; */
/* 	} */

/*       netPacketPtr = EmbQueueTakeWord (transmitQueue); */
/*       if (NULL == (void*)(uint64_t)netPacketPtr) netPacketPtr = NullEmbPtr; */

/*       if (netPacketPtr != NullEmbPtr) */
/* 	{ */
/* 	  if (/\*netChannel->status & EmbNetStatusHostReady*\/1) */
/* 	    { */
/* 	      u_char *pptr; */
/* 	      u_short proto; */
/* 	      netPacket = (EmbNetPacket*) HostPointer (netPacketPtr); */
/* 	      nBytes = (ssize_t) netPacket->nBytes; */
/* #if BYTE_ORDER == BIG_ENDIAN */
/* 	      bswap32_block(&netPacket->data, nBytes); */
/* #endif */

/* 	      memcpy(netChannel->sll.sll_addr, */
/* 		     ((struct ethhdr*)netPacket->data)->h_dest, */
/* 		     ETH_ALEN); */

/* #if 0 */
/* 	      /\* *\/ */
/* 	      pptr = (char *)netPacket->data; */
/* 	      proto = (pptr[12] << 8) | pptr[13]; */

/* 	      nBytes -= 14; */

/* 	      if (proto == 0x800) */
/* 		actualBytes = write(netChannel->fd, pptr + 14, nBytes); */
/* 	      else */
/* 		actualBytes = nBytes; */
/* #else */
/* 	      actualBytes = write(netChannel->fd, (char *)netPacket->data, */
/* 				  nBytes); */
/* #endif */

/* 	      if (actualBytes != nBytes) */
/* 		{ */
/* 		  printf("tx error\n"); */
/* 		  netChannel->nTransmitFailures++; */
/* 		} */
/* #if 1 */
/* 	      if (new_packet((char *)new_packet, nBytes) || 1) { */
/* 		dump_packet("tx", (unsigned char *)&netPacket->data[0], nBytes); */
/* 	      } */
/* #endif */
/* 	    } */

/* 	  EmbQueuePutWord (returnQueue, netPacketPtr); */
/* 	} */
/*     } */
/* } */


/* Network Channel receiver thread -- Can it be written to not copy??? */

// orig 1000000 = 1ms, now 100000 = 100 us
#define OneMillisecond 100000L
#define PollTimeoutInMillis 500

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
  uint8_t broadcast[ETH_ALEN] ;


  pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);
  memset(broadcast,0xff,ETH_ALEN);

  WaitUntilInitializationComplete ();

  pollReceiver.fd = netChannel->fd;
  pollReceiver.events = POLLIN;

  while (TRUE)
    {
      pthread_testcancel ();
		
      pollReceiver.revents = 0;
      poll (&pollReceiver, 1, PollTimeoutInMillis );

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
	u_char *pptr = (u_char *)netChannel->receiveBuffer;
	u_short proto = (pptr[12] << 8) | pptr[13];

	//
	// if network is broken, don't do any checks
	//
	if (! netChannel->net_broken )
		//
		// check if eth packet is either for us or a broadcast
		//
		if (memcmp(pptr,
			   (char *)&netChannel->hardwareAddressHigh,
			   ETH_ALEN) &&
		    memcmp(pptr, broadcast, ETH_ALEN)) 
			continue ;

	//
	// only accept IP, ARP, and CHAOS packets
	//
	switch (proto) {
	case ETHERTYPE_IP:
	case ETHERTYPE_CHAOS:
	case ETHERTYPE_ARP:
	  break;
	default:
	  continue;
	}
      }

#if DEBUG_NETWORK
      dump_packet("rx", (u_char *)&netChannel->receiveBuffer, actualBytes);
#endif // DEBUG_NETWORK
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

#endif /* USE_TAP */
