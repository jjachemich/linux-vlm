#ifndef __chaos_h__
#define __chaos_h__
//
#include <arpa/inet.h>
#include <net/if.h>
#include <net/if_arp.h>
#include <netinet/if_ether.h>
#include <netinet/ip.h>
#include <netpacket/packet.h>
//
// globals
//
#define ETH_P_CHAOS 0x0804
#define ETH_P_ARP 0x0806
#define MIN_CHAOS_PACKET_SIZE 34
#define MIN_ARP_CHAOS_PACKET_SIZE 38
#define MAX_CHAOS_DATA_BYTES 488
#define MAX_CHAOS_DATA_SHORTS 244
#define MAX_CHAOS_DATA_WORDS 122
#define CHAOS_MIN_HOST_NUM 1
#define CHAOS_MAX_HOST_NUM 255
#define CHAOS_MIN_SUBNET_NUM CHAOS_MIN_HOST_NUM
#define CHAOS_MAX_SUBNET_NUM CHAOS_MAX_HOST_NUM
//
// CHAOS opcodes
//
#define CHAOS_OP_RFC 0x01
#define CHAOS_OP_OPN 0x02
#define CHAOS_OP_CLS 0x03
#define CHAOS_OP_FWD 0x04
#define CHAOS_OP_ANS 0x05
#define CHAOS_OP_SNS 0x06
#define CHAOS_OP_STS 0x07
#define CHAOS_OP_RUT 0x08
#define CHAOS_OP_LOS 0x09
#define CHAOS_OP_LSN 0x0A
#define CHAOS_OP_MNT 0x0B
#define CHAOS_OP_EOF 0x0C
#define CHAOS_OP_UNC 0x0D
#define CHAOS_OP_BRD 0x0E
#define CHAOS_OP_DAT 0x80
//
// CHAOS packet struct
//
typedef uint8_t mac_address[ETH_ALEN] ;
typedef uint8_t *mac_address_p ;

#pragma pack(push,1)

struct eth_header {
  mac_address eth_dst ;
  mac_address eth_src ;
  uint16_t eth_prot ;
} ;

struct arp_chaos_packet {
	struct eth_header eth ; // 14 bytes
	struct arphdr arp ;     // 8 bytes
	mac_address src_mac ;   // 6 bytes
	uint16_t src_chaos ;    // 2 bytes
	mac_address dst_mac ;   // 6 bytes
	uint16_t dst_chaos ;    // 2 bytes
	                        // ttl. 38 bytes
	uint8_t padding[26];    // pad to 64 byte length
} ;

struct arp_ip_packet {
	struct eth_header eth ;  // 14 bytes
	struct arphdr arp ;      // 8 bytes
	mac_address src_mac ;    // 6 bytes
	struct in_addr src_ip ;  // 4 bytes
	mac_address dst_mac ;    // 6 bytes
	struct in_addr dst_ip ;  // 4 bytes
	                         // ttl. 42 bytes
	uint8_t padding[22];     // pad to 64 byte length
} ;

struct chaos_header {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  uint8_t prot ;
  uint8_t opcode ;
#else
  uint8_t opcode ;
  uint8_t prot ;
#endif
  union { unsigned short lfcwhole;
    struct {
      unsigned short nbytes:12;	        /* Length of packet */
      unsigned short fwd_count:4;	/* Forwarding count */
    } ;
  } ;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  uint8_t dest_host ;
  uint8_t dest_subnet ;
#else
  uint8_t dest_subnet ;
  uint8_t dest_host ;
#endif
  uint16_t dest_index_num ;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  uint8_t source_host ;
  uint8_t source_subnet ;
#else
  uint8_t source_subnet ;
  uint8_t source_host ;
#endif
  uint16_t source_index_num ;
  uint16_t num ;
  uint16_t ack_num ;
} ;

struct chaos_packet {
  struct chaos_header hd;
  union {
    uint8_t  data8[MAX_CHAOS_DATA_BYTES] ;
    uint16_t data16[MAX_CHAOS_DATA_SHORTS] ;
    uint32_t data32[MAX_CHAOS_DATA_WORDS] ;
  } ;
} ;

struct eth_chaos_packet {
  struct eth_header eth ;
  struct chaos_packet chaos ;
} ;

struct eth_ip_packet {
  struct eth_header eth ;
  struct ip ip ;
} ;

#pragma pack(pop)

#endif // __chaos_h__
