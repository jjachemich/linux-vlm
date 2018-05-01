#include <sys/types.h>
#include <sys/mman.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <signal.h>

#define PAGESIZE (1024*8)
#define MAPSIZE  (1024*8*32)
#define NMAP	 30
#define MAPBASE	 0x20000000000

long addrarr[NMAP];
long maddrarr[NMAP];

void segv_handler (int sigval, int code, register struct sigcontext *scp)
{
  int ret;
  caddr_t aligned_vma = (caddr_t) (scp->sc_traparg_a0 & ~(PAGESIZE -1));
  int prot = (random()&01)?(PROT_READ):(PROT_READ|PROT_WRITE);

  ret = mprotect(aligned_vma, PAGESIZE, prot);
  if(ret == -1)
  printf("mprotect/write failed @ 0x%lxprot %d, errno=%d\n", aligned_vma, prot, errno);
  
  ret = mvalid(aligned_vma, PAGESIZE, prot);
  if(ret == -1)
  printf("mvalid failed @ 0x%lx prot %d, errno=%d\n", aligned_vma, prot, errno);
}

main(int argc, char *argv[])
 {
	int i, ret;
	long startaddr;
	int fd;
        struct sigaction action;
  
	fd = open(argv[1], O_RDONLY, 0);
	if (fd == -1) {
		printf("open failed, errno=%d\n", errno);
	}

	startaddr = 0;
	for(i=0; i<NMAP; i++) {
		addrarr[i] = startaddr;
		startaddr += 1024*1024;
	}

	for(i=0; i<NMAP; i++) {
	    if (random()&01) {
	      maddrarr[i] = (long)mmap(MAPBASE+addrarr[i], MAPSIZE, PROT_READ,
			      MAP_FILE|MAP_PRIVATE|MAP_FIXED, fd, addrarr[i]);
	    }
	    else {
	      maddrarr[i] = (long)mmap(MAPBASE+addrarr[i], MAPSIZE, PROT_READ,
			      MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED, -1, 0);
	    }
    
	    if(maddrarr[i] == -1) 
		printf("mmap failed,addr=0x%lx,offset=0x%lx, errno=%d\n",
					MAPBASE+addrarr[i],addrarr[i],errno);
	    else
	    	printf("maddrarr[%d]=0x%lx\n",i, maddrarr[i]);
	}

	action.sa_handler = (void*) &segv_handler;
	action.sa_mask = 0;
	action.sa_flags = 0;
	if (-1 == sigaction(SIGSEGV, &action, NULL))
	  printf("sigaction failed, errno=%d\n", errno);

	for ( ; ; ) {
          int mapi = (random() % NMAP);
          int pagei = (random() % (MAPSIZE/PAGESIZE));
	  caddr_t addr = (caddr_t)(maddrarr[mapi] + (pagei * PAGESIZE));
          int prot = (random()&01)?(PROT_READ):(PROT_NONE);

	  if (prot)
   	    *addr  = 1;
          else
            *addr;

          mapi = (random() % NMAP);
          pagei = (random() % (MAPSIZE/PAGESIZE));
	  addr = (caddr_t)(maddrarr[mapi] + (pagei * PAGESIZE));
          prot = (random()&01)?(PROT_READ):(PROT_NONE);

          ret = mprotect(addr, PAGESIZE, prot);
          if(ret == -1)
             printf("mprotect/read failed @ 0x%lx, errno=%d\n", addr, errno);


	}
}
