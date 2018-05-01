
/*
* Swap statistics for DEC OSF/1 (either V1.0 or V2.0, MIPS or ALPHA)
*
* To build:     cc -o swapstat -O swapstat.c
* To install:  install -g kmem -u bin -m 2755 -S -f /usr/sbin swapstat
*               (you must be root to execute the above command)
* Results:
*       Swap space free = 94.29% (241.4MB out of 256.0MB)
*       Total swap space to physical memory = 200.05% (256.0MB to 128.0MB)
*/

#include <stdio.h>
#include <nlist.h>
#include <paths.h>
#include <unistd.h>
#include <sys/vm.h>
#include <sys/file.h>

struct nlist nl[] = {
    { "_vm_swap_space" },
#define N_VM_SWAP_SPACE         0
    { "_vm_total_swap_space" },
#define N_VM_TOTAL_SWAP_SPACE   1
    { "_physmem" },
#define N_PHYSMEM               2
    { NULL },
};

main(
    int argc,
    char *argv[])
{
    vm_size_t vm_swap_space, vm_total_swap_space;
    double to_mb = (double) getpagesize() / (1024.0*1024.0);
    int kmem, physmem;

    if (nlist(_PATH_UNIX, nl) < 0)
        exit(1);

    kmem = open(_PATH_KMEM, O_RDONLY);
    if (kmem < 0) {
        perror(_PATH_KMEM);
        exit(1);
    }
    lseek(kmem, (off_t) nl[N_VM_SWAP_SPACE].n_value, SEEK_SET);
    if (read(kmem, &vm_swap_space, sizeof(vm_swap_space)) != sizeof(vm_swap_space)) {
        perror("vm_swap_space");
        exit(1);
    }
    lseek(kmem, (off_t) nl[N_VM_TOTAL_SWAP_SPACE].n_value, SEEK_SET);
    if (read(kmem, &vm_total_swap_space, sizeof(vm_total_swap_space)) != sizeof(vm_swap_space)) {
        perror("vm_total_swap_space");
        exit(1);
    }
    lseek(kmem, (off_t) nl[N_PHYSMEM].n_value, SEEK_SET);
    if (read(kmem, &physmem, sizeof(physmem)) != sizeof(physmem)) {
        perror("physmem");
        exit(1);
    }

    printf("Swap space free = %.2lf%%",
            (double) (vm_swap_space * 100.0 / vm_total_swap_space));
    printf(" (%.1lfMB out of %.1lfMB)\n",
            (double) vm_swap_space * to_mb,
            (double) vm_total_swap_space * to_mb);
    printf("Total swap space to physical memory = %.2lf%%",
            (double) (vm_total_swap_space * 100.0 / physmem));
    printf(" (%.1lfMB to %.1lfMB)\n",
            (double) vm_total_swap_space * to_mb,
            (double) physmem * to_mb);
    return 0;
}
