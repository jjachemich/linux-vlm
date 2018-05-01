/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from vlm:alpha-emulator;aistat.sid  Any changes made to it will be lost. */

#ifndef _AISTAT_
#define _AISTAT_



typedef struct processorstate {
	uint64_t       transpare3;
	uint64_t       transpare2;
	uint64_t       transpare1;
	uint64_t       carcdrsubroutine;
	uint64_t       cdrsubroutine;
	uint64_t       carsubroutine;
	uint64_t       linkage;
	uint64_t       resumeema;
	char          *statistics;
	char          *trace_hook;
	int64_t        instruction_count;
	uint64_t       long_pad0;
	uint64_t       asrr9;
	uint64_t       asrr10;
	uint64_t       asrr11;
	uint64_t       asrr12;
	uint64_t       asrr13;
	uint64_t       asrr14;
	uint64_t       asrr15;
	uint64_t       long_pad1;
	uint64_t       asrr26;
	uint64_t       asrr27;
	uint64_t       asrr29;
	uint64_t       asrr30;
	uint64_t       asrf2;
	uint64_t       asrf3;
	uint64_t       asrf4;
	uint64_t       asrf5;
	uint64_t       asrf6;
	uint64_t       asrf7;
	uint64_t       asrf8;
	uint64_t       asrf9;
	char          *meterdatabuff;
	uint32_t       meterpos;
	uint32_t       metermax;
	uint32_t       meterfreq;
	uint32_t       metermask;
	uint32_t       metervalue;
	uint32_t       metercount;
	uint64_t       choiceptr;
	uint64_t       sstkchoiceptr;
	uint64_t       dbcbase;
	uint64_t       dbcmask;
	char          *coprocessorreadhook;
	char          *coprocessorwritehook;
	char          *flushcaches_hook;
	char          *i_stage_error_hook;
	uint64_t       sfp1;
	uint64_t       fp0;
	uint64_t       fp1;
	uint64_t       floating_exception;
	uint64_t       aluandrotatecontrol;
	uint64_t       rotatelatch;
	uint64_t       aluborrow;
	uint64_t       aluoverflow;
	uint64_t       alulessthan;
	uint64_t       aluop;
	uint64_t       byterotate;
	uint64_t       bytesize;
	int64_t        bindingstacklimit;
	int64_t        bindingstackpointer;
	uint64_t       catchblock;
	uint64_t       extraandcatch;
	uint64_t       msclockcache;
	uint64_t       mscmultiplier;
	uint64_t       previousrcpp;
	char          *rlink;
	uint32_t       interruptreg;
	uint32_t       zoneoldspace;
	uint32_t       ephemeraloldspace;
	uint32_t       int_pad0;
	uint64_t       eqnoteql;
	uint32_t       lclength;
	uint32_t       sclength;
	uint64_t       lcarea;
	uint64_t       lcaddress;
	uint64_t       scarea;
	uint64_t       scaddress;
	uint64_t       restartsp;
	uint64_t       stop_interpreter;
	uint64_t       immediate_arg;
	uint64_t       continuationcp;
	int64_t        continuation;
	int64_t        control;
	int64_t        niladdress;
	int64_t        taddress;
	int64_t        bar0;
	int64_t        bar1;
	int64_t        bar2;
	int64_t        bar3;
	int64_t        epc;
	int64_t        fp;
	int64_t        lp;
	int64_t        sp;
	char          *cp;
	uint64_t       fccrmask;
	uint32_t       cslimit;
	uint32_t       csextralimit;
	char          *trapmeterdata;
	uint64_t       fepmodetrapvecaddress;
	uint64_t       trapvecbase;
	uint64_t       tvi;
	uint64_t       fccrtrapmask;
	char          *ptrtype;
	char          *vmattributetable;
	uint64_t       vma;
	int64_t        mostnegativefixnum;
	char          *icachebase;
	char          *endicache;
	uint64_t       fullworddispatch;
	uint64_t       halfworddispatch;
	int64_t        areventcount;
	uint64_t       stackcachesize;
	uint64_t       stackcachetopvma;
	uint64_t       cdrcodemask;
	char          *stackcachedata;
	uint64_t       stackcachebasevma;
	uint32_t       scovlimit;
	uint32_t       scovdumpcount;
	int64_t        mostpositivefixnum;
	uint64_t       internalregisterread1;
	uint64_t       internalregisterread2;
	uint64_t       internalregisterwrite1;
	uint64_t       internalregisterwrite2;
	uint64_t       dataread_mask;
	char          *dataread;
	uint64_t       datawrite_mask;
	char          *datawrite;
	uint64_t       bindread_mask;
	char          *bindread;
	uint64_t       bindwrite_mask;
	char          *bindwrite;
	uint64_t       bindreadnomonitor_mask;
	char          *bindreadnomonitor;
	uint64_t       bindwritenomonitor_mask;
	char          *bindwritenomonitor;
	uint64_t       header_mask;
	char          *header;
	uint64_t       structureoffset_mask;
	char          *structureoffset;
	uint64_t       scavenge_mask;
	char          *scavenge;
	uint64_t       cdr_mask;
	char          *cdr;
	uint64_t       gccopy_mask;
	char          *gccopy;
	uint64_t       raw_mask;
	char          *raw;
	uint64_t       rawtranslate_mask;
	char          *rawtranslate;
	int32_t        please_stop;
	int32_t        please_trap;
	int64_t        runningp;
	uint64_t       ac0array;
	uint64_t       ac0arword;
	uint64_t       ac0locat;
	uint64_t       ac0length;
	uint64_t       ac1array;
	uint64_t       ac1arword;
	uint64_t       ac1locat;
	uint64_t       ac1length;
	uint64_t       ac2array;
	uint64_t       ac2arword;
	uint64_t       ac2locat;
	uint64_t       ac2length;
	uint64_t       ac3array;
	uint64_t       ac3arword;
	uint64_t       ac3locat;
	uint64_t       ac3length;
	uint64_t       ac4array;
	uint64_t       ac4arword;
	uint64_t       ac4locat;
	uint64_t       ac4length;
	uint64_t       ac5array;
	uint64_t       ac5arword;
	uint64_t       ac5locat;
	uint64_t       ac5length;
	uint64_t       ac6array;
	uint64_t       ac6arword;
	uint64_t       ac6locat;
	uint64_t       ac6length;
	uint64_t       ac7array;
	uint64_t       ac7arword;
	uint64_t       ac7locat;
	uint64_t       ac7length;
	uint32_t       tmcurrenttransaction;
	uint32_t       tmwritestart;
	uint32_t       tmwritecurrent;
	uint32_t       tmwritelimit;
	uint32_t       tmrecordingreads;
	uint32_t       tmreadstart;
	uint32_t       tmreadcurrent;
	uint32_t       tmreadlimit;
	} PROCESSORSTATE, *PROCESSORSTATEP;

#define PROCESSORSTATE_SIZE 1440

typedef struct cacheline {
	uint64_t       annotation;
	uint32_t       nextpcdata;
	uint32_t       nextpctag;
	char          *nextcp;
	uint32_t       instruction;
	uint32_t       operand;
	uint32_t       pcdata;
	uint32_t       pctag;
	char          *code;
	} CACHELINE, *CACHELINEP;

#define CACHELINE_SIZE 48

#define CacheLine_Bits 18

#define CacheLine_Mask 262143

#define CacheLine_RShift 16

#define CacheLine_LShift 6

#define CacheLine_FillAmount 20

typedef struct arraycache {
	uint64_t       array;
	uint64_t       arword;
	uint64_t       locat;
	uint64_t       length;
	} ARRAYCACHE, *ARRAYCACHEP;

#define AutoArrayReg_Mask 224

#define AutoArrayReg_Size 32

#define AutoArrayReg_Shift 0

#define MSclock_UnitsToMSShift 24

#define MSclock_UnitsPerMicrosecond 16777216

#define Stack_CacheSize 1792

#define Stack_MaxFrameSize 128

#define Stack_CacheMargin 128

#define Stack_CacheDumpQuantum 896

#define IvoryMemory_Data 35

#define IvoryMemory_Tag 33

typedef struct savedregisters {
	uint64_t       r9;
	uint64_t       r10;
	uint64_t       r11;
	uint64_t       r12;
	uint64_t       r13;
	uint64_t       r14;
	uint64_t       r15;
	uint64_t       r29;
	uint64_t       f2;
	uint64_t       f3;
	uint64_t       f4;
	uint64_t       f5;
	uint64_t       f6;
	uint64_t       f7;
	uint64_t       f8;
	uint64_t       f9;
	} SAVEDREGISTERS, *SAVEDREGISTERSP;

#define SAVEDREGISTERS_SIZE 128

typedef struct tracedata {
	uint64_t       n_entries;
	uint32_t       recording_p;
	uint32_t       wrap_p;
	uint64_t       start_pc;
	uint64_t       stop_pc;
	char          *records_start;
	char          *records_end;
	char          *current_entry;
	char          *printer;
	} TRACEDATA, *TRACEDATAP;

#define TRACEDATA_SIZE 64

typedef struct tracerecord {
	uint64_t       counter;
	uint64_t       epc;
	uint64_t       tos;
	uint64_t       sp;
	char          *instruction;
	uint64_t       instruction_data;
	uint32_t       operand;
	uint32_t       trap_p;
	uint64_t       trap_data_0;
	uint64_t       trap_data_1;
	uint64_t       trap_data_2;
	uint64_t       trap_data_3;
	uint32_t       catch_block_p;
	uint32_t       int_pad0;
	uint64_t       catch_block_0;
	uint64_t       catch_block_1;
	uint64_t       catch_block_2;
	uint64_t       catch_block_3;
	} TRACERECORD, *TRACERECORDP;

#define TRACERECORD_SIZE 128

#define CacheMeter_Pwr 14

#define CacheMeter_DefaultFreq 1000
/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from vlm:alpha-emulator;aistat.sid  Any changes made to it will be lost. */

#endif


