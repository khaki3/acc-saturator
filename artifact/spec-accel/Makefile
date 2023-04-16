
NVHPC=nvc
NVHPC_OPT=-DSPEC=1 -O3 -gpu=cc80,ptxinfo,fastmath -Minfo=accel -Minfo=mp -Msafeptr
NVHPC_ACC_OPT=-acc=gpu $(NVHPC_OPT)
NVHPC_OMP_OPT=-mp=gpu $(NVHPC_OPT)

GCC=gcc
GCC_OPT=-DSPEC=1 -DSPEC_NO_INLINE=1 -DSPEC_NEED_EXPLICIT_SIZE=1 -O3 -lm -mcmodel=large -foffload=nvptx-none -foffload=-lm -foffload=-latomic -ffast-math
GCC_ACC_OPT=-fopenacc $(GCC_OPT)
GCC_OMP_OPT=-fopenmp $(GCC_OPT)

CLANG=clang
CLANG_OPT=-DSPEC=1 -lm -fopenmp -fopenmp-targets=nvptx64 -O3 -ffast-math -fbracket-depth=4096

NVPROF=nsys nvprof

NCU=ncu
NCU_METRICS=smsp__sass_thread_inst_executed_op_dadd_pred_on.sum,smsp__sass_thread_inst_executed_op_dmul_pred_on.sum,smsp__sass_thread_inst_executed_op_dfma_pred_on.sum

COMPINIT=ulimit -s unlimited; rm -f nvhpc$$TAG.log gcc$$TAG.log clang$$TAG.log nvhpc$$TAG gcc$$TAG clang$$TAG *.sqlite *-rep

ACCCOMP_NVHPC=$$ACCSAT $(NVHPC) $(NVHPC_ACC_OPT) $$FILE -o nvhpc$$TAG

ACCCOMP_GCC=$$ACCSAT $(GCC) $(GCC_ACC_OPT) $$FILE -o gcc$$TAG

ACCCOMP=$(COMPINIT); $(ACCCOMP_NVHPC); $(ACCCOMP_GCC)

OMPCOMP_NVHPC=$$ACCSAT $(NVHPC) $(NVHPC_OMP_OPT) $$FILE -o nvhpc$$TAG

OMPCOMP_GCC=$$ACCSAT $(GCC) $(GCC_OMP_OPT) $$FILE -o gcc$$TAG

OMPCOMP_CLANG=$$ACCSAT $(CLANG) $(CLANG_OPT) $$FILE -o clang$$TAG

OMPCOMP=$(COMPINIT); $(OMPCOMP_NVHPC); $(OMPCOMP_GCC); $(OMPCOMP_CLANG)

ACCEXEC_NVHPC=$(NVPROF) ./nvhpc$$TAG $$OPT 2>&1 | tee -a nvhpc$$TAG.log; \
	$(NVPROF) ./nvhpc$$TAG $$OPT 2>&1 | tee -a nvhpc$$TAG.log; \
	$(NVPROF) ./nvhpc$$TAG $$OPT 2>&1 | tee -a nvhpc$$TAG.log

ACCEXEC_GCC=LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc$$TAG $$OPT 2>&1 | tee -a gcc$$TAG.log; \
	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc$$TAG $$OPT 2>&1 | tee -a gcc$$TAG.log; \
	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc$$TAG $$OPT 2>&1 | tee -a gcc$$TAG.log

ACCEXEC=$(ACCEXEC_NVHPC); $(ACCEXEC_GCC)

OMPEXEC_NVHPC=$(ACCEXEC_NVHPC)

OMPEXEC_GCC=$(ACCEXEC_GCC)

OMPEXEC_CLANG=LD_LIBRARY_PATH=/usr/local/lib $(NVPROF) ./clang$$TAG $$OPT 2>&1 | tee -a clang$$TAG.log; \
	LD_LIBRARY_PATH=/usr/local/lib $(NVPROF) ./clang$$TAG $$OPT 2>&1 | tee -a clang$$TAG.log; \
	LD_LIBRARY_PATH=/usr/local/lib $(NVPROF) ./clang$$TAG $$OPT 2>&1 | tee -a clang$$TAG.log

OMPEXEC=$(OMPEXEC_NVHPC); $(OMPEXEC_GCC); $(OMPEXEC_CLANG)

PROF_NVHPC=$(NCU) --metrics $(NCU_METRICS) ./nvhpc$$TAG $$OPT 2>&1 | tee -a nvhpc$$TAG.log

PROF_GCC=LD_LIBRARY_PATH=/usr/local/lib64 $(NCU) --metrics $(NCU_METRICS) ./gcc$$TAG $$OPT 2>&1 | tee -a  gcc$$TAG.log

PROF_CLANG=LD_LIBRARY_PATH=/usr/local/lib $(NCU) --metrics $(NCU_METRICS) ./clang$$TAG $$OPT 2>&1 | tee -a clang$$TAG.log

PROF=$(PROF_NVHPC); $(PROF_GCC); $(PROF_CLANG)

all: 303 304 314 352 354 357 370 503 504 514 552 554 557 570

303:
	cd $@.*/src; \
	export OPT="512 512 98 20000"; \
	export FILE="-Ipbcommon_sources *.c */*.c"; \
	$(ACCCOMP); $(ACCEXEC)

304:
	cd $@.*/src; \
	export OPT="5000 reference.dat 0 0 ../data/ref/input/100_100_130_ldc.of"; \
	export FILE="*.c"; \
	$(ACCCOMP); $(ACCEXEC)

# Need to copy 114.mriq/data/ref/input/
314:
	cd $@.*/src; \
	export OPT="-i ../data/ref/input/128x128x128.bin 200000"; \
	export FILE="-Ipbcommon_sources main.c file.c */*.c"; \
	$(ACCCOMP); $(ACCEXEC)

352:
	cd $@.*/src; \
	rm -f ep.input; ln -s ../data/ref/input/ep.input .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(ACCCOMP); $(ACCEXEC_NVHPC); \
	rm ep.input; ln -s ../data/test/input/ep.input .; \
	$(ACCEXEC_GCC); rm ep.input

354:
	cd $@.*/src; \
	rm -f cg.input; ln -s ../data/ref/input/cg.input .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(ACCCOMP); $(ACCEXEC); rm cg.input

357:
	cd $@.*/src; \
	rm -f inputsp.data; ln -s ../data/ref/input/inputsp.data .; \
	export OPT=""; \
	export FILE="`ls *.c | grep -v tzetar`"; \
	$(ACCCOMP); $(ACCEXEC_NVHPC); \
	rm inputsp.data; ln -s ../data/test/input/inputsp.data .; \
	$(ACCEXEC_GCC); rm inputsp.data

370:
	cd $@.*/src; \
	rm inputbt.data; ln -s ../data/ref/input/inputbt.data .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(ACCCOMP); $(ACCEXEC_NVHPC); \
	rm inputbt.data; ln -s ../data/test/input/inputbt.data .; \
	$(ACCEXEC_GCC); rm inputbt.data

503:
	cd $@.*/src; \
	export OPT="512 512 98 20000"; \
	export FILE="*.c"; \
	$(OMPCOMP); $(OMPEXEC)

504:
	cd $@.*/src; \
	export OPT="5000 reference.dat 0 0 ../data/ref/input/100_100_130_ldc.of"; \
	export FILE="*.c"; \
	$(OMPCOMP); $(OMPEXEC)

514:
	cd $@.*/src; \
	export OPT="-i ../data/ref/input/le/128x128x128.bin 200000"; \
	export FILE="main.c file.c parboil.c"; \
	$(OMPCOMP); $(OMPEXEC)

552:
	cd $@.*/src; \
	rm -f ep.input; ln -s ../data/ref/input/ep.input .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(COMPINIT); $(OMPCOMP_NVHPC); $(OMPCOMP_GCC); \
	if [ -z $${ACCSAT+x} ]; then \
		$(CLANG) $(CLANG_OPT) $$FILE -o clang; \
	else \
		rm -f *.o ep2.c; \
		$$ACCSAT $(CLANG) $(CLANG_OPT) -c `ls *.c | grep -v ep.c`; \
		$$ACCSAT $(CLANG) --version ep.c; \
		grep '#include' ep.c > ep2.c; awk '/#pragma/,0' ep.c.sat >> ep2.c; \
		$(CLANG) $(CLANG_OPT) -c ep2.c; $(CLANG) $(CLANG_OPT) *.o -o clang$$TAG; \
		rm ep2.c; \
	fi; \
	$(OMPEXEC); rm ep.input

554:
	cd $@.*/src; \
	rm -f cg.input; ln -s ../data/ref/input/cg.input .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(OMPCOMP); $(OMPEXEC); rm cg.input

557:
	cd $@.*/src; \
	rm -f inputsp.data; ln -s ../data/ref/input/inputsp.data .; \
	export OPT=""; \
	export FILE="`ls *.c | grep -v tzetar`"; \
	$(OMPCOMP); $(OMPEXEC); rm inputsp.data

570:
	cd $@.*/src; \
	rm -f inputbt.data; ln -s ../data/ref/input/inputbt.data .; \
	export OPT=""; \
	export FILE="*.c"; \
	$(OMPCOMP); $(OMPEXEC); rm inputbt.data
