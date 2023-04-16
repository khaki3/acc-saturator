
CLASS=C

NVPROF=nsys nvprof

NCU=ncu
NCU_METRICS=smsp__sass_thread_inst_executed_op_dadd_pred_on.sum,smsp__sass_thread_inst_executed_op_dmul_pred_on.sum,smsp__sass_thread_inst_executed_op_dfma_pred_on.sum

ACCEXEC_NVHPC=$(NVPROF) ./nvhpc $$OPT 2>&1 | tee -a nvhpc.log; \
	$(NVPROF) ./nvhpc $$OPT 2>&1 | tee -a nvhpc.log; \
 	$(NVPROF) ./nvhpc $$OPT 2>&1 | tee -a nvhpc.log; \
 	$(NVPROF) ./nvhpc-cse $$OPT 2>&1 | tee -a nvhpc-cse.log; \
 	$(NVPROF) ./nvhpc-cse $$OPT 2>&1 | tee -a nvhpc-cse.log; \
 	$(NVPROF) ./nvhpc-cse $$OPT 2>&1 | tee -a nvhpc-cse.log; \
 	$(NVPROF) ./nvhpc-nosat $$OPT 2>&1 | tee -a nvhpc-nosat.log; \
 	$(NVPROF) ./nvhpc-nosat $$OPT 2>&1 | tee -a nvhpc-nosat.log; \
 	$(NVPROF) ./nvhpc-nosat $$OPT 2>&1 | tee -a nvhpc-nosat.log; \
 	$(NVPROF) ./nvhpc-nobulk $$OPT 2>&1 | tee -a nvhpc-nobulk.log; \
 	$(NVPROF) ./nvhpc-nobulk $$OPT 2>&1 | tee -a nvhpc-nobulk.log; \
 	$(NVPROF) ./nvhpc-nobulk $$OPT 2>&1 | tee -a nvhpc-nobulk.log; \
 	$(NVPROF) ./nvhpc-sat $$OPT 2>&1 | tee -a nvhpc-sat.log; \
 	$(NVPROF) ./nvhpc-sat $$OPT 2>&1 | tee -a nvhpc-sat.log; \
	$(NVPROF) ./nvhpc-sat $$OPT 2>&1 | tee -a nvhpc-sat.log

ACCEXEC_GCC=LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc $$OPT 2>&1 | tee -a gcc.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc $$OPT 2>&1 | tee -a gcc.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc $$OPT 2>&1 | tee -a gcc.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-cse $$OPT 2>&1 | tee -a gcc-cse.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-cse $$OPT 2>&1 | tee -a gcc-cse.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-cse $$OPT 2>&1 | tee -a gcc-cse.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nosat $$OPT 2>&1 | tee -a gcc-nosat.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nosat $$OPT 2>&1 | tee -a gcc-nosat.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nosat $$OPT 2>&1 | tee -a gcc-nosat.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nobulk $$OPT 2>&1 | tee -a gcc-nobulk.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nobulk $$OPT 2>&1 | tee -a gcc-nobulk.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-nobulk $$OPT 2>&1 | tee -a gcc-nobulk.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-sat $$OPT 2>&1 | tee -a gcc-sat.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-sat $$OPT 2>&1 | tee -a gcc-sat.log; \
 	LD_LIBRARY_PATH=/usr/local/lib64 $(NVPROF) ./gcc-sat $$OPT 2>&1 | tee -a gcc-sat.log

ACCEXEC=ulimit -s unlimited; $(ACCEXEC_NVHPC); $(ACCEXEC_GCC)

.PHONY: BT  CG  EP  FT  LU  MG SP

all: BT  CG  EP  FT  LU  MG SP

BT  CG  EP  FT  LU  MG SP:
	cd $@/$@; export BENCH_LOWER=$$(echo $@ | tr '[:upper:]' '[:lower:]'); rm -f *.log *.sqlite *.nsys-rep; \
	make clean; CC=pgcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x nvhpc; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOSAT=1 ACCSAT_NOBULKLOAD=1 CC=pgcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x nvhpc-cse; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOSAT=1 CC=pgcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x nvhpc-nosat; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOBULKLOAD=1 CC=pgcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x nvhpc-nobulk; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat CC=pgcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x nvhpc-sat; \
	make clean; CC=gcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x gcc; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOSAT=1 ACCSAT_NOBULKLOAD=1 CC=gcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x gcc-cse; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOSAT=1 CC=gcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x gcc-nosat; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat ACCSAT_NOBULKLOAD=1 CC=gcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x gcc-nobulk; \
	make clean; ACCSAT_DEBUG=1 ACCSAT=/root/acc-saturator/accsat CC=gcc make CLASS=$(CLASS); mv ./$$BENCH_LOWER.$(CLASS).x gcc-sat; \
	$(ACCEXEC)
