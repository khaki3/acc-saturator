## ACCSAT-Artifact

This archive provides the artifact to manifest our paper "ACC Saturator: Automatic Kernel Optimization for Directive-Based GPU Code". We evaluated the generated code by our source-code optimizer ACC Saturator.

### Preparation for SPEC ACCEL
As the SPEC ACCEL benchmark suite is proprietary software, we suggest that reviewers copy their local SPEC v1.3 to the spec-accel directory. Specify SPEC_PATH to the directory of SPEC ACCEL, and run the following commands in this artifact directory:

```sh
cp -r $SPEC_PATH/benchspec/ACCEL/{303.*,304.*,314.*,352.*,354.*,357.*,370.*} spec-accel/
cp -r $SPEC_PATH/benchspec/ACCEL/{503.*,504.*,514.*,552.*,554.*,557.*,570.*} spec-accel/
cp -r $SPEC_PATH/benchspec/ACCEL/114.mriq/data/ref/input spec-accel/314.omriq/data/ref/
pushd spec-accel; patch -p1 < spec.patch; popd
```

### Install with Dockerfile
We provide Dockerfile to build a container by the NVIDIA Container Toolkit. You would find two benchmark suites inside the container for the artifact evaluation.

```sh
% cp -r ../rewriter .
% docker build -t sc23_pap143s2 .
% docker run -v ~/:/work -it --rm --gpus all sc23_pap143s2

====================
== NVIDIA HPC SDK ==
====================

NVIDIA HPC SDK version 22.9

Copyright (c) 2022, NVIDIA CORPORATION & AFFILIATES.  All rights reserved.

root@8ebade416a10:~# ls
acc-saturator  openacc-npb  spec-accel
```

### Benchmarking
To generate plots from the results, you need to run all the benchmarks first:
```sh
pushd openacc-npb; make; popd
pushd spec-accel; ./RUN.sh; popd
```

You can get the list of the results from: `find | egrep '(nvhpc|gcc|clang).*.log'`

### Plot Generation
```sh
% pushd openacc-npb; bash gen-plot.sh; popd
% pushd spec-accel; bash gen-plot.sh; popd
```

Then, copy generated PDFs under your home directory:
```
% mkdir /work/pdf
% cp */*.pdf /work/pdf
```
