#!/bin/bash

TAG= ACCSAT= make

export ACCSAT=/root/acc-saturator/accsat
export ACCSAT_DEBUG=1

TAG=-cse ACCSAT_NOSAT=1 ACCSAT_NOBULKLOAD=1 make
TAG=-nosat ACCSAT_NOSAT=1 make
TAG=-nobulk ACCSAT_NOBULKLOAD=1 make
TAG=-sat make
