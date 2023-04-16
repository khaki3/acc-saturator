COMP=nvhpc TAG= ./EXTRACT.sh > nvhpc.log
COMP=nvhpc TAG=-cse ./EXTRACT.sh > nvhpc-cse.log
COMP=nvhpc TAG=-nobulk ./EXTRACT.sh > nvhpc-nobulk.log
COMP=nvhpc TAG=-nosat ./EXTRACT.sh > nvhpc-nosat.log
COMP=nvhpc TAG=-sat ./EXTRACT.sh > nvhpc-sat.log

COMP=gcc TAG= ./EXTRACT.sh > gcc.log
COMP=gcc TAG=-cse ./EXTRACT.sh > gcc-cse.log
COMP=gcc TAG=-nobulk ./EXTRACT.sh > gcc-nobulk.log
COMP=gcc TAG=-nosat ./EXTRACT.sh > gcc-nosat.log
COMP=gcc TAG=-sat ./EXTRACT.sh > gcc-sat.log

COMP=clang TAG= ./EXTRACT.sh > clang.log
COMP=clang TAG=-cse ./EXTRACT.sh > clang-cse.log
COMP=clang TAG=-nobulk ./EXTRACT.sh > clang-nobulk.log
COMP=clang TAG=-nosat ./EXTRACT.sh > clang-nosat.log
COMP=clang TAG=-sat ./EXTRACT.sh > clang-sat.log

python3.8 spec-acc-cse.py
python3.8 spec-acc-nobulk.py
python3.8 spec-acc-nosat.py
python3.8 spec-acc-sat.py

python3.8 spec-omp-cse.py
python3.8 spec-omp-nobulk.py
python3.8 spec-omp-nosat.py
python3.8 spec-omp-sat.py
