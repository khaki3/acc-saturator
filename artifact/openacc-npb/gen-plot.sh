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

python3.8 npb-cse.py
python3.8 npb-nobulk.py
python3.8 npb-nosat.py
python3.8 npb-sat.py

bash bt-gen-breakdown.sh
python3.8 bt-breakdown-nvhpc.py
python3.8 bt-breakdown-gcc.py
