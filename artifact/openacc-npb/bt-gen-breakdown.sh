
REG=`tempfile`
ORIG=`tempfile`
CSE=`tempfile`
NOBULK=`tempfile`
SAT=`tempfile`
NOSAT=`tempfile`
RATIO=`tempfile`
cat bt-nvhpc-reg.txt | tr -d '" ' | sort -t, -k 1 > $REG
cat BT/BT/nvhpc.log | grep _gpu | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $ORIG
cat BT/BT/nvhpc-cse.log | grep _gpu | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $CSE
cat BT/BT/nvhpc-nobulk.log | grep _gpu | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $NOBULK
cat BT/BT/nvhpc-sat.log | grep _gpu | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $SAT
cat BT/BT/nvhpc-nosat.log | grep _gpu | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $NOSAT
cat BT/BT/nvhpc.log | grep _gpu | tail -46 | awk '{ print $9 "," $1 }' | sort -t, -k 1 > $RATIO

paste $REG $ORIG $CSE $NOBULK $SAT $RATIO $NOSAT -d " " | tr , ' ' | awk '{ print $12 "," $4 / $6 "," $4 / $8 "," $4 / $10 "," $4 / $14 "," $2 }' | sort -t, -k 1 -g -r > bt-breakdown-nvhpc.log


cat bt-gcc-reg.txt | tr -d '" ' | sort -t, -k 1 > $REG
cat BT/BT/gcc.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $ORIG
cat BT/BT/gcc-cse.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $CSE
cat BT/BT/gcc-nobulk.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $NOBULK
cat BT/BT/gcc-sat.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $SAT
cat BT/BT/gcc-nosat.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $4 }' | sort -t, -k 1 > $NOSAT
cat BT/BT/gcc.log | grep _omp_fn | tail -46 | awk '{ print $9 "," $1 }' | sort -t, -k 1 > $RATIO

paste $REG $ORIG $CSE $NOBULK $SAT $RATIO $NOSAT -d " " | tr , ' ' | awk '{ print $12 "," $4 / $6 "," $4 / $8 "," $4 / $10 "," $4 / $14 "," $2 }' | sort -t, -k 1 -g -r > bt-breakdown-gcc.log


rm $REG $ORIG $CSE $NOBULK $SAT $RATIO $NOSAT

