
for i in *.txt 
do
    echo $i > gout.txt
    #echo $i > data${c}.txt
    psiblast -query $i -db ../../nr/nr -out $i.out -num_iterations 3 -out_ascii_pssm $i.pssm -inclusion_ethresh 0.001 -num_threads 8    
done
