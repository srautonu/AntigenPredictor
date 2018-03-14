export BLASTDB=$HOME/blastDB
for i in *.fasta
do
  echo $i > curProtein.txt
  echo Running PSI Blast for $i ...
  psiblast -query $i -db nr -out $i.out -num_iterations 3 -out_ascii_pssm $i.pssm -inclusion_ethresh 0.001 -num_threads 8
  echo PSI Blast completed for $i.
  echo $i >> completedProteins.txt
done
