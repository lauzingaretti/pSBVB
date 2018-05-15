###run polyploid toy example and sorting files in a new folder.    
 #cat  toy.gen | path-to sbvb -i toy.par
  #cat toy.gen | /home/useradmin/Desktop/Ordenar_Trabajo/diallelic/sbvb -i toy.par
  zcat GenosB.vcf.gz | perl vcf2tped2.pl -hap | cut -d ' ' -f 1,4- | bvb -i toy.par
           mkdir -p results
           cp Y.grm.1  results
           cp Y.outy   results
           cp Y.outq   results
           cp Y.outm.1.tped results 
           cp Y.outy.tfam results 
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy.tfam
           rm Y.outm.1.tped 

###done


 
