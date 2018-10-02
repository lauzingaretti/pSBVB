###run polyploid toy example and sorting files in a new folder.    
 #cat  toy.gen | path-to sbvb -i toy.par
 # run the option h2=0.3  0.4 and .0.5 G mimic diploid
 
cat toy_st.gen | ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb -i file_1.par
           mkdir -p results_example1
           cp Y.grm.1  results_example1
           cp Y.outy   results_example1
           cp Y.outq   results_example1
           cp Y.outm*.1.gz   results_example1
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy           
           rm Y.outm*.1.gz
           
        
#h2= 0.5 y G mimic diploid
 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb   -i file_2.par
           mkdir -p results_example2
           cp Y.grm.1  results_example2
           cp Y.outy   results_example2
           cp Y.outq   results_example2
           cp Y.outm*.1.gz  results_example2
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy           
           rm Y.outm*.1.gz
           
         
#h2= 0.5 y G mimic diploid
 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_3.par
           mkdir -p results_example3
           cp Y.grm.1  results_example3
           cp Y.outy   results_example3
           cp Y.outq   results_example3
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy
         
#h2 0.5 y G total 

 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_4.par
           mkdir -p results_example4
           cp Y.grm.1  results_example4
           cp Y.outy  results_example4
           cp Y.outq   results_example4
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy
 

 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_5.par
           mkdir -p results_example5
           cp Y.grm.1  results_example5
           cp Y.outy   results_example5
           cp Y.outq   results_example5
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy
 
 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_6.par
           mkdir -p results_example6
           cp Y.grm.1  results_example6
           cp Y.outy   results_example6
           cp Y.outq   results_example6
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy
           
 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_7.par
           mkdir -p results_example7
           cp Y.grm.1  results_example7
           cp Y.outy   results_example7
           cp Y.outq   results_example7
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy           
           
 cat toy_st.gen |  ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file_8.par
           mkdir -p results_example8
           cp Y.grm.1  results_example8
           cp Y.outy   results_example8
           cp Y.outq   results_example8
           cp Y.outm*.1.gz  results_example8
           rm Y.grm.1 
           rm Y.outq
           rm Y.outy           
           rm Y.outm*.1.gz
           
