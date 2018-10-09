NQTL=100

    # run models 100 times simultaneosly, using different parameters option (3 matrices G)
    # Models to additive effects
    # Script to generate simulationes (ADD EFF)
    # LZ 
        for i in $(seq 2 100); do

           shuf Data.chip | head -n $NQTL | sort -gk1,1 -gk2,2 > Y.qtn

        cat  test.gen | /path to program  -i sbvb.par
           mkdir -p Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1  Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.outy   Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.outq   Random/Y_qr_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
        cat  test.gen | /path to program  -i sbvbDip.par
           mkdir -p Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.grm.1  Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.outy  Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.outq   Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
        cat  test.gen | /path to program  -i sbvbRestDip.par
           mkdir -p  Random/Y_qr_chipRestringido_GDiploide_$(echo $i)
           cp Y.grm.1  Random/Y_qr_chipRestringido_GDiploide_$(echo $i)
           cp Y.outy   Random/Y_qr_chipRestringido_GDiploide_$(echo $i)
           cp Y.outq   Random/Y_qr_chipRestringido_GDiploide_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
     rm *hap

     done
      for i in $(seq 2 100); do
       cat  test.gen | /path to program  -i sbvbYPath.par
            mkdir -p QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1 QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.outy  QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.outq  QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy

       cat  test.gen | /path to program  -i sbvbYPathDip.par
            mkdir -p  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.grm.1 QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.outy  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.outq  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy

       cat  test.gen | /path to program  -i sbvbYPathRestDip.par
            mkdir -p  QAP/Y_qap_chipRestringido_GDiploide_$(echo $i)
            cp Y.grm.1 QAP/Y_qap_chipRestringido_GDiploide_$(echo $i)
            cp Y.outy  QAP/Y_qap_chipRestringido_GDiploide_$(echo $i)
            cp Y.outq  QAP/Y_qap_chipRestringido_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy
        rm *hap

        done
####ultimos casos

 for i in $(seq 2 100); do
 cat  test.gen | /path to program  -i  sbvbYBiallelicPath.par
            mkdir -p   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1  QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.outy   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.outq   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy

       cat  test.gen | /path to program  -i sbvbYBiallelicPathDip.par
            mkdir -p  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.grm.1 QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.outy  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.outq  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy

       cat  test.gen | /path to program  -i sbvbYBiallelicPathRestDip.par
            mkdir -p  QBP/Y_qbp_chipRestringido_GDiploide_$(echo $i)
            cp Y.grm.1 QBP/Y_qbp_chipRestringido_GDiploide_$(echo $i)
            cp Y.outy  QBP/Y_qbp_chipRestringido_GDiploide_$(echo $i)
            cp Y.outq  QBP/Y_qbp_chipRestringido_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy
        rm *hap

        done

###done
