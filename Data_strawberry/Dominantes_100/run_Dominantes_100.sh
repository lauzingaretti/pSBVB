
    # Simulations to Dominant effects
    # Dominant effects are stated equals to additvies
    # Then, we used the additive effects from additve effects simulations

        for i in $(seq 2 100); do
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/Random/Y_qr_chipCompleto_GReal_$(echo $i)/Y.outq > Y.qtn
        #rm *hap
        cat  test.gen | /path_to/_soft/sbvb -i sbvb.par
           mkdir -p Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1  Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.outy   Random/Y_qr_chipCompleto_GReal_$(echo $i)
           cp Y.outq   Random/Y_qr_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/Random/Y_qr_chipCompleto_GDiploide_$(echo $i)/Y.outq > Y.qtn
        cat  test.gen | /path_to/_soft/sbvb -i sbvbDip.par
           mkdir -p Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.grm.1  Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.outy  Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           cp Y.outq   Random/Y_qr_chipCompleto_GDiploide_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/Random/Y_qr_chipRestringido_GDiploide_$(echo $i)/Y.outq > Y.qtn
        cat  test.gen | /path_to/_soft/sbvb -i sbvbRestDip.par
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
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QAP/Y_qap_chipCompleto_GReal_$(echo $i)/Y.outq > YPathways.qtn
       cat  test.gen | /path_to/_soft/sbvb -i sbvbYPath.par
            mkdir -p QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1 QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.outy   QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           cp Y.outq   QAP/Y_qap_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)/Y.outq > YPathways.qtn
       cat  test.gen | /path_to/_soft/sbvb -i sbvbYPathDip.par
            mkdir -p  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.grm.1 QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.outy  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            cp Y.outq  QAP/Y_qap_chipCompleto_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QAP/Y_qap_chipRestringido_GDiploide_$(echo $i)/Y.outq > YPathways.qtn
       cat  test.gen | /path_to/_soft/sbvb -i sbvbYPathRestDip.par
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
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QBP/Y_qbp_chipCompleto_GReal_$(echo $i)/Y.outq > YBialleic.qtn
 cat  test.gen | /path_to/_soft/sbvb -i  sbvbYBiallelicPath.par
            mkdir -p   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.grm.1  QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.outy   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           cp Y.outq   QBP/Y_qbp_chipCompleto_GReal_$(echo $i)
           rm Y.grm.1
           rm Y.outq
           rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)/Y.outq > YBialleic.qtn
       cat  test.gen | /path_to/_soft/sbvb -i sbvbYBiallelicPathDip.par
            mkdir -p  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.grm.1 QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.outy  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            cp Y.outq  QBP/Y_qbp_chipCompleto_GDiploide_$(echo $i)
            rm Y.grm.1
            rm Y.outq
            rm Y.outy
awk  '{print  $1,"",$2,"",$5,"",$5}' ~/Aditivos_100/QBP/Y_qbp_chipRestringido_GDiploide_$(echo $i)/Y.outq > YBialleic.qtn
       cat  test.gen | /path_to/_soft/sbvb -i sbvbYBiallelicPathRestDip.par
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
#This script create  folders in parten dir with QAP, QBP and Random, to the three genetics architectures. 
