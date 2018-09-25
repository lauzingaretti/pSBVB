Nsample=1500
#shuf Data.chip | head -n $NQTL | sort -gk1,1 -gk2,2 > Y.qtn
shuf /home/useradmin/Desktop/20_DE_FEBRERO_2017/Aditivos_100/test.gen | head -n $Nsample | sort -gk1,1 -gk2,2 > toy_st.gen
awk -F'\t' '{print $1" "$2}' 'toy_st.gen' > Data_st.chip  
NQTL=40
shuf Data_st.chip | head -n $NQTL | sort -gk1,1 -gk2,2 > Y_st.qtn
NQTL1=80
shuf Data_st.chip | head -n $NQTL1 | sort -gk1,1 -gk2,2 > Y_80.qtn
NQTL2=150
shuf Data_st.chip | head -n $NQTL1 | sort -gk1,1 -gk2,2 > Y_150.qtn
