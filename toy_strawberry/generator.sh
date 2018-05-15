NQTL=40
shuf Data_st.chip | head -n $NQTL | sort -gk1,1 -gk2,2 > Y_st.qtn
