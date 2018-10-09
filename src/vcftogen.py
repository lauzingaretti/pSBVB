#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  8 12:00:00 2018
@author: Laura Zingaretti (laura.zingaretti@gmail.com)
This function parses vcf or genotype file,
genotype varying between 0 and ploidy level in both, 
either plain text or gz compressed
prints raw genotype data and snp positions
missing have your dataset missing values? if is True values are imputed getting in account allelic frecuency 
INPUT:
- sys.stdin read file using sys : standar input vcf file name with (sequence) genotypes [req]
- ploidy (int): ploidy level 

"""

import numpy as np
import random
import re 
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-p", "--ploidy",type = int , default= 2,help="ploidy level in dataset")
parser.add_argument("-m", "--missing",  action='store_true', help="variable to indicates if you want to impute missing values")




args = vars(parser.parse_args())

ploidy= args["ploidy"]  
missing = args["missing"]
ploidy=int(ploidy)



if ploidy%2 != 0: print('ULL: ploidy must be even')
vcf= sys.stdin

for ln in vcf:
    if ((not ln.startswith('#'))):
                           Cols=re.split('[\t , \n]',ln)
                           if (("/"  in Cols[9]) | ("|"  in Cols[9])): 
                               Info_ch_pos = Cols[0:2]
                               Genos=Cols[9:len(Cols)]
                               #calculate frecuency to give probabilities in sampling ifg missing (0 and 1 )
                               ones = sum([m.split(":")[0].count("1") for m in Genos])
                               zeros= sum([m.split(":")[0].count("0") for m in Genos])
                               fr=ones/(ones+zeros)
                                     
                               genotypes=[]
                               for gen in Genos:
                                   #remove dirty from each line, all contents before : 
                                   #names.append(gen.split(':')[0])
                                   #answering if there aren't missing value at the position
                                   if((gen.split(':')[0].count('/') + gen.split(':')[0].count('|') +1)== ploidy):
                                       #answering if data are phased or not
                                       if(str(gen.split(':')[0])[1]=='/'):
                                           x=gen.split(':')[0].split("/") 
                                           random.sample(x, ploidy)
                                           genotypes.append(x)
                                       if(str(gen.split(':')[0])[1]=='|'):
                                           genotypes.append(gen.split(':')[0].split("|")) 
                                   elif missing is True: 
                                       #if there are missing values sampling randomly in agreement with ploidy estimated 
                                       #print('warning: missing data will be imputed')
                                       s=[0,1]
                                       x=np.random.choice(s,ploidy,p=[(1-fr),fr])
                                       genotypes.append(x)
                                   else: sys.exit("Stop: missing values in your dataset use -m option")    
                               xx = [y for x in genotypes for y in x]   
                               a= Info_ch_pos + xx
                               print(" ".join(str(v) for v in a))
                                                                       
                           else: 
                               if (re.search('[a-zA-Z]', Cols[2]) is None):
                                   Info_ch_pos = Cols[0:2]
                                   Genos=Cols[2:len(Cols)]
                                   #calculate frecuency to give probabilities in sampling ifg missing (0 and 1 )
                                   ones=0
                                   for m in Genos: 
                                       if len(m)>0:
                                           ones=ones+int(m)
                                   zeros=ploidy*len(Genos) -ones        
                                   fr=ones/(ones+zeros)
                                     
                                       
                                   genotypes=[]
                                   for gen in Genos:
                                       if ((len(gen)==0) | (gen== str(".")) | (gen==str("NA"))): 
                                           if(missing is True):  
                                               s=[0,1]
                                               #1-fr prob of 0 and fr prob of 1 
                                               x=np.random.choice(s,ploidy,p=[(1-fr),fr])
                                               genotypes.append(x)
                                           if(missing is False): 
                                               sys.exit("Missing data in your dataset: run with -m option")
                                       else:
                                           x = np.random.permutation(np.concatenate((np.repeat(1, int(gen)),np.repeat(0,(int(ploidy)-int(gen))))))
                                           genotypes.append(x)
                                           
                                   xx = [y for x in genotypes for y in x]   
                                   a= Info_ch_pos + xx
                                   print(" ".join(str(v) for v in a))
                                    
   
  
