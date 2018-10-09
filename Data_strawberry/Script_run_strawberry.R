##Run models after simulations###
##Strawberry dataset###
##Chose WD
#setwd("Dir")

#read Ped based matrix
#Ped<-read.table("fileS43.nrm")
#Go to dir where are models' output
#setwd("~/Aditivos_100")
#or setwd("~/Dominantes_100")
#Look for phenos and G matrix
'''
Phenotypos<-list.files(recursive =TRUE,pattern = ".outy")
Gmatrix<-list.files(recursive = TRUE,pattern=".grm")
'''

#parallelize using parallel library 
library("parallel")
#Source the functions needed
'''
source("/Additional_Functions/pedigree.R")
source("/Additional_Functions/RelationshipMatrix.R")
source("/Additional_Functions/GBlupFunction.R")
source("/Additional_Functions/generate_vcf_from_gen.R")
'''
'''
h2=0.5
ntraits=1
# split testing and training(the last 150 individuals are used to test)
make_predictions=c(554:1553)
Estimac<-c()
for(i in 1: length(Phenotypos)){
  Y<-read.table(Phenotypos[[i]])
  Gene<-read.table(Gmatrix[[i]])
  PredGenetic<-GBlup_predict(G=Gene,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)
  PredPed<-GBlup_predict(G=Ped,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)
  Rep<-strsplit(Phenotypos[[i]],"/")[[1]][2]
  Estimac<-rbind(Estimac,c(Rep,PredGenetic$rho,PredPed$rho))
  rm(Y)
  rm(Gene)
}

'''
#write Estimac as table and perform ggplot boxplot 
#colnames(Estimac)

'''
Estimations<-apply(Estimac[,-1],2,as.numeric)
Estimations[,1:2]<-sqrt(Estimations[,1:2])
as.numeric(Estimations[,2])
'''

'''
Genetics_Architectures<-c(rep("RQP",300),rep("RQDp",300),rep("RQG",300))
G_Estimation<-rep(c(rep("G2*",100),rep("GT",100),rep("G2",100)),3)
G_estim<-rep("BLUP",900)
'''
'''
Genetics_comp_factors<-data.frame(Genetics_Architectures,G_Estimation,Estimations[,1])
Pedigree_comp_factors<-data.frame(Genetics_Architectures,G_estim,Estimations[,2])
colnames(Genetics_comp_factors)<-c("Arch","Genetic","Est")
colnames(Pedigree_comp_factors)<-c("Arch","Genetic","Est")
RelComp<-data.frame(Genetics_Architectures,G_Estimation,(Genetics_comp_factors[,3]-Pedigree_comp_factors[,3])/Genetics_comp_factors[,3])

library(ggplot2)

boxplot<-ggplot(aes(y = Genetics_comp_factors[,3], x = Arch, fill = Genetic),
             data = Genetics_comp_factors,  axisLine = c(0.5, "solid", "black"),
             removePanelGrid=TRUE,removePanelBorder=TRUE) 

print(boxplot)
'''