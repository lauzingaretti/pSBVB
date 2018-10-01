###RUN POTATO EXAMPLE FROM GENOTYPES ####
##PREPARING DATASET######################
####GENERATE DATA FROM POTATO genotype####
## The program allows unphased SNPs owing to incorporate an inner option to expad base population 
## Use expand base population and exclude initial individuals in order to generate desequilibrium and perform models. 

# call your functions 
# go to container folder, for example:  
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions/")
source("pedigree.R")
source("RelationshipMatrix.R")
source("GBlupFunction.R")
source("generate_vcf_from_gen.R")

# Locate the genotypes' folder 
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/")

# Read map and genotypes 
#read map file, obtained from: 
# Rosyara, U. R., De Jong, W. S., Douches, D. S., & Endelman, J. B. (2016). 
#Software for genome-wide association studies in autopolyploids and
#its application to potato. The plant genome, 9(2).

map<-read.table("Sol_tet_mapa.map",sep="\t")

#read genotypes, obtained from Enciso-Rodriguez, F., D. Douches, M. Lopez-Cruz, J. Coombs, and
#G. de los Campos, 2018 Genomic selection for late blight and common scab resistance in tetraploid potato (solanum tubero-
# sum). G3: Genes, Genomes, Genetics pp. g3–200273.

G<-read.delim("Pot_gen.gen",sep="\t")


# generate files 
dim(map)
dim(G)
Chip=map[,c(2:3)]
colnames(Chip)<-NULL

Chip[order(Chip[,1],Chip[,2]),]
#generate a chip file 

Chip<-Chip[-c(which(Chip[,1]==0)),]
write.table(Chip, "Data.chip",sep="\t",row.names = FALSE,col.names=FALSE)
#select a sub-sample from whole dataset 

G=G[sample(c(1:nrow(G)),150),]
G=G[,sample(c(1:ncol(G)),500)]
G<-G[,colnames(G)%in%map[,1]]
dim(G)

#generate genotypes vcf format 
A=GenotoVcf(G,p=4,map,path="NULL")

dim(A$file)
dim(A$data)

write.table(A$file[-c(which(A$file[,1]==0)),],"data.gen",row.names = FALSE,col.names=FALSE,sep="\t")

A$file<-A$file[-c(which(A$file[,1]==0)),]

#generate random QTNs file
QTN<-A$file[sample(c(1:nrow(A$file)),140),c(1,2)]
QTN<-QTN[order(QTN[,1],QTN[,2]),]
write.table(QTN,"File.qtn",sep="\t",row.names = FALSE,col.names=FALSE)

#generate pedigree file (see help in Pedigree.R)
dim(A$file)

#generate pedigree file 
pedgenerator(250,4,c(rep(100,3),150),sex=FALSE,path="/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/"
             ,exclude=150)

data<-read.table("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/File_st.ped",header=FALSE)
#check the dimensions of dataset 
#generate relationship matrix (see help in RelMatrix)
dim(data)


A<-RelMatrix(data,dominance=FALSE,path= "/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/")

#### After running pSBVB (see help file)


####RUN GBLUP and made predictions
library(ggplot2)

### calling GBlup function 
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions/")
source("GBlupFunction.R")

# Read files 
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/results_potato/")

G<- read.table("Y.grm.1")
y<- read.table("Y.outy")

RelMatrix<-A[-c(1:150),-c(1:150)]
dim(G)

#check matrix dimention
dim(y)
dim(G)
dim(RelMatrix)

h2=0.5
ntraits=1

make_predictions=c(401:550)
library(ggplot2)
S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)
S$rho

Training=S$yout_corr[1:400]
Train=S$uhat[1:400] + S$mean
cor(Training, Train)

Phenohat<-S$uhat[make_predictions] + S$mean
Pheno<-S$yout_corr[make_predictions]
cor(Pheno,Phenohat)


datos_Train<-data.frame(Train, Training)
datos<-data.frame(Phenohat,Pheno)
colnames(datos)<-c("Yhat","Y")
colnames(datos_Train)<-colnames(datos)
Dataset<-rbind(datos_Train,datos)
Dataset[1:400,3]<-"Training"
Dataset[401:550,3]<-"Testing"
colnames(Dataset)[3]<-"Class"
apply(datos,2,class)
ggplot(datos, aes(y=Yhat, x=Y,colour="red")) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  geom_abline(intercept=lm(Yhat ~ Y,data=datos)$coefficients[1], slope=lm(Yhat ~ Y,data=datos)$coefficients[2])+
  #scale_x_continuous(limits = c(min(datos[,1]), max(datos[,1])))+
  #scale_y_continuous(limits = c(min(datos[,2]), max(datos[,2])))+
  
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")


#Training and Test Plot
ggplot(Dataset, aes(y=Yhat, x=Y,colour=Class)) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  
  #geom_abline(intercept=lm(Dataset$Yhat[Dataset[,3]=="Training",] ~ Dataset$Y[Dataset[,3]=="Training",])$coefficients[1], slope=lm(Dataset$Yhat[Dataset[,3]=="Training",] ~ Dataset$Y[Dataset[,3]=="Training",])$coefficients[2])+
  #scale_x_continuous(limits = c(min(datos[,1]), max(datos[,1])))+
  #scale_y_continuous(limits = c(min(datos[,2]), max(datos[,2])))+
  
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




make_predictions=c(401:550)


SR<-GBlup_predict(G=RelMatrix,y=y,h2=h2,make_predictions=c(401:550),ntraits=ntraits)
SR$rhoM

Pheno<-SR$yout_corr[make_predictions]
datos_r<-data.frame(Phenohat,Pheno)
colnames(datos_r)<-c("Yhat","Y")

#Relationship- Based  Prediction 

ggplot(datos_r, aes(y=Yhat, x=Y,colour="red")) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  geom_abline(intercept=lm(Yhat ~ Y,data=datos_r)$coefficients[1], slope=lm(Yhat ~ Y,data=datos_r)$coefficients[2])+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")



