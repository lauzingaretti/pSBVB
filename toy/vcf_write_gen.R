library(vcfR)
data(vcfR_test)
vcfR_test
head(vcfR_test)
orig_dir <- getwd()
temp_dir <- tempdir()
setwd( temp_dir )
write.vcf( vcfR_test, file = "vcfR_test.vcf.gz" )
vcf <- read.vcfR( file = "vcfR_test.vcf.gz", verbose = FALSE )
vcf
setwd( orig_dir )
#ejemplo para saber cómo hacer con los archivos 
#luego leo mis datos, tengo que escribirlos en este formato
#tengo las tres partes
#meta es el encabezado que tiene todo file vcf
#fix es la parte fija q tienen los files
#gt es la parte de la variación genotípica
#espero que me soporte los files con poliploidía 
dim(vcf@fix)
dim(vcf@gt)
#fix tiene la info de los cromosomas para todas las pos
vcf@gt
#tiene el formato y los nombres de los individuos con la chorrada
#son tres individuos y el format que podría poner cualquier cosa
vcf@fix
#tiene Chrom POS Id del chrom que no será nada
#Ref y Alt que los dejo igual QUAL que le puedo poner un nu,ero fijo
#filter que le pongo PASS
#info que le voy a poner cualquier cosa fija 
dim(vcf@fix)
#son ocho columnas

#readdatasets to create files
setwd("/home/useradmin/Dropbox/DoctoradoCRAG/Salidas29DeAgosto/20Reads/")
setwd("/home/useradmin/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy")
dir()
M2<-read.table("toy.gen",sep="")

dim(M2)
A<-new("vcfR",meta=character(),fix=matrix(),gt=matrix())
class(A)

A@fix<-matrix(,nrow=nrow(M2),ncol=ncol(vcf@fix))
colnames(A@fix)<-colnames(vcf@fix)
A@fix
colnames(A@fix)
A@fix[,1]<-M2[,1]
A@fix[,2]<-M2[,2]
A@fix[,3]<-"SNP"
A@fix[,4]<-"A"
A@fix[,5]<-"G"
A@fix[,6]<-30
A@fix[,7]<-"."
A@fix[,8]<-"."
A@fix

M2[1,]

Aux<-matrix(,nrow=nrow(M2),ncol=3)

for(j in 1:nrow(M2)){
  for( i in 0: 2){
    Aux[j,i+1]<- paste0(M2[j,3+i*4],"|",M2[j,4+i*4],"|",M2[j,5+i*4],"|",M2[j,6+i*4])
  }
}
Aux

Nam<-c("I1","I2","I3")
for(i in 1:ncol(M2)){
  Nam[i]<-substr(colnames(M2[,-c(1:2)])[i],11,nchar(colnames(M2[,-c(1:2)])[i])-3)
}

unique(Nam)
colnames(Aux)<-unique(Nam)[1:47]
Aux<-data.frame("FORMAT"=".",Aux)
Auxx<-as.matrix(Aux)

colnames(vcf@gt)
rownames(vcf@gt)
vcf@gt[,1]

vcf@gt

strwrap(vcf@meta[1:7])
queryMETA(vcf)
getFIX(vcf)

A@meta<-vcf@meta
A@fix
A@gt<-Auxx
dim(A@fix)
dim(A@gt)


class(A)
dim(A@meta)
dim(A@fix)

dim(A@gt)
A@gt
#crear el objeto y pasarlo ahora a vcf.gz 
setwd("/home/useradmin/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy")
#setwd("/home/useradmin/Desktop/start psvb")
write.vcf(A, file = "GenosB.vcf.gz")
getwd()
S1<-M2
colnames(S1)<-NULL
M2$Pos

rownames(S1)
setwd("/home/useradmin/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy")
#setwd("/home/useradmin/Desktop/start psvb")

write.table(S1,"test.gen",sep="\t",row.names = FALSE,col.names = FALSE,quote=1)
