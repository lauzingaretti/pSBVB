
# function to generate "like- vcf file" from genotypes. Now you only need to execute the program using python based scripo
# This function is not necessary. See software help from https://lauzingaretti.github.io/pSBVB/

#' Funtion \code{GenotoVcf} 
#' @param G matrix with genotypes, ind in rows, markers in columns. Note that colnames should be SNPs names. File should be include cols and rowsnames
#' @param p ploidy level
#' @param map  File with coordinate locations map, snp_identification, chrom and position
#' @param path path to save the file
#' @return 
#' @param Output_File VCF file  
#' @author M L Zingaretti 



GenotoVcf <- function(G,p=2,map,path="NULL"){
  if(is.null(G)){
    stop("you have to give an genotype imput matrix")
  }
  M<-unique(unlist(lapply(c(1:nrow(G)),function(i){any(is.na(G[,i]))})))
  if(any(M=="TRUE")){
    stop("NA values are not allowed")
  }
  if(!is.data.frame(G)){
    stop("G should be a data frame object")
  }
  if (is.null(colnames(G))){
    stop("your dataset should be include colnames")
  }
  if (is.null(rownames(G))){
    stop("your dataset should be include rownames")
  }
  if(is.null(map)){
    stop("Map file should be included in order to get chrom and position")
  }
 
  if(!require("data.table")) install.packages("data.table")
  
  if(length(which(map[,1] %in% colnames(G)))==0){
    stop("Any Data colnames should be match with map snps")
  }
  if(p!=round(p)){
  stop("ploidy should be an integer")
  }
  #if(!require("vcfR")) install.packages("vcfR")
  if(max(G) > p){
    stop("Check your dataset. Ploidy level should'nt be less than max(G)")
  }
  

  VCF<- t(apply(G,2,function(x){Auxily(x,p)}))
  rownames(VCF)<-colnames(G)
  colnames(VCF)<-unlist(lapply(c(1:length(rownames(G))),function(i){paste0(rownames(G)[i],"_",seq(1:4))}))
  datos<-data.frame(map[map[,1]%in%rownames(VCF),],VCF[rownames(VCF)%in%map[,1],])
  file<-datos[,-1]
  dim(file)
  colnames(file)<-NULL
  rownames(file)<-NULL
  file[order(file[,1],file[,2]),]
  
  if(!is.null(path)){
    write.table(file,paste0(path,"Generator.gen"),col.names = FALSE,row.names = FALSE,
                quote=FALSE)
  }else{
    write.table(file,"Generator.gen",sep="\t",col.names = FALSE, row.names=FALSE, 
                quote=FALSE)
  }
  return(list(file=file,data=datos))
  
}

###Aux function####
Auxily<-function(A,p){
M<-c()
for(i in 1:length(A)){
  x=c(rep(0,p))
 if(A[i]!=0){ 
   x[sample(c(1:p),A[i],replace=FALSE)]=1
 M<-c(M,x)
   }else{
 M<-c(M,x) 
   }
  }
return(M)  
}  

    
