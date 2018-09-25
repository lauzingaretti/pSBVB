#generate pedigree function 
#id id_father id_mother [sex] (optional)
#sex is 1 to males and 2 to females 
#this function could be created a ped file with any founders, optional sex
#father 
#'Funtion \code{PedGenerator} 
#'This function is use to make a Pedigree file 
#'@param nfounder number of founders individuals for the population  
#'@param ngen number of generations into population 
#'@param nind_bygen vector of ngen lenght indicating the number of individuals into 
#'each generation 
#'@param sex logical. If is TRUE, it must be generated a column indicating individual's sex. Default is FALSE 
#'@param path path to save the pedigree file
#'@param exlude Must some individuals excludes of analysis?  how many of founder must be excluded of analysis, default is 0. 
#'@return
#'  Data.frame with the simulated pedigree. 
#'  \item{file.ped}{Pedigree file }
#'
#'
#'
#'@author M L Zingaretti
#'
#'


pedgenerator<-function(nfounder=10, ngen=5, nind_bygen=c(2,2,3,2,5),sex=FALSE,path=NULL, exclude =0){
if(nfounder <= 1){
  stop("nfounder must be a positive integer > 1")
}
if(as.integer(nfounder)!=nfounder){
  stop("nfounder must be a positive integer > 1")
}
  
if(exclude<0){
  warning("exclude must be a positive value, indicating how many observations are excluded of pedigree")
exclude=0 
  }  
if(ngen<1){
  stop("the number of generations have to be al least 1")
}

if(length(nind_bygen)!=ngen){
 stop("you must be to specify how many individuals have each generation")
}

if(sex==FALSE){
            
init<-data.frame(seq(1,nfounder),rep(0,nfounder),rep(0,nfounder))
colnames(init)<-c("index","father","mother")
m= nrow(init)
deb<-c()
for(i in 1: ngen){
deb<-rbind(deb,data.frame(seq(m+1,m+nind_bygen[i]),
      matrix(unlist(lapply(seq(1:nind_bygen[i]),function(i){sample((exclude+1):m,2)})),ncol=2,byrow=TRUE)))
m=m+nind_bygen[i]
  
}
colnames(deb)<-c("index","father","mother")
pedigree<-rbind(init,deb)
colnames(pedigree)<-c()
}
  if(sex==TRUE){
    
    
    deb<-data.frame(seq(1,nfounder),rep(0,nfounder),rep(0,nfounder),unlist(lapply(c(1:nfounder),function(i){sample(c(1,2),1)})))
    m= nrow(deb)
    colnames(deb)<-c("index","father","mother","sex")
    if(exclude>0){
      a<-c(1:exclude)
     
    for(i in 1: ngen){
      
      Aux=data.frame(seq(m+1,m+nind_bygen[i]),sample(x=unlist(deb[deb[-a,4]==1,1]),size=nind_bygen[i],replace=TRUE),
                   sample(unlist(deb[deb[-a,4]==2,1]),nind_bygen[i],replace=TRUE),
                   unlist(lapply(c(1:nind_bygen[i]),function(i){sample(c(1,2),1)})))
      colnames(Aux)<-c("index","father","mother","sex")
      deb<-rbind(deb,Aux)

      m=m+nind_bygen[i]
      
    }
    } else{
      for(i in 1: ngen){
        
        Aux=data.frame(seq(m+1,m+nind_bygen[i]),sample(x=unlist(deb[deb[,4]==1,1]),size=nind_bygen[i],replace=TRUE),
                       sample(unlist(deb[deb[,4]==2,1]),nind_bygen[i],replace=TRUE),
                       unlist(lapply(c(1:nind_bygen[i]),function(i){sample(c(1,2),1)})))
        colnames(Aux)<-c("index","father","mother","sex")
        deb<-rbind(deb,Aux)
        
        m=m+nind_bygen[i]
        
      }     
      }
    pedigree<-deb
    colnames(pedigree)<-c()
  }  
A<-list("pedigree"=pedigree,"founder"=nfounder,"generations"=ngen)

if(!is.null(path)){
write.table(pedigree,paste0(path,"/File_st.ped"),col.names = FALSE,row.names = FALSE,
           quote=FALSE)
}else{
write.table(pedigree,"File_st.ped",sep="\t",col.names = FALSE, row.names=FALSE, 
            quote=FALSE)
  
}
  return(A)          

}
#
# pedgenerator(100,4,c(rep(100,3),150),sex=FALSE,path="~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions"
# ,exclude=47)
