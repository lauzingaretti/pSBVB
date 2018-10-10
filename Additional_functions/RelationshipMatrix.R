#'Function to generate Relationship based matrix 

#'Funtion \code{RelMatrix} 
#' Creates a additive relationship matrix A from a pedigree data (ped file)

#' @param data pedigree data with 3 column. Unknown are equal to zero. See pedigree function to more info. 
#' @param dominance if true, returns the dominance relationship matrix
#' @param path path to save the file. Default is NULL

#' @return Matrix with the Numerator Relationship Matrix. 



RelMatrix <- function(data = NULL, dominance=FALSE,path=NULL){
  if( is.null(data))
    stop("The data argument can't be empty") 
  
  s <- data[,2]
  d <- data[,3]
  
  if( is.null(s) || is.null(d) )
    stop(deparse("Please define a validate pedigree file"))
  
  if( !is.numeric(s) || !is.numeric(d) )
    stop(deparse("Pleasy verify your dataset because the pedigree must be numeric"))
  
  n <- length(s)
  A <- matrix(NA,ncol=n,nrow=n)
  diag(A)<-1




    for(j in 1:(n-1)){
        for (i in (j+1):(n)){
        #father and mother unknown 
        if(s[i] == 0 && d[i] == 0 ){  
        A[j,i]<-A[i,j]<-0
        } ## father is unknown
        if(s[i] == 0 && d[i] != 0 ){
        A[j,i]<-A[i,j] <- 0.5*(A[j,d[i]])
        } ##mother is unknown 
        if(d[i] == 0 && s[i] != 0 ){
          A[j,i]<-A[i,j] <- 0.5*(A[j,s[i]])
        }# both are knowing
        if (d[i] != 0 && s[i] != 0){
          A[i,i] <- 1+0.5*(A[d[i],s[i]])
          A[j,i]<-A[i,j] <- 0.5*(A[j,s[i]]+A[j,d[i]])
        }
      }
      }
 
 
  if(dominance){
    DR <- matrix(NA,ncol=n,nrow=n)
    diag(DR)<-1
    for(j in 1:(n-1)){
      for (i in (j+1):(n)){
        d1 <- ifelse(length(A[s[i],s[j]])>0,A[s[i],s[j]],0)
        d2 <- ifelse(length(A[d[i],d[j]])>0,A[d[i],d[j]],0)
        d3 <- ifelse(length(A[s[i],d[j]])>0,A[s[i],d[j]],0)
        d4 <- ifelse(length(A[s[j],d[i]])>0,A[s[j],d[i]],0)
        DR[i,j] <- 0.25*(d1*d2+d3*d4)
      }
    }
    A<-DR
    DR<-c()
    }
  
  

  
  rownames(A) <- colnames(A) <- data[,1]
  
  if(!is.null(path)){
  write.table(A,paste0(path,"/Relationship.mat"),col.names = FALSE,row.names = FALSE,
              quote=FALSE)
  }else{
    write.table(A,"Relationship.mat",sep="\t",col.names = FALSE, row.names=FALSE, 
                quote=FALSE)
    
  }
  return(A)   
}

