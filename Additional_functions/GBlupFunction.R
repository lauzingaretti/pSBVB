# GBLUP MODEL (you could to use any output G from pSBVB)
#' Funtion \code{GBlup_predict} 
#' @param G Genomic Relationship Matrix (any of standar G output from pSBVB)
#' @param y phenotypes file (from pSBVB output)
#' @param make_predictions index of individuals which you want to made predictions or proportion (split dataset between test/train)
#' @param h2 heredability (if ntraits is higher than one, it should be a vector of length = ntraits)
#' @param ntraits how many traits has been simulated? You can run the model to all traits simultaneosly

#' @return 
#' @param rhoM Predictive Ability 
#' @param uhat Estimated u values. 
#' @param yout_corr Phenotype with corrected by heritability parameter (h2)
#' @param yout Orignal phenotype 
#' @param mean Mean estimated by model. 
#' @author M L Zingaretti 



GBlup_predict <- function(G=G,y=y,h2=c(0.3,0.4,0.5),make_predictions=c(120:203),ntraits=3){
if(ntraits==1 & length(h2)==1){
 A=GBlup_pred(G=G,y=y,h2=h2,miss=make_predictions)  
}
  if(ntraits==1 & length(h2)!=1){
  stop("check heredability")
}
  if(ntraits>1 & length(h2)==ntraits){
 A<-lapply(c(1:ntraits),function(i){GBlup_pred(G=G,y=y[,c(1,i+1,i+1+ntraits,i+1+2*ntraits,i+1+3*ntraits)],h2=h2[i],miss=make_predictions)}) 
}
  if(ntraits>1 & length(h2)!=ntraits){
  stop("check heredability")
}
 return(A) 
}


# id phenotype additive additive + dominant additive + dominant + epista
GBlup_pred<-function(G,y,h2,miss){
  #check input 
    if(!is.matrix(G)){
    G<-as.matrix(G)
  }
   if (!is.data.frame(y)){
     y<-as.data.frame(y)
   }

  yPhen=NewPheno(y[,3],hered_esp2=h2)
  yn=y[,2]
  #u=y[,3]
  n = ncol(G)
  # Incidence matrix, 1 for the mean in last row
  W=cbind(diag(n),rep(1))
  
  # assign missing
  #whichna = contains missing (data to be predicted)
  if(length(miss)==1){
    if((miss>=1 || miss<=0)){
      stop("miss must be a value between 0 and 1, indicating the proportion of values to 
           predict")
    }else{
      whichna = sample(1:length(u),floor(length(u)*miss)) 
    } 
  }
  if(length(miss)>1){
    if(min(miss)<=1){
      stop("miss must be higher than 1, the first element of dataset")
    }
    if(max(miss)>n){
      stop("miss must be less than n (number of data in your matrix)")
    }
    if ((unlist(unique(lapply(miss,is.integer))))!=TRUE){
      stop("The index should be integer")
    }
    else{
      whichna = miss 
    }
  }
  
  W[whichna,]=0
  # G is usually singular, if so
  diag(G) = diag(G)*1.05
  G_i = chol2inv(chol(G))
  # Left Hand side
  LHS=t(W) %*% W
  LHS[1:n,1:n] = LHS[1:n,1:n] + G_i * (1-h2)/h2
  RHS = t(W)%*%yPhen
  # predictive ability, last row is the mean
  uhat = solve(LHS,RHS)[-(n+1)]
  mean = solve(LHS,RHS)[(n+1)]
  #efecto de la media. ahora si está bien 
  #rho<-cor(uhat[whichna],u[whichna])
  rhoM<-cor(uhat[whichna]+mean,yPhen[whichna])
  
  #plot(uhat[whichna]+mean,u[whichna])
  return(list("rhoM"=rhoM,"uhat"=uhat,"yout_corr"=yPhen,"yout"=yn,"mean"=mean))
  
  }
  
  


#######Aux########### 

Heredabilidad<-function(Ad_ef,Phen){
  Hered<-var(Ad_ef)/var(Phen)
  return("Heredabilidad"=Hered)
}

#function to perform an adjustment of heredability
#replace the before phenotype by this one.
NewPheno<-function(feno,hered_esp2=0.5){
  #To perform the adjustment by expectation heredability 
  ve<-(var(feno)*(1-hered_esp2))/hered_esp2
  NewFeno<-feno+rnorm(n=length(feno),sd=sqrt(ve))
  return("Newfeno"=NewFeno)
}

