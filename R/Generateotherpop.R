
#' Create the thetastar_{Si}^{q1...qell} 
#'
#' @param lamda an object with same structure than [lambda1f(XX)]
#' @param delta an object with same structure than
#' @examples
#' set.seed(1)
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' lambda1=lambda1f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda=lambdaSif(XX,lambda1,delta)
#' sigma=Gen_hyper_parameters(XX)$sigma
#' alpha=alphaSif(lambda,sigma)
#' alpha0=1
#' thetastarf(alpha,alpha0)

thetaotherstarf<-function(alpha,alpha0){
  alphas<-drop(do.call(abind::abind,c(alpha,list(along=1))))
  dimnames(alphas)[[1]]<-1:(dim(alphas)[1])
  alpha0+plyr::aaply(alphas,2,sum)}

#' Generates y 
#'
#' @param lamda an object with same structure than [lambda1f(XX)]
#' @param delta an object with same structure than
#' @examples
#' set.seed(1)
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' lambda1=lambda1f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda=lambdaSif(XX,lambda1,delta)
#' sigma=Gen_hyper_parameters(XX)$sigma
#' alpha=alphaSif(lambda,sigma)
#' alpha0=1
#' thetastar=thetastarf(alpha,alpha0)
#' sigma_y=Gen_hyper_parameters(XX)$sigma_y
#' yf(XX,thetastar,sigma_y,3)

yotherf<-function(XX,thetastar,sigma_y,nrep){
  Strata<-data.frame(XX$Strata,
                     N_j=XX$N_j,
                     thetastar=thetastar)
  do.call(rbind,plyr::alply(1:nrow(Strata),1,function(x){matrix(rnorm(Strata$N_j[x]*nrep,mean=Strata$thetastar[x],sd=sigma_y),Strata$N_j[x],nrep)}))
}

#' Create model hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=2;p=3
#' XX<-Gen_design_variables(N,Q,p)
#' Gen_hyper_parameters(XX)

Gen_hyper_otherparameters<-function(XX){
  #hyper parametres
  sigma_y<-abs(5*rt(1,1))  
  delta<-abs(rnorm(XX$Q))
  lambda1<-lambda1f(XX)
  sigma<-abs(rt(1,1))  
  alpha0<-rnorm(1,sd=sqrt(10))
  list(sigma_y=sigma_y,
       delta=delta,
       lambda1=lambda1,
       sigma=sigma,
       alpha0=alpha0)
}

#' Create model hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=3;p=3
#' GG<-Generate_all(N,Q,p)

Generateother_all<-function(N=NULL,
                       Q=NULL,
                       p=NULL,
                       K_q=sample(2:p,Q,replace=T),
                       XX=Gen_design_variables(N,Q,p,K_q),
                       hyper=Gen_hyper_parameters(XX),
                       nrep=3){
  #depending parameters  
  lambda<-lambdaSif(XX,hyper$lambda1,hyper$delta)
  alpha<-alphaSif(lambda,hyper$sigma)
  thetastar<-thetastarf(alpha,hyper$alpha0)
  y<-yf(XX,thetastar,hyper$sigma_y,nrep)
  list(N=N,Q=Q,p=p,K_q=K_q,hyper=hyper,XX=XX,nrep=nrep,lambda=lambda,alpha=alpha,thetastar=thetastar,y=y)}


thetastar.otherpopmean._f<-function(GG){
  
  XX<-cbind(GG$XX$Xd,Y<-GG$y[,1])
  XX<-merge(XX,data.frame(thetastar=GG$thetastar,Strata=names(GG$thetastar)),by="Strata")
  plyr::ddply(XX,~Strata,function(d){
    data.frame(
      thetastar=unique(d$thetastar),
      Pop.Mean=mean(d$Y))})
}

plotthetastarvsydisp<-function(GG){
  
  hyper$sigma_y<-0
  GG<-Generate_all(N=10000,Q=3,p=5,nrep=2,hyper = hyper)
  GG$XX$K_q
  
  library(forcats)
  XX<-cbind(GG$XX$Xd,Y=GG$y[,1])
  XX<-merge(XX,data.frame(thetastar=GG$thetastar,N_j=GG$XX$N_j,Strata=names(GG$thetastar)),by="Strata")
  XX$Strata2<-forcats::fct_reorder(.f = XX$Strata,.x = XX$thetastar,.fun=median)
  lm(XX$Y~XX$thetastar)
  ggplot(XX,aes(x=Strata2,y=Y))+geom_boxplot()+geom_point(aes(x=Strata2,y=thetastar),color="red")
  
  plyr::ddply(XX,~Strata,function(d){
    data.frame(
      thetastar=unique(d$thetastar),
      Pop.Mean=mean(d$Y))})
  
  
}

