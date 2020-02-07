#' Generate design variables
#' 
#' @details 
#' Generates X, Stratum indicator, computes N_j's and model matrix.
#' @param N population size
#' @param Q Number of design variables
#' @param p maximum number of levels for design variables, >=2. 
#' @return a list of 10 elements. the element named Xd contains one 
#' line per individual and gives the value of the stratification 
#' variables as well as the stratum number for each individual.
#' The element named vars gives the names of the stratification variables
#' The element named Q gives the number of stratification variables
#' The element named K_q gives for each stratification variable, the number of 
#' possible values
#' The element N_j gives the count per stratum
#' The element named Strata is a matrix with one line per stratum and 
#' contains the stratum identifier (column "Strata") as  well as the 
#' corresponding values of the stratification variables.
#' @examples
#' N=1000;Q=2;p=5
#' XX<-Gen_design_variables(N,Q,p)
#' 
Gen_design_variables<-function(N,Q,p,K_q=sample(2:p,Q,replace=T)){
  vars<-paste0("X",1:Q)#names of the variables : X1 ...XQ
  X<-aperm(plyr::aaply(K_q,1,function(x){sample(x,N,replace=T)}),2:1)#Creation of the matrix of X1...XQ
  X<-X[do.call(order,data.frame(X,stringsAsFactors = FALSE)),]#order X by X1...XQ
  dimnames(X)<-list(k=1:N,Variable=vars)#rename dimensions of X
  Strata<-unique.array(X,margin=1)#Look at unique combinaisons of X1...XQ on the population
  J<-dim(Strata)[1]#Definition of J: number of strata
  Strata=cbind(Strata,Strata=paste0("S",1:J))#Defintion of the Strata variable
  rownames(Strata)<-Strata[,"Strata"]
  merge(data.frame(X,stringsAsFactors = FALSE),data.frame(Strata,stringsAsFactors = FALSE), by=colnames(X))->Xd#Creates Xd: rows:1 to N, columns X1 to XQ and Stratum
  Xd[1:Q]<-plyr::llply(Xd[1:Q],as.factor)#Convert columns of Xd to factor
  K_q2<-plyr::laply(Xd[1:Q],nlevels)  #Computes the effective number of levels for each stratum
  names(K_q2)<-vars                   #Rename elements of K_q2
  names(Xd$Strata)<-NULL              #Remove names from Xd$Strata
  list(Xd=Xd,
       vars=vars,
       Q=Q,
       K_q=K_q,
       K_q2=K_q2,
       N_j=plyr::daply(Xd,~Strata,nrow)[Strata[,"Strata"]],
       Strata=Strata,
       J=J,
       X.model.matrix=do.call(
         cbind,plyr::alply(
           c(vars,"Strata"),1,
           function(x){model.matrix(as.formula(paste0("~0+",x)),Xd)})),
       k=plyr::maply(expand.grid(q=1:Q,j=1:J),function(q,j){Strata[j,q]}))
}
#' Generate gamma0s
#' @param XX output from \code{Gen_design_variables}
#' @examples
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0f(XX)
gamma0f<-function(XX){
  y<-plyr::alply(XX$vars,1,function(q){
    x<-abs(rnorm(XX$K_q2[q]))
    names(x)<-1:XX$K_q2[q]
    x})
  names(y)<-paste0("gamma0.",XX$vars)
  y}

#' Create the lambda_{j}^{q1...qell} 
#' lambda_{k1...kell}^{q1...qell}  can be deduced from the lambda_{S_i}^{q1...qell}
#' If for example, Q=3, ell=2, q_1=1, q_2=2, k_1=k_2=1, then there will be many strata S_i
#' that correspond to X_{q_1}=1, X_{q_2}=1, and all of them will have the same value for 
#' lambda_{S_i}^{q1=1...q2=2}
#' @param XX an output from [Gen_design_variables] 
#' @param gamma0 an object with same structure than [gamma0f(XX)]
#' @param delta an object with same structure than [Gen_hyper_parameters(XX)$delta]
#' @examples
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0=gamma0f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda.j.q1...qell.f(XX,gamma0,delta)

lambda.j.q1...qell.f<-function(XX,gamma0,delta){
  #print(XX$Q)
  plyr::alply(1:XX$Q,1,function(ell){
    comb<-combn(XX$Q,ell)
    dimnames(comb)<-list(q_l=paste0("q_",1:ell),q1...qell=plyr::aaply(comb,2,paste,collapse="."))
    #concernedk1...kell<-unique(XX$Strata[,q1...qell])
    plyr::aaply(comb,2,function(q1...qell){
           lambdas=plyr::aaply(XX$Strata[,q1...qell,drop=FALSE],1,
                               .fun=function(x){
                                 prod(plyr::aaply(1:ell,1,function(i){as.vector(gamma0[[q1...qell[i]]][x[i]])}))*delta[ell]})
         },.drop=FALSE)
  })}

#' Create the lambda_{k1...kell}
#' @param XX an output from [Gen_design_variables] 
#' @param gamma0 an object with same structure than [gamma0f(XX)]
#' @param delta an object with same structure than [Gen_hyper_parameters(XX)$delta]
#' @examples
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0=gamma0f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda.j.q1...qell.f(XX,gamma0,delta)
#' #' lambdaf(XX,gamma0,delta)

lambdaf<-function(XX,gamma0,delta){
  do.call(c,lapply(1:XX$Q,function(ell){
    comb<-combn(XX$Q,ell)
    dimnames(comb)<-list(q_l=paste0("q_",1:ell),q1...qell=plyr::aaply(comb,2,paste,collapse="."))
    #concernedk1...kell<-unique(XX$Strata[,q1...qell])
    
    lambda.q1...qell=plyr::alply(comb,2,function(q1...qell){
      lambdas=plyr::aaply(unique(XX$Strata[,q1...qell]),1,
                          .fun=function(x){
                            prod(plyr::aaply(1:ell,1,function(i){as.vector(gamma0[[q1...qell[i]]][x[i]])}))*delta[ell]})
      lambdas<-array(lambdas,dim=XX$K_q[q1...qell])
      dimnames(lambdas)<-lapply(dim(lambdas),function(x){1:x})
      names(dimnames(lambdas))<-paste0("k",1:length(dim(lambdas)))
      lambdas
    })
    names(lambda.q1...qell)<-plyr::alply(comb,2,function(q1...qell){paste0("lambda.",paste("X",q1...qell,collapse=".",sep=""))})
    lambda.q1...qell
  }))}



#' Create the alpha_{Si}^{q1...qell} 
#'
#' @param lamda an object with same structure than [gamma0f(XX)]
#' @param delta an object with same structure than
#' @examples
#' set.seed(1)
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0=gamma0f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda=lambda.j.q1...qell.f(XX,gamma0,delta)
#' sigma=Gen_hyper_parameters(XX)$sigma
#' alphaSif(lambda,sigma)

alphaSif<-function(lambda,sigma){
  plyr::llply(lambda,function(x){
    dime<-if(!is.null(dim(x))){1:length(dim(x))}else{1}
    plyr::aaply(x,dime,function(xx){rnorm(1,sd=sigma*xx)},.drop=FALSE)})}

#' Create the thetastar_{Si}^{q1...qell} 
#'
#' @param lamda an object with same structure than [gamma0f(XX)]
#' @param delta an object with same structure than
#' @examples
#' set.seed(1)
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0=gamma0f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda=lambda.j.q1...qell.f(XX,gamma0,delta)
#' sigma=Gen_hyper_parameters(XX)$sigma
#' alpha=alphaSif(lambda,sigma)
#' alpha0=1
#' thetastarf(alpha,alpha0)

thetastarf<-function(alpha,alpha0){
  alphas<-drop(do.call(abind::abind,c(alpha,list(along=1))))
  dimnames(alphas)[[1]]<-1:(dim(alphas)[1])
  alpha0+plyr::aaply(alphas,2,sum)}


#' Generates y 
#'
#' @param lamda an object with same structure than [gamma0f(XX)]
#' @param delta an object with same structure than
#' @examples
#' set.seed(1)
#' XX<-Gen_design_variables(N=1000,Q=3,p=4)
#' gamma0=gamma0f(XX)
#' delta=Gen_hyper_parameters(XX)$delta
#' lambda=lambda.j.q1...qell.f(XX,gamma0,delta)
#' sigma=Gen_hyper_parameters(XX)$sigma
#' alpha=alphaSif(lambda,sigma)
#' alpha0=1
#' thetastar=thetastarf(alpha,alpha0)
#' sigma_y=Gen_hyper_parameters(XX)$sigma_y
#' yf(XX,thetastar,sigma_y,3)

yf<-function(XX,thetastar,sigma_y,nrep){
  Strata<-data.frame(Strata=XX$Strata[,"Strata"],
                     N_j=XX$N_j,
                     thetastar=thetastar,
                     stringsAsFactors = FALSE)
  do.call(rbind,plyr::alply(1:nrow(Strata),1,function(x){matrix(rnorm(Strata$N_j[x]*nrep,
                                                                      mean=Strata$thetastar[x],sd=sigma_y),Strata$N_j[x],nrep)}))
  #plot(merge(XX$Xd,Strata,by="Strata")$thetastar,y[,1])
  
  
  do.call(rbind,plyr::alply(1:nrow(Strata),1,function(x){matrix(rep(Strata$Strata[x],Strata$N_j[x]),Strata$N_j[x],1)}))
  }



#' Create model hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=2;p=3
#' XX<-Gen_design_variables(N,Q,p)
#' Gen_hyper_parameters(XX)

Gen_hyper_parameters<-function(XX,model="model1"){
  (get(paste0("Gen_hyper_parameters_",model)))(XX)}




#' Create model 1 hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=2;p=3
#' XX<-Gen_design_variables(N,Q,p)
#' Gen_hyper_parameters_model1(XX)

Gen_hyper_parameters_model1<-function(XX){
  #hyper parametres
  sigma_y<-abs(5*rt(1,1))  
  delta<-abs(rnorm(XX$Q))
  gamma0<-gamma0f(XX)
  sigma<-abs(rt(1,1))  
  alpha0<-rnorm(1,sd=sqrt(10))
  list(sigma_y=sigma_y,
       delta=delta,
       gamma0=gamma0,
       sigma=sigma,
       alpha0=alpha0)
}



#' Create model 2 hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=2;p=3
#' XX<-Gen_design_variables(N,Q,p)
#' Gen_hyper_parameters_model2(XX)

Gen_hyper_parameters_model2<-function(XX){
  #hyper parametres
  sigma_y<-abs(5*rt(1,1))  
  delta<-abs(rnorm(XX$Q))
  gamma0<-gamma0f(XX)
  sigma<-abs(rt(1,1))  
  alpha0<-rnorm(1,sd=sqrt(10))
  list(sigma_y=sigma_y,
       delta=delta,
       gamma0=gamma0,
       sigma=sigma,
       alpha0=alpha0)
}



#' Create model hyper-parameters and parameters procedure
#'
#' @param XX an output from [Gen_design_variables] 
#' @examples
#' N=1000;Q=3;p=3
#' GG<-Generate_all(N,Q,p)

Generate_all<-function(N=NULL,
                       Q=NULL,
                       p=NULL,
                       K_q=sample(2:p,Q,replace=T),
                       XX=Gen_design_variables(N,Q,p,K_q),
                       model="model1",
                       hyper=Gen_hyper_parameters(XX,model),
                       nrep=3){
  #depending parameters  
  lambda<-lambda.j.q1...qell.f(XX,hyper$gamma0,hyper$delta)
  alpha<-alphaSif(lambda,hyper$sigma)
  thetastar<-thetastarf(alpha,hyper$alpha0)
  y<-yf(XX,thetastar,hyper$sigma_y,nrep)
  list(N=N,Q=Q,p=p,K_q=K_q,hyper=hyper,XX=XX,nrep=nrep,lambda=lambda,alpha=alpha,thetastar=thetastar,y=y)}
