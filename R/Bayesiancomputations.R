#' Create model file
#' @details 
#' Generates X, Stratum indicator, computes N_j's and model matrix.
#' @param GG an object generated by the function Generate_all
#' @examples
#' cat(model.text(Generate_all(N=1000,Q=2,p=5)))
model.text<-function(GG){
  N<-GG$N
  Q<-GG$Q
  K_q<-GG$K_q
  J=GG$XX$J
  paste("model{
for(i in 1:N){y[i]~dnorm(thetastar[j_i[i]],1/sqrt(sigma_y^2));}",
        paste0("for (j in 1:J){thetastar[j]=alpha0+",paste(
          unlist(plyr::alply(1:Q,1,function(l){
            plyr::alply(combn(Q,l),2,function(q){                    
              paste0("alpha.",paste("X",q,collapse=".",sep=""),"[j]")})})),
          collapse="+"),";}"),
        paste(
          unlist(plyr::alply(1:Q,1,function(l){
            plyr::alply(combn(Q,l),2,function(q){
              paste0("for (j in 1:J){alpha.",paste("X",q,collapse=".",sep=""),"[j]~dnorm(0,1/sqrt((lambda.",paste("X",q,collapse=".",sep=""),"[j]*sigma)^2));}"
              )})})),collapse="\n"),
        paste(
          unlist(plyr::alply(1:Q,1,function(l){
            plyr::alply(combn(Q,l),2,function(q){
              paste0(
                "for (j in 1:J){lambda.",paste("X",q,collapse=".",sep=""),"[j]=delta[",l,"]*",
                paste("gamma0.X",q,"[k_qj[",q,",j]]",collapse="*",sep=""),";}")})})),collapse="\n"),
        paste(
          unlist(plyr::alply(1:Q,1,function(l){
            paste0("for(k in 1:K_q[",l,"]){gamma0.X",l,"[k]~dnorm(0,1)}")})),collapse="\n"),
        "for(l in 1:Q){delta[l]~dnorm(0,1)}",
        "sigma=abs(sigmarel)",
        "sigma_y=abs(sigma_yrel)",
        "sigma_yrel~dt(0,1/sqrt(5),1)",
        "sigmarel~dt(0,1,1)",
        "alpha0~dnorm(0,.1)","}",sep="\n")}

#' Compute posterior of all parameters
#' @details 
#' Generates X, Stratum indicator, computes N_j's and model matrix.
#' @param GG an object generated by the function Generate_all
#' @param initf  a character string, either "arbitrary" or "real values"
#' @examples
#' Trangucci.fit(Generate_all(N=1000,Q=2,p=5))
Trangucci.fit<-function(GG,initf="random"){
  if(GG$Q>9){error("Q must be <10")}
  .data<-plyr::alply(GG$y,2,function(y){c(GG[c("N","Q","K_q")],list(
    j_i=(1:GG$XX$J)[strtoi(substr(GG$XX$Xd$Strata,2,length(GG$XX$Xd$Strata)-1))],
    k_qj=GG$XX$k,
    J=GG$XX$J,
    y=GG$y[,1]))})
  if(initf=="random"){
    init=function(){c(list(
      sigmarel=1,
      sigma_yrel=1,
      delta=rep(.5,GG$Q),
      alpha0=0),
      do.call(c,lapply(1:GG$Q,function(l){
        alpha<-plyr::alply(combn(GG$Q,l),2,function(y){
          rep(1,GG$XX$J)})
        names(alpha)<-plyr::alply(combn(GG$Q,l),2,function(y){
          paste0("alpha.",paste("X",y,collapse=".",sep=""))})
        alpha
      })),
      (function(){x<-lapply(1:GG$Q,function(l){rep(1,GG$K_q[l])})
      names(x)<-paste0("gamma0.X",1:GG$Q)
      x})())}}else{
        init=function(){c(GG$hyper[c("sigma","sigma_y","delta","alpha0")],
                          GG$hyper$gamma0,
                          GG$lambda,
                          list(GG$alpha),
                          do.call(c,lapply(1:GG$Q,function(l){
                            alpha<-plyr::alply(combn(GG$Q,l),2,function(y){
                              array(1,dim=c(GG$XX$J,GG$K_q[y]))})
                            names(alpha)<-plyr::alply(combn(GG$Q,l),2,function(y){
                              paste0("alpha.",paste("X",y,collapse=".",sep=""))})
                            alpha
                          })),
                          (function(){x<-lapply(1:GG$Q,function(l){rep(1,GG$K_q[l])})
                          names(x)<-paste0("gamma0.X",1:GG$Q)
                          x})())}
      }
  model.texte<-model.text(GG)
  parameters.to.save = unique(c("thetastar",
                         names(init()),
                         "sigma_y","sigma",
                         paste0("gamma0.X",1:GG$Q),
                         unlist(plyr::alply(1:GG$Q,1,function(l){plyr::alply(combn(GG$Q,l),2,function(qq){paste0("lambda.",paste("X",qq,collapse=".",sep=""))})})),
                         unlist(plyr::alply(1:GG$Q,1,function(l){plyr::alply(combn(GG$Q,l),2,function(qq){paste0("alpha." ,paste("X",qq,collapse=".",sep=""))})})))
  )
  true.parameters=c(thetastar=GG$thetastar,GG$XX)
  
  model.fit<-plyr::llply(.data,function(.data0){
    jags(model.file=textConnection(model.texte),
         data=.data0 ,
         inits=init,
         n.burnin=1500,
         n.thin = 2,
         n.iter=3000,
         parameters.to.save = parameters.to.save)})
  }

