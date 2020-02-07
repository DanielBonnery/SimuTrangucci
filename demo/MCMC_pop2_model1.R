library(SimuTrangucci)
library(R2jags)
library(ggplot2)
library(plyr)
GG<-Generate_all(N=1000,Q=2,p=5)
x<-model.text(GG);x
GG<-Generate_all(N=1000,Q=5,p=5)
x<-model.text(GG);x
@
  \begin{verbatim}
\Sexpr{x}
\end{verbatim}


What it does:
  
  <<eval=FALSE>>=
  set.seed(2)
GG<-Generate_all(N=1000,Q=4,p=5)
GG$XX$K_q
gibbs.samples<-Trangucci.fit(GG)
@
  <<echo=FALSE>>=
  wheretosave=file.path(Mydirectories::googledrive.directory(),"Travail/Recherche/Travaux/SimuTrangucci/Simu_/datanotpushed/simu2.rda")
if(!file.exists(wheretosave)){
  set.seed(2)
  hyper=Gen_hyper_parameters(XX<-Gen_design_variables(N=10000,Q=4,p=5))
  hyper$sigma_y<-0.1
  GG<-Generate_all(N=10000,Q=4,p=5,nrep=2,hyper = hyper)
  GG$XX$K_q
  gibbs.samples<-Trangucci.fit(GG)
  save(GG,gibbs.samples,file=wheretosave)}else{load(wheretosave)}

GG$XX$K_q
@
  <<>>=
  X=data.frame(j=1:GG$XX$J,thetastar=GG$thetastar,t(gibbs.samples[[1]]$BUGSoutput$sims.list$thetastar[sample(nrow(gibbs.samples[[1]]$BUGSoutput$sims.list$thetastar),100),]))
names(X[3:ncol(X)])<-paste0("rep",1:(ncol(X)-2))
XX<-reshape2::melt(X,id.vars=c("j","thetastar"),value.name="sample")
graph1<-ggplot(XX,aes(x=thetastar,y=sample))+geom_point()+geom_abline(slope=1,intercept=0)+
  geom_point(aes(x=thetastar,y=thetastar,colour="red"))
XXX<-plyr::ddply(cbind(GG$XX$Xd,y=GG$y[,1]), ~Strata, function(d){data.frame(mean=mean(d$y))})
YYY<-data.frame(thetastar=GG$thetastar,posteriormean=as.vector(gibbs.samples[[2]]$BUGSoutput$mean$thetastar),GG$XX$Strata)
ZZZ<-merge(XXX,YYY,by="Strata")
ZZZ<-ZZZ[order(ZZZ$thetastar),]
ZZZ$j<-1:nrow(ZZZ)
ZZZ$diffe<-ZZZ$posteriormean-ZZZ$mean
graph5<-ggplot(ZZZ,aes(x=thetastar,     y=mean))+geom_point()+geom_segment(aes(x = thetastar, y =mean , xend = thetastar, yend = posteriormean,colour=diffe),linejoin="mitre",size=1)+geom_abline(slope=1,intercept=0) +
  scale_colour_gradientn(colours = terrain.colors(10))

r<-ggplot_build(graph5)$layout$panel_params[[1]]$x.range
s<-ggplot_build(graph5)$layout$panel_params[[1]]$y.range
t<-c(min(r[1],s[1]),max(r[2],s[2]))
graph5<-graph5+coord_equal(xlim=t,ylim=t)
graph4<-ggplot(ZZZ,aes(x=thetastar,     y=mean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)
graph2<-ggplot(ZZZ,aes(x=thetastar,y=posteriormean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)+geom_smooth()
graph3<-ggplot(ZZZ,aes(x=mean,     y=posteriormean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)
graph6<-ggplot(reshape2::melt(ZZZ[c("j","thetastar","mean","posteriormean")],id.vars=c("j")),aes(x=j,y=value,colour=variable))+geom_line()
graph7<-ggplot(reshape2::melt(ZZZ[c("j","thetastar","posteriormean")],id.vars=c("j")),aes(x=j,y=value,colour=variable))+geom_line()

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$sigma),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$sigma)+
  geom_vline(xintercept=GG$hyper$sigma,colour="red")

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$sigma_y),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$sigma_y)+
  geom_vline(xintercept=GG$hyper$sigma_y,colour="red")

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$lambda.X3[,1]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$lambda.X3[1])+
  geom_vline(xintercept=GG$lambda$`3`[1],colour="red")

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$lambda.X2.X4[,1]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$lambda.X2.X4[1])+
  geom_vline(xintercept=GG$lambda$`3`[1],colour="red")

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$delta[,2]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$delta[2])+
  geom_vline(xintercept=GG$hyper$delta[2],colour="red")

ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$delta[,3]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$delta[3])+
  geom_vline(xintercept=GG$hyper$delta[3],colour="red")

ggplot(data.frame(y=gibbs.samples[[2]]$BUGSoutput$mean$delta,x=GG$hyper$delta),aes(x,y))+geom_point()+geom_abline(intercept=0,slope=1)
X<-data.frame(y=gibbs.samples[[2]]$BUGSoutput$mean$lambda0.X1,x=GG$hyper$lambda1[[1]])
ggplot(X,aes(x,y))+geom_point()+geom_abline(intercept=0,slope=1)

@
  
  
  
  
  \Sexpr{graphtikzcode("print(graph2)")}

\Sexpr{graphtikzcode("print(graph3)")}

\Sexpr{graphtikzcode("print(graph4)")}

\Sexpr{graphtikzcode("print(graph5)")}