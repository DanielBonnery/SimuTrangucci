library(SimuTrangucci)
library(R2jags)
library(ggplot2)
library(plyr)
x=TRUE
seed=1
while(x){
set.seed(seed)

GG<-Generate_all(N=10000,Q=4,p=5,nrep=2)
GG$XX$K_q



gibbs.samples<-Trangucci.fit(GG)
x=inherits(x,"try-error")
seed=seed+1}
GG$XX$K_q

X=data.frame(j=1:GG$XX$J,thetastar=GG$thetastar,t(gibbs.samples[[1]]$BUGSoutput$sims.list$thetastar[sample(nrow(gibbs.samples[[1]]$BUGSoutput$sims.list$thetastar),100),]))
names(X[3:ncol(X)])<-paste0("rep",1:(ncol(X)-2))
XX<-reshape2::melt(X,id.vars=c("j","thetastar"),value.name="sample")
graph1<-ggplot(XX,aes(x=thetastar,y=sample))+geom_point()+geom_abline(slope=1,intercept=0)+
  geom_point(aes(x=thetastar,y=thetastar,colour="red"))


plot(GG$y[,1],merge(GG$XX$Xd["Strata"],data.frame(GG$XX$Strata,
                                        thetastar=GG$thetastar),by="Strata")$thetastar)
zz<-merge(data.frame(Strata=GG$XX$Xd["Strata"],Y=GG$y[,1]),data.frame(Strata=GG$XX$Strata[,"Strata"],
                                        thetastar=GG$thetastar),by="Strata")
plot(zz$Y,zz$thetastar)


XXX<-plyr::ddply(cbind(GG$XX$Xd,y=GG$y[,1]), ~Strata, function(d){data.frame(mean=mean(d$y))})
YYY<-data.frame(thetastar=GG$thetastar,posteriormean=as.vector(gibbs.samples[[2]]$BUGSoutput$mean$thetastar),GG$XX$Strata)
ZZZ<-merge(XXX,YYY,by="Strata")
ZZZ<-ZZZ[order(ZZZ$thetastar),]
ZZZ$j<-1:nrow(ZZZ)
ZZZ$improvement<--abs(ZZZ$posteriormean-ZZZ$thetastar)+abs(ZZZ$mean-ZZZ$thetastar)
  graph5<-ggplot(ZZZ,aes(x=thetastar,     y=mean))+geom_segment(aes(x = thetastar, y =mean , xend = thetastar, yend = posteriormean,colour=improvement),linejoin="mitre",size=1)+geom_abline(slope=1,intercept=0) +
    scale_colour_gradient2(low = "red", mid = "white",
                         high = "green")+geom_point()+geom_point(aes(y=posteriormean),shape=17)

r<-ggplot_build(graph5)$layout$panel_params[[1]]$x.range
s<-ggplot_build(graph5)$layout$panel_params[[1]]$y.range
t<-c(min(r[1],s[1]),max(r[2],s[2]))
graph5<-graph5+coord_equal(xlim=t,ylim=t)+ylab("mean (disc), posterior mean (triangle)")
graph4<-ggplot(ZZZ,aes(x="\\theta^\star",     y=mean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)
graph2<-ggplot(ZZZ,aes(x=thetastar,y=posteriormean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)+geom_smooth()
graph3<-ggplot(ZZZ,aes(x=mean,     y=posteriormean))+geom_point()+geom_abline(slope=1,intercept=0)+coord_equal(xlim=t,ylim=t)
graph6<-ggplot(reshape2::melt(ZZZ[c("j","thetastar","mean","posteriormean")],id.vars=c("j")),aes(x=j,y=value,colour=variable))+geom_line()
graph7<-ggplot(reshape2::melt(ZZZ[c("j","thetastar","posteriormean")],id.vars=c("j")),aes(x=j,y=value,colour=variable))+geom_line()

graph8<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$sigma),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$sigma)+
  geom_vline(xintercept=GG$hyper$sigma,colour="red")

graph9<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$sigma_y),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$sigma_y)+
  geom_vline(xintercept=GG$hyper$sigma_y,colour="red")

graph10<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$lambda.X3[,1]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$lambda.X3[1])+
  geom_vline(xintercept=GG$lambda$`3`[1],colour="red")

graph11<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$lambda.X2.X4[,1]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$lambda.X2.X4[1])+
  geom_vline(xintercept=GG$lambda$`3`[1],colour="red")

graph12<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$delta[,2]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$delta[2])+
  geom_vline(xintercept=GG$hyper$delta[2],colour="red")

graph13<-ggplot(data.frame(y=1:2250,x=gibbs.samples[[2]]$BUGSoutput$sims.list$delta[,3]),aes(x))+geom_density()+
  geom_vline(xintercept=gibbs.samples[[2]]$BUGSoutput$mean$delta[3])+
  geom_vline(xintercept=GG$hyper$delta[3],colour="red")

graph14<-ggplot(data.frame(y=gibbs.samples[[2]]$BUGSoutput$mean$delta,x=GG$hyper$delta),aes(x,y))+geom_point()+geom_abline(intercept=0,slope=1)
X<-data.frame(y=gibbs.samples[[2]]$BUGSoutput$mean$lambda0.X1,x=GG$hyper$lambda1[[1]])
graph15<-ggplot(X,aes(x,y))+geom_point()+geom_abline(intercept=0,slope=1)
