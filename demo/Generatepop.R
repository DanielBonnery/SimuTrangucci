set.seed(11)
GG<-Generate_all(N=1000,Q=2,p=5)
Strata1<-data.frame(GG$XX$Strata,
                    N_j=GG$XX$N_j,
                    thetastar=GG$thetastar)
set.seed(7)
GG<-Generate_all(N=1000,Q=2,p=5)
Strata2<-data.frame(GG$XX$Strata,
                    N_j=GG$XX$N_j,
                    thetastar=GG$thetastar)
colnames(Strata)<-c("$X_1$","$X_2$","Stratum (j)","$N_j$","$\\theta^\\star$")
