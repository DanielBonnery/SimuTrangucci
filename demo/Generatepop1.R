#Generate the first population - Trangucci model N = 1000,Q = 2, p = 5
set.seed(1)
GG1<-Generate_all(N=1000,Q=2,p=5)
Strata1<-data.frame(GG1$XX$Strata,
                    N_j=GG1$XX$N_j,
                    thetastar=GG1$thetastar)
colnames(Strata1)<-c("$X_1$","$X_2$","Stratum (j)","$N_j$","$\\theta^\\star$")
