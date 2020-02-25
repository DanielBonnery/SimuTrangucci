#Generate the first population - Trangucci model N = 1000,Q = 2, p = 5
set.seed(2)
GG2<-Generate_all(N=1000,Q=2,p=5)
Strata2<-data.frame(GG2$XX$Strata,
                    N_j=GG2$XX$N_j,
                    thetastar=GG2$thetastar)
colnames(Strata2)<-c("$X_1$","$X_2$","Stratum (j)","$N_j$","$\\theta^\\star$")
