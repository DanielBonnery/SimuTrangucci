#Generate the first population - Trangucci model N = 10000,Q = 2, p = 5
  set.seed(1)
  N=10000; Q=2; p=5; K_q=sample(2:p,Q,replace=T)
  XX=Gen_design_variables(N=N,Q=Q,p=p,K_q = K_q)
  hyper=Gen_hyper_parameters(XX,model="model1")
  GG<-Generate_all(N=N,Q=Q,p=p,nrep=2,K_q = K_q,XX = XX,hyper = hyper,model = "model1")
  gibbs.samples<-Trangucci.fit(GG)
  save(GG,gibbs.samples,file=wheretosave)}else{load(wheretosave)}
  folderwheretosave=file.path(Mydirectories::googledrive.directory(),"Travail/Recherche/Travaux/SimuTrangucci/data")
  if(!file.exists(folderwheretosave)){
    
  
  set.seed(1)
GG1<-Generate_all(N=1000,Q=2,p=5)
Strata1<-data.frame(GG$XX$Strata,
                    N_j=GG$XX$N_j,
                    thetastar=GG$thetastar)
set.seed(7)
GG<-Generate_all(N=1000,Q=2,p=5)
Strata2<-data.frame(GG$XX$Strata,
                    N_j=GG$XX$N_j,
                    thetastar=GG$thetastar)
colnames(Strata)<-c("$X_1$","$X_2$","Stratum (j)","$N_j$","$\\theta^\\star$")
