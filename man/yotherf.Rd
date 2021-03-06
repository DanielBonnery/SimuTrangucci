% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generateotherpop.R
\name{yotherf}
\alias{yotherf}
\title{Generates y}
\usage{
yotherf(XX, thetastar, sigma_y, nrep)
}
\arguments{
\item{lamda}{an object with same structure than [lambda1f(XX)]}

\item{delta}{an object with same structure than}
}
\description{
Generates y
}
\examples{
set.seed(1)
XX<-Gen_design_variables(N=1000,Q=3,p=4)
lambda1=lambda1f(XX)
delta=Gen_hyper_parameters(XX)$delta
lambda=lambdaSif(XX,lambda1,delta)
sigma=Gen_hyper_parameters(XX)$sigma
alpha=alphaSif(lambda,sigma)
alpha0=1
thetastar=thetastarf(alpha,alpha0)
sigma_y=Gen_hyper_parameters(XX)$sigma_y
yf(XX,thetastar,sigma_y,3)
}
