% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generatepop.R
\name{yf}
\alias{yf}
\title{Generates y}
\usage{
yf(XX, thetastar, sigma_y, nrep)
}
\arguments{
\item{lamda}{an object with same structure than [gamma0f(XX)]}

\item{delta}{an object with same structure than}
}
\description{
Generates y
}
\examples{
set.seed(1)
XX<-Gen_design_variables(N=1000,Q=3,p=4)
gamma0=gamma0f(XX)
delta=Gen_hyper_parameters(XX)$delta
lambda=lambda.j.q1...qell.f(XX,gamma0,delta)
sigma=Gen_hyper_parameters(XX)$sigma
alpha=alphaSif(lambda,sigma)
alpha0=1
thetastar=thetastarf(alpha,alpha0)
sigma_y=Gen_hyper_parameters(XX)$sigma_y
yf(XX,thetastar,sigma_y,3)
}
