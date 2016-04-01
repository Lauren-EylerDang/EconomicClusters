\name{Pts}
\alias{Pts}
\docType{data}
\title{Simulated Patient Data for 'EC_patient'
}
\description{
  This data set represents simulated patient data for the asset variables selected by 'EconomicClusters'.
}
\usage{data(Pts)}
\format{
  A data frame with 250 observations on the following 5 variables.
  \describe{
    \item{\code{V2}}{ a binary 0/1 variable with probability 0.4 of value 1}
    \item{\code{V5}}{ a binary 0/1 variable with probability 0.6 of value 1}
    \item{\code{V6}}{ a binary 0/1 variable with probability 0.8 of value 1}
    \item{\code{V9}}{ a categorical variable with the following probabilities: p(V9=1)=0.4, p(V9=2)=0.3, p(V9=3)=0.2, p(V9=4)=0.1. V9 is highly correlated to V11 (correlation coefficient=0.95)}
    \item{\code{V11}}{ a categorical variable with the following probabilities: p(V11=1)=0.4, p(V11=2)=0.3, p(V11=3)=0.2, p(V11=4)=0.1. V11 is highly correlated to V9 (correlation coefficient=0.95)}
  }  
}
\details{
  The data frame 'Pts' was simulated to represent the economic data collected in a patient registry to assess patient economic status using the 5 variables selected by the 'EconomicClusters' algorithm. Binary variables were generated using 'rbinom'. Multi-level categorical variables were generated with function 'ordsample' from package 'GenOrd'. 
}
\source{
  This data set was simulated by the package authors in order to demonstrate the functionality of the 'EconomicClusters' package.
}
\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}
}
\examples{
#We previously defined an economic clustering model for our population using 'EconomicClusters'.
#We saved a data frame containing the responses to the economic questions 
#selected by the model for the cluster medoids as 'Medoids'.
#We have since collected trauma registry data for 250 patients 
#and asked them the 5 economic questions selected by the model.
#Data set 'Pts' contains the new patients' responses to these five questions.
#We now want to know which economic cluster each patient belongs in.

data(Pts)
data(Medoids)
Pt_clust<-EC_patient(Pts, Medoids)
#number of patients per cluster:
table(Pt_clust$group)

#We now have a vector of patient cluster membership ('Pt_clust$group')
}
\keyword{datasets}
