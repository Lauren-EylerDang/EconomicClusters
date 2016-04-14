\name{PopClusters}
\alias{PopClusters}
\docType{data}
\title{Simulated DHS Population Cluster Membership Data for 'EC_patient'
}
\description{
  This dataset was designed to represent a vector noting which economic cluster each observation from the DHS population dataset was assigned to by the 'EconomicClusters' algorithm.
}
\usage{data(Pop)}
\format{
  A data frame with 13 observations on 1 variable. To simplify the analysis for the sake of demonstration, this data frame was generated manually.
    \describe{
      \item{\code{PopClusters}}{ a character vector of cluster names}
    }
  
}
\details{
  The data frame 'PopClusters' represents a vector noting which economic cluster each observation from the DHS population dataset was assigned to by the 'EconomicClusters' algorithm. Note: A real DHS dataset has many more observations than this. This data frame was generated manually.
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
  #selected by the model for the cluster medoids as Medoids.
  #We saved a data frame consisting of the DHS population's responses to the economic questions
  #selected by the model as Pop.
  #We saved a vector of the cluster membership of each DHS observation as PopClusters.
  #We have since collected trauma registry data for 250 patients 
  #and asked them the 5 economic questions selected by the model.
  #Data set Pts contains the new patients' responses to these five questions.
  #We now want to know which economic cluster each patient belongs in.
  
  data(Pts)
  data(Medoids)
  data(Pop)
  data(PopClusters)
  Pt_clust<-EC_patient(Pts, Medoids, Pop, PopClusters)
  #number of patients per cluster:
  table(Pt_clust)
  
  #We now have a vector of patient cluster membership.
}
\keyword{datasets}
