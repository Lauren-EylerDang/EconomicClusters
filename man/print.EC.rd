\name{print.EC}
\alias{print.EC}
\title{Print Output of 'EconomicClusters' Algorithm
}
\description{
  Prints output of the 'EconomicClusters' algorithm in a user-friendly manner.
}
\usage{
\method{print}{EC}(x, ...)
}
\arguments{
  \item{x}{ an object of class 'EC', as produced by function 'EconomicClusters'}
  \item{\dots}{ additional arguments to \code{\link{print}}
  }
}
\details{ This function is called internally by 'EconomicClusters'.
}
\value{
  A printout of maximum ASW, variables selected, cluster number, and medoid row indices
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}
\seealso{\code{\link{EconomicClusters}}
}

