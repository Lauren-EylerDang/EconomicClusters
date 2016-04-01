\name{EC_vars}
\alias{EC_vars}
\title{Select Common Asset Variables}
\description{Because 'EconomicClusters' requires significant computing time to run, researchers may choose to limit the asset variables considered for selection. Assets that are very rare in a population may be useful for defining only very small (and perhaps less significant) economic groups. 'EC_vars' creates a data frame of binary variables owned by at least a specified proportion of the population and all categorical asset variables with >2 levels.}

\usage{EC_vars(X, p)}
\arguments{
  \item{X}{ a data frame of all asset variables coded as factors with column names specified}
  \item{p}{ lowest proportion of individuals in the population that own a given asset for that asset to be considered for variable selection}
}
\value{
  A data frame consisting of all binary asset variables owned by at least proportion p of households in the population and all asset variables with >2 levels
}
\note{
  The determination of p should be based on the researcher's goals for the granularity of economic clusters defined by the model and available server computing time. The number of variables to consider for selection can also be limited in other ways. For example, researchers might choose to include only one of a series of categorical variables that assess similar aspects of economic status, such as multiple variables describing materials used for housing construction. Consider whether the variables not selected by 'EC_vars' would be important to consider for selection despite being rare based on the economic context in your country.
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}

\seealso{\code{\link[EconomicClusters:EconomicClusters-package]{EC_DHSwts}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_time}}, \code{\link[EconomicClusters:EconomicClusters-package]{assets_fullset}}
}

\examples{
#Let's say our DHS dataset has 13 assets variables.
#We want to ask 5 questions to determine patient economic status in our trauma registry.
#If we include all 13 variables, we will have 1,287 possible combinations to assess!
#We will use EC_vars to narrow down the variables we consider to only relatively common assets.

data(assets_fullset)
assets<-EC_vars(assets_fullset, 0.10)

#By selecting assets owned by at least 10 percent of the population, 
#we now have 10 variables and 252 possible combinations. 
#We can determine whether or not this number of combinations 
#will result in a reasonable computing time using function 'EC_time'.
}


\keyword{function}