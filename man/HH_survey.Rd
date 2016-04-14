\name{HH_survey}
\alias{HH_survey}
\docType{data}
\title{Data for Generating Weighted Number of Household Members
}
\description{This data set contains simulated variables for determining the weighted number of household members as per Demographic and Health Surveys (DHS) wealth index protocols (Rutstein, n.d.). 
}
\usage{data(HH_survey)}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{HHwt}}{ a numeric vector of household sampling weights}
    \item{\code{dejure}}{ a numeric vector describing number of dejure household members}
    \item{\code{defacto}}{ a numeric vector describing number of defacto household members}
  }
}
\details{
  This data frame includes simulated values for the three variables found in the Demographic and Health Surveys (DHS) datasets that are necessary to determine the weighted number of household members, as described in the DHS wealth index protocols (Rutstein, n.d.). These variables are: number of dejure household members, number of defacto household members, and household sampling weight. Variables were simulated using function 'sample'.
}
\source{
  This data set was simulated by the package authors in order to demonstrate the functionality of the 'EconomicClusters' package.
}

\references{
Rutstein SO. (n.d.). Steps to Constructing the New DHS Wealth Index. ICF International: Rockville, Maryland, <\url{http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
}>
}
\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EC_vars}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_time}}, \code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}
}
\examples{
#In our sample data set, relevant variables are coded as follows:
#HH_survey$HHwt = household sampling weight
#HH_survey$dejure = number of dejure household members
#HH_survey$defacto = number of defacto household members
#Let's assume that we have already made a data frame containing 
#only the asset variables we will consider for variable selection, called 'assets'.
#Now, let's create a data frame with Column 1 containing weighted number of household members 
#and Columns 2 through 11 containing all asset variables to be considered for selection.

data(HH_survey)
data(assets)
data_for_EC<-EC_DHSwts(assets, HH_survey$dejure, HH_survey$defacto, HH_survey$HHwt)

#data_for_EC is now a data frame in the format needed to run 'EconomicClusters'

}
\keyword{datasets}
