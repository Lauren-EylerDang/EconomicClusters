\name{assets}
\alias{assets}
\docType{data}
\title{Simulated Household Survey Assets Data - Common Variables
}
\description{This data set contains simulated household survey assets data, including binary and multi-level categorical assets variables, for use with function 'EC_DHSwts'. 
}
\usage{data(assets)}
\format{
  A data frame with 100 observations on the following 10 variables coded as factors.
 \describe{
    \item{\code{V2}}{ a binary 0/1 variable with probability 0.4 of value 1}
    \item{\code{V5}}{ a binary 0/1 variable with probability 0.6 of value 1}
    \item{\code{V6}}{ a binary 0/1 variable with probability 0.8 of value 1}
    \item{\code{V7}}{ a categorical variable with the following probabilities: p(V7=1)=0.4, p(V7=2)=0.3, p(V7=3)=0.2, p(V7=4)=0.1}
    \item{\code{V8}}{ a categorical variable with the following probabilities: p(V8=1)=0.4, p(V8=2)=0.3, p(V8=3)=0.2, p(V8=4)=0.1}
    \item{\code{V9}}{ a categorical variable with the following probabilities: p(V9=1)=0.4, p(V9=2)=0.3, p(V9=3)=0.2, p(V9=4)=0.1. V9 is highly correlated to V11 (correlation coefficient=0.95)}
    \item{\code{V10}}{ a categorical variable with the following probabilities: p(V10=1)=0.4, p(V10=2)=0.3, p(V10=3)=0.2, p(V10=4)=0.1}
    \item{\code{V11}}{ a categorical variable with the following probabilities: p(V11=1)=0.4, p(V11=2)=0.3, p(V11=3)=0.2, p(V11=4)=0.1. V11 is highly correlated to V9 (correlation coefficient=0.95)}
    \item{\code{V12}}{ a categorical variable with the following probabilities: p(V12=1)=0.4, p(V12=2)=0.3, p(V12=3)=0.2, p(V12=4)=0.1}
    \item{\code{V13}}{ a categorical variable with the following probabilities: p(V13=1)=0.4, p(V13=2)=0.3, p(V13=3)=0.2, p(V13=4)=0.1}
  }
}

\details{
  Data frame 'assets' was simulated in a format similar to assets data collected in a large-scale household survey. Such data sets generally include binary variables (e.g. does your household own a cell phone?) and multi-level categorical variables (e.g. what type of water source does your household use?). In 'assets', each row represents a household, and each household's responses to the assets questions are coded as factors. Binary variables were generated using function 'rbinom' with varying probabilities. Multi-level categorical variables were generated using function 'ordsample' from package 'GenOrd'. Variable names are not contiguous, as we are assuming some rare assets variables were already eliminated from the data set using function 'EC_vars'.
}
\source{
  This data set was simulated by the package authors to demonstrate the functionality of the 'EconomicClusters' package.
}
\references{
Rutstein SO. (n.d.). Steps to Constructing the New DHS Wealth Index. ICF International: Rockville, Maryland, <\url{http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
}>
}
\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EC_vars}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_DHSwts}}, \code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}
}
\examples{
#'assets' is meant to be used in conjunction with 'HH_survey' 
#to demonstrate the use of function 'EC_DHSwts'
#In our sample dataset, relevant variables are coded as follows:
#HH_survey$HHwt = household sampling weight
#HH_survey$dejure = number of dejure household members
#HH_survey$defacto = number of defacto household members
#Let's assume that we've already made a data frame containing 
#only the asset variables we will consider for variable selection, called 'assets'.
#Now, let's create a data frame with Column 1 containing weighted number of household members 
#as per DHS Wealth Index protocols (Rutstein, n.d.) 
#and Columns 2 to 11 containing all asset variables to be considered for selection.

data(HH_survey)
data(assets)
data_for_EC<-EC_DHSwts(assets, HH_survey$dejure, HH_survey$defacto, HH_survey$HHwt)

#data_for_EC is now a data frame in the format needed to run 'EconomicClusters'

}

\keyword{datasets}
