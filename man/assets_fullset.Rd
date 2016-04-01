\name{assets_fullset}
\alias{assets_fullset}
\docType{data}
\title{Simulated Household Survey Assets Data - Full Set
}
\description{This data set contains simulated household survey assets data including binary and multi-level categorical assets variables for use with function 'EC_vars'. 
}
\usage{data(assets_fullset)}
\format{
  A data frame with 1000 observations on the following 13 variables coded as factors.
  \describe{
    \item{\code{V1}}{ a binary 0/1 variable with probability 0.05 of value 1}
    \item{\code{V2}}{ a binary 0/1 variable with probability 0.4 of value 1}
    \item{\code{V3}}{ a binary 0/1 variable with probability 0.05 of value 1}
    \item{\code{V4}}{ a binary 0/1 variable with probability 0.05 of value 1}
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
  This data set was simulated in a format similar to assets data collected in a large-scale household survey. Such data sets generally include binary variables (e.g. does your household own a cell phone?) and multi-level categorical variables (e.g. what type of water source does your household use?). In 'assets_fullset', each row represents a household, and each household's responses to the assets questions are coded as factors. Binary variables were generated using function 'rbinom' with varying probabilities. Multi-level categorical variables were generated using function 'ordsample' from package 'GenOrd'.
}
\source{
  This data set was simulated by the package authors to demonstrate the functionality of the 'EconomicClusters' package.
}
\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EC_vars}}
}
\examples{
#Let's say our household survey dataset has 13 asset variables.
#We want to ask 5 questions to determine patient economic status in our trauma registry.
#If we include all 13 variables, we will have 1,287 possible combinations to assess!
#We will use EC_vars to narrow down the variables we consider to only relatively common assets.
#Note: The first asset variable with >2 levels is listed in Column 7 
#of our data set 'assets_fullset'.

data(assets_fullset)
assets<-EC_vars(assets_fullset, 0.10, 7)

#By selecting assets owned by at least 10 percent of the population, 
#we now have 10 variables and 252 possible combinations. 
#We can determine whether or not this number of combinations 
#will result in a reasonable computing time using function 'EC_time'.
}
\keyword{datasets}
