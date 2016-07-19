\name{EconomicClusters-package}
\alias{EconomicClusters-package}
\docType{package}
\title{Population-Specific Economic Groups Defined by Few Asset Variables}
\description{The aim of this package is to facilitate health disparities research in time-constrained, low-resource settings, such as trauma registries in low- and middle-income countries. The 'EconomicClusters' algorithm defines population-specific metrics of economic status based on limited numbers of asset variables. This package is designed for use with large-scale household survey data, such as Demographic and Health Survey (DHS) data sets. Economic groups are defined by running weighted k-medoids clustering using package 'WeightedCluster' on all combinations of a limited number of asset variables. The combination of variables and number of clusters with the highest average sillhouette width (ASW) are selected as the optimal economic clustering model. We also include support functions to facilitate the use of the 'EconomicClusters' algorithm, including a function to assign study subjects to an economic cluster. For a more detailed description of this methodology, please see Eyler et al. (2016). Please note that the 'EconomicClusters' algorithm can require significant computing time to run on a real household survey data set. We have developed an EC shiny app to facilitate your use of the algorithm with our server. Please email Dr. Lauren Eyler at economic.clusters@gmail.com for more information about how to obtain and use this app.}
\details{
\tabular{ll}{
Package: \tab EconomicClusters\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2015-10-30\cr
License: \tab GPL (>=3)\cr
}




}
\author{
Lauren Eyler, Alan Hubbard, Catherine Juillard

Maintainer: Lauren Eyler <economic.clusters@gmail.com>
}
\references{
Eyler LE, Hubbard A, Juillard CJ. (2016). Assessment of Economic Status in Trauma Registries: A New Algorithm for Generating Population-Specific Clustering-Based Models of Economic Status for Time-Constrained Low-Resource Settings. International Journal of Medical Informatics, 94, 49-58. doi: 10.1016/j.ifmedinf.2016.05.004.

Rutstein SO. (n.d.). Steps to Constructing the New DHS Wealth Index. ICF International: Rockville, Maryland, <\url{http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
}>

Kaufman L, Rousseeuw PJ. (2009). Finding Groups in Data: An Introduction to Cluster Analysis (Vol. 344). John Wiley & Sons: Hoboken, New Jerkey. <ISBN-13: 978-0471735786; ISBN-10: 0471735787>
}

\keyword{ package }
\seealso{\code{\link[WeightedCluster]{wcKMedRange}}, \code{\link[cluster]{daisy}}, \code{\link[foreach]{foreach}}, \code{\link[doParallel]{doParallel}
}}
\examples{
#Step 1: Consider limiting the number of variables to be considered for selection
#to only assets that are relatively common in your population.
#'assets_fullset' is a data frame of all asset variables from our simulated data set.
#We want to select binary variables owned by at least 10 percent of households.

data(assets_fullset)
assets<-EC_vars(assets_fullset, p=0.10)

#'assets' is a data frame of all binary variables from 'assets_fullset' 
#owned by at least 10 percent of households 
#and all categorical asset variables with >2 levels.

#Step 2: Generate a vector of weighted number of household members, 
#as per DHS wealth index protocols (Rutstein, n.d.) 
#and bind it as the first column in your 'assets' data frame.
#The variables needed to calculate the DHS weighted number of household members variable are: 
#HH_survey$dejure = number of dejure household members, 
#HH_survey$defacto = number of defacto household members, 
#HH_survey$HHwt = household sampling weight.

data(HH_survey)
data(assets)
data_for_EC<-EC_DHSwts(assets, HH_survey$dejure, HH_survey$defacto, HH_survey$HHwt)

#Step 3: Estimate how long it will take to run the full 'EconomicClusters' algorithm 
#by running the algorithm on 2 combinations in parallel on 2 computing cores 
#to select 5 variables with cluster number ranging from 5 to 10:

data(data_for_EC)
EC_time(data_for_EC, nvars=5, kmin=5, kmax=10, ncores=2)

#Step 4: Run 'EconomicClusters' to define a population-specific model of ecnomomic status 
#with 5 variables and between 5 and 10 clusters for your data set.
#The 'EconomicClusters' algorithm will require significantly more computing time
#to run on a real household survey data set. 
#For a free, publically available option for acquiring server time, 
#please see www... (insert tutorial link).

data(data_for_EC)
EC<-EconomicClusters(data_for_EC, nvars=5, kmin=5, kmax=10, ncores=2)


#For a guide for interpreting ASW_max, see Kaufman and Rousseeuw (2009).

#To view a data frame consisting of the cluster medoids' responses to the model-defining variables:

EC$Medoid_dataframe

#To view a vector of cluster membership (as denoted by the row index for the cluster medoid) 
#for all observations in the original data set:

EC$Cluster

#To interpret what being a member of each economic cluster means, 
#look at the distribution of all assets in each population cluster. 
#We recommend interpreting the clusters in conjunction 
#with researchers familiar with the economic context 
#in the country whose data you are using, 
#if you are not personally familiar with this context.

#Step 5: Assign patients from your trauma registry ('Pts') 
#to an economic cluster based on their similarity to the cluster 'Medoids':

data(Pts)
data(Medoids)
data(Pop)
data(PopClusters)
Pt_clust<-EC_patient(Pts, Medoids, Pop, PopClusters)
#number of patients per cluster:
table(Pt_clust)

#We now have a vector of patient cluster membership (Pt_clust).

#Step 6: Use this information to assess for and take steps to eliminate 
#health disparities in your population.
#Unfortunately, there is no statistical algorithm for the latter part...
#Good luck!

}
