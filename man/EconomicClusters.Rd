\name{EconomicClusters}
\alias{EconomicClusters}
\title{Population-Specific Economic Groups Defined by Few Asset Variables
}
\description{This function runs weighted k-medoids clustering using package 'WeightedCluster' for each combination of 'nvars' number of asset variables and selects the combination of asset variables and number of clusters with the highest average sillhouette width (ASW). The number of asset variables per combination and range of cluster numbers to be considered may be specified by the user. The aim of this function is to define the optimal clustering-based metric of economic status for a given population using a limited number of variables that can be assessed in a time-constrained setting, such as a trauma registry in a low- or middle-income country. Please note that the 'EconomicClusters' algorithm can require significant computing time to run on a real household survey data set. For a free, publically available option for acquiring server time, please see www... (insert tutorial link).
}
\usage{
EconomicClusters(X, Y, nvars, kmin, kmax, ncores, threshold)
}
\arguments{
  \item{X}{ a data frame with Column 1 containing weighted number of household members (coded as numeric) and Columns 2 through n containing all asset variables to be considered for variable selection (coded as factors). A data frame in this form can be produced by function 'EC_DHSwts' for DHS data. Column names should be specified. If Column 1 is coded as a factor, it will be assumed to be an asset variable rather than weights, and weights will default to 1.}
  \item{Y}{ optional variable in vector form to be included in all variable combinations. See details below.}
  \item{nvars}{ number of asset variables used to define economic clusters. If Y is not missing, the number of asset variables used to define economic clusters will be nvars+1.}
  \item{kmin}{ minimum number of clusters to be considered}
  \item{kmax}{ maximum number of clusters to be considered}
  \item{ncores}{ number of computing cores to be used in parallel}
  \item{threshold}{ optional minimum value of ASW that will stop the algorithm at a given number of clusters. If threshold is not specified, then the algorithm will assess all cluster values from kmin to kmax. See details.}
}
\details{
  'EconomicClusters' generates data frames consisting of all possible combinations of 'nvars' number of asset variables and generates distance matrices for each data frame using function 'daisy' from package 'cluster' with metric = "gower". It then runs weighted k-medoids clustering using package 'WeightedCluster' on each of these data frames. The algorithm selects the clustering model with the maximum average sillhouette width (ASW) and reports the ASW, variables selected, and number of clusters of this model. The resulting 'EconomicClusters' object also includes a list of cluster medoids for the selected model, a data frame consisting of the cluster medoids' responses to the model-defining variables, a vector listing cluster membership for all observations in the orginial data set, and a data frame of the full population's responses to the selected variables.
  
  Some researchers may wish to include a specific variable in all cluster models. For example, the DHS now reports two different wealth index models, one for rural and one for urban populations, because the meaning of asset-based wealth within these two populations can be very different (Rutstein, 2008). One could include rural/urban setting as a variable in all models by specifying Y as a vector indicating rural or urban location.
  
  Generating a model with too many clusters may create challenges in data analysis due to small sample size in some clusters. For this reason, some researchers may prefer to set a threshold ASW. In this case, the algorithm will assess all combinations of 'nvars' assets variables for each cluster number starting from 'kmin' but will stop after completing calculations for the cluster number being analyzed when the threshold ASW is reached. The algorithm will then select the combination of variables with that cluster number that have the highest ASW. If a threshold is not specified, then the algorithm will assess all cluster numbers from kmin to kmax.
  
  In some cases, more than one model may be tied for highest ASW. In this case, 'EconomicClusters' will return results for each of the models achieving the maximum ASW. In this case, the 'Medoid_dataframe', 'Cluster', and 'Population_assets' results will be in list form and must be converted to data frames or vectors if they are to be used in those formats. If only one model is selected, 'Medoid_dataframe' and 'Population_assets' are returned as data frames, and 'Cluster' is returned as a vector.
  
  For further information regarding this methodology, please see Eyler et al. (2016)[2]. Recommended methods of intepreting the results of 'EconomicClusters' are also described in the examples below.
}
\value{
  \item{ASW_max}{ ASW of the selected model, which is the maximum ASW over all tested models}
  \item{Assets}{ asset variables included in the selected model}
  \item{K}{ number of clusters in the selected model}
  \item{Medoids}{ vector containing the row indices of the observations designated as cluster medoids in the selected model}
  \item{Medoid_dataframe}{ data frame consisting of the cluster medoids' responses to the model-defining variables}
  \item{Cluster}{ vector of cluster membership (as denoted by the row index for the cluster medoid) for all observations in the original data set}
  \item{Population_assets}{ data frame consisting of the full population's responses to the model-defining variables}
}

\references{
Rutstein SO. (2008). The DHS wealth index: Approaches for rural and urban areas. DHS Working Papers. MEASURE DHS+, ORC Macro: Calverton, Maryland, <\url{http://pdf.usaid.gov/pdf_docs/PNADN521.pdf
>}.

Eyler LE, Hubbard A, Juillard CJ...

Kaufman L, Rousseeuw PJ. (2009). Finding Groups in Data: An Introduction to Cluster Analysis (Vol. 344). John Wiley & Sons: Hoboken, New Jerkey. <ISBN-13: 978-0471735786; ISBN-10: 0471735787>
}

\seealso{\code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters-package}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_vars}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_DHSwts}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_time}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_patient}}, \code{\link[EconomicClusters:EconomicClusters-package]{data_for_EC}}, \code{\link[WeightedCluster]{wcKMedRange}}, \code{\link[cluster]{daisy}}, \code{\link[foreach]{foreach}}, \code{\link[doParallel]{doParallel}}
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}

\examples{
#Note: Running the below example can take significant computing time (approx. 45 minutes). 
#Use function 'EC_time' first to determine how long it will take
#to run the full 'EconomicClusters' algorithm 
#on the sample data set 'data_for_EC' on your computer.
#Using function 'EC_DHSwts', we have generated data frame 'data_for_EC' 
#with Column 1 containing weighted number of household members 
#and Columns 2 to 11 describing ownership of 10 assets we want to consider for variable selection.
#Now, let's use 'EconomicClusters' to select the 5 variables 
#and number of clusters that best cluster our population.
#We choose to set cluster range from 5 to 10.

data(data_for_EC)
EC<-EconomicClusters(data_for_EC, nvars=5, kmin=5, kmax=10, ncores=2)

#For a guide for interpreting ASW_max, see Kaufman and Rousseeuw (2009).

#To view a data frame consisting of the cluster medoids' responses to the model-defining variables:

EC$Medoid_dataframe

#To view a vector of cluster membership (as denoted by the row index for the cluster medoid) 
#for all observations in the original data set:

EC$Cluster

#To view a data frame consisting of the population's responses to the model-defining variables:

EC$Population_assets

#To interpret what being a member of each economic cluster means, 
#look at the distribution of all assets in each population cluster. 
#We recommend interpreting the clusters in conjunction 
#with researchers familiar with the economic context 
#in the country whose data you are using, 
#if you are not personally familiar with this context.

#We have now defined our economic model. 
#To assign new patients to economic clusters based on this model, 
#use the function 'EC_patient'.

}


\keyword{function}
