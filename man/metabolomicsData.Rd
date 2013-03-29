\name{metabolomicsData}
\alias{metabolomicsData}
\docType{data}
\title{
An example of metabolomics data set used by \code{\link{addKeggCodes}}.
}
\description{
metabolomicsData is an example of a typical metabolomics data set, where it has a list of identified metabolites in the first column and their respective abundances in the different samples in the following columns. The first row of metabolomicsData, Replicates, defines to which experimental condition each sample belongs.
}
\usage{data(metabolomicsData)}
\format{
  A data frame containing the names of identified metabolites in the first column and their respective abundances in the different samples in the following columns.
  \describe{
    \item{\code{Names}}{list of identified metabolites}
    \item{\code{Sample1}}{the abundance of each metabolite in Sample 1}
    \item{\code{Sample2}}{the abundance of each metabolite in Sample 2}
    \item{\code{Sample3}}{the abundance of each metabolite in Sample 3}
    \item{\code{Sample4}}{the abundance of each metabolite in Sample 4}
    \item{\code{Sample5}}{the abundance of each metabolite in Sample 5}
    \item{\code{Sample6}}{the abundance of each metabolite in Sample 6}
  }
}

\examples{
data(metabolomicsData)
print(metabolomicsData)
}
\keyword{datasets}
