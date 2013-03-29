\name{papiData}
\alias{papiData}
\docType{data}
\title{
An example of the data frame produced by \code{\link{addKeggCodes}} and used by \code{\link{papi}}.
}
\description{
papiData is a data frame containing a list of identified metabolites in the first column, in this case represented by their respective KEGG codes, and their abundances in the different samples in the following columns.
}
\usage{data(papiData)}
\format{
  papiData is a data frame containing a list of identified metabolites in the first column, in this case represented by their respective KEGG codes, and their abundances in the different samples in the following columns.
  \describe{
    \item{\code{Name}}{list of identified metabolites, represented by their respective KEGG codes.}
    \item{\code{Sample1}}{the abundance of each metabolite in Sample 1}
    \item{\code{Sample2}}{the abundance of each metabolite in Sample 2}
    \item{\code{Sample3}}{the abundance of each metabolite in Sample 3}
    \item{\code{Sample4}}{the abundance of each metabolite in Sample 4}
    \item{\code{Sample5}}{the abundance of each metabolite in Sample 5}
    \item{\code{Sample6}}{the abundance of each metabolite in Sample 6}
  }
}

\examples{
data(papiData)
print(papiData)
}
\keyword{datasets}
