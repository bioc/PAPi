\name{papiResults}
\alias{papiResults}
\docType{data}
\title{
An example of the data frame produced by \code{\link{papi}}.
}
\description{
An example of the data frame produced by papi, where the first column contains the name of the metabolic pathways identified and the following columns contains their respective Activity Score (AS) in the different samples analyzed.
}
\usage{data(papiResults)}
\format{
  A data frame where the first column contains the name of the metabolic pathways identified and the following columns contains their respective Activity Score (AS) in the different samples analyzed.
  \describe{
    \item{\code{pathwayname}}{a list of metabolic pathways identified.}
    \item{\code{Sample1}}{the AS of each metabolic pathway in Sample 1}
    \item{\code{Sample2}}{the AS of each metabolic pathway in Sample 2}
    \item{\code{Sample3}}{the AS of each metabolic pathway in Sample 3}
    \item{\code{Sample4}}{the AS of each metabolic pathway in Sample 4}
    \item{\code{Sample5}}{the AS of each metabolic pathway in Sample 5}
    \item{\code{Sample6}}{the AS of each metabolic pathway in Sample 6}
  }
}

\examples{
data(papiResults)
print(papiResults)
}
\keyword{datasets}
