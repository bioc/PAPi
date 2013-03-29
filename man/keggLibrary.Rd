\name{keggLibrary}
\alias{keggLibrary}
\docType{data}
\title{
An example of the library mapping name of compounds to their respective KEGG codes.
}
\description{
keggLibrary is an example of the keggCodes library used by the function \code{\link{addKeggCodes}}, which automates the substitution of compounds names by KEGG codes. keggLibrary is a data frame containing a list of KEGG compounds codes in the first column and their respective names in the second column. 
}
\usage{data(keggLibrary)}
\format{
  A data frame with 2 columns and n rows, where n is the number of compounds present in the library.
  \describe{
    \item{\code{kegg}}{a list of KEGG codes for the compounds described in the second column.}
    \item{\code{Name}}{a list of compound names related to the KEGG codes in column one.}
  }
}
\details{
For a high-throughput platform, the keggLibrary must contain all the compounds potentially identifiable by the protocol in use. Every time a new compound is added, for example in the GC-MS library, the same compound must be added to the keggLibrary.
}

\references{
Aggio, R.B.M; Ruggiero, K. and Villas-Boas, S.G. (2010) - Pathway Activity Profiling (PAPi): from metabolite profile to metabolic pathway activity. Bioinformatics.
}
\examples{
data(keggLibrary)
print(keggLibrary)
}
\keyword{datasets}
