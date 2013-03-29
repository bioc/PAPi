\name{buildDatabase}
\alias{buildDatabase}

\title{
Build a local database containing the required information from KEGG (Kyoto Encyclopedia of Genes and Genomes) database.
}
\description{
buildDatabase uses the internet connection to access KEGG database and locally install the required data to use \code{\link{papi}} offline.  
}
\usage{
buildDatabase(save = TRUE, folder, saveAs)
}

\arguments{
  \item{save}{
when save = TRUE, the new local database is saved in the folder defined by folder in addition to the folder R.home("library/PAPi/databases/").
}
  \item{folder}{
when save = TRUE and folder is missing, a pop up dialog box will be presented to the user. The user can then select the directory to which the results will be saved. Alternatively, folder can take a character string naming the path to the folder where the new database must be saved.
}
  \item{saveAs}{
A character string defining the name of the new local database.
}
}
\details{
\code{\link{papi}} is an algorithm to relate metabolite abundances to the activity of metabolic pathways. For this, PAPi uses the information available in KEGG database. However, applying PAPi online may be considerably time consuming depending on the internet connection available. In addition, KEGG database is constantly updated, which means that results produced by PAPi today may be different from results produced one month earlier. \code{\link{buildDatabase}} allows users to build local databases containing the required data from KEGG database. It allows to reproduce results at anytime and considerably faster analysis.    
    
}
\value{
buildDatabase stores the local database in R.home("library/PAPi/databases/").
}
\references{
Aggio, R.B.M; Ruggiero, K. and Villas-Boas, S.G. (2010) - Pathway Activity Profiling (PAPi): from metabolite profile to metabolic pathway activity. Bioinformatics.
}
\author{
Raphael Aggio
}
\note{
Raphael Aggio (raphael.aggio@gmail.com)
}

\seealso{
\code{\link{addKeggCodes}}, \code{\link{papi}}, \code{\link{papiHtest}} and \code{\link{papiLine}}.
}

\examples{
#buildDatabase(save = FALSE, saveAs = "MyNewDatabase")
}