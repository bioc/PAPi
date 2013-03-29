\name{papi}
\alias{papi}
\title{
papi - Pathway Activity Profiling
}
\description{
papi was developed to apply the PAPi method to a metabolomics data set (See references). PAPi relates metabolite abundances to metabolic pathways activities/fluxes in order to generate potential hypothesis for achieving the biological interpretation. 
}
\usage{
papi(inputData, save = TRUE, folder, output = "papi_results", offline = TRUE, localDatabase = "default")
}

\arguments{
  \item{inputData}{
when inputData is missing, a dialog box will pop up allowing the user to click-and-point to the comma separated value (CSV) file from which the data is to be read. Alternatively, inputData can take a character string naming the path to the CSV file to be read or the name of a variable (data frame) containing the metabolomics data (See Details).
}
  \item{save}{
A logical vector (TRUE or FALSE) defining if the results must be saved into a CSV file.
}
  \item{folder}{
when save = TRUE and folder is missing, a pop up dialog box will be presented to the user. The user can then select the directory to which the results will be saved. Alternatively, folder can take a character string naming the path to the folder where the results must be saved.
}
  \item{output}{
A character string indicating the name of the file containing the results.
}
  \item{offline}{
A logical vector (TRUE or FALSE) defining if the papi analysis should be done offline using a local database (see \code{\link{buildDatabase}}).
}
  \item{localDatabase}{
localDatabase may receive the value "default", "choose" or the name of a specific local database. When localDatabase = "default", the default local database is used. When localDatabase = "choose", a list with the available databases is presented to the user. Alternatively, localDatabase can receive the name of the database to be used.
}
}
\details{
papi is an algorithm to relate metabolite abundances to the activity of metabolic pathways. The inputData for papi is a typical metabolomics data set (see data(metabolomicsData)), which is generally organized as a list of metabolites in the first column with their respective abundances in different samples in the following columns. For papi, the name of metabolites MUST be substituted by their respective KEGG codes. The KEGG code of a compound is a unique identifier used by KEGG database. See data(papiData) for an example of the inputData. 
The KEGG codes can be found directly at KEGG website (http://www.genome.jp/kegg/) or by using the function \code{\link{addKeggCodes}}. The first row of the inputData MAY be used to define to which experimental conditions each sample belongs. For this, the first column of the first row must receive the word "Replicates" and the following columns must receive a character string indicating the experimental conditions to which each sample belongs.
papi can be applied by simply inserting papi() at the R console. A pop up window will let the user point to a CSV file containing the input data, while another pop up window will let the user indicate the folder where the results are to be saved. Alternatively, the user may use a character string to indicate the path to the CSV file containing the inputData or the user may indicate a R data frame containing the inputData.
The argument offline is used to define if papi will use the KEGG database through an internet connection or if papi will use a local database. PAPi packages contains a default local database, which was created on 25/03/2013. The function \code{\link{buildDatabase}} can be used to create new local databases. The default behavior of papi is to use a local database, as it is considerably faster than using the internet.    
}
\value{
papi returns a data frame containing the identified metabolic pathways in the first column and their respective Activity Score for each sample in the following columns (See data(papiResults)).
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
\code{\link{papiLine}}, \code{\link{papiHtest}} and \code{\link{addKeggCodes}}.
}
\examples{
### Building input data ####
Names <- c("Replicates", "C00197", "C05345", "C00031", "C00118", "C00111")
Sample1 <- c("cond1", 0.2, 0.3, 0.8, 1.1, 1.2)
Sample2 <- c("cond1", 0.3, 0.2, 0.6, 1.5, 1.5)
Sample3 <- c("cond1", 0.5, 0.4, 0.7, 1.2, 1.3)
Sample4 <- c("cond2", 1.1, 0.6, 1.2, NA, 0.2)
Sample5 <- c("cond2", 1.0, 0.7, 1.1, NA, 0.3)
Sample6 <- c("cond2", 0.9, 0.7, 1.5, NA, 0.2) 

papiData <- data.frame(cbind(Names, Sample1, Sample2, Sample3, Sample4, 
Sample5, Sample6), stringsAsFactors = FALSE)

### Applying papi ####
#papiResults <- papi(papiData, save = FALSE)
}