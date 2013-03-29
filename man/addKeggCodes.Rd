\name{addKeggCodes}
\alias{addKeggCodes}

\title{
In a data frame, it substitutes the name of compounds by their respective KEGG codes.
}
\description{
addKeggCodes makes use of a user library to automatically substitute compounds names by KEGG compounds codes. KEGG codes are required to use the function \code{\link{papi}}.  
}
\usage{
addKeggCodes(inputData, keggCodes, folder, save = TRUE, 
output = "data_with_kegg_codes", addCodes = TRUE)
}

\arguments{
  \item{inputData}{
when inputData is missing, a dialog box will pop up allowing the user to click-and-point to the comma separated value (CSV) file from which the input data is to be read. Alternatively, inputData can take a character string naming the path to the CSV file to be read or the name of a variable (data frame) containing the metabolomics data (See data(metabolomicsData)).
}
  \item{keggCodes}{
when keggCodes is missing, a dialog box will pop up allowing the user to click-and-point to the comma separated value (CSV) file containing the KEGG codes library (See Details).
}
  \item{folder}{
when save = TRUE and folder is missing, a pop up dialog box will be presented to the user. The user can then select the directory to which the results will be saved. Alternatively, folder can take a character string naming the path to the folder where the results must be saved.
}
  \item{save}{
A logical vector (TRUE or FALSE) defining if the results must be saved into a CSV file.
}
  \item{output}{
A character string indicating the name of the file containing the results.
}
  \item{addCodes}{
A logical vector (TRUE or FALSE) defining if missing KEGG codes should be added to the library in use (See Details).
}
}
\details{
\code{\link{papi}} is an algorithm to relate metabolite abundances to the activity of metabolic pathways. The input data for papi is a typical metabolomics data set, which is generally organized as a list of metabolites in the first column with their respective abundances in different samples in the following columns (See data(metabolomicsData)). For papi, the name of metabolites MUST be substituted by their respective KEGG codes. The KEGG code of a compound is a unique identifier used by KEGG database. addKeggCodes automatically substitutes the name of compounds by their KEGG codes found in a KEGG code library built and defined by the user. 
The inputData for addKeggCodes is a data frame containing the name of compounds in the first column and their respective abundances in the different samples in the following columns. See data(metabolomicsData) for an example of inputData.
The keggCodes library is a data frame that MUST contain Kegg codes in the first column and their respective compounds names in the second column. See data(keggLibrary) for an example of Kegg codes library. Ideally, the keggCodes library must contain all the compounds potentially identifiable by the analytical technique and protocol in use.
When addCodes = TRUE, compounds that are present in the inputData but not present in the KeggCodes library will be reported to the user. Then, the missing KEGG codes for these compounds can be automatically searched in the KEGG database or they can be manually added to the KeggCodes library. Compounds not present in KEGG database must receive the value 'absent' as KEGG code. Compounds not related to any KEGG code will NOT be analyzed by papi.  
    
}
\value{
addKeggCodes returns a data frame in the same format of the inputData, however, containing KEGG codes instead of the name of compounds in the first column.
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
\code{\link{buildDatabase}}, \code{\link{papi}}, \code{\link{papiHtest}} and \code{\link{papiLine}}.
}

\examples{
## Building the input data ####
Names <- c("Replicates", "Glucose", "Fructose 6-phosphate", 
"Glyceraldehyde 3-phosphate", "Glycerone phosphate", "3-Phospho-D-glycerate")
Sample1 <- c("cond1", 0.8, 0.3, 1.1, 1.2, 0.2)
Sample2 <- c("cond1", 0.6, 0.2, 1.5, 1.5, 0.3)
Sample3 <- c("cond1", 0.7, 0.4, 1.2, 1.3, 0.5)
Sample4 <- c("cond2", 1.2, 0.6, NA, 0.2, 1.1)
Sample5 <- c("cond2", 1.1, 0.7, NA, 0.3, 1.0)
Sample6 <- c("cond2", 1.5, 0.7, NA, 0.2, 0.9) 

metabolomicsData <- data.frame(cbind(Names, Sample1, Sample2, Sample3, Sample4,
 Sample5, Sample6), stringsAsFactors = FALSE)

## Building the keggCodes library ####
kegg <- c("C00031", "C05345", "C00118", "C00111", "C00197", "absent")
Name <- c("Glucose", "Fructose 6-phosphate", "Glyceraldehyde 3-phosphate", 
"Glycerone phosphate", "3-Phospho-D-glycerate", "Citrate")

keggLibrary <- data.frame(cbind(kegg, Name), stringsAsFactors = FALSE)

### Applying addKeggCodes ####
papiData <- addKeggCodes(metabolomicsData, keggLibrary, save = FALSE, addCodes = FALSE)
}