addKeggCodes <-
function(inputData, keggCodes, folder, save = TRUE, output = "data_with_kegg_codes", addCodes = TRUE) {

	## Function to check if file is CSV ##
	isCSVdlg <- function(titleMSG, errorMSG) {
		t = 0
		while (t == 0) {
			checkIfCsv <- dlgOpen(title = titleMSG, multiple = FALSE)$res
			checkIfCsv2 <- basename(checkIfCsv)
			checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
			checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
			if (checkIfCsv4 %in% c("CSV", "csv")) {
				t = 1
				return(checkIfCsv)
			} else {
				dlgMessage(errorMSG)
			}
		}
	}
	#########################################################
	
	#### No dialog box ###
	isCSV <- function(pathFile, errorMSG) {
		t = 0
		checkIfCsv <- pathFile
		checkIfCsv2 <- basename(checkIfCsv)
		checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
		checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
		if (checkIfCsv4 %in% c("CSV", "csv")) {
			t = 1
			return(t)
		} else {
			#dlgMessage(errorMSG)
			return(t)
		}
	}
	#########################################################
    
    #### Check if the OS is Widnows ###
    OSsystem <- Sys.info()["sysname"]
    if (OSsystem == "Windows"){
        FolderDivisor <- "\\"
    } else {
        FolderDivisor <- "/"
    }
    
	## Begin collecting arguments
	if (missing(inputData)) {
		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
		dataKegg <- read.csv(inputData, colClasses = "character")
		print("Input file loaded...")
	} else {
		if (is.data.frame(inputData) == TRUE) {
			dataKegg <- inputData
			print("Data frame loaded...")
		} else {
			if (is.character(inputData)) {
				checkIfCsv <- isCSV(inputData)
				if (checkIfCsv == 1) {
					inputTest <- file.access(inputData, 0)
					if (inputTest == 0) {
						dataKegg = read.csv(inputData, colClasses = "character")
					} else {
						dlgMessage("The input file specified is not accessible. Please, choose a valid CSV file to be used as input data.")
						inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
						dataKegg <- read.csv(inputData, colClasses = "character")
						print("Input file loaded...")
					}
				} else {
					dlgMessage("The input file specified is not in CSV format. Please, choose a valid CSV file to be used as input data.")
					inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
					dataKegg <- read.csv(inputData, colClasses = "character")
					print("Input file loaded...")
				}
			} else {
				dlgMessage("The path to the input data must be specified as character string. Please, choose a valid CSV file to be used as input data.")
				inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
				dataKegg <- read.csv(inputData, colClasses = "character")
				print("Input file loaded...")
			}
		}
	}

	#### Upload KEGG codes #####
	if (missing(keggCodes)) {
		keggCodes <- isCSVdlg("Select the CSV file containing the KEGG codes", "The KEGG code library MUST be in the format of comma-separated value (csv). Please, choose a KEGG code library showing the extension .csv.")
		codesKegg <- read.csv(keggCodes, colClasses = "character")
		print("keggCodes - csv file loaded...")
	} else {
		if (is.data.frame(keggCodes) == TRUE) {
			codesKegg <- keggCodes
			print("keggCodes - Data frame loaded...")
		} else {
			if (is.character(keggCodes)) {
				checkIfCsv <- isCSV(keggCodes)
				if (checkIfCsv == 1) {
					inputTest <- file.access(keggCodes, 0)
					if (inputTest == 0) {
						codesKegg = read.csv(keggCodes, colClasses = "character")
					} else {
						dlgMessage("The KEGG code library specified is not accessible. Please, choose a valid CSV file to be used as KEGG code library.")
						keggCodes <- isCSVdlg("Select the CSV file containing KEGG codes", "The KEGG code library MUST be in the format of comma-separated value (csv). Please, choose a KEGG code library showing the extension .csv.")
						codesKegg <- read.csv(keggCodes, colClasses = "character")
						print("keggCodes - csv file loaded...")
					}
				} else {
					dlgMessage("The KEGG code library specified is not in CSV format. Please, choose a valid CSV file to be used as KEGG code library.")
					keggCodes <- isCSVdlg("Select the CSV file containing KEGG code library", "The KEGG code library MUST be in the format of comma-separated value (csv). Please, choose a KEGG code library showing the extension .csv.")
					codesKegg <- read.csv(keggCodes, colClasses = "character")
					print("keggCodes - csv file loaded...")
				}
			} else {
				dlgMessage("The path to the KEGG code library must be specified as character string. Please, choose a valid CSV file to be used as KEGG code library.")
				keggCodes <- isCSVdlg("Select the CSV file containing the KEGG code library", "The KEGG code library MUST be in the format of comma-separated value (csv). Please, choose a KEGG code library showing the extension .csv.")
				codesKegg <- read.csv(keggCodes, colClasses = "character")
				print("keggCodes - csv file loaded...")
			}
		}
	}

	if (save == TRUE) {
		if (missing(folder)) {
			print("No folder was defined to save the results.")
			print("Please, point to the folder where the results should be saved.")
			folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
		} else {
			if (is.character(inputData)) {
				isFolder <- file.access(as.character(folder), 0)
				if (isFolder == 0) {
					isFolder <- file.info(folder)
					if (isFolder$isdir != TRUE) {
						print("The folder defined to save the results is not a valid path.")
						print("Please, point to the folder where the results should be saved.")
						folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
					}
				} else {
					print("The folder defined to save the results is not a valid path.")
					print("Please, point to the folder where the results should be saved.")
					folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
				}
			} else {
				print("The path to the folder where the results will be saved must be specified as character.")
				print("Please, point to the folder where the results should be saved.")
				folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
			}
		}
	}

	if (dataKegg[1, 1] %in% c("Replicates", "Replicate", "replicates", "replicate")) {
		replicates <- as.character(dataKegg[1, ])
		dataKegg <- dataKegg[-1, ]
		rep <- 1
	}

	#######
	original.data <- dataKegg
	original.kegglib <- codesKegg
	original.comps <- dataKegg[1]
	names(dataKegg)[1] <- "Name"
	dataKegg[1] <- gsub(" ", "", dataKegg[, 1])
	names(codesKegg)[c(1, 2)] <- c("kegg", "Name")
	codesKegg[2] <- gsub(" ", "", codesKegg[, 2])
	#######
	
	if (addCodes == TRUE) {
		pre.data <- merge(dataKegg, codesKegg, by.x = "Name", by.y = "Name", all.x = TRUE)
		missingCpd <- pre.data[is.na(pre.data$kegg), ]
		if (nrow(missingCpd) == 0){
			print("Every compound in the inputData was found in the keggCodes library.")
		} else {
			print(original.comps[which(gsub(" ", "", original.comps[,1]) %in% missingCpd[,1]),])
			addCPD <- dlgMessage("Some compounds of your input data are not listed in the kegg code library used. Some of these compounds are listed in the R console. Would you like to include them into your kegg library right now?", type = "yesno")$res
			if (addCPD == "yes") {
                if (OSsystem == "Windows"){
                    ## Do nothing
                } else {
                    dlgMessage("This is what is going to happen now: KEGG database will be searched for the missing compounds. For each missing compound, a list of potential matches will be presented to you. If the missing compound is part of the list, select it and its respective KEGG code will be added to your KEGG code library. If the desired compound is not listed, you can go to the end of the list and click on OTHER to manually add its KEGG code, or you can click on SKIP if you have no KEGG code for this compound. Compounds showing no KEGG codes will not be analyzed by PAPi.")
                }
				for (i in 1:nrow(missingCpd)) {
					compToSearch = original.comps[which(gsub(" ", "", original.comps[,1]) == missingCpd[i,1]),]
					potentialCPDS <- keggFind("compound", as.character(compToSearch))
					cpdChosen <- select.list(c(potentialCPDS, "OTHER", "SKIP"), title = as.character(compToSearch))
					if (cpdChosen == "OTHER") {
						cpdChosen <- dlgInput(paste("Enter the KEGG code for ", as.character(compToSearch), 
						" or leave it as absent.", sep = ""), 
						default = "absent")$res
						if (!length(cpdChosen)) {
							cpdChosen <- "absent"
						}
						original.kegglib <- rbind(original.kegglib, c(cpdChosen, as.character(compToSearch)))
					} else {
						if (cpdChosen == "SKIP"){
						
						} else {
							cpdChosen <- potentialCPDS[which(potentialCPDS == cpdChosen)]
							cpdChosen <- names(cpdChosen)
							cpdChosen <- gsub("cpd:", "", cpdChosen)
							original.kegglib <- rbind(original.kegglib, c(cpdChosen, as.character(compToSearch)))
						}
					}
				}
				saveLib <- dlgMessage("Would you like to save your new KEGG library to a CSV file?", type = c("yesno"))$res
				if (saveLib == "yes") {
					placeToSave <- dlgDir(title = "Select the folder where the library will be saved.")$res
					dateForFile <- Sys.time()
					dateForFile <- gsub(" ", "", dateForFile)
					dateForFile <- gsub(":", "", dateForFile)
					fileName <- dlgInput(message = "What is the name of the new library?", default = paste("KEGGLibrary", dateForFile, sep=""))$res
					fileName <- paste(fileName, ".csv", sep = "")
					placeToSave <- paste(placeToSave, FolderDivisor, fileName, sep = "")
					write.csv(original.kegglib, file = placeToSave, row.names = FALSE)
					print("Your new KEGG code library has been saved.")
				}
				codesKegg <- original.kegglib
			}
		}	
	}
	
	#######	
	names(codesKegg)[c(1, 2)] <- c("kegg", "Name")
	codesKegg[2] <- gsub(" ", "", codesKegg[, 2])
	#######
	
	FinalData <- merge(dataKegg, codesKegg, by.x = "Name", by.y = "Name", all.x = TRUE)
	FinalData <- subset(FinalData, !(FinalData$kegg == "absent"))
	FinalData$Name <- FinalData$kegg
	FinalData$kegg <- NULL

	if (rep == 1) {
		FinalData <- rbind(c(replicates), FinalData)
	}

	if (save == TRUE) {
		sheet <- output
		store <- paste(folder, FolderDivisor, sheet, ".csv", sep = "")
		write.csv(FinalData, file = store, row.names = FALSE)
		print(paste("The file ", output, ".csv", " was saved in the folder ", folder, sep = ""))
	} else {
		print("The final data frame was not saved because the argument save was set as FALSE")
	}
	return(FinalData)
}
