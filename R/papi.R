papi <-
function(inputData, save = TRUE, folder, output = "papi_results", offline = TRUE, localDatabase = "default"){
  
  ## Function to check if file is CSV ##
  isCSVdlg <- function(titleMSG, errorMSG){
  	t = 0
  	while (t == 0){
  	checkIfCsv <- dlgOpen(title = titleMSG, multiple = FALSE)$res
    checkIfCsv2 <- basename(checkIfCsv)
    checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
    checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
    	if (checkIfCsv4 %in% c("CSV", "csv")){
    		t = 1
    		return(checkIfCsv)
       	} else {
    		dlgMessage(errorMSG)
  		}
    }
  }
  #########################################################
  
  #### No dialog box ###
  isCSV <- function(pathFile, errorMSG){
  	t = 0
   	checkIfCsv <- pathFile
    checkIfCsv2 <- basename(checkIfCsv)
    checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
    checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
    if (checkIfCsv4 %in% c("CSV", "csv")){
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
  
################### Select database for a local analysis ###############
	online <- 0
	##### OFFLINE and localDatabase = "default" #######
	if(offline == TRUE && localDatabase == "default"){ 		
		#### use the default database that comes with the package ####
		#### Load COMPbase.csv #####
		listOfComps <- try(read.csv(R.home("library/PAPi/databases/default/COMPbase.csv"), colClasses="character"), TRUE)		
		if ("try-error" %in% class(listOfComps)){
			print(class(listOfComps))
			dlgMessage("The compound's database could not be loaded. We will look for additional databases to be used.")$res
			listOfDatabases <- try(list.files(R.home("library/PAPi/databases/")), TRUE)
			if ("try-error" %in% class(listOfDatabases)){
				print(class(listOfDatabases))
				decision1 <- dlgMessage("We could not search for additional databases. The related error is printed on your screen. Press OK to apply PAPi online or CANCEL to stop the function.", type = "okcancel")$res
					if (decision1 == "cancel"){
						stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
					} else {
						online <- 1
					}
			} else {
				baseToUse <- select.list(listOfDatabases)
				baseToUse <- paste(R.home("library/PAPi/databases/"), baseToUse, sep="")
				listOfComps <- try(read.csv(paste(baseToUse, "/", "COMPbase.csv", sep=""), colClasses="character"), TRUE)
				if ("try-error" %in% class(listOfComps)){
					print(class(listOfComps))
					decision2 <- dlgMessage("The compound's database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
					if (decision2 == "cancel"){
						stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
					} else {
						online <- 1
					}
				} else {
					pathname <- try(read.csv(paste(baseToUse, "/", "PATHbase.csv", sep=""), colClasses="character"), TRUE)
					if ("try-error" %in% class(pathname)){
						print(class(pathname))
						decision3 <- dlgMessage("The pathways' database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
						if (decision3 == "cancel"){
							stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
						} else {
							online <- 1
						}
					}
					pathname[2] <- gsub("path:map", "ko", pathname[,2])
				}
			}
		} else {
			pathname <- try(read.csv(R.home("library/PAPi/databases/default/PATHbase.csv"), colClasses="character"), TRUE)

			if ("try-error" %in% class(pathname)){
				print(class(listOfComps))
				dlgMessage("The pathways' database could not be loaded. We will look for additional databases to be used.")$res
				listOfDatabases <- try(list.files(R.home("library/PAPi/databases/")), TRUE)
				if ("try-error" %in% class(listOfDatabases)){
					print(class(listOfDatabases))
					decision1 <- dlgMessage("We could not search for additional databases. The related error is printed on your screen. Press OK to apply PAPi online or CANCEL to stop the function.", type = "okcancel")$res
					if (decision1 == "cancel"){
						stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
					} else {
						online <- 1
					}
				} else {
					baseToUse <- select.list(listOfDatabases)
					baseToUse <- paste(R.home("library/PAPi/databases/"), baseToUse, sep="")
					listOfComps <- try(read.csv(paste(baseToUse, "/", "COMPbase.csv", sep=""), colClasses="character"), TRUE)
					if ("try-error" %in% class(listOfComps)){
						print(class(listOfComps))
						decision2 <- dlgMessage("The compound's database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
						if (decision2 == "cancel"){
							stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
						} else {
							online <- 1
						}
					} else {
						pathname <- try(read.csv(paste(baseToUse, "/", "PATHbase.csv", sep=""), colClasses="character"), TRUE)
						if ("try-error" %in% class(pathname)){
							print(class(pathname))
							decision3 <- dlgMessage("The pathways' database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
							if (decision3 == "cancel"){
								stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
							} else {
								online <- 1
							}
						}
						pathname[2] <- gsub("path:map", "ko", pathname[,2])
					}
				}
			}
			pathname[2] <- gsub("path:map", "ko", pathname[,2])
		}
	}
		###########################################


		##### OFFLINE and localDatabase = "choose" #######
		if(offline == TRUE && localDatabase == "choose"){ 		
			listOfDatabases <- try(list.files(R.home("library/PAPi/databases/")), TRUE)
			if ("try-error" %in% class(listOfDatabases)){
				print(class(listOfDatabases))
				decision1 <- dlgMessage("We could not search for databases. The related error is printed on your screen. Press OK to apply PAPi online or CANCEL to stop the function.", type = "okcancel")$res
					if (decision1 == "cancel"){
						stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
					} else {
						online <- 1
					}
			} else {
				baseToUse <- select.list(listOfDatabases, title = "Which database do you want to use?")
				baseToUse <- paste(R.home("library/PAPi/databases/"), baseToUse, sep="")
				listOfComps <- try(read.csv(paste(baseToUse, "/", "COMPbase.csv", sep=""), colClasses="character"), TRUE)
				if ("try-error" %in% class(listOfComps)){
					print(class(listOfComps))
					decision2 <- dlgMessage("The compound's database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
					if (decision2 == "cancel"){
						stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
					} else {
						online <- 1
					}
				} else {
					pathname <- try(read.csv(paste(baseToUse, "/", "PATHbase.csv", sep=""), colClasses="character"), TRUE)
					if ("try-error" %in% class(pathname)){
						print(class(pathname))
						decision3 <- dlgMessage("The pathways' database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
						if (decision3 == "cancel"){
							stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
						} else {
							online <- 1
						}
					}
					pathname[2] <- gsub("path:map", "ko", pathname[,2])
				}
			}
		} else {
			if (offline == TRUE){
				###### If an specific database is defined #############
				listOfDatabases <- try(list.files(R.home("library/PAPi/databases/")), TRUE)
				if (localDatabase %in% listOfDatabases){
					baseToUse <- paste(R.home("library/PAPi/databases/"), localDatabase, sep="")
					listOfComps <- try(read.csv(paste(baseToUse, "/", "COMPbase.csv", sep=""), colClasses="character"), TRUE)
					if ("try-error" %in% class(listOfComps)){
						print(class(listOfComps))
						decision2 <- dlgMessage("The compound's database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
						if (decision2 == "cancel"){
							stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
						} else {
							online <- 1
						}
					} else {
						pathname <- try(read.csv(paste(baseToUse, "/", "PATHbase.csv", sep=""), colClasses="character"), TRUE)
						if ("try-error" %in% class(pathname)){
							print(class(pathname))
							decision3 <- dlgMessage("The pathways' database could not be loaded. Would you like to do this analysis online ?.", type = "okcancel")$res
							if (decision3 == "cancel"){
								stop("The process was stopped by the user. See ?buildDatabase for creating a local database.")
							} else {
								online <- 1
							}
						}
						pathname[2] <- gsub("path:map", "ko", pathname[,2])
					}
				} else {
					stop("The selected database is not installed. Try to reapply the function using localDatabase = \"choose\". See ?buildDatabase for creating a local database.")
				}
			}
			########################################################
		}
		###########################################

	#### If online analysis ######################
	if(offline == FALSE){	
		###check if there is internet connection #####
		testInternet <- try(keggList("pathway"), TRUE)
		if ("try-error" %in% class(testInternet)){
			stop("We could not connect to KEGG database. You probably have no internet connection. Try to use a local database by reapplying papi using localDatabase = \"choose\".")
		}
		online <- 1
	}

	############### If online = 1, convert offline to FALSE ######
	if (online == 1){
		offline <- FALSE
		print("Online analysis being performed...")
	} else {
		print("Offline analysis being performed...")
	}
	##############################################################
	
	################## Start PAPi #############  
	######## Begin collecting arguments #######
  	print("PAPi in progess...")
  	if (missing(inputData)){
  		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    	omics.data.frame <- read.csv(inputData, colClasses = "character")
    	print("Input file loaded...")
  	} else {
  		if (is.data.frame(inputData) == TRUE){
  			omics.data.frame <- inputData
  			print("Data frame loaded...")	  		
  		} else {
  			if (is.character(inputData)){
  				checkIfCsv <- isCSV(inputData)
  				if (checkIfCsv == 1){
  					inputTest <- file.access(inputData, 0)
      				if(inputTest == 0){
          				omics.data.frame = read.csv(inputData, colClasses = "character")
          			} else {
          				print("The input file specified is not accessible. Please, choose a valid CSV file to be used as input data.")
          				inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    					omics.data.frame <- read.csv(inputData, colClasses = "character")
    					print("Input file loaded...")
    				}
    			} else {
    				print("The input file specified is not in CSV format. Please, choose a valid CSV file to be used as input data.")
          			inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    				omics.data.frame <- read.csv(inputData, colClasses = "character")
    				print("Input file loaded...")
    			}
    		} else {
    			print("The path to the input data must be specified as character string. Please, choose a valid CSV file to be used as input data.")
    			inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    			omics.data.frame <- read.csv(inputData, colClasses = "character")
    			print("Input file loaded...")
    		}
    	}
 	}

  if (save == TRUE) {
  	if (missing(folder)){
  		print("No folder was defined to save the results.")
        print("Please, point to the folder where the results should be saved.")
        folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
  	} else {
  		if (is.character(folder)){
			isFolder <- file.access(as.character(folder), 0)
  			if (isFolder == 0){
      			isFolder <- file.info(folder)
        			if (isFolder$isdir != TRUE){
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
  ## Finish collecting arguments ##
  if (omics.data.frame[1, 1] %in% c("Replicates", "Replicate", "replicates", "replicate")) {
    replicates <- as.character(omics.data.frame[1, ])
    omics.data.frame <- omics.data.frame[-1, ]
    rep <- 1
    reps <- factor(replicates[-1])
  } else {
    rep <- 0
  }
  ################################################################  
  ############## If offlineAnalysis <- FALSE #####################
  ################################################################
  	if (online == 1){
  		## Function to go to KEGG and collect pathways related to each compound
  		getcomp <- function(x){
    		comp <- try(keggGet(x), TRUE)
			if("try-error" %in% class(comp)){
				comp <- 0
    		} else {
				comp <- comp[[1]]$PATHWAY
    			comp <- data.frame(comp)
    			names(comp)[1] <- "value"
			}
    		return(comp)
  		}
  		##
  		## Function to go to KEGG and collect compounds playing part in each pathway
  		numbcomp <- function(x){
   		 	compounds <- try(keggGet(x), TRUE)
			if("try-error" %in% class(compounds)){
				compounds <- 0
			} else {
   	 			compounds <- data.frame(compounds[[1]]$COMPOUND)
    			names(compounds)[1] <- "value"
    			compounds <- nrow(compounds)
			}
    		return(compounds)
  		}
		pathname <- data.frame(keggList("pathway"))
		pathname[2] <- row.names(pathname)
		row.names(pathname) <- 1:nrow(pathname)
		names(pathname)[c(1,2)] <- c("pathwayname", "idpathway")
		pathname[2] <- gsub("path:map", "ko", pathname[,2])		
	} else {
		## Function to go to KEGG and collect pathways related to each compound
  		getcomp <- function(x){
    		comp <- which(listOfComps[2] == x)
    		if (length(comp) > 0){
				comp <- listOfComps[comp[1], 3]
				comp <- gsub("map", "ko", comp)
				comp <- data.frame(unlist(strsplit(comp, ";")))
				pathwaysFound <- which(pathname[,2] %in% comp[,1])
				if (length(pathwaysFound) > 0){
					pathwaysFound <- pathname[pathwaysFound,]
					row.names(pathwaysFound) <- pathwaysFound[,2]
					pathwaysFound <- pathwaysFound[1]
    				names(pathwaysFound)[1] <- "value"
				} else {
					pathwaysFound <- 0
				}
  			} else {
				pathwaysFound <- 0
			}
			return(pathwaysFound)
		}
  		##
  		## Function to go to KEGG and collect compounds playing part in each pathway
  		numbcomp <- function(x){
   		 	compounds <- which(pathname[2] == x)
			if (length(compounds) > 0){
				compounds <- as.numeric(pathname[compounds[1], 3])
			} else {
				compounds <- 0
			}
			return(compounds)
		}
	}

	## Apply PAPi for each column of the input data
  	## Starts the progress bar
  	pb <- txtProgressBar(min = 0, max = ncol(omics.data.frame), style = 3, width=50)
  	papi.frame <- numeric()
  	for(j in 2:ncol(omics.data.frame)){
  		Sys.sleep(0.01)
    	setTxtProgressBar(pb, j)
    	## Prepare the list of compunds that will be searched in KEGG
    	data.df <- omics.data.frame[,c(1,j)]
    	data.df <- subset(data.df, !is.na(data.df[2]))
    	complist <- as.character(data.df[,1])
    	complist <- gsub("C", "cpd:C", complist, ignore.case = TRUE)
    	## Give to pathways the abundance of the related metabolite
    	getpath.final <- numeric()
    	for (i in 1:length(complist)){
      		getpath <- getcomp(complist[i])
      		if (getpath != 0){
        		if(length(getpath.final) == 0){
          			getpath$rate <- as.numeric(data.df[i,2])
          			getpath.final <- getpath
        		} else {
          			getpath$rate <- as.numeric(data.df[i,2])
          			getpath.final <- rbind(getpath, getpath.final)
        		}
      		} else {
      		}
    	}

    	## Calculate rates ###
    	getpath.final$rate <- as.numeric(as.character(getpath.final$rate))
    	res.arr <- with(getpath.final, tapply(rate, value, sum))
    	res.df <- data.frame(pathwayname = names(res.arr), rate = res.arr)
    	rownames(res.df) <- NULL
    	pathwayfreq <- data.frame(table(getpath.final$value))
    	names(pathwayfreq)[1] <- "pathwayname"
		names(pathname)[c(1,2)] <- c("pathwayname", "idpathway")
    	freqname <- merge(pathwayfreq, pathname)
    	freqname$idpathway <- gsub("path:map", "ko", freqname$idpathway)
    	total <- nrow(pathwayfreq)
    	## Starts the final calculations - Normalization
    	selecrow.final <- numeric()
    	for (i in 1:nrow(res.df)){
      		selecrow <- freqname[i,]
      		numberCompounds <- try(numbcomp(selecrow$idpathway), silent = TRUE)
      		if ("try-error" %in% class(numberCompounds)){
    			selecrow$percentage <- 1
      		} else {
      			if (numberCompounds != 0){
      				selecrow$percentage <- (selecrow$Freq/numberCompounds)
      			} else {
      				selecrow$percentage <- 1
      			}
      			if(i == 1){
       		 		selecrow.final <- selecrow
      			} else {
       		 		selecrow.final <- rbind(selecrow.final, selecrow)
      			}
      		}
    	}
		if (online == 1){
    		selecrow.final <- selecrow.final[,-c(2,3)]
		} else {
			selecrow.final <- selecrow.final[,-c(2,3,4)]
		}
    	final2.df <- merge(selecrow.final, res.df)
    	final2.df[2] <- final2.df$rate/final2.df$percentage
    	names(final2.df)[2] <- names(data.df)[2]
    	final2.df[3] <- NULL
    	if(j == 2){
      		papi.frame <- final2.df
    	} else {
      		papi.frame <- merge(papi.frame, final2.df, all = TRUE)
    	}
  	}
  	close(pb)
  	## Finish PAPi algorithm
  	papi.frame <- papi.frame[order(papi.frame[,2], decreasing = T),]
  	papi.frame[1] <- as.character(papi.frame[,1])
  	if (rep == 1) {
    	papi.frame <- rbind(c(replicates), papi.frame)
  	}
  	## Save it in the specified folder
  	if (save == TRUE) {
    	sheet <- output
    	store <- paste(folder, FolderDivisor, sheet, ".csv", sep = "")
    	write.csv(papi.frame, file = store, row.names = FALSE)
    	print(paste("The file ", output, ".csv", " was saved in the folder ", folder, sep=""))   
  	} else {
    	print("No file was saved because the argument save was set as FALSE")
  	}
  	return(papi.frame)
}
