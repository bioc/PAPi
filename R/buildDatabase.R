buildDatabase <- function(save = TRUE, folder, saveAs){
	### Test if internet is available ###
	testInternet <- try(keggList("pathway"), TRUE)
	if ("try-error" %in% class(testInternet)){
		stop("We could not connect to KEGG database. You probably have no internet connection.")
	}
	###########################
	#### Check if the OS is Widnows ###
     OSsystem <- Sys.info()["sysname"]
     if (OSsystem == "Windows"){
        FolderDivisor <- "\\"
     } else {
        FolderDivisor <- "/"
     }
	

	### Apply with progress bar ####
	apply_pb <- function(X, MARGIN, FUN, ...){
  		env <- environment()
  		pb_Total <- sum(dim(X)[MARGIN])
  		counter <- 0
  		pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
			wrapper <- function(...){
    				curVal <- get("counter", envir = env)
    				assign("counter", curVal +1 ,envir= env)
    				setTxtProgressBar(get("pb", envir= env), curVal +1)
				FUN(...)
			}
  		res <- apply(X, MARGIN, wrapper, ...)
  		close(pb)
  		res
	}

	#### saveAs function ####
	NameToSave <- function(){
		dateForFile <- Sys.time()
		dateForFile <- gsub(" ", "", dateForFile)
		dateForFile <- gsub(":", "", dateForFile)
		NameFile <- dlgInput(message = "What is the name of the new database?", default = paste("KEGGDatabase", dateForFile, sep=""))$res
		return(NameFile)
	}

	### Collecting arguments ####
	error1 <- "The folder defined to save the database is not a valid path."
	error2 <- "Please, point to the folder where the database should be saved."
	titleDLG <- "Select the folder where the new database will be saved."
	if (save == TRUE) {
		if (missing(folder)){
			print(error1)
			print(error2)
			folder = dlgDir(title = titleDLG)$res
			if (missing(saveAs)){
				fileName <- NameToSave()
			} else {
				fileName <- saveAs
			}
		} else {
			if (is.character(folder)){
				isFolder <- file.access(as.character(folder), 0)
				if (isFolder == 0){
					isFolder <- file.info(folder)
					if (isFolder$isdir != TRUE){
						print(error1)
						print(error2)
						folder = dlgDir(title = titleDLG)$res
						if (missing(saveAs)){
							fileName <- NameToSave()
						} else {
							fileName <- saveAs
						}
					}
    				} else {
						print(error1)
      					print(error2)
      					folder = dlgDir(title = titleDLG)$res
    					if (missing(saveAs)){
						fileName <- NameToSave()
					} else {
						fileName <- saveAs
					}
					}
    			} else {
    				print("The path to the folder where the results will be saved must be specified as character.")
          			print(error2)
          			folder = dlgDir(title = titleDLG)$res    		
    			}
    		}
  	} else {
		if (missing(saveAs)){
			fileName <- NameToSave()
		} else {
			fileName <- saveAs
		}
	}
	### Building KEGG database ####

	##### Collect the list of compounds available in KEGG ###
	
	##### Function to get the pathways related to each compound ###
	getPathways <- function(x){
		TotalReport <- keggGet(x)
		pathways <- TotalReport[[1]]$PATHWAY
		pathways <- data.frame(pathways)
		pathways <- row.names(pathways)
		pathways <- paste(pathways, collapse = ";")
		return(pathways)
	}
	####################################################

	listOfComps <- keggList("compound")
	listOfComps <- data.frame(listOfComps)
	listOfComps[2] <- row.names(listOfComps)
	row.names(listOfComps) <- 1:nrow(listOfComps)
	listOfComps[3] <- apply_pb(listOfComps[2], 1, function(x) getPathways(x))
	
	### Prepare the heading of the file COMPbase ####
	 FileName <- "COMPbase"
	 dateForFile <- Sys.time()
	 dateForFile <- gsub(" ", "", dateForFile)
	 dateForFile <- gsub(":", "", dateForFile)
	 userName <- Sys.info()
	 userName <- userName[[7]]
	 names(listOfComps)[c(1,2,3)] <- c(FileName, dateForFile, userName)


	### Build list of pathways with their IDs and the number of compounds in each pathway ###
	
	#### Function to get the number of compounds in each pathway ####
	numbcomp <- function(x){
		compounds <- keggGet(x)
		compounds <- data.frame(compounds[[1]]$COMPOUND)
		compounds <- nrow(compounds)
		if (compounds == 0){
			x <- gsub("path:map", "ko", x, fixed = TRUE)
			compounds <- try(keggGet(x), silent = TRUE)
			if("try-error" %in% class(compounds)){
				compounds <- 0
			} else {
				compounds <- data.frame(compounds[[1]]$COMPOUND)
				compounds <- nrow(compounds)
			}
		}
		return(compounds)
	}
	
	pathname <- data.frame(keggList("pathway"))
	pathname[2] <- row.names(pathname)
	row.names(pathname) <- 1:nrow(pathname)
	pathname[3] <- apply_pb(pathname[2], 1, function(x) numbcomp(x))
	FileName <- "PATHbase"
	dateForFile <- Sys.time()
	dateForFile <- gsub(" ", "", dateForFile)
	dateForFile <- gsub(":", "", dateForFile)
	userName <- Sys.info()
	userName <- userName[[7]]
	names(pathname)[c(1,2,3)] <- c(FileName, dateForFile, userName)


	### Installing database ####
	dir.create(paste(R.home("library/PAPi/databases/"),fileName, sep=""), recursive = TRUE)
	placeToSave <- paste(R.home("library/PAPi/databases/"),fileName, "/COMPbase.csv", sep="")
	write.csv(listOfComps, file = placeToSave, row.names = FALSE)
	placeToSave <- paste(R.home("library/PAPi/databases/"),fileName, "/PATHbase.csv", sep="")
	write.csv(pathname, file = placeToSave, row.names = FALSE)
	print("Your new KEGG database has been installed.")
	
	### Saving to folder ####
	if (save == TRUE){
		### Create folder in folder ###
		dir.create(paste(folder, fileName, sep=""))
		placeToSave <- paste(folder, fileName, FolderDivisor, "COMPbase.csv", sep="")
		write.csv(listOfComps, file = placeToSave, row.names = FALSE)
		placeToSave <- paste(folder, fileName, FolderDivisor, "PATHbase.csv", sep="")
		write.csv(pathname, file = placeToSave, row.names = FALSE)
		print("Your new KEGG database has been saved.")
	}
}