papiLine <-
function (inputData, relative = TRUE, save=TRUE, folder, output = "papi_graph", setRef.cond = FALSE, Ref.cond, color = "auto",
    cex.xlab, cex.ylab = 1, position.ylab, margins = c(25, 8, 2, 2), yscale, dot.size = 1.5, legend.position, cex.legend = 1,
    graphWidth=3000, graphHeight=2000, graph.bg="transparent", graph.res = 300)
{ 
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
    
## Begin collecting arguments
if (missing(inputData)){
  	inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    PAPidata <- read.csv(inputData, colClasses = "character")
    print("Input file loaded...")
  } else {
  	if (is.data.frame(inputData) == TRUE){
  		PAPidata <- inputData
  		print("Data frame loaded...")	  		
  	} else {
  		if (is.character(inputData)){
  			checkIfCsv <- isCSV(inputData)
  			if (checkIfCsv == 1){
  				inputTest <- file.access(inputData, 0)
      			if(inputTest == 0){
          			PAPidata = read.csv(inputData, colClasses = "character")
          		} else {
          			dlgMessage("The input file specified is not accessible. Please, choose a valid CSV file to be used as input data.")
          			inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    				PAPidata <- read.csv(inputData, colClasses = "character")
    				print("Input file loaded...")
    			}
    		} else {
    			dlgMessage("The input file specified is not in CSV format. Please, choose a valid CSV file to be used as input data.")
          		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    			PAPidata <- read.csv(inputData, colClasses = "character")
    			print("Input file loaded...")
    		}
    	} else {
    		dlgMessage("The path to the input data must be specified as character string. Please, choose a valid CSV file to be used as input data.")
    		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
    		PAPidata <- read.csv(inputData, colClasses = "character")
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
  		if (is.character(inputData)){
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


    if (PAPidata[1, 1] %in% c("Replicates", "Replicate", "replicates", "replicate")) {
        replicates <- as.character(PAPidata[1, ])
        PAPidata <- PAPidata[-1, ]
        rep <- 1
        reps <- factor(replicates[-1])
    } else {
    	# Indicate replicates
    	dlgMessage("For a faster process of pipe.line, the first row of the input data can be used to indicate which experimental condition each sample bellongs to. In the csv file, just add a new row below the header and fill it with the following information: the word 'Replicates' in the first column and the name of the experiemntal condition correponding to each sample (or replicate) in the following columns. See data(papi.input) for an example of input data.")
    	PAPidata <- rbind(c("Replicates", 2:ncol(PAPidata)), PAPidata)
    	conditions <- as.numeric(dlgList(1:1000, title="How many experimental conditions?")$res)
    	samples <- names(PAPidata)
    	for (k in 1:conditions){
    		cond <- dlgList(samples, multiple = TRUE, title = paste("Which samples bellong to experimental condition", k))$res
    		cond <- which(samples %in% cond)
    		PAPidata[1,cond] <- k
    	}
    	replicates <- as.character(PAPidata[1, ])
    	PAPidata <- PAPidata[-1, ]
        rep <- 1
        reps <- factor(replicates[-1])
    }

names(PAPidata)[1] <- "pathwayname"


## Finish collecting arguments

##

## Building graph
    yMat <- PAPidata[-1]
    ## Build average colum
    avg <- t(apply(yMat, 1, function(y) tapply(as.numeric(y), reps, function(x) mean(x, na.rm = TRUE))))
    if (nlevels(reps) == 1){
      avg <- as.data.frame(t(avg))
    } else {
      avg <- as.data.frame(avg)
    }
    row.names(avg) <- PAPidata$pathwayname
    ## Start the scale invert process
    if (nlevels(reps) > 1){
    	if (relative == TRUE){
    		if (setRef.cond == TRUE){
    			if (missing(Ref.cond)){
    				ref.condition <- dlgList(levels(reps), title = "Please, select the condition to be used as reference.")$res
    				ref.condition <- which(levels(reps) == as.character(ref.condition))
    			} else {
    				ref.condition <- which(levels(reps) == as.character(Ref.cond))
    				if (length(ref.condition) == 0){
    					dlgMessage(paste("The condition specified as reference (", Ref.cond, ") was not found.", sep=""))
    					ref.condition <- dlgList(levels(reps), title = "Please, select the condition to be used as reference.")$res    					
	    				ref.condition <- which(levels(reps) == as.character(ref.condition))										    					
    				}
    			}
    		} else {
    			ref.condition <- 1
    		}
    		avg <- avg[c(ref.condition, which(levels(reps) != levels(reps)[ref.condition]))]
    		for (j in 2:ncol(avg)){
    			for (i in 1:nrow(avg)){
    				if (!is.na(avg[i, 1])){
      					if (!is.na(avg[i,j])){
							if(avg[i, 1] < avg[i,j]){
      							avg[i,j] <- -(avg[i,j]/avg[i, 1])
      						} else {
								avg[i,j] <- (avg[i,j]/avg[i, 1])
							}
						}
					}
				}
			}
			avg <- avg[order(avg[2], decreasing=TRUE),]
			avg.g <- avg
			avg.g[1] <- 0 
    		avg.g[is.na(avg.g)] <- 0
      } else {
      	 avg <- avg[order(avg[1], decreasing=TRUE),]
      	 avg.g <- avg
      }
   } else {
    	names.row <- row.names(avg)
      	lines.new <- order(avg, decreasing=TRUE)
      	lines.new2 <- data.frame(avg[lines.new,])
      	lines.new2 <- cbind(names.row[c(lines.new)], lines.new2)
      	names(lines.new2) <- c("pathwayname", names(avg))
      	row.names(lines.new2) <- lines.new2$pathwayname
      	lines.new2$pathwayname <- NULL
      	avg <- lines.new2 
    	print("The argument relative is only applied for 2 conditions or more.")
    	relative <- FALSE
    	avg <- data.frame(avg)
		avg.g <- avg
    }

#### Geting the max and min values for ylim ####
if (missing(yscale)){
	if (relative == TRUE && (nlevels(reps) > 1)){
		MaxValue <- max(apply(avg.g[-1], 2, function(x) max(x, na.rm = TRUE)), na.rm = TRUE)
		MinValue <- min(apply(avg.g[-1], 2, function(x) min(x, na.rm = TRUE)), na.rm = TRUE)
		if (MaxValue > 0){
			if (round(MaxValue) <= MaxValue){
				MaxValue <- MaxValue + 0.5
			}
		}
		if (MaxValue < 0){
			if (round(MaxValue) >= MaxValue){
				MaxValue <- MaxValue - 0.5
			}
		}
		if (MinValue > 0){
			if (round(MinValue) <= MinValue){
				MinValue <- MinValue - 0.5
			}
		}
		if (MinValue < 0){
			if (round(MinValue) >= MinValue){
				MinValue <- MinValue - 0.5
			}
		}
		if (MaxValue >= 0 && MinValue >= 0){
			yscale = c(0,round(MaxValue))
		}
		if (MaxValue <= 0 && MinValue <= 0){
			yscale = c(round(MinValue),0)
		}
		if (MaxValue >= 0 && MinValue <= 0){
			yscale = c(round(MinValue),round(MaxValue))
		}
		if (MaxValue <= 0 && MinValue >= 0){
			yscale = c(round(MinValue),round(MaxValue))
		}
	} else {
		MaxValue <- max(apply(avg.g, 2, function(x) max(x, na.rm = TRUE)), na.rm = TRUE)
		MinValue <- min(apply(avg.g, 2, function(x) min(x, na.rm = TRUE)), na.rm = TRUE)
		if (MaxValue > 0){
			if (round(MaxValue) <= MaxValue){
				MaxValue <- MaxValue + 0.5
			}
		}
		if (MaxValue < 0){
			if (round(MaxValue) >= MaxValue){
				MaxValue <- MaxValue - 0.5
			}
		}
		if (MinValue > 0){
			if (round(MinValue) <= MinValue){
				MinValue <- MinValue - 0.5
			}
		}
		if (MinValue < 0){
			if (round(MinValue) >= MinValue){
				MinValue <- MinValue - 0.5
			}
		}
		if (MaxValue >= 0 && MinValue >= 0){
			yscale = c(0,round(MaxValue))
		}
		if (MaxValue <= 0 && MinValue <= 0){
			yscale = c(round(MinValue),0)
		}
		if (MaxValue >= 0 && MinValue <= 0){
			yscale = c(round(MinValue),round(MaxValue))
		}
		if (MaxValue <= 0 && MinValue >= 0){
			yscale = c(round(MinValue),round(MaxValue))
		}
		# MaxValue <- max(apply(avg.g, 2, function(x) max(x, na.rm = TRUE)), na.rm = TRUE)
		# MinValue <- max(apply(avg.g, 2, function(x) min(x, na.rm = TRUE)), na.rm = TRUE)
		# yscale = c(round(MinValue),round(MaxValue))	
	}
}

#### CEX.XLAB - Size of the characters of X axis ####
if (missing(cex.xlab)){
	ncharacter <- data.frame(row.names(avg))
	ncharacter <- data.frame(apply(ncharacter, 1, function(x) nchar(x)))
	ncharacter <- max(ncharacter)
	npathways <- nrow(avg)
	if (npathways > 35){
		cex.xlab <- 0.8
	} else {
		if (ncharacter %in% c(1:40)){
			cex.xlab <- 1.5
		}
		if (ncharacter %in% c(41:55)){
			cex.xlab <- 1
		}
		if (ncharacter > 56){
			cex.xlab <- 0.8
		}
	}
}

##### Defining Colors ####
	if (color[1] == "auto"){
		color <- colors()[30*c(1:nlevels(reps))]
	}
	if (color[1] == "manual"){
		color <- c()
		for (i in 1:nlevels(reps)){
			color <- c(color, dlgList(colors(), title = paste("Choose the color to represent the condition", levels(reps)[i], sep = " "))$res)
		}
	}
	
#### Position YLab ####
if (missing(position.ylab)){
	position2 <- (abs(yscale[2])+abs(yscale[1]))/2
	if (yscale[2] >= 0 && yscale[1] >= 0){
		position2 <- position2
	}
	if (yscale[2] <= 0 && yscale[1] <= 0){
		position2 <- -position2
	}
	if (yscale[2] >= 0 && yscale[1] <= 0){
		position2 <- (yscale[2]+abs(yscale[1]))/2
		position2 <- yscale[2] - position2
	}
	if (nrow(avg) >= 45){
		position1 <- -5		
	}
	if (nrow(avg) %in% c(36:44)){
		position1 <- -4		
	}
	if (nrow(avg) %in% c(26:35)){
		position1 <- -3		
	}
	if (nrow(avg) %in% c(16:25)){
		position1 <- -2		
	}
	if (nrow(avg) %in% c(1:15)){
		position1 <- -1		
	}
	position.ylab = c(position1,position2)
}

###### Legend ########
if (missing(legend.position)){
	if (relative == FALSE){
		legend.position <- "topright"
	} else {
		if (yscale[1] < 0){
			if (!(min(apply(avg.g[(1:(nrow(avg.g)/2)),], 2, function(x) min(x, na.rm = TRUE)), na.rm = TRUE) <= (yscale[1]+1))){
				legend.position <- "bottomleft"
			} else {
				if (!(min(apply(avg.g[((nrow(avg.g)/2):nrow(avg.g)),], 2, function(x) min(x, na.rm = TRUE)), na.rm = TRUE) <= (yscale[1]+1))){
					legend.position <- "bottomright"
				} else {
					legend.position <- "topright"
				}
			}
		}
		if (yscale[1] >= 0){
			if (!(max(apply(avg.g[(1:(nrow(avg.g)/2)),], 2, function(x) max(x, na.rm = TRUE)), na.rm = TRUE) <= (yscale[2]-1))){
				legend.position <- "topleft"
			} else {
				if (!(max(apply(avg.g[((nrow(avg.g)/2):nrow(avg.g)),], 2, function(x) max(x, na.rm = TRUE)), na.rm = TRUE) <= (yscale[2]-1))){
					legend.position <- "topright"
				} else {
					legend.position <- "topright"
				}
			}
		}
	}	
}


#############################
##### If only one condition ###
if (nlevels(reps) == 1){
	if (save == TRUE){
		sheet <- paste(output, ".png", sep = "")
		store <- paste(folder,  FolderDivisor, sheet, sep="")
		png(store, width = graphWidth, height = graphHeight, bg = graph.bg, res = graph.res)
		par(mar=margins + 0.1, xpd = TRUE)
	} else {
		par(mar=margins - c(5,2,0,0), xpd = TRUE)
		cex.legend = cex.legend - 0.3	
	}
	plot(replicate(nrow(avg),0), type="l", lwd = 3, col=color[1], axes=FALSE, ann=FALSE, lty=1, ylim=yscale)
		for (p in 1:nrow(avg)){		
			points(p,as.matrix(avg[p,1]), col=color[1], type="h")
		}
	lines(as.matrix(avg[1]), type="l", lwd = 3, pch=19, lty=1, col=color[1])
	axis(1, at=1:nrow(avg), labels=row.names(avg), las=2, hadj=1, cex.axis=cex.xlab, tck=-.01)
    	axis(2, las=1,cex.axis=1, lwd.ticks = 1)
    if (save == TRUE){
		text(position.ylab[1]-2, position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)
	} else {
		text(position.ylab[1]-4, position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)		
	}
	if (length(legend.position) == 2){
		legend(legend.position[1], legend.position[2], names(avg), cex=cex.legend, col=color[1:ncol(avg)],
	lty = c(1,1,0,0), title="Conditions", lwd = 3, merge=FALSE)
	} else {
		legend(legend.position, names(avg), cex=cex.legend, col=color[1:ncol(avg)],
	lty = c(1,1,0,0), title="Conditions", lwd = 3, merge=FALSE)
	}
	if (save == TRUE){
	   graphics.off()
	   print(paste("The file ", output, ".png ", "has been saved in ", folder, sep =""))
	}
}

#############################
##### If only two conditions ###
if (nlevels(reps) == 2){
	if (save == TRUE){
		sheet <- paste(output, ".png", sep = "")
		store <- paste(folder,  FolderDivisor, sheet, sep="")
		png(store, width = graphWidth, height = graphHeight, bg = graph.bg, res = graph.res)
		par(mar=margins + 0.1, xpd = TRUE)
	} else {
		par(mar=margins - c(5,2,0,0), xpd = TRUE)
		cex.legend = cex.legend - 0.3	
	}
	par(mar=margins - c(5,2,0,0), xpd = TRUE)
	plot(as.matrix(avg.g[1]), type="l", lwd = 3, col=color[1], axes=FALSE, ann=FALSE, lty=1, ylim=yscale)
		for (p in 1:nrow(avg)){		
			if (is.na(avg[p,1])){
				points(p,0, col=color[1], type="o", pch = 19, cex = dot.size)
			}
			if (is.na(avg[p,2])){
				points(p,0, col=color[2], type="o", pch = 19, cex = dot.size)
			}		
		}
		lines(as.matrix(avg[2]), type="l", lwd = 3, pch=19, lty=1, col=color[2])
		for (p in 1:nrow(avg)){		
			points(p,as.matrix(avg[p,2]), col=color[2], type="h")
		}
	axis(1, at=1:nrow(avg), labels=row.names(avg), las=2, hadj=1, cex.axis=cex.xlab, tck=-.01)
    	axis(2, las=1,cex.axis=1, lwd.ticks = 1)
	if (save == TRUE){
		text(position.ylab[1], position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)
	} else {
		text(position.ylab[1]-2, position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)		
	}
	if (length(legend.position) == 2){
		legend(legend.position[1], legend.position[2], c(names(avg), paste("Not present in", names(avg)[1]),
		paste("Not present in", names(avg)[2])), cex=cex.legend, col=c(color[1:ncol(avg)], color[1:ncol(avg)]),
		lty = c(1,1,0,0), pch=c(NA,NA,19,19), title="Conditions", lwd = 3, merge=FALSE)
	} else {
		legend(legend.position, c(names(avg), paste("Not present in", names(avg)[1]),
		paste("Not present in", names(avg)[2])), cex=cex.legend, col=c(color[1:ncol(avg)], color[1:ncol(avg)]),
		lty = c(1,1,0,0), pch=c(NA,NA,19,19), title="Conditions", lwd = 3, merge=FALSE)
	}
    if (save == TRUE){
    	graphics.off()
    	print(paste("The file ", output, ".png ", "has been saved in ", folder, sep =""))
    }
}
#############################
##### If more than two conditions ###
if (nlevels(reps) > 2){
	if (save == TRUE){
		sheet <- paste(output, ".png", sep = "")
		store <- paste(folder,  FolderDivisor, sheet, sep="")
		png(store, width = graphWidth, height = graphHeight, bg = graph.bg, res = graph.res)
		par(mar=margins + 0.1, xpd = TRUE)
	} else {
		par(mar=margins - c(5,2,0,0), xpd = TRUE)
		cex.legend = cex.legend - 0.3	
	}
	plot(as.matrix(avg.g[1]), type="l", lwd = 3, col=color[1], axes=FALSE, ann=FALSE, lty=1, ylim=yscale)
		for (j in 1:ncol(avg.g)){
			for (p in 1:nrow(avg)){		
				if (is.na(avg[p,j])){
					points(p,0, col=color[j], type="o", pch = 19, cex = dot.size)
				}		
			}
		}
		# for (p in 1:nrow(avg)){	
			# find.na <- t(is.na(avg[p,]))
			# find.na <- subset(find.na, find.na[1] == "TRUE")
				# if(nrow(find.na) > 0){
					# if(find.na[1,1] == "TRUE"){ 	
						# points(p,0, col=color[1], type="o", pch = 19, cex = dot.size)
					# }		
				# }
		# }
		for (i in 2:ncol(avg)){
			lines(as.matrix(avg[i]), type="l", lwd = 3, pch=19, lty=1, col=color[i])
			for (p in 1:nrow(avg)){		
				points(p,as.matrix(avg[p,i]), col=color[i], type="h")
			}
		}
	axis(1, at=1:nrow(avg), labels=row.names(avg), las=2, hadj=1, cex.axis=cex.xlab, tck=-.01)
    	axis(2, las=1,cex.axis=1, lwd.ticks = 1)
	if (save == TRUE){
		text(position.ylab[1], position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)
	} else {
		text(position.ylab[1]-3, position.ylab[2], labels = "Activity Score (AS)", cex = 1.5, srt = 90)		
	}
	if (length(legend.position) == 2){
		legend(legend.position[1], legend.position[2], c(names(avg)),
      	cex=cex.legend, col=c(color[1:ncol(avg)], color[1]), lty = c(replicate(ncol(avg), 1), 0), 
		pch=c(replicate(ncol(avg), NA)), title="Conditions", lwd = 3, merge=FALSE)
	} else {
		legend(legend.position, c(names(avg)),
      	cex=cex.legend, col=c(color[1:ncol(avg)], color[1]), lty = c(replicate(ncol(avg), 1), 0), 
		pch=c(replicate(ncol(avg), NA)), title="Conditions", lwd = 3, merge=FALSE)
	}
	if (save == TRUE){
   		graphics.off()
   		print(paste("The file ", output, ".png ", "has been saved in ", folder, sep =""))
	}
}
#############################
}
