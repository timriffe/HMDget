HMDget <-
function(countries=NULL,wanteditems=NULL,years=TRUE,column=NULL,drop.tadj=TRUE,format=0,path=NULL,username,password,syntax=TRUE){
# by Tim Riffe, with several chunks by Carl Boe.
# I ) definine internally used functions:

#####################################################################################
# selectyears scans for available years, prints available ranges for each country to the console,
# and brings up a selection list. This is called once the data is already in R, as a first step in
# reducing it just to desired years. Produces object 'SelectedYears'
#A <- output
selectyears <- function(A){
	Countries           <- names(A)
	Variables           <- names(A[[Countries[1]]])
	SelectedYears       <- list()
	
	# loop through variables
	for (i in 1:length(Variables)){
		rangemati               <- matrix(nrow = 2, ncol = length(Countries))
		colnames(rangemati)     <- Countries
		rownames(rangemati)     <- c("firstyear", "latestyear")
		
		# for non-numeric year expressions (ranges)
		variuniques             <- c()
		
		# loop through Countries
		years.in.Countries.i    <- c()
		for (j in 1:length(Countries)){
			rangemati[, j]       <- range(A[[Countries[j]]][[Variables[i]]][, "Year"])
			if (is.numeric(rangemati[,j])) {
				by              <- diff(unique(A[[Countries[j]]][[Variables[i]]][, "Year"])[1:2])
			} else {
				variuniques     <- c(variuniques, unique(A[[Countries[j]]][[Variables[i]]][, "Year"]))
			}
			years.in.Countries.i <- c(years.in.Countries.i, unique(A[[j]][[i]]$Year))
		}
		
		# those items common to all countries will have table values equal to length(Countries)
		CT                      <- table(years.in.Countries.i)
		years.in.all.Countries  <- sort(names(CT[CT == length(Countries)]))
		years.in.any.Country    <- as.character(sort(unique(years.in.Countries.i)))
		
		# if they're the same length, then we're good to go
		if(length(years.in.all.Countries) == length(years.in.any.Country)){
			wantedyears     <- select.list(years.in.any.Country, preselect = NULL, multiple = TRUE, title = paste("Select Years,", Variables[i]))
		} else {
			# if not, then we ask which list to show:
			cat(paste("\n the countries selected have different years available for", Variables[i], ".\n do you want to see a list of years in ANY of your countries or ALL of them?\n"))
			ANYALL          <- select.list(c("in ANY selected country", "in ALL selected countries"), preselect = "in ALL selected countries", multiple = FALSE, title = paste("Show which years", Variables[i],"?"))
			if (ANYALL == "in ANY selected country"){
				# some extra info to print
				cat(paste("\nThis is the full range of years available for", Variables[i], "in each country:\n"))
				cat(paste("If you select years from the list that are not in all countries requested, then output will default to format style 0.\n"))
				rangemati[is.infinite(rangemati)] <- NA
				print(rangemati)
				wantedyears <- select.list(years.in.any.Country, preselect = NULL, multiple = TRUE, title = paste("Select Years,", Variables[i]))
			} else {
				wantedyears <- select.list(years.in.all.Countries, preselect = NULL, multiple = TRUE, title = paste("Select Years,", Variables[i]))
			}
		}
		SelectedYears[[Variables[i]]] <- wantedyears
	}
	return(SelectedYears)
}

#####################################################################################
# parse years is the second step step. it takes the SelectedYears list object and then parses the
# the dataframes to include just desired years.
parseyears <- function(A, SelectedYears){
	Countries           <- names(A)
	Variables           <- names(SelectedYears)
	for (i in 1:length(Countries)){
		for (j in 1:length(Variables)){
			keepers     <- A[[i]][[j]]$Year %in% SelectedYears[[j]]
			A[[i]][[Variables[j]]] <- A[[i]][[Variables[j]]][keepers, ] 
		}
	}
	return(A)
}

#####################################################################################
# a checker function in case data.frame is TRUE. If items are to be coerced into a data.frame
# it only makes sense if their dimensions are equal.
dimply <- function(x){
	lapply(x, dim)
}
#####################################################################################
# this is Carl Boe's drop.tadj adjustment, but put into a function (everything cycles through it once):
# Original Comment:	
# 	fix for territorial adjustments in population data.  Usually, we just
# 	want to ignore the before/after distinction (keep the '+' variant)
droptadj <- function(drop.tadj, item, wanteditem){
	if (drop.tadj & wanteditem == "Population"){
		isel        <-grep("-", item$Year);
		if (length(isel) > 0){
			item    <- item[-isel, ]  
		}
		# keep all but those with '-'
		item$Year   <- gsub("\\+", "", item$Year) # strip '+'
		item$Year   <- as.integer(item$Year)
	}
	return(item)
}

#####################################################################################
# remove "+" from open age group, applies to all items with an Age column, convert to numeric
# this is just a convenience for users: otherwise the Age column is read in as a character vector
# due to the e.g. 110+, which makes it annoying to use that column in lifetable construction, axis
# plotting, or other inherently numeric things.
removeplus <- function(item){
	if ("Age" %in% colnames(item)){
		if (any(sapply(item$Age, grepl, pattern = "+"))){
			item$Age        <- gsub("\\+", "", item$Age)
			if (!any(sapply(item$Age, grepl, pattern = "-"))){
				item$Age    <- as.integer(item$Age)
			}
		}
	}
	return(item)
}

#####################################################################################
# remove ".txt", for listing variables (items), called when the local version is used,
# since available variables are scanned in the STATS folder of each country. 
txtparse <- function(x){
	gsub(".txt","",x)
}

#####################################################################################
# all items available in HMD. if countries selected have same availability, only these
# will be shown. if not, we need to ask whether to show only those available in all of the
# selected countries, or on those available in ANY of the selected countries.
possible.items<-c("Births","bltcoh_1x1","bltcoh_1x10","bltcoh_1x5","bltcoh_5x1","bltcoh_5x10",
		"bltcoh_5x5","bltper_1x1","bltper_1x10","bltper_1x5","bltper_5x1","bltper_5x10",
		"bltper_5x5","cExposures_1x1","cExposures_1x10","cExposures_1x5","cExposures_5x1",
		"cExposures_5x10","cExposures_5x5","cMx_1x1","cMx_1x10","cMx_1x5","cMx_5x1",
		"cMx_5x10","cMx_5x5","Deaths_1x1","Deaths_1x10","Deaths_1x5","Deaths_5x1","Deaths_5x10",
		"Deaths_5x5","Deaths_lexis","E0coh","E0coh_1x10","E0coh_1x5","E0per","E0per_1x10",
		"E0per_1x5","Exposures_1x1","Exposures_1x10","Exposures_1x5","Exposures_5x1",
		"Exposures_5x10","Exposures_5x5","fltcoh_1x1","fltcoh_1x10","fltcoh_1x5","fltcoh_5x1",
		"fltcoh_5x10","fltcoh_5x5","fltper_1x1","fltper_1x10","fltper_1x5","fltper_5x1",
		"fltper_5x10","fltper_5x5","mltcoh_1x1","mltcoh_1x10","mltcoh_1x5","mltcoh_5x1",
		"mltcoh_5x10","mltcoh_5x5","mltper_1x1","mltper_1x10","mltper_1x5","mltper_5x1",
		"mltper_5x10","mltper_5x5","Mx_1x1","Mx_1x10","Mx_1x5","Mx_5x1","Mx_5x10","Mx_5x5",
		"Population","Population5")


####################################################################################
# once all the data are in, they can be converted from the ugy default form into 3 different
# less ugly forms:
# formatoutput, options = 0,1,2,3. 
# 0 returns the harder-to-work-with, but totally felxible nested list. variables inside country
# 1 returns a single data.frame, if the dimensions permit
# 2 returns a separate data.frame for each variable, long (stacked years)
# 3 returns a separate matrix for each
formatoutput <- function(inlist, format, column){
	# 0, leave in basic nested list format
	if (format == 0){ output <- inlist }
	
	# 1, try to coerce to single data.frame, if dimensions permit
	if (format == 1){
		if (length(unique(unlist(lapply(inlist, dimply)))) == 2){
			output <- as.data.frame(inlist)
			# remove redundant columns
			if (length(grep(".Year", colnames(output))) > 1){
				output      <- output[, -grep(".Year",colnames(output))[-1]]
				colnames(output)[c(grep("Year",colnames(output)))]  <- "Year"
			}
			if (length(grep(".Age",colnames(output)))>1){
				output      <- output[,-grep(".Age", colnames(output))[-1]]
				colnames(output)[c(grep("Age", colnames(output)))]  <- "Age"
			}	
		} else {
			cat("\nsorry, can't stick everything into one data.frame, probably because not all countries have selected years\n")
			cat("\ndefault data output will be a list of countries, where each country is a list with a data.frame element for each variable\n")
		}
	}
	
	# 2, separate data.frame for each variable, long (stacked years)
	if (format == 2){
		wanteditems <- names(inlist[[1]])
		countries   <- names(inlist)
		output      <- list()
		for (i in 1:length(wanteditems)){				
			thisitem            <- assign(wanteditems[i], list())
			for (j in 1:length(inlist)){
				thiscountryitem <- assign(countries[j], inlist[[j]][[i]])
				thisitem[[countries[j]]] <- thiscountryitem
			}
			
			thisitem            <- as.data.frame(thisitem)
			# remove redundant columns
			if (length(grep(".Year",colnames(output))) > 1){
				output          <- output[,-grep(".Year",colnames(output))[-1]]
				colnames(output)[c(grep("Year", colnames(output)))]  <- "Year"
			}
			if (length(grep(".Age",colnames(output))) > 1){
				output          <- output[, -grep(".Age", colnames(output))[-1]]
				colnames(output)[c(grep("Age", colnames(output)))]  <- "Age"
			}
			output[[wanteditems[i]]] <- thisitem
		}
	}	
	# 3 separate data.frame for each variable & column, years in columns, countries stacked, ages within countries
	if (format == 3){
		wanteditems             <- names(inlist[[1]])
		countries               <- names(inlist)
		output                  <- list()
		for (i in 1:length(wanteditems)){
			
			columnsitemi <- allcolumnsitemi <- colnames(inlist[[1]][[i]])
			if (is.element("Year", columnsitemi)) {
				Years           <- unique(inlist[[1]][[i]][, "Year"])
				Nyears          <- length(Years)
				columnsitemi    <- columnsitemi[ columnsitemi != "Year"]
			}
			if (is.element("Age",columnsitemi)){
				Ages            <-	unique(inlist[[1]][[i]][,"Age"])
				Nages           <- length(Ages)
				columnsitemi    <- columnsitemi[-(columnsitemi=="Age")]
			} else {
                Nages           <- 1
                Ages            <- 1
            }
			# lexis triangles are a special case
			if (wanteditems[i] == "Deaths_lexis"){
				AgesLower       <- paste(Ages,"lower",sep="_")
				AgesUpper       <- paste(Ages[-length(Ages)],"upper",sep="_")
				Ages            <- vector(length=((length(Ages)*2)-1))
				ind             <- 1:length(Ages)
				Ages[ind %% 2 == 0] <- AgesUpper
				Ages[ind %% 2 == 1] <- AgesLower
				Nages           <- length(Ages)
			}
			col.i               <- allcolumnsitemi %in% columnsitemi
			
			Variablei           <- list()
			# now make a data.frame for each column, rbind together for countries
			for (j in 1:length(columnsitemi)){
				varicolumnj     <- assign(columnsitemi[j], matrix(ncol = Nyears,nrow = 0))
				cntryvec        <- c()
                agesvec         <- c()
				for (k in 1:length(inlist)){
					varicoljctryk   <- matrix(inlist[[k]][[i]][, col.i [j]], ncol = Nyears)
					varicolumnj     <- rbind(varicolumnj, varicoljctryk)
					cntryvec        <- c(cntryvec,rep(names(inlist)[k], Nages))
					agesvec         <- c(agesvec, Ages)
				}
				varicolumnj         <- as.data.frame(varicolumnj)
				varicolumnj         <- cbind(cntryvec, agesvec, varicolumnj)
				colnames(varicolumnj)           <- c("CNTRY", "Age", Years)
				if (Nages == 1) {varicolumnj    <- varicolumnj[, colnames(varicolumnj) != "Age"]}
				Variablei[[columnsitemi[j]]]    <- varicolumnj
			}
			output[[wanteditems[i]]]            <- Variablei
		}
	}
	if (format == 4){
		if (length(inlist) == 1 & length(inlist[[1]]) == 1){
			possiblecolumns <- colnames(inlist[[1]][[1]])[! colnames(inlist[[1]][[1]]) %in% c("Year", "Age")]
			if (is.null(column) | !(is.null(column) & !any(column %in% possiblecolumns))){
				cat("If format = 4, then you'll have to choose just 1 column.\nData will be returned in a matrix with ages in the rows (names) \nand years in the columns (names)\n ")
				column          <- select.list(possiblecolumns, multiple = FALSE, title = "Select Column to Extract")
				assign("column", column, pos = parent.frame(n = 1))
			}
			if (is.element("Age", colnames(inlist[[1]][[1]]))){
				Ages            <- unique(inlist[[1]][[1]][, "Age"])
			} else {Ages        <- NA}
			if (is.element("Year", colnames(inlist[[1]][[1]]))){
				Years           <- unique(inlist[[1]][[1]][, "Year"])
			} else {Years       <- NA}
			if (names(inlist[[1]])=="Deaths_lexis"){
				AgesLower       <- paste0(Ages, "lower")
				AgesUpper       <- paste0(Ages[-length(Ages)], "upper")
				Ages            <- vector(length = ((length(Ages) * 2) - 1))
				ind             <- 1:length(Ages)
				Ages[ind%%2==0] <- AgesUpper
				Ages[ind%%2==1] <- AgesLower
			}
			output              <- matrix(as.numeric(inlist[[1]][[1]][, column]), nrow = length(Ages), ncol = length(Years))
            dimnames(output)    <- list(Ages, Years)
		} else {cat("you selected format=4, but this is intended for 1 country, 1 item, 1 column selections (multiple years OK)\n your data will be extracted, probably as a list")}
	}
	
	if (!is.data.frame(output) & length(inlist) == 1 & length(inlist[[1]]) == 1 & format != 3 & format != 4){
		output <- as.data.frame(output)
	}
	return(output)
}

####################################################################################
# Now define 2 separate functions for web vs local usage. Both are large, but have similar layouts.
####################################################################################
# 1) HMD2Rlocal is for a database on your system. They essentially work in the same way, but grab the data differently

HMD2Rlocal <- function(countries = NULL, wanteditems = NULL, drop.tadj = TRUE, years = TRUE, column = NULL, format = 0, path = NULL, syntax = TRUE){
	# get database path if missing, need to just choose the HMD_COUNTRIES folder, not its parent
	if (missing(path) || is.null(path)){
		path                <- choose.dir(caption = "Select HMD_COUNTRIES folder")
	}
	# make list of countries, if missing. use HMD abbreviations
	if(missing(countries) || is.null(countries)){
		Countries           <- select.list(list.files(path), preselect = NULL, multiple = TRUE, title = "Select Countries")
	} else {
        Countries           <- countries
    }
	
	# get a list of only availabel items
	items.in.Countries      <- c()
	for (i in 1:length(Countries)){
		ctry.files          <- list.files(file.path(path, Countries[i],"STATS"))
		ctry.items          <- sapply(ctry.files, txtparse)
		items.in.Countries  <- c(items.in.Countries, possible.items[possible.items %in% ctry.items])
	}
	
	# those items common to all countries will have table values equal to length(Countries)
	CT                      <- table(items.in.Countries)
	items.in.all.Countries  <- sort(names(CT[CT == length(Countries)]))
	items.in.any.Country    <- sort(unique(items.in.Countries))
	
	# if wanteditems is missing, we select from the list of available items:
	if (is.null(wanteditems) || missing(wanteditems)){
		# if they're the same length, then we're good to go
		if(length(items.in.all.Countries) == length(items.in.any.Country)){
			wanteditems     <- select.list(items.in.any.Country, preselect = NULL, multiple = TRUE, title = "Select Items)")
		} else {
			# if not, then we ask which list to show:
			cat("\n the countries selected have different data available.\n do you want to see a list of items in ANY of your countries or ALL of them?\n")
			ANYALL          <- select.list(c("in ANY selected country", "in ALL selected countries"), preselect = NULL, multiple = FALSE, title = "Which Items to show?)")
			if (ANYALL == "in ANY selected country"){
				wanteditems <- select.list(items.in.any.Country, preselect = NULL, multiple = TRUE, title = "Select Items")
			} else {
				wanteditems <- select.list(items.in.all.Countries, preselect = NULL, multiple = TRUE, title = "Select Items")
			}
		}
	} else {
		# otherwise, the user must have specified the items
		# are they in all countries?
		if (!all(wanteditems %in% items.in.all.Countries)){
			cat(paste("\nIt looks like not all wanted items are in each selected country.\nFunction will try to continue, but might have empty slots for unavailable requested items\n"))
		}
	}
	######## now we know what items to fetch...
	
	# output is our big basket list 
	output                  <- list()
	for (i in 1:length(Countries)){
		this.country        <- Countries[i]
		countryitemslist    <- list()
		for (j in 1:length(wanteditems)){
			pathj           <- file.path(path, Countries[i], "STATS", paste0(wanteditems[j], ".txt"))
			itemj           <- read.table(file = pathj, fill = TRUE, skip = 1, as.is = TRUE, header = TRUE, na.string = ".")
			itemj           <- droptadj(drop.tadj, item = itemj, wanteditem = wanteditems[j])  # if it's Population, option removal of lower territorial adjustment
			itemj           <- removeplus(item = itemj)			# in case Age column includes "+" identifying open ages. Prefer column as an integer.
			countryitemslist[[wanteditems[j]]] <- itemj
		}
		output[[this.country]] <- countryitemslist
	}
	# now we have a big output list, possibly parse down to selected years:
	# in case Years are to be selected, but not yet specified
	if (is.logical(years)){
		if (years){
			years           <- selectyears(output)
			output          <- parseyears(output, years)
		} 
	} else {if (is.list(years)){
			output          <- parseyears(output, years)
		}
	}
	# now reformat:
	###################
	if (missing(format) | !any(format %in% c(0,1,2,3,4))){
		format              <- 0
	}
	output                  <- formatoutput(output, format = format, column = column)
	# now optional syntax return to avoid repeating the menus (where applicable)...
	# syntax return:
	if (syntax){
		options(useFancyQuotes = FALSE) # so that quotes properly appear in cat()
		# produce single string of Countries
		countriesstring     <- dQuote(Countries[1])
		if (length(Countries) > 1){
			for (i in 2:length(Countries)){
				countriesstring <- paste(countriesstring, dQuote(Countries[i]), sep = ", ")
			}
		}
		# single string of wanteditems
		wanteitemsstring    <- dQuote(wanteditems[1])
		if (length(wanteditems) > 1){
			for (i in 2:length(wanteditems)){
				wanteitemsstring <- paste(wanteitemsstring, dQuote(wanteditems[i]), sep = ", ")
			}
		}
		# years comes up as a few lines of code, since it's a list item
		# I'm sure this could be easier...
		cat("\nTo repeat this without menus, the syntax would have been:\n\n")
		cat("years <- list()\n")
		for (i in 1:length(wanteditems)){
			# I think "-" should identify year ranges adequately, ones that need to appear in double quotes...
			if (any(sapply(years[[i]], grepl, pattern = "-"))){
				yrstringi           <- dQuote(years[[i]][1])
				if (length(years[[i]]) > 1){
					for (j in 2:length(years[[i]])){
						yrstringi   <- paste(yrstringi, dQuote(years[[i]][j]), sep = ", ")
					}
				} 
			} else {
				# even though this is a character string, it will cat without the quotes.
				yrstringi           <- years[[i]][1]
				if (length(years[[i]]) > 1){
					for (j in 2:length(years[[i]])){
						yrstringi   <- paste(yrstringi, years[[i]][j], sep = ", ")
					}
				} 
				if (length(years[[i]]) == (as.integer(years[[i]][length(years[[i]])]) - as.integer(years[[i]][1]) + 1)){
					yrstringi       <- paste(years[[i]][1], ":", years[[i]][length(years[[i]])], sep = "")
				}
			}
			cat(paste("years$", wanteditems[i], " <- c(", yrstringi, ")\n", sep = ""))
		}	
		path <- gsub("\\\\", "\\\\\\\\", path)
		cat(paste("DATA <- HMDget(countries = c(", countriesstring, "), wanteditems = c(", wanteitemsstring, 
                        "), years = years, column =", dQuote(column), ", drop.tadj = ", drop.tadj, ", format = ", format, 
                        ", path =", dQuote(path), ")\n\n(just copy and paste it next time)\n", sep = ""))
		
	}
	invisible(output)
}

####################################################################################
# 2) HMD2Rweb is for online data access. uses chunks from Carl Boe's HMD2R code. password is not sent protected

HMD2Rweb <- function(countries = NULL, wanteditems = NULL, years = TRUE, column = NULL, drop.tadj = TRUE, format = 0 , username, password, syntax = TRUE){
	
	###############################################################################
	## This function exploits the HMD2R code by Carl Boe, HMD (2011), especially
	## the RCurl syntax
	## below the HMD2R metainfo from Carl Boe, HMD 2011:
	
	##### same, except aggregated data now usually work.
	## fetches all non-aggregated data for the specified country
	## from the HMD website, using the username and password
	## of the registered user.  NB: passwords are not sent encrypted
	
	##### deprecated:
	## With no arguments, prints a list of available country codes and
	## returns a list of country code (invisible)
	
	##### same:
	## wanteditems is a char vector of the data chunks desired,
	## e.g. c("E0per","fltper_1x1") if only certain statistics
	## are desired.
	
	##### same:
	## This function uses RCurl library.  Install using either
	##     install.packages("RCurl", dep=TRUE)
	## or
	##     source("http://bioconductor.org/biocLite.R")
	##     biocLite("RCurl")
	
	## Carl Boe, Human Mortality Database, 2011
	
	# check if RCurl installed, if not, use bioconductor lite:
	if (!is.element("RCurl", installed.packages()[, 1])){
		cat("didn't find package RCurl, installing using biocLite")
		source("http://bioconductor.org/biocLite.R")
		biocLite("RCurl")
		Sys.sleep(1)
	}
	# some items to be recycled throughout:
	
	urlbase <- "http://www.mortality.org/hmd"
	tf      <- tempfile();
	on.exit(unlink(tf))
	# concatenate password
	if(any(c(missing(username), missing(password), is.null(username), is.null(password))))
		stop("username and password required for HMD access")
	this.pw <- paste(username, password, sep = ":");
	# reuse handle, reduce connection starts
	handle  <- getCurlHandle(userpwd = this.pw)
	
	#####################################################################################
	# select countries if missing
	if(missing(countries) || is.null(countries)){
		this.url            <- "http://www.mortality.org/countries.csv"
		cat(getURL(this.url), file = tf)
		ctrylist            <- read.csv(file = tf, header = TRUE, as.is = TRUE)
		tmp                 <- c(CCode=ctrylist$Subpop.Code.1)
		cat("** Country code countries required! \nHMD Countries and Country Codes\n\n")
		Countries           <- select.list(tmp, preselect = NULL, multiple = TRUE, title = "Select Countries")
	} else {Countries <- countries}
	#####################################################################################
	# Checking wanteditems, make selection lists available if missing
	
	# we need a nifty grepl checker to see what's available:
	greplavail <- function(possible.items, STATShtml){
		possible.items[sapply(possible.items, grepl, x = STATShtml)]
	}
	
	cat("\nChecking item availability, could take a minute\n")
	# now we get our big list of all available items in all countries
	items.in.Countries      <- c()
	for (i in 1:length(Countries)){
		this.url            <- file.path("http://www.mortality.org/hmd", Countries[i], "STATS", fsep = "/")
		STATShtml	        <- getURL(this.url, curl = handle)
		items.in.Countries  <- c(items.in.Countries, greplavail(possible.items, STATShtml))
	}
   
	# those items common to all countries will have table values equal to length(Countries)
	CT                      <- table(items.in.Countries)
	items.in.all.Countries  <- sort(names(CT[CT == length(Countries)]))
	items.in.any.Country    <- sort(unique(items.in.Countries))
	
	# if wanteditems is missing, we select from the right list of items:
	if (is.null(wanteditems) || missing(wanteditems)){
		# if they're the same length, then we're good to go
		if(length(items.in.all.Countries) == length(items.in.any.Country)){
			wanteditems     <- select.list(items.in.any.Country, preselect = NULL, multiple = TRUE, title = "Select Items)")
		} else {
			# if not, then we ask which list to show:
			cat("\n the countries selected have different data available.\n do you want to see a list of items in ANY of your countries or ALL of them?\n")
			ANYALL          <- select.list(c("in ANY selected country","in ALL selected countries"),preselect=NULL,multiple=FALSE,title="Which Items to show?)")
			if (ANYALL=="in ANY selected country"){
				wanteditems <- select.list(items.in.any.Country, preselect = NULL, multiple = TRUE, title = "Select Items)")
			} else {
				wanteditems <- select.list(items.in.all.Countries, preselect = NULL, multiple = TRUE, title = "Select Items)")
			}
		}
	} else {
		# otherwise, the user must have specified the items
		# are they in all countries?
		if (!all(wanteditems %in% items.in.all.Countries)){
			cat(paste("\nIt looks like not all wanted items are in each selected country.\nFunction will try to continue, but might have empty slots for unavailable requested items\n"))
		}
	}
	# 
	#####################################################################################
	# the f.fetchit function from Carl Boe of the HMD, 2011: (modified a little bit)
	# in theory it should never return errors if items were selected from the pop-up lists.
	# if items were prespecified then they may not be available, and this function returns
	# NULL
	f.fetchit2 <- function(stat = this.stat, this.skip = 2, countries, handle){
		cat(paste("  *** Fetching...", countries, ",", stat, "\n"))
		this.stat.txt   <- paste0(stat,".txt")
		this.url        <- file.path(urlbase, countries, "STATS", this.stat.txt)
		stat.return     <- NULL
		x               <- NULL
		stat.return     <-tryCatch(getURL(this.url, curl = handle),
				error = function(e) {
					cat("HTTP error: ", e$message, "\n")
				}
		);
		if(! is.null(stat.return)){         #something was returned from server
			cat(stat.return, file = tf)
			x.header    <- scan(file = tf, what = "character", nlines = 1, sep = "\n", quiet = TRUE)
			
			if( grepl('Last modified:', x.header)){ # has HMD table been returned
				x       <- read.table(file = tf, header = TRUE, skip = this.skip, as.is = TRUE, na.string = ".")
			} else {
				cat(paste(countries, stat, "..Problem fetching data"), immediate. = TRUE)
			}
			if (grepl("not found", stat.return) || grepl("Not Found", stat.return)){
				cat(paste("\n", stat, "not available for", countries, "\n"))
			}
		}
		return(x)
	} # end of f.fetchit2
	#####################################################################################
	
	# now we loop through countries and build up the output list- first level = countries, next down = items
	output <- list()
	for (i in 1:length(Countries)){
		this.country        <- Countries[i]
		countryitemslist    <- list()
		for (j in 1:length(wanteditems)){
			itemj <- f.fetchit2(wanteditems[j], countries = Countries[i], handle = handle)
			itemj <- droptadj(drop.tadj, item = itemj, wanteditem = wanteditems[j])  # if it's Population, option removal of lower territorial adjustment
			itemj <- removeplus(item = itemj)			# in case Age column includes "+" identifying open ages. Prefer column as an integer.
			countryitemslist[[wanteditems[j]]] <- itemj
		}
		output[[this.country]] <- countryitemslist
	}
	
	# in case Years are to be selected, but not yet specified
	if (is.logical(years)){
		if (years){
			years   <- selectyears(output)
			output  <- parseyears(output, years)
		} 
	} else {if (is.list(years)){
			output  <- parseyears(output, years)
		}
	}
	
	# now reformat:
	###################
	if (missing(format) | !any(format %in% c(0:4))){
		format      <- 0
	}
	output          <- formatoutput(output, format = format, column = column)
	
	# now optional syntax return to avoid repeating the menus (where applicable)...
	# syntax return:
	if (syntax){
		options(useFancyQuotes = FALSE) # so that quotes properly appear in cat()
		
		# produce single string of Countries
		countriesstring         <- dQuote(Countries[1])
		if (length(Countries) > 1){
			for (i in 2:length(Countries)){
				countriesstring <- paste(countriesstring, dQuote(Countries[i]), sep = ", ")
			}
		}
		
		# single string of wanteditems
		wanteitemsstring        <- dQuote(wanteditems[1])
		if (length(wanteditems) > 1){
			for (i in 2:length(wanteditems)){
				wanteitemsstring <- paste(wanteitemsstring, dQuote(wanteditems[i]), sep = ", ")
			}
		}
		
		# years comes up as a few lines of code, since it' a list item
		# I'm sure this could be easier...
		cat("\nTo repeat this without menus, the syntax would have been:\n\n")
		cat("years <- list()\n")
		
		for (i in 1:length(wanteditems)){
			# I think "-" should identify year ranges adequately, ones that need to appear in double quotes...
			if (any(sapply(years[[i]], grepl, pattern = "-"))){
				yrstringi           <- dQuote(years[[i]][1])
				if (length(years[[i]]) > 1){
					for (j in 2:length(years[[i]])){
						yrstringi   <- paste(yrstringi, dQuote(years[[i]][j]), sep = ", ")
					}
				} 
			} else {
				# even though this is a character string, it will cat without the quotes.
				yrstringi           <- years[[i]][1]
				if (length(years[[i]]) > 1){
					for (j in 2:length(years[[i]])){
						yrstringi   <- paste(yrstringi, years[[i]][j], sep = ", ")
					}
				}
				if (length(years[[i]]) == (as.integer(years[[i]][length(years[[i]])]) - as.integer(years[[i]][1])+1)){
					yrstringi       <- paste(years[[i]][1], ":", years[[i]][length(years[[i]])],sep="")
				}
			}
			cat(paste("years$", wanteditems[i]," <- c(", yrstringi, ")\n",sep=""))
		}
		cat(paste("DATA <- HMDget(countries = c(", countriesstring, "), wanteditems = c(", wanteitemsstring,
                        "), years = years, column =", dQuote(column), ", drop.tadj = ", drop.tadj, 
                        ", format = ", format, ", username = ", dQuote(username), ", password = ", 
                        dQuote(password), ")\n\n(just copy and paste it next time)\n", sep = ""))
	}
	invisible(output)
}

####################################################################################
# now we send to one or the other grabber:
####################################################################################
# decide to use local or web:

# if both are missing or both are specified, we need a selection list
if ((missing(path) & (missing(password) | missing(username))) | (!missing(path) & !missing(password) & !missing(username))){
	LOC     <- select.list(c("web (slower, but up to date)", "from disk"), preselect = NULL, multiple = FALSE, title = "How to access HMD")
	local   <- (LOC == "from disk")
} else {
	# if path is specified and password or username missing, then it's local
	if (!missing(path) & (missing(password) | missing(username))){
		local <- TRUE
	} else {
		# if path is missing and both username and password are given, then it's web
		if (missing(path) & !(missing(password) | missing(username))){
			local <- FALSE
		}
	}
} 
# by this point, local must have been specified as either TRUE or FALSE...
if (local){
	output <- HMD2Rlocal(countries = countries, wanteditems = wanteditems, years = years, 
            column = column, drop.tadj = drop.tadj, format = format, path = path, syntax = syntax)
} else {
	if (missing(username) | missing(password)){
		stop(paste("\nfor the web-based HMDget function, you need to specify your username and password\nas character strings in the arguments. go to http://www.mortality.org/ \nand register if you haven't already, you'll have to anyway if you want to download \nthe full database (HMD_COUNTRIES)\n\nthen specify it like this:\nMY_HMD_EXTRACT <- HMDget(local = FALSE, username =", dQuote("myusername"),", password =",dQuote("mypassword"),")"))
	}
	output <- HMD2Rweb(countries = countries, wanteditems = wanteditems, years = years, 
            column = column, drop.tadj = drop.tadj, format = format, username = username, password = password, syntax = syntax)
}
invisible(output)
}

