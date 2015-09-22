########### WHO VA 2014 -> CSV Coding file format mapping ###############
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tool submissions to a coding file 
##			format ready to be coded in a VA Coding Software 
##			(e.g. InterVA4/SmartVA).
## @input:		mapping file who 2014 -> coding variables, and
##			who 2014 submission csv (exported from odk aggregate)
## @output:		csv for usage in coding software
## @author: 	RMI
## @date: 		09.2015
##
## ----------------------------------------------------------------------

# load the required libraries:

#load foreach package
library(foreach)

cat("\nWHO VA Instrument 2014 -> Coding Software Conversion\n\n")

#Clear variables
rm(list=ls(all=TRUE))

#Start time
ptm <- proc.time()

## Define your file path variables here###############################
######################################################################
######################################################################
workingDir = "C:/dev/workspace_R/who_va_2014_interva4_mapping/data";
mappingFileName = "tariff_mapping_full_v3.csv"
#mappingFileName = "interva4_mapping.csv"
submissionFileName = "who.csv"
outputFileName = "outputData_coding.csv"
######################################################################

# store the current directory
initial.dir<-getwd()
print(paste("initial Working directory:" , initial.dir))
# change to the new directory
print(paste("Set to working directory:" , workingDir))
setwd(workingDir)
print(paste("Set to working directory to:" , getwd()))

#load who submission file:
who = read.csv(submissionFileName)

#store column names
v <- colnames(who)
n = ncol(who);
entries = nrow(who);

getValueInSubmissionForWHOId<-function(id){
	#print(paste("Get object with id:", id))
	value = get(id)
	return(value)
}

loadAndSetAllVariablesFromWHOInstrument<-function(entryLevel){
	entry = who[entryLevel,] #Get current entry
	x <- foreach(j=1:n) %do% {
		header = names(who)[j]
		value =  as.character(entry[1,j])
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))

		#Set value to 0 if NA to prevent NA when evaluating expression
		#Check how to replace this, since this introduces some errors (e.g. value < 10)
		if(is.na(value)){
			value = -1;
		}
		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

mapValues <- function(from, to, value){

	if(value == -1){
		print("Value is undefined (-1)")
		return(-1)
	}
	#print(paste("from:",from))
	#print(paste("to:", to))


	#from_vector = scan(textConnection(from), what="character()", sep=",")
	#to_vector = scan(textConnection(to), what="character()", sep=",")


	from_vector = unlist(strsplit(from, ","))
	to_vector = unlist(strsplit(to, ","))

	#print(paste("from:",is.character(from_vector)))
	#print(paste("to:", is.character(to_vector)))

	#print(paste("from:",length(from_vector)))
	#print(paste("to:", length(to_vector)))

	#print(paste("Mapping", from_vector, "to", to_vector, "Actual value:", value))

	elementAt = match(value,from_vector)

	#print(paste("Class of from vector:",class(from_vector)))

	#print(paste("to_vector[2]",to_vector[2]))

	#print(paste("from_vector:",from_vector))
	#print(paste("to_vector:",to_vector))
	#print(paste("value:",value))
	#print(paste("ElementAt:", elementAt))
	#print(paste("Mapped value:", to_vector[elementAt]))

	if(is.na(elementAt)){
		return(-1)
	}
	else{
		return(to_vector[elementAt])
	}
}


#Load mapping csv file:
mapping = read.csv2(mappingFileName, stringsAsFactors=F, dec=".")

#Run through mappings file and fill in value for every mapping variable
variables_n = nrow(mapping)
#variables_n = 1 # Limit to first x entries for testing purposes

print(paste("No. of variables to map:", variables_n))
counter = 1

outputData <- data.frame(matrix(ncol=variables_n)) #Initialize output dataframe

rows <- foreach(entryCount=1:entries ) %do%{	
	loadAndSetAllVariablesFromWHOInstrument(entryCount)

	#Prepare output
	currentData <- data.frame(matrix(ncol=variables_n))

	x <- foreach(i=1:variables_n) %do%{	

		##Assign who variable
		who_var = as.character(mapping[i,5]) # Convert to class character from factors
		expression = as.character(mapping[i,6])
		destination_var = as.character(mapping[i, 2])
		id = mapping[i, 1]
		question = mapping[i,3]
		fix_value = as.character(mapping[i,7])
		dynamic_value = as.character(mapping[i,8])
		mapping_from = as.character(mapping[i,9])
		mapping_to = as.character(mapping[i,10])

		assign("current_row", i, envir = .GlobalEnv)

		colnames(currentData)[i] <- destination_var

		#Checks
		if(!is.na(fix_value) && nchar(fix_value) > 0 && !is.na(dynamic_value) && nchar(dynamic_value)){
			stop(paste("ERROR: Line", (i+1), "Fix value and dynamic value can NOT be set at the same time!"))
			print("ERROR: Fix value and dynamic value can't be set at the same time. Please fix this in the mapping file and try again.")
		}

		if(!is.na(fix_value) && nchar(fix_value) > 0 && nchar(expression) == 0){
			#print(paste(i,"CONTAINS fixed ENTRY", fix_value))
			currentData[i] = fix_value
		}
		else if(!is.na(dynamic_value) && nchar(dynamic_value) > 0 && nchar(expression) == 0){
			#print(paste(i,"CONTAINS Dynamic ENTRY", dynamic_value))
			dynamic_value_parsed = eval(parse(text=dynamic_value))
			if(dynamic_value_parsed != -1){
				#print(paste(i,"CONTAINS DYNAMIC VALUE::", dynamic_value_parsed))
				currentData[i] = dynamic_value_parsed
			}
			else{
				currentData[i] = 0
			}
		}
		else if(nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0 && nchar(expression) == 0){
			#print(paste(i, "Mapping",mapping_from,"to",mapping_to))
			#print(paste(class(mapping_from), class(mapping_to)))
			#print(paste("Value:", get(who_var),"(" ,who_var, ")"))

			if(get(who_var) != -1){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
			
				#print(paste("Value", get(who_var), "mapped to", mapped_value))
				currentData[i] = mapped_value
			}
			else{
				currentData[i] = 9
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && (is.na(mapping_from) || nchar(mapping_from) == 0)){
			#print(paste("Expression:", expression))
			evalBool = eval(parse(text=expression))
			if(evalBool == TRUE){
				#print("Expression fullfilled!")
				if(!is.na(fix_value) && nchar(fix_value) > 0){
					#print(paste(i,"CONTAINS fixed ENTRY", fix_value))
					currentData[i] = fix_value
				}
				else if(!is.na(dynamic_value) && nchar(dynamic_value) > 0){
					dynamic_value_parsed = eval(parse(text=dynamic_value));
					#print(paste(i,"CONTAINS DYNAMIC ENTRY:", dynamic_value_parsed))
					currentData[i] = dynamic_value_parsed
				}
			}
			else{
				#print(paste(i,"evaluated to FALSE"))
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0){
			evalBool = eval(parse(text=expression))
			if(evalBool == TRUE){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
				#print(paste(i, "Value::", get(who_var), "mapped to", mapped_value))
				currentData[i] = mapped_value
			}
			else{
				#print(paste(i, "evaluation resulted in FALSE!"))
			}
		}
		else{
			#print(paste(i,"IS EMPTY"))
			currentData[i] = ""
		}

		counter = counter + 1
	}

	colnames(outputData) <- colnames(currentData) #Set column names for output
	outputData[entryCount,] <- currentData
}

#print(outputData) #print

write.csv(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="")

cat("\n\n\n")
etm<-proc.time() - ptm # End time
print(paste("Total run time:", etm[3], "s")) #print elapsed time

# change back to the original directory
setwd(initial.dir)

# unload the libraries
detach("package:foreach")

cat("\nWHO VA Instrument -> Mapping Done!\n\n")