########### WHO VA 2014 -> InterVA4 file format mapping ################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to InterVA4 file 
##			format ready to be coded in InterVA4.
## @input:		mapping file who 2014 -> interva4 variables, and
##			who 2014 submission csv (exported from odk aggregate)
## @output:		csv for usage in InterVA4
## @author: 	RMI
## @date: 		07.2015
##
## ----------------------------------------------------------------------

# load the required libraries:

#load InterVA4 package
library(InterVA4)

#load foreach package
library(foreach)

cat("\nWHO VA Instrument 2014 -> InterVA4 Conversion\n\n")

#Clear variables
rm(list=ls(all=TRUE))

#Start time
ptm <- proc.time()

## Define your file path variables here###############################
######################################################################
######################################################################
workingDir = "C:/dev/workspace_R/who_va_2014_interva4_mapping/data";
mappingFileName = "mappings.csv"
submissionFileName = "who.csv"
outputFileName = "outputData.csv"

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

getValueInSubmissionForWHOId<-function(id){
	#print(paste("Get object with id:", id))
	value = get(id)
	return(value)
}

loadAndSetAllVariablesFromWHOInstrument<-function(){
	x <- foreach(j=1:n) %do% {
		entry = who[j] #Get current entry
		header = names(who)[j]
		value = entry[1,1]
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))

	#Set value to 0 if NA to prevent NA when evaluating expression
	#Check how to replace this, since this introduces some errors (e.g. value < 10)
		if(is.na(value)){
			value = -1;
		}

		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
		#print(get(header_cleaned))
		#print(paste(header_cleaned,":", value))
	}
}

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

loadAndSetAllVariablesFromWHOInstrument()

#print(ls(all.names = TRUE)) #print all variables

#Prepare output
outputData <- data.frame(matrix(ncol=246))

#Run through mappings file and fill in value for every InterVA4 variable
who_n = nrow(mapping)
#who_n = 1 # Limit to first x entries for testing purposes
counter = 1

x <- foreach(i=1:who_n) %do%{	

	##Assign who variable
	#who_var = as.character(mapping[i,4]) # Convert to class character from factors
	expression = as.character(mapping[i,5])
	interva = as.character(mapping[i, 2])
	id = mapping[i, 1]
	question = mapping[i,3]
	
	#Set Column names and also ID
	if(i == 1){
		colnames(outputData)[1] <- "ID"
		outputData[1] = i
	}
	colnames(outputData)[i+1] <- interva

	if(class(interva) == "character" && nchar(interva) > 0){
		#value = getValueInSubmissionForWHOId(who_var)

		##Evaluate expression and set InterVA4 variable accordingly
		retVal = eval(parse(text=expression))
		if(retVal == TRUE){
			printTrueOutput = FALSE
			if(printTrueOutput){
				print(counter)
				print(paste("Id:",id))
				print(paste("Question:",question))
				#print(paste("Got back value:", value))
				print(paste("Expression:", expression), max.levels=0)
				#print(paste(who_var,":", get(who_var)))
				print(paste("InterVA4:", interva))
			}
			##Evaluate expression and set InterVA4 variable accordingly
			outputData[i+1] = 'y'
		}
	}else{
		print("Empty")
	}
	counter = counter + 1
}

colnames(outputData) <- toupper(colnames(outputData)) #Change colnames to all uppercase

#print(outputData) #output InterVA4 data input structure

#Write output to file (comma separated for interva4 input, no quotation marks)
write.csv(outputData,file=outputFileName, quote = FALSE, na="", row.names=FALSE)

#InterVA4 analysis
## to get causes of death with group code for further usage
va <- InterVA(outputData, HIV = "l", Malaria = "l", directory = "VA test", 
filename = "VA_result_wt_code", output = "extended", append = FALSE,
replicate = TRUE, groupcode = TRUE, write = TRUE)

#str(va) # Output InterVA4 object structure

cat("\nInterVA4 Result:\n\n")
print(paste("Most likely WHO 2012 VA cause category:", va$VA[[1]]$CAUSE1))
print(paste("Likelihood:", va$VA[[1]]$LIK1, "%"))

InterVA.plot(va$VA[[1]], type = "pie", min.prob = 0.01,
main = "1st sample VA analysis using pie chart", clockwise = FALSE,
radius = 0.6, cex = 0.6, cex.main = 0.8)

## Save plot as image
#dev.copy(jpeg,filename="plot.jpg");
#dev.off ();

cat("\n\n\n")
etm<-proc.time() - ptm # End time
print(paste("Total run time:", etm[3], "s")) #print elapsed time

# change back to the original directory
setwd(initial.dir)

# unload the libraries
detach("package:InterVA4")
detach("package:foreach")

cat("\nWHO VA Instrument -> InterVA4 Mapping Done!\n\n")