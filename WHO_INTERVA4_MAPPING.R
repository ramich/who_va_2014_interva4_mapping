## TEST VA WHO <-> InterVa4 mapping

cat("\nWHO VA Instrument -> InterVA4 Mapping\n\n")

#Clear variables
rm(list=ls(all=TRUE)) 

ptm <- proc.time()

# store the current directory
initial.dir<-getwd()
print(paste("initial Working directory:" , initial.dir))
# change to the new directory
setwd("C:/dev/workspace_R/who_va_2014_interva4_mapping/data")

# load the necessary libraries
#library(nlme)
#load foreach package
library(foreach)

#load who submission file:
who = read.csv("C:/dev/workspace_R/who_va_2014_interva4_mapping/data/who.csv")

#store column names
v <- colnames(who)
n = ncol(who);

#Test function
myfunction<-function(x){
	#print(paste("Input:", x))
}

#Function to return String value for who id
getValueInSubmissionForWHOId<-function(id){
	if(class(id) != "character" || nchar(id)==0){
		#print(paste("Invalid input:", id))
		return(NULL)
	}

	returnValue = NULL
	#print(paste("Columncount:", n))
	#print(paste("CounteR:", j))
	print(paste("Looking for:", id))
	found = FALSE
#	x <- foreach(j=1:n, .errorhandling='pass') %do% {
	x <- foreach(j=1:n) %do% {
		if(!found){
			#print(paste("J:",j)) #Print current position in loop
			entry = who[j] #Get current entry
			#print(entry) #print entry (header with data)
			header = names(who)[j]
			value = entry[1,1]
			#cat("\n")
			#print(paste("Header:", header)) # print header
			#print(paste("Value:", value)) # print value
			#cat("\n")

			header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
			#print(paste("Cleaned header:", header_cleaned))

			#print(grep(id, header_cleaned))
		
			matches = grep(id, header_cleaned)
			if(length(matches) > 0 && matches == 1){
				print(paste("Found match:", id,"==", header_cleaned, "| Value:", value ))
				returnValue = value
				#return(value)
				found = TRUE
			}
		}
	}
	return(returnValue)
}

getValueInSubmissionForWHOId2<-function(id){
	print(paste("Get object with id:", id))
	value = get(id)
	return(value)
}

grabAllVariables<-function(){
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

#Define columnName we are looking for
searchString = "id6H300"
print(paste("Looking for string", searchString))

#Search for column index that contains this pattern
returnValue = grep(searchString , v)

#if found, print out columnName, otherwise display not found message
if(!is.na(returnValue[1])){
	print(returnValue)
	print(paste("Found string", searchString, "in", v[returnValue[1]], "in column", returnValue[1]))
	#get data value for this column:
	print(paste("Data:",who[returnValue[1], 1]))
}else{
	print(paste("Could not find string", searchString ))
}

x <- foreach(i=1:n) %do% v[i]
	myfunction(x)

#Evaluate string expression
expression="if(5+5 > 10) { print('larger 10')} else { print('smaller 10')}"
eval(parse(text=expression))

#test and/or in boolean expression
if( 1 > 0 && 1 < 0){
	print("True!")
}else{
	print("False!")
}

print(paste("STHSTH:", who[121][1,1]))

val = who[121][1,1]
if(!is.na(val)){
	print(paste("is not na:", val ))
}else{
	print(paste("is na:", val ))
}


if( 1 > 0 || 1 < 0){
	print("True!")
}else{
	print("False!")
}

#Load mapping csv file:
mapping = read.csv2("C:/dev/workspace_R/who_va_2014_interva4_mapping/data/mappings.csv")

print("Mapping follows:")
print(mapping[1,]) #print whole line



##Read expression
expr = mapping[1,9]
print(expr)


grabAllVariables()

#print(ls(all.names = TRUE)) #print all variables

#Call function to get value for id
#who_var = "unknownvar"

who_n = nrow(mapping)
who_n = 43 # Limit to first x entries for testing purposes
counter = 1
x <- foreach(i=1:who_n) %do%{	
	##Assign who variable
	who_var = as.character(mapping[i,5]) # Convert to class character from factors
	#print(who_var, max.levels=0)
	#print(class(who_var))
	expression = as.character(mapping[i,9])
	interva = as.character(mapping[i, 2])
	id = mapping[i, 1]
	question = mapping[i,3]

	#if(class(who_var) == "character" && nchar(who_var) > 0){
	if(class(interva) == "character" && nchar(interva) > 0){

		print(who_var, max.levels=0)

		#value = getValueInSubmissionForWHOId(who_var)
		value = getValueInSubmissionForWHOId2(who_var)

		if(!is.null(value)){
			print(counter)
			print(paste("Id:",id))
			print(paste("Question:",question))
			print(paste("Got back value:", value))
			print(paste("Expression:", expression), max.levels=0)
			print(paste(who_var,":", get(who_var)))
			print(paste("InterVA4:", interva))
			##Evaluate expression and set InterVA4 variable accordingly
			retVal = eval(parse(text=expression))
			print(paste("Eval:", retVal))
			counter = counter + 1
		}else{
			print("------------------------Could not find variable-------------------")
		}
	}else{
		print("Empty")
	}
}

etm<-proc.time() - ptm

print(paste("Total run time:", etm[3], "s")) #print elapsed time


# load the dataset
# unload the libraries
detach("package:foreach")
# change back to the original directory
setwd(initial.dir)

cat("\nWHO VA Instrument -> InterVA4 Mapping Done!\n\n")