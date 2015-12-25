#Phone Number Extraction
#Copyright Scott Brunstein

# Given a URL as input, write a program that finds all the phone numbers that appear on the webpage given by the URL. This is an open-ended question, where you would have to identify various possible patterns of phone numbers and extract numbers based on patterns. The quality of the program will be judged based on the number of correct and number of incorrect phone numbers your program extracts.

#############################################################################################Original website:
#Choose a website

############################################################################################
library(stringr) #Used for stringsplitting functions

#Enter URL link inside the quotation marks
url <- readLines("http://www.whitepages.com/phone/1-757-455-4500") 

#Setting a generic pattern all phone numbers follow
pattern <- "[(]?[0-9]{3}[)]?[ .-]?[0-9]{3}[ .-]?[0-9]{4}" #Patterns phone numbers can follow

#Searching for all combinations of numbers that follow this pattern
all_Numbers <- grep(pattern, url, value = TRUE)
numbers = NULL
for (i in 1:length(all_Numbers))
{
  value <- str_extract(all_Numbers[i], pattern)
  numbers <- append(numbers, value)
}

#Putting the numbers in xxxxxxxxxx format
numbers <- gsub("\\(", "", numbers)
numbers <- gsub("\\)", "", numbers)
numbers <- gsub(" ", "", numbers)
numbers <- gsub("-", "", numbers)
#numbers is currently of class "character"

#Changing numbers to class integer
phoneNumber <- (as.numeric(numbers))

#Finding all duplicate values
duplicates <- duplicated(phoneNumber)

#Changing the duplicates from character to integer so I can subset the data in the data frame
duplicates <- as.character(duplicates)

#All unique phone numbers on this web page
phone_Numbers <- phoneNumber[!duplicated(phoneNumber)]

#Number of duplicate values on this web page
number_of_duplicates <- length(phoneNumber) - length(phone_Numbers)

#Extracting area codes from phone numbers
areaCode <- NULL
areaVals <- NULL
numberChars <- strsplit(numbers, "")
for (i in 1:length(numberChars))
{
  areaDigits <- rev(numberChars[[i]])
  a <- areaDigits[10]
  b <- areaDigits[9]
  c <- areaDigits[8]
  areaVals <- paste(a,b,c)
  areaVals <- gsub(" ", "", areaVals)
  areaCode <- append(areaCode, areaVals)
}
areaCode <- as.integer(areaCode)
#areaCode

#Putting all the data into a data frame that includes the numbers, area code, and whether or not it is a duplicate
data_with_duplicates <- data.frame(phoneNumber, areaCode, duplicates)

#Taking out the duplicates
data <- subset(data_with_duplicates, data_with_duplicates[ , 3] == 'FALSE')
#data

print("The total number of phone numbers on this page is:")
print(length(phoneNumber))
print("The total number of duplicates on this page is:")
print(number_of_duplicates)
print("The list of total unique numbers on this webpage is:")
#phone_Numbers

#Importing area code key matcher
areaCodeDictionary <- read.csv("~/Desktop/Job Applications/Trooly/AreaCode.txt", sep=";", dec=",")
#areaCodeRegion <- read.csv("~/Desktop/Job Applications/Trooly/AreaCodeDefinitionsEXCEL.csv")

#Changing description of county from factor to character
areaCodeDictionary$Description <- as.character(areaCodeDictionary$Description)
#areaCodeDictionary["Region"] <- areaCodeRegion$Region


#Matching up the correct county area with the area code for each phone number
#If there is no matching county, I will append the value "No Match" and take that value out in the data frame, as this indicates the initial regular expression just found a string of 10 numbers that happened to not be a phone number
county <- NULL
#finalRegion <- NULL
noMatch <- "No Match"
for (i in 1:length(data$areaCode))
{
  if (data$areaCode[i] %in% areaCodeDictionary$Area.Code)
  {
    areaCodeVal <- which(areaCodeDictionary$Area.Code %in% data$areaCode[i])
    indexVal <- areaCodeDictionary$Description[areaCodeVal]
    #indexRegion <- areaCodeDictionary$Region[areaCodeVal]
    county <- append(county, indexVal)
    #finalRegion <- append(finalRegion,indexVal)
  }
  else
  {
    county <- append(county, noMatch)
  }
}
#county

#Adding the county to the data frame and taking out the duplicates column since there are no longer any duplicates
data["County"] <- county
#data["Region"] <- finalRegion
data$duplicates <- NULL

data <- subset(data, County != "No Match")
View(data)

#Some simple statistics
histogram(as.factor(data$County))
