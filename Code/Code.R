#==============================================================================================================
# Project name: Analysis on cirminal dataset
#
# Team:
#      Zan Zver, student ID: 18133498
#
# Available on GitHub https://github.com/ZanZver/AppliedStatisticsCoursework
#==============================================================================================================



#==============================================================================================================
# Library downloads 
#==============================================================================================================



#==============================================================================================================
# Library imports
#==============================================================================================================
library(dplyr)
library(explore)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
#install.packages('e1071')
library(e1071)

#==============================================================================================================
# Functions
#==============================================================================================================

mergeData <- function(data1, data2){
  newData <- rbind(data1, data2)
  if( (as.integer(nrow(data1)) + as.integer(nrow(data2))) == nrow(newData)){
    print("Its the same") 
    return(newData)
  }else{
    print("Not the same")
    return(NULL)
  }
}

removeNA <- function(data){
  for (i in colnames(data)){
    print(i)
    tmp <- data %>% select(all_of(i))
    print(sum(is.na(tmp)))
  }
  
  print("==============================================================================================")
  data <- na.omit(data)
  
  for (i in colnames(data)){
    print(i)
    tmp <- data %>% select(all_of(i))
    print(sum(is.na(tmp)))
  }
  
  return(data)
}

dataStatistics <- function(data){
  print(paste("Mean:", mean(data)))
  print(paste("Median:", median(data)))
  print(paste("Standard deviation:", sd(data)))
  print(paste("Min:", min(data)))
  print(paste("Max:", max(data)))
  print(paste("Skewness:", skewness(data)))
  print(paste("Number of attributes:", length(data)))
}

drawPlot <- function(data, col, xName){
  p <- ggplot(data, aes(x=col)) + 
    geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    labs(x = xName, title = paste("Plot of", xName, sep = " ")) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  print(p)
}

z.test = function(a, mu, var){
  zeta = (mean(a) - mu) / (sqrt(var / length(a)))
  return(zeta)
}
#==============================================================================================================
# Data import
#==============================================================================================================

# Main dataset
dataPath <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime.csv"
dataPath_lite <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime_lite.csv"
# Supporting datasets
IUCR_Data_Path <-"/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/IUCR_Codes.csv"
Common_Areas_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/CommAreas.csv"
Police_Beat_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/PoliceBeatDec2012.csv"
Police_District_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/PoliceDistrictDec2012.csv"
NIBRS_Offense_Codes_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/NIBRS_Offense_Codes.csv"
ISR2016_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2016-ISR.csv"
ISR2017_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2017-ISR.csv"
ISR2018_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2018-ISR.csv"
ISR2019_1_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2019-ISR-Jan-Jun.csv"
ISR2019_2_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2019-ISR-Jul-Dec.csv"
ISR2020_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2020-ISR.csv"
ISR2022_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/ISR/2022-ISR.csv"

# Load the data
criminal_data <- read.csv(dataPath, stringsAsFactors=TRUE)
IUCR_Data <- read.csv(IUCR_Data_Path, stringsAsFactors=TRUE)
Common_Areas <- read.csv(Common_Areas_Path, stringsAsFactors=TRUE)
Police_Beat <- read.csv(Police_Beat_Path, stringsAsFactors=TRUE)
Police_District <- read.csv(Police_District_Path, stringsAsFactors=TRUE)
NIBRS_Offense_Codes <- read.csv(NIBRS_Offense_Codes_Path, stringsAsFactors=TRUE)
ISR2016_data <- read.csv(ISR2016_Path, stringsAsFactors=TRUE)
ISR2017_data <- read.csv(ISR2017_Path, stringsAsFactors=TRUE)
ISR2018_data <- read.csv(ISR2018_Path, stringsAsFactors=TRUE)
ISR2019_1_data <- read.csv(ISR2019_1_Path, stringsAsFactors=TRUE)
ISR2019_2_data <- read.csv(ISR2019_2_Path, stringsAsFactors=TRUE)
ISR2020_data <- read.csv(ISR2020_Path, stringsAsFactors=TRUE)
ISR2022_data <- read.csv(ISR2022_Path, stringsAsFactors=TRUE)

# Check the columns
#colnames(criminal_data)
# Remove useless columns
criminal_data <- subset(criminal_data, select = -c(X.Coordinate, Y.Coordinate, Location, Year))

# Remove all the columns except the listed ones
ISR2016_data <- ISR2016_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2017_data <- ISR2017_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2018_data <- ISR2018_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2019_1_data <- ISR2019_1_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2019_2_data <- ISR2019_2_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2020_data <- ISR2020_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]
ISR2022_data <- ISR2022_data[,c("AGE", "HEIGHT", "WEIGHT", "RACE_CODE_CD", "CITY", "DISTRICT", "BEAT")]

# Merge ISR data together
merge1 <- mergeData(ISR2016_data, ISR2017_data)
rm(ISR2016_data, ISR2017_data)
merge2 <- mergeData(ISR2018_data, ISR2019_1_data)
rm(ISR2018_data, ISR2019_1_data)
merge3 <- mergeData(ISR2019_2_data, ISR2020_data)
rm(ISR2019_2_data, ISR2020_data)
merge4 <- mergeData(ISR2022_data, merge1)
rm(ISR2022_data, merge1)
merge5 <- mergeData(merge2, merge3)
rm(merge2, merge3)
ISR <- mergeData(merge4, merge5) # Final merge
rm(merge4, merge5)

# Remove paths
rm(dataPath, 
   dataPath_lite, 
   IUCR_Data_Path, 
   Common_Areas_Path, 
   Police_Beat_Path, 
   Police_District_Path, 
   NIBRS_Offense_Codes_Path,
   ISR2016_Path,
   ISR2017_Path,
   ISR2018_Path,
   ISR2019_1_Path,
   ISR2019_2_Path,
   ISR2020_Path,
   ISR2022_Path)

head(ISR)

#==============================================================================================================
# Data cleaning
#==============================================================================================================

#colnames(criminal_data)

criminal_data <- removeNA(criminal_data)
ISR <- removeNA(ISR)

#sapply(criminal_data, mode)
#sapply(criminal_data, class)

criminal_data <- transform(criminal_data, 
                            ID = as.integer(ID),
                            Case.Number = as.character(Case.Number),
                            Date = as.POSIXct(criminal_data$Date, format="%m/%d/%Y %I:%M:%S %p",tz="America/Chicago"),
                            Block = as.character(Block),
                            IUCR = as.character(IUCR),
                            Primary.Type = as.character(Primary.Type),
                            Description = as.character(Description),
                            Location.Description = as.character(Location.Description),
                            Arrest = as.logical(Arrest),
                            Domestic = as.logical(Domestic),
                            Beat = as.integer(Beat),
                            District = as.integer(District),
                            Ward = as.integer(Ward),
                            Community.Area = as.integer(Community.Area),
                            FBI.Code = as.character(FBI.Code),
                            Updated.On = as.POSIXct(criminal_data$Updated.On, format="%m/%d/%Y %I:%M:%S %p",tz="America/Chicago"),
                            Latitude = as.numeric(Latitude),
                            Longitude = as.numeric(Longitude)
                            )

ISR <- transform(ISR,
                 AGE = as.integer(AGE),
                 HEIGHT = as.numeric(HEIGHT),
                 WEIGHT = as.numeric(WEIGHT),
                 RACE_CODE_CD = as.character(RACE_CODE_CD),
                 CITY = as.character(CITY),
                 DISTRICT = as.integer(DISTRICT),
                 BEAT = as.integer(BEAT)
                 )

# Remove all of the variables that don't match IUCR type
mystr <- sapply(list(levels(as.factor(IUCR_Data$IUCR))), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$IUCR),]

# Remove all of the variables that don't match area number
mystr <- sapply(list(levels(as.factor(Common_Areas$AREA_NUMBE))), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$Community.Area),]

# Remove all of the variables that don't match beat number
mystr <- sapply(list(levels(as.factor(Police_Beat$BEAT_NUM))), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$Beat),]

# Remove all of the variables that don't match district number
mystr <- sapply(list(levels(as.factor(Police_District$DIST_NUM))), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$District),]

# IF the city isn't CHICAGO, remove it
ISR <- ISR[grepl("CHICAGO", ISR$CITY),]

# Remove all of the variables that don't match beat number
mystr <- sapply(list(levels(as.factor(Police_Beat$BEAT_NUM))), paste, collapse = "|")
ISR <- ISR[grepl(mystr, ISR$BEAT),]

# Remove all of the variables that don't match district number
mystr <- sapply(list(levels(as.factor(Police_District$DIST_NUM))), paste, collapse = "|")
ISR <- ISR[grepl(mystr, ISR$DISTRICT),]

# Make sure that race matches the requirements in https://home.chicagopolice.org/wp-content/uploads/2020/08/ISR-Data-Dictionary.csv
ISR <- ISR[grepl("BLK|WHI|API|WBH|WWH|I|U|P|WHT", ISR$RACE_CODE_CD),]

# Transfer HEIGHT from inces to CM
ISR$HEIGHT <- (2.54 * ISR$HEIGHT)

# Limit age from 12 (before that parents would be responsible) to 120 (that is feezable maximum...).
ISR <- subset(ISR, AGE>=12)
ISR <- subset(ISR, AGE<=120)

# Smallest person is 55cm, wile the tallest person is 270, we are using limit of 120 since this is where the extreme is in our dataset
ISR <- subset(ISR, HEIGHT>=55)
ISR <- subset(ISR, HEIGHT<=121)

# Minimum weight is 25kg (since this is the lightest person on the globe), the heaviest person is 410kg which is another extreme, therefore we did set our max to 200kg 
ISR <- subset(ISR, WEIGHT>=25)
ISR <- subset(ISR, WEIGHT<=200)

rm(mystr)

#==============================================================================================================
# TEMP!!! - Data exploration
#==============================================================================================================

#--------------------------------
# TRUE vs FALSE arrests 
#--------------------------------
# Create new dataframe that has TRUE and FALSE arrests (separated) with date column
data <- data.frame(
  type = c( criminal_data$Arrest[criminal_data$Arrest == "TRUE"], 
            criminal_data$Arrest[criminal_data$Arrest == "FALSE"]),
  value = criminal_data$Date
)

# Plot number of TRUE vs FALSE arrests 
p <- data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef", 
                 alpha=0.6, 
                 position = 'identity', 
                 bins=45) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(x = "Year", 
       y = "Number of arrests", 
       title = "True vs False arests over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="")
p

rm(data, p)

#--------------------------------
# Crime over time
#--------------------------------

myNewData <- table(as.Date(criminal_data$Date))
newCount <- newDate <- c()

for (i in 1:length(myNewData)){
  newCount <- append(newCount, myNewData[[i]])
  newDate  <- append(newDate, as.Date(names(myNewData)[i]))
}

dateAndCount = data.frame(month = newDate, air = newCount)

dateAndCount %>%
  mutate(year = year(month),
         month_day = as_date(paste(2021,month(month),day(month), sep = "-"))) %>%
  ggplot(aes(x = month_day, y = air)) +
  labs(x = "Month", 
       y = "Crime rate (in thousands)", 
       title = "Crime over time") +
  geom_line() +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

rm(i, myNewData, newCount, newDate)

#==============================================================================================================
# Data exploration
#==============================================================================================================

explore(criminal_data)

opt <- options("scipen" = 20)
op <- par(mar = c(5,7,4,2) + 0.1)

# Attributes:
# "ID" - unique IDs
table(criminal_data$ID)

# "Case.Number" - unique IDs
table(criminal_data$Case.Number)

# "Date" - data of the crime
table(criminal_data$Date)
hist(table(criminal_data$Date), main = "Date")
my_tab <- table(criminal_data$Date)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Date")
head(my_tab_sort2,54)
#barplot(head(my_tab_sort2,54), main = "Date")
barplot(head(my_tab_sort2,54), las = 1, ylab = "", main = "Date", ylim = c(0,200))
title(line = 5.5)
par(op)
options(opt)


# "Block" - location name
table(criminal_data$Block)
barplot(table(criminal_data$Block), main = "Block")
my_tab <- table(criminal_data$Block)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Block")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,30), main = "Block")
barplot(head(my_tab_sort2,54), las = 1, ylab = "", main = "Block", ylim = c(0,16000))
title(line = 5.5)
par(op)
options(opt)


# "IUCR" - Illinois Uniform Crime Reporting
table(criminal_data$IUCR)
barplot(table(criminal_data$IUCR), main = "IUCR")
my_tab <- table(criminal_data$IUCR)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "IUCR")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,30), main = "IUCR")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "IUCR", ylim = c(0,700000))
title(line = 5.5)
par(op)
options(opt)


# "Primary.Type" - primary description of crime
table(criminal_data$Primary.Type)
barplot(table(criminal_data$Primary.Type), main = "Primary.Type")
my_tab <- table(criminal_data$Primary.Type)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Primary.Type")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,30), main = "Primary.Type")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Primary.Type", ylim = c(0,2000000))
title(line = 5.5)
par(op)
options(opt)

# "Description" - secondary description of crime
table(criminal_data$Description)
barplot(table(criminal_data$Description), main = "Description")
my_tab <- table(criminal_data$Description)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Description")
head(my_tab_sort2,30)
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Description", ylim = c(0,800000))
title(line = 5.5)
par(op)
options(opt)

# "Location.Description" - description of where crime happened
table(criminal_data$Location.Description)
barplot(table(criminal_data$Location.Description), main = "Location.Description")
my_tab <- table(criminal_data$Location.Description)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Location.Description")
head(my_tab_sort2,57)
#barplot(head(my_tab_sort2,60), main = "Location.Description")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Location.Description", ylim = c(0,2000000))
title(line = 5.5)
par(op)
options(opt)


# "Arrest" - if person was arrested or not
table(criminal_data$Arrest)
#barplot(table(criminal_data$Arrest), main = "Arrest")
barplot(table(criminal_data$Arrest), las = 1, ylab = "", main = "Arrest", ylim = c(0,6000000))
title(line = 5.5)
par(op)
options(opt)

# "Domestic" - if person was domestic or not
table(criminal_data$Domestic)
#barplot(table(criminal_data$Domestic), main = "Domestic")
barplot(table(criminal_data$Domestic), las = 1, ylab = "", main = "Domestic", ylim = c(0,7000000))
title(line = 5.5)
par(op)
options(opt)

# "Beat" - Smallest police selected area
table(criminal_data$Beat)
#barplot(table(criminal_data$Beat), main = "Beat")
my_tab <- table(criminal_data$Beat)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Beat")
head(my_tab_sort2,50)
#barplot(head(my_tab_sort2,50), main = "Beat")
barplot(head(my_tab_sort2,50), las = 1, ylab = "", main = "Beat", ylim = c(0,60000))
title(line = 5.5)
par(op)
options(opt)

# "District" - District numbers
table(criminal_data$District)
barplot(table(criminal_data$District), main = "District")
my_tab <- table(criminal_data$District)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
my_tab_sort2
#barplot(my_tab_sort2, main = "District")
barplot(my_tab_sort2, las = 1, ylab = "", main = "District",ylim = c(0,500000))
title(line = 5.5)
par(op)
options(opt)


# "Ward" - Ward number
table(criminal_data$Ward)
barplot(table(criminal_data$Ward), main = "Ward")
my_tab <- table(criminal_data$Ward)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
my_tab_sort2
#barplot(my_tab_sort2, main = "Ward")
barplot(my_tab_sort2, las = 1, ylab = "", main = "Ward",ylim = c(0,350000))
title(line = 5.5)
par(op)
options(opt)

# "Community.Area" = Community Area ID
table(criminal_data$Community.Area)
barplot(table(criminal_data$Community.Area), main = "Community.Area")
my_tab <- table(criminal_data$Community.Area)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
my_tab_sort2
#barplot(my_tab_sort2, main = "Community.Area")
barplot(my_tab_sort2, las = 1, ylab = "", main = "Community Area",ylim = c(0,500000))
title(line = 5.5)
par(op)
options(opt)

# "FBI.Code" - FBI.Code
table(criminal_data$FBI.Code)
barplot(table(criminal_data$FBI.Code), main = "FBI.Code")
my_tab <- table(criminal_data$FBI.Code)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
my_tab_sort2
#barplot(my_tab_sort2, main = "FBI.Code")
barplot(my_tab_sort2, las = 1, ylab = "", main = "FBI.Code", ylim = c(0,2000000))
title(line = 5.5)
par(op)
options(opt)

# "Updated.On" When was last updated
table(criminal_data$Updated.On)
barplot(table(criminal_data$Updated.On), main = "Updated.On")
my_tab <- table(criminal_data$Updated.On)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Updated.On")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,54), main = "Updated.On")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Updated.On", ylim = c(0,3000000))
title(line = 5.5)
par(op)
options(opt)

# "Latitude" - LAT
table(criminal_data$Latitude)
barplot(table(criminal_data$Latitude), main = "Latitude")
my_tab <- table(criminal_data$Latitude)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Latitude")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,55), main = "Latitude")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Latitude", ylim = c(0,16000))
title(line = 5.5)
par(op)
options(opt)

# "Longitude" - LON
table(criminal_data$Longitude)
barplot(table(criminal_data$Longitude), main = "Longitude")
my_tab <- table(criminal_data$Longitude)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
#barplot(head(my_tab_sort2,200), main = "Longitude")
head(my_tab_sort2,30)
#barplot(head(my_tab_sort2,60), main = "Longitude")
barplot(head(my_tab_sort2,30), las = 1, ylab = "", main = "Longitude", ylim = c(0,16000))
title(line = 5.5)
par(op)
options(opt)


#==============================================================================================================
# Data analysis
#==============================================================================================================


dataStatistics(ISR$AGE)
dataStatistics(ISR$HEIGHT)
dataStatistics(ISR$WEIGHT)

drawPlot(ISR, ISR$AGE, "Age")
drawPlot(ISR, ISR$HEIGHT, "Height")
drawPlot(ISR, ISR$WEIGHT, "Weight")

#Sample call
z.test(ISR$AGE, 75, 18)
z.test(ISR$HEIGHT, 75, 18)
z.test(ISR$WEIGHT, 75, 18)


#==============================================================================================================
# Split the data
#==============================================================================================================

