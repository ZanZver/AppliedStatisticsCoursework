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
library('dplyr')


#==============================================================================================================
# Data import
#==============================================================================================================

# Main dataset
dataPath <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime.csv"
dataPath_lite <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime_lite.csv"
# Supporting dataset
IUCR_Data_Path <-"/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/IUCR_Codes.csv"
Common_Areas_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/CommAreas.csv"
Police_Beat_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/PoliceBeatDec2012.csv"
Police_District_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/PoliceDistrictDec2012.csv"
NIBRS_Offense_Codes_Path <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/SupportingDatasets/NIBRS_Offense_Codes.csv"

criminal_data <- read.csv(dataPath, stringsAsFactors=TRUE)
IUCR_Data <- read.csv(IUCR_Data_Path, stringsAsFactors=TRUE)
Common_Areas <- read.csv(Common_Areas_Path, stringsAsFactors=TRUE)
Police_Beat <- read.csv(Police_Beat_Path, stringsAsFactors=TRUE)
Police_District <- read.csv(Police_District_Path, stringsAsFactors=TRUE)
NIBRS_Offense_Codes <- read.csv(NIBRS_Offense_Codes_Path, stringsAsFactors=TRUE)


# Check the columns
colnames(criminal_data)
# Remove useless columns
criminal_data <- subset(criminal_data, select = -c(X.Coordinate, Y.Coordinate, Location, Year))

#==============================================================================================================
# Data cleaning
#==============================================================================================================

colnames(criminal_data)

for (i in colnames(criminal_data)){
  print(i)
  tmp <- criminal_data %>% select(all_of(i))
  print(sum(is.na(tmp)))
}

criminal_data <- na.omit(criminal_data)

for (i in colnames(criminal_data)){
  print(i)
  tmp <- criminal_data %>% select(all_of(i))
  print(sum(is.na(tmp)))
}

sapply(criminal_data, mode)
sapply(criminal_data, class)

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

rm(tmp,i)

mystr <- sapply(list(levels(IUCR_Data$IUCR)), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$IUCR),]

mystr <- sapply(list(levels(Common_Areas$AREA_NUMBE)), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$Community.Area),]

mystr <- sapply(list(levels(Police_Beat$BEAT_NUM)), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$Beat),]

mystr <- sapply(list(levels(Police_District$DIST_NUM)), paste, collapse = "|")
criminal_data <- criminal_data[grepl(mystr, criminal_data$District),]


#==============================================================================================================
# Data exploration
#==============================================================================================================

opt <- options("scipen" = 20)
op <- par(mar = c(5,7,4,2) + 0.1)

# Attributes:
# "ID" - unique IDs
table(criminal_data$ID)

# "Case.Number" - unique IDs
table(criminal_data$Case.Number)

# "Date" - data of the crime
table(criminal_data$Date)
barplot(table(criminal_data$Date), main = "Date")
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


#==============================================================================================================
# Split the data
#==============================================================================================================

