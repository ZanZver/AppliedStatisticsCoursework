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
library(BSDA)

#==============================================================================================================
# Functions
#==============================================================================================================

format_number <- function(x) {
  if (nchar(as.character(x)) != 3){
    x = paste(x,"0", sep = "")
  }
  return(as.numeric(x))
}

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
  myMean     <- formatC(mean(data),width=5,format='f',digits=2,flag='0')
  myMedian   <- formatC(median(data),width=5,format='f',digits=2,flag='0')
  mySd       <- formatC(sd(data),width=5,format='f',digits=2,flag='0')
  myMin      <- formatC(min(data),width=5,format='f',digits=2,flag='0')
  myMax      <- formatC(max(data),width=5,format='f',digits=2,flag='0')
  mySkewness <- formatC(skewness(data),width=5,format='f',digits=2,flag='0')
  myLength   <- format(length(data), digits = 6)
  
  l1 <- "+-------------------------------------------+\n"
  l2 <- paste(paste("| Mean                 |", myMean), "             |\n")
  l3 <- paste(paste("| Median               |", myMedian),"             |\n")
  l4 <- paste(paste("| Standard deviation   |", mySd),"             |\n")
  l5 <- paste(paste("| Min                  |", myMin),"             |\n")
  l6 <- paste(paste("| Max                  |", myMax),"            |\n")
  l7 <- paste(paste("| Skewness             |", mySkewness),"             |\n")
  l8 <- paste(paste("| Number of attributes |", myLength),"            |\n")
  l9 <- "+-------------------------------------------+\n"
  
  cat(paste(l1, l2, l3, l4, l5, l6, l7, l8, l9, sep = ""))
  
  
  #print(paste("Mean:", mean(data)))
  #print(paste("Median:", median(data)))
  #print(paste("Standard deviation:", sd(data)))
  #print(paste("Min:", min(data)))
  #print(paste("Max:", max(data)))
  #print(paste("Skewness:", skewness(data)))
  #print(paste("Number of attributes:", length(data)))
}

drawPlot <- function(data, col, xName){
  p <- data %>%
        #filter( price<300 ) %>%
        ggplot( aes(x=col)) +
        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
        theme_ipsum() +
        labs(x = xName, title = paste("Plot of", xName, sep = " ")) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  # p <- ggplot(data, aes(x=col)) + 
  #   geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
  #   geom_density(alpha=.2, fill="#FF6666") +
  #   labs(x = xName, title = paste("Plot of", xName, sep = " ")) +
  #   theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  print(p)
}

#z.test = function(a, mu, var){
#  return(mean(a) - mu) / (sqrt(var / length(a)))
#}
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

ncol(ISR2016_data)

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

#==============================================================================================================
# Data cleaning
#==============================================================================================================

ISR[ISR==0] <- NA
ISR[ISR=="J"] <- NA

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


# ISR <- transform(ISR,
#                  AGE = as.integer(AGE),
#                  HEIGHT = as.numeric(HEIGHT),
#                  WEIGHT = as.numeric(WEIGHT),
#                  RACE_CODE_CD = as.character(RACE_CODE_CD),
#                  CITY = as.character(CITY),
#                  DISTRICT = as.integer(DISTRICT),
#                  BEAT = as.integer(BEAT)
# )

tempHi = as.character(ISR$HEIGHT)
tempHi = as.numeric(gsub("^(.)(0)(.*)$", "\\1\\3", tempHi))
count <- 1
for (i in tempHi){
  tempHi[count] <- format_number(i)
  count <- count + 1
}
ISR$HEIGHT <- round(((tempHi* 0.01) * 30.48), 2)

ISR <- transform(ISR,
                 AGE = as.integer(AGE),
                 HEIGHT = as.numeric(HEIGHT),
                 WEIGHT = as.numeric(WEIGHT),
                 RACE_CODE_CD = as.character(RACE_CODE_CD),
                 CITY = as.character(CITY),
                 DISTRICT = as.integer(DISTRICT),
                 BEAT = as.integer(BEAT)
                 )

#explore(ISR)

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

criminal_data <- subset(criminal_data, Latitude>=41)
criminal_data <- subset(criminal_data, Latitude<=43)

criminal_data <- subset(criminal_data, Longitude>=-89)
criminal_data <- subset(criminal_data, Longitude<=-85)


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


# Limit age from 12 (before that parents would be responsible) to 120 (that is feezable maximum...).
ISR <- subset(ISR, AGE>=12)
ISR <- subset(ISR, AGE<=120)

# Smallest person is 55cm, wile the tallest person is 270, we are using limit of 250 since this is where the extreme is in our dataset
ISR <- subset(ISR, HEIGHT>=55)
ISR <- subset(ISR, HEIGHT<=250)

# Minimum weight is 25kg (since this is the lightest person on the globe), the heaviest person is 410kg which is another extreme, therefore we did set our max to 200kg 
ISR <- subset(ISR, WEIGHT>=25)
ISR <- subset(ISR, WEIGHT<=200)

rm(mystr, count, i, tempHi)

#explore(ISR)

#==============================================================================================================
#  Data exploration - Crime over time
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
  scale_fill_manual(values=c("#69b3a2", "#404080"), labels=c("Not\narrested", "Arrested")) +
  labs(x = "Year", 
       y = "Number of arrests", 
       title = "Criminals arrested vs not arrested over time") +
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
       y = "Crime rate", 
       title = "Crime over time between 2001 and 2022") +
  geom_line() +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
  facet_wrap(~year) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90,
                                                                           vjust = 0.5,
                                                                           hjust = 1))

# Subset 1
tmp <- (subset(criminal_data, Date> "2020-05-28" & Date < "2020-06-28"))
data <- data.frame(
  type = c( tmp$Arrest[tmp$Arrest == "TRUE"], 
            tmp$Arrest[tmp$Arrest == "FALSE"]),
  value = tmp$Date
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
       title = "Criminals arrested vs not arrested in Jun") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="")
p

# Subset 2
tmp <- (subset(criminal_data, Date> "2020-05-30" & Date < "2020-06-04"))
data <- data.frame(
  type = c( tmp$Arrest[tmp$Arrest == "TRUE"], 
            tmp$Arrest[tmp$Arrest == "FALSE"]),
  value = tmp$Date
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
       title = "Criminals arrested vs not arrested in first week of Jun") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="")
p

rm(i, myNewData, newCount, newDate, p, tmp)

#==============================================================================================================
# Data analysis
#==============================================================================================================
explore(ISR)

dataStatistics(ISR$AGE)
dataStatistics(ISR$HEIGHT)
dataStatistics(ISR$WEIGHT)

average(ISR$AGE)

# Make the histogra



drawPlot(ISR, ISR$AGE, "Age")
drawPlot(ISR, ISR$HEIGHT, "Height")
drawPlot(ISR, ISR$WEIGHT, "Weight")

#==============================================================================================================
# Z test
#==============================================================================================================

library(BSDA)
z.test(ISR$AGE, 
       mu=42, 
       alternative="greater", 
       sigma.x = sd(ISR$AGE))

#==============================================================================================================
# Correlation and linear regression
#==============================================================================================================

cor.test(ISR$HEIGHT, ISR$WEIGHT)

hist(criminal_data$Longitude)
plot(HEIGHT ~ WEIGHT, data = ISR)

person.lm <- lm(WEIGHT ~ HEIGHT, data = ISR)
summary(person.lm)

# 
# lte_criminal_data$Latitude<-signif(lte_criminal_data$Latitude, digits = 4)
# lte_criminal_data$Longitude<-signif(lte_criminal_data$Longitude, digits = 4)
# #0.00000000000000022
# 
# library(ggplot2)
# library(hrbrthemes)
# library(hexbin)
# 
# ggplot(ISR, aes(x=HEIGHT, y=WEIGHT) ) +
#   geom_hex(bins = 70) +
#   scale_fill_continuous(type = "viridis") +
#   theme_bw()
# 
# 
# 
# library(ggplot2)
# library(hrbrthemes)
# 
# # Create dummy data
# data <- data.frame(
#   cond = rep(c("condition_1", "condition_2"), each=10), 
#   my_x = 1:100 + rnorm(100,sd=9), 
#   my_y = 1:100 + rnorm(100,sd=16) 
# )
# 
# head(data)
# 
# # Basic scatter plot.
# p1 <- ggplot(data, aes(x=my_x, y=my_y)) + 
#   geom_point( color="#69b3a2") +
#   theme_ipsum()
# 
# p1 <- ggplot(ISR, aes(x=HEIGHT, y=WEIGHT)) + 
#   geom_point( color="#69b3a2") +
#   theme_ipsum()
# 
# p1
# 
# # with linear trend
# p2 <- ggplot(data, aes(x=my_x, y=my_y)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", se=FALSE) +
#   theme_ipsum()
# 
# p2 <- ggplot(ISR, aes(x=HEIGHT, y=WEIGHT)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", se=FALSE) +
#   theme_ipsum()
# 
# p2
# 
# # linear trend + confidence interval
# p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme_ipsum()
# 
# tmp2 <- head(criminal_data, 2000)
# 
# p3 <- ggplot(tmp2, aes(x=Latitude, y=Longitude)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme_ipsum()
# 
# p3
# 
# 
# p3 <- ggplot(criminal_data, aes(x=Latitude, y=Longitude)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme_ipsum()
# p3
# 
# 
# lte_criminal_data <- head(criminal_data,100)
# head(lte_criminal_data)
# lte_criminal_data <- lte_criminal_data[,c("Latitude", "Longitude","District")]
# lte_criminal_data$Latitude<-signif(lte_criminal_data$Latitude, digits = 4)
# lte_criminal_data$Longitude<-signif(lte_criminal_data$Longitude, digits = 4)
# table(lte_criminal_data)
# 
# head(lte_criminal_data)
# 
# library(ggmap)
# library(tidyverse)
# mymap <- get_map("Chicago.gov")
# ggmap(mymap)
# 
# 
# 
# 
# 
# 
# library(data.table)
# lte_criminal_data <- setDT(lte_criminal_data)[,list(Count=.N),names(lte_criminal_data)]
# 
# head(lte_criminal_data)
# ggplot() +
#   geom_polygon(data = lte_criminal_data, aes(x=Longitude, y = Latitude), 
#                fill="grey", alpha=0.3) +
#   geom_point( data=lte_criminal_data, aes(x=Longitude, y=Latitude)) +
#   theme_void() + ylim(40,49) + 
#   coord_map() 
# 
# tmp2 <- map_data("world") %>% filter(region=="USA")
# table(tmp2$subregion)
# view(tmp2)
# head(tmp2)
# library(viridis)
# # Left: use size and color
# ggplot() +
#   geom_polygon(data = tmp2, 
#                aes(x=long, y = lat), 
#                fill="grey", alpha=0.3) +
#   geom_point( data=lte_criminal_data, 
#               aes(x=Longitude, y=Latitude, size=Count, color=Count)) +
#   scale_size_continuous(range=c(1,12)) +
#   scale_color_viridis(trans="log") +
#   theme_void() + ylim(40,49) + coord_map() 
# 
# library(ggplot2)
# library(dplyr)
# 
# # Get the world polygon and extract UK
# library(maps)
# UK <- map_data("world") %>% filter(region=="UK")
# head(UK)
# data <- world.cities %>% filter(country.etc=="UK")
# head(data)
# 
# data %>%
#   arrange(pop) %>% 
#   mutate( name=factor(name, unique(name))) %>% 
#   ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), 
#                fill="grey", alpha=0.3) +
#   geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
#   scale_size_continuous(range=c(1,12)) +
#   scale_color_viridis(trans="log") +
#   theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")


#==============================================================================================================
# Split the data
#==============================================================================================================

