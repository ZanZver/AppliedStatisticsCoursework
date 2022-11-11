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



#==============================================================================================================
# Data import
#==============================================================================================================

# Zans data path
dataPath <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime.csv"
dataPath_lite <- "/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime_lite.csv"

criminal_data <- read.csv(dataPath_lite, stringsAsFactors=TRUE)

# Check the columns
colnames(criminal_data)
# Remove useless columns
criminal_data <- subset(criminal_data, select = -c(X,X.Coordinate, Y.Coordinate, Location) )

#==============================================================================================================
# Data check
#==============================================================================================================



str(criminal_data$Updated.On)

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
barplot(head(my_tab_sort2,200), main = "Date")

# "Block" - location name
table(criminal_data$Block)
barplot(table(criminal_data$Block), main = "Block")
my_tab <- table(criminal_data$Block)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Block")

# "IUCR" - Illinois Uniform Crime Reporting
table(criminal_data$IUCR)
barplot(table(criminal_data$IUCR), main = "IUCR")
my_tab <- table(criminal_data$IUCR)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "IUCR")

# "Primary.Type" - primary description of crime
table(criminal_data$Primary.Type)
barplot(table(criminal_data$Primary.Type), main = "Primary.Type")
my_tab <- table(criminal_data$Primary.Type)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Primary.Type")

# "Description" - secondary description of crime
table(criminal_data$Description)
barplot(table(criminal_data$Description), main = "Description")
my_tab <- table(criminal_data$Description)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Description")

# "Location.Description" - description of where crime happened
table(criminal_data$Location.Description)
barplot(table(criminal_data$Location.Description), main = "Location.Description")
my_tab <- table(criminal_data$Location.Description)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Location.Description")

# "Arrest" - if person was arrested or not
table(criminal_data$Arrest)
barplot(table(criminal_data$Arrest), main = "Arrest")

# "Domestic" - if person was domestic or not
table(criminal_data$Domestic)
barplot(table(criminal_data$Domestic), main = "Domestic")

# "Beat" - Smallest police selected area
table(criminal_data$Beat)
barplot(table(criminal_data$Beat), main = "Beat")
my_tab <- table(criminal_data$Beat)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Beat")


# "District" - District numbers
table(criminal_data$District)
barplot(table(criminal_data$District), main = "District")

# "Ward" - Ward number
table(criminal_data$Ward)
barplot(table(criminal_data$Ward), main = "Ward")

# "Community.Area" = Community Area ID
table(criminal_data$Community.Area)
barplot(table(criminal_data$Community.Area), main = "Community.Area")

# "FBI.Code" - FBI.Code
table(criminal_data$FBI.Code)
barplot(table(criminal_data$FBI.Code), main = "FBI.Code")

# "Year" = year of crime
table(criminal_data$Year)
barplot(table(criminal_data$Year), main = "Year")

# "Updated.On" When was last updated
table(criminal_data$Updated.On)
barplot(table(criminal_data$Updated.On), main = "Updated.On")
my_tab <- table(criminal_data$Updated.On)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Updated.On")

# "Latitude" - LAT
table(criminal_data$Latitude)
barplot(table(criminal_data$Latitude), main = "Latitude")
my_tab <- table(criminal_data$Latitude)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Latitude")

# "Longitude" - LON
table(criminal_data$Longitude)
barplot(table(criminal_data$Longitude), main = "Longitude")
my_tab <- table(criminal_data$Longitude)
my_tab_sort2 <- my_tab[order(my_tab,decreasing = TRUE)]
barplot(head(my_tab_sort2,200), main = "Longitude")


pairs(criminal_data)


# Check the columns
colnames(criminal_data)
# Remove useless columns
criminal_data <- subset(criminal_data, select = -c(ID,Case.Number) )

head(criminal_data$Location,4)

la <- as.matrix(criminal_data[1, "Location"])
la

as.array()
as.matrix()


class(la)


pairs(criminal_data,                     # Data frame of variables
      labels = colnames(data),  # Variable names
      pch = 21,                 # Pch symbol
      bg = rainbow(3)[groups],  # Background color of the symbol (pch 21 to 25)
      col = rainbow(3)[groups], # Border color of the symbol
      main = "Iris dataset",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)          # Font style of the diagonal text






colnames(criminal_data)

head(criminal_data$Date,200)

plot(head(criminal_data$Date,200),head(criminal_data$Block,200))

plot(head(criminal_data$Date,20),head(criminal_data$Block,20))



colnames(criminal_data[1])

colnames(criminal_data)

dataPathGraph <-"/Users/zanzver/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Code/Graphs/" 
for (i in 1:ncol(criminal_data)){
  colName <- colnames(criminal_data[i])
  for (j in 1:ncol(criminal_data)){
    colName2 <- colnames(criminal_data[j])
    #print(paste(colName, colName2, sep = " * "))
    tryCatch(
      expr = {
        jpeg(file=paste(paste(dataPathGraph, i*j, sep = ""), ".jpg", sep = ""))
        plot(head(criminal_data[,colnames(criminal_data[i])],20),
             head(criminal_data[,colnames(criminal_data[j])],20),
             xlab = colnames(criminal_data[i]), 
             ylab = colnames(criminal_data[j]),
             main = paste(colnames(criminal_data[i]), colnames(criminal_data[j]), sep = " * ")
        )
        dev.off() 
      },
      error = function(e){ 
        print(e)
      },
      warning = function(w){
        print(w)
      }
    )
  }
}




jpeg(file=paste(paste(dataPathGraph, 1, sep = ""), ".jpg", sep = ""))
plot(head(criminal_data[,colnames(criminal_data[15])],20),
     head(criminal_data[,colnames(criminal_data[4])],20),
     xlab = colnames(criminal_data[15]), 
     ylab = colnames(criminal_data[4]),
     main = paste(colnames(criminal_data[15]), colnames(criminal_data[4]), sep = " * ")
)
dev.off()

print()

print(paste(paste(dataPathGraph, 1, sep = ""), ".jpg", sep = ""))

colnames(criminal_data[15])
ta
criminal_data[,colnames(criminal_data[4])]

#==============================================================================================================
# Data check
#==============================================================================================================
# Inspect the data
str(criminal_data)
# Check the column names
colnames(criminal_data)

# Data processing

# Contains duplicates




