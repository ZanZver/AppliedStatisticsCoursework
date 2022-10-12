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


#==============================================================================================================
# Data check
#==============================================================================================================
# Inspect the data
str(criminal_data)
# Check the column names
colnames(criminal_data)

# Data processing

# Contains duplicates




