# Original file path
CSVfilepath <- "~/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime.csv"
# Save CSV to dataframe
wholeData <- read.csv(CSVfilepath,stringsAsFactors = TRUE, header = TRUE, sep = ",")

# Get first 10000 rows as lite dataframe
liteData <- head(wholeData, 10000)
# Declare lite data path
CSVLitePath <- "~/Documents/BCU2/Masters/CMP7205-A-S1-2022:3_Applied_Statistics/Coursework/CourseworkAppliedStatistics/Data/Chicago_Crime_lite.csv"
# Save lite dataset to the folder
write.csv(liteData, CSVLitePath)

