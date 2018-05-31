library(TextDeduplicate)
#****************************************************************************************************************************************************
#    													PACKAGES TO BE USED IN THE ANALYSIS
#****************************************************************************************************************************************************
library(plyr)
library(fuzzywuzzyR)

#Setting Working Directory and setting options
setwd("C:/Users/chirag.jain/Desktop/Dodge_Reed_Duplicates/07042017")
options(stringsAsFactors = F)
Reed <- read_input_file("C:/Users/chirag.jain/Desktop/Projects_06212017_v2.csv")
Dodge <- read_input_file("C:/Users/chirag.jain/Desktop/Projects_06212017_v2.csv")

lis <- clean_data(Reed, Dodge)
Reed <- lis[[1]]
Dodge <- lis[[2]]

#****************************************************************************************************************************************************
#															DATA TO BE USED IN THE ANALYSIS
#****************************************************************************************************************************************************
Reed = Reed[Reed$Cleaned_Address != "various locations", ]
Reed = Reed[Reed$Cleaned_Address != "", ]
Dodge = Dodge[Dodge$Cleaned_Address != "various locations", ]
Dodge = Dodge[Dodge$Cleaned_Address != "", ]


#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
#*********************************STEP 2 - Setting up important columns and getting list of unique cities present in the data*********************
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************

#changing important columns to upper case for better visibilty
Reed$CITY_FINAL__C = toupper(Reed$CITY_FINAL__C)
Reed$CITY_FINAL__C<-as.character(Reed$CITY_FINAL__C)
Reed$Cleaned_Address = toupper(Reed$Cleaned_Address)
Reed$Cleaned_Name = toupper(Reed$Cleaned_Name)

Dodge$CITY_FINAL__C = toupper(Dodge$CITY_FINAL__C)
Dodge$CITY_FINAL__C<-as.character(Dodge$CITY_FINAL__C)
Dodge$Cleaned_Address = toupper(Dodge$Cleaned_Address)
Dodge$Cleaned_Name = toupper(Dodge$Cleaned_Name)

#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
#*********************************STEP 3 - Find the closet matches based on Address and then Name******************************************************
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************

#Storing all unique cities present in the data
Unique_Dodge_State = unique(Dodge$STATE_FINAL__C)
#Initiating data frame to store final result
df<-data.frame()

matcher <- FuzzMatcher$new()
for(k in 1 : length (Unique_Dodge_State))
{
  Unique_Dodge_City = unique(Dodge[which(Dodge$STATE_FINAL__C==Unique_Dodge_State[k]),]$CITY_FINAL__C)
  for (i in 1 : length (Unique_Dodge_City)){
    #Assigning data for current city to a local variable
    dodge = Dodge[which(Dodge$STATE_FINAL__C==Unique_Dodge_State[k] & Dodge$CITY_FINAL__C==Unique_Dodge_City[i]),]
    reed =  Reed[which(toupper(Reed$STATE_FINAL__C)==toupper(Unique_Dodge_State[k]) & toupper(Reed$CITY_FINAL__C)==toupper(Unique_Dodge_City[i])),]
    unique_reed_address <- unique(reed$Cleaned_Address)
    
    #Function to get closet matches
    
    for(j in 1:nrow(dodge)) {
      str <- dodge$Cleaned_Address[j]
      #Get 10 closest records based on address
      closest <- GetCloseMatches(str, sequence_strings = unique_reed_address, n = 10, cutoff = 0.5)
      if(length(closest)==0) {
        closest <- cbind(dodge[j,], matrix(as.character(NA), ncol=ncol(reed)))
        colnames(closest)[(ncol(dodge)+1):ncol(closest)] <- paste0("Reed_", colnames(reed))
      }
      else {
        closest <- data.frame(Cleaned_Address = closest)
        closest <- merge(closest, reed,by.x = "Cleaned_Address",by.y = "Cleaned_Address", all.x=TRUE)
        
        colnames(closest) <- paste0("Reed_", colnames(closest))
        closest <- cbind(dodge[j,], closest)
      }
      df <- rbind.fill(df, data.frame(closest))
    }
  }
}
#Get 10 similarity metrics for address
df <- get_ratios(data = df, allow_remove = F, colname1 = "Cleaned_Address", colname2 = "Reed_Cleaned_Address")
colnames(df)[33:42] <- paste0("Address_", colnames(df)[33:42])

#Get 10 similarity metrics for name
df <- get_ratios(data = df, allow_remove = F, colname1 = "Cleaned_Name", colname2 = "Reed_Cleaned_Name")
colnames(df)[43:52] <- paste0("Name_", colnames(df)[43:52]) 
#Take average of all above mentioned metrics
df$Similarity<-apply(df[c(33:52)],1,mean)
df1 = df[c(1:32,53)]

#Write output to a CSV file
write.table(Reed, file = "Reed_Clean.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")