#Load data into data frame df and replace blanks with NA
df<-read.csv("titanic3_original.csv", na.strings = "")
#Replace missing data in port of embarkation with 'S'
df$embarked[is.na(df$embarked)]<-"S"
#Replace the empty age cells with mean of age then round the value
df$age[is.na(df$age)]<-mean(df$age,na.rm = TRUE)
df$age <-round(df$age,2)
#Convert boat columns to character and replace NA values with 'None'
df$boat<-as.character(df$boat)
df$boat[is.na(df$boat)]<- "None"
#create a new column 'has_cabin_number with value 1 if there is a cabin number and 0 otherwise
df$has_cabin_number<-ifelse(is.na(df$cabin),0,1)
write.csv(df,"titanic_clean.csv")