install.packages("readxl")
install.packages("superml")
 
library("readxl")
library("superml")
setwd("C:\\Users\\Acer\\OneDrive\\Desktop\\R Project\\Airline Fair Prediction")

df = read_excel("train.xlsx")
View(df)
head(df)
names(df)
str(df)
summary(df)
summary(is.na(df))

miss = which(is.na(df))
df[miss,]
#Handling missing values
df = na.omit(df)
summary(is.na(df))

#Suitable data type for the column
unique(df$Total_Stops)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "non-stop",0)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "1 stop",1)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "2 stops",2)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "3 stops",3)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "4 stops",4)
unique(df$Total_Stops)

class(df$Total_Stops) <- "integer"
head(df$Total_Stops)

#Suitable metrics for the column
duration <- c()
for (each in df$Duration) {
  dur <- 0
  if(unlist(gregexpr("h",each)) != -1){
    dur <- dur + as.integer(substr(each,1,unlist(gregexpr("h",each))-1))*60
  }
  if(unlist(gregexpr("m",each)) != -1){
    if(unlist(gregexpr("h",each)) != -1){
      dur <- dur + as.integer(substr(each,unlist(gregexpr("h",each))+1,unlist(gregexpr("m",each))-1))
    }
    else{
      dur <- dur + as.integer(substr(each,1,unlist(gregexpr("h",each))-1))
    }
  }
  duration <- c(duration,dur)
}
df["Duration (in mins)"] <- duration

dates <- c()
for (each in df$Date_of_Journey) {
  date <- as.Date(each,"%d/%m/%Y")
  dates <- c(dates, date)
}
df$`Date of Journey` <- dates
df$`Date of Journey` <- as.Date(df$`Date of Journey`,origin = "1970-01-01")

unique(df$Airline)
lb = LabelEncoder$new()
lb$fit(df$Airline)
df$Airline = lb$fit_transform(df$Airline)

#Dropping columns which are of no use
data <- subset(df, select = -c(Route,Dep_Time,Arrival_Time,`Additional Info`,Duration,Source,Destination,Date_of_Journey))
View(data)

