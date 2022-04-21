install.packages("readxl")
install.packages("superml")
install.packages("ggplot2")
install.packages("dplyr")

library("ggplot2") 
library("readxl")
library("superml")
library("dplyr")
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
total_stops <- function(dx){
  unique(dx$Total_Stops)
  dx$Total_Stops <- replace(dx$Total_Stops,dx$Total_Stops == "non-stop",0)
  dx$Total_Stops <- replace(dx$Total_Stops,dx$Total_Stops == "1 stop",1)
  dx$Total_Stops <- replace(dx$Total_Stops,dx$Total_Stops == "2 stops",2)
  dx$Total_Stops <- replace(dx$Total_Stops,dx$Total_Stops == "3 stops",3)
  dx$Total_Stops <- replace(dx$Total_Stops,dx$Total_Stops == "4 stops",4)
  return(dx$Total_Stops)
}
df$Total_Stops <- total_stops(df)

#Suitable metrics for the column
durat <- function(dx){
  duration <- c()
  for (each in dx$Duration) {
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
  return(duration)
}
df["Duration (in mins)"] <- durat(df)

date <- function(dx){
  dates <- c()
  for (each in dx$Date_of_Journey) {
    date <- as.Date(each,"%d/%m/%Y")
    dates <- c(dates, date)
  }
  dx$`Date of Journey` <- dates
  dx$`Date of Journey` <- as.Date(dx$`Date of Journey`,origin = "1970-01-01")
  return(dx$`Date of Journey`)
}
df$`Date of Journey` <- date(df)

airline <- function(dx){
  unique(dx$Airline)
  lb = LabelEncoder$new()
  lb$fit(dx$Airline)
  dx$Airline = lb$fit_transform(dx$Airline)
  return(dx$Airline)
}
df$Airline <- airline(df)

#Dropping columns which are of no use
data <- subset(df, select = -c(Route,Dep_Time,Arrival_Time,`Additional Info`,Duration,Source,Destination,Date_of_Journey))
View(data)

#Histogram
ggplot(data, aes(x=Price)) + geom_histogram()

#Airline, Price
plot(data$Airline, data$Price)                                                                                                    
ggplot(data=df, aes(x=Airline, y=Price))+geom_boxplot()

#Duration, Price
ggplot(data=data, aes(x=`Duration (in mins)`, y=Price))+geom_line()

#Total Stop, Price
ggplot(data=data, aes(x=Total_Stops, y=Price))+geom_jitter()

model <- lm( Price ~ Airline + Total_Stops + `Duration (in mins)` + `Date of Journey`,data = data)
print(model)

test = read_excel("train.xlsx")
test$Total_Stops <- total_stops(test)
test$`Duration (in mins)` <- durat(test)
test$`Date of Journey` <- date(test)
test$Airline <- airline(test)
result <- predict(model,test)

summary(model)$coefficient

result_data <- data.frame(actual= head(test$Price), predicted=head(predict(model)))
result_data
