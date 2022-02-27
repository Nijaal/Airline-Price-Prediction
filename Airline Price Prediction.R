install.packages("readxl")
 
library("readxl")

df = read_excel("train.xlsx")
head(df)
names(df)
str(df)
summary(df)
summary(is.na(df))

df = na.omit(df)
summary(is.na(df))

unique(df$Total_Stops)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "non-stop",0)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "1 stop",1)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "2 stops",2)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "3 stops",3)
df$Total_Stops <- replace(df$Total_Stops,df$Total_Stops == "4 stops",4)
unique(df$Total_Stops)

class(df$Total_Stops) <- "integer"
head(df$Total_Stops)