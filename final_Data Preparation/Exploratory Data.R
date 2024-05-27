#Exploratory Data Analysis


PATH 		= "/Users/uc/Desktop/R Code"
setwd( PATH )

bike_df <- read_csv("seoul_bike_sharing.csv",
                    col_types = cols( 
                      'DATE' = col_character()
                    ))

str(bike_df)

#Recast DATE column

bike_df <- mutate(bike_df, DATE = as.Date(DATE, format = "%d/%m/%Y"))
class(bike_df$DATE)
summary(bike_df)

bike_df$HOUR <- as.factor(bike_df$HOUR)

str(bike_df)
#check no missing values
sum(is.na(bike_df))


#Descriptive Statistics

summary(bike_df)



