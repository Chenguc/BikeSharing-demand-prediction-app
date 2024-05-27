# It may take several minutes to install those libraries in Watson Studio
#install.packages("rlang")
#install.packages("tidymodels")

PATH 		= "/Users/uc/Desktop/R Code"
setwd( PATH )

library(tidymodels)
library(tidyverse)
library(stringr)


bike_df <- read_csv("seoul_bike_sharing.csv",
                    col_types = cols( 
                      'DATE' = col_character()
                    ))

str(bike_df)

#Recast DATE as a date
bike_df <- mutate(bike_df, DATE = as.Date(DATE, format = "%d/%m/%Y"))
class(bike_df$DATE)
summary(bike_df)

#Cast HOURS as a categorical variable
bike_df$HOUR <- as.factor(bike_df$HOUR)
str(bike_df)


sum(is.na(bike_df))

#calculate how many Holidays there are.
a<- bike_df %>%  count(HOLIDAY == "Holiday")
print(a)
b<- bike_df %>%  count(HOLIDAY)
print(b)


#Calculate the percentage of records that fall on a holiday.
Holiday_percentage <- 400/(8057+408) *100
round(Holiday_percentage,2)


#group the data by SEASONS, calculate the seasonal total rainfall and snowfall.

seasonal_totals <- bike_df %>%
  group_by(SEASONS) %>%
  summarize(total_rainfall = sum(RAINFALL),
            total_snowfall = sum(SNOWFALL))

print(seasonal_totals)

#RENTED_BIKE_COUNT vs DATE.
bike_df %>% ggplot( aes(x = DATE, y= RENTED_BIKE_COUNT))+
  geom_point(alpha=0.5)


#RENTED_BIKE_COUNT time series, add HOURS as the colour
bike_df %>% ggplot( aes(x = DATE, y= RENTED_BIKE_COUNT, color= HOUR))+
  geom_point(alpha=0.5)

#Create a histogram overlaid with a kernel density curve
ggplot(bike_df, aes(x = RENTED_BIKE_COUNT, y = ..density..)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "blue") +
  labs(title = "Histogram with Kernel Density Curve",
       x = "Rented Bike Count",
       y = "Density")

#Use a scatter plot to visualize the correlation between RENTED_BIKE_COUNT and TEMPERATURE by SEASONS
ggplot(bike_df, aes(x= RENTED_BIKE_COUNT, y= TEMPERATURE, color = HOUR))+
  geom_point(alpha=0.5)+
  facet_wrap(~SEASONS)

ggplot(bike_df) +
  geom_point(aes(x=TEMPERATURE,y=RENTED_BIKE_COUNT,colour=HOUR),alpha=1/5)

#boxplots of RENTED_BIKE_COUNT vs. HOUR grouped by SEASON

ggplot(bike_df, aes(x= HOUR, y= RENTED_BIKE_COUNT))+
  geom_boxplot()+
  facet_wrap(~SEASONS)


#calculate the daily total rainfall and snowfall

daily_totals <- bike_df %>%
  group_by(DATE) %>%
  summarize(total_rainfall = sum(RAINFALL),
            total_snowfall = sum(SNOWFALL))

#head(daily_totals)

ggplot(daily_totals, aes(x = DATE)) +
  geom_line(aes(y = total_rainfall, color = "Rainfall")) +
  geom_line(aes(y = total_snowfall, color = "Snowfall")) +
  labs(x = "Date", y = "Total", color = "Type") +  # Set axis labels and color legend
  ggtitle("Daily Total Rainfall and Snowfall") +  # Set plot title
  theme_minimal()  # Apply minimal theme


#Snow days
days_with_snowfall <- daily_totals %>%
  filter(total_snowfall > 0) %>%
  nrow()

print(days_with_snowfall)
