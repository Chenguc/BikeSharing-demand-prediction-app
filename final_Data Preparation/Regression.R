#Linear Regression Models

# 1.Split data into training and testing datasets
# 2.Build a linear regression model using only the weather variables
# 3.Build a linear regression model using both weather and date variables
# 4.Evaluate the models and identify important variables

PATH 		= "/Users/uc/Desktop/R Code/final_Data Preparation"
setwd( PATH )

library(tidymodels)
library(tidyverse)
library(stringr)

dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)
df <- bike_sharing_df
# Use the `initial_split()`, `training()`, and `testing()` functions to split the dataset
# With seed 1234
set.seed(1234)
# prop = 3/4 / 0.75
# train_data 
# test_data

split <- initial_split(df, prop = 3/4)

# Extract the training and testing sets
train_data <- training(split)
test_data <- testing(split)

dim(train_data)
dim(test_data)
dim(df)

#Build a linear regression model using weather variables only

# Use `linear_reg()` with engine `lm` and mode `regression`
lm_model_weather <- lm(RENTED_BIKE_COUNT ~ TEMPERATURE, data = train_data)
summary(lm_model_weather)
lm_model_weather$coefficient
# Fit the model called `lm_model_weather`
# RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL,  with the training data
lm_model_weather <- lm(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL, data = train_data)
summary(lm_model_weather)
lm_model_weather$coefficient

print(lm_model_weather$fit)

#Build a linear regression model using all variables

# Fit the model called `lm_model_all`
# `RENTED_BIKE_COUNT ~ .` means use all other variables except for the response variable
lm_model_all <- lm(RENTED_BIKE_COUNT ~ ., data = train_data)
summary(lm_model_all)

summary(lm_model_all$fit)
plot(lm_model_all)

#Model evaluation and identification of important variables

# Use predict() function to generate test results for `lm_model_weather` and `lm_model_all`
# and generate two test results dataframe with a truth column:
predict_weather <- predict( lm_model_weather , newdata = test_data)
predict_all <- predict( lm_model_all , newdata = test_data)

# test_results_weather for lm_model_weather model
actual_values <- test_data$RENTED_BIKE_COUNT
# test_results_all for lm_model_all

# rsq_weather <- rsq(...)
# rsq_all <- rsq(...)
mse <- mean(lm_model_weather$residuals^2)
rmse <- sqrt(mse)
rsquared <- summary(lm_model_weather)$r.squared



# rmse_weather <- rmse(...)
# rmse_all <- rmse(...)

mse_all <- mean(lm_model_all$residuals^2)
rmse_all <- sqrt(mse_all)
rsquared_all <- summary(lm_model_all)$r.squared

print(rmse)
print(rsquared)
print(rmse_all)
print(rsquared_all)


lm_model_all$coefficients

# Sort coefficient list
coefficients_df <- data.frame(lm_model_all$coefficients)
names(coefficients_df)[1] <- paste("coefficients") # works
coefficients_df <- replace_na(coefficients_df, list(coefficients = 0))
coefficients_df



library(broom)

# Extract coefficients and tidy them up
coefficients_tidy <- tidy(lm_model_all$coefficients)

names(coefficients_tidy)[1] <- paste("Variables") # works
names(coefficients_tidy)[2] <- paste("Coefficients") # works
#remove na
aa <- coefficients_tidy$Coefficients %>% replace_na (0)
coefficients_tidy$Coefficients <- aa

# Print the tidy coefficients
coefficients_tidy


# Visualize the list using ggplot and geom_bar
# Sort coefficients in descending order
#coefficients_df <- coefficients_df[order(-coefficients_df$coefficients), ]

# Sort coefficients in descending order
coefficients_tidy <- coefficients_tidy[order(-coefficients_tidy$Coefficients), ]

# Create a bar plot
ggplot(coefficients_tidy, aes(x = Variables, y = Coefficients)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Coefficients", y = "Variables") +  # Swap x and y labels
  coord_flip()  # Flip the axes to make it horizontal

