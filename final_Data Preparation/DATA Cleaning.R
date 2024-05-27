library(tidyverse)

PATH 		= "/Users/uc/Desktop/R Code/final_Data Preparation"
setwd( PATH )

#Standardize column names for all collected datasets

dataset_list <- c('raw_bike_sharing_systems.csv', 'raw_seoul_bike_sharing.csv', 'raw_cities_weather_forecast.csv', 'raw_worldcities.csv')

for (dataset_name in dataset_list) {
  # Read dataset
  dataset <- read_csv(dataset_name)
  
  # Standardize column names
  colnames(dataset) <- toupper(colnames(dataset))  # Convert column names to uppercase
  colnames(dataset) <- str_replace_all(colnames(dataset), "\\s+", "_")  # Replace whitespace with underscores
  
  # Save the dataset
  write.csv(dataset, dataset_name, row.names = FALSE)
}

for (dataset_name in dataset_list){
  dataset <- read_csv(dataset_name)
  print(summary(dataset))
  # Print a summary for each data set to check whether the column names were correctly converted
}



#Process the web-scraped bike sharing system dataset¶

bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv")
str(bike_sharing_df)

sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)

sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)

# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) grepl("[^0-9]", strings)

sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)

# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)

# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)

# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)

###Remove undesired reference links using regular expressions

# remove reference link
remove_ref <- function(strings) {
  ref_pattern <- "\\[\\d+\\]"
  # Replace all matched substrings with a white space using str_replace_all()
  clean<- str_replace_all(strings, ref_pattern, "")
  # Trim the reslt if you want
  clean <- trimws(clean)
  
  return(clean)
}

# sub_bike_sharing_df %>% mutate(column1=remove_ref(column1), ... )
result<-sub_bike_sharing_df %>%
  mutate(
    CITY = remove_ref(CITY),
    SYSTEM = remove_ref(SYSTEM)
  )

result %>% 
  select(CITY, SYSTEM, BICYCLES) %>% 
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))


#Extract the numeric value using regular expressions

# Extract the first number
extract_num <- function(columns){
  # Define a digital pattern
  digital_pattern <- "\\d+"
  
  # Find the first match using str_extract
  first_number <- str_extract(columns, digital_pattern)
  
  # Convert the result to numeric using as.numeric()
  first_number <- as.numeric(first_number)
  #first_number <- as.na(first_number)
  
  # Return the first number
  return(first_number)
}


# Use the mutate() function on the BICYCLES column
#BICYCLE still in chr, change to numeric and replace na =0 

result <- result %>% 
  mutate(
    BICYCLES = replace(BICYCLES, is.na(BICYCLES), 0),
    BICYCLES = as.numeric(BICYCLES)
  )


summary(result)
str(result)
head(result)


write.csv(result, "bike_sharing_systems.csv", row.names=FALSE)





#Cleaning seoul_bike_sharing data
bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")
summary(bike_sharing_df)
dim(bike_sharing_df)

# Drop rows with `RENTED_BIKE_COUNT` column == NA
# Print the dataset dimension again after those rows are dropped
dim(bike_sharing_df)

#check the missing values in the TEMPERATURE column.
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))

# Calculate the summer average temperature
a<- bike_sharing_df %>% 
  filter(SEASONS == 'Summer')

temp_mean <- mean(a$TEMPERATURE, na.rm = TRUE)
# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df <- bike_sharing_df %>%
  mutate(TEMPERATURE = replace_na(TEMPERATURE, temp_mean))
# Print the summary of the dataset again to make sure no missing values in all columns
summary(bike_sharing_df)

write.csv(bike_sharing_df, "seoul_bike_sharing.csv", row.names = FALSE)





#Create indicator (dummy) variables for categorical variables


# Using mutate() function to convert HOUR column into character type
df<- bike_sharing_df %>% 
  mutate(HOUR = as.character(HOUR))

#sapply(df$SEASONS, unique)
unique(df$SEASONS)
unique(df$HOLIDAY)
unique(df$FUNCTIONING_DAY)
unique(df$HOUR)

# Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns.
#Convert SEASONS
df_encoded <- df %>%
  mutate(dummy = 1) %>%
  spread(key = SEASONS, value = dummy, fill = 0)

#Convert Functioning_Day
#Add one columns as FUNCTIONING_DAY.Flag and replace YES to 1
df_encoded$FUNCTIONING_DAY.Flag <- (df_encoded$FUNCTIONING_DAY == "Yes") +0
#Drop original columns
df_encoded <- df_encoded[,-12]


#Convert HOUR
df_encoded <- df_encoded %>%
  mutate(dummy = 1) %>%
  spread(key = HOUR, value = dummy, fill = 0)

summary(df_encoded)

write_csv(df_encoded, "seoul_bike_sharing_converted.csv")




#Normalize data

str(bike_sharing_df)

# Use the `mutate()` function to apply min-max normalization on columns 
# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, 
# `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`


df_nmlz <- bike_sharing_df %>%
  mutate(
    RENTED_BIKE_COUNT = (RENTED_BIKE_COUNT - min(RENTED_BIKE_COUNT)) / (max(RENTED_BIKE_COUNT) - min(RENTED_BIKE_COUNT)),
    TEMPERATURE = (TEMPERATURE - min(TEMPERATURE)) / (max(TEMPERATURE) - min(TEMPERATURE)),
    HUMIDITY = (HUMIDITY - min(HUMIDITY)) / (max(HUMIDITY) - min(HUMIDITY)),
    WIND_SPEED = (WIND_SPEED - min(WIND_SPEED)) / (max(WIND_SPEED) - min(WIND_SPEED)),
    VISIBILITY = (VISIBILITY - min(VISIBILITY)) / (max(VISIBILITY) - min(VISIBILITY)),
    DEW_POINT_TEMPERATURE = (DEW_POINT_TEMPERATURE - min(DEW_POINT_TEMPERATURE)) / (max(DEW_POINT_TEMPERATURE) - min(DEW_POINT_TEMPERATURE)),
    SOLAR_RADIATION = (SOLAR_RADIATION - min(SOLAR_RADIATION)) / (max(SOLAR_RADIATION) - min(SOLAR_RADIATION)),
    RAINFALL = (RAINFALL - min(RAINFALL)) / (max(RAINFALL) - min(RAINFALL)),
    SNOWFALL = (SNOWFALL - min(SNOWFALL)) / (max(SNOWFALL) - min(SNOWFALL))
  )
# Print the summary of the dataset again to make sure the numeric columns range between 0 and 1
summary(df_nmlz)

write_csv(df_nmlz, "seoul_bike_sharing_converted_normalized.csv")


#Standardize the column names again for the new dataset

# Dataset list
dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}

