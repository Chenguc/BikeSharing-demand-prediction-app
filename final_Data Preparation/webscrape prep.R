
rm(list=ls())
library(httr)
library(rvest)

PATH 		= "/Users/uc/Desktop/R Code/final_Data Preparation"
setwd( PATH )

#Extract bike sharing systems Wiki page and convert it into a data frame
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
# Get the root HTML node by calling the `read_html()` method with URL
webpage <- read_html(url)

table_nodes <- html_nodes(webpage, "table")


# Convert the bike-sharing system table into a dataframe
# Assuming the first table contains the bike-sharing information
bike_table <- html_table(table_nodes[[1]], fill = TRUE)

bike_df <- as.data.frame(bike_table)

head(bike_df, 10)
summary(bike_df)

# Export the dataframe into a csv file
write.csv(bike_df, "raw_bike_sharing_data.csv", row.names = FALSE)





# Get forecast data for a given city list


# URL for Current Weather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'
# need to be replaced by your real API key
your_api_key <- "13fa64c1d3dbcd0bd8a1cf4a6b316ee3"
# Input `q` is the city name
# Input `appid` is your API KEY, 
# Input `units` are preferred units such as Metric or Imperial
current_query <- list(q = "Seoul", appid = your_api_key, units="metric")
response <- GET(current_weather_url, query=current_query)
http_type(response)
json_result <- content(response, as="parsed")
class(json_result)

#Get 5-day weather forecasts for a list of cities using the OpenWeather API

forecast_weather_url <- 'https://api.openweathermap.org/data/2.5/forecast'
forecast_query <- list(q = "Seoul", appid = your_api_key, units="metric") #cnt=40 use for change the days it's around 5 days
f_response <- GET(forecast_weather_url, query=forecast_query)

http_type(f_response)
json_result<- content(f_response, as="parsed")
class(json_result)
#print(json_result_1)

# Create empty vectors to hold data temporarily
dt <- c()
temp <- c()
feels_like <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
temp_kf <- c()
weather_main <- c()
weather_description <- c()
weather_icon <- c()
clouds_all <- c()
wind_speed <- c()
wind_deg <- c()
wind_gust <- c()
visibility <- c()
pop <- c()
sys_pod <- c()
dt_txt <- c()

get_weather_forecaset_by_cities <- function(city_names){
  df <- data.frame()
  for (city_name in city_names){
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    # Create query parameters
    forecast_query <- list(q = city_name, appid = your_api_key , units="metric")
    # Make HTTP GET call for the given city
    respone<- GET(forecast_url, query=forecast_query)
    json_result<-content(respone, as="parsed")
    
    for (i in 1:length(json_result$list)) {
      # Extract data from each element and append to corresponding vectors
      city <- c(city, city_name)
      dt <- c(dt, json_result$list[[i]]$dt)
      temp <- c(temp, json_result$list[[i]]$main$temp)
      feels_like <- c(feels_like, json_result$list[[i]]$main$feels_like)
      temp_min <- c(temp_min, json_result$list[[i]]$main$temp_min)
      temp_max <- c(temp_max, json_result$list[[i]]$main$temp_max)
      pressure <- c(pressure, json_result$list[[i]]$main$pressure)
      humidity <- c(humidity, json_result$list[[i]]$main$humidity)
      temp_kf <- c(temp_kf, json_result$list[[i]]$main$temp_kf)
      weather_main <- c(weather_main, json_result$list[[i]]$weather[[1]]$main)
      weather_description <- c(weather_description, json_result$list[[i]]$weather[[1]]$description)
      weather_icon <- c(weather_icon, json_result$list[[i]]$weather[[1]]$icon)
      clouds_all <- c(clouds_all, json_result$list[[i]]$clouds$all)
      wind_speed <- c(wind_speed, json_result$list[[i]]$wind$speed)
      wind_deg <- c(wind_deg, json_result$list[[i]]$wind$deg)
      wind_gust <- c(wind_gust, json_result$list[[i]]$wind$gust)
      visibility <- c(visibility, json_result$list[[i]]$visibility)
      pop <- c(pop, json_result$list[[i]]$pop)
      sys_pod <- c(sys_pod, json_result$list[[i]]$sys$pod)
      dt_txt <- c(dt_txt, json_result$list[[i]]$dt_txt)
    }
    
    # Create a data frame from the extracted data
    weather_data_frame <- data.frame(
      City = city,
      Date = dt,
      temp = temp,
      feels_like = feels_like,
      temp_min = temp_min,
      temp_max = temp_max,
      pressure = pressure,
      humidity = humidity,
      temp_kf = temp_kf,
      weather_main = weather_main,
      weather_description = weather_description,
      weather_icon = weather_icon,
      clouds_all = clouds_all,
      wind_speed = wind_speed,
      wind_deg = wind_deg,
      wind_gust = wind_gust,
      visibility = visibility,
      pop = pop,
      sys_pod = sys_pod,
      dt_txt = dt_txt
    )
    # Return a data frame
    return(weather_data_frame)
    
    
  }
}

get_weather_forecaset_by_cities('Seoul')

cities <- c('Seoul', 'Washington, D.C.', 'Paris', 'Suzhou')
cities_weather_df <- get_weather_forecaset_by_cities(cities)

view(cities_weather_df)



# Write cities_weather_df to `cities_weather_forecast.csv`
write.csv(cities_weather_df, "cities_weather_forecast.csv", row.names=FALSE)


# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")



