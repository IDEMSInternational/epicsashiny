library(rapidpror)
key <- read.table("R Code/config/key.txt")
set_rapidpro_key(key[[1]])
set_rapidpro_site("https://rapidpro-next.idems.international/api/v2/")
user_data <- get_data_from_rapidpro_api(call_type = "contacts.json",
                                        date_from = NULL,
                                        date_to = NULL)

# todo: add in column for "name" - call it "user"

update_data <- function(){
  # Create the various data sets
  # station metadata data
  station_name <- user_data$fields$station_name
  station_type <- user_data$fields$type_of_station
  station_element <- user_data$fields$station_element
  station_df <- data.frame(station_name, station_type, station_element)
  station_df <- unique(station_df)
  
  rain_df <- element_data(user_data$fields$rainfall_measurements, "Rain")
  tmin_df <- element_data(user_data$fields$tmin_measurements, "Min Temp")
  tmax_df <- element_data(user_data$fields$tmax_measurements, "Max Temp")
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- station_df
  objects_to_return[[2]] <- rain_df
  objects_to_return[[3]] <- tmin_df
  objects_to_return[[4]] <- tmax_df
  return(objects_to_return)
}


# 
# x <- c("datedate, 22, C, edited, date2, 24, U, edited,",
#        "datedate, 22, C, edited, edited, edited,",
#        "datedate, 22, C, edited, date2, 24, U, edited, date3, 26, C",
#        "datedate, 22, C")
# 
# # for that station
# vars_to_check <- grep("edited,", x)
# edit_correction(x, vars_to_check)
