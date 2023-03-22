
# New Functions for EPICSA  -----------------------------------------------------
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# if it says edited, but ends on "edited," then remove "edited,"
# if it says edited, but does not end on "edited,", then remove everything before "edited,"
edit_correction <- function(x, vars_to_check){
  for (i in c(vars_to_check)){
    if (substrRight(x[i], 7) == "edited,"){
      # remove "edited," - removes last one
      x[i] <- substring(x[i], 1, nchar(x[i])-8)
      
      # recursive function here now, in case of other edited,'s.
      if (grepl("edited,", x[i])){
        y <- x[i]
        x[i] <- edit_correction(y, 1)
      } else {
        x[i] <- x[i]
      }
    } else {
      # remove everything before the final "edited,"
      x[i] <- gsub(".*edited,","",x[i])
    }
    
  }
  return(x)
}

element_data <- function(element_col = user_data$fields$rainfall_measurements,
                         element_name = "Rain"){
  element_measure <- element_col
  element_split <- stringr::str_split(element_measure, pattern = stringr::fixed("|"))
  names(element_split) <- paste(user_data$fields$station_name, user_data$uuid, sep = "_")
  for (i in 1:length(element_split)){
    x <- element_split[[i]]
    # which elements have "edited" in them?
    vars_to_check <- grep("edited", x)
    x <- edit_correction(x, vars_to_check)
    x <- x[x!=""]
    element_split[[i]] <- data.frame(stringr::str_split(x, pattern = stringr::fixed(","), simplify = TRUE))
  }
  element_data <- plyr::ldply(element_split, `.id` = "Station") %>%
    mutate(Type = element_name) %>% relocate(Type, .after = Station)   
  # split station into two columns: Station and User
  station_user = data.frame(stringr::str_split(element_data$Station, pattern = "_", simplify = TRUE))
  names(station_user) = c("Station", "User")
  element_data <- element_data %>% dplyr::select(-c(Station)) 
  element_data <- cbind(station_user, element_data)
  # remove columns and rows that contain all NAs
  element_data <- element_data %>% 
    dplyr::select(where(function(x) any(!is.na(x)))) %>%
    filter(rowSums(is.na(element_data)) != (length(element_data)-1)) # minus 1 because all have the type column
  if (length(element_data) == 6){
    names(element_data)[4:6] <- c("Entry Time", "Measurement", "Confirmed")
    element_data$`Entry Time` <- gsub("\\..*","", element_data$`Entry Time`)
    element_data <- element_data %>% filter(Confirmed != "D") # TODO: should do this earlier in the code
  }
  return(element_data)
}

raw_data <- function(rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key()){
  get_command <- paste(rapidpro_site, "contacts.json", sep = "")
  response <- httr::GET(get_command,
                        config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  return(raw)
}
