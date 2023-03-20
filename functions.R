#' Interaction with api
#' Description: Make API call, specify query string, save data in dataframe

#' 1. Defining package environment -------------------------------------------
#' Sorting set and get calls for: key, site, uuid flow names
#' 
#' Define package environment
utils::globalVariables(c("pkg_env"))
pkg_env <- new.env(parent = emptyenv())
pkg_env$rapidpro_key <- NULL
pkg_env$rapidpro_site <- NULL
pkg_env$rapidpro_uuid_names <- NULL 

set_rapidpro_key = function(key) {
  if (!is.character(key)){
    stop("`key` provided should be a character variable")
  }
  pkg_env$rapidpro_key <- key 
}

set_rapidpro_site = function(site) {
  if (!is.character(site)){
    stop("`site` provided should be a character variable")
  }
  pkg_env$rapidpro_site <- site 
}

set_rapidpro_uuid_names = function(uuid_names = get_flow_names()){
  pkg_env$rapidpro_uuid_names <- uuid_names 
}

get_rapidpro_key = function() {
  get("rapidpro_key", envir = pkg_env)
}

get_rapidpro_site = function() {
  get("rapidpro_site", envir = pkg_env)
}

get_rapidpro_uuid_names = function(){
  get("rapidpro_uuid_names", envir = pkg_env)
}

get_user_data <- function(rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE, date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  user_data <- get_data_from_rapidpro_api(call_type = "contacts.json?group=joined", rapidpro_site = rapidpro_site, token = token, flatten = flatten, date_from = NULL, date_to = NULL, format_date = format_date, tzone_date = tzone_date)
  
  if (!flatten){
    if (!is.null(date_from)){
      user_data <- user_data %>% dplyr::filter(as.POSIXct(date_from, format=format_date, tzone = tzone_date) < as.POSIXct(user_data$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
    if (!is.null(date_to)){
      user_data <- user_data %>% dplyr::filter(as.POSIXct(date_to, format=format_date, tzone = tzone_date) > as.POSIXct(user_data$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
  } else {
    if (!is.null(date_from)){
      user_data <- user_data %>% dplyr::filter(as.POSIXct(date_from, format=format_date, tzone = tzone_date) < as.POSIXct(user_data$fields.starting_date, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
    if (!is.null(date_to)){
      user_data <- user_data %>% dplyr::filter(as.POSIXct(date_to, format=format_date, tzone = tzone_date) > as.POSIXct(user_data$fields.starting_date, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
  }
  return(user_data)
}

get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE,
                                       date_from, date_to, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  if (is.null(rapidpro_site)){
    stop("rapidpro_site is NULL. Set a website with `set_rapidpro_site`.")
  }
  if (is.null(token)){
    stop("token is NULL. Set a token with `set_rapidpro_key`.")
  }
  if (is.null(call_type)){
    stop("call_type is NULL. Expecting a valid call_type.")
  }
  if (!is.logical(flatten)){
    stop("flatten should be TRUE or FALSE")
  }
  get_command <- paste(rapidpro_site, call_type, sep = "")
  user_result <- httr_get_call(get_command = get_command, token = token)
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  if (!is.null(date_from)){
    user_result <- user_result %>% dplyr::filter(as.POSIXct(date_from, format=format_date, tzone = tzone_date) < as.POSIXct(user_result$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!is.null(date_to)){
    user_result <- user_result %>% dplyr::filter(as.POSIXct(date_to, format=format_date, tzone = tzone_date) > as.POSIXct(user_result$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  return(user_result)
}

httr_get_call <- function(get_command, token = get_rapidpro_key()){
  if (is.null(token)){
    stop("token is NULL. Set token with `set_rapidpro_key`.")
    # could there be a case where the key isn't needed?
  }
  if (is.null(get_command)){
    stop("get_command is NULL. Expecting a website.")
  }
  response <- httr::GET(get_command, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if(!is.null(results$'next')){
    dplyr::bind_rows(results$results, httr_get_call(results$'next', token))
  } else {
    return(results$results)
  }
}


# New Functions for Climatic ShinyBot -----------------------------------------------------
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
