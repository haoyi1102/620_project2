rm(list = ls())
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

df <- read_xlsx('Fulldata_620W24_Project2.xlsx',sheet = 1)
bs <- read_xlsx('Fulldata_620W24_Project2.xlsx',sheet = 2)

convert_dates <- function(date) {
  if (is.na(as.numeric(date))) { 
    # If the date is not a number, it is assumed to be a string in m/d/y format
    date <- mdy(date) 
  } else {
    # If the date is a number, convert it to a date
    date <- as.Date(as.numeric(date), origin = "1899-12-30")
  }
  # Format a string with date in 'yyyy/mm/dd' format
  date <- format(date, "%Y/%m/%d")
  return(date)
}

  # Apply the function to the date column
df$Date <- sapply(df$Date, convert_dates)

# Find the location of the last non-NA Date value with pseudo_id 6419
last_valid_index <- max(which(df$pseudo_ID == 6419 & !is.na(df$Date)))

# Gets the Date value for that location and converts it to a Date object
last_valid_date <- as.Date(df$Date[last_valid_index], "%Y/%m/%d")

# Fill the values for all NA dates of pseudo_id 6419
for (i in (last_valid_index+1):nrow(df)) {
  if (df$pseudo_ID[i] == 6419) {
    # Generate the next date string
    next_date <- format(last_valid_date + (i - last_valid_index), "%Y/%m/%d")
    # Assigns the resulting Date string to the Date column
    df$Date[i] <- next_date
  }
}

selected_data <- df[c("pseudo_ID", "Date", "Total.ST", "Social.ST", "Pickups")]

#hm_to_min = function(hm){unlist(lapply(hm,function(x){splt = strsplit(x,"h")[[1]];hr = as.numeric(splt[1]); mn = as.numeric(strsplit(splt[2],"m")[[1]][1]); return(60*hr + mn)}))}

hm_to_min <- function(hm) {
  unlist(lapply(hm, function(x) {
    if (grepl("h", x) && grepl("m", x)) {
      splt = strsplit(x, "h")[[1]]
      hr = as.numeric(splt[1])
      mn = as.numeric(strsplit(splt[2], "m")[[1]][1])
      return(60 * hr + mn)
    } else if (grepl("h", x)) {
      hr = as.numeric(strsplit(x, "h")[[1]][1])
      return(60 * hr)
    } else if (grepl("m", x)) {
      mn = as.numeric(strsplit(x, "m")[[1]][1])
      return(mn)
    } else {
      return(NA)  # Return NA for formats that do not match expected pattern
    }
  }))
}

selected_data <- selected_data %>%
  mutate(
    Total.ST.min = hm_to_min(Total.ST),
    Social.ST.min = hm_to_min(Social.ST)
  )
