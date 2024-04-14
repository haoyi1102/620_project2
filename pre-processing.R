library(readxl)
library(lubridate)
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


