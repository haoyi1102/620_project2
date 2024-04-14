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
    } 
  }))
}
st_count = 0
st_na_id = list()
for( i in 1:nrow(df)){
  if(is.na(df$Total.ST[i]) == 1 & is.na(df$Total.ST.min[i]) == 1){
    st_na_id = c(st_na_id,df$pseudo_ID[i])
    st_count = st_count +1
  }else if(is.na(df$Total.ST[i]) == 0 & is.na(df$Total.ST.min[i]) == 1){
    df$Total.ST.min[i] = hm_to_min(df$Total.ST[i])
  }
  st_na_id = unique(st_na_id)
}

social_count = 0
social_na_id = list()
for( i in 1:nrow(df)){
  if(is.na(df$Social.ST[i]) == 1 & is.na(df$Social.ST.min[i]) == 1){
    social_na_id = c(social_na_id,df$pseudo_ID[i])
    social_count = social_count + 1
  }else if(is.na(df$Social.ST[i]) == 0 & is.na(df$Social.ST.min[i]) == 1){
    df$Social.ST.min[i] = hm_to_min(df$Social.ST[i])
  }
  social_na_id = unique(social_na_id)
}

selected_data <- df[c("pseudo_ID", "Date", "Total.ST.min", "Social.ST.min", "Pickups")]

## impute

library(dplyr)
# 先按 pseudo_ID 分组，并去除完全是NA的行：
data_cleaned <- data %>%
  group_by(pseudo_ID) %>%
  filter(!(is.na(Total.ST.min) & is.na(Social.ST.min) & is.na(Pickups)))
# 应用k-NN补全
data_imputed <- data_cleaned %>%
  group_by(pseudo_ID) %>%
  mutate(
    Total.ST.min = if(all(is.na(Total.ST.min))) NA else kNN(Total.ST.min, k = 5),
    Social.ST.min = if(all(is.na(Social.ST.min))) NA else kNN(Social.ST.min, k = 5),
    Pickups = if(all(is.na(Pickups))) NA else kNN(Pickups, k = 5)
  )


