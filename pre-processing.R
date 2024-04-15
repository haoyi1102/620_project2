rm(list = ls())
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

df <- read_xlsx('Fulldata_620W24_Project2.xlsx',sheet = 1, na = 'NA')
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

all_na_id = list()
imputation_id = list()
for(i in 1:nrow(selected_data)){
  if(is.na(selected_data$Total.ST.min[i]) == 1 &
     is.na(selected_data$Social.ST.min[i])==1 &
     is.na(selected_data$Pickups[i])==1){
    all_na_id = c(all_na_id,selected_data$pseudo_ID[i])
  }else if(is.na(selected_data$Total.ST.min[i]) == 1 |
           is.na(selected_data$Social.ST.min[i])==1 |
           is.na(selected_data$Pickups[i])==1){
    imputation_id = c(imputation_id,selected_data$pseudo_ID[i])
  }
  imputation_id = unique(imputation_id)
  all_na_id = unique(all_na_id)
}


## impute

<<<<<<< HEAD


=======
library(impute)
imputed_data <- impute.knn(as.matrix(selected_data[, c("Total.ST.min", "Social.ST.min", "Pickups")]),k=5)$data
# Convert the imputed data back to a dataframe and combine with the non-imputed columns
data_imputed <- as.data.frame(imputed_data)
data_imputed <- cbind(selected_data[1:2], data_imputed) # Assuming the first two columns are 'pseudo_ID' and 'Date'

# Check for rows with all NA and apply mean imputation if any
for (i in 1:nrow(data_imputed)) {
  if (all(is.na(data_imputed[i, -c(1,2)]))) { # Assuming the first two columns are 'pseudo_ID' and 'Date'
    data_imputed[i,] <- apply(data_imputed[, -c(1,2)], 2, mean, na.rm = TRUE)
  }
}
>>>>>>> 16281c741289abea2dba6512a0e094852b3ee0e1


