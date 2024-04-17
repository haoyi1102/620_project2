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
# 确保 Date 列是日期格式
selected_data$Date <- as.Date(selected_data$Date, format = "%Y/%m/%d")
# 添加 is_weekday 列
selected_data <- selected_data %>%
  mutate(is_weekday = if_else(wday(Date, week_start = 1) %in% 2:6, 1, 0))


library(impute)


# 2508
data_2508 <- selected_data %>%
  filter(pseudo_ID == 2508) %>%
  select(Total.ST.min, Social.ST.min, Pickups,is_weekday)

imputed_2508 <- impute.knn(as.matrix(data_2508), k = 5)$data
data_2508_imputed <- as.data.frame(imputed_2508)
colnames(data_2508_imputed) <- c("Total.ST.min", "Social.ST.min", "Pickups")

data_2508_final <- cbind(selected_data %>% filter(pseudo_ID == 2508) %>% select(pseudo_ID, Date), data_2508_imputed)

# 4278
data_4278 <- selected_data %>%
  filter(pseudo_ID == 4278) %>%
  select(Total.ST.min, Social.ST.min, Pickups,is_weekday)

imputed_4278 <- impute.knn(as.matrix(data_4278), k = 5)$data
data_4278_imputed <- as.data.frame(imputed_4278)
colnames(data_4278_imputed) <- c("Total.ST.min", "Social.ST.min", "Pickups")

data_4278_final <- cbind(selected_data %>% filter(pseudo_ID == 4278) %>% select(pseudo_ID, Date), data_4278_imputed)

# 2243
data_2243 <- selected_data %>%
filter(pseudo_ID == 2243) %>%
  select(Total.ST.min, Social.ST.min, Pickups,is_weekday)

imputed_2243 <- impute.knn(as.matrix(data_2243), k = 5)$data
data_2243_imputed <- as.data.frame(imputed_2243)
colnames(data_2243_imputed) <- c("Total.ST.min", "Social.ST.min", "Pickups")

data_2243_final <- cbind(selected_data %>% filter(pseudo_ID == 2243) %>% select(pseudo_ID, Date), data_2243_imputed)

# 957
data_957 <- selected_data %>%
filter(pseudo_ID == 957) %>%
  select(Total.ST.min, Social.ST.min, Pickups,is_weekday)

imputed_957 <- impute.knn(as.matrix(data_957), k = 5)$data
data_957_imputed <- as.data.frame(imputed_957)
colnames(data_957_imputed) <- c("Total.ST.min", "Social.ST.min", "Pickups")

data_957_final <- cbind(selected_data %>% filter(pseudo_ID == 957) %>% select(pseudo_ID, Date), data_957_imputed)

# 合并所有处理后的数据
final_data <- bind_rows(data_2508_final, data_4278_final, data_2243_final, data_957_final)

# 需要移除的 pseudo_ID 列表
ids_to_remove <- c(2508, 4278, 2243, 957)

# 从 selected_data 中移除这些 pseudo_ID 的数据
selected_data_filtered <- selected_data %>%
  filter(!pseudo_ID %in% ids_to_remove)

# 合并填补后的数据回原始数据集
updated_selected_data <- bind_rows(selected_data_filtered, final_data)

updated_selected_data = updated_selected_data %>% 
  mutate(
    Daily_Prop_Social_ST = Social.ST.min / Total.ST.min,
    Daily_Duration_Per_Use = Total.ST.min / Pickups
  )

# 合并数据集
colnames(updated_selected_data)[1] <- "pseudo_id"
merged_data <- left_join(updated_selected_data, bs, by = "pseudo_id")


#### 生成compliance

merged_data$Date <- as.Date(merged_data$Date, format = "%Y/%m/%d")

# 筛选指定日期范围的数据
date_range_data <- merged_data %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02"))

# 筛选特定日期范围的数据并计算 compliance
date_range_data <- merged_data %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  mutate(compliance = case_when(
    Treatment == "A" & Total.ST.min <= 200 ~ 1,  # Treatment A，屏幕时间 <= 200分钟
    Treatment == "B" & Pickups <= 50 ~ 1,        # Treatment B，拿起次数 <= 50次
    TRUE ~ 0                                     # 其他情况
  ))
# 合并计算的 compliance 列回原数据集
merged_data <- left_join(merged_data, date_range_data %>% select(pseudo_id, Date, compliance), by = c("pseudo_id", "Date"))
# 设置不在日期范围内的 compliance 为 NA 或其他逻辑值
merged_data$compliance[is.na(merged_data$compliance)] <- NA

merged_data <- merged_data %>%
  select(-`...6`)

# 添加 has_intervention 列
merged_data <- merged_data %>%
  mutate(is_intervention = if_else(is.na(compliance), 0, 1))

# 保存数据到CSV文件
write.csv(merged_data, "cleaned_data.csv", row.names = FALSE)
