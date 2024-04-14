library(readxl)

bs <- read_xlsx('Fulldata_620W24_Project2.xlsx',sheet = 2)

full_data <- read_excel('Fulldata_620W24_Project2.xlsx', sheet = 1)
selected_data <- full_data[c("pseudo_ID", "Date", "Total.ST", "Social.ST", "Pickups")]


