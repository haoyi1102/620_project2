rm(list = ls())
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
bs <- read_xlsx('Fulldata_620W24_Project2.xlsx',sheet = 2)
data = read.csv('./cleaned_data.csv')
st = left_join(data,bs,by = "pseudo_id")
st <- st %>% 
  filter(pseudo_id != 1329)

stA = st[st$Treatment.x == "A",]
stB = st[st$Treatment.x == "B",]

stA$intervention = ifelse(is.na(stA$compliance),0,1)
stB$intervention = ifelse(is.na(stB$compliance),0,1)
stB <- stB %>%
  mutate(is_weekday = if_else(wday(Date, week_start = 1) %in% 2:6, 1, 0))

st9285 = stA[stA$pseudo_id == "9285",c("Total.ST.min","Social.ST.min","Pickups")]
st9285 <- na.omit(st9285)
st9285s = st9285[st9285$Total.ST.min <= 200,]
st9285f = st9285[st9285$Total.ST.min > 200,]
st.s.mean = mean(st9285s$Total.ST.min)
s.st.s.mean = mean(st9285s$Social.ST.min)
p.s.mean = mean(st9285s$Pickups)
st.f.mean = mean(st9285f$Total.ST.min)
s.st.f.mean = mean(st9285f$Social.ST.min)
p.f.mean = mean(st9285f$Pickups)
for(i in 1:nrow(stA)){
  if(stA$pseudo_id[i] == "9285" & stA$intervention[i] == 1){
    if(stA$compliance[i] == 1){
      stA$Total.ST.min[i] = st.s.mean
      stA$Social.ST.min[i] = s.st.s.mean
      stA$Pickups[i] = p.s.mean
    }else{
      stA$Total.ST.min[i] = st.f.mean
      stA$Social.ST.min[i] = s.st.f.mean
      stA$Pickups[i] = p.f.mean
    }
  }
}
stA <- stA[!is.na(stA$Total.ST.min), ]

library(table1) 
# stA
table1(~ Total.ST.min + Social.ST.min + Pickups + is_weekday + age.x + sex.x + `procrastination score`| intervention, data=stA)
# without 
stA_wo <- stA[!stA$pseudo_id %in% c(9285, 1329), ]
table1(~ Total.ST.min + Social.ST.min + Pickups + is_weekday + age.x + sex.x + `procrastination score`| intervention, data=stA_wo)
# stB
table1(~ Total.ST.min + Social.ST.min + Pickups + is_weekday + age.x + sex.x + `procrastination score`| intervention, data=stB)

table1(~ Total.ST.min + Social.ST.min + Pickups + is_weekday + age.x + sex.x + `procrastination score`| Treatment.x, data=st)

