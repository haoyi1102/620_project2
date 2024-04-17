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
# Linear model comparetion
modelA = lm(Total.ST.min ~ intervention+Social.ST.min + Pickups+is_weekday,data = stA)
summary(modelA)
modelB = lm(Pickups ~ intervention+Social.ST.min + Total.ST.min+is_weekday,data = stB)
summary(modelB)
# IPW model
modelAipw = glm(intervention ~ Pickups +Social.ST.min+is_weekday,
                family = binomial(link = "logit"),
                data = stA
)

modelBipw = glm(intervention ~ Total.ST.min +Social.ST.min+is_weekday,
                family = binomial(link = "logit"),
                data = stB
)
fitted_values_A <- fitted(modelAipw)
fitted_values_B <- fitted(modelBipw)

effect.A.IPW = 1/length(fitted_values_A) * sum(stA$intervention * stA$Total.ST.min / fitted_values_A
                                               - (1-stA$intervention)*stA$Total.ST.min / (1-fitted_values_A))
effect.B.IPW = 1/length(fitted_values_B) * sum(stB$intervention * stB$Pickups / fitted_values_B
                                               - (1-stB$intervention)*stB$Pickups / (1-fitted_values_B))
varA = 1/length(fitted_values_A) * sum(
  (stA$intervention/fitted_values_A^2 + (1-stA$intervention)/(1-fitted_values_A)^2)
  *(stA$Social.ST.min - mean(stA$Social.ST.min))^2
)

# IPW 2
# double robust 
modelAipw <- glm(intervention ~ Pickups + Social.ST.min + is_weekday,
                 family = binomial(link = "logit"),
                 data = stA)
propensity_scores <- predict(modelAipw, type = "response")
weights <- ifelse(stA$intervention == 1, 
                  1 / propensity_scores, 
                  1 / (1 - propensity_scores))
ipw_estimate <- sum(weights * stA$intervention * stA$Total.ST.min) / sum(weights * stA$intervention) - 
  sum(weights * (1 - stA$intervention) * stA$Total.ST.min) / sum(weights * (1 - stA$intervention))

weighted_residuals_squared <- (stA$intervention - ipw_estimate)^2 / propensity_scores^2 + 
  ((1 - stA$intervention) - (1 - propensity_scores))^2 / (1 - propensity_scores)^2

weighted_residuals_squared <- weighted_residuals_squared * (stA$Total.ST.min - ipw_estimate)^2

var_ipw <- sum(weighted_residuals_squared) / length(stA$Total.ST.min)






st <- st %>% select(-...6)

for(i in 1:nrow(st)){
  if(st$Date[i]>= as.Date("2024-03-27") &st$Date[i] <= as.Date("2024-04-02")){
    if(st$Treatment[i] == "A"){
      st$compliance[i] = ifelse(st$Total.ST.min[i] <= 200 , 1, 0)
    }else{
      st$compliance[i] = ifelse(st$Pickups[i] <= 50 , 1, 0)
    }
  }
}


st <- st %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  mutate(compliance = case_when(
    Treatment == "A" & Total.ST.min <= 200 ~ 1, 
    Treatment == "B" & Pickups <= 50 ~ 1,        
    TRUE ~ 0                                     
  ))






















