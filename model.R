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
modelB = lm(Pickups ~ intervention+Social.ST.min + Total.ST.min+is_weekday,data = stB)

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




# double robostic

dataA = stA[c('Social.ST.min','Pickups','is_weekday')]
intervention_column <- rep(1, nrow(dataA))
dataA_intervention <- cbind(intervention = intervention_column, dataA)
dataA_nonintervention = cbind(intervention = 1- intervention_column, dataA)
predicted_value_intervention_A <- predict(modelA, newdata = dataA_intervention)
predicted_value_nonintervention_A <- predict(modelA, newdata = dataA_nonintervention)

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

trt.effect.dr.a = 1/length(fitted_values_A)*sum(
  stA$intervention * stA$Total.ST.min / fitted_values_A - 
    (1-stA$intervention) * stA$Total.ST.min / (1-fitted_values_A) -
    (stA$intervention - fitted_values_A)/fitted_values_A * predicted_value_intervention_A - 
    (stA$intervention - fitted_values_A)/ (1-fitted_values_A) * predicted_value_nonintervention_A
)

dataB = stB[c('Social.ST.min','Total.ST.min','is_weekday')]
intervention_column <- rep(1, nrow(dataB))
dataB_intervention <- cbind(intervention = intervention_column, dataB)
dataB_nonintervention = cbind(intervention = 1- intervention_column, dataB)
predicted_value_intervention_B <- predict(modelB, newdata = dataB_intervention)
predicted_value_nonintervention_B <- predict(modelB, newdata = dataB_nonintervention)
Yb = stB$Pickups
A = stB$intervention
pi = fitted_values_B
trt.effect.dr.b = 1/length(pi) * sum(
  A * Yb / pi - (1-A)*Yb/(1-pi) - (A - pi) / pi * predicted_value_intervention_B - (A - pi)/(1-pi)*predicted_value_nonintervention_B
)


# bootstrap IPW
set.seed(1234)
n_bootstrap <- 1000

effect_A_IPW <- numeric(n_bootstrap)
effect_B_IPW <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  boot_indices <- sample(nrow(stA), replace = TRUE)
  boot_data_A <- stA[boot_indices, ]
  boot_data_B <- stB[boot_indices, ]
  
  boot_model_A <- glm(intervention ~ Pickups + Social.ST.min + is_weekday,
                      family = binomial(link = "logit"), data = boot_data_A)
  boot_model_B <- glm(intervention ~ Total.ST.min + Social.ST.min + is_weekday,
                      family = binomial(link = "logit"), data = boot_data_B)
  
  fitted_values_A <- fitted(boot_model_A)
  fitted_values_B <- fitted(boot_model_B)
  
  effect_A_IPW[i] <- 1/length(fitted_values_A) * sum(boot_data_A$intervention * boot_data_A$Total.ST.min / fitted_values_A
                                                     - (1 - boot_data_A$intervention) * boot_data_A$Total.ST.min / (1 - fitted_values_A))
  effect_B_IPW[i] <- 1/length(fitted_values_B) * sum(boot_data_B$intervention * boot_data_B$Pickups / fitted_values_B
                                                     - (1 - boot_data_B$intervention) * boot_data_B$Pickups / (1 - fitted_values_B))
}

var_effect_A_IPW <- var(effect_A_IPW)
var_effect_B_IPW <- var(effect_B_IPW)

var_effect_A_IPW = 484.4584
var_effect_B_IPW = 14.88707

# bootstrap dr
n_bootstrap <- 1000

effect.A.dr <- numeric(n_bootstrap)
effect.B.dr <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  boot_indices <- sample(nrow(stA), replace = TRUE)
  boot_data_A <- stA[boot_indices, ]
  boot_data_B <- stB[boot_indices, ]
  
  modelA = lm(Total.ST.min ~ intervention+Social.ST.min + Pickups+is_weekday,data = boot_data_A)
  modelB = lm(Pickups ~ intervention+Social.ST.min + Total.ST.min+is_weekday,data = boot_data_B)
  
  boot_model_A <- glm(intervention ~ Pickups + Social.ST.min + is_weekday,
                      family = binomial(link = "logit"), data = boot_data_A)
  boot_model_B <- glm(intervention ~ Total.ST.min + Social.ST.min + is_weekday,
                      family = binomial(link = "logit"), data = boot_data_B)
  
  fitted_values_A <- fitted(boot_model_A)
  fitted_values_B <- fitted(boot_model_B)
  
  dataA = boot_data_A[c('Social.ST.min','Pickups','is_weekday')]
  intervention_column <- rep(1, nrow(dataA))
  dataA_intervention <- cbind(intervention = intervention_column, dataA)
  dataA_nonintervention = cbind(intervention = 1- intervention_column, dataA)
  predicted_value_intervention_A <- predict(modelA, newdata = dataA_intervention)
  predicted_value_nonintervention_A <- predict(modelA, newdata = dataA_nonintervention)
  
  dataB = boot_data_B[c('Social.ST.min','Total.ST.min','is_weekday')]
  intervention_column <- rep(1, nrow(dataB))
  dataB_intervention <- cbind(intervention = intervention_column, dataB)
  dataB_nonintervention = cbind(intervention = 1- intervention_column, dataB)
  predicted_value_intervention_B <- predict(modelB, newdata = dataB_intervention)
  predicted_value_nonintervention_B <- predict(modelB, newdata = dataB_nonintervention)
  
  effect.A.dr[i] = 1/length(fitted_values_A)*sum(
    boot_data_A$intervention * boot_data_A$Total.ST.min / fitted_values_A - 
      (1-boot_data_A$intervention) * boot_data_A$Total.ST.min / (1-fitted_values_A) -
      (boot_data_A$intervention - fitted_values_A)/fitted_values_A * predicted_value_intervention_A - 
      (boot_data_A$intervention - fitted_values_A)/ (1-fitted_values_A) * predicted_value_nonintervention_A
  )
  effect.B.dr[i] = 1/length(fitted_values_B)*sum(
    boot_data_B$intervention * boot_data_B$Pickups / fitted_values_B -
      (1-boot_data_B$intervention)*boot_data_B$Pickups / (1-fitted_values_B) -
      (boot_data_B$intervention - fitted_values_B)/fitted_values_B * predicted_value_intervention_B - 
      (boot_data_B$intervention - fitted_values_B)/(1-fitted_values_B) * predicted_value_nonintervention_B
  )
}
var_effect_A_IPW <- var(effect.A.dr)
var_effect_B_IPW <- var(effect.B.dr)





