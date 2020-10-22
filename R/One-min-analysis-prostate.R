Prostate = read.table("data/prostate.txt", header=T)
Prostate = na.omit(Prostate); dim(Prostate) # 377 8
Prostate = Prostate[,-1]
Prostate$dpros = as.factor(Prostate$dpros)
Prostate$dcaps = as.factor(Prostate$dcaps)
Prostate$race = as.factor(Prostate$race)
attach(Prostate)

table(dcaps, capsule)
#capsule
#   0 1
#1 216 121
#2 10 30
chisq.test(x=dcaps, y=capsule)
#Pearson's Chi-squared test with Yates' continuity correction
#X-squared = 21.162, df = 1, p-value = 4.221e-06

# Model of capsule on gleason, dpros, and the two-way interaction
modfit = glm(capsule ~ gleason*dpros, family = binomial, data=Prostate)

fit1 <- glm(capsule ~ dcaps, family = binomial, data=Prostate)
library(broom)
library(tidyverse)
tidy(fit1) %>% 
  mutate(OR = exp(estimate))  %>% 
  modify_if(is.numeric, round, 4)

fit2 <- glm(capsule ~ dcaps + psa, family = binomial, data=Prostate)
 
fit3 <- glm(capsule ~ dcaps + psa + dcaps*psa, family = binomial, data=Prostate)


tidy(fit2) %>% modify_if(is.numeric, round, 4)
tidy(fit3) %>% modify_if(is.numeric, round, 4)
