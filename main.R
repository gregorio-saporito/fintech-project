library(readr)
library(tidyverse)
library(survival)
library(ggfortify)

data <- read_csv("data.csv") %>%
  # extra attempts dummy
  mutate(extra_attempts_dummy = ifelse(extra_attempts==0,0,1)) %>%
  mutate(main_extra_attempt = ifelse(is.na(main_extra_attempt),"none",main_extra_attempt)) %>% 
  mutate(main_extra_attempt = factor(main_extra_attempt))

# many outliers for survival time
# few clients go through the system for a long time
boxplot(data$surv_time)
# also a high number of extra attempts is rare
boxplot(data$extra_attempts)

# the distribution is asymmetric
# Kernel density estimation
plot(density(data$surv_time))
plot(density(data$extra_attempts))

# Kaplan-Meier estimates of the probability of survival over time
km_fit <- survfit(Surv(surv_time, status) ~ 1, data=data)

summary(km_fit)

# as time passes the probability of concluding increases
autoplot(km_fit)

# now we look at the survival curve between who makes extra attempts and who doesn't
km_grp_fit <- survfit(Surv(surv_time, status) ~ extra_attempts_dummy, data=data)
# who makes extra attempts is more likely not to conclude and there are 
# small overlaps between the curves
autoplot(km_grp_fit)
summary(km_grp_fit)

# now we compare groups where the main stuck occurs
# remove the ones with a too high confidence interval
data_reduced <- data %>% 
  filter(!main_extra_attempt %in% 
           c("antiriciclaggio","rapporto","contract-subscription"))

# there are still a lot of overlapps with confidence intervals though
km_grp_fit <- survfit(Surv(surv_time, status) ~ main_extra_attempt, data=data_reduced)
autoplot(km_grp_fit)
summary(km_grp_fit)

# Next, I’ll fit a Cox Proportional Hazards Model that makes use of all of the
# covariates in the data set, extra_attempts non dummified
cox <- coxph(Surv(surv_time, status) ~ extra_attempts, data = data)
# we can show that extra_attempts is significant
summary(cox)

cox_fit <- survfit(cox)
autoplot(cox_fit)

# the Cox model assumes that the covariates do not vary with time
# Let's show how the effects of the covariates change over time
# using Aalen’s additive regression model
aa_fit <-aareg(Surv(surv_time, status) ~ extra_attempts, 
               data = data)
aa_fit

# if time-dependent so the assumptions for the Cox model are not met
# The plots show how the effects of the covariates change over time.
autoplot(aa_fit)

