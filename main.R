library(readr)
library(tidyverse)
library(survival)
library(ggfortify)

data <- read_csv("data.csv") %>%
  # extra attempts dummy
  mutate(extra_attempts_dummy = ifelse(extra_attempts==0,0,1))

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

# time-dependent so the assumptions for the Cox model are not met
# The plots show how the effects of the covariates change over time.
# Notice the steep slope and then abrupt change.
autoplot(aa_fit)
