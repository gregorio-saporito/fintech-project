library(readr)
library(tidyverse)
library(survival)
library(ggfortify)

API_Subcription <- read_csv("API_Subcription.csv") %>%
  # remove column not needed
  select(-NETWORK_ID) %>%
  # remove part of string not needed from API
  mutate(COMPLETED_STEP = str_remove_all(COMPLETED_STEP,
                                     '/api/subscribe-|/api//subscribe-|.json')) %>%
  # we want to track if the user goes back and forth counting the number of attempts
  group_by(USER_ID,COMPLETED_STEP) %>%
  arrange(USER_ID,COMPLETED_STEP,DATE_EVENT, ID) %>%
  mutate(attempt = 1:n()) %>%
  ungroup()

# get ids of people that concluded
ids <- API_Subcription %>% 
  filter(COMPLETED_STEP == 'conclude') %>%
  pull(USER_ID) %>% unique()


# filter on users that concluded to see the steps
API_Subcription %>%
  filter(USER_ID %in% ids) %>%
  arrange(USER_ID,ID)

# some users just have one conclude without the previous steps

# we need to count for how long each client has survived
data <- tibble()
count_ids = 0

for(i in ids) {
x <- API_Subcription %>%
  filter(USER_ID == i)

time_diff <- max(x$DATE_EVENT) - min(x$DATE_EVENT)

extra_attempts <- x %>% 
  group_by(COMPLETED_STEP) %>%
  filter(attempt == max(attempt)) %>%
  mutate(check = ifelse(attempt==1,0,attempt-1)) %>%
  ungroup() %>%
  summarise(extra_attempt = sum(check)) %>% pull()

# status 1 if death occurred (conclude)

status <- ifelse(nrow(filter(x,COMPLETED_STEP=='conclude'))==0,0,1)

data <- data %>%
  bind_rows(
    tibble(
      CLIENT_id = i,
      # time in seconds
      surv_time = as.double(time_diff),
      status = status,
      extra_attempts = extra_attempts
    )
  )
# track percentage until complete
count_ids <- count_ids + 1
print(paste0("complete",round((count_ids/length(ids))*100),"%"))
}

data <- data %>%
  # extra attempts dummy
  mutate(extra_attempts_dummy = ifelse(extra_attempts==0,0,1))

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
