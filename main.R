library(readr)
library(tidyverse)
library(survival)
library(ggfortify)
library(ggthemes)

data <- read_csv("data.csv") %>%
  # extra attempts dummy
  mutate(extra_attempts_dummy = ifelse(extra_attempts==0,0,1)) %>%
  mutate(main_extra_attempt = ifelse(is.na(main_extra_attempt),"none",
                                     main_extra_attempt)) %>% 
  mutate(main_extra_attempt = factor(main_extra_attempt)) %>%
  mutate(prom_code = ifelse(is.na(prom_code_id),0,1)) %>%
  mutate(gender = ifelse(is.na(gender),"not specified", gender)) %>%
  mutate(age_group = case_when(
    age >= 0 & age < 20   ~ "less than 20",
    age >= 20 & age < 40  ~ "20-40",
    age >= 40 & age < 60  ~ "40-60",
    age >= 60  ~ "more than 60",
    is.na(age) ~ "not specified"
  )) %>%
  mutate(stuck = factor(stuck),
         gender = factor(gender),
         age_group = factor(age_group))

# boxplot of survival time
boxplot(data$surv_time)

# donut charts to explore the data
type <- c("status","extra_attempts_dummy","main_extra_attempt","stuck","prom_code")

donut_df <- map_df(type, function(i){
a <- data[,i] %>%
  group_by_all() %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(prop=n/sum(n)) %>%
  mutate(type=i)

a[,2:ncol(a)] %>%
  mutate(class=as.character(pull(a[,1]))) %>% 
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
  
})

ggplot(donut_df, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, 
                label = 
                  ifelse(prop<0.03,"",paste0(round(prop*100),"%") )
                ),
                color = "white")+
  theme_void()+
  xlim(0.5, 2.5) +
  facet_wrap(~type) +
  scale_fill_manual(values=c("#0f3057", "#00587a", "#008891", "#e7e7de",
                             "#db6400", "#8db596", "#bedbbb", "#ac4b1c",
                             "#87431d", "#83142c", "#aa7070", "#66779c", "#b2deec"))

# Kaplan-Meier estimates of the probability of survival over time
km_fit <- survfit(Surv(surv_time, status) ~ 1, data=data)
autoplot(km_fit) +
  ggtitle("Kaplan-Meier estimates of \n the probability of survival over time") +
  theme_economist()


# we now look at survival curves by gender
# we can see that those without specified gender have the highest probability of survival
# followed by females
km_trt_fit <- survfit(Surv(surv_time, status) ~ gender, data=data)
autoplot(km_trt_fit) +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Survival curves by gender") +
  theme_economist() +
  scale_fill_economist() +
  scale_color_economist()


# now we look at the differences between different age groups
km_AG_fit <- survfit(Surv(surv_time, status) ~ age_group, data=data)
autoplot(km_AG_fit) +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Survival curves by age group") +
  theme_economist() +
  scale_fill_economist() +
  scale_color_economist()

# now we look at the survival curve between who makes extra attempts and who doesn't
km_att_fit <- survfit(Surv(surv_time, status) ~ extra_attempts_dummy, data=data)
# interesting intersection between the curves
# making extra attempts before the intersection means slightly higher probability of survival
# after the intersection slightly lower probability of survival
autoplot(km_att_fit) +
  theme_economist() +
  scale_fill_economist() +
  scale_colour_economist() +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Extra attempts dummy")

# now we compare groups where the main_extra_attempt occurs
# remove the ones with a too high confidence interval and coclude
data_reduced <- data %>% 
  filter(!main_extra_attempt %in% 
           c("antiriciclaggio","rapporto","contract-subscription","conclude"))

# there are still a lot of overlapps with confidence intervals though
km_grp_fit <- survfit(Surv(surv_time, status) ~ main_extra_attempt, data=data_reduced)
autoplot(km_grp_fit) +
theme_economist() +
  scale_fill_economist() +
  scale_colour_economist() +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Where the main extra attempt occurs")

#-------- LOGISTIC REGRESSION ---------------

inputData <- data[2:13]

# Create Training Data
input_ones <- inputData[which(inputData$status == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$status == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  
# 1's for training

input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  
# 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

logitMod <- glm(status ~ stuck + surv_time + extra_attempts +
                main_extra_attempt + mean_time +  gender + prom_code, data=trainingData, 
                family=binomial(link="logit"))

predicted <- predict(logitMod, testData, type="response")  # predicted scores
predicted

library(InformationValue)
optCutOff <- optimalCutoff(testData$status, predicted)[1] 

summary(logitMod)

library(car)
vif(logitMod)

misClassError(testData$status, predicted, threshold = optCutOff)

plotROC(testData$status, predicted)

Concordance(testData$status, predicted)

sensitivity(testData$status, predicted, threshold = optCutOff)

specificity(testData$status, predicted, threshold = optCutOff)
