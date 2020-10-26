library(readr)
library(tidyverse)
library(survival)
library(ggfortify)
library(ggthemes)

data <- read_csv("data.csv") %>%
  # extra attempts dummy
  mutate(extra_attempts_dummy = ifelse(extra_attempts==0,0,1)) %>%
  mutate(main_extra_attempt = ifelse(is.na(main_extra_attempt),"none",main_extra_attempt)) %>% 
  mutate(main_extra_attempt = factor(main_extra_attempt))

# many outliers for survival time
# few clients go through the system for a long time
boxplot(data$surv_time,main="Many outliers going throuh the \n system for a long time")

# donut charts to explore the data
type <- c("status","extra_attempts_dummy","main_extra_attempt","stuck")

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

summary(km_fit)

# as time passes the probability of concluding increases
autoplot(km_fit) +
  ggtitle("Kaplan-Meier estimates of \n the probability of survival over time") +
  theme_economist()

# now we look at the survival curve between who makes extra attempts and who doesn't
km_grp_fit <- survfit(Surv(surv_time, status) ~ extra_attempts_dummy, data=data)
# who makes extra attempts is more likely not to conclude and there are 
# small overlaps between the curves
autoplot(km_grp_fit) +
  theme_economist() +
  scale_fill_economist() +
  scale_colour_economist() +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Extra attempts dummy")
  
summary(km_grp_fit)

# now we compare groups where the main stuck occurs
# remove the ones with a too high confidence interval
data_reduced <- data %>% 
  filter(!main_extra_attempt %in% 
           c("antiriciclaggio","rapporto","contract-subscription"))

# there are still a lot of overlapps with confidence intervals though
km_grp_fit <- survfit(Surv(surv_time, status) ~ main_extra_attempt, data=data_reduced)
autoplot(km_grp_fit) +
theme_economist() +
  scale_fill_economist() +
  scale_colour_economist() +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Where the main extra attempt occurs")

summary(km_grp_fit)

# Look at where the client was last stuck
data_reduced <- data %>% 
  filter(!stuck %in% 
           c("rapporto"))

km_grp_fit <- survfit(Surv(surv_time, status) ~ stuck, data=data_reduced)
autoplot(km_grp_fit) +
  theme_economist() +
  scale_fill_economist() +
  scale_colour_economist() +
  labs(title = "Kaplan-Meier estimates of the probability of survival over time",
       subtitle = "Where the client is stuck") +
  theme(legend.text = element_text(size=10))

summary(km_grp_fit)


#-------- LOGISTIC REGRESSION ---------------

inputData <- data[2:7]

# Create Training Data
input_ones <- inputData[which(inputData$status == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$status == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("status", "stuck")
continuous_vars <- c("surv_time", "extra_attempts", "mean_time")

#-----
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(5))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="status", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="status", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df
#-----

logitMod <- glm(status ~ stuck + surv_time + extra_attempts + mean_time, data=trainingData, family=binomial(link="logit"))

predicted <- predict(logitMod, testData, type="response")  # predicted scores
predicted

library(InformationValue)
optCutOff <- optimalCutoff(testData$status, predicted)[1] 

summary(logitMod)

library(car)
vif(logitMod)

misClassError(testData$status, predicted, threshold = optCutOff)

plotROC(testData$status, predicted)

