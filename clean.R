library(readr)
library(tidyverse)

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
  ungroup() %>%
  rename(GENDER = SESSO, AGE = ETA)

# get ids of people
ids <- API_Subcription %>%
  pull(USER_ID) %>% unique()

# some users just have one conclude without the previous steps


# function to calculate the survival time
getsurvtime <- function(){
  if('conclude' %in% x$COMPLETED_STEP){
    final <- max(filter(x,COMPLETED_STEP=="conclude")$DATE_EVENT)
    initial <- min(x$DATE_EVENT)
    difftime(final, initial, units = "hours")
  }else{
    # the last time the dataset was extracted
    final <- as.POSIXct("2020-11-04 23:35:00", tz = "UCT")
    initial <- min(x$DATE_EVENT)
    difftime(final, initial, units = "hours")
  }
}

# we need to count for how long each client has survived
data <- tibble()
count_ids = 0
perc = 0

for(i in ids) {
  x <- API_Subcription %>%
    filter(USER_ID == i)
  
  # survival time in hours
  time_diff <- getsurvtime()
  
  mean_time <- difftime(max(x$DATE_EVENT), min(x$DATE_EVENT),
                        units = "hours")/nrow(x)
  
  extra_attempts <- 0
  for(s in x$COMPLETED_STEP){
    ctesp <- x %>%
      filter(COMPLETED_STEP == s) %>%
      nrow()
  extra_attempts <- extra_attempts + ctesp - 1
  }
  
  # status 1 if death occurred (conclude)
  
  status <- ifelse(nrow(filter(x,COMPLETED_STEP=='conclude'))==0,0,1)
  
  main_extra_attempt <- filter(filter(x, attempt > 1),
                               attempt==max(attempt))$COMPLETED_STEP[1]
  
  stuck <- x %>%
    filter(DATE_EVENT==max(DATE_EVENT)) %>%
    pull(COMPLETED_STEP)
  
  prom <- x$TEMP_PROMOTION_CODE_ID[1]
  
  data <- data %>%
    bind_rows(
      tibble(
        CLIENT_id = i,
        # time in seconds
        surv_time = as.double(time_diff),
        status = status,
        extra_attempts = extra_attempts,
        main_extra_attempt = main_extra_attempt,
        stuck = stuck,
        mean_time = mean_time,
        gender = x$GENDER[1],
        age = x$AGE[1],
        prom_code_id = prom
      )
    )
  
  # track percentage until complete
  count_ids <- count_ids + 1
  
  if(perc!=round((count_ids/length(ids))*100)){
    print(paste0("complete",round((count_ids/length(ids))*100),"%"))
  }
  perc <- round((count_ids/length(ids))*100)
}

write_csv(data,"data.csv")
