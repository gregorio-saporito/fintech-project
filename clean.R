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
  ungroup()

# get ids of people
ids <- API_Subcription %>%
  pull(USER_ID) %>% unique()

# some users just have one conclude without the previous steps

# we need to count for how long each client has survived
data <- tibble()
count_ids = 0
perc = 0

for(i in ids) {
  x <- API_Subcription %>%
    filter(USER_ID == i)
  
  time_diff <- difftime(max(x$DATE_EVENT), min(x$DATE_EVENT),
           units = "hours")
  
  extra_attempts <- x %>% 
    group_by(COMPLETED_STEP) %>%
    filter(attempt == max(attempt)) %>%
    mutate(check = ifelse(attempt==1,0,attempt-1)) %>%
    ungroup() %>%
    summarise(extra_attempt = sum(check)) %>% pull()
  
  # status 1 if death occurred (conclude)
  
  status <- ifelse(nrow(filter(x,COMPLETED_STEP=='conclude'))==0,0,1)
  
  main_extra_attempt <- filter(filter(x, attempt > 1),attempt==max(attempt))$COMPLETED_STEP[1]
  
  stuck <- x %>%
    filter(DATE_EVENT==max(DATE_EVENT)) %>%
    pull(COMPLETED_STEP)
  
  data <- data %>%
    bind_rows(
      tibble(
        CLIENT_id = i,
        # time in seconds
        surv_time = as.double(time_diff),
        status = status,
        extra_attempts = extra_attempts,
        main_extra_attempt = main_extra_attempt,
        stuck = stuck
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