library(tidyverse)
library(lubridate) # we'll use this for some date processing

# Read in the data
intake <- read.csv("Intake.csv")
initiation <- read.csv("Initiation.csv")

# Turn RECEIVED_DATE from strings to date objects
intake <- intake %>%
  mutate(RECEIVED_DATE = as.Date(RECEIVED_DATE, '%m/%d/%Y'))

# Number of felony filings, more or less, made by the SA every month
monthly_felonies <- (intake %>% 
                     mutate(month = round_date(RECEIVED_DATE, "month")) %>%
                     group_by(month) %>%
                     summarize(felonies=n()))

plot(felonies ~ month, data=monthly_felonies)
plot(felonies ~ month, data=monthly_felonies, type='l')

# There's been significant decline, but also weird stuff seems
# to be happening at beginning and end, let's trim that out

summary(monthly_felonies)

monthly_felonies <- filter(monthly_felonies, month > as.Date('2011-02-01'))
monthly_felonies <- filter(monthly_felonies, month < as.Date('2018-01-01'))

plot(felonies ~ month, data=monthly_felonies, type='l')
plot(felonies ~ month, data=monthly_felonies, type='l', ylim=c(0,5000))

# We are going to be doing more work with intake, so let's go ahead
# and trim the starting dataset

intake <- filter(intake, RECEIVED_DATE >= as.Date('2011-03-01'))
intake <- filter(intake, RECEIVED_DATE < as.Date('2018-01-01'))

# Now let's look at the number of cases that are directly filed by
# the police bodies in Cook County without felony review, and which
# result in a felony filing. These should just be felony drug cases

monthly_drug_felonies <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                          filter(Offense_Category == 'Narcotics') %>%
                          filter(FR_RESULT == '') %>%
                          mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                          group_by(month) %>%
                          summarise(drug_felonies = n_distinct(CASE_PARTICIPANT_ID)))

# We'll add these numbers to our total monthly numbers, lining up the
# data by month
monthly_felonies <- left_join(monthly_felonies, 
                              monthly_drug_felonies, by='month')

lines(drug_felonies ~ month, data=monthly_felonies, lty=2)

# It's hard to tell if the trend in drug cases has been different
# than the overall trend. To take a closer look let's see how proportion
# of drug cases has changed over time
plot(drug_felonies/felonies ~ month, data=monthly_felonies, type='l')

# So there's been a big drop looking at about the start of 2016. Let's
# dig in more by looking at diferent levels of charges within 
# felony drug cases
monthly_drug_felonies_4 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(CLASS == '4') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies_4 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_3 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(CLASS == '3') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies_3 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_2 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(CLASS == '2') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies_2 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_1 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(CLASS == '1') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies_1 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_x <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(CLASS == 'X') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies_x = n_distinct(CASE_PARTICIPANT_ID)))

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_4,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_3,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_2,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_1,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_x,
                              by='month')

# So Level 4 cases, which is the lowest severity class, makes up 
# the bulk of felony drug cases, and it seems that it's really 
# these cases that have dropped so much
plot(drug_felonies ~ month, 
     data=monthly_felonies, 
     type='l', 
     ylim=c(0, 2000))

lines(drug_felonies_4 ~ month, 
     data=monthly_felonies, 
     lty=2)

lines(drug_felonies_3 ~ month, 
      data=monthly_felonies, 
      lty=3)

lines(drug_felonies_2 ~ month, 
      data=monthly_felonies, 
      lty=4)

lines(drug_felonies_1 ~ month, 
      data=monthly_felonies, 
      lty=5)

lines(drug_felonies_x ~ month, 
      data=monthly_felonies, 
      lty=6)

plot(drug_felonies_4 ~ month, 
     data=monthly_felonies, 
     type='l', 
     ylim=c(0, 1500))

# Okay, let's switch gears and see how different races and ethnicities
# are being treated by the system

drug_felonies_by_race <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(FR_RESULT == '') %>% 
                            group_by(RACE.x) %>%
                            summarise(drug_felonies = n_distinct(CASE_PARTICIPANT_ID)))

drug_felonies_by_race <- arrange(drug_felonies_by_race, desc(drug_felonies))

drug_felonies_by_race
