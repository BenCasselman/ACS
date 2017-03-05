# Replicating/expanding on Pew work: http://www.pewsocialtrends.org/2016/05/24/for-first-time-in-modern-era-living-with-parents-edges-out-other-living-arrangements-for-18-to-34-year-olds/

library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RSQLite)

ACS_db <- src_sqlite("ACS_db.sqlite3", create = T)

working <- read_csv("home.csv")

partners <- working %>%
  filter(RELATED == 1114) %>%
  select(SERIAL, YEAR)

working$partner <- 0
working$partner[(working$SERIAL %in% partners$SERIAL) & working$YEAR == partners$YEAR] <- 1

rm(partners)
gc(verbose = T)

younghome <- copy_to(ACS_db, working, 
                     name = "young_home_data", temporary = FALSE)

rm(working)

# Connect to DB
ACS_db <- src_sqlite("ACS_db.sqlite3", create = F)

# Create a table of all the 2014 data. This won't actually pull it into memory.
younghome <- tbl(ACS_db, sql("SELECT * FROM young_home_data"))

younghome <- younghome %>%
  filter(AGE >= 18, AGE < 35, YEAR >= 1960) %>%
  select(SERIAL, YEAR, HHWT, GQ, GQTYPE, HHINCOME, PERWT, NCHILD, RELATE, RELATED, SEX, AGE, MARST, RACE, HISPAN,
         SCHOOL, EDUC, EDUCD, EMPSTAT, WKSWORK2, UHRSWORK, INCTOT)

partners <- younghome %>%
  filter(RELATED == 1114) %>%
  select(SERIAL, YEAR) %>%
  collect()
partners$partner <- 1

working <- younghome %>%
  collect()

working <- left_join(working, partners, by = c("SERIAL", "YEAR"))
rm(partners)

working <- working %>%
  mutate(partner = ifelse(is.na(partner), 0, 1),
         home = ifelse(RELATE== 3, "Home",
                       ifelse(RELATE == 1 & MARST %in% c(1,2), "Married",
                              ifelse(RELATE == 2, "Married",
                                     ifelse(RELATED == 1114, "Married",
                                            ifelse(RELATE == 1 & partner == 1, "Married",
                                                   ifelse(RELATE == 1, "Alone", "Other")))))))
         
save.image()

table <- working %>% 
  filter(YEAR >= 1960) %>% 
  mutate(agegroup = cut(AGE, breaks = c(0, 24, 100), labels = c("Younger", "Older"))) %>%
  group_by(YEAR, home, agegroup) %>%
  summarize(count = sum(PERWT))

# Replicate Pew chart
table %>%
  filter(YEAR %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2014)) %>%
  group_by(YEAR, home) %>%
  summarize(count = sum(count)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(YEAR, Home, Married) %>%
  gather(home, value, -YEAR) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() 

working %>% 
  filter(YEAR %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2014)) %>%
  group_by(YEAR, home, SEX) %>%
  summarize(count = sum(PERWT)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(YEAR, Home, Married, SEX) %>% 
  gather(home, value, -YEAR, -SEX) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_grid(~SEX)

# Split by age groups
table %>%
  filter(YEAR %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2014)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(YEAR, Home, Married, agegroup) %>%
  gather(home, value, -YEAR, -agegroup) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_grid(~agegroup)


table %>%
  filter(YEAR >= 1960) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(YEAR, Home, agegroup) %>%
  gather(home, value, -YEAR, -agegroup) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_grid(~agegroup)

table %>%
  filter(YEAR >= 2000) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(YEAR, Home, Married, agegroup) %>%
  gather(home, value, -YEAR, -agegroup) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_grid(~agegroup)

# Demographics
working %>% 
  filter(YEAR >= 1960) %>% 
  mutate(EDUC = as.numeric(EDUC),
         agegroup = cut(AGE, breaks = c(0, 24, 100), labels = c("Younger", "Older")),
         college = cut(EDUC, breaks = c(0, 6, 9, 11), labels = c("HS or less", "Some college", "BA+"))) %>%
  group_by(YEAR, home, agegroup, college) %>%
  summarize(count = sum(PERWT)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(-total, -Alone, -Other) %>%
  filter(!is.na(college), agegroup == "Older") %>%
  gather(home, value, -YEAR, -agegroup, -college) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_grid(~college)

working %>% 
  filter(YEAR >= 1960) %>% 
  mutate(EMPSTAT = factor(EMPSTAT, labels = c("Employed", "Unemployed", "Not in labor force")),
         SEX = factor(SEX, labels = c("Men", "Women")),
         agegroup = cut(AGE, breaks = c(0, 24, 100), labels = c("Younger", "Older"))) %>%
  group_by(YEAR, home, agegroup, EMPSTAT, SEX) %>%
  summarize(count = sum(PERWT)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(-total, -Alone, -Other) %>%
  filter(agegroup == "Younger") %>%
  gather(home, value, -YEAR, -agegroup, -EMPSTAT, -SEX) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_wrap(~SEX + EMPSTAT)

working %>% 
  filter(YEAR >= 1980) %>% 
  mutate(job = ifelse(EMPSTAT == 2, "Unemployed", 
                      ifelse(EMPSTAT == 3, "NILF",
                             ifelse(EMPSTAT == 1 & WKSWORK2 == 6 & UHRSWORK >=35, "FT", "PT"))),
             SEX = factor(SEX, labels = c("Men", "Women")),
         agegroup = cut(AGE, breaks = c(0, 24, 100), labels = c("Younger", "Older"))) %>%
  group_by(YEAR, home, job, SEX) %>%
  summarize(count = sum(PERWT)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(-total, -Alone, -Other) %>%
  gather(home, value, -YEAR, -job, -SEX) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() + facet_wrap(~SEX + job)


# Share of 25-year-olds living at home
working %>%
  filter(AGE == 25) %>%
  mutate(mom = ifelse(home == "Home", PERWT, 0)) %>%
  group_by(YEAR) %>%
  summarize(home = sum(mom)/sum(PERWT))

working %>%
  filter(AGE == 25) %>%
  mutate(mom = ifelse(home == "Home", PERWT, 0),
         SEX = factor(SEX, labels = c("Men", "Women"))) %>%
  group_by(YEAR, SEX) %>%
  summarize(home = sum(mom)/sum(PERWT)) %>%
  spread(SEX, home)

working %>%
  filter(AGE == 25) %>%
  mutate(mom = ifelse(home == "Home", PERWT, 0),
         SEX = factor(SEX, labels = c("Men", "Women"))) %>%
  group_by(YEAR, SEX) %>%
  summarize(home = sum(mom)/sum(PERWT)) %>%
  ggplot(., aes(YEAR, home, colour = SEX)) + geom_line()

table %>%
  group_by(YEAR, home) %>%
  summarize(count = sum(count)) %>%
  spread(home, count) %>%
  mutate(total = Home + Married + Alone + Other,
         Home = Home/total, Married = Married/total, Alone = Alone/total, Other = Other/total) %>%
  select(-total) %>%
  gather(home, value, -YEAR) %>%
  ggplot(., aes(YEAR, value, colour = home)) + geom_line() 






working %>% 
  filter(YEAR >= 1980) %>% 
  group_by(YEAR, home) %>%
  summarize(count = sum(PERWT)) %>%
  spread(home, count) %>%
  mutate(total = Home + Partner + Married + Head + Other,
         Home = Home/total, Married = (Married + Partner)/total, Head = Head/total, Other = Other/total) %>%
  select(-total, -Partner)


working %>% 
  filter(YEAR == 2014) %>% 
  mutate(home = ifelse(RELATE== 3, "Home",
                       ifelse(RELATE == 1 & MARST %in% c(1), "Married",
                              ifelse(RELATE == 2, "Married",
                                     ifelse(RELATED == 1114, "Married",
                                            ifelse(RELATE == 1 & partner == 1, "Married",
                                                   ifelse(RELATE == 1, "Alone", "Other"))))))) %>% 
  group_by(home) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(count = count/sum(count))

