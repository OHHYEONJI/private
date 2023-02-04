# preprocessing 
# project: predict loss of autonomy in swallowing 
# validation; snuh dataset 

# EDA 
# Preprocessing 

library(tidyverse)
library(survival)
library(survminer)
library(plotly)

# read snuh alsfrs dataset (from snuh_alsfrs_cleaning.R)  
snuh_alsfrs = readRDS("data/alsfrs_cleaned.RDS")
snuh_alsfrs$Hosp_ID = as.integer(snuh_alsfrs$Hosp_ID)

# include only ALS patients 
# read snuh als registry dataset 
base = readRDS("data/base.rds")
dx = readRDS("data/dx.rds")
fu = readRDS("data/fu.rds")
fvc = readRDS("data/fu_fvc.rds")
wt = readRDS("data/fu_wt.rds")
event = readRDS("data/event.rds")
close = readRDS("data/close.rds")

# create dx dataframe supplemented with Study_ID (from base)
dx = dx %>%
  select(-c(Sex, Age_dx, Dx)) %>% # leave out the features in base 
  inner_join(base, by = "Study_ID") %>%
  select(Study_ID, Hosp_ID, Sex, Dx, Age_dx,
         Date_onset, Date_dx, Date_enrollment,
         Onset_region)

# filter out non-ALS 
dx_als = dx %>%
  filter(Dx == "ALS")
fvc_als = fvc %>%
  mutate(Study_ID = as.character(Study_ID)) %>%
  filter(Study_ID %in% dx_als$Study_ID)
wt_als = wt %>%
  mutate(Study_ID = as.character(Study_ID)) %>%
  filter(Study_ID %in% dx_als$Study_ID)

# exclude alsfrs records of non-ALS patients 
# create snuh_alsfrs dataset supplemented with Study_ID, removing Hosp_ID 
temp = dx_als %>% 
  select(c(Study_ID, Hosp_ID))
snuh_alsfrs = snuh_alsfrs %>%
  inner_join(temp, by = "Hosp_ID") 

# create snuh_dx dataset excluding patients with following criteria 
# exclude patients w/o alsfrs records 
# exclude Date_dx <= 2016-01-01
# exclude Date_onset <= 2015-01-01
# exclude Date_enrollmemnt <= 2017-01-01
snuh_dx = dx_als %>% 
  filter(Study_ID %in% snuh_alsfrs$Study_ID) %>%
  filter(Date_dx > "2016-01-01") %>%
  filter(Date_onset > "2015-01-01") %>%
  filter(Date_enrollment > "2017-01-01") %>%
  select(-c(Hosp_ID, Dx)) 
dim(snuh_dx) # 380 patients 

# create snuh_alsfrs dataset of the patients in snuh_dx dataset 
snuh_alsfrs = snuh_alsfrs %>%
  filter(Study_ID %in% snuh_dx$Study_ID)
length(unique(snuh_alsfrs$Study_ID))# 380 patients 

# exclude patients who got gastrostomy already at enrollment 
# exclude patients who scored point 1 in Q3 at enrollment 
temp = snuh_alsfrs %>%
  group_by(Study_ID) %>%
  filter(Date_visit == min(Date_visit)) %>%
  filter(Q3 <= 1)
length(temp$Study_ID) # 9 patients 

temp_excl_gastr_enroll = snuh_alsfrs %>%
  anti_join(temp, by = "Study_ID") 
dim(temp_excl_gastr_enroll) # 1221 records 
length(unique(temp_excl_gastr_enroll$Study_ID)) # 371 patients 
snuh_alsfrs = temp_excl_gastr_enroll

# exclude patients with only one record 
# how many alsfrs records (visits) per patient? 
snuh_alsfrs %>%
  count(Study_ID) %>%
  filter(n == 1) # 138 patients 
temp = snuh_alsfrs %>%
  count(Study_ID) %>%
  filter(n > 1)
temp2 = snuh_alsfrs %>%
  inner_join(temp, by = "Study_ID")
length(unique(temp2$Study_ID)) # 233 patients 
snuh_alsfrs = temp2 %>%
  select(-n)

snuh_alsfrs %>%
  count(Study_ID, Date_visit) %>%
  filter(n>1)

snuh_alsfrs = snuh_alsfrs[,c(17,1,2,14,3:6,16,7:13,15)]
dim(snuh_alsfrs)
snuh_alsfrs %>%
  select(-Hosp_ID) -> snuh_alsfrs
write.csv(snuh_alsfrs, "data/snuh_alsfrs.csv", 
          row.names = F, quote = F)

# create snuh_dx dataset 
snuh_dx = snuh_dx %>%
  filter(Study_ID %in% snuh_alsfrs$Study_ID)
length(unique(snuh_dx$Study_ID)) # 233 patients 

write.csv(snuh_dx, "data/snuh_dx.csv", row.names = F, quote = F)

# create snuh_fvc dataset 
# exclude patients w/o alsfrs records 
snuh_fvc = fvc_als %>%
  filter(Study_ID %in% snuh_alsfrs$Study_ID) %>%
  select(c(Study_ID, Date, FVC)) %>%
  group_by(Study_ID) %>%
  arrange(Date)
dim(snuh_fvc) # 4 records 
length(unique(snuh_fvc$Study_ID)) # 4 patients 

snuh_fvc_20211025 <- read_csv("data/snuh_fvc_20211025.csv")
snuh_fvc_20211025 = rename(snuh_fvc_20211025, FVC = FVC_percent, Date = Date_visit)
snuh_fvc_20211025$Date = as.Date(snuh_fvc_20211025$Date, format = "%Y.%m.%d")
snuh_fvc_20211025 = snuh_fvc_20211025 %>% select(-Hosp_ID)
snuh_fvc2 = rbind(snuh_fvc_20211025, snuh_fvc)
dim(snuh_fvc2) # 259 records 

temp = distinct(snuh_fvc2)
# exclude inconsistent data, different FVC on the same day 
temp %>%
  count(Study_ID, Date) %>%
  filter(n > 1) 

temp %>%
  group_by(Study_ID, Date) %>%
  mutate(n = n()) %>%
  filter(n == 1) -> temp2

temp3 = temp2 %>%
  group_by(Study_ID) %>%
  arrange(Date) %>%
  mutate(interval = round(as.numeric(Date - first(Date))/365*12, 1)) 

p = temp3 %>%
  ggplot(aes(interval, FVC, group = Study_ID)) + 
  geom_line() + 
  geom_point()

ggplotly(p)

dim(temp3) # 258 records 
length(unique(temp3$Study_ID)) # 169 patients

snuh_fvc = temp3 %>%
  select(-c(n, interval))

write.csv(snuh_fvc, "data/snuh_fvc.csv", row.names = F, quote = F)

# exclude wt of patients w/o alsfrs records 
snuh_wt = wt_als %>%
  filter(Study_ID %in% snuh_alsfrs$Study_ID) %>%
  select(c(Study_ID, Date, Wt)) %>%
  group_by(Study_ID) %>%
  arrange(Date)

dim(snuh_wt) # 1040
snuh_wt %>%
  count(Study_ID, Date) %>%
  filter(n>1)

snuh_wt %>%
  group_by(Study_ID, Date) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n) -> snuh_wt

dim(snuh_wt) # 1040
length(unique(snuh_wt$Study_ID)) # 232 patients 

min(snuh_wt$Date) # 0202-07-17 

snuh_wt %>%
  filter(Date > 2016-01-01) -> snuh_wt

temp = snuh_wt %>%
  group_by(Study_ID) %>%
  arrange(Date) %>%
  mutate(interval = round(as.numeric(Date - first(Date))/365*12, 1)) 

p = temp %>%
  ggplot(aes(interval, Wt, group = Study_ID)) + 
  geom_line() + 
  geom_point()

ggplotly(p)


write.csv(snuh_wt, "data/snuh_wt.csv", row.names = F, quote = F)

# Creatinine 
snuh_cr <- read_csv("data/snuh_cr.csv")
snuh_cr$Date = as.Date(snuh_cr$Date, format = "%Y.%m.%d")
snuh_cr$Cr = as.numeric(snuh_cr$Cr)
snuh_cr %>%
  count(Study_ID, Date) %>%
  filter(n>1) # 19 
dim(snuh_cr) # 1084 
snuh_cr %>%
  group_by(Study_ID, Date) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n) -> temp
dim(temp) # 1045
snuh_cr = temp

temp = snuh_cr %>%
  group_by(Study_ID) %>%
  arrange(Date) %>%
  mutate(interval = round(as.numeric(Date - first(Date))/365*12, 1)) 

p = temp %>%
  ggplot(aes(interval, Cr, group = Study_ID)) + 
  geom_line() + 
  geom_point()

ggplotly(p)

snuh_cr %>%
  select(-Hosp_ID) -> snuh_cr

write.csv(snuh_cr, "data/snuh_cr_cleaned.csv", row.names = F, quote = F)

# Height 
snuh_height <- read_csv("data/snuh_height.csv")
dim(snuh_height)
temp = distinct(snuh_height)
dim(temp)

# EDA 

# how long the time interval between the first and last visit? 
temp = snuh_alsfrs %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  mutate(interval = round(as.numeric(Date_visit - first(Date_visit))/365*12, 1)) 

temp %>%
  ggplot(aes(interval, ALSFRS_Total, group = Study_ID)) + 
  geom_line() + 
  geom_point()

# Q3 item point 1, censoring rate? 
temp %>%
  group_by(Study_ID) %>%
  summarise(event = any(Q3 <= 1)) -> temp2
table(temp2$event) # 29/233 = 12.4% (event rate)

# fu duration 
temp %>%
  group_by(Study_ID) %>%
  summarize(fu_dur = max(interval) - min(interval)) -> temp3 
summary(temp3$fu_dur) # median 8.3 months (IQR, 4.1 - 16.8, range 0.7 - 50.3 months)

# KM curve 
# patients in whom the event occured; group 1
df1 = temp %>%
  group_by(Study_ID) %>%
  nest() %>%
  mutate(event = map_lgl(data, ~any(.x$Q3 <= 1))) %>%
  filter(event == TRUE) 
# patients in whom the event did not occur; group 2
df2 = temp %>%
  group_by(Study_ID) %>%
  nest() %>%
  mutate(event = map_lgl(data, ~any(.x$Q3 <= 1))) %>%
  filter(event == FALSE) 
# time to event in group 1
temp1 = df1 %>%
  mutate(time = map(data, ~filter(.x, Q3 <= 1))) %>%
  mutate(time2 = map_dbl(time, ~min(.x$interval))) %>%
  select(-time)
range(temp1$time2) 
# time to censoring in group 2
temp2 = df2 %>%
  mutate(time2 = map_dbl(data, ~max(.x$interval)))

snuh_alsfrs_event = rbind(temp1, temp2)
df = snuh_alsfrs_event %>%
  select(-data) %>%
  mutate(event = ifelse(event == TRUE, 1, 0)) %>%
  rename(time = time2)

# KM curve with risk table 
surv = with(df, Surv(time, event))
km = survfit(surv ~ 1)
ggsurvplot(km, data = df, 
           risk.table = T) # a lot of censoring at the early stage

# gastrostomy from event data table  
gastro = event %>%
  filter(Event == "Gastrostomy")
base_als = base %>%
  filter(Dx == "ALS") %>%
  select(c(Study_ID, Hosp_ID))
snuh_gastro = gastro %>%
  inner_join(base_als, by = "Study_ID") %>%
  select(c("Study_ID", "Hosp_ID", "Event", "Date_event"))
snuh_gastro = snuh_gastro %>%
  filter(Study_ID %in% snuh_dx$Study_ID) 
length(unique(snuh_gastro$Study_ID)) # 41 patients 

snuh_gastro %>%
  select(-Hosp_ID) -> snuh_gastro
write.csv(snuh_gastro, "data/snuh_gastro.csv", row.names = F, 
          quote = F)




