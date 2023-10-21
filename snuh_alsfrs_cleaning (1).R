library(readxl)
library(tidyverse)

# read data; 2017-06-12 to 2021-07-26
frs1 <- read_excel("data/20170612_20210726_ALSFRS.xlsx",
col_types = c("text", "skip", "text",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "skip"))

frs1$`》 작성일자 》` = as.Date(frs1$`》 작성일자 》`, format = "%Y-%m-%d")
range(frs1$`》 작성일자 》`, na.rm = T)
dim(frs1)

# read data 2017-04-20 to 2019-10-30
frs2 <- read_excel("data/20170420_20191030_ALSFRS.xlsx",
col_types = c("text", "skip", "text",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "skip", "skip"))
frs2$`》 작성일자 》` = as.Date(frs2$`》 작성일자 》`, format = "%Y-%m-%d")
range(frs2$`》 작성일자 》`, na.rm = T)
dim(frs2)

names(frs1) = c("Hosp_ID", "Date_visit", "Q1", "Q2", "Q3", 
                "Q4", "Q5a", "Q5b", "Q6", "Q7", "Q8", "Q9",
                "Q10", "Q11", "Q12", "ALSFRS_Total")
names(frs2) = c("Hosp_ID", "Date_visit", "Q1", "Q2", "Q3", 
                "Q4", "Q5a", "Q5b", "Q6", "Q7", "Q8", "Q9",
                "Q10", "Q11", "Q12", "ALSFRS_Total")

summary(frs1)
summary(frs2)

frs = rbind(frs1, frs2)
dim(frs) # 2423 records 

# exclude duplicates 
frs = distinct(frs) # exclude 2 records (duplicates)
dim(frs) # 2421 records 

# exclude Date_visit missing 
date_na = frs %>%
  filter(is.na(Date_visit)) 
dim(date_na) # exclude 103 records (Date_visit missing)

write_excel_csv(date_na, "date_na.csv")

temp_excl_date_na = frs %>%
  anti_join(date_na, by = c("Hosp_ID","Date_visit"))
dim(temp_excl_date_na) # 2318 records 
frs = temp_excl_date_na

# exclude inconsistent data 
temp_incon = frs %>%
  count(Hosp_ID, Date_visit) %>%
  filter(n > 1)

dim(temp_incon) # exclude 4 records (inconsistent)

temp_incon = frs %>%
  inner_join(temp_incon, by = c("Hosp_ID", "Date_visit")) %>%
  arrange(Hosp_ID)

write_excel_csv(temp_incon, "alsfrs_inconsistent.csv")

temp_excl_incon <- frs %>%
  anti_join(temp_incon, by = c("Hosp_ID", "Date_visit"))

dim(temp_excl_incon) # 2310 records 
frs = temp_excl_incon

# exclude Q5a and Q5b, both missing 
q5_na = frs %>%
  filter(is.na(Q5a) & is.na(Q5b))
dim(q5_na) # exclude 18 records (both Q5a and Q5b missing)

write_excel_csv(q5_na, "Q5_missing.csv")

temp_excl_q5_na = frs %>%
  anti_join(q5_na, by = c("Hosp_ID", "Date_visit"))

frs = temp_excl_q5_na
dim(frs) # 2292 records 

# exclude Q5a and Q5b, both filled-in 
q5_both = frs %>%
  filter(!is.na(Q5a)) %>%
  filter(!is.na(Q5b))
dim(q5_both) # exclude 1 record with both Q5a and Q5b filled in

write_excel_csv(q5_both, "Q5_both.csv")

temp_excl_q5_both = frs %>%
  anti_join(q5_both, by = c("Hosp_ID", "Date_visit"))

frs = temp_excl_q5_both
dim(frs) # 2291 records 

# exclude records with missing values 
temp = frs %>%
  select(-c(Q5a, Q5b))
temp_missing = temp[!complete.cases(temp),] %>%
  arrange(Hosp_ID)
dim(temp_missing) # exclude 41 records with missing values 

write_excel_csv(temp_missing, "missing_item.csv")

temp_excl_missing = frs %>%
  anti_join(temp_missing, by = c("Hosp_ID", "Date_visit"))

dim(temp_excl_missing) # 2250 records 
frs = temp_excl_missing

dim(frs) # 2250 records 
length(unique(frs$Hosp_ID)) # 699 patients 
min(frs$Date_visit, na.rm = T) # "2017-04-20"
max(frs$Date_visit, na.rm = T) # "2021-07-26"

# create gastrostomy field 
frs = frs %>%
  mutate(gastrostomy = ifelse(is.na(Q5a), T, F)) %>%
  mutate(Q5 = ifelse(is.na(Q5a), Q5b, Q5a)) %>%
  select(-c(Q5a, Q5b))

saveRDS(frs, "data/alsfrs_cleaned.RDS")

