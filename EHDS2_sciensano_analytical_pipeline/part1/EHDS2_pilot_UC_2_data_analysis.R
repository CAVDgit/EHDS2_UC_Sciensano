# Script that generates the aggregate results into one csv file with 
# statistics related to each research questions.
# script_version <- "1.0"

library(tidyverse)
library(DT)

###################### INSTRUCTIONS #######################
# Modify the sections 1. to 4. then execute the script.

# 1. Place in a same folder, this script and your individual data
# And set the working directory to the location of this script.

# ## ######################################################
#################### PATH to DATA (CSV) ###################
###########################################################
# specify the file name of your individual data

file_path <- sprintf("EHDS2_pilot_UC1_mockup_data_%s.csv", country)

# 2. ######################################################
########## Enter the country CODE of your node ############
###########################################################
###########################################################

# Accepted value : BE, FR, FI, HU, HR, DK
country <- 'BE'

# 3. ######################################################
#################### Cut-off / Threshold ##################
##### To excluded aggregated data with number of ##########
##### individual inferior to this threshold ###############
###########################################################

# For example, threshold = 10 means that all groups with less then 10 individuals (10 excluded) 
# are excluded from the final aggregated output. 
# Excluded groups will be represented in the output with n = -1 and associated data will not be captured

threshold <- 10

# 4. ######################################################
#######################  Cohort limit #####################
###########################################################

date_from <- as.Date('2021-01-01')  
date_to <- as.Date('2022-12-31')
age_max <- 115
age_min <- 18

# Do not modify any lines after this point. !!!




###########################################################
##################### Data analysis #######################
###########################################################

data_types <- c(
  person_id = "character",
  sex_cd = "character",
  age_nm = "integer",
  age_cd = "character",
  exitus_dt = "Date",
  exitus_bl = "logical",
  country_origin_cd = "character",
  country_cd = "character",
  residence_area_nuts2_cd = "character",
  residence_area_cd = "character",
  residence_area_lau_cd = "character",
  education_level_cd = "character",
  income_category_cd = "character",
  migration_background_cd = "character",
  household_type_cd = "character",
  hospi_due_to_covid_bl = "logical",
  test_positive_to_covid_nm = "integer",
  test_nm = "integer",
  dose_1_brand_cd = "character",
  dose_1_dt = "Date",
  dose_2_brand_cd = "character",
  dose_2_dt = "Date",
  dose_3_brand_cd = "character",
  dose_3_dt = "Date",
  doses_nm = "integer",
  fully_vaccinated_dt = "Date",
  fully_vaccinated_bl = "logical"
)

df <- read.csv(file_path, colClasses = data_types, header = TRUE)


# map gender
df$sex_cd <- ifelse(df$sex_cd == 1, 'M',
                    ifelse(df$sex_cd == 2, 'F', NA))

# remove NA from essential variable
df_clean <- df %>% filter(!is.na(person_id),
                          !is.na(sex_cd),
                          !is.na(country_cd),
                          age_nm >= age_min & age_nm <age_max,
                          exitus_bl == F)

# Research questions (RQ)
columns_to_aggregate <- c("age_cd",
                          "sex_cd",
                          "education_level_cd",
                          "income_category_cd",
                          "migration_background_cd",
                          "household_type_cd",
                          "residence_area_nuts2_cd",
                          "residence_area_cd",
                          "residence_area_lau_cd")

# Count number of individuals
individuals_nm <- count(df_clean)
individuals_nm <- individuals_nm[1, 1, drop = TRUE]
individuals_vaccinated_nm <- count(filter(df_clean, fully_vaccinated_bl == TRUE))
individuals_vaccinated_nm <- individuals_vaccinated_nm[1, 1, drop = TRUE]

individuals_not_vaccinated_nm <- count(filter(df_clean, fully_vaccinated_bl == FALSE))
individuals_not_vaccinated_nm <- individuals_not_vaccinated_nm[1, 1, drop = TRUE]

rq <- data.frame(
  RQ = "dataset_info",
  n = individuals_nm,
  range1 = individuals_vaccinated_nm,
  range2 = individuals_not_vaccinated_nm,
  range3 = threshold
  
)

dataset_info2 <- data.frame(
  RQ = "dataset_info2",
  range1 = age_min,
  range2 = age_max,
  education_level_cd = as.character(date_from),
  income_category_cd = as.character(date_to)
)

rq <- bind_rows(rq, dataset_info2)

#### RQ1 

# distribution of test
# range1 == all tests
df_tests <- df_clean %>% 
  count(test_nm) %>% 
  mutate(ratio = n / sum(n), 
         RQ = "rq1_distribution", 
         range1 = test_nm ) %>% 
  select(-test_nm) %>%
  mutate(n = ifelse(n < threshold, -1, n),
         ratio = ifelse(n < threshold, -1, ratio))

# range1 == positive tests
df_pos_tests <- df_clean %>% 
  count(test_positive_to_covid_nm) %>% 
  mutate(ratio = n / sum(n), 
         RQ = "rq2_distribution", 
         range1 = test_positive_to_covid_nm) %>% 
  select(-test_positive_to_covid_nm) %>%
  mutate(n = ifelse(n < threshold, -1, n),
         ratio = ifelse(n < threshold, -1, ratio))

rq <- bind_rows(rq, df_tests)
rq <- bind_rows(rq, df_pos_tests)


# summary
rq1_summary <- df_clean %>% 
  filter(!is.na(education_level_cd), 
         !is.na(income_category_cd), 
         !is.na(household_type_cd), 
         !is.na(migration_background_cd), 
         !is.na(test_nm)) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
  summarize(q1 = quantile(test_nm, 0.25),
            median = median(test_nm),
            mean = mean(test_nm),
            q3 = quantile(test_nm, 0.75),
            sd = sd(test_nm),
            n = n()) %>%
  mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
         upperfence = q3 + 1.5 * (q3 - q1),
         n = ifelse(n < threshold, -1, n),
         q1 = ifelse(n < threshold, NA, q1),
         median = ifelse(n < threshold, NA, median),
         mean = ifelse(n < threshold, NA, mean),
         q3 = ifelse(n < threshold, NA, q3),
         sd = ifelse(n < threshold, NA, sd),
         lowerfence = ifelse(n < threshold, NA, lowerfence),
         upperfence = ifelse(n < threshold, NA, upperfence),
         RQ = "rq1_summary")

rq <- bind_rows(rq, rq1_summary)


# education, income, household, migration, gender, sex
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), !is.na(test_nm)) %>%
    group_by_at(vars(col)) %>%
    summarize(q1 = quantile(test_nm, 0.25),
              median = median(test_nm),
              mean = mean(test_nm),
              q3 = quantile(test_nm, 0.75),
              sd = sd(test_nm),
              n = n()) %>%
    mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
           upperfence = q3 + 1.5 * (q3 - q1),
           n = ifelse(n < threshold, -1, n),
           q1 = ifelse(n < threshold, NA, q1),
           median = ifelse(n < threshold, NA, median),
           mean = ifelse(n < threshold, NA, mean),
           q3 = ifelse(n < threshold, NA, q3),
           sd = ifelse(n < threshold, NA, sd),
           lowerfence = ifelse(n < threshold, NA, lowerfence),
           upperfence = ifelse(n < threshold, NA, upperfence))

  stats_df$RQ <- paste0("rq1_", col)
  
  
  rq <- bind_rows(rq, stats_df)
}


# number of tests
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>% 
    filter(!is.na(get(col)), !is.na(test_nm)) %>%
    mutate(range1 = ifelse(test_nm < 1, 1, 0),
           range2 = ifelse(test_nm >= 1 & test_nm <= 5, 1, 0),
           range3 = ifelse(test_nm > 5, 1, 0)) %>%
    group_by_at(vars(col)) %>%
    summarize(range1 = sum(range1),
              range2 = sum(range2),
              range3 = sum(range3),
              n = n()) %>%
    mutate(n = ifelse(n < threshold, -1, n),
           range1 = ifelse(range1 < threshold, -1, range1),
           range2 = ifelse(range2 < threshold, -1, range2),
           range3 = ifelse(range3 < threshold, -1, range3))
  stats_df$RQ <- paste0("rq1_", col, "_test")

  rq <- bind_rows(rq, stats_df)
}


#### RQ2
# summary
rq2_summary <- df_clean %>% 
  filter(!is.na(education_level_cd), 
         !is.na(income_category_cd), 
         !is.na(household_type_cd), 
         !is.na(migration_background_cd), 
         !is.na(test_positive_to_covid_nm)) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
  summarize(q1 = quantile(test_positive_to_covid_nm, 0.25),
            median = median(test_positive_to_covid_nm),
            mean = mean(test_positive_to_covid_nm),
            q3 = quantile(test_positive_to_covid_nm, 0.75),
            sd = sd(test_positive_to_covid_nm),
            n = n()) %>% 
  mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
         upperfence = q3 + 1.5 * (q3 - q1),
         n = ifelse(n < threshold, -1, n),
         q1 = ifelse(n < threshold, NA, q1),
         median = ifelse(n < threshold, NA, median),
         mean = ifelse(n < threshold, NA, mean),
         q3 = ifelse(n < threshold, NA, q3),
         sd = ifelse(n < threshold, NA, sd),
         lowerfence = ifelse(n < threshold, NA, lowerfence),
         upperfence = ifelse(n < threshold, NA, upperfence),
         RQ = "rq2_summary")

rq <- bind_rows(rq, rq2_summary)


# education, income, household, migration, gender, sex
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), 
           !is.na(test_positive_to_covid_nm)) %>%
    group_by_at(vars(col)) %>%
    summarize(q1 = quantile(test_positive_to_covid_nm, 0.25),
              median = median(test_positive_to_covid_nm),
              mean = mean(test_positive_to_covid_nm),
              q3 = quantile(test_positive_to_covid_nm, 0.75),
              sd = sd(test_positive_to_covid_nm),
              n = n()) %>%
    mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
           upperfence = q3 + 1.5 * (q3 - q1),
           n = ifelse(n < threshold, -1, n),
           q1 = ifelse(n < threshold, NA, q1),
           median = ifelse(n < threshold, NA, median),
           mean = ifelse(n < threshold, NA, mean),
           q3 = ifelse(n < threshold, NA, q3),
           sd = ifelse(n < threshold, NA, sd),
           lowerfence = ifelse(n < threshold, NA, lowerfence),
           upperfence = ifelse(n < threshold, NA, upperfence))
  
  stats_df$RQ <- paste0("rq2_", col)
  
  rq <- bind_rows(rq, stats_df)
}

# number of tests
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>% 
    filter(!is.na(get(col)), !is.na(test_positive_to_covid_nm)) %>%
    mutate(range1 = ifelse(test_positive_to_covid_nm < 1, 1, 0),
           range2 = ifelse(test_positive_to_covid_nm >= 1 & test_positive_to_covid_nm <= 5, 1, 0),
           range3 = ifelse(test_positive_to_covid_nm > 5, 1, 0)) %>%
    group_by_at(vars(col)) %>%
    summarize(range1 = sum(range1),
              range2 = sum(range2),
              range3 = sum(range3),
              n = n()) %>%
    mutate(n = ifelse(n < threshold, -1, n),
           range1 = ifelse(range1 < threshold, -1, range1),
           range2 = ifelse(range2 < threshold, -1, range2),
           range3 = ifelse(range3 < threshold, -1, range3))
  stats_df$RQ <- paste0("rq2_", col, "_positive_test")
  
  rq <- bind_rows(rq, stats_df)
}


#### rq3-4
rq3_4_general <- df_clean %>% 
  filter(!is.na(hospi_due_to_covid_bl),
         !is.na(fully_vaccinated_bl)) %>%
  group_by(hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = n()) %>%
  mutate(RQ = "rq3_4_general",
         n = ifelse(n < threshold, -1, n))

rq <- bind_rows(rq, rq3_4_general)

#### rq3
rq3_summary <- df_clean %>% 
  filter(!is.na(education_level_cd), 
         !is.na(income_category_cd), 
         !is.na(household_type_cd), 
         !is.na(migration_background_cd), 
         !is.na(fully_vaccinated_bl)) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
  summarise(range1 = sum(fully_vaccinated_bl == TRUE),
            n = n()) %>%
  mutate(RQ = "rq3_summary",
         n = ifelse(n < threshold, -1, n),
         range1 = ifelse(range1 < threshold, -1, range1))


rq <- bind_rows(rq, rq3_summary)


# distribution of vaccine doses
df_doses <- df_clean %>% 
  count(doses_nm) %>% 
  mutate(ratio = n / sum(n), 
         RQ = "rq3_distribution", 
         range1 = doses_nm) %>% 
  select(-doses_nm) %>%
  mutate(n = ifelse(n < threshold, -1, n),
         ratio = ifelse(n < threshold, -1, ratio))

rq <- bind_rows(rq, df_doses)


for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), 
           !is.na(fully_vaccinated_bl)) %>%
    group_by_at(vars(col, 
                     fully_vaccinated_bl)) %>%
    summarise(n = n()) %>%
    mutate(n = ifelse(n < threshold, -1, n))
  
  stats_df$RQ <- paste0("rq3_", col)
  
  rq <- bind_rows(rq, stats_df)
}


# number of doses
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>% 
    filter(!is.na(get(col)), !is.na(doses_nm)) %>%
    mutate(range1 = ifelse(doses_nm < 1, 1, 0),
           range2 = ifelse(doses_nm == 1, 1, 0),
           range3 = ifelse(doses_nm > 1, 1, 0)) %>%
    group_by_at(vars(col)) %>%
    summarize(range1 = sum(range1),
              range2 = sum(range2),
              range3 = sum(range3),
              n = n()) %>%
    mutate(n = ifelse(n < threshold, -1, n),
           range1 = ifelse(range1 < threshold, -1, range1),
           range2 = ifelse(range2 < threshold, -1, range2),
           range3 = ifelse(range3 < threshold, -1, range3))
  stats_df$RQ <- paste0("rq3_", col, "_doses")
  
  rq <- bind_rows(rq, stats_df)
}


#### rq4
rq4_summary <- df_clean %>% 
  filter(!is.na(education_level_cd), 
         !is.na(income_category_cd), 
         !is.na(household_type_cd), 
         !is.na(migration_background_cd), 
         !is.na(hospi_due_to_covid_bl),
         !is.na(fully_vaccinated_bl)) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd,
           hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = n()) %>%
  mutate(RQ = "rq4_summary",
         n = ifelse(n < threshold, -1, n))

rq <- bind_rows(rq, rq4_summary)


for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), 
           !is.na(hospi_due_to_covid_bl),
           !is.na(fully_vaccinated_bl)) %>%
    group_by_at(vars(col, 
                     hospi_due_to_covid_bl, 
                     fully_vaccinated_bl)) %>%
    summarise(n = n()) %>% 
    mutate(n = ifelse(n < threshold, -1, n))
  
  stats_df$RQ <- paste0("rq4_", col)
  
  rq <- bind_rows(rq, stats_df)
}



# Mettre la colonne RQ en 1er
rq <- rq[, c("RQ", setdiff(names(rq), "RQ"))]

# Ajouter la colonne du node
rq <- rq %>% mutate(country = as.factor(country))

# Attribute the data type to each variable
aggregated_data <- rq %>%
  mutate_at(vars("RQ", "age_cd"), as.factor)

aggregated_data <- aggregated_data %>%
  mutate_at(vars("lowerfence",
                 "q1", 
                 "median",
                 "mean",
                 "q3",
                 "upperfence",
                 "ratio",
                 "sd",
                 "n"), as.numeric)

write.table(aggregated_data, 
            sprintf("EHDS2_pilot_UC1_data_%s.csv", country), 
            row.names=FALSE,
            col.names = TRUE,
            sep =" ", 
            fileEncoding = "UTF-8", 
            na = "NA")

# Test import

# test_import <- read.csv(sprintf("EHDS2_pilot_UC1_data_%s.csv", country), 
#                       header = TRUE, 
#                       sep =" ", 
#                       fileEncoding = "UTF-8", 
#                       na = "NA")
