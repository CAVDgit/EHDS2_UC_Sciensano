# Script that generates the aggregate results into one csv file with 
# statistics related to each research questions.

# library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(DT)


# Replace by your country abbreviation (BE for Belgium, FR for France ...)
country <- "FR"


# variables
now_date  <- as.Date("2021-01-01")
date_from <- as.Date('2021-01-01')  
date_to <- as.Date('2022-12-31')
age_max <- 115
age_min <- 18

# read the file

df <- read_delim("use_case_1_synthetic_data_50K_individuals.csv", trim_ws = TRUE)
df <- df %>% filter(age_nm >= age_min, age_nm < age_max)
dt_ind <- as.data.frame(introduce(df))


# Remove missing values
required_v <- 
  c("person_id","sex_cd","age_nm","exitus_bl","country_origin_cd",
    "country_cd","education_level_cd","income_category_cd","migration_background_cd",
    "household_type_cd","hospi_due_to_covid_bl","test_positive_to_covid_nm",
    "test_nm","dose_1_brand_cd","dose_1_dt","dose_2_brand_cd","dose_2_dt","doses_nm","fully_vaccinated_bl")

df_clean <- df[complete.cases(df[, required_v]), ]

# Count number of individuals
individuals_nm <- count(df_clean)

# Create additional columns
df_clean$age_group <- cut(df_clean$age_nm, breaks = c(0, 18, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115), 
                          right = FALSE, 
                          labels = c("0-18", "18-25", "25-35", "35-45", "45-55", "55-65", "65-75", "75-85", "85-95", "95-105", "105-115"))

df_clean$sex_cd <- as.factor(ifelse(df_clean$sex_cd == 1, 'M',
                                    ifelse(df_clean$sex_cd == 2, 'F', NA)))

# Attribute data types
df_clean <- df_clean %>%
  mutate_at(vars("person_id",
                 "sex_cd",
                 "country_cd",
                 "country_origin_cd",
                 "residence_area_cd",
                 "education_level_cd", 
                 "income_category_cd", 
                 "household_type_cd",
                 "migration_background_cd",
                 "dose_1_brand_cd",
                 "dose_2_brand_cd",
                 "dose_3_brand_cd"), as.factor)

df_clean <- df_clean %>%
  mutate_at(vars("exitus_bl", "hospi_due_to_covid_bl", "fully_vaccinated_bl"), as.logical)

df_clean <- df_clean %>%
  mutate_at(vars("dose_1_dt", "dose_2_dt", "dose_3_dt", "exitus_dt"), as.Date)
            

df_clean <- df_clean %>%
  mutate_at(vars("age_nm",
                 "test_nm",
                 "doses_nm",
                 "test_positive_to_covid_nm"), as.numeric)

summary(df_clean)

# Research questions (RQ)
columns_to_aggregate <- c("age_group",
                          "sex_cd",
                          "education_level_cd",
                          "income_category_cd",
                          "migration_background_cd",
                          "household_type_cd")


rq <- data.frame()

# RQ1 

# rq1_summary <- df_clean %>% 
#   group_by(education_level_cd, 
#            income_category_cd, 
#            household_type_cd, 
#            migration_background_cd) %>% 
#   summarize(mean_PCR_tests=round(mean(test_nm),1)) %>% 
#   mutate(RQ = "rq1_summary")

rq1_summary <- df_clean %>% 
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
                summarize(min = min(test_nm),
                          q1 = quantile(test_nm, 0.25),
                          median = median(test_nm),
                          mean = mean(test_nm),
                          q3 = quantile(test_nm, 0.75),
                          max = max(test_nm),
                          sd = sd(test_nm),
                          n = n()) %>% 
                  mutate(RQ = "rq1_summary")

rq <- bind_rows(rq, rq1_summary)

rq1_area <- df_clean %>% 
  group_by(residence_area_cd) %>% 
  summarize(min = min(test_nm),
            q1 = quantile(test_nm, 0.25),
            median = median(test_nm),
            mean = mean(test_nm),
            q3 = quantile(test_nm, 0.75),
            max = max(test_nm),
            sd = sd(test_nm),
            n = n()) %>% 
  mutate(RQ = "rq1_area")

rq <- bind_rows(rq, rq1_area)


for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    group_by_at(vars(col)) %>%
    summarize(min = min(test_nm),
              q1 = quantile(test_nm, 0.25),
              median = median(test_nm),
              mean = mean(test_nm),
              q3 = quantile(test_nm, 0.75),
              max = max(test_nm),
              sd = sd(test_nm),
              n = n())

  stats_df$RQ <- paste0("rq1_", col)
  
  rq <- bind_rows(rq, stats_df)
}

# Graph template from stat df
# ggplot(rq1, aes(education_level_cd)) +
#   geom_boxplot(
#     aes(ymin = min, lower = q1, middle = median, upper = q3, ymax = max),
#     stat = "identity")


#RQ2

# rq2_summary <- df_clean %>% 
#   group_by(education_level_cd, 
#            income_category_cd, 
#            household_type_cd, 
#            migration_background_cd) %>% 
#   summarize(mean_Positive_PCR_tests=round(mean(test_positive_to_covid_nm),1)) %>% 
#   mutate(RQ = "rq2_summary")


rq2_summary <- df_clean %>% 
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
              summarize(min = min(test_positive_to_covid_nm),
                        q1 = quantile(test_positive_to_covid_nm, 0.25),
                        median = median(test_positive_to_covid_nm),
                        mean = mean(test_positive_to_covid_nm),
                        q3 = quantile(test_positive_to_covid_nm, 0.75),
                        max = max(test_positive_to_covid_nm),
                        sd = sd(test_positive_to_covid_nm),
                        n = n()) %>% 
                  mutate(RQ = "rq2_summary")

rq <- bind_rows(rq, rq2_summary)

rq2_area <- df_clean %>% 
  group_by(residence_area_cd) %>% 
  summarize(min = min(test_positive_to_covid_nm),
            q1 = quantile(test_positive_to_covid_nm, 0.25),
            median = median(test_positive_to_covid_nm),
            mean = mean(test_positive_to_covid_nm),
            q3 = quantile(test_positive_to_covid_nm, 0.75),
            max = max(test_positive_to_covid_nm),
            sd = sd(test_positive_to_covid_nm),
            n = n()) %>% 
  mutate(RQ = "rq2_area")

rq <- bind_rows(rq, rq2_area)

for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    group_by_at(vars(col)) %>%
    summarize(min = min(test_positive_to_covid_nm),
              q1 = quantile(test_positive_to_covid_nm, 0.25),
              median = median(test_positive_to_covid_nm),
              mean = mean(test_positive_to_covid_nm),
              q3 = quantile(test_positive_to_covid_nm, 0.75),
              max = max(test_positive_to_covid_nm),
              sd = sd(test_positive_to_covid_nm),
              n = n())
  
  stats_df$RQ <- paste0("rq2_", col)
  
  rq <- bind_rows(rq, stats_df)
}


#rq3

rq3_summary <- df_clean %>% 
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
            summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
                      n = n()) %>%
              mutate(RQ = "rq3_summary")

# Extract value from data.frame linked to the summarize
rq3_summary$ratio <- rq3_summary$ratio[, 1]


rq <- bind_rows(rq, rq3_summary)

rq3_area <- df_clean %>% 
  group_by(residence_area_cd) %>% 
  summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
            n = n()) %>%
  mutate(RQ = "rq3_area")

rq3_area$ratio <- rq3_area$ratio[, 1]

rq <- bind_rows(rq, rq3_area)


for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    group_by_at(vars(col)) %>%
    summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
              n = n())
  
  stats_df$RQ <- paste0("rq3_", col)
  
  # Extract value from data.frame linked to the summarize
  stats_df$ratio <- stats_df$ratio[, 1]
  
  rq <- bind_rows(rq, stats_df)
}


#rq4a

rq4a_summary <- df_clean %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
  summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
            n = n()) %>%
  mutate(RQ = "rq4a_summary")

# Extract value from data.frame linked to the summarize
rq4a_summary$ratio <- rq4a_summary$ratio[, 1]


rq <- bind_rows(rq, rq4a_summary)

for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    group_by_at(vars(col, hospi_due_to_covid_bl)) %>%
    summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
              n = n())
  
  stats_df$RQ <- paste0("rq4a_", col)
  
  # Extract value from data.frame linked to the summarize
  stats_df$ratio <- stats_df$ratio[, 1]
  
  rq <- bind_rows(rq, stats_df)
}

rq4a_area <- df_clean %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(residence_area_cd) %>% 
  summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_nm,
            n = n()) %>%
  mutate(RQ = "rq4a_area")

rq4a_area$ratio <- rq4a_area$ratio[, 1]

rq <- bind_rows(rq, rq4a_area)





#rq4b

individuals_vaccinated_nm <- count(filter(df_clean, fully_vaccinated_bl == TRUE))

rq4b_summary <- df_clean %>%
  filter(fully_vaccinated_bl == TRUE, hospi_due_to_covid_bl == TRUE) %>%
  group_by(education_level_cd, 
           income_category_cd, 
           household_type_cd, 
           migration_background_cd) %>% 
  summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_vaccinated_nm,
            n = n()) %>%
  mutate(RQ = "rq4b_summary")

# Extract value from data.frame linked to the summarize
rq4b_summary$ratio <- rq4b_summary$ratio[, 1]


rq <- bind_rows(rq, rq4b_summary)


for (col in columns_to_aggregate) {
  stats_df <- filter(df_clean, fully_vaccinated_bl == TRUE) %>%
    group_by_at(vars(col, hospi_due_to_covid_bl)) %>%
    summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_vaccinated_nm,
              n = n())
  
  stats_df$RQ <- paste0("rq4b_", col)
  
  # Extract value from data.frame linked to the summarize
  stats_df$ratio <- stats_df$ratio[, 1]
  
  rq <- bind_rows(rq, stats_df)
}

rq4b_area <- df_clean %>% 
  filter(fully_vaccinated_bl == TRUE, hospi_due_to_covid_bl == TRUE) %>%
  group_by(residence_area_cd) %>% 
  summarise(ratio = sum(is.na(person_id) == FALSE) / individuals_vaccinated_nm,
            n = n()) %>%
  mutate(RQ = "rq4b_area")

rq4b_area$ratio <- rq4b_area$ratio[, 1]

rq <- bind_rows(rq, rq4b_area)



# Mettre la colonne RQ en 1er
rq <- rq[, c("RQ", setdiff(names(rq), "RQ"))]

# Ajouter la colonne du node
rq <- rq %>% mutate(country = as.factor(country))


# as.data.frame(sapply(rq,class))

# Attribute the data type to each variable
rq_final <- rq %>%
  mutate_at(vars("RQ", "age_group"), as.factor)

rq_final <- rq %>%
  mutate_at(vars("min", 
                 "q1", 
                 "median",
                 "mean",
                 "q3",
                 "max",
                 "ratio",
                 "sd",
                 "n"), as.numeric)


write.table(rq_final, 
            sprintf("EHDS2_pilot_UC1_data_%s.csv", country), 
            row.names=FALSE,
            col.names = TRUE,
            sep =" ", 
            fileEncoding = "UTF-8", 
            na = "NA")


# Test import

df_import <- read.csv(sprintf("EHDS2_pilot_UC1_data_%s.csv", country), 
                      header = TRUE, 
                      sep =" ", 
                      fileEncoding = "UTF-8", 
                      na = "NA")
