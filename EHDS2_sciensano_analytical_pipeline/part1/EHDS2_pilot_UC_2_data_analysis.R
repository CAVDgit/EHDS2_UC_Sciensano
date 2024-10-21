# Script that generates the aggregate results into one csv file with 
# statistics related to each research questions.
# script_version <- "1.9" 
# date of version : 24-10-24

library(tidyverse)
library(DT)

###################### INSTRUCTIONS #######################
# Modify the sections 1. to 3. then execute the script.

# 1. ######################################################
########## Enter the country CODE of your node ############
###########################################################
###########################################################

# Accepted value : BE, FR, FI, HU, HR, DK
country <- 'BE'

# 2. Place in a same folder, this script and your individual data
# And set the working directory to the location of this script.

# ## ######################################################
#################### PATH to DATA (CSV) ###################
###########################################################
# specify the file name of your individual data

file_path <- sprintf("EHDS2_pilot_UC1_mockup_data_%s.csv", country)

# 3. ######################################################
#################### Cut-off / Threshold ##################
##### To excluded aggregated data with number of ##########
##### individual inferior to this threshold ###############
###########################################################

# For example, threshold = 10 means that all groups with less then 10 individuals (10 excluded) 
# are excluded from the final aggregated output. 
# Excluded groups will be represented in the output with n = -1 and associated data will not be captured

threshold <- 10

# Do not modify any lines after this point. !!!

###########################################################
#######################  Cohort limit #####################
###########################################################

date_from <- as.Date('2021-01-01')  
date_to <- as.Date('2022-12-31')
age_max <- 115
age_min <- 18


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

# Function to remove a statistic if it based on a low number of individuals
threshold_median <- function(x, threshold) {
  med <- median(x)
  if (sum(x == med) < threshold) {
    print(paste(sum(x == med)))
    return(NA)
  } else {
    return(med)
  }
}

threshold_quartile <- function(x, prob, threshold) {
  q <- quantile(x, prob)
  if (sum(x == q) < threshold) {
    print(paste(sum(x == q)))
    return(NA)
  } else {
    return(q)
  }
}


# Count number of individuals
individuals_nm <- count(df_clean)
individuals_nm <- individuals_nm[1, 1, drop = TRUE]
individuals_vaccinated_nm <- count(filter(df_clean, fully_vaccinated_bl == TRUE))
individuals_vaccinated_nm <- individuals_vaccinated_nm[1, 1, drop = TRUE]

individuals_not_vaccinated_nm <- count(filter(df_clean, fully_vaccinated_bl == FALSE))
individuals_not_vaccinated_nm <- individuals_not_vaccinated_nm[1, 1, drop = TRUE]

# Function to round individuals_nm for privacy and redistribute the added value
# Ensuring at least threshold number individuals are added to either group
round_for_privacy <- function(individuals_nm, individuals_vaccinated_nm, individuals_not_vaccinated_nm) {
  
  # Calculate the initial ratios based on individuals_nm
  ratio_vaccinated <- individuals_vaccinated_nm / individuals_nm
  ratio_not_vaccinated <- individuals_not_vaccinated_nm / individuals_nm
  
  # Round individuals_nm to the nearest centaine
  rounded_individuals_nm <- ceiling(individuals_nm / 100) * 100
  
  # Check the difference
  difference <- rounded_individuals_nm - individuals_nm
  
  # If the difference is less than threshold number, increase to the next hundred
  if (difference < threshold) {
    rounded_individuals_nm <- rounded_individuals_nm + 100
    difference <- rounded_individuals_nm - individuals_nm
  }
  
  # Redistribute the added value according to the initial ratios
  added_to_vaccinated <- round(difference * ratio_vaccinated)
  added_to_not_vaccinated <- difference - added_to_vaccinated  # Ensures total added equals difference
  
  # Ensure at least 10 individuals are added to either group
  if (added_to_vaccinated < threshold) {
    added_to_vaccinated <- threshold
    added_to_not_vaccinated <- difference - added_to_vaccinated
  } else if (added_to_not_vaccinated < threshold) {
    added_to_not_vaccinated <- threshold
    added_to_vaccinated <- difference - added_to_not_vaccinated
  }
  
  # Adjust the individual counts
  individuals_vaccinated_nm <- individuals_vaccinated_nm + added_to_vaccinated
  individuals_not_vaccinated_nm <- individuals_not_vaccinated_nm + added_to_not_vaccinated
  
  return(list(
    rounded_individuals_nm = rounded_individuals_nm,
    individuals_vaccinated_nm = individuals_vaccinated_nm,
    individuals_not_vaccinated_nm = individuals_not_vaccinated_nm
  ))
}

result <- round_for_privacy(individuals_nm, individuals_vaccinated_nm, individuals_not_vaccinated_nm)

# Print the results
cat("Original individuals_nm:", individuals_nm, "\n")
cat("Original individuals_vaccinated_nm:", individuals_vaccinated_nm, "\n")
cat("Original individuals_not_vaccinated_nm:", individuals_not_vaccinated_nm, "\n")

# Print the results
cat("Rounded individuals_nm:", result$rounded_individuals_nm, "\n")
cat("Adjusted individuals_vaccinated_nm:", result$individuals_vaccinated_nm, "\n")
cat("Adjusted individuals_not_vaccinated_nm:", result$individuals_not_vaccinated_nm, "\n")


rq <- data.frame(
  RQ = "dataset_info",
  n = result$rounded_individuals_nm,
  range1 = result$individuals_vaccinated_nm,
  range2 = result$individuals_not_vaccinated_nm,
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
  mutate(n = ifelse(n > 0 & n < threshold, -1, n),
         ratio = ifelse(n == -1, NA, ratio))

# range1 == positive tests
df_pos_tests <- df_clean %>% 
  count(test_positive_to_covid_nm) %>% 
  mutate(ratio = n / sum(n), 
         RQ = "rq2_distribution", 
         range1 = test_positive_to_covid_nm) %>% 
  select(-test_positive_to_covid_nm) %>%
  mutate(n = ifelse(n > 0 & n < threshold, -1, n),
         ratio = ifelse(n == -1, NA, ratio))

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
  summarize(q1 = threshold_quartile(test_nm, 0.25, threshold),
            median = threshold_median(test_nm, threshold),
            mean = mean(test_nm),
            q3 = threshold_quartile(test_nm, 0.75, threshold),
            sd = sd(test_nm),
            n = n()) %>%
  mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
         upperfence = q3 + 1.5 * (q3 - q1),
         n = ifelse(n > 0 & n < threshold, -1, n),
         q1 = ifelse(n == -1, NA, q1),
         median = ifelse(n == -1, NA, median),
         mean = ifelse(n == -1, NA, mean),
         q3 = ifelse(n == -1, NA, q3),
         sd = ifelse(n == -1, NA, sd),
         lowerfence = ifelse(n == -1, NA, lowerfence),
         upperfence = ifelse(n == -1, NA, upperfence),
         RQ = "rq1_summary")

rq <- bind_rows(rq, rq1_summary)


# education, income, household, migration, area, sex
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), !is.na(test_nm)) %>%
    group_by_at(vars(col)) %>%
    summarize(q1 = threshold_quartile(test_nm, 0.25, threshold),
              median = threshold_median(test_nm, threshold),
              mean = mean(test_nm),
              q3 = threshold_quartile(test_nm, 0.75, threshold),
              sd = sd(test_nm),
              n = n()) %>%
    mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
           upperfence = q3 + 1.5 * (q3 - q1),
           n = ifelse(n > 0 & n < threshold, -1, n),
           q1 = ifelse(n == -1, NA, q1),
           median = ifelse(n == -1, NA, median),
           mean = ifelse(n == -1, NA, mean),
           q3 = ifelse(n == -1, NA, q3),
           sd = ifelse(n == -1, NA, sd),
           lowerfence = ifelse(n == -1, NA, lowerfence),
           upperfence = ifelse(n == -1, NA, upperfence))
  
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
    mutate(
      range1 = ifelse(range1 > 0 & range1 < threshold, -1, range1),
      range2 = ifelse(range2 > 0 & range2 < threshold, -1, range2),
      range3 = ifelse(range3 > 0 & range3 < threshold, -1, range3),
      n = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, ifelse(n > 0 & n < threshold, -1, n)),
      range1 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range1),
      range2 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range2),
      range3 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range3)
    )
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
  summarize(q1 = threshold_quartile(test_positive_to_covid_nm, 0.25, threshold),
            median = threshold_median(test_positive_to_covid_nm, threshold),
            mean = mean(test_positive_to_covid_nm),
            q3 = threshold_quartile(test_positive_to_covid_nm, 0.75, threshold),
            sd = sd(test_positive_to_covid_nm),
            n = n()) %>% 
  mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
         upperfence = q3 + 1.5 * (q3 - q1),
         n = ifelse(n > 0 & n < threshold, -1, n),
         q1 = ifelse(n == -1, NA, q1),
         median = ifelse(n == -1, NA, median),
         mean = ifelse(n == -1, NA, mean),
         q3 = ifelse(n == -1, NA, q3),
         sd = ifelse(n == -1, NA, sd),
         lowerfence = ifelse(n == -1, NA, lowerfence),
         upperfence = ifelse(n == -1, NA, upperfence),
         RQ = "rq2_summary")

rq <- bind_rows(rq, rq2_summary)


# education, income, household, migration, gender, sex
for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), 
           !is.na(test_positive_to_covid_nm)) %>%
    group_by_at(vars(col)) %>%
    summarize(q1 = threshold_quartile(test_positive_to_covid_nm, 0.25, threshold),
              median = threshold_median(test_positive_to_covid_nm, threshold),
              mean = mean(test_positive_to_covid_nm),
              q3 = threshold_quartile(test_positive_to_covid_nm, 0.75, threshold),
              sd = sd(test_positive_to_covid_nm),
              n = n()) %>%
    mutate(lowerfence = pmax(0, q1 - 1.5 * (q3 - q1)),
           upperfence = q3 + 1.5 * (q3 - q1),
           n = ifelse(n > 0 & n < threshold, -1, n),
           q1 = ifelse(n == -1, NA, q1),
           median = ifelse(n == -1, NA, median),
           mean = ifelse(n == -1, NA, mean),
           q3 = ifelse(n == -1, NA, q3),
           sd = ifelse(n == -1, NA, sd),
           lowerfence = ifelse(n == -1, NA, lowerfence),
           upperfence = ifelse(n == -1, NA, upperfence))
  
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
    mutate(
      range1 = ifelse(range1 > 0 & range1 < threshold, -1, range1),
      range2 = ifelse(range2 > 0 & range2 < threshold, -1, range2),
      range3 = ifelse(range3 > 0 & range3 < threshold, -1, range3),
      n = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, ifelse(n > 0 & n < threshold, -1, n)),
      range1 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range1),
      range2 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range2),
      range3 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range3)
    )
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
         n = ifelse(n > 0 & n < threshold, -1, n))

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
         n = ifelse(n > 0 & n < threshold, -1, n),
         range1 = ifelse(range1 > 0 & range1 < threshold, -1, range1))


rq <- bind_rows(rq, rq3_summary)


# distribution of vaccine doses
df_doses <- df_clean %>% 
  count(doses_nm) %>% 
  mutate(ratio = n / sum(n), 
         RQ = "rq3_distribution", 
         range1 = doses_nm) %>% 
  select(-doses_nm) %>%
  mutate(n = ifelse(n > 0 & n < threshold, -1, n),
         ratio = ifelse(n == -1, NA, ratio))

rq <- bind_rows(rq, df_doses)


for (col in columns_to_aggregate) {
  stats_df <- df_clean %>%
    filter(!is.na(get(col)), 
           !is.na(fully_vaccinated_bl)) %>%
    group_by_at(vars(col, 
                     fully_vaccinated_bl)) %>%
    summarise(n = n()) %>%
    mutate(n = ifelse(n > 0 & n < threshold, -1, n))
  
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
    mutate(
      range1 = ifelse(range1 > 0 & range1 < threshold, -1, range1),
      range2 = ifelse(range2 > 0 & range2 < threshold, -1, range2),
      range3 = ifelse(range3 > 0 & range3 < threshold, -1, range3),
      n = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, ifelse(n > 0 & n < threshold, -1, n)),
      range1 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range1),
      range2 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range2),
      range3 = ifelse(range1 == -1 | range2 == -1 | range3 == -1, -1, range3)
    )
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
         n = ifelse(n > 0 & n < threshold, -1, n))

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
    mutate(n = ifelse(n > 0 & n < threshold, -1, n))
  
  stats_df$RQ <- paste0("rq4_", col)
  
  rq <- bind_rows(rq, stats_df)
}



# Mettre la colonne RQ en 1er
rq <- rq[, c("RQ", setdiff(names(rq), "RQ"))]

# Ajouter la colonne du node
rq <- rq %>% mutate(country = as.factor(country))



# NEW V1.6

# Calculate total cohort summary without grouping by sex
total_summary <- df_clean %>%
  summarise(
    count = result$rounded_individuals_nm,
    
    # Age statistics (set -1 if count < threshold)
    avg_age = ifelse(count >= threshold, round(mean(age_nm, na.rm = TRUE), 2), -1),
    median_age = ifelse(count >= threshold, round(median(age_nm, na.rm = TRUE), 2), -1),
    sd_age = ifelse(count >= threshold, round(sd(age_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Age
    iqr_age = ifelse(count >= threshold, 
                     paste0(round(quantile(age_nm, 0.25, na.rm = TRUE), 2), "-", 
                            round(quantile(age_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Age
    
    # Test statistics (set -1 if individuals_with_tests < threshold)
    individuals_with_tests = sum(test_nm > 0, na.rm = TRUE),
    avg_test_nm = ifelse(individuals_with_tests >= threshold, round(mean(test_nm, na.rm = TRUE), 2), -1),
    median_test_nm = ifelse(individuals_with_tests >= threshold, round(median(test_nm, na.rm = TRUE), 2), -1),
    sd_test_nm = ifelse(individuals_with_tests >= threshold, round(sd(test_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Tests
    iqr_test_nm = ifelse(individuals_with_tests >= threshold, 
                         paste0(round(quantile(test_nm, 0.25, na.rm = TRUE), 2), "-", 
                                round(quantile(test_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Tests
    individuals_with_tests = ifelse(individuals_with_tests >= threshold, individuals_with_tests, -1),
    
    
    # Vaccine statistics (set -1 if individuals_with_vaccine < threshold)
    individuals_with_vaccine = sum(doses_nm > 0, na.rm = TRUE),
    avg_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(mean(doses_nm, na.rm = TRUE), 2), -1),
    median_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(median(doses_nm, na.rm = TRUE), 2), -1),
    sd_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(sd(doses_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Doses
    iqr_dose_nm = ifelse(individuals_with_vaccine >= threshold, 
                         paste0(round(quantile(doses_nm, 0.25, na.rm = TRUE), 2), "-", 
                                round(quantile(doses_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Doses
    individuals_with_vaccine = ifelse(individuals_with_vaccine >= threshold, individuals_with_vaccine, -1),
    
    
    # Vaccination rate (set -1 if fully_vaccinated_count < threshold)
    fully_vaccinated_count = result$individuals_vaccinated_nm,
    vaccination_rate = ifelse(fully_vaccinated_count >= threshold, 
                              round(fully_vaccinated_count / count * 100, 2), -1),
    fully_vaccinated_count = ifelse(fully_vaccinated_count >= threshold, fully_vaccinated_count, -1),
    
    # Hospitalisation rate (set -1 if hospitalised_true < threshold)
    hospitalised_true = sum(hospi_due_to_covid_bl == TRUE, na.rm = TRUE),
    hospitalisation_rate = ifelse(hospitalised_true >= threshold, round(hospitalised_true / count * 100, 3), -1),
    hospitalised_true = ifelse(hospitalised_true >= threshold, hospitalised_true, -1)

  )

# Collapse the entire row into a single string with ";" separating the values
total_summary_collapsed <- paste(total_summary, collapse = ";")

# Create a new dataframe with the desired format
dataset_info_all <- data.frame(
  RQ = "dataset_info_all",
  country = total_summary_collapsed
)

rq <- bind_rows(rq, dataset_info_all)



# Function to calculate summary statistics for any column, excluding NAs
create_group_summary <- function(df_clean, group_col, group_value, rq_label) {
  # Filter the dataframe by the group value, excluding NA
  filtered_df <- df_clean %>%
    filter(.data[[group_col]] == group_value, !is.na(.data[[group_col]]))
  
  # Check if the count is below the threshold, if so, return NULL
  count_value <- nrow(filtered_df)
  if (count_value < threshold) {
    return(NULL)  # Skip if count is below the threshold
  }
  
  group_summary <- filtered_df %>%
    summarise(
      count = count_value,
      
      # Age statistics (set -1 if count < threshold)
      avg_age = ifelse(count >= threshold, round(mean(age_nm, na.rm = TRUE), 2), -1),
      median_age = ifelse(count >= threshold, round(median(age_nm, na.rm = TRUE), 2), -1),
      sd_age = ifelse(count >= threshold, round(sd(age_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Age
      iqr_age = ifelse(count >= threshold, 
                       paste0(round(quantile(age_nm, 0.25, na.rm = TRUE), 2), "-", 
                              round(quantile(age_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Age
      
      # Test statistics (set -1 if individuals_with_tests < threshold)
      individuals_with_tests = sum(test_nm > 0, na.rm = TRUE),
      avg_test_nm = ifelse(individuals_with_tests >= threshold, round(mean(test_nm, na.rm = TRUE), 2), -1),
      median_test_nm = ifelse(individuals_with_tests >= threshold, round(median(test_nm, na.rm = TRUE), 2), -1),
      sd_test_nm = ifelse(individuals_with_tests >= threshold, round(sd(test_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Tests
      iqr_test_nm = ifelse(individuals_with_tests >= threshold, 
                           paste0(round(quantile(test_nm, 0.25, na.rm = TRUE), 2), "-", 
                                  round(quantile(test_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Tests
      individuals_with_tests = ifelse(individuals_with_tests >= threshold, individuals_with_tests, -1),
      
      
      # Vaccine statistics (set -1 if individuals_with_vaccine < threshold)
      individuals_with_vaccine = sum(doses_nm > 0, na.rm = TRUE),
      avg_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(mean(doses_nm, na.rm = TRUE), 2), -1),
      median_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(median(doses_nm, na.rm = TRUE), 2), -1),
      sd_dose_nm = ifelse(individuals_with_vaccine >= threshold, round(sd(doses_nm, na.rm = TRUE), 2), -1),  # Standard Deviation for Doses
      iqr_dose_nm = ifelse(individuals_with_vaccine >= threshold, 
                           paste0(round(quantile(doses_nm, 0.25, na.rm = TRUE), 2), "-", 
                                  round(quantile(doses_nm, 0.75, na.rm = TRUE), 2)), -1),  # IQR for Doses
      individuals_with_vaccine = ifelse(individuals_with_vaccine >= threshold, individuals_with_vaccine, -1),
      
      
      # Vaccination rate (set -1 if fully_vaccinated_count < threshold)
      fully_vaccinated_count = sum(fully_vaccinated_bl == TRUE, na.rm = TRUE),
      vaccination_rate = ifelse(fully_vaccinated_count >= threshold, 
                                round(fully_vaccinated_count / count * 100, 2), -1),
      fully_vaccinated_count = ifelse(fully_vaccinated_count >= threshold, fully_vaccinated_count, -1),
      
      # Hospitalisation rate (set -1 if hospitalised_true < threshold)
      hospitalised_true = sum(hospi_due_to_covid_bl == TRUE, na.rm = TRUE),
      hospitalisation_rate = ifelse(hospitalised_true >= threshold, round(hospitalised_true / count * 100, 3), -1),
      hospitalised_true = ifelse(hospitalised_true >= threshold, hospitalised_true, -1)
    )
  
  
  # Collapse the entire row into a single string with ";" separating the values
  group_summary_collapsed <- paste(group_summary, collapse = ";")
  
  # Create the new dataframe
  data.frame(
    RQ = rq_label,
    country = group_summary_collapsed
  )
}

# Function to create summaries for multiple columns, excluding specific columns and low counts
create_summaries_for_columns <- function(df_clean, columns_to_aggregate) {
  results <- list()  # To store all summary dataframes
  
  for (col in columns_to_aggregate) {
    # Skip specific columns
    if (col %in% c("residence_area_lau_cd", "residence_area_cd")) {
      next  # Skip these columns
    }
    
    unique_values <- unique(df_clean[[col]])  # Get unique values in the column
    
    # For each unique value in the column, calculate the summary
    for (value in unique_values) {
      if (!is.na(value)) {  # Exclude NA values
        rq_label <- paste("dataset_info", col, value, sep = "_")  # Create a unique label
        summary_df <- create_group_summary(df_clean, col, value, rq_label)
        
        # If the summary_df is not NULL (i.e., count >= threshold), add it to the results
        if (!is.null(summary_df)) {
          results[[rq_label]] <- summary_df  # Store the result in the list
        }
      }
    }
  }
  
  # Combine all summaries into a single dataframe
  final_df <- do.call(bind_rows, results)
  return(final_df)
}
# Generate summaries for each column
final_summaries_df <- create_summaries_for_columns(df_clean, columns_to_aggregate)

rq <- bind_rows(rq, final_summaries_df)



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
