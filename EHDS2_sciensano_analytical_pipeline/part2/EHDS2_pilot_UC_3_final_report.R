
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(rlang)
library(eurostat)

country <- 'BE'

# read the file
filename <- sprintf("EHDS2_pilot_UC1_data_%s.csv", country)
df <- read.csv(filename, header = TRUE, 
               sep =" ", 
               fileEncoding = "UTF-8", 
               na = "NA")


var_stat <- c("lowerfence", "q1", "mean", "median", "q3", "upperfence", "sd", "n")

#xlabel order
plotOrderEducation <- c('Low', 'Middle', 'High')
plotOrderIncome <- c('Low', 'Middle', 'High')
plotOrderMigration <- c('NATIVE', 'EU', 'NON-EU')
plotOrderHousehold <- c("ALONE", "COUPLE", "COUPLE_CHILD", "LONE", "EXTENDED", "OTHER")
plotOrderAge <- c("18-25", "25-35", "35-45", "45-55", "55-65", "65-75", "75-85", "85-95", "95-105", "105-115")
plotOrderSex <- c("F", "M")
# template for graphs


eurostat <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = "all",
  year = "2021",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  crs = "4326",
  make_valid = FALSE
) %>% filter(CNTR_CODE == country)

write.csv(eurostat, "eurostat.csv", row.names = FALSE)



boxplot_custom <- function(data, 
                           x_var, 
                           x_label, 
                           y_label, 
                           plot_title, 
                           plot_title_font = list(family = "Arial", size = 20, color = "#333333"),
                           x_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           y_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_position = "bottom",
                           x_axis_order = NULL) {
  plot <- plot_ly(data, 
                  type = 'box', 
                  x = ~get(x_var), 
                  color = ~get(x_var),
                  lowerfence = ~lowerfence, 
                  q1 = ~q1, 
                  mean = ~mean,
                  median = ~median, 
                  q3 = ~q3, 
                  upperfence = ~upperfence) %>%
    layout(title = list(text = plot_title, font = plot_title_font),
           xaxis = list(title = x_label, titlefont = x_label_font, 
                        type = "category", 
                        categoryorder = "array", 
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, titlefont = y_label_font),
           legend = list(font = legend_font, orientation = legend_position),
           plot_bgcolor = 'rgba(233, 233, 233, 0.3)') # Light background for plot area
  
  return(plot)
}


boxplot_custom_group <- function(data, 
                           x_var, 
                           x_label, 
                           y_label, 
                           plot_title, 
                           plot_title_font = list(family = "Arial", size = 20, color = "#333333"),
                           x_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           y_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_position = "bottom",
                           x_axis_order = NULL) {
  plot <- plot_ly(data, 
                  type = 'box', 
                  x = ~get(x_var), 
                  color = ~data[['type']],
                  lowerfence = ~lowerfence, 
                  q1 = ~q1, 
                  mean = ~mean,
                  median = ~median, 
                  q3 = ~q3, 
                  upperfence = ~upperfence) %>%
    layout(title = list(text = plot_title, font = plot_title_font),
           xaxis = list(title = x_label, titlefont = x_label_font, 
                        type = "category", 
                        categoryorder = "array", 
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, titlefont = y_label_font),
           legend = list(font = legend_font, orientation = legend_position),
           plot_bgcolor = 'rgba(233, 233, 233, 0.3)',
           boxmode = "group") # Light background for plot area
  
  return(plot)
}




barplot_custom <- function(data,
                           x_var,
                           y_var,
                           x_label,
                           y_label,
                           plot_title,
                           plot_title_font = list(family = "Arial", size = 20, color = "#333333"),
                           x_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           y_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_position = "bottom",
                           x_axis_order = NULL) {
  plot <- plot_ly(data,
                  type = 'bar',
                  x = ~get(x_var),
                  color = ~get(x_var),
                  y = ~get(y_var)) %>%
    layout(title = list(text = plot_title, font = plot_title_font),
           xaxis = list(title = x_label, titlefont = x_label_font,
                        type = "category",
                        categoryorder = "array",
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, titlefont = y_label_font),
           legend = list(font = legend_font, orientation = legend_position),
           plot_bgcolor = 'rgba(233, 233, 233, 0.3)') # Light background for plot area

  return(plot)
}



barplot_stacked_custom <- function(data, x_var, y_var, x_label, y_label, plot_title, legend_title, x_axis_order = NULL, y_var2 = NULL) {
  
  # Validate y_var structure
  if (!all(sapply(y_var, function(x) length(x) == 2))) {
    stop("Each element in 'y_var' must be a list of two elements: the column name and its legend name")
  }
  
  fig <- plot_ly(data = data)
  
  # Dynamically add traces for each set of y values and names
  for (y_info in y_var) {
    y_col <- y_info[[1]]  # The column name for y values
    y_name <- y_info[[2]]  # The name for the legend
    
    # Calculate labels directly within add_trace
    # Adjusted to create a separate dataframe slice for dynamic label calculation
    temp_data <- data %>%
      mutate(text_label = paste0(round(!!sym(y_col) * 100, 1), "%")) %>%
      select(!!sym(x_var), !!sym(y_col), text_label)
    
    fig <- fig %>%
      add_trace(data = temp_data,
                x = ~get(x_var), y = as.formula(paste0("~", y_col)), type = 'bar', name = y_name,
                text = ~text_label, hoverinfo = 'text+x+y')
  }
  
  # Optionally add a line trace
  if (!is.null(y_var2) && length(y_var2) == 2) {
    y2_col <- y_var2[[1]]
    y2_name <- y_var2[[2]]
    
    # More explicit environment handling for NSE
    fig <- fig %>%
      add_trace(x = ~get(x_var), y = as.formula(paste0("~", eval(sym(y2_col)))), type = 'scatter', mode = 'lines',
                name = y2_name, line = list(width = 3))
  }
  
  fig <- fig %>%
    layout(barmode = 'stack',
           xaxis = list(title = x_label,
                        type = "category",
                        categoryorder = "array",
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, tickformat = ',.0%'),
           title = plot_title,
           legend = list(title = list(text = legend_title)),
           hovermode = 'closest')
  
  return(fig)
}



dataset_info <- df %>% filter(RQ == "dataset_info") %>% select(n, range1, range2)
individuals_nm <- dataset_info[1, 1, drop = TRUE]
individuals_vaccinated_nm <- dataset_info[1, 2, drop = TRUE]
individuals_not_vaccinated_nm <- dataset_info[1, 3, drop = TRUE]





rq1_distribution <- df %>%
  filter(RQ == "rq1_distribution") %>%
  select(range1, range2, ratio, n)



rq1_summary <- df %>% 
  filter(RQ == "rq1_summary" & 
           !is.na(education_level_cd) &
           !is.na(income_category_cd) &
           !is.na(household_type_cd) &
           !is.na(migration_background_cd)) %>% 
  select(education_level_cd, 
         income_category_cd, 
         household_type_cd, 
         migration_background_cd,
         median,
         sd,
         n)

rq2_summary <- df %>% 
  filter(RQ == "rq2_summary" & 
           !is.na(education_level_cd) &
           !is.na(income_category_cd) &
           !is.na(household_type_cd) &
           !is.na(migration_background_cd)) %>% 
  mutate(median_positive = median,
         mean_positive = mean,
         sd_positive = sd,
         n_positive = n) %>%
  select(education_level_cd, 
         income_category_cd, 
         household_type_cd, 
         migration_background_cd,
         median_positive,
         mean_positive,
         sd_positive,
         n_positive)


rq1_2_summary <- full_join(rq1_summary, rq2_summary)


rq1_2_distribution <- df %>% 
  filter(RQ %in% c("rq1_distribution", "rq2_distribution") & 
           !is.na(ratio)) %>% 
  mutate(tests = ifelse(RQ == "rq1_distribution", 'All', 'Positives'),
         test_number = range1) %>%
  select(tests, test_number, n, ratio) %>% 
  group_by(tests)


rq1_2_educ_aggreg <- df %>% 
  filter(RQ %in% c("rq1_education_level_cd", "rq2_education_level_cd") & 
           !is.na(education_level_cd)) %>% 
  mutate(type = ifelse(RQ == "rq1_education_level_cd", 'All', 'Positives')) %>%
  select(type, education_level_cd, var_stat) %>% 
  group_by(education_level_cd, type)


rq1_2_income_aggreg <- df %>%
  filter(RQ %in% c("rq1_income_category_cd", "rq2_income_category_cd") & 
           !is.na(income_category_cd)) %>%
  mutate(type = ifelse(RQ == "rq1_income_category_cd", 'All', 'Positives')) %>%
  select(type, income_category_cd, var_stat) %>%
  group_by(income_category_cd, type)

rq1_2_migration_aggreg <- df %>%
  filter(RQ %in% c("rq1_migration_background_cd", "rq2_migration_background_cd") & 
           !is.na(migration_background_cd)) %>%
  mutate(type = ifelse(RQ == "rq1_migration_background_cd", 'All', 'Positives')) %>%
  select(type, migration_background_cd, var_stat) %>%
  group_by(migration_background_cd, type)

rq1_2_household_aggreg <- df %>%
  filter(RQ %in% c("rq1_household_type_cd", "rq2_household_type_cd") & 
           !is.na(household_type_cd)) %>%
  mutate(type = ifelse(RQ == "rq1_household_type_cd", 'All', 'Positives')) %>%
  select(type, household_type_cd, var_stat) %>%
  group_by(household_type_cd, type)

rq1_2_age_group <- df %>%
  filter(RQ %in% c("rq1_age_cd", "rq2_age_cd") & 
           !is.na(age_cd)) %>%
  mutate(type = ifelse(RQ == "rq1_age_cd", 'All', 'Positives')) %>%
  select(type, age_cd, var_stat) %>%
  group_by(age_cd, type)

rq1_2_gender <- df %>%
  filter(RQ %in% c("rq1_sex_cd", "rq2_sex_cd") & sex_cd %in% c("M", "F") & 
           !is.na(sex_cd)) %>%
  mutate(type = ifelse(RQ == "rq1_sex_cd", 'All', 'Positives')) %>%
  select(type, sex_cd, var_stat) %>%
  group_by(sex_cd, type)

rq1_2_area <- df %>%
  filter(RQ %in% c("rq1_residence_area_nuts2_cd", 
                   "rq1_residence_area_cd", 
                   "rq1_residence_area_lau_cd",
                   "rq2_residence_area_nuts2_cd", 
                   "rq2_residence_area_cd", 
                   "rq2_residence_area_lau_cd")) %>%
  mutate(type = ifelse(RQ %in% c("rq1_residence_area_nuts2_cd", 
                                 "rq1_residence_area_cd", 
                                 "rq1_residence_area_lau_cd"), 'All', 'Positives')) %>%
  select(type, residence_area_nuts2_cd, residence_area_cd, residence_area_lau_cd, var_stat)


library(sf)
library(leaflet)
library(dplyr)


eurostat <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = "all",
  year = "2021",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  crs = "4326",
  make_valid = FALSE
) %>% filter(CNTR_CODE == country)

rq1_2_eurostat <- data.frame()  # Initialize as empty data frame

# Loop through each type in rq1_2_area$type
for (i in unique(rq1_2_area$type)) {
  # Create a temporary data frame with the type appended to all rows
  temp <- eurostat %>% 
    mutate(type = i)
  
  # If rq1_2_eurostat is empty, initialize it with temp
  if (nrow(rq1_2_eurostat) == 0) {
    rq1_2_eurostat <- temp
  } else {
    # Otherwise, bind the rows of temp to rq1_2_eurostat
    rq1_2_eurostat <- bind_rows(rq1_2_eurostat, temp)
  }
}


rq1_2_eurostat <- data.frame()

# Use lapply to create a list of data frames with each type from rq1_2_area$type
temp_list <- lapply(unique(rq1_2_area$type), function(i) {
  mutate(eurostat, type = i)
})

rq1_2_eurostat <- bind_rows(temp_list)

rq1_2_eurostat <- rq1_2_eurostat %>% filter(LEVL_CODE == 3) %>% select(NUTS_ID)

print(rq1_2_eurostat$NUTS_ID)

rq1_2_area <- rq1_2_eurostat %>% 
  left_join(rq1_2_area, join_by(NUTS_ID == residence_area_cd, type == type)) %>%
  filter(LEVL_CODE == 3)

rq1_2_area_map1 <- rq1_2_eurostat %>% 
  left_join(rq1_2_area, join_by(NUTS_ID == residence_area_cd, type == type)) %>%
  select(LEVL_CODE, NUTS_ID, NUTS_NAME, type, mean, geometry) %>%
  filter(LEVL_CODE == 3)

# country_boundary <- eurostat %>%
#   filter(LEVL_CODE == 0, CNTR_CODE == country) %>%
#   select(LEVL_CODE, NUTS_ID, NUTS_NAME, geometry)

custom_palette <- colorNumeric(palette = "viridis", domain = rq1_2_area$mean, na.color = "lightgrey")

custom_label <- paste(
  "ID: ", rq1_2_area$NUTS_ID, "<br/>",
  "Name: ", rq1_2_area$NUTS_NAME, "<br/>",
  "n: ", rq1_2_area$n, "<br/>",
  "Mean: ", round(rq1_2_area$mean, 2),
  "±", round(rq1_2_area$sd, 2), "<br/>",
  "Median: ", round(rq1_2_area$median, 2), "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Create the map
map <- leaflet(rq1_2_area) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = rq1_2_area %>% filter(type == 'All'), 
              fillColor = ~custom_palette(mean), 
              fillOpacity = 0.7,
              weight = 1, 
              opacity = 1, 
              dashArray = "5, 5", 
              color = "white", 
              smoothFactor = 0.5,
              label = custom_label,
              group = "All Tests") %>%
  addPolygons(data = rq1_2_area %>% filter(type == 'Positives'), 
              fillColor = ~custom_palette(mean), 
              fillOpacity = 0.7,
              weight = 1, 
              opacity = 1, 
              dashArray = "5, 5", 
              color = "white",
              smoothFactor = 0.5,
              label = custom_label,
              group = "Positives Tests") %>%
  addLegend(
    position = "bottomright",
    pal = color_palette, 
    values = ~mean[!is.na(mean)],
    title = "Mean Tests",
    labFormat = labelFormat(),
    opacity = 1
  ) %>%
  addLayersControl(baseGroups = c("All Tests", "Positives Tests"),
                   options = layersControlOptions(collapsed = FALSE))

map





create_choropleth_map <- function(dataRQ) {
  # Define the color palette
  custom_palette <- colorNumeric(palette = "viridis", domain = dataRQ$mean, na.color = "lightgrey")
  
  # Create the map
  map <- leaflet(dataRQ) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = dataRQ %>% filter(type == 'All'), 
                fillColor = ~custom_palette(mean), 
                fillOpacity = 0.7,
                weight = 1, 
                opacity = 1, 
                dashArray = "5, 5", 
                color = "white", 
                smoothFactor = 0.5,
                label = ~paste(
                  "ID: ", NUTS_ID, "<br/>",
                  "Name: ", NUTS_NAME, "<br/>",
                  "n: ", n, "<br/>",
                  "Mean: ", round(mean, 2),
                  "±", round(sd, 2), "<br/>",
                  "Median: ", round(median, 2), "<br/>"
                ),
                group = "All Tests") %>%
    addPolygons(data = dataRQ %>% filter(type == 'Positives'), 
                fillColor = ~custom_palette(mean), 
                fillOpacity = 0.7,
                weight = 1, 
                opacity = 1, 
                dashArray = "5, 5", 
                color = "white",
                smoothFactor = 0.5,
                label = ~paste(
                  "ID: ", NUTS_ID, "<br/>",
                  "Name: ", NUTS_NAME, "<br/>",
                  "n: ", n, "<br/>",
                  "Mean: ", round(mean, 2),
                  "±", round(sd, 2), "<br/>",
                  "Median: ", round(median, 2), "<br/>"
                ),
                group = "Positives Tests") %>%
    addLegend(
      position = "bottomright",
      pal = custom_palette, 
      values = ~dataRQ$mean[!is.na(dataRQ$mean)],
      title = "Mean Tests",
      labFormat = labelFormat(),
      opacity = 1
    ) %>%
    addLayersControl(baseGroups = c("All Tests", "Positives Tests"),
                     options = layersControlOptions(collapsed = FALSE))
  
  # Return the map
  return(map)
}


map <- create_choropleth_map(rq1_2_area)
map












rq1_2_educ_tests <- df %>% 
  filter(RQ %in% c("rq1_education_level_cd_test", "rq2_education_level_cd_test") & 
          !is.na(education_level_cd)) %>%
  select(education_level_cd, range1, range2, range3, n) %>% 
  group_by(education_level_cd)

rq1_2_income_tests <- df %>% 
  filter(RQ %in% c("rq1_income_category_cd_test", "rq2_income_category_cd_test") & 
           !is.na(income_category_cd)) %>%
  select(income_category_cd, range1, range2, range3, n) %>% 
  group_by(income_category_cd)

rq1_2_household_tests <- df %>% 
  filter(RQ %in% c("rq1_household_type_cd_test", "rq2_household_type_cd_test") & 
           !is.na(household_type_cd)) %>%
  select(household_type_cd, range1, range2, range3, n) %>% 
  group_by(household_type_cd)

rq1_2_migration_tests <- df %>% 
  filter(RQ %in% c("rq1_migration_background_cd_test", "rq2_migration_background_cd_test") & 
           !is.na(migration_background_cd)) %>%
  select(migration_background_cd, range1, range2, range3, n) %>% 
  group_by(migration_background_cd)

rq1_2_age_group_tests <- df %>% 
  filter(RQ %in% c("rq1_age_cd_test", "rq2_age_cd_test") & 
           !is.na(age_cd)) %>%
  select(age_cd, range1, range2, range3, n) %>% 
  group_by(age_cd)

rq1_2_gender_tests <- df %>% 
  filter(RQ %in% c("rq1_sex_cd_test", "rq2_sex_cd_test") & 
           sex_cd %in% c("M", "F") & 
           !is.na(sex_cd)) %>%
  select(sex_cd, range1, range2, range3, n) %>% 
  group_by(sex_cd)




rq3_summary <- df %>% 
  filter(RQ == "rq3_summary") %>% 
  select(education_level_cd, 
         income_category_cd, 
         household_type_cd, 
         migration_background_cd,
         ratio,
         ratio2,
         n)

rq3_distribution <- df %>% 
  filter(RQ %in% c("rq3_distribution") & 
           !is.na(ratio)) %>% 
  mutate(dose_number = range1,
         percentage = ratio*100) %>%
  select(dose_number, n, ratio, percentage)



rq3_educ_aggreg <- df %>% 
  filter(RQ == "rq3_education_level_cd") %>% 
  select(education_level_cd, ratio, ratio2, n) %>% 
  mutate(ratio3 = 1-ratio)




rq3_income_aggreg <- df %>% 
  filter(RQ == "rq3_income_category_cd") %>% 
  select(income_category_cd, fully_vaccinated_bl, ratio, n) %>% 
  group_by(income_category_cd)

rq3_migration_aggreg <- df %>% 
  filter(RQ == "rq3_migration_background_cd") %>% 
  select(migration_background_cd, fully_vaccinated_bl, ratio, n) %>% 
  group_by(migration_background_cd)

rq3_household_aggreg <- df %>% 
  filter(RQ == "rq3_household_type_cd") %>% 
  select(household_type_cd, fully_vaccinated_bl, ratio, n) %>% 
  group_by(household_type_cd)

rq3_age_group <- df %>% 
  filter(RQ == "rq3_age_cd") %>% 
  select(age_cd, fully_vaccinated_bl, ratio, n) %>% 
  group_by(age_cd)

rq3_gender <- df %>% 
  filter(RQ == "rq3_sex_cd" & sex_cd %in% c("M", "F")) %>% 
  select(sex_cd, fully_vaccinated_bl, ratio, n) %>% 
  group_by(sex_cd)

rq3_area <- df %>% 
  filter(RQ == "rq3_area") %>% 
  select(residence_area_cd,
         fully_vaccinated_bl,
         ratio,
         n)



rq3_area <- df %>%
  filter(RQ %in% c("rq3_residence_area_nuts2_cd", 
                   "rq3_residence_area_cd", 
                   "rq3_residence_area_lau_cd")) %>%
  select(residence_area_nuts2_cd, residence_area_cd, residence_area_lau_cd, ratio, ratio2, n)

rq3_area_overall <- rq3_area %>% mutate(type = 'overall') %>% select(-ratio2)
rq3_area_subgroup <- rq3_area %>% mutate(type = 'subgroup', ratio = ratio2) %>% select(-ratio2)

rq3_area <- bind_rows(rq3_area_overall, rq3_area_subgroup)


rq3_area_map1 <- eurostat %>% 
  left_join(rq3_area_final, join_by(NUTS_ID == residence_area_nuts2_cd)) %>%
  filter(LEVL_CODE == 2)







rq3_educ_doses <- df %>% 
  filter(RQ == "rq3_education_level_cd_doses") %>% 
  select(education_level_cd, range1, range2, range3, n) %>% 
  group_by(education_level_cd)

rq3_income_doses <- df %>% 
  filter(RQ == "rq3_income_category_cd_doses") %>% 
  select(income_category_cd, range1, range2, range3, n) %>% 
  group_by(income_category_cd)

rq3_household_doses <- df %>% 
  filter(RQ == "rq3_household_type_cd_doses") %>% 
  select(household_type_cd, range1, range2, range3, n) %>% 
  group_by(household_type_cd)

rq3_migration_doses <- df %>% 
  filter(RQ == "rq3_migration_background_cd_doses") %>% 
  select(migration_background_cd, range1, range2, range3, n) %>% 
  group_by(migration_background_cd)

rq3_age_group_doses <- df %>% 
  filter(RQ == "rq3_age_cd_doses") %>% 
  select(age_cd, range1, range2, range3, n) %>% 
  group_by(age_cd)

rq3_gender_doses <- df %>% 
  filter(RQ == "rq3_sex_cd_doses" & sex_cd %in% c("M", "F")) %>% 
  select(sex_cd, range1, range2, range3, n) %>% 
  group_by(sex_cd)





variables <- c("age_cd", 
               "sex_cd", 
               "education_level_cd", 
               "income_category_cd", 
               "migration_background_cd", 
               "household_type_cd")

# Loop over each variable and perform the operations
for (var in variables) {
  # Construct the dynamic variable names and filter conditions
  rq_variable <- paste0("rq4_", var, "_aggreg")
  RQ_filter <- paste0("rq4_", var)
  
  # Perform the data manipulations
  assign(rq_variable, df %>%
           filter(RQ == RQ_filter) %>%
           group_by(!!sym(var), hospi_due_to_covid_bl, fully_vaccinated_bl) %>%
           summarise(n = sum(n), .groups = 'drop'))
  
  group <- df %>%
    filter(RQ == RQ_filter) %>%
    group_by(!!sym(var)) %>%
    summarise(nGroup = sum(n), .groups = 'drop')
  
  # Join back to the main data
  aggreg_data <- left_join(get(rq_variable), group, by = var)
  
  groupVacc <- df %>%
    filter(RQ == RQ_filter, fully_vaccinated_bl == TRUE) %>%
    group_by(!!sym(var), fully_vaccinated_bl) %>%
    summarise(nGroupVacc = sum(n), .groups = 'drop')
  
  aggreg_data <- left_join(aggreg_data, groupVacc, by = var)
  
  groupUnVacc <- df %>%
    filter(RQ == RQ_filter, fully_vaccinated_bl == FALSE) %>%
    group_by(!!sym(var), fully_vaccinated_bl) %>%
    summarise(nGroupUnVacc = sum(n), .groups = 'drop')
  
  aggreg_data <- left_join(aggreg_data, groupUnVacc, by = var) %>%
    distinct()
  
  # Calculate hospitalizations within cohorts
  rq_variable_a <- paste0("rq4a_", var, "_aggreg")
  assign(rq_variable_a, aggreg_data %>%
           select(-fully_vaccinated_bl) %>%
           group_by(!!sym(var), hospi_due_to_covid_bl) %>%
           summarise(overall = sum(n) / first(individuals_nm),
                     subgroup = sum(n) / first(nGroup),
                     .groups = 'drop') %>%
           distinct())
  
  # Hospitalizations within vaccinated cohort
  vacc <- aggreg_data %>%
    filter(fully_vaccinated_bl == TRUE) %>%
    group_by(!!sym(var), hospi_due_to_covid_bl) %>%
    summarise(overallVacc = sum(n) / first(individuals_vaccinated_nm),
              subgroupVacc = sum(n) / first(nGroupVacc),
              .groups = 'drop')
  
  # Hospitalizations within unvaccinated cohort
  unvacc <- aggreg_data %>%
    filter(fully_vaccinated_bl == FALSE) %>%
    group_by(!!sym(var), hospi_due_to_covid_bl) %>%
    summarise(overallUnVacc = sum(n) / first(individuals_not_vaccinated_nm),
              subgroupUnVacc = sum(n) / first(nGroupUnVacc),
              .groups = 'drop')
  
  rq_variable_b <- paste0("rq4b_", var, "_aggreg")
  assign(rq_variable_b, vacc %>% left_join(unvacc, by = c(var, "hospi_due_to_covid_bl")))
  
  # Clean up temporary variables
  rm(list = c("vacc", "unvacc", "group", "groupVacc", "groupUnVacc"))
}

# At the end of the loop, all aggregated variables like rq4_education_level_cd_aggreg are available in the environment.

































# rq4 education
rq4_educ_aggreg <- df %>%
  filter(RQ == "rq4_education_level_cd") %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = sum(n))

group <- df %>%
  filter(RQ == "rq4_education_level_cd") %>%
  group_by(education_level_cd) %>% 
  summarise(nGroup = sum(n))

rq4_educ_aggreg <- left_join(rq4_educ_aggreg, group)

groupVacc <- df %>%
  filter(RQ == "rq4_education_level_cd", fully_vaccinated_bl == TRUE) %>%
  group_by(education_level_cd, fully_vaccinated_bl) %>% 
  summarise(nGroupVacc = sum(n)) 

rq4_educ_aggreg <- left_join(rq4_educ_aggreg, groupVacc)

groupUnVacc <- df %>%
  filter(RQ == "rq4_education_level_cd", fully_vaccinated_bl == FALSE) %>%
  group_by(education_level_cd, fully_vaccinated_bl) %>% 
  summarise(nGroupUnVacc = sum(n))

rq4_educ_aggreg <- rq4_educ_aggreg %>% 
  left_join(groupUnVacc) %>% distinct()
 
rm(group, groupVacc, groupUnVacc)


# hospi within cohort
rq4a_educ_aggreg <- rq4_educ_aggreg %>%
  select(-fully_vaccinated_bl) %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overall = sum(n) / individuals_nm,
          subgroup = sum(n) / nGroup) %>% distinct()


# hospi within vacc cohort
vacc <- rq4_educ_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overallVacc = sum(n) / individuals_vaccinated_nm,
          subgroupVacc = sum(n) / nGroupVacc)


# within unVacc cohort
unvacc <- rq4_educ_aggreg %>%
  filter(fully_vaccinated_bl == FALSE) %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overallUnVacc = sum(n) / individuals_not_vaccinated_nm,
          subgroupUnVacc = sum(n) / nGroupUnVacc)


rq4b_educ_aggreg <- vacc %>% left_join(unvacc)

rm(vacc, unvacc)





### rq4 income
rq4_income_aggreg <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = sum(n))

group <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd) %>% 
  summarise(nGroup = sum(n))

rq4_income_aggreg <- left_join(rq4_income_aggreg, group)

groupVacc <- df %>%
  filter(RQ == "rq4_income_category_cd", fully_vaccinated_bl == TRUE) %>%
  group_by(income_category_cd, fully_vaccinated_bl) %>% 
  summarise(nGroupVacc = sum(n)) 

rq4_income_aggreg <- left_join(rq4_income_aggreg, groupVacc)

groupUnVacc <- df %>%
  filter(RQ == "rq4_income_category_cd", fully_vaccinated_bl == FALSE) %>%
  group_by(income_category_cd, fully_vaccinated_bl) %>% 
  summarise(nGroupUnVacc = sum(n))

rq4_income_aggreg <- rq4_income_aggreg %>% 
  left_join(groupUnVacc) %>% distinct()

rm(group, groupVacc, groupUnVacc)


# hospi within cohort
rq4a_income_aggreg <- rq4_income_aggreg %>%
  select(-fully_vaccinated_bl) %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overall = sum(n) / individuals_nm,
          subgroup = sum(n) / nGroup) %>% distinct()


# hospi within vacc cohort
vacc <- rq4_income_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overallVacc = sum(n) / individuals_vaccinated_nm,
          subgroupVacc = sum(n) / nGroupVacc)


# within unVacc cohort
unvacc <- rq4_income_aggreg %>%
  filter(fully_vaccinated_bl == FALSE) %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>%
  reframe(overallUnVacc = sum(n) / individuals_not_vaccinated_nm,
          subgroupUnVacc = sum(n) / nGroupUnVacc)


rq4b_income_aggreg <- vacc %>% left_join(unvacc)

rm(vacc, unvacc)
















fig <- plot_ly(rq4b_educ_aggreg, x = ~education_level_cd, y = ~subgroupVacc, type = 'bar', name = 'Vaccinated')

fig <- fig %>% add_trace(y = ~LA_Zoo, name = 'LA Zoo')

fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')


fig



plot_ly() %>%
  add_bars(data = rq4b_educ_aggreg %>% filter(hospi_due_to_covid_bl == FALSE), 
           x = ~education_level_cd, 
           y = ~subgroupVacc, 
           type = 'bar',
           name = "Non Hospi", 
           marker = list(color = 'rgba(0,255,0,0.6)')) %>%
  add_trace(x = ~education_level_cd, 
           y = ~subgroupUnVacc, 
           type = 'bar',
           name = "Hospi", 
           marker = list(color = 'rgba(0,0,255,0.6)')) %>%
  layout(title = "COVID-19 hospitalisation and vaccination",
         xaxis = list(title = "COVID-19 PCR Tests",
                      type = "category",
                      categoryorder = "array",
                      categoryarray = plotOrderEducation),
         yaxis = list(title = "Percentage",
                      tickformat = ",.0%",  # Formats y-axis values as percentages
                      hoverformat = ".2%"),  # This will format the hover text as percentages
         barmode = 'stack')





















# hospi in whole cohort
rq4a_educ_aggreg <- rq4_educ_aggreg %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / individuals_nm) %>% distinct()




# hospi+vacc in vacc cohort
rq4b_educ_aggreg_all <- rq4_educ_aggreg %>%
  select(-fully_vaccinated_bl) %>%
  group_by(education_level_cd) %>% 
  reframe(nGroup = sum(n))

rq4b_educ_aggreg_vacc <- rq4_educ_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  select(-fully_vaccinated_bl) %>%
  group_by(education_level_cd) %>% 
  reframe(nGroupVacc = sum(n)) %>% left_join(rq4b_educ_aggreg_all)

rq4b_educ_aggreg <- rq4_educ_aggreg %>%
  filter(fully_vaccinated_bl == FALSE) %>%
  select(-fully_vaccinated_bl) %>%
  group_by(education_level_cd) %>% 
  reframe(nGroupNotVacc = sum(n)) %>% left_join(rq4b_educ_aggreg_vacc)

rq4b_educ_aggreg <- rq4_educ_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  select(-fully_vaccinated_bl, -nsubgroup) %>% 
  left_join(rq4b_educ_aggreg_temp) %>%
  group_by(education_level_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / individuals_vaccinated_nm) %>% distinct()




















rq4_income_aggreg_subgoup <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd) %>% 
  summarise(nsubgroup = sum(n))

rq4_income_aggreg <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = sum(n))

rq4_income_aggreg <- left_join(rq4_income_aggreg, rq4_income_aggreg_subgoup)

# hospi in whole cohort
rq4a_income_aggreg <- rq4_income_aggreg %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / individuals_nm)

# hospi+vacc in vacc cohort
rq4b_income_aggreg <- rq4_income_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / individuals_vaccinated_nm)

individuals_vaccinated_nm
barplot_stacked_custom_q4 <- function(data,
                                      x_var,
                                      y_var,
                                      group_var = NULL, # New argument for grouping within the x-axis categories
                                      x_label,
                                      y_label,
                                      plot_title,
                                      plot_title_font = list(family = "Arial", size = 20, color = "#333333"),
                                      x_label_font = list(family = "Arial", size = 16, color = "#333333"),
                                      y_label_font = list(family = "Arial", size = 16, color = "#333333"),
                                      legend_title,
                                      legend_font = list(family = "Arial", size = 16, color = "#333333"),
                                      legend_position = "bottom",
                                      x_axis_order = NULL,
                                      barmode = 'stack', # New argument to specify bar mode: 'group' (default) or 'stack'
                                      color_palette = 'Viridis' # Optional argument to specify color palette
) {
  # Basic plot setup
  plot <- plot_ly(data, type = 'bar')
  
  # Conditional setup if group_var is specified
  if (!is.null(group_var)) {
    plot <- plot %>%
      add_bars(x = ~get(x_var), y = ~get(y_var), color = ~get(group_var), colors = color_palette)
  } else {
    plot <- plot %>%
      add_bars(x = ~get(x_var), y = ~get(y_var))
  }
  
  # Additional plot settings
  plot <- plot %>%
    layout(title = list(text = plot_title, font = plot_title_font),
           xaxis = list(title = x_label, titlefont = x_label_font,
                        type = "category",
                        categoryorder = "array",
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, titlefont = y_label_font, tickformat = ',.0%'),
           barmode = barmode, # Apply barmode setting
           legend = list(title = list(text = legend_title), font = legend_font, orientation = legend_position),
           hovermode = 'closest')
  
  return(plot)
}






p1 <- barplot_stacked_custom_q4(data = rq4a_income_aggreg,
                                x_var = "income_category_cd",
                                y_var = "ratioSubGroup",
                                group_var = "hospi_due_to_covid_bl",  # Grouping variable
                                x_label = "Income Category",
                                y_label = "Percentage",
                                plot_title = "Hospitalized due to COVID-19/subgroup population",
                                legend_title = "Covid-19 Hospitalisations",
                                x_axis_order = plotOrderIncome)


p2 <- barplot_stacked_custom_q4(data = rq4b_income_aggreg,
                                x_var = "income_category_cd",
                                y_var = "ratioSubGroup",
                                group_var = "hospi_due_to_covid_bl",  # Grouping variable
                                x_label = "Income Category",
                                y_label = "Percentage",
                                plot_title = "Hospitalized due to COVID-19/subgroup population",
                                legend_title = "Covid-19 Hospitalisations",
                                x_axis_order = plotOrderIncome)


subplot(p1, p2, titleX = TRUE, shareY = TRUE) %>% layout(barmode = 'stack')



# # rq4b
# # % in whole vaccinated cohort
# rq4a_educ_aggreg_subgoup <- df %>%
#   filter(RQ == "rq4_education_level_cd", fully_vaccinated_bl == TRUE) %>%
#   group_by(education_level_cd,
#            hospi_due_to_covid_bl) %>% 
#   summarise(nsubgroup = sum(n))
# 
# 
# 
# rq4b_educ_aggreg <- df %>%
#   filter(RQ == "rq4_education_level_cd", fully_vaccinated_bl == TRUE) %>%
#   group_by(education_level_cd,
#            hospi_due_to_covid_bl) %>% 
#   summarise(n = sum(n),
#             nTot = individuals_vaccinated_nm)
#   





# % in each group
p1_plotly <- barplot_custom(data = rq4a_educ_aggreg %>% filter(hospi_due_to_covid_bl == TRUE),
                            x_var = "education_level_cd",
                            y_var = "ratioCohort",
                            x_label = "Education Level",
                            y_label = "Percentage",
                            plot_title = "rq4 education: hospitalized due to COVID-19/total population",
                            legend_title = 'Status',
                            x_axis_order = plotOrderEducation)

p1_plotly


library(plotly)

barplot_custom_stacked_q4 <- function(data,
                           x_var,
                           y_var,
                           group_var = NULL, # New argument for grouping within the x-axis categories
                           x_label,
                           y_label,
                           plot_title,
                           plot_title_font = list(family = "Arial", size = 20, color = "#333333"),
                           x_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           y_label_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_title,
                           legend_font = list(family = "Arial", size = 16, color = "#333333"),
                           legend_position = "bottom",
                           x_axis_order = NULL,
                           barmode = 'stack', # New argument to specify bar mode: 'group' (default) or 'stack'
                           color_palette = 'Viridis' # Optional argument to specify color palette
) {
  # Basic plot setup
  plot <- plot_ly(data, type = 'bar')
  
  # Conditional setup if group_var is specified
  if (!is.null(group_var)) {
    plot <- plot %>%
      add_bars(x = ~get(x_var), y = ~get(y_var), color = ~get(group_var), colors = color_palette)
  } else {
    plot <- plot %>%
      add_bars(x = ~get(x_var), y = ~get(y_var))
  }
  
  # Additional plot settings
  plot <- plot %>%
    layout(title = list(text = plot_title, font = plot_title_font),
           xaxis = list(title = x_label, titlefont = x_label_font,
                        type = "category",
                        categoryorder = "array",
                        categoryarray = x_axis_order),
           yaxis = list(title = y_label, titlefont = y_label_font, tickformat = ',.0%'),
           barmode = barmode, # Apply barmode setting
           legend = list(title = list(text = legend_title), font = legend_font, orientation = legend_position),
           hovermode = 'closest')
  
  return(plot)
}





p1_plotly <- barplot_custom_stacked_q4(data = rq4a_educ_aggreg,
                                    x_var = "education_level_cd",
                                    y_var = "ratioSubGroup",
                                    group_var = "hospi_due_to_covid_bl",  # Grouping variable
                                    x_label = "Education Level",
                                    y_label = "Percentage",
                                    plot_title = "rq4 education: hospitalized due to COVID-19/subgroup population",
                                    legend_title = "Covid-19 Hospitalisations",
                                    x_axis_order = plotOrderEducation)

p1_plotly



fig <- barplot_stacked_custom(data = rq3_gender_doses,
                              x_var = 'sex_cd',
                              y_var = map_col_doses, 
                              x_label = "Gender",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Gender",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderSex)
fig






rq4_income_aggreg_subgoup <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd) %>% 
  summarise(nsubgroup = sum(n))

rq4_income_aggreg <- df %>%
  filter(RQ == "rq4_income_category_cd") %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl,
           fully_vaccinated_bl) %>% 
  summarise(n = sum(n),
            nTot = individuals_nm)

rq4_income_aggreg <- left_join(rq4_income_aggreg, rq4_income_aggreg_subgoup)

# hospi in whole cohort
rq4a_income_aggreg <- rq4_income_aggreg %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / nTot)

# hospi+vacc in vacc cohort
rq4b_income_aggreg <- rq4_income_aggreg %>%
  filter(fully_vaccinated_bl == TRUE) %>%
  group_by(income_category_cd,
           hospi_due_to_covid_bl) %>% 
  reframe(ratioSubGroup = sum(n) / nsubgroup,
          ratioCohort = sum(n) / nTot)
















rq4_income_aggreg <- df %>% 
  filter(RQ == "rq4_income_category_cd") %>% 
  select(income_category_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(income_category_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)


rq4_migration_aggreg <- df %>% 
  filter(RQ == "rq4_migration_background_cd") %>% 
  select(migration_background_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(migration_background_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)

rq4_household_aggreg <- df %>% 
  filter(RQ == "rq4_household_type_cd") %>% 
  select(household_type_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(household_type_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)

rq4_age_group <- df %>% 
  filter(RQ == "rq4_age_cd") %>% 
  select(age_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(age_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)

rq4_gender <- df %>% 
  filter(RQ == "rq4_sex_cd" & sex_cd %in% c("M", "F")) %>% 
  select(sex_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(sex_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)

rq4_area <- df %>% 
  filter(RQ == "rq4_area") %>% 
  select(residence_area_cd,
         range1,
         range2,
         range3,
         range4,
         n) %>% 
  group_by(residence_area_cd) %>%
  summarise("-hospi-vaccinated" = range1 / n * 100,
            "-hospi+vaccinated" = range2 / n * 100,
            "+hospi-vaccinated" = range3 / n * 100,
            "+hospi+vaccinated" = range4 / n * 100,
            n = n)


# range1 = ifelse(hospi_due_to_covid_bl < 1, 1, 0)
# range2 = ifelse(hospi_due_to_covid_bl == 1, 1, 0)
# range3 = ifelse(hospi_due_to_covid_bl > 1, 1, 0)

rq4_educ_hospi <- df %>% 
  filter(RQ == "rq4_education_level_cd_hospi") %>% 
  select(education_level_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(education_level_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)


rq4_income_hospi <- df %>% 
  filter(RQ == "rq4_income_category_cd_hospi") %>% 
  select(income_category_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(income_category_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)

rq4_household_hospi <- df %>% 
  filter(RQ == "rq4_household_type_cd_hospi") %>% 
  select(household_type_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(household_type_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)

rq4_migration_hospi <- df %>% 
  filter(RQ == "rq4_migration_background_cd_hospi") %>% 
  select(migration_background_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(migration_background_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)

rq4_age_group_hospi <- df %>% 
  filter(RQ == "rq4_age_cd_hospi") %>% 
  select(age_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(age_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)

rq4_gender_hospi <- df %>% 
  filter(RQ == "rq4_sex_cd_hospi" & sex_cd %in% c('M', 'F')) %>% 
  select(sex_cd, fully_vaccinated_bl, range1, range2, range3, n) %>% 
  group_by(sex_cd, fully_vaccinated_bl) %>%
  summarise("0" = range1 / n * 100,
            "1" = range2 / n * 100,
            ">1" = range3 / n * 100,
            n = n)






rq4b_summary <- df %>% 
  filter(RQ == "rq4b_summary") %>% 
  select(country,
         education_level_cd, 
         income_category_cd, 
         household_type_cd, 
         migration_background_cd,
         ratio,
         n)

rq4b_educ_aggreg <- df %>% 
  filter(RQ == "rq4b_education_level_cd") %>% 
  select(education_level_cd, ratio, n, country) %>% 
  group_by(country, education_level_cd)

rq4b_income_aggreg <- df %>% 
  filter(RQ == "rq4b_income_category_cd") %>% 
  select(income_category_cd, ratio, n, country) %>% 
  group_by(country, income_category_cd)

rq4b_migration_aggreg <- df %>% 
  filter(RQ == "rq4b_migration_background_cd") %>% 
  select(migration_background_cd, ratio, n, country) %>% 
  group_by(country, migration_background_cd)

rq4b_household_aggreg <- df %>% 
  filter(RQ == "rq4b_household_type_cd") %>% 
  select(household_type_cd, ratio, n, country) %>% 
  group_by(country, household_type_cd)

rq4b_age_group <- df %>% 
  filter(RQ == "rq4b_age_cd") %>% 
  select(country, age_cd, n , ratio) %>% 
  group_by(country, age_cd)

rq4b_gender <- df %>% 
  filter(RQ == "rq4b_sex_cd" & sex_cd %in% c("M", "F")) %>% 
  select(country, sex_cd, n , ratio) %>% 
  group_by(country, sex_cd)

rq4b_area <- df %>% 
  filter(RQ == "rq4b_area") %>% 
  select(country,
         residence_area_cd,
         ratio,
         n)

rq4b_educ_hospi <- df %>% 
  filter(RQ == "rq4b_education_level_cd_hospi") %>% 
  select(education_level_cd, range1, range2, range3, n) %>% 
  group_by(education_level_cd)

rq4b_income_hospi <- df %>% 
  filter(RQ == "rq4b_income_category_cd_hospi") %>% 
  select(income_category_cd, range1, range2, range3, n) %>% 
  group_by(income_category_cd)

rq4b_household_hospi <- df %>% 
  filter(RQ == "rq4b_household_type_cd_hospi") %>% 
  select(household_type_cd, range1, range2, range3, n) %>% 
  group_by(household_type_cd)

rq4b_migration_hospi <- df %>% 
  filter(RQ == "rq4b_migration_background_cd_hospi") %>% 
  select(migration_background_cd, range1, range2, range3, n) %>% 
  group_by(migration_background_cd)

rq4b_age_group_hospi <- df %>% 
  filter(RQ == "rq4b_age_cd_hospi") %>% 
  select(age_cd, range1, range2, range3, n) %>% 
  group_by(age_cd)

rq4b_gender_hospi <- df %>% 
  filter(RQ == "rq4b_sex_cd_hospi" & sex_cd %in% c("M", "F")) %>% 
  select(sex_cd, range1, range2, range3, n) %>% 
  group_by(sex_cd)




### RQ1 & RQ2

# Average Testing graph

p1_plotly <- boxplot_custom_group(data = rq1_2_educ_aggreg, 
                            x_var = "education_level_cd",
                            x_label = "Education Level",
                            y_label = "PCR Tests",
                            plot_title = "RQ1 Education: Average number of COVID-19 PCR tests",
                            x_axis_order = plotOrderEducation)

p1_plotly

p1_plotly <- boxplot_custom_group(data = rq1_2_income_aggreg, 
                                  x_var = "income_category_cd",
                                  x_label = "Income Category",
                                  y_label = "PCR Tests",
                                  plot_title = "RQ1 Income: Average number of COVID-19 PCR tests",
                                  x_axis_order = plotOrderIncome)

p1_plotly

p1_plotly <- boxplot_custom_group(data = rq1_2_migration_aggreg, 
                                  x_var = "migration_background_cd",
                                  x_label = "Migration Background",
                                  y_label = "PCR Tests",
                                  plot_title = "RQ1 Migration: Average number of COVID-19 PCR tests",
                                  x_axis_order = plotOrderMigration)

p1_plotly

p1_plotly <- boxplot_custom_group(data = rq1_2_household_aggreg, 
                                  x_var = "household_type_cd",
                                  x_label = "Household type",
                                  y_label = "PCR Tests",
                                  plot_title = "RQ1 Household: Average number of COVID-19 PCR tests",
                                  x_axis_order = plotOrderHousehold)

p1_plotly

p1_plotly <- boxplot_custom_group(data = rq1_2_age_group, 
                                  x_var = "age_cd",
                                  x_label = "Age groups",
                                  y_label = "PCR Tests",
                                  plot_title = "RQ1 Household: Average number of COVID-19 PCR tests",
                                  x_axis_order = plotOrderAge)

p1_plotly

p1_plotly <- boxplot_custom_group(data = rq1_2_gender, 
                                  x_var = "sex_cd",
                                  x_label = "Gender",
                                  y_label = "PCR Tests",
                                  plot_title = "RQ1 Gender: Average number of COVID-19 PCR tests",
                                  x_axis_order = plotOrderSex)

p1_plotly



# average Testing Statistic tables 
# education
new_col_names <- c(education_level_cd = "Education Level",
                   type = "Tests",
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_educ_table <- setNames(rq1_2_educ_aggreg[, names(new_col_names)], new_col_names)

datatable(rq1_2_educ_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold




# income
new_col_names <- c(income_category_cd = "Income Category", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_income_table <- setNames(rq1_2_income_aggreg[, names(new_col_names)], new_col_names)

datatable(rq1_2_income_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold


# migration
new_col_names <- c(migration_background_cd = "Migration Background", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_migration_table <- setNames(rq1_2_migration_aggreg[, names(new_col_names)], new_col_names)

datatable(rq1_2_migration_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold



# household
new_col_names <- c(household_type_cd = "Household type", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_household_table <- setNames(rq1_2_household_aggreg[, names(new_col_names)], new_col_names)

datatable(rq1_2_household_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold


# age group
new_col_names <- c(age_cd = "Age groups", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_age_group_table <- setNames(rq1_2_age_group[, names(new_col_names)], new_col_names)

datatable(rq1_2_age_group_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold



# gender
new_col_names <- c(sex_cd = "Gender", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_gender_table <- setNames(rq1_2_gender[, names(new_col_names)], new_col_names)

datatable(rq1_2_gender_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold

# area
new_col_names <- c(residence_area_cd = "Area", 
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_area_table <- setNames(rq1_2_area[, names(new_col_names)], new_col_names)

datatable(rq1_2_area_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold


# summary
new_col_names <- c(type = "Tests",
                   education_level_cd = "Education",
                   income_category_cd = "Income",
                   household_type_cd = "Household",
                   migration_background_cd = "Migration",
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_summary_table <- setNames(rq1_2_summary[, names(new_col_names)], new_col_names)

datatable(rq1_2_summary_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%  # Round "mean" and "sd" column values
  formatStyle(columns = c("Mean"), fontWeight = 'bold')  # Make "mean" column values bold



# by vaccination tests
map_col_tests <- list(
  list("range1", "0"),
  list("range2", "1"),
  list("range3", "2 or more")
)


fig <- barplot_stacked_custom(data = rq1_2_educ_tests,
                              x_var = 'education_level_cd',
                              y_var = map_col_tests, 
                              x_label = "Education level",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Education Level",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderEducation)
fig


fig <- barplot_stacked_custom(data = rq1_2_income_tests,
                              x_var = 'income_category_cd',
                              y_var = map_col_tests, 
                              x_label = "Income category",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Income category",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderIncome)
fig

fig <- barplot_stacked_custom(data = rq1_2_migration_tests,
                              x_var = 'migration_background_cd',
                              y_var = map_col_tests, 
                              x_label = "Migration background",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Migration background",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderMigration)
fig

fig <- barplot_stacked_custom(data = rq1_2_household_tests, 
                              x_var = "household_type_cd",
                              y_var = map_col_tests, 
                              x_label = "Household type",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Household type",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderHousehold)
fig

fig <- barplot_stacked_custom(data = rq1_2_age_group_tests,
                              x_var = 'age_cd',
                              y_var = map_col_tests, 
                              x_label = "Age groups",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Age Group",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderAge)
fig

fig <- barplot_stacked_custom(data = rq1_2_gender_tests,
                              x_var = 'sex_cd',
                              y_var = map_col_tests, 
                              x_label = "Gender",
                              y_label = "Percentage",
                              plot_title = "Covid-19 Tests by Gender",
                              legend_title = 'Number of tests',
                              x_axis_order = plotOrderSex)
fig

















### RQ3

# Ratio vaccination graph

map_vacc <- list(
  list("ratio", "Vaccinated"),
  list("ratio3", "Not vaccinated")
)


map_vacc2 <- c("ratio2", "Overall Status")


fig <- barplot_stacked_custom(data = rq3_educ_aggreg,
                              x_var = 'education_level_cd',
                              y_var = map_vacc,
                              y_var2 = c("ratio2" = "Overall Status"),
                              x_label = "Education level",
                              y_label = "Percentage",
                              plot_title = "Vaccination by Education Level",
                              legend_title = 'Status',
                              x_axis_order = plotOrderEducation)
fig












# p1_plotly <- barplot_custom(data = rq3_educ_aggreg, 
#                             x_var = "education_level_cd",
#                             y_var = "ratio",
#                             x_label = "Education Level",
#                             y_label = "Ratio",
#                             plot_title = "RQ3 Education: Vaccinated against Covid-19 individuals in the cohort",
#                             x_axis_order = plotOrderEducation)
# 
# p1_plotly

p1_plotly <- barplot_custom(data = rq3_income_aggreg, 
                            x_var = "income_category_cd",
                            y_var = "ratio",
                            x_label = "Income Category",
                            y_label = "Ratio",
                            plot_title = "RQ3 Income: Vaccinated against Covid-19 individuals in the cohort",
                            x_axis_order = plotOrderIncome)

p1_plotly

p1_plotly <- barplot_custom(data = rq3_migration_aggreg, 
                            x_var = "migration_background_cd",
                            y_var = "ratio",
                            x_label = "Migration Background",
                            y_label = "Ratio",
                            plot_title = "RQ3 Migration: Vaccinated against Covid-19 individuals in the cohort",
                            x_axis_order = plotOrderMigration)

p1_plotly

p1_plotly <- barplot_custom(data = rq3_household_aggreg, 
                            x_var = "household_type_cd",
                            y_var = "ratio",
                            x_label = "Household type",
                            y_label = "Ratio",
                            plot_title = "RQ3 Household: Vaccinated against Covid-19 individuals in the cohort",
                            x_axis_order = plotOrderHousehold)

p1_plotly

p1_plotly <- barplot_custom(data = rq3_age_group, 
                            x_var = "age_cd",
                            y_var = "ratio",
                            x_label = "Age groups",
                            y_label = "Ratio",
                            plot_title = "RQ3 Household: Vaccinated against Covid-19 individuals in the cohort",
                            x_axis_order = plotOrderAge)

p1_plotly

p1_plotly <- barplot_custom(data = rq3_gender, 
                            x_var = "sex_cd",
                            y_var = "ratio",
                            x_label = "Gender",
                            y_label = "Ratio",
                            plot_title = "RQ3 Gender: Vaccinated against Covid-19 individuals in the cohort",
                            x_axis_order = plotOrderSex)

p1_plotly





# education
new_col_names <- c(education_level_cd = "Education Level",
                   ratio = "Ratio", 
                   n = "N")

rq3_educ_table <- setNames(rq3_educ_aggreg[, names(new_col_names)], new_col_names)

datatable(rq3_educ_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  




# income
new_col_names <- c(income_category_cd = "Income Category", 
                   ratio = "Ratio", 
                   n = "N")

rq3_income_table <- setNames(rq3_income_aggreg[, names(new_col_names)], new_col_names)

datatable(rq3_income_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  


# migration
new_col_names <- c(migration_background_cd = "Migration Background", 
                   ratio = "Ratio", 
                   n = "N")

rq3_migration_table <- setNames(rq3_migration_aggreg[, names(new_col_names)], new_col_names)

datatable(rq3_migration_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  



# household
new_col_names <- c(household_type_cd = "Household type", 
                   ratio = "Ratio", 
                   n = "N")

rq3_household_table <- setNames(rq3_household_aggreg[, names(new_col_names)], new_col_names)

datatable(rq3_household_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  


# age group
new_col_names <- c(age_cd = "Age groups", 
                   ratio = "Ratio", 
                   n = "N")

rq3_age_group_table <- setNames(rq3_age_group[, names(new_col_names)], new_col_names)

datatable(rq3_age_group_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  



# gender
new_col_names <- c(sex_cd = "Gender", 
                   ratio = "Ratio", 
                   n = "N")

rq3_gender_table <- setNames(rq3_gender[, names(new_col_names)], new_col_names)

datatable(rq3_gender_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  

# area
new_col_names <- c(residence_area_cd = "Area", 
                   ratio = "Ratio", 
                   n = "N")

rq3_area_table <- setNames(rq3_area[, names(new_col_names)], new_col_names)

datatable(rq3_area_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  


# summary
new_col_names <- c(education_level_cd = "Education",
                   income_category_cd = "Income",
                   household_type_cd = "Household",
                   migration_background_cd = "Migration",
                   n = "N", 
                   ratio = "Ratio")

rq3_summary_table <- setNames(rq3_summary[, names(new_col_names)], new_col_names)

datatable(rq3_summary_table, 
          filter = "top", 
          options = list(pageLength = 8, class = 'row-border stripe hover compact'), 
          rownames = FALSE, 
          autoHideNavigation = TRUE, escape = FALSE) %>%
  formatRound(columns = c("Ratio"), digits = 2) %>%  
  formatStyle(columns = c("Ratio"), fontWeight = 'bold')  





# by vaccination doses
map_col_doses <- list(
  list("range1", "0"),
  list("range2", "1"),
  list("range3", "2 or more")
)

fig <- barplot_stacked_custom(data = rq3_educ_doses,
                              x_var = 'education_level_cd',
                              y_var = map_col_doses, 
                              x_label = "Education level",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Education Level",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderEducation)
fig


fig <- barplot_stacked_custom(data = rq3_income_doses,
                              x_var = 'income_category_cd',
                              y_var = map_col_doses, 
                              x_label = "Income category",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Income category",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderIncome)
fig

fig <- barplot_stacked_custom(data = rq3_migration_doses,
                              x_var = 'migration_background_cd',
                              y_var = map_col_doses, 
                              x_label = "Migration background",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Migration background",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderMigration)
fig

fig <- barplot_stacked_custom(data = rq3_household_doses, 
                              x_var = "household_type_cd",
                              y_var = map_col_doses, 
                              x_label = "Household type",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Household type",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderHousehold)
fig

fig <- barplot_stacked_custom(data = rq3_age_group_doses,
                              x_var = 'age_cd',
                              y_var = map_col_doses, 
                              x_label = "Age groups",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Age Group",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderAge)
fig

fig <- barplot_stacked_custom(data = rq3_gender_doses,
                              x_var = 'sex_cd',
                              y_var = map_col_doses, 
                              x_label = "Gender",
                              y_label = "Percentage",
                              plot_title = "Doses Distribution by Gender",
                              legend_title = 'Number of doses',
                              x_axis_order = plotOrderSex)
fig


# income
# migration
# household
# age group
# gender
# area

### RQ4a & RQ4b
# summary
# education
# income
# migration
# household
# age group
# gender
# area


# by hospitatilsation
map_col_hospi <- list(
  list("range1", "0"),
  list("range2", "1"),
  list("range3", "2 or more")
)

fig <- barplot_stacked_custom(data = rq4a_educ_hospi,
                              x_var = 'education_level_cd',
                              y_var = map_col_hospi, 
                              x_label = "Education level",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations by Education Level",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderEducation)
fig



fig <- barplot_stacked_custom(data = rq4a_income_hospi,
                              x_var = 'income_category_cd',
                              y_var = map_col_hospi, 
                              x_label = "Income category",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations by Income category",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderIncome)
fig

fig <- barplot_stacked_custom(data = rq4a_migration_hospi,
                              x_var = 'migration_background_cd',
                              y_var = map_col_hospi, 
                              x_label = "Migration background",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisationsby Migration background",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderMigration)
fig

fig <- barplot_stacked_custom(data = rq4a_household_hospi, 
                              x_var = "household_type_cd",
                              y_var = map_col_hospi, 
                              x_label = "Household type",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations by Household type",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderHousehold)
fig

fig <- barplot_stacked_custom(data = rq4a_age_group_hospi,
                              x_var = 'age_cd',
                              y_var = map_col_hospi, 
                              x_label = "Age groups",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations by Age Group",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderAge)
fig

fig <- barplot_stacked_custom(data = rq4a_gender_hospi,
                              x_var = 'sex_cd',
                              y_var = map_col_hospi, 
                              x_label = "Gender",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations by Gender",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderSex)
fig






# RQ4b












fig <- barplot_stacked_custom(data = rq4b_educ_hospi,
                              x_var = 'education_level_cd',
                              y_var = map_col_hospi, 
                              x_label = "Education level",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Education Level",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderEducation)
fig



fig <- barplot_stacked_custom(data = rq4b_income_hospi,
                              x_var = 'income_category_cd',
                              y_var = map_col_hospi, 
                              x_label = "Income category",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Income category",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderIncome)
fig

fig <- barplot_stacked_custom(data = rq4b_migration_hospi,
                              x_var = 'migration_background_cd',
                              y_var = map_col_hospi, 
                              x_label = "Migration background",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Migration background",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderMigration)
fig

fig <- barplot_stacked_custom(data = rq4b_household_hospi, 
                              x_var = "household_type_cd",
                              y_var = map_col_hospi, 
                              x_label = "Household type",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Household type",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderHousehold)
fig

fig <- barplot_stacked_custom(data = rq4b_age_group_hospi,
                              x_var = 'age_cd',
                              y_var = map_col_hospi, 
                              x_label = "Age groups",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Age Group",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderAge)
fig

fig <- barplot_stacked_custom(data = rq4b_gender_hospi,
                              x_var = 'sex_cd',
                              y_var = map_col_hospi, 
                              x_label = "Gender",
                              y_label = "Percentage",
                              plot_title = "Covid-19 hospitalisations in vaccinated population by Gender",
                              legend_title = 'Covid-19 hospitalisations',
                              x_axis_order = plotOrderSex)
fig







new_col_names <- c(type = "PCR Tests",
                   education_level_cd = "Education",
                   income_category_cd = "Income",
                   household_type_cd = "Household",
                   migration_background_cd = "Migration",
                   n = "N", 
                   mean = "Mean", 
                   sd = "SD",
                   q1 = "Q1",
                   median = "Median", 
                   q3 = "Q3")

rq1_2_summary_table <- setNames(rq1_2_summary[, names(new_col_names)], new_col_names)

datatable(rq1_2_summary_table, 
          filter = "top", 
          options = list(
            pageLength = 8, 
            class = 'row-border stripe hover compact',
            autoHideNavigation = TRUE, 
            escape = FALSE,
            # Define column groups here
            columnGroups = list(
              list(title = "Socio-economic Parameters", columns = c(2, 3, 4, 5)),
              list(title = "Statistics", columns = c(6, 7, 8, 9, 10, 11))
            )
          ), 
          rownames = FALSE) %>%
  formatRound(columns = c("Mean", "SD", "Median"), digits = 2) %>%
  formatStyle(columns = c("Mean"), fontWeight = 'bold')







