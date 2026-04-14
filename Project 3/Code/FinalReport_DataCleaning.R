# Data Cleaning for Final Report


# Necessary packages
library(here)
library(dplyr)


# Loading original dataset
frmgham_data <- read.csv(here("Project 3/Data", "frmgham2.csv"))


# Removing anyone that reported stroke at baseline as it thus took place before the study
frmgham_data_clean <- frmgham_data %>%
  filter(!(RANDID %in% (frmgham_data %>% 
                          filter(STROKE == 1 & TIMESTRK == 0) %>%
                          pull(RANDID))))


# Censoring stroke and death at 10 years
# stroke_10yr: indicator of if stroke occurred before 10 years and before death
# stroke_time: if stroke occurred then stroke time, if death occurred before stroke then death time, otherwise 10 years
ten_years <- 365.25 * 10
frmgham_data_subset <- frmgham_data_clean %>%
  mutate(stroke_10yr = ifelse(STROKE == 1 & TIMESTRK <= ten_years & (is.na(TIMEDTH) | TIMESTRK <= TIMEDTH), 1, 0),
         stroke_time = case_when(STROKE == 1 & TIMESTRK <= ten_years & (is.na(TIMEDTH) | TIMESTRK <= TIMEDTH) ~ TIMESTRK,
                                 DEATH == 1 & TIMEDTH <= ten_years ~ TIMEDTH,
                                 TRUE ~ ten_years))


# Subsetting censored data to only baseline visits since we are only interested in predictors at baseline
frmgham_data_subset <- frmgham_data_subset %>%
  filter(TIME == 0)

# Only keeping variables of interest
vars_of_interest <- c("SEX", "stroke_10yr", "stroke_time", "DIABETES", "SYSBP", "AGE", "PREVCHD", "BPMEDS", "CURSMOKE", "TOTCHOL", "BMI")

frmgham_data_subset <- frmgham_data_subset[, vars_of_interest]



# Creating csv file of clean data set for analysis
write.csv(frmgham_data_subset, here("Project 3/Data", "frmgham_data_cleaned.csv"), row.names = FALSE)


