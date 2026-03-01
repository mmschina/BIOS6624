# Data Cleaning for Final Report


# Necessary packages
library(here)
library(dplyr)


# Loading original dataset
hiv_data <- read.csv(here("Project 1/Data", "hiv_6624_final.csv"))


# Subsetting dataset to only include baseline and year 2
hiv_data_cleaned <- hiv_data %>%
  filter(years %in% c(0,2))


# Removing any participants missing either baseline or year 2 visits
hiv_data_cleaned <- hiv_data_cleaned %>%
  group_by(newid) %>%
  filter(n() == 2) %>%
  ungroup()


# Log10 transforming Viral Load to address skewness found during exploratory study
hiv_data_cleaned <- hiv_data_cleaned %>%
  mutate(log_vload = log10(VLOAD))


# Reorganizing dataframe so there is only one row per participant
# Keeping baseline covariates of age, bmi, race, education, hard drug use indicator, smoking status, income
# Keeping year 2 covariate of adherence
hiv_data_org <- hiv_data_cleaned %>%
  group_by(newid) %>%
  summarise(VLOAD_B = VLOAD[years == 0],
            log_VLOAD_B = log_vload[years == 0],
            log_VLOAD_Y2 = log_vload[years == 2],
            LEU3N_B = LEU3N[years == 0],
            LEU3N_Y2 = LEU3N[years == 2],
            AGG_MENT_B = AGG_MENT[years == 0],
            AGG_MENT_Y2 = AGG_MENT[years == 2],
            AGG_PHYS_B = AGG_PHYS[years == 0],
            AGG_PHYS_Y2 = AGG_PHYS[years == 2],
            age_B = age[years == 0],
            bmi_B = BMI[years == 0],
            race_B = RACE[years == 0],
            education_B = EDUCBAS[years == 0],
            hard_drugs_B = hard_drugs[years == 0],
            smoking_B = SMOKE[years == 0],
            income_B = income[years == 0],
            adherence_Y2 = ADH[years == 2] 
  )


# Collapsing education, race, smoking and adherence categories
hiv_data_org <- hiv_data_org %>%
  mutate(education_B = case_when(education_B %in% c(1,2,3,4) ~ "Not College Graduate",
                                 education_B %in% c(5,6,7) ~ "College Graduate"),
         race_B = case_when(race_B %in% c(2,3,4,7,8) ~ "Other",
                            race_B == 1 ~ "White, Non-Hispanic"),
         smoking_B = case_when(smoking_B %in% c(1,2) ~ "Not Current Smoker",
                               smoking_B == 3 ~ "Current Smoker"),
         adherence_Y2 = case_when(adherence_Y2 %in% c(1,2) ~ "> 95%",
                                  adherence_Y2 %in% c(3,4) ~ "< 95%"))


# Removing observations with "Do Not Wish to Answer" for income (9)
no_income_ids <- hiv_data_org %>%
  filter(income_B == 9) %>%
  pull(newid)

hiv_data_org <- hiv_data_org %>%
  filter(!newid %in% no_income_ids)


# Removing observations with implausible BMI values, < 6.7 (anorexic) or > 250 (extreme obesity)
implausible_bmi_ids <- hiv_data_org %>%
  filter(bmi_B < 6.7 | bmi_B > 250) %>%
  pull(newid)

hiv_data_org <- hiv_data_org %>%
  filter(!newid %in% implausible_bmi_ids)


# Excluding any observations that have missing values for any of the outcomes of interest
hiv_data_org <- hiv_data_org %>%
  filter(if_all(c(log_VLOAD_B, log_VLOAD_Y2, LEU3N_B, LEU3N_Y2, AGG_MENT_B, AGG_MENT_Y2, AGG_PHYS_B, AGG_PHYS_Y2), ~ !is.na(.)))



# Creating csv file of clean data set for analysis
write.csv(hiv_data_org, here("Project 1/Data", "hiv_data_cleaned.csv"), row.names = FALSE)


