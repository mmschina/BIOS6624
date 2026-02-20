# Data Cleaning for Final Report


# Necessary packages
library(here)
library(dplyr)


# Loading original dataset
hiv_data <- read.csv(here("Project 1/Data", "hiv_6624_final.csv"))


# Subsetting dataset to only include baseline and year 2
hiv_data <- hiv_data %>%
  filter(years %in% c(0,2))

# Removing any observations missing values for four outcomes of interest
hiv_data_cleaned <- hiv_data %>%
  filter(!is.na(VLOAD),
         !is.na(LEU3N),
         !is.na(AGG_MENT),
         !is.na(AGG_PHYS))


# Handling implausible values
# Excluding participants with BMIs less than 6.7 or greater than 250
hiv_data_cleaned <- hiv_data_cleaned %>%
  filter(bmi >= 6.7 & bmi <= 250)


# Removing any participants missing either baseline or year 2 visits 
hiv_data_cleaned <- hiv_data_cleaned %>%
  group_by(newid) %>%
  filter(n() == 2) %>%
  ungroup() 


# Log10 transforming Viral Load
hiv_data_cleaned <- hiv_data_cleaned %>%
  mutate(log_vload = log10(VLOAD))



# Reorganizing dataframe so there is only one row per participant
# Maintaining baseline age, bmi, race, education, hard drug use indicator, smoking status, income
# Maintaining year 2 adherence
hiv_data_org <- hiv_data_cleaned %>%
  group_by(newid) %>%
  summarise(log_VLOAD_B = log_vload[years == 0],
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



# Collapsing race and education categories
# Might need to more 4 for education, clarify collapse criteria (At least one year college but no degree)
hiv_data_org <- hiv_data_or %>%
  mutate(education_B = case_when(education_B %in% c(1,2,3) ~ "High School",
                                 education_B %in% c(4,5,6,7) ~ "College Experience or Better"),
         race_B = case_when(race_B %in% c(2,3,4,7,8) ~ "Other",
                            race_B == 1 ~ "White, Non-Hispanic"))



# Creating csv file of clean data set for analysis
# write.csv(hiv_data_org, "hiv_data_cleaned.csv", row.names = FALSE)


