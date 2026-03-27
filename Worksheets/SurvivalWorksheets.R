
library(here)
library(survival)
library(survminer)
library(gtsummary)

# Survival Worksheet 1
# Loading the data on ovarian cancer
ov_data <- read.csv(here("Worksheets", "ovarian.csv"))

treat1 <- subset(ov_data, rx == 1)
km_fit1 <- survfit(Surv(futime, fustat) ~ 1, data = treat1)


# Extract table with CIs to verify by hand
km_table1 <- data.frame(
  time     = km_fit1$time,
  n.risk   = km_fit1$n.risk,
  n.event  = km_fit1$n.event,
  n.censor = km_fit1$n.censor,
  surv     = round(km_fit1$surv, 4),
  lower_CI = round(km_fit1$lower, 4),   # lower 95% CI
  upper_CI = round(km_fit1$upper, 4)    # upper 95% CI
)

print(km_table1)


# Fit KM curve for both groups using full dataset
km_fit_both <- survfit(Surv(futime, fustat) ~ rx, data = ov_data)


# Plot both curves together
ggsurvplot(km_fit_both,
           data = ov_data,
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,                  # adds log-rank p-value to plot
           xlab = "Time",
           ylab = "Survival Probability",
           title = "KM Curves by Treatment Group",
           legend.labs = c("Treatment 0", "Treatment 1"),
           surv.median.line = "hv")


# Median survival times with CIs for each group
print(km_fit_both)   # gives median + 95% CI per group

# Log-rank test to compare groups
logrank_test <- survdiff(Surv(futime, fustat) ~ rx, data = ov_data)
print(logrank_test)


# Table 1
descriptive_table <- ov_data %>%
  select(futime, age, ecog.ps, rx, fustat) %>%
  tbl_summary(by = rx,
              missing = "ifany",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              label = list(futime ~ "Survival Time (days after diagnosis)",
                           age ~ "Age (years)",
                           ecog.ps ~ "ECOG Performance Status",
                           fustat ~ "Censored Status")) %>%
  add_overall() %>%
  modify_header(stat_1 ~ "**Treatment 1** <br> N = {n}",
                stat_2 ~ "**Treatment 2** <br> N = {n}")


# Cox PH Model
coxph_model <- coxph(formula = Surv(futime, fustat) ~ rx + age + ecog.ps + resid.ds, data = ov_data)
summary(coxph_model)

