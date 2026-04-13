
library(here)
library(survival)
library(survminer)
library(gtsummary)

## Survival Worksheet 1
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







## Survival Worksheet 3
# Loading the data
cross_data <- read.csv(here("Worksheets", "CrossData.csv"))


# Plotting log-minus-log survival curves
surv_dig <- survfit(Surv(time, status) ~ group, data = cross_data)
ggsurvplot(surv_dig, conf.int=F, censor=F, fun="cloglog")


# Calculating and plotting Schoenfeld residuals for the group variable
res.cox1 <- coxph(Surv(time, status)~ group, data = cross_data)
ph_res <- cox.zph(res.cox1)
ggcoxzph(ph_res, se=F, var = "group")


# Fitting Cox PH Model with group
coxph_fit_group <- coxph(Surv(time, status) ~ group,
                   data = cross_data)
summary(coxph_fit_group)


# Fitting Cox PH Model with interaction between group and time
coxph_fit_interact <- coxph(Surv(time, status) ~ group + tt(group),
                            data = cross_data,
                            tt = function(x, t, ...) x * t)
summary(coxph_fit_interact)


# Fitting Cox PH Model with interaction between group and dichotomous time
coxph_fit_ditime <- coxph(Surv(time, status) ~ group + tt(group),
                          data = cross_data,
                          tt = function(x, t, ...) x * ifelse(t >= 1.5, 1, 0))
summary(coxph_fit_ditime)


## Survival Worksheet 4
# Loading the data on ovarian cancer
ov_data <- read.csv(here("Worksheets", "ovarian.csv"))

# Plots to check PH assumptions
cox_model <- coxph(Surv(futime, fustat) ~ rx + age + resid.ds + ecog.ps, data = ov_data)
ph_test <- cox.zph(cox_model)
ggcoxzph(ph_test)

# Summarizing cox model
summary(cox_model)


# Kaplan Meier Plot
surv_km <- survfit(Surv(futime, fustat) ~ rx, data = ov_data)
ggsurvplot(surv_km, data = ov_data, pval = TRUE,
           legend.labs = c("Treatment 1", "Treatment 2"),
           title = "Kaplan-Meier Survival Curves by Treatment")

# Estimating and plotting the survival curves for both treatment groups
ref_data <- data.frame(rx = c(1, 2),
                       age = mean(ov_data$age),
                       resid.ds = 1,
                       ecog.ps = 1)

surv_cox <- survfit(cox_model, newdata = ref_data)
ggsurvplot(surv_cox, data = ref_data, conf.int = FALSE)


# Assessing different ECOG and residual disease factors
risk_profiles <- data.frame(rx = c(1, 1, 1, 1),
                            age = mean(ov_data$age),
                            resid.ds = c(1, 1, 2, 2),
                            ecog.ps = c(1, 2, 1, 2))


surv_profiles <- survfit(cox_model, newdata = risk_profiles)
summary(surv_profiles, times = 365)

ggsurvplot(surv_profiles, data = risk_profiles,
           legend.labs = c("resid.ds=1, ecog.ps=1",
                           "resid.ds=1, ecog.ps=2",
                           "resid.ds=2, ecog.ps=1",
                           "resid.ds=2, ecog.ps=2"),
           conf.int = FALSE)


# Survival Curves of two individual patients
two_patients <- data.frame(rx = c(1, 2),
                           age = c(60, 55),
                           resid.ds = c(1, 2),
                           ecog.ps = c(1, 2))

surv_patients <- survfit(cox_model, newdata = two_patients)

ggsurvplot(surv_patients, data = two_patients,
           conf.int = FALSE,
           legend.labs = c("Patient 1", "Patient 2"))

# Probability of death within 1, 1.5 and 2 years
summary_times <- summary(surv_patients, times = c(365, 547.5, 730))
death_probs <- data.frame(Patient = c("Patient 1", "Patient 1", "Patient 1", "Patient 2", "Patient 2", "Patient 2"),
                          Time = c("1 Year", "1.5 Years", "2 Years", "1 Year", "1.5 Years", "2 Years"),
                          Prob_Death = round(1 - summary_times$surv, 3))
death_probs

