

#### Simulation Worksheet 1 ####

# Question 1 - Create 20 simuated datasets from the given linear regression model
simulate_datasets <- function(n_datasets = 1, N = 100,
                              beta0 = 1, beta1 = 1, beta2 = 0.5, beta3 = 0.25,
                              sigma = 1, seed = NULL) {
  
  set.seed(seed)
  
  sim_list <- vector("list", n_datasets)
  
  for (i in 1:n_datasets) {
    
    # Predictors
    X1 <- rbinom(N, 1, 0.5)
    X2 <- rnorm(N, mean = 39, sd = 5)
    X3 <- X1 * X2
    
    # Error
    epsilon <- rnorm(N, mean = 0, sd = sigma)
    
    # Outcome
    Y <- beta0 + beta1*X1 + beta2*X2 + beta3*X3 + epsilon
    
    # Save results
    sim_list[[i]] <- data.frame(Y, X1, X2, X3)
  }
  
  return(sim_list)
}

simulated_datasets <- simulate_datasets(n_datasets = 20, seed = 6624)


# Question 2 - Plot Y vs. X2 for each dataset
library(ggplot2)
library(dplyr)

# Combining all 20 datasets into one data frame
combined_data <- bind_rows(simulated_datasets, .id = "dataset") |>
  mutate(dataset = as.integer(dataset),
         X1_group = factor(X1, levels = c(0, 1), labels = c("X1 = 0", "X1 = 1")))

# Creating plots
ggplot(combined_data, aes(x = X2, y = Y, color = X1_group)) +
  geom_point(alpha = 0.4, size = 0.8) +
  facet_wrap(~ dataset, ncol = 5) +
  scale_color_manual(name = "Group", values = c("X1 = 0" = "darkolivegreen1", "X1 = 1" = "darkorchid1")) +
  labs(title = "Y vs. X2 across 20 simulated datasets",
       x = "X2",
       y = "Y") +
  theme_bw()



# Question 3 - Summarizing operating characteristics
true_params <- c(
  "(Intercept)" = 1,
  "X1"          = 1,
  "X2"          = 0.5,
  "X3"          = 0.25
)

results_list <- vector("list", length(simulated_datasets))

for (i in 1:length(simulated_datasets)) {
  
  fit   <- lm(Y ~ X1 + X2 + X3, data = simulated_datasets[[i]])
  coefs <- summary(fit)$coefficients
  cis   <- confint(fit)
  
  results_list[[i]] <- data.frame(
    dataset  = i,
    term     = rownames(coefs),
    estimate = coefs[, "Estimate"],
    lower    = cis[, 1],
    upper    = cis[, 2],
    p_value  = coefs[, "Pr(>|t|)"]
  )
}

results_df <- bind_rows(results_list)

operating_chars <- results_df |>
  mutate(
    true_value = true_params[term],
    covers     = lower < true_value & true_value < upper,
    reject     = p_value < 0.05
  ) |>
  group_by(term) |>
  summarise(
    true_value = first(true_value),
    mean_est   = mean(estimate),
    bias       = mean(estimate) - first(true_value),
    coverage   = mean(covers),
    power      = mean(reject),
    .groups    = "drop"
  )

print(operating_chars)





#### Simulation Worksheet 2 ####
library(hdrm)
library(corrplot)

# Question 4
testdata <- gen_data(100, 10, 5)

# Part d
data.frame(mean = colMeans(testdata$X),
           sd   = apply(testdata$X, 2, sd))

# Part e
cor_matrix <- cor(testdata$X, use = "complete.obs")
cor_matrix

corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.7,
         tl.col = "black", 
         tl.srt = 45)

# Part f
# Build the design matrix
X_mat <- as.matrix(cbind(1, testdata$X))
y_vec <- testdata$y

# Estimate beta
beta_hat <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% y_vec
beta_hat

# Calculate y_hat
yhat <- X_mat %*% beta_hat

# Calculate residuals
residuals <- y_vec - yhat

# Calculate sigma^2_e
n <- nrow(X_mat)
p <- ncol(X_mat) - 1   # exclude intercept column

sigma2_e <- sum(residuals^2) / (n - p - 1)
cat("sigma^2_e =", sigma2_e, "\n")

# Part g
RSS <- sum(residuals^2)
TSS <- sum((y_vec - mean(y_vec))^2)

R2 <- 1 - (RSS / TSS)

SNR <- R2 / (1 - R2)

cat("R^2 =", R2, "\n")
cat("SNR =", SNR, "\n")




# Question 5
set.seed(6624)
beta <- c(runif(5, min = 0.5, max = 2), rep(0, 5))
new_data <- gen_data(n = 100, p = 10, p1 = 5, beta = beta)

# Part a
data.frame(mean = colMeans(new_data$X),
           sd   = apply(new_data$X, 2, sd))

# Part b
cor_matrix <- cor(new_data$X, use = "complete.obs")
cor_matrix

corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.7,
         tl.col = "black", 
         tl.srt = 45)

# Part c
# Build the design matrix
X_mat <- as.matrix(cbind(1, new_data$X))
y_vec <- new_data$y

# Estimate beta
beta_hat <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% y_vec
beta_hat

# Calculate y_hat
yhat <- X_mat %*% beta_hat

# Calculate residuals
residuals <- y_vec - yhat

# Calculate sigma^2_e
n <- nrow(X_mat)
p <- ncol(X_mat) - 1   # exclude intercept column

sigma2_e <- sum(residuals^2) / (n - p - 1)
cat("sigma^2_e =", sigma2_e, "\n")

# Part d
RSS <- sum(residuals^2)
TSS <- sum((y_vec - mean(y_vec))^2)

R2 <- 1 - (RSS / TSS)

SNR <- R2 / (1 - R2)

cat("R^2 =", R2, "\n")
cat("SNR =", SNR, "\n")


# Question 6
set.seed(6624)
beta <- c(runif(5, min = 0.5, max = 2), rep(0, 5))
new_data <- gen_data(n = 100, p = 10, p1 = 5, beta = beta,
                     corr = "exchangeable", rho = 0.5)

# Part a
data.frame(mean = colMeans(new_data$X),
           sd   = apply(new_data$X, 2, sd))

# Part b
cov_matrix <- cov(new_data$X)
cov_matrix


# Part c
# Build the design matrix
X_mat <- as.matrix(cbind(1, new_data$X))
y_vec <- new_data$y

# Estimate beta
beta_hat <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% y_vec
beta_hat

# Calculate y_hat
yhat <- X_mat %*% beta_hat

# Calculate residuals
residuals <- y_vec - yhat

# Calculate sigma^2_e
n <- nrow(X_mat)
p <- ncol(X_mat) - 1   # exclude intercept column

sigma2_e <- sum(residuals^2) / (n - p - 1)
cat("sigma^2_e =", sigma2_e, "\n")

# Part d
RSS <- sum(residuals^2)
TSS <- sum((y_vec - mean(y_vec))^2)

R2 <- 1 - (RSS / TSS)

SNR <- R2 / (1 - R2)

cat("R^2 =", R2, "\n")
cat("SNR =", SNR, "\n")




#### Simulation Worksheet 3 ####
# Question 3 - Collider Effect
# Function to simulate dataset with collider
simulate_collider <- function(n = 1000) {
  # Exposure (X)
  x <- rnorm(n)
  
  # Outcome (Y) - independent of X
  y <- rnorm(n)
  
  # Collider (Z) - caused by BOTH X and Y
  z <- 0.7*x + 0.7*y + rnorm(n)
  
  data.frame(x = x, y = y, z = z)
}

# Fitting models with and without the collider
set.seed(6624)
dat <- simulate_collider(10000)

# Model without collider
model_no_collider <- lm(y ~ x, data = dat)

# Model with collider
model_with_collider <- lm(y ~ x + z, data = dat)

summary(model_no_collider)
summary(model_with_collider)




