#### BIOS 6624 Advanced Methods - Project 4 ####
## Madelynn Schina
## May 2026

###########################################################
# The following code is defined to run the variable 
# selection simulations required for this project
###########################################################


# Notes
# "exchangable" correlation; 0, 0.35, 0.7


# Loading necessary packages
library(hdrm)
library(glmnet)




# Defining two functions to extract results
harvest <- function(mod_obj, param, alpha = 0.05) {
  # Extract model results
  estimates <- as.data.frame(summary(mod_obj)$coefficients) 
  estimates <- within(estimates, {param <- row.names(estimates)})
  CIs <- as.data.frame(confint.default(mod_obj, level = 1-alpha))
  names(CIs) <- c('LCL','UCL')
  CIs <- within(CIs, {param <- row.names(CIs)})
  
  var_names <- sprintf("V%02d", 1:length(param))
  
  # Summarize
  res_dat <- data.frame(variables = var_names, true_values = param) |>
    within({selected <- ifelse(variables %in% row.names(estimates), 1, 0)
            signif <- ifelse(variables %in% row.names(estimates[estimates[,4] < alpha,]), 1, 0)
            true_non_zero <- ifelse(variables %in% sprintf("V%02d", 1:5), 1, 0)}) |>
    merge(CIs, by.x = 'variables', by.y = 'param', all.x = T) |>
    within({covered <- ifelse(LCL <= true_values & true_values <= UCL, 1, 0)}) |>
    merge(estimates[, c('param', 'Estimate')],   # <-- added for bias
          by.x = 'variables',
          by.y = 'param',
          all.x = TRUE)
  
  names(res_dat)[names(res_dat) == 'Estimate'] <- 'estimate'
  
  # Fix non-selected for coverage
  res_dat[is.na(res_dat$covered) == T & res_dat$selected == 0 & res_dat$true_non_zero == 1,'covered'] <- 0 
  res_dat[is.na(res_dat$covered) == T & res_dat$selected == 0 & res_dat$true_non_zero == 0,'covered'] <- 1 
  
  return(res_dat)
}


harvest_glmnet <- function(cv_fit, lambda, param, alpha = 0.05) {
  var_names <- sprintf("V%02d", 1:length(param))
  
  # Extract coefficients at chosen lambda (drop intercept)
  coefs <- coef(cv_fit, s = lambda)
  coefs <- as.matrix(coefs)[-1, , drop = FALSE]
  
  selected <- which(coefs != 0)
  
  res_dat <- data.frame(variables = var_names,
                        true_values = param,
                        true_non_zero = ifelse(var_names %in% sprintf("V%02d", 1:5), 1, 0),
                        selected = ifelse(1:length(param) %in% selected, 1, 0),
                        signif = ifelse(1:length(param) %in% selected, 1, 0),
                        estimate = coefs[, 1],
                        LCL = NA,
                        UCL = NA,
                        covered = NA)
  
  res_dat[res_dat$selected == 0 & res_dat$true_non_zero == 1, 'covered'] <- 0
  res_dat[res_dat$selected == 0 & res_dat$true_non_zero == 0, 'covered'] <- 1
  
  return(res_dat)
}





# Defining simulation function: data generation and variable selection
simfunc <- function(n, rho = 0) {
  
  # Defining nonzero coefficients as required in project description
  param <- c(1/6, 1/3, 1/2, 2/3, 5/6, rep(0, 15))
  
  # Generating Data using function from hdrm package
  data_obj <- gen_data(n = n, 
                       p = 20, 
                       p1 = 5, 
                       beta = param, 
                       family = 'gaussian', 
                       corr = 'exchangeable', 
                       rho = rho) 
  data <- data.frame(y = data_obj$y, data_obj$X)
  
  # Prepare matrix form for glmnet 
  X_mat <- as.matrix(data_obj$X)
  Y_vec <- data_obj$y
  
  # Fitting model for stepwise methods
  fitted_model <- lm(y ~ ., data = data)
  
  
  #### P-Value Method ####
  pval_redux <- step(fitted_model, 
                     direction = 'backward',
                     trace = 0,
                     k = qchisq(1-0.05, 1))
  
  pval_res <- harvest(mod_obj = pval_redux, param = param, alpha = 0.05)
  pval_res$method <- 'PVAL'
  pval_res$n <- n
  pval_res$rho <- rho
  
  
  #### AIC Method ####
  AIC_redux <- step(fitted_model,
                    direction = 'backward',
                    trace = 0,
                    k = 2)
  
  AIC_res <- harvest(mod_obj = AIC_redux, param = param, alpha = 0.05)
  AIC_res$method <- 'AIC'
  AIC_res$n <- n
  AIC_res$rho <- rho
  
  
  #### BIC Method ####
  BIC_redux <- step(fitted_model,
                    direction = 'backward',
                    trace = 0,
                    k = log(n))
  
  BIC_res <- harvest(mod_obj = BIC_redux, param = param, alpha = 0.05)
  BIC_res$method <- 'BIC'
  BIC_res$n <- n
  BIC_res$rho <- rho
  
  
  #### LASSO Method ####
  cv_lasso <- cv.glmnet(X_mat, Y_vec, alpha = 1, nfolds = 10)
  
  # lambda.min
  lasso_min_redux <- cv_lasso$glmnet.fit
  lasso_min_res <- harvest_glmnet(cv_fit = cv_lasso, lambda = cv_lasso$lambda.min, param = param)
  lasso_min_res$method <- 'LASSO_min'
  lasso_min_res$n <- n
  lasso_min_res$rho <- rho
  
  # lambda.1se
  lasso_1se_res <- harvest_glmnet(cv_fit = cv_lasso, lambda = cv_lasso$lambda.1se, param = param)
  lasso_1se_res$method <- 'LASSO_1se'
  lasso_1se_res$n <- n
  lasso_1se_res$rho <- rho
  
  
  #### Elastic Net Method ####
  # Note that alpha is set to 0.5 a priori to balance the penalties
  cv_enet <- cv.glmnet(X_mat, Y_vec, alpha = 0.5, nfolds = 10)
  
  # lambda.min
  enet_min_res <- harvest_glmnet(cv_fit = cv_enet, lambda = cv_enet$lambda.min, param = param)
  enet_min_res$method <- 'ENET_min'
  enet_min_res$n <- n
  enet_min_res$rho <- rho
  
  # lambda.1se
  enet_1se_res <- harvest_glmnet(cv_fit = cv_enet, lambda = cv_enet$lambda.1se, param = param)
  enet_1se_res$method <- 'ENET_1se'
  enet_1se_res$n <- n
  enet_1se_res$rho <- rho
  
  
  #### Combining results from all methods ####
  common_cols <- c('variables', 'true_values', 'true_non_zero', 'selected', 'signif', 'estimate', 'LCL', 'UCL', 'covered', 'method', 'n', 'rho')
  
  pval_res <- pval_res[, common_cols]
  AIC_res <- AIC_res[, common_cols]
  BIC_res <- BIC_res[, common_cols]
  lasso_min_res <- lasso_min_res[, common_cols]
  lasso_1se_res <- lasso_1se_res[, common_cols]
  enet_min_res <- enet_min_res[, common_cols]
  enet_1se_res <- enet_1se_res[, common_cols]
  
  return(rbind(pval_res, AIC_res, BIC_res, lasso_min_res, lasso_1se_res, enet_min_res, enet_1se_res))
  
}




