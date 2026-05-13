#### BIOS 6624 Advanced Methods - Project 4 ####
## Madelynn Schina
## May 2026

############################################################
# The following code runs the simulations in parallel across
# multiple cores for the sake of decreasing computation time
############################################################


# Loading necessary packages
library(parallel)
library(doParallel)
library(foreach)


##### Import Simulation Functions ####
proj_dir <- getwd()
source(file.path(proj_dir, "Project 4", "Code", "SimulationFunctions.R"))


#### Defining simWrapper ####
simWrapper <- function(n_sim, f_sim, TF_parallel = F, n_cores = 2, list_export = list(),
                       list_package = c(), f_clusterCall = function(){}, combine = rbind){
  v_seed <- sample.int(.Machine$integer.max, n_sim)
  
  if(TF_parallel){
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, append(list_export, list("f_clusterCall", "list_package")), envir = environment())
    if(length(list_package)>0){
        parallel::clusterEvalQ(cl, lapply(list_package, require, character.only = TRUE))}
    parallel::clusterEvalQ(cl, f_clusterCall())
    output_full <- foreach::foreach(i = 1:n_sim, .combine = combine, .errorhandling = "stop") %dopar% {
                                                                                            set.seed(v_seed[i])                  
                                                                                            f_sim(i)}
  }else{
    output_full <- foreach(i = 1:n_sim, .combine = comine) %do% {
              set.seed(v_seed[i])
              return(f_sim(i))
    }
  }
  return(output_full)
}


#### Running Simulations in Parallel ####
# Configurations
n_sims <- 1000
n_cores <- 6
out_dir <- file.path(getwd(), "Project 4/Data")


# Function to Run Single Simulation
run_sim <- function(i, n_sample, rho){
    results_path <- file.path(out_dir, paste0("sim_n", n_sample, "_rho", rho*100, "_", sprintf("%03d", i), ".rds"))
    
    if(file.exists(results_path)){
        return("AlreadyDone")
    }else{
        output <- simfunc(n = n_sample, rho = rho)
        
        saveRDS(output, results_path)
        return("NewlyDone")
    }
  
}



# Define scenarios
scenarios <- list(s1a = list(n = 250, rho = 0),
                  s1b_35 = list(n = 250, rho = 0.35),
                  s1b_70 = list(n = 250, rho = 0.70),
                  s2a = list(n = 500, rho = 0),
                  s2b_35 = list(n = 500, rho = 0.35),
                  s2b_70 = list(n = 500, rho = 0.70))

# Running each scenario
set.seed(6624)
for (s in names(scenarios)) {
  cat("Running scenario:", s, "\n")
  
  simWrapper(n_sim = n_sims,
             f_sim = function(i) run_sim(i, n_sample = scenarios[[s]]$n, rho = scenarios[[s]]$rho),
             TF_parallel = TRUE,
             n_cores = n_cores,
             list_export = c("out_dir", "scenarios", "run_sim", "s"),
             list_package = c("hdrm", "glmnet"),
             f_clusterCall = function() { source(file.path(getwd(), "Project 4/Code/SimulationFunctions.R")) })
}




