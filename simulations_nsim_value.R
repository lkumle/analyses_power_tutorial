# ------------------------------------------------------------------------- #
#               SIMULATIONS INLCUDED IN BRM MANUSCRIPT                      #
# ------------------------------------------------------------------------- #

library(lme4)
library(mixedpower)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ------------------------------------------------------------------------- #
# NUMBER OF SIMULATIONS IN MIXEDPOWER

# repeat simulations 20 times with different nsim 
# --> record time to see how fast this is


# ------------------------- #
# PREPARE


# nsims to test: 
nsims <- c(100, 1000, 2500, 5000)

# store run times
run_times <- matrix(rep(NA, 20*4), nrow = 20, ncol = 4) # empty matrix
colnames(run_times) <- nsims

# store power results
sim_results <- data.frame(matrix(nrow = 3*20, ncol = 5))
names(sim_results) <- c(100, 1000, 2500, 5000, "effect") 
sim_results$effect <- rep(c("word_length","complexity","word_length:complexity"), 20)  # store effect names



# load stuff
load("Yan_et_al.RData")
data["word_length"] <- data$wl.c
data["complexity"] <- data$sn.c
data["subject"] <- data$nsub
data["sentence"] <- data$nsen

FLPmodel <- lmer(flp ~ word_length * complexity + (1|subject) + (1|sentence),
                 data=data)

# ------------------------- #
# RUN SIMULATIONS

# loop through all nsims
for(nsim in nsims){

  # repeat simulation 20 times 
  for (i in 1:20){
    
    # record time
    start <- Sys.time()
    
    # run simulation 
    power <- mixedpower(model = FLPmodel, data = data,
                            fixed_effects = c("word_length", "complexity"),
                            simvar = "subject", steps = c(48), # only simulate power for number of participants in data
                            critical_value = 2, n_sim = nsim)
    
    # store run times
    run_times[i,as.character(nsim)] <- Sys.time() - start
    
    sim_results[as.character(nsim)][(3*i-2):(3*i),] <- power$`48`
    
    
  } # end inner for loop 
} # end outer for loop 

# save stuff just to be sure!
save(sim_results, file = "nsim_simulations.Rdata")

save(run_times, file = "run_times.Rdata")

# ------------------------- #
# ANALYSE RESULTS

# --------- runtimes ------ # 
t_sumstats <- run_times %>% 
  as_tibble() %>%
  summarise(
    m100 = mean(`100`, na.rm = T),
    m1000 = mean(`1000`, na.rm = T),
    m2500 = mean(`2500`, na.rm = T),
    m5000 = mean(`5000`, na.rm = T),)

# --------- power --------- # 

# get mean for different nsims
nsim_sumstats <- sim_results %>% 
  group_by(effect) %>%
  summarise(
    m100 = mean(`100`, na.rm = T), 
    m1000 = mean(`1000`, na.rm = T),
    m2500 = mean(`2500`, na.rm = T),
    m5000 = mean(`5000`, na.rm = T),)

# get sd/var/range for different nsims 
nsim_sumstats <- sim_results %>% 
  group_by(effect) %>%
  summarise(
    m100 = sd(`100`, na.rm = T), 
    m1000 = sd(`1000`, na.rm = T),
    m2500 = sd(`2500`, na.rm = T),
    m5000 = sd(`5000`, na.rm = T),)