# ------------------------------------------------------------------------- #
#               SCENARIO 3: SIMULATING DIFFERNT ASSUMPTIONS                 #
# ------------------------------------------------------------------------- #

rm(list = ls())

library(simr)
library(mixedpower)

# ------------------------------------------------------------------------- #
# prepare SCENARIO 3: 

# --- RANDOM EFFECTS --- # 

# combine them in one data set
artificial_data <- expand.grid(Word = (1:100), Subject = (1:20))

# --- FIXED EFFECTS --- # 
artificial_data["NativeLanguage"] <- c(rep(-0.5, 1000), rep(0.5, 1000)) 

# create different frequency ratings to loop through later
normal1 <- rnorm(100, mean = 5, sd = 1)
normal2 <- rnorm(100, mean = 0.5, sd = 0.1)
normal3 <- rnorm(100, mean = 2.5, sd = 0.5)


distributions <- list(normal1, normal2, normal3)

# --- EFFECT SIZES ETC --- # 
fixed_effects <- c(1.7, -0.25, 0.02, 0.03)
random_variance <- list(0.007, 0.05)

sigma <- 0.26



# ------------------------------------------------------------------------- #
# loop through differnt distributions and compute power
# ------------------------------------------------------------------------- #

# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 7))
names(sim_results) <- c("20", "30" ,"40","50", "60", "effect", "dist") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 3)
sim_results$dist <- c(rep("normal1",3), rep("normal2",3), rep("normal3",3))


for (i in 1:length(distributions)){
  
  
  print("new dist")
  # create vector for data set: Multiply by 20 as we have 20 subject in our artificial data set who each respond to all words
  frequency <- rnorm(100, mean = 5, sd = 2)
  artificial_data["Frequency"] <- frequency # add to data set 
  
  # prepare model 
  artificial_lmer <- makeLmer(formula = Speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                              fixef = fixed_effects, VarCorr = random_variance, sigma = sigma,
                              data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_lmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "Frequency"),
                      simvar = "Subject", steps = c(20,30,40,50,60),
                      critical_value = 2, n_sim = 5000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`20`
  sim_results["30"][(3*i-2):(3*i),] <- power$`30`
  sim_results["40"][(3*i-2):(3*i),] <- power$`40`
  sim_results["50"][(3*i-2):(3*i),] <- power$`50`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  
} # end distribution loop 

# save results to analyse later 
save(sim_results, file = "simulations_distributions_S3.Rdata")


# ------------------------------------------------------------------------- #
# look at result

# ------------------------------------------------------------------------- #
# loop through differnt sigmas
# ------------------------------------------------------------------------- #

# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 12, ncol = 7))
names(sim_results) <- c("20", "30" ,"40","50", "60", "effect", "sigma") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 4)
sim_results$dist <- c(rep(0.05,3), rep(0.26 ,3), rep(1,3), rep(5,3))

# set distributions to the one used in example
# create vector for data set: Multiply by 20 as we have 20 subject in our artificial data set who each respond to all words
frequency <- rep(rnorm(100, mean = 5, sd = 1), 20)
artificial_data["Frequency"] <- frequency # add to data set 


# sigmas to try 
sigmas <- c(0.01, 0.26, 1, 5)

for (i in 1:length(sigmas)){
  
  
  # get sigma
  sigma <- sigmas[i]
  
  # prepare model 
  artificial_lmer <- makeLmer(formula = Speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                              fixef = fixed_effects, VarCorr = random_variance, sigma = sigma,
                              data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_lmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "Frequency"),
                      simvar = "Subject", steps = c(20,30,40,50,60),
                      critical_value = 2, n_sim = 5000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`20`
  sim_results["30"][(3*i-2):(3*i),] <- power$`30`
  sim_results["40"][(3*i-2):(3*i),] <- power$`40`
  sim_results["50"][(3*i-2):(3*i),] <- power$`50`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  
} # end distribution loop 

# save results to analyse later 

save(sim_results, file = "simulations_sigmas_S3.Rdata")


# ------------------------------------------------------------------------- #
# loop through differnt clusters
# ------------------------------------------------------------------------- #

# set sigma back to normal
sigma <- 0.26


# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 7))
names(sim_results) <- c("20", "30" ,"40","50", "60", "effect", "balance") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 4)
sim_results$dist <- c(rep("80/20",3), rep("65/35" ,3), rep("50/50",3))

# vary balance
balance <- c(0.8, 0.65, 0.5)
 

for (i in 1:length(balance)){
  
  n <- 2000*balance[i]
  n2 <- 2000- n
  # get sigma
  artificial_data["NativeLanguage"] <- c(rep(-0.5, n), rep(0.5, n2))
  
  # prepare model 
  artificial_lmer <- makeLmer(formula = Speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                              fixef = fixed_effects, VarCorr = random_variance, sigma = sigma,
                              data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_lmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "Frequency"),
                      simvar = "Subject", steps = c(20,30,40,50,60),
                      critical_value = 2, n_sim = 5000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`20`
  sim_results["30"][(3*i-2):(3*i),] <- power$`30`
  sim_results["40"][(3*i-2):(3*i),] <- power$`40`
  sim_results["50"][(3*i-2):(3*i),] <- power$`50`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  
} # end distribution loop 


save(sim_results, file = "simulations_clusters_S3.Rdata")
