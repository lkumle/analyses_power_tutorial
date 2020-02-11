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
uniform <- runif(100, min = 0, max = 1)
normal <- rnorm(100, mean = 0.5, sd = 0.2)
beta <- rbeta(100, 2,5) # right skewed distribution

distributions <- list(uniform, normal, beta)

# --- EFFECT SIZES ETC --- # 
fixed_effects <- c(1.7, -0.25, 0.02, 0.03)
random_variance <- list(0.007, 0.05)

sigma <- 0.26



# ------------------------------------------------------------------------- #
# loop through differnt distributions and compute power

# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 3))
names(sim_results) <- c("power", "effect", "dist") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 3)
sim_results$dist <- c(rep("uniform",3), rep("normal",3), rep("right_skewed",3))


for (i in 1:length(distributions)){
  
  
  print("new dist")
  # create vector for data set: Multiply by 20 as we have 20 subject in our artificial data set who each respond to all words
  frequency <- rep(distributions[[i]], 20)
  artificial_data["Frequency"] <- frequency # add to data set 
  
  # prepare model 
  artificial_lmer <- makeLmer(formula = Speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                              fixef = fixed_effects, VarCorr = random_variance, sigma = sigma,
                              data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_lmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "Frequency"),
                      simvar = "Subject", steps = c(20),
                      critical_value = 2, n_sim = 5000)
  
  # store results
  sim_results["power"][(3*i-2):(3*i),] <- power$`20`
  
} # end distribution loop 



# ------------------------------------------------------------------------- #
# look at result







