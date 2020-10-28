# ------------------------------------------------------------------------- #
#      ANANYSES INLCUDED IN BRM MANUSCRIPT  SCENARIO 3                      #
# ------------------------------------------------------------------------- #

library(lme4)
library(simr)
library(mixedpower)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --------------------------------------------------------------------------- #
# ------------------------------ lexdec data frame  ---------------------------
library(languageR)
data <- lexdec

# sum code native language
data$Frequency <- scale(data$Frequency, scale = F)
model <- glmer(Correct ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word), 
               family = "binomial", data = data)

summary(model)


# --------------------------------------------------------------------------- #
# ---------------- general simulation for tutorial ---------------------------

formula <- Correct ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word)


# 1: RANDOM EFFECTS
# including variables used as random effects in artificial data 
artificial_data <- expand.grid(Subject = (1:20), Word = (1:100))


# 2. FIXED EFFECTS

#  generate frequency ratings 
frequency_ratings <- rnorm(100, mean = 5, sd = 1)

# repeat for every subject in data (20 times)
artificial_data["Frequency"] <- rep(frequency_ratings, 20)
artificial_data$ScaledFrequency <- scale(artificial_data$Frequency, scale = F)


# include native language
artificial_data["NativeLanguage"] <- c(rep(-0.5, 1000), rep(0.5, 1000))


# create model 
artificial_glmer <- makeGlmer(formula = Correct ~ NativeLanguage * ScaledFrequency + (1 | Subject) + (1 | Word),
                              fixef = c(-4.3, 0.35, -0.4, -0.32), VarCorr = list(1.04, 0.65), 
                              family = "binomial",  data = artificial_data)

summary(artificial_glmer)
## power analysis: 

# simr
power_simr <- powerSim(fit = artificial_glmer, test = fixed("NativeLanguage"), nsim = 1000)

save(power_simr,  file = "S3_simr_NL_glmer.Rdata")

# mixedpwoer

power_S3 <- mixedpower(model = artificial_glmer, data = artificial_data,
                       fixed_effects = c("NativeLanguage", "ScaledFrequency"),
                       simvar = "Subject", steps = c(20,60,100,140,180),
                       critical_value = 2, n_sim = 1000, 
                       SESOI = c(-4.3,  0.30 ,-0.34, -0.27))


multiplotPower(power_S3)

save(power_S3, file = "S3_mixedpower_glmer.Rdata")


# --------------------------------------------------------------------------- #
# ------------------------------ Figure 6- -------- ------------------------- #


# ------------------------------------------------------------------------- #
# loop through differnt distributions and compute power
# ------------------------------------------------------------------------- #
# create different frequency ratings to loop through later
normal1 <- rnorm(100, mean = 5, sd = 1)
normal2 <- rnorm(100, mean = 5, sd = 5)
normal3 <- rnorm(100, mean = 5, sd = 10)


distributions <- list(normal1, normal2, normal3)

# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 7))
names(sim_results) <- c("20", "60" ,"100", "140", "180","effect", "dist") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 3)
sim_results$dist <- c(rep("normal1",3), rep("normal2",3), rep("normal3",3))


for (i in 1:length(distributions)){
  
  
  print("new dist")
  # create vector for data set: Multiply by 20 as we have 20 subject in our artificial data set who each respond to all words
  frequency <- distributions[[i]]
  artificial_data["Frequency"] <- rep(frequency, 20) # add to data set 
  artificial_data$ScaledFrequency <- scale(artificial_data$Frequency, scale = F)
  
  # prepare model 
  artificial_glmer <- makeGlmer(formula = Correct ~ NativeLanguage * ScaledFrequency + (1 | Subject) + (1 | Word),
                                fixef = c(-4.3, 0.36, -0.48, -0.39), VarCorr = list(1.04, 0.65), 
                                family = "binomial",  data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_glmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "ScaledFrequency"),
                      simvar = "Subject", steps = c(20,60,100,140,180),
                      critical_value = 2, n_sim = 1000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`20`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  sim_results["100"][(3*i-2):(3*i),] <- power$`100`
  sim_results["140"][(3*i-2):(3*i),] <- power$`140`
  sim_results["180"][(3*i-2):(3*i),] <- power$`180`
  
} # end distribution loop 

# save results to analyse later 
save(sim_results, file = "simulations_distributions_S3_glmer.Rdata")



# ------------------------------------------------------------------------- #
# loop through differnt clusters
# ------------------------------------------------------------------------- #

# set frequency back to normal
frequency_ratings <- rnorm(100, mean = 5, sd = 1)

# repeat for every subject in data (20 times)
artificial_data["Frequency"] <- rep(frequency_ratings, 20)
artificial_data$ScaledFrequency <- scale(artificial_data$Frequency, scale = F)


# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 7))
names(sim_results) <- c("20", "60" ,"100", "140", "180","effect", "balance") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 3)
sim_results$balance <- c(rep("80/20",3), rep("65/35" ,3), rep("50/50",3))

# vary balance
balance <- c(0.8, 0.65, 0.5)


for (i in 1:length(balance)){
  
  n <- 2000*balance[i]
  n2 <- 2000- n
  # get sigma
  artificial_data["NativeLanguage"] <- c(rep(-0.5, n), rep(0.5, n2))
  
  # prepare model 
  # prepare model 
  artificial_glmer <- makeGlmer(formula = Correct ~ NativeLanguage * ScaledFrequency + (1 | Subject) + (1 | Word),
                                fixef = c(-4.3, 0.36, -0.48, -0.39), VarCorr = list(1.04, 0.65), 
                                family = "binomial",  data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_glmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "ScaledFrequency"),
                      simvar = "Subject", steps = c(20,60,100,140,180),
                      critical_value = 2, n_sim = 1000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`40`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  sim_results["100"][(3*i-2):(3*i),] <- power$`100`
  sim_results["140"][(3*i-2):(3*i),] <- power$`140`
  sim_results["180"][(3*i-2):(3*i),] <- power$`180`
  
} # end distribution loop 


save(sim_results, file = "simulations_clusters_S3_glmer.Rdata")



# ------------------------------------------------------------------------- #
# loop through differnt random variances
# ------------------------------------------------------------------------- #

# set balance back to normal
# include native language
artificial_data["NativeLanguage"] <- c(rep(-0.5, 1000), rep(0.5, 1000))


# prepare storing power output 
sim_results <- data.frame(matrix(nrow = 9, ncol = 7))
names(sim_results) <- c("20", "60" ,"100", "140", "180","effect", "randvar") 
sim_results$effect <- rep(c("NativeLanguage","Frequency","NativeLanguage:Frequency"), 3)
sim_results$randvar <- c(rep("half",3), rep("original" ,3), rep("double",3))

# vary random variance
# different variances
var_word <- c(0.0035,0.007,0.014)
var_sub <- c(0.025, 0.05, 0.1)

rand_vars <- data.frame(var_word, var_sub)



for (i in 1:3){
  
  
  # get variance
  random_variance <-  list(as.numeric(rand_vars[i,1]),as.numeric(rand_vars[i,2]))
  
  # prepare model 
  artificial_glmer <- makeGlmer(formula = Correct ~ NativeLanguage * ScaledFrequency + (1 | Subject) + (1 | Word),
                                fixef = c(-4.3, 0.36, -0.48, -0.39), VarCorr = random_variance, 
                                family = "binomial",  data = artificial_data)
  
  # compute power
  power <- mixedpower(model = artificial_glmer, data = artificial_data,
                      fixed_effects = c("NativeLanguage", "ScaledFrequency"),
                      simvar = "Subject", steps = c(20,60,100,140,180),
                      critical_value = 2, n_sim = 1000)
  
  # store results
  sim_results["20"][(3*i-2):(3*i),] <- power$`40`
  sim_results["60"][(3*i-2):(3*i),] <- power$`60`
  sim_results["100"][(3*i-2):(3*i),] <- power$`100`
  sim_results["140"][(3*i-2):(3*i),] <- power$`140`
  sim_results["180"][(3*i-2):(3*i),] <- power$`180`
  
} # end distribution loop 


save(sim_results, file = "simulations_randvarss_S3_glmer.Rdata")



# --------------------------------------------------------------------------- #
# ---------------------------- Notebook ------------------------------------ #

# create lmer: 
sigma <- 0.26

# ------------------------------------------ #
# CREATE LMER
artificial_lmer <- makeLmer(formula = Speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                            fixef = fixed_effects, VarCorr = random_variance, sigma = sigma,
                            data = artificial_data)

# lets have a look!
summary(artificial_lmer)


# power analysis:
power_simr <- powerSim(artificial_lmer, test= fixed("NativeLanguage"))

# ------------------------------------------ #
# RUN SIMULATION WITH MIXEDPOWER
power <- mixedpower(model = FLPmodel, data = YanData,
                    fixed_effects = c("word_length", "complexity"),
                    simvar = "subject", steps = c(20,30,40,50,60,70,80),
                    critical_value = 2, n_sim = 1000,
                    SESOI = c(1.7, -0.2, 0.017, 0.027))

# ------------------------------------------ #
# PLOT THE RESULTS
multiplotPower(power)