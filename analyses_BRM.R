# ------------------------------------------------------------------------- #
#               ANANYSES INLCUDED IN BRM MANUSCRIPT                         #
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #

library(lme4)
library(simr)
library(mixedpower)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------------------------------------- #
# ----- SCENARIO 1 ------- #
# ------------------------------------------------------------------------- #

#### load and preprocessed data
# --> full analysis and preprocessing script and data can be downloaded here:
# https://tinyurl.com/rq7v496
load("Yan_et_al.RData")


#### rename 
# rename variables for demonstartion reasons
data["word_length"] <- data$wl.c
data["complexity"] <- data$sn.c
data["subject"] <- data$nsub
data["sentence"] <- data$nsen

YanData <- data


#### fit model intended for simulation

FLPmodel <- lmer(flp ~ word_length * complexity + (1|subject) + (1|sentence),
                 data=data)

sink('FLP_summary.txt')

summary(FLPmodel)
sink()


# --------------------- #
#### SIMR ANALYSIS

# powerSim
power_complexity <- powerSim(fit = FLPmodel, test = fixed("complexity"), nsim = 100)

sink("powerSim_output.txt")
print(power_complexity)
sink()

save(power_complexity, file = "simrpower_scenario1_snc.Rdata")

# powerCurve
artificial_pC <- extend(FLPmodel, along="subject", n = 60) # extend model/data

powerC <- powerCurve(artificial_pC, test= fixed("complexity"), along = "subject", breaks =c(20,30,40,60), nsim = 100)
save(power, file = "simr_powerC_scenario1_snc.Rdata")

sink('powerCurve_output.txt')
print(powerC)
sink()

# --------------------- #
#### MIXEDPOWER + SESOI
# --> we can get both results in one simulation

SESOI <- c(3.66, 1, -0.05, 0.05)

# only SESOI, not databased (we already did this in the simulation above)
power_FLP <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "subject", steps = c(20,30,40,50,60),
                        critical_value = 2, n_sim = 1000, 
                        SESOI = SESOI, databased = F)

save(power_FLP, file = "mixedpower_scenario1_SESOI_DB.Rdata")

# get a text file of output to include it in Latex file 
sink("mixedpower_S1.txt")
print(power_FLP)
sink()




# ------------------------------------------------------------------------- #
### ------ SCENARIO 2  ------ ##
# ------------------------------------------------------------------------- #


# ------------------ #
# 1) SIMPLE
#    - only vary sentences (+ SESOI)
power_sentences <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "sentence", steps = c(80,100,120,140,160),
                        critical_value = 2, n_sim = 1000, SESOI = SESOI)

save(power_sentences, file = "mixedpower_scenario2_48subjects.Rdata")


# ----------------- #
# 1) ADVANCED
#    - vary sentences and subject 

# ----- start with 25
power20_sentences <- R2power(model = FLPmodel, data = YanData,
                             fixed_effects = c("word_length", "complexity"),
                             simvar = "sentence", steps = c(80,100,120,140,160),
                             R2var = "subject", R2level = 20,  critical_value = 2, 
                             n_sim = 1000, SESOI = SESOI, databased = T)

save(power20_sentences, file = "mixedpower_scenario2_20subjects.Rdata")

# ---- same with 60
power40_sentences <- R2power(model = FLPmodel_40, data = data,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(80,100,120,140,160),
                                R2var = "subject", R2level = 60, critical_value = 2,
                                n_sim = 1000, SESOI = SESOI, databased = T)

save(power40_sentences, file = "mixedpower_scenario2_60subjects.Rdata")


# ------------------------------------------------------------------------- #
### ------ SCENARIO 3  ------ ##
# ------------------------------------------------------------------------- #

# ------- SETUP -------- # 

formula <- speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word)


# 1: RANDOM EFFECTS
# including variables used as random effects in artificial data 
artificial_data <- expand.grid(Word = (1:100), Subject = (1:20))


# 2. FIXED EFFECTS

#  generate frequency ratings [This step should be replaced with actual ratings!]
frequency_ratings <- runif(100)

# repeat for every subject in data (20 times)
artificial_data["Frequency"] <- sort(rep(frequency_ratings, 20))


# include native language
artificial_data["NativeLanguage"] <- c(rep(-0.5, 1000), rep(0.5, 1000))


# create model 
artificial_lmer <- makeLmer(formula = speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                            fixef = c(1.7, -0.25, 0.02, 0.03), VarCorr = list(0.007, 0.05), 
                            sigma = 0.26, data = artificial_data)


# ------ POWER ------- # 
