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


# --------------------- #
#### MIXEDPOWER

power_FLP <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "subject", steps = c(20,30,40,50,60),
                        critical_value = 2, n_sim = 1000)

save(power_FLP, file = "mixedpower_scenario1.Rdata")

sink("mixedpower_S1.txt")
print(power_FLP)
sink()

multiplotPower(power_FLP)

# --------------------- #
#### SESOI

# only SESOI, not databased (we already did this in the simulation above)
SESOI <- c(3.66, 1, -0.05, 0.05)

power_SESOI <- mixedpower(model = FLPmodel_SESOI, data = YanData,
                          fixed_effects = c("word_length", "complexity"),
                          simvar = "subject", steps = c(20,30,40,50,60),
                          critical_value = 2, n_sim = 1000, SESOI = SESOI, 
                          databased = F)

save(power_SESOI, file = "mixedpower_SESOI_scenario1.Rdata")


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
                             R2var = "subject", R2level = 25, critical_value = 2,
                             n_sim = 1000, SESOI = F, databased = T)

save(power20_sentences, file = "mixedpower_scenario2_20subjects.Rdata")

# ---- same with 40
power40_sentences <- R2power(model = FLPmodel_40, data = data,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(80,100,120,140,160),
                                R2var = "subject", R2level = 40, critical_value = 2,
                                n_sim = 1000, SESOI = F, databased = T)

save(power40_sentences, file = "mixedpower_scenario2_40subjects.Rdata")

# ------------------------------------------------------------------------- #
### ------ SCENARIO 3  ------ ##
# ------------------------------------------------------------------------- #

# prepare simulating 
formula <- speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word)

# 1. create subject variable.
# We will start with 20 subjects - changes in the number of subjects can be done later
subject <- (1:20)

# 2. create word variable.
# We decided to include 100 words in our study
word <- (1:100)

# combine them in one data set
artificial_data <- expand.grid(Word = word, Subject = subject)



# ------------------------------------------------------------------------- #
### ------ SLIGHT CHANGES IN MODELS  ------ ##
# ------------------------------------------------------------------------- #

# somehow simulate slight changes in models

# take FLPmodel, simulate 
