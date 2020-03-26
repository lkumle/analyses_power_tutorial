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


# --------------------- #
#### MIXEDPOWER + SESOI
# --> we can get both results in one simulation

SESOI <- c(3.66, 0.75, -0.065, 0.09)

# only SESOI, not databased (we already did this in the simulation above)
power_FLP <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "subject", steps = c(20,30,40,50,60),
                        critical_value = 2, n_sim = 1000, 
                        SESOI = SESOI, databased = T)

save(power_FLP, file = "mixedpower_scenario1_SESOI_DB.Rdata")


multiplotPower(power_FLP, filename = "mixedpower_S1.png")


# ------------------------------------------------------------------------- #
### ------ SCENARIO 2  ------ ##
# ------------------------------------------------------------------------- #


# ------------------ #
# 1) SIMPLE
#    - only vary sentences (+ SESOI)
power_sentences <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "sentence", steps = c(100,120,140,160, 180),
                        critical_value = 2, n_sim = 1000, SESOI = SESOI)

save(power_sentences, file = "mixedpower_scenario2_48subjects.Rdata")

# ----- confirm with R2: should be very similar!
power48_sentences <- R2power(model = FLPmodel, data = YanData,
                             fixed_effects = c("word_length", "complexity"),
                             simvar = "sentence", steps = c(100,120,140,160, 180),
                             R2var = "subject", R2level = 48,  critical_value = 2, 
                             n_sim = 1000, SESOI = SESOI, databased = T)

save(power48_sentences, file = "R2_S2_48subjects.Rdata")

# ----------------- #
# 1) ADVANCED
#    - vary sentences and subject 

# ----- start with 30
power30_sentences <- R2power(model = FLPmodel, data = YanData,
                             fixed_effects = c("word_length", "complexity"),
                             simvar = "sentence", steps = c(100,120,140,160,180),
                             R2var = "subject", R2level = 30,  critical_value = 2, 
                             n_sim = 1000, SESOI = SESOI, databased = T)

save(power20_sentences, file = "R2_S2_30subjects.Rdata")

# ---- same with 60
power60_sentences <- R2power(model = FLPmodel, data = YanData,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(100,120,140,160, 180),
                                R2var = "subject", R2level = 60, critical_value = 2,
                                n_sim = 1000, SESOI = SESOI, databased = T)

save(power60_sentences, file = "R2_S2_60subjects.Rdata")


# ------------------------------------------------------------------------- #
### ------ SCENARIO 3  ------ ##
# ------------------------------------------------------------------------- #

# ------- SETUP -------- # 

formula <- speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word)


# 1: RANDOM EFFECTS
# including variables used as random effects in artificial data 
artificial_data <- expand.grid(Word = (1:100), Subject = (1:20))


# 2. FIXED EFFECTS

#  generate frequency ratings 
frequency_ratings <- rnorm(100, mean = 5, sd = 1)

# repeat for every subject in data (20 times)
artificial_data["Frequency"] <- rep(frequency_ratings, 20)


# include native language
artificial_data["NativeLanguage"] <- c(rep(-0.5, 1000), rep(0.5, 1000))


# create model 
artificial_lmer <- makeLmer(formula = speed ~ NativeLanguage * Frequency + (1 | Subject) + (1 | Word),
                            fixef = c(1.7, -0.25, 0.02, 0.03), VarCorr = list(0.007, 0.05), 
                            sigma = 0.26, data = artificial_data)


# ------ POWER ------- # 

# SIMR 
power_simr <- powerSim(fit = artificial_lmer, test = fixed("NativeLanguage"), nsim = 100)

save(power_simr,  file = "S3_simr_NL.Rdata")

# mixedpower

power <- mixedpower(model = FLPmodel, data = YanData,
                       fixed_effects = c("word_length", "complexity"),
                       simvar = "subject", steps = c(20,30,40,50,60),
                       critical_value = 2, n_sim = 1000, 
                       SESOI = c(1.7, -0.2, 0.017, 0.027))

multiplotPower(power)

save(power_S3, file = "S3_mixedpower.Rdata")
