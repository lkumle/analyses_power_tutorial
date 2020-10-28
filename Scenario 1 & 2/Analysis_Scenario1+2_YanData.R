# ------------------------------------------------------------------------- #
#      ANANYSES INLCUDED IN BRM MANUSCRIPT  SCENARIO 1 & 2                 #
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #

library(lme4)
library(simr)
library(mixedpower)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------------------------------------- #
#                       ----- SCENARIO 1 -------
# ------------------------------------------------------------------------- #

#### load and preprocessed data
# --> full analysis and preprocessing script and data can be downloaded here:
# https://tinyurl.com/rq7v496
load("Yan_et_al.RData")

#### fit model intended for simulation

FLPmodel <- lmer(flp ~ word_length * complexity + (1|subject) + (1|sentence),
                 data=YanData)

summary(FLPmodel)


# ------------------------------ #
# MIXEDPOWER + SESOI

# determine SESOI: 
FLPmodel@beta * 0.85 # check what 0.15 smaller looks like

SESOI <- c(3.66, 0.13, -0.06, 0.09) # keep intercept the same 

# only SESOI, not databased (we already did this in the simulation above)
power_FLP <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "subject", steps = c(20,30,40,50,60),
                        critical_value = 2, n_sim = 1000, 
                        SESOI = SESOI, databased = T)

save(power_FLP, file = "mixedpower_scenario1_SESOI_DB.Rdata")

multiplotPower(power_FLP, filename = "mixedpower_S1.png")


# ------------------------------------------------------------------------- #
#                       ----- SCENARIO 2 -------
# ------------------------------------------------------------------------- #

# ------------------ #
# 1) SIMPLE
#    - only vary sentences (+ SESOI)
power_sentences <- mixedpower(model = FLPmodel, data = YanData,
                              fixed_effects = c("word_length", "complexity"),
                              simvar = "sentence", steps = c(100,120,140,160, 180),
                              critical_value = 2, n_sim = 1000, SESOI = SESOI)

save(power_sentences, file = "mixedpower_scenario2_48subjects.Rdata")


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



