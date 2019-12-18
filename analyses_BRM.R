# ------------------------------------------------------------------------- #
#               ANANYSES INLCUDED IN BRM MANUSCRIPT                         #
# ------------------------------------------------------------------------- #

library(lme4)
library(simr)
library(mixedpower)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------------------------------------- #
# ----- SCENARIO 1 ------- #
# ------------------------------------------------------------------------- #

#### load and preprocessed data
# --> full analysis and preprocessing script and data can be downloaded here: >>> include link <<<
load("Yan_et_al.RData")



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
#### simr analysis

# powerSim
power_complexity <- powerSim(fit = FLPmodel, test = fixed("complexity"), nsim = 100)

save(power_complexity, file = "simrpower_scenario1_snc.Rdata")

# powerCurve
artificial_pC <- extend(FLPmodel, along="subject", n = 60) # extend model/data

powerC <- powerCurve(artificial_pC, test= fixed("complexity"), along = "subject", breaks =c(20,30,40,60), nsim = 100)
save(power, file = "simr_powerC_scenario1_snc.Rdata")


# --------------------- #
#### mixedpower

power_FLP <- mixedpower(model_emp = FLPmodel, data_emp = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "subject", steps = c(20,30,40,50,60),
                        critical_value = 2, n_sim = 1000)

save(power_FLP, file = "mixedpower_scenario1.Rdata")

multiplotPower(power_FLP)

# --------------------- #
# SESOI
FLPmodel_SESOI <- FLPmodel
FLPmodel_SESOI@beta <- c(1, -0.05, 0.05)

power_SESOI <- mixedpower(model = FLPmodel_SESOI, data = YanData,
                          fixed_effects = c("word_length", "complexity"),
                          simvar = "subject", steps = c(20,30,40,50,60),
                          critical_value = 2, n_sim = 1000)

save(power_SESOI, file = "mixedpower_SESOI_scenario1.Rdata")


# ------------------------------------------------------------------------- #
### ------ SCENARIO 2  ------ ##
# ------------------------------------------------------------------------- #


# ----------- 1) SIMPLE ---- #
power_sentences <- mixedpower(model = FLPmodel, data = YanData,
                        fixed_effects = c("word_length", "complexity"),
                        simvar = "sentence", steps = c(80,100,120,140,160),
                        critical_value = 2, n_sim = 1000)

save(power_sentences, file = "mixedpower_scenario2_48subjects.Rdata")


# ----------- 2) ADVANCEF ---- #
## simulate power for range of sentences and one specific sample size

# ----- start with 25

FLPmodel_20 <- simulateModel(FLPmodel, data, n_want = 20, simvar = "subject", nsim = 100)

summary(FLPmodel_25)

power20_sentences <- mixedpower(model = FLPmodel_20, data = data,
                              fixed_effects = c("word_length", "complexity"),
                              simvar = "sentence", steps = c(80,100,120,140,160),
                              critical_value = 2, n_sim = 1000)

# ---- same with 40

FLPmodel_40 <- simulateModel(FLPmodel, data, n_want = 40, simvar = "subject", nsim = 100)

summary(FLPmodel_25)

power40_sentences <- mixedpower(model = FLPmodel_40, data = data,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(80,100,120,140,160),
                                critical_value = 2, n_sim = 1000)

# ----- SESOI --- #
# start with 25

FLPmodel_20_SESOI <- simulateModel(FLPmodel_SESOI, data, n_want = 20, simvar = "subject", nsim = 100)

summary(FLPmodel_25)

power20_sentences <- mixedpower(model = FLPmodel_20_sentences, data = data,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(80,100,120,140,160),
                                critical_value = 2, n_sim = 1000)

# same with 40

FLPmodel_40_SESOI <- simulateModel(FLPmodel_SESOI, data, n_want = 40, simvar = "subject", nsim = 100)

summary(FLPmodel_25)

power40_sentences <- mixedpower(model = FLPmodel_40_SESOI, data = data,
                                fixed_effects = c("word_length", "complexity"),
                                simvar = "sentence", steps = c(80,100,120,140,160),
                                critical_value = 2, n_sim = 1000)

# ------------------------------------------------------------------------- #
### ------ SCENARIO 3  ------ ##
# ------------------------------------------------------------------------- #

