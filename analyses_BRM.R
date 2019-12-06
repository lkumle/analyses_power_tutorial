# ------------------------------------------------------------------------- #
#               ANANYSES INLCUDED IN BRM MANUSCRIPT                         # 
# ------------------------------------------------------------------------- #

library(lme4)
library(simr)
library(mixedpower)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ----- SCENARIO 1 ------- #

#### load and preprocessed data 
# --> full analysis and preprocessing script and data can be downloaded here: >>> include link <<<
load("Yan_et_al.RData")

# rename variables for demonstartion reasons 
data["word_length"] <- data$wl.c
data["complexity"] <- data$sn.c
data["subject"] <- data$nsub
data["sentence"] <- data$nsen


#### fit model intended for simulation 
FLPmodel <- lmer(flp ~ word_length * complexity + (1|subject) + (1|sentence),
                 data=data)




#### simr analysis

# powerSim
power_complexity <- powerSim(fit = FLPmodel, test = fixed("complexity"), nsim = 100)

save(power_complexity, file = "simrpower_scenario1_snc.Rdata")

# powerCurve
artificial_pC <- extend(FLPmodel, along="subject", n = 60) # extend model/data

powerC <- powerCurve(artificial_pC, test= fixed("complexity"), along = "subject", breaks =c(20,30,40,60), nsim = 100)
save(power, file = "simr_powerC_scenario1_snc.Rdata")

