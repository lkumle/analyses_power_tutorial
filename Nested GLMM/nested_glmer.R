# ------------------------------------------------------------------------- #
#               ANANYSES INLCUDED NESTED NOTEBOOK                           #
# ------------------------------------------------------------------------- #
rm(list= ls())

library(lme4)
library(mixedpower)
library(simr)
# load data

hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")

# preprocessing
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
  CancerStage <- factor(CancerStage)
})

hdp$DID <- as.numeric(as.character(hdp$DID))
hdp$HID <- as.numeric(as.character(hdp$HID))


# model m3b
model_final <- glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 + CRP + CancerStage +
               Experience + (1 + LengthofStay | DID) + (1 | HID), data = hdp, family = binomial,
             nAGQ = 1)

summary(model_final)


# ------------------------------------------------------------------------- #
# SIMPLE POWER ANALYSIS

# effect age
power <- powerSim(model_final, test = fixed("Age"), nsim = 100)

save(power, file = "nested_powerSim_Age.Rdata")

# effect Length of Stay
power <- powerSim(model_final, test = fixed("LengthofStay"), nsim = 100)

save(power, file = "nested_powerSim_LengthofStay.Rdata")


# ------------------------------------------------------------------------- #
# Changing the number of levels in random variables

# along length of Stay
power <- powerCurve(model_final, test = fixed("LengthofStay"), along = "DID", 
                    breaks = c(100, 200), nsim = 100)


save(power, file = "nested_powerCurve_LengthofStay.Rdata")


# mixedpower analysis
fixed_effects <- c("Age", "LengthofStay",  "FamilyHx" , "IL6", "CRP",  "CancerStage" ,"Experience")
SESOI <- c(-0.54, model_final@beta[2:10]*0.80)


power <- R2power(model = model_final, data = hdp,
                 fixed_effect, simvar = "DID", 
                 steps = c(100, 200,407),
                 critical_value = 2, n_sim = 100, 
                 R2var = "HID", R2level = 35)

save(power, file = "R2power_SESOI.Rdata")


# ------------------------------------------------------------------------- #
# CHANGING EFFECT SIZES

# with simr: 
fixef(model_final)["LengthofStay"] <- -0.095

power <- powerSim(model_final, test = fixed("LengthofStay"), nsim = 100)

save(power, file = "nested_powerSim_SESOI.Rdata")