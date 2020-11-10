#-----------------------------------------------------------------------------#
#                         SUPPLEMENTARY NOTEBOOK                              #
#-----------------------------------------------------------------------------#


# This R-script features analyses implemented for the Supplementary Notebook
# of (Kumle, Vo & Draschkow, under review). 
# The Notebook can be accessed at 
# https://lkumle.github.io/power_notebooks/Supplementary_notebook.html

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(lme4)
library(mixedpower)
library(readxl)
library(tictoc)
library(tidyverse)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 1: PEREA ET AL.
# --> data and analysis retrieved from: https://osf.io/5v7tc/

Perea_used_for_analysis <- read_excel("Perea data.xlsx",
                                      col_types = c("numeric", "numeric",
                                                    "text", "text", "text",
                                                    "numeric", "numeric"))

perea <- na.omit(Perea_used_for_analysis)



# ------- power analysis with different priming effect ----------------------#


#### 15 ms priming effect 
#Calculate the power for the Perea data if the RTs in the repeated prime condition were 25 ms higher 
#(leaving a 15 sm priming effect)
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+25
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT

fit <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3) 

# mixedpower():
power_15ms  <- mixedpower(model = fit, data = perea3,
                           fixed_effects = c("REPETITION"),
                           simvar = "ITEM", steps = c(120),
                           critical_value = 2, n_sim = 1000,
                           SESOI = F, databased = T)

save(power_15ms, file = "Perea_15ms.Rdata")

# R2power():
power_15ms  <- R2power(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T, 
                          R2var = "SUBJECT", R2level = 40)

save(power_15ms, file = "Perea_15ms_R2power.Rdata")

# simr 
power_15ms_simr <- powerSim(fit,nsim=1000)


#### 12 ms priming effect 
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+28
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT

fit <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3) 

# mixedpower():
power_12ms  <- mixedpower(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          # use same steps as simr in powerCurve
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T)


save(power_12ms, file = "Perea_12ms.Rdata")

# R2power():
power_12ms  <- R2power(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T, 
                          R2var = "SUBJECT", R2level = 40)

save(power_12ms, file = "Perea_12ms_R2power.Rdata")


# simr 
power_12ms_simr <- powerSim(fit,nsim=1000)

#### 10 ms priming effect 
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+30
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT

fit <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3) 

# mixepower():
power_10ms  <- mixedpower(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          # use same steps as simr in powerCurve
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T)

save(power_10ms, file = "Perea_10ms.Rdata")

# R2power():
power_10ms  <- R2power(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T, 
                          R2var = "SUBJECT", R2level = 40)

save(power_10ms, file = "Perea_10ms_R2power.Rdata")

# simr 
power_10ms_simr <- powerSim(fit,nsim=1000)

#### 7 ms priming effect 
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+33
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT

fit <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3) 

# mixedpower():
power_7ms  <- mixedpower(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          # use same steps as simr in powerCurve
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T)

save(power_7ms, file = "Perea_7ms.Rdata")

# R2power():
power_7ms  <- R2power(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T, 
                          R2var = "SUBJECT", R2level = 40)

save(power_7ms, file = "Perea_7ms_R2power.Rdata")

# simr 
power_7ms_simr <- powerSim(fit,nsim=1000)

#### 5 ms priming effect 
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+35
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT
fit <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3)


# mixedpower()
power_5ms  <- mixedpower(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          # use same steps as simr in powerCurve
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T)

save(power_5ms, file = "Perea_5ms.Rdata")

# R2power():
power_5ms  <- R2power(model = fit, data = perea3,
                          fixed_effects = c("REPETITION"),
                          simvar = "ITEM", steps = c(120),
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T, 
                          R2var = "SUBJECT", R2level = 40)

save(power_5ms, file = "Perea_5ms_R2power.Rdata")

# simr 
power_5ms_simr <- powerSim(fit,nsim=1000)


# ----------------------------------------------------------------------------#
# mixedpower() vs. powerCurve() for 15 ms priming effect 

## change effect size to 15 ms:
perea1 <- subset(perea, REPETITION=="unrelated")
perea2 <- subset(perea, REPETITION=="repeated")
perea2$RT = perea2$RT+25
perea3 <-rbind(perea1,perea2)
perea3$invRT <- -1000/perea3$RT

## fit model: 
fit_15ms <- lmer(invRT ~ REPETITION + (1|ITEM) + (REPETITION|SUBJECT), data=perea3) 

## power analysis with simr: 
power_itm <- powerCurve(fit, along = "ITEM", nsim = 100) # items
plot(power_itm)

power_sub <- powerCurve(fit, along = "SUBJECT", nsim = 100) # subject
plot(power_sub)



## power analysis with mixedpower()

power_subs  <- mixedpower(model = fit_15ms, data = perea3,
                          fixed_effects = c("REPETITION"),
                          # use same steps as simr in powerCurve
                          simvar = "SUBJECT", steps = c(3,7,11,15,19,24,28,32,36,40), 
                          critical_value = 2, n_sim = 1000,
                          SESOI = F, databased = T)

save(power_subs, file = "Perea_along_subs.Rdata")

power_items  <- mixedpower(model = fit_15ms, data = perea3,
                           fixed_effects = c("REPETITION"),
                           # use same steps as simr in powerCurve
                           simvar = "ITEM", steps = c(3, 16, 29, 42, 55, 68, 81, 94, 107, 120),
                           critical_value = 2, n_sim = 1000,
                           SESOI = F, databased = T)

save(power_items, file = "Perea_along_items.Rdata")

## power analysis with R2power()

power_subs_R2  <- R2power(model = fit, data = perea3,
                         fixed_effects = c("REPETITION"),
                         simvar = "SUBJECT", steps = c(3,7,11,15,19,24,28,32,36,40),
                         critical_value = 2, n_sim = 1000,
                         SESOI = F, databased = T, 
                         R2var = "ITEM", R2level = 120)

save(power_subs_R2, file = "Perea_along_subs_R2.Rdata")


power_item_R2  <- R2power(model = fit, data = perea3,
                             fixed_effects = c("REPETITION"),
                             simvar = "ITEM", steps = c(3, 16, 29, 42, 55, 68, 81, 94, 107, 120),
                             critical_value = 2, n_sim = 1000,
                             SESOI = F, databased = T, 
                             R2var = "SUBJECT", R2level = 40)

save(power_item_R2, file = "Perea_along_item_R2.Rdata")


## plot mixedpower

plot_items <- gather(power_items, key = "step", value = "power", `3`:`120`)
plot_items$power <- plot_items$power*100

plot(plot_items$step, plot_items$power, xlab = "number of items", ylab = "power", 
     type = "o", col = "black", pch = 19, ylim = c(0,100))
abline(h=80, col="black", lty = "dashed")

plot_subs <- gather(power_subs, key = "step", value = "power", `3`:`40`)
plot_subs$power <- plot_subs$power*100

plot(plot_subs$step, plot_subs$power, xlab = "number of subjects", ylab = "power", 
     type = "o", col = "black", pch = 19, ylim = c(0,100))
abline(h=80, col="black", lty = "dashed")


## plot R2power
plot_item_R2 <- gather(power_item_R2, key = "step", value = "power", `3`:`120`)
plot_item_R2$power <- plot_item_R2$power*100

plot(plot_item_R2$step, plot_item_R2$power, xlab = "number of items", ylab = "power", 
     type = "o", col = "black", pch = 19, ylim = c(0,100))
abline(h=80, col="black", lty = "dashed")

plot_subs_R2 <- gather(power_subs_R2, key = "step", value = "power", `3`:`40`)
plot_subs_R2$power <- plot_subs_R2$power*100

plot(plot_subs_R2$step, plot_subs_R2$power, xlab = "number of subjects", ylab = "power", 
     type = "o", col = "black", pch = 19, ylim = c(0,100))
abline(h=80, col="black", lty = "dashed")


