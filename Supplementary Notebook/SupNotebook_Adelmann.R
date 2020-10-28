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

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 1: ADELMANN ET AL.
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# --> data and analysis retrieved from: https://osf.io/5v7tc/

## load/ "preprocess" data according to analysis by Brysbaert & Stevens (2018)
Adelman_data <- read_excel("Adelman with two levels.xlsx",
                           col_types = c("text", "text", "text", "numeric", "numeric"))

#Take out the trials without RTs (errors and outliers)
adelman2 <- na.omit(Adelman_data)[1:10000,]

# convert variables needed for simulation into right format
adelman2$subID <- as.numeric(adelman2$participant) #numerical subject identifier 
adelman2$itemID <- as.numeric(as.factor(adelman2$item)) # also convert item into numeric dummy variable

## fit mode (use numeric sub and item identifier)
full_model <- lmer(RT ~ prime + (prime|itemID) + (prime|subID), data=adelman2)

summary(full_model)


#### ----------  POWER ANALYSIS WITH MIXEDPOWER ----------------------- ####


# use mixedpower() and average over different samples of participants/items
itm <- unique(adelman2$itemID)

# ---------------------------------- #
### 1: 40 participants/ 40 stimuli
# ---------------------------------- #

### A: sample 40 random subjects and 40 random stimuli. repeat 100 times and average
pow <- list()

tic("average over 100 mixedpower runs")
for (i in 1:100) {

  # randomly select  items & refit model
  # --> data is subsetted to 40 subjects in mixedpower() function
  selectionitem <- sample(itm,40)
  adelman3 <- adelman2[which(adelman2$itemID %in% selectionitem), ]

  sub_model <- lmer(RT ~ prime + (prime|itemID) + (prime|subID),
                    data=adelman3)

  # power analysis: include 40 subjects (steps = 40)
  power  <- mixedpower(model = full_model, data = adelman2,
                      fixed_effects = c("prime"),
                      simvar = "subID", steps = c(40),
                      critical_value = 2, n_sim = 100,
                      SESOI = F, databased = T)

  pow[i] <- power[1,1]

}
toc()
p <- unlist(pow)

save(p, file = "Adelman_1A_40_40.Rdata") # save results

# get mean power for 40 subjects/40 participants
mean(p)
hist(p)


# ---------------------------------- #
### 1: 60 participants/ 80 stimuli
# ---------------------------------- #

### A: sample 40 random subjects and 40 random stimuli. repeat 100 times and average
pow <- list()

tic("average over 100 mixedpower runs")
for (i in 1:100) {
  
  # randomly select  items & refit model
  # --> data is subsetted to 40 subjects in mixedpower() function
  selectionitem <- sample(itm,80)
  adelman3 <- adelman2[which(adelman2$itemID %in% selectionitem), ]
  
  sub_model <- lmer(RT ~ prime + (prime|itemID) + (prime|subID),
                    data=adelman3)
  
  # power analysis: include 40 subjects (steps = 80)
  power  <- mixedpower(model = full_model, data = adelman2,
                       fixed_effects = c("prime"),
                       simvar = "subID", steps = c(60),
                       critical_value = 2, n_sim = 100,
                       SESOI = F, databased = T)
  
  pow[i] <- power[1,1]
  
}
toc()
p <- unlist(pow)

save(p, file = "Adelman_1A_60_80.Rdata") # save results




# ---------------------------------- #
### 1: 20 participants/ 120 stimuli
# ---------------------------------- #

### A: sample 40 random subjects and 40 random stimuli. repeat 100 times and average
pow <- list()

tic("average over 100 mixedpower runs")
for (i in 1:100) {
  
  # randomly select  items & refit model
  # --> data is subsetted to 40 subjects in mixedpower() function
  selectionitem <- sample(itm,120)
  adelman3 <- adelman2[which(adelman2$itemID %in% selectionitem), ]
  
  sub_model <- lmer(RT ~ prime + (prime|itemID) + (prime|subID),
                    data=adelman3)
  
  # power analysis: include 40 subjects (steps = 40)
  power  <- mixedpower(model = full_model, data = adelman2,
                       fixed_effects = c("prime"),
                       simvar = "subID", steps = c(20),
                       critical_value = c(2), n_sim = 100,
                       SESOI = F, databased = T)
  
  pow[i] <- power[1,1]
  
}
toc()
p <- unlist(pow)

save(p, file = "Adelman_1A_20_120.Rdata") # save results


#---------------------------------------------------------------------# 
# CHECk FOR SOURCES OF VARIATION

# ----------------------------------------- #
### replicate 60 subjects/80 items with simr
# ----------------------------------------- #

# use mixedpower() and average over different samples of participants/items
itm <- unique(adelman2$itemID)
part <- unique(adelman2$subID)

pow=list()
for (i in 1:100) {
  selectionpart <- sample(part,60)
  selectionitem <- sample(itm,80)
  adelman3 <- adelman2[which(adelman2$participant %in% selectionpart &   adelman2$item %in% selectionitem), ]
  fit6 <- lmer(RT ~ prime + (prime|item) + (prime|participant), data=adelman3)
  power <- powerSim(fit6,nsim=20)
  pow[i] <- power[1]
}
p <- unlist(pow)
p = p*5 #to go from 20 simulations to percentages
mean(p)
hist(p)

save(p, file = "Adelman_simr_60_80.Rdata")


# ---------------------------------------------------------- #
### replicate 60 subjects/80 items with mixedpower (nsim = 20)
# ---------------------------------------------------------- #
pow <- list()

tic("average over 100 mixedpower runs")
for (i in 1:100) {
  
  # randomly select  items & refit model
  # --> data is subsetted to 40 subjects in mixedpower() function
  selectionitem <- sample(itm,80)
  adelman3 <- adelman2[which(adelman2$itemID %in% selectionitem), ]
  
  sub_model <- lmer(RT ~ prime + (prime|itemID) + (prime|subID),
                    data=adelman3)
  
  # power analysis: include 40 subjects (steps = 80)
  power  <- mixedpower(model = full_model, data = adelman2,
                       fixed_effects = c("prime"),
                       simvar = "subID", steps = c(60),
                       critical_value = 2, n_sim = 20,
                       SESOI = F, databased = T)
  
  pow[i] <- power[1,1]
  
}
toc()
p <- unlist(pow)

save(p, file = "Adelman_1A_60_80_nsim20.Rdata") # save results


#### ----------  POWER ANALYSIS WITH R2POWER --------------------------- ####


# same but with R2power: reproduce whole table (easier and faster with R2power)


# --------------------------------------- #
#### 2: replicate table:
# --------------------------------------- #

### R2power:
p_20i <- R2power(model = full_model, data = adelman2,
                         fixed_effects = c("prime"),
                         simvar = "subID", steps = c(20,40,60,80,100,120,1020), # all sample sizes
                         R2var = "itemID", R2level = c(20),   #20 items
                         critical_value = 2,  n_sim = 1000,
                         SESOI = F, databased = T)
save(p_20i, file = "Adelman_20i_R2.Rdata") # save results

p_40i <- R2power(model = full_model, data = adelman2,
                 fixed_effects = c("prime"),
                 simvar = "subID", steps = c(20,40,60,80,100,120), 
                 R2var = "itemID", R2level = c(40),   #40 items
                 critical_value = 2,  n_sim = 1000,
                 SESOI = F, databased = T)
save(p_40i, file = "Adelman_40i_R2.Rdata") # save results

p_60i <- R2power(model = full_model, data = adelman2,
                          fixed_effects = c("prime"), 
                          simvar = "subID", steps = c(20,40,60,80,100), 
                          R2var = "itemID", R2level = 60,  #60 items
                          critical_value = 2,  n_sim = 1000,
                          SESOI = F, databased = T)
save(p_60i, file = "Adelman_60i_R2.Rdata") # save results

p_80i <- R2power(model = full_model, data = adelman2,
                          fixed_effects = c("prime"),
                          simvar = "subID", steps = c(20,40,60,80),
                          R2var = "itemID", R2level = 80,  #80 items
                          critical_value = 2,  n_sim = 1000,
                          SESOI = F, databased = T)
save(p_80i, file = "Adelman_80i_R2.Rdata") # save results

p_100i <- R2power(model = full_model, data = adelman2,
                           fixed_effects = c("prime"),
                           simvar = "subID", steps = c(20,40,60),
                           R2var = "itemID", R2level = 100,  #100 items
                           critical_value = 2,  n_sim = 1000,
                           SESOI = F, databased = T)
save(p_100i, file = "Adelman_100i_R2.Rdata") # save results

p_120i <- R2power(model = full_model, data = adelman2,
                          fixed_effects = c("prime"),
                          simvar = "subID", steps = c(20,40),
                          R2var = "itemID", R2level = 120,  #120 items
                          critical_value = 2,  n_sim = 1000,
                          SESOI = F, databased = T)
save(p_120i, file = "Adelman_120i_R2.Rdata") # save results

p_420i <- R2power(model = full_model, data = adelman2,
                          fixed_effects = c("prime"),
                          simvar = "subID", steps = c(20),
                          R2var = "itemID", R2level = 420,  #420 items
                          critical_value = 2,  n_sim = 1000,
                          SESOI = F, databased = T)
save(p_420i, file = "Adelman_420i_R2.Rdata") # save results

