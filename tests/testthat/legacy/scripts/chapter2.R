# ====================================================
# Chapter 2 : Dyadic Pattern Univariate Single-Case
# ====================================================

# ---- 1. Analysis for S = 2 ----

## ---- 1.1 Load the data and set the parameters ----

load("data/chap2/APM.RData")
load("data/chap2/AM.RData")
load("data/chap2/PM.RData")
load("data/chap2/IM.RData")

s <- 2

## ---- 1.2 Actor-partner pattern ----

apmFM <- APM$chain1
apmSM <- APM$chain2

empMatFM.APM <- countEmp(states = s, chainFM = apmFM, chainSM = apmSM)
empMatSM.APM <- countEmp(states = s, chainFM = apmSM, chainSM = apmFM)

estimationFM.APM <- mleEstimation(empirical = empMatFM.APM)
estimationSM.APM <- mleEstimation(empirical = empMatSM.APM)

estimationFM.APM
estimationSM.APM

#identification of the pattern
apm_resFM <- univariatePattern(states = s, chainFM = apmFM, chainSM = apmSM, alpha = 0.05)
apm_resSM <-  univariatePattern(states = s, chainFM = apmSM, chainSM = apmFM, alpha = 0.05)

apm_resFM
apm_resSM

## ---- 1.3 Actor pattern ----

amFM <- AM$chain1
amSM <- AM$chain2

empMatFM.AM <- countEmp(states = s, chainFM = amFM, chainSM = amSM)
empMatSM.AM <- countEmp(states = s, chainFM = amSM, chainSM = amFM)

estimationFM.AM <- mleEstimation(empirical = empMatFM.AM)
estimationSM.AM <- mleEstimation(empirical = empMatSM.AM)

estimationFM.AM
estimationSM.AM

#identification of the pattern
am_resFM <- univariatePattern(states = s, chainFM = amFM, chainSM = amSM, alpha = 0.05)
am_resSM <-  univariatePattern(states = s, chainFM = amSM, chainSM = amFM, alpha = 0.05)

am_resFM
am_resSM

## ---- 1.4 Partner pattern ----

pmFM <- PM$chain1
pmSM <- PM$chain2

empMatFM.PM <- countEmp(states = s, chainFM = pmFM, chainSM = pmSM)
empMatSM.PM <- countEmp(states = s, chainFM = pmSM, chainSM = pmFM)

estimationFM.PM <- mleEstimation(empirical = empMatFM.PM)
estimationSM.PM <- mleEstimation(empirical = empMatSM.PM)

estimationFM.PM
estimationSM.PM

#identification of the pattern
pm_resFM <- univariatePattern(states = s, chainFM = pmFM, chainSM = pmSM, alpha = 0.05)
pm_resSM <-  univariatePattern(states = s, chainFM = pmSM, chainSM = pmFM, alpha = 0.05)

pm_resFM
pm_resSM

## ---- 1.5 Independence pattern ----

imFM <- IM$chain1
imSM <- IM$chain2

empMatFM.IM <- countEmp(states = s, chainFM = imFM, chainSM = imSM)
empMatSM.IM <- countEmp(states = s, chainFM = imSM, chainSM = imFM)

estimationFM.IM <- mleEstimation(empirical = empMatFM.IM)
estimationSM.IM <- mleEstimation(empirical = empMatSM.IM)

estimationFM.IM
estimationSM.IM

#identification of the pattern
im_resFM <- univariatePattern(states = s, chainFM = imFM, chainSM = imSM, alpha = 0.05)
im_resSM <- univariatePattern(states = s, chainFM = imSM, chainSM = imFM, alpha = 0.05)

im_resFM
im_resSM

# ---- 2. Analysis for S = 3 ----

load("data/chap2/APM3.RData")
load("data/chap2/AM3.RData")
load("data/chap2/PM3.RData")
load("data/chap2/IM3.RData")

s <- 3

## ---- 2.2 Actor-partner pattern ----

apmFM3 <- APM3$chain1
apmSM3 <- APM3$chain2

empMatFM3.APM <- countEmp(states = s, chainFM = apmFM3, chainSM = apmSM3)
empMatSM3.APM <- countEmp(states = s, chainFM = apmSM3, chainSM = apmFM3)

estimationFM3.APM <- mleEstimation(empirical = empMatFM3.APM)
estimationSM3.APM <- mleEstimation(empirical = empMatSM3.APM)

estimationFM3.APM
estimationSM3.APM

#identification of the pattern
apm3_resFM <- univariatePattern(states = s, chainFM = apmFM3, chainSM = apmSM3, alpha = 0.05)
apm3_resSM <-  univariatePattern(states = s, chainFM = apmSM3, chainSM = apmFM3, alpha = 0.05)

apm3_resFM
apm3_resSM

## ---- 2.3 Actor pattern ----

amFM3 <- AM3$chain1
amSM3 <- AM3$chain2

empMatFM3.AM <- countEmp(states = s, chainFM = amFM3, chainSM = amSM3)
empMatSM3.AM <- countEmp(states = s, chainFM = amSM3, chainSM = amFM3)

estimationFM3.AM <- mleEstimation(empirical = empMatFM3.AM)
estimationSM3.AM <- mleEstimation(empirical = empMatSM3.AM)

estimationFM3.AM
estimationSM3.AM

#identification of the pattern
am3_resFM <- univariatePattern(states = s, chainFM = amFM3, chainSM = amSM3, alpha = 0.05)
am3_resSM <-  univariatePattern(states = s, chainFM = amSM3, chainSM = amFM3, alpha = 0.05)

am3_resFM
am3_resSM

## ---- 2.4 Partner pattern ----

pmFM3 <- PM3$chain1
pmSM3 <- PM3$chain2

empMatFM3.PM <- countEmp(states = s, chainFM = pmFM3, chainSM = pmSM3)
empMatSM3.PM <- countEmp(states = s, chainFM = pmSM3, chainSM = pmFM3)

estimationFM3.PM <- mleEstimation(empirical = empMatFM3.PM)
estimationSM3.PM <- mleEstimation(empirical = empMatSM3.PM)

estimationFM3.PM
estimationSM3.PM

#identification of the pattern
pm3_resFM <- univariatePattern(states = s, chainFM = pmFM3, chainSM = pmSM3, alpha = 0.05)
pm3_resSM <-  univariatePattern(states = s, chainFM = pmSM3, chainSM = pmFM3, alpha = 0.05)

pm3_resFM
pm3_resSM

## ---- 2.5 Independence pattern ----

imFM3 <- IM3$chain1
imSM3 <- IM3$chain2

empMatFM3.IM <- countEmp(states = s, chainFM = imFM3, chainSM = imSM3)
empMatSM3.IM <- countEmp(states = s, chainFM = imSM3, chainSM = imFM3)

estimationFM3.IM <- mleEstimation(empirical = empMatFM3.IM)
estimationSM3.IM <- mleEstimation(empirical = empMatSM3.IM)

estimationFM3.IM
estimationSM3.IM

#identification of the pattern
im3_resFM <- univariatePattern(states = s, chainFM = imFM3, chainSM = imSM3, alpha = 0.05)
im3_resSM <-  univariatePattern(states = s, chainFM = imSM3, chainSM = imFM3, alpha = 0.05)

im3_resFM
im3_resSM
