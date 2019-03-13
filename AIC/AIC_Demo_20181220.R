library(tidyverse)
library(MuMIn)
library(AICcmodavg)

sp <- read.csv("sp.csv", header = T, row.names = 1)

LM_sp <- lm(SR~FecCol + BOD5 + tss + NH3, data = sp, na.action = "na.fail")
summary(LM_sp)

DLM_sp <- dredge(LM_sp, extra = c("R^2", "adjR^2"))

LM8_sp <- lm(SR~FecCol + BOD5 + tss + NH3 + qmax + Domg + Phos + pH, data = sp, na.action = "na.fail")
DLM8_sp <- dredge(LM8_sp, extra = c("R^2", "adjR^2"))

write_csv(DLM8_sp, "sp_Dredge_8.csv", na = "" )

importance(DLM_sp)

library(yhat)

GLM_sp <- glm(SR~FecCol + BOD5 + tss + NH3, data = sp,family = poisson(),  na.action = "na.fail")
summary(GLM_sp)
DGLM_sp <- dredge(GLM_sp, extra = c("R^2", "adjR^2"))