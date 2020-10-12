#Acute data files

acutevirus<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/Acute data/Acute CSV/Acute_virus.csv", header = T)
acutemass2<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/Acute data/Acute CSV/Acute_mass3.csv", header = T)
acutemortality<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/Acute data/Acute CSV/Acute_mortality.csv", header = T)
acutelipid<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/Acute data/Acute CSV/Acute_lipid.csv", header = T)

#Chronic data files

chronicvirus<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/chronic data/chronic CSV/chronic_virus.csv", header = T)
chronicmortality<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/chronic data/chronic CSV/chronic_mortality.csv", header = T)
chroniclipidmass<- read.csv("C:/Users/Adam/Box Sync/Projects/2015 Experiments/Larval Stress - Alex/2020 Final data formatting/chronic data/chronic CSV/chronic_lipid_mass.csv", header = T)

#packages

library(ggplot2)
library(reshape2)
library(MASS)
library(lme4)
library(gridExtra)
library(dplyr)
library(latticeExtra)
library(multcomp)

########  ACUTE DATA ANALYSIS ################

#Acute mortality
head(acutemortality)

q.model.acutemortality <- lmer((Proportion.Mortality) ~ Treatment + (1| Date.group), acutemortality, REML = FALSE)
hist(resid(q.model.acutemortality ))
qqmath(resid(q.model.acutemortality ))

q.model.acutemortalityNull <- lmer(Proportion.Mortality ~ (1|Date.group) , acutemortality, REML = FALSE)


anova(q.model.acutemortality, q.model.acutemortalityNull)

summary(glht(q.model.acutemortality, linfct = mcp(Treatment = "Tukey")))


#### Acute Mortality outputs:


###ANOVA

#Data: acutemortality
#Models:
# q.model.acutemortalityNull: Proportion.Mortality ~ (1 | Date.group)
#q.model.acutemortality: (Proportion.Mortality) ~ Treatment + (1 | Date.group)
#Df      AIC      BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#q.model.acutemortalityNull  3  -59.551  -52.442 32.775  -65.551                             
#q.model.acutemortality      6 -127.067 -112.851 69.534 -139.067 73.516      3  7.534e-16 ***
---
  # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  ###TUKEY MULTCOMP
  
  #Simultaneous Tests for General Linear Hypotheses
  
  #Multiple Comparisons of Means: Tukey Contrasts
  
  
  #Fit: lmer(formula = (Proportion.Mortality) ~ Treatment + (1 | Date.group), 
  #         data = acutemortality, REML = FALSE)

#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)    
#Control Virus - Control Control == 0    0.17667    0.03173   5.567  < 1e-04 ***
#  Starved Control - Control Control == 0  0.05667    0.03173   1.786  0.28015    
#Starved Virus - Control Control == 0    0.32702    0.03215  10.172  < 1e-04 ***
#  Starved Control - Control Virus == 0   -0.12000    0.03173  -3.782  0.00081 ***
#  Starved Virus - Control Virus == 0      0.15035    0.03215   4.677  < 1e-04 ***
#  Starved Virus - Starved Control == 0    0.27035    0.03215   8.410  < 1e-04 ***
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Adjusted p values reported -- single-step method)


####Bee Mass; post emergence pre-treatment (so only control diet vs starved)####



t.test(mass..mg. ~ Treatment, data=acutemass2)

#Welch Two Sample t-test

#data:  mass..mg. by Treatment
#t = 3.5369, df = 33.132, p-value = 0.001221
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  58.4501 216.6999
#sample estimates:
# mean in group Control mean in group Starved 
#1098.950               961.375 #
##
          
####Bee Mass; post emergence pre-treatment (so only control diet vs starved)#### 

acutelipid
t.test(proportion.lipid ~ treatment, data=acutelipid)

#Welch Two Sample t-test

#data:  proportion.lipid by treatment
#t = -2.1556, df = 7.2702, p-value = 0.06661
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -0.0131389319  0.0005582446
#sample estimates:
#  mean in group control mean in group starved 
#0.01626218            0.02255253 



####ACUTE VIRUS TITERS ###

head(acutevirus)

q.model.acutevirus <- lmer((log.sq.mean) ~ Treatment + (1| Date.code), acutevirus, REML = FALSE)
hist(resid(q.model.acutevirus ))
qqmath(resid(q.model.acutevirus ))

q.model.acutevirusNull <- lmer(log.sq.mean ~ (1|Date.code) , acutevirus, REML = FALSE)


anova(q.model.acutevirus, q.model.acutevirusNull)

summary(glht(q.model.acutevirus, linfct = mcp(Treatment = "Tukey")))


##result:

#Data: acutevirus
#Models:
#  q.model.acutevirusNull: log.sq.mean ~ (1 | Date.code)
#q.model.acutevirus: (log.sq.mean) ~ Treatment + (1 | Date.code)
#Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
#q.model.acutevirusNull  3 163.04 168.11 -78.523   157.04                            
#q.model.acutevirus      6 146.01 156.14 -67.003   134.01 23.04      3  3.962e-05 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  ### MULTIPLE COMPARISONS ##
  
  #Simultaneous Tests for General Linear Hypotheses

#Multiple Comparisons of Means: Tukey Contrasts


#Fit: lmer(formula = (log.sq.mean) ~ Treatment + (1 | Date.code), data = acutevirus, 
   #       REML = FALSE)

#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)    
#Control Virus - Control Control == 0     2.9434     0.5986   4.917  < 0.001 ***
#  Starved Control - Control Control == 0   0.7399     0.5429   1.363  0.52116    
#Starved Virus - Control Control == 0     3.4433     0.5986   5.752  < 0.001 ***
 # Starved Control - Control Virus == 0    -2.2035     0.5986  -3.681  0.00129 ** 
#  Starved Virus - Control Virus == 0       0.4999     0.5429   0.921  0.79264    
#Starved Virus - Starved Control == 0     2.7034     0.5986   4.516  < 0.001 ***
#  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###$(Adjusted p values reported -- single-step method)


############ CHRONIC EXPERIMENT  #########################

#Chronic mortality

head(chronicmortality)

q.model.chronicmortality <- lmer((Proportion.mortality) ~ Treatment + (1| Date.code), chronicmortality, REML = FALSE)
hist(resid(q.model.chronicmortality ))
qqmath(resid(q.model.chronicmortality ))

q.model.chronicmortalityNull <- lmer(Proportion.mortality ~ (1|Date.code) , chronicmortality, REML = FALSE)


anova(q.model.chronicmortality, q.model.chronicmortalityNull)

summary(glht(q.model.chronicmortality, linfct = mcp(Treatment = "Tukey")))

# RESULTS CHRONIC MORTALITY


#Data: chronicmortality
#Models:
#  q.model.chronicmortalityNull: Proportion.mortality ~ (1 | Date.code)
#q.model.chronicmortality: (Proportion.mortality) ~ Treatment + (1 | Date.code)
#Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
#q.model.chronicmortalityNull  3 -64.723 -58.200 35.361  -70.723                            
#q.model.chronicmortality      6 -71.481 -58.435 41.740  -83.481 12.758      3    0.00519 **
#  ---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


###MULTIPLE COMPARISON

#Simultaneous Tests for General Linear Hypotheses

#Multiple Comparisons of Means: Tukey Contrasts


#Fit: lmer(formula = (Proportion.mortality) ~ Treatment + (1 | Date.code), 
#          data = chronicmortality, REML = FALSE)

#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)   
#Chestnut virus - Chestnut control == 0  0.030303   0.052435   0.578  0.93800   
#Cistus control - Chestnut control == 0  0.005378   0.046683   0.115  0.99945   
#Cistus virus - Chestnut control == 0    0.130264   0.046104   2.825  0.02429 * 
#  Cistus control - Chestnut virus == 0   -0.024925   0.046683  -0.534  0.95022   
#Cistus virus - Chestnut virus == 0      0.099961   0.046104   2.168  0.13061   
#Cistus virus - Cistus control == 0      0.124886   0.037562   3.325  0.00472 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Adjusted p values reported -- single-step method)



#Chronic virus


head(chronicvirus)

q.model.chronicvirus <- lmer((log.sq.mean) ~ Treatment + (1| Date.code), chronicvirus, REML = FALSE)
hist(resid(q.model.chronicvirus ))
qqmath(resid(q.model.chronicvirus ))

q.model.chronicvirusNull <- lmer(log.sq.mean ~ (1|Date.code) , chronicvirus, REML = FALSE)


anova(q.model.chronicvirus, q.model.chronicvirusNull)

summary(glht(q.model.chronicvirus, linfct = mcp(Treatment = "Tukey")))

##RESULTS CHRONIC VIRUS

#Data: chronicvirus
#Models:
#  q.model.chronicvirusNull: log.sq.mean ~ (1 | Date.code)
#q.model.chronicvirus: (log.sq.mean) ~ Treatment + (1 | Date.code)
#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
#q.model.chronicvirusNull  3 110.45 113.98 -52.224  104.447                            
#q.model.chronicvirus      6 102.24 109.30 -45.118   90.236 14.212      3   0.002631 **
 # ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> summary(glht(q.model.chronicvirus, linfct = mcp(Treatment = "Tukey")))

#Simultaneous Tests for General Linear Hypotheses

#Multiple Comparisons of Means: Tukey Contrasts


#Fit: lmer(formula = (log.sq.mean) ~ Treatment + (1 | Date.code), data = chronicvirus, 
 #         REML = FALSE)

#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)    
#Chestnut virus - Chestnut control == 0   2.8623     1.3264   2.158   0.1296    
#Cistus control - Chestnut control == 0   0.4564     1.3264   0.344   0.9854    
#Cistus virus - Chestnut control == 0     3.6072     1.2109   2.979   0.0148 *  
#  Cistus control - Chestnut virus == 0    -2.4059     1.0027  -2.399   0.0735 .  
#Cistus virus - Chestnut virus == 0       0.7448     0.8439   0.883   0.8080    
#Cistus virus - Cistus control == 0       3.1508     0.8439   3.734   <0.001 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Adjusted p values reported -- single-step method)


