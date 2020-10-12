#10-12-20
# IAPV titers R code. The following analyses are done on log-transformed values for starting quantities (as calculated from the standard curve).

```
chiapv<-read.csv("chronic IAPV virus titers expression.csv")
head(chiapv)

library("lme4")
library("emmeans")
```

#Full treatment model
```
iapv.model1 <- lmer(log.sq.av ~ Pollen.virus + (1|Pulse), data = chiapv)
summary(iapv.model1)
anova(iapv.model1)

#To get pairwise: contrasts via Tukey HSD post hoc
emmeans(iapv.model1, "Pollen.virus", contr = "pairwise")
```

#Model for different effects of diet and virus treatments
```
iapv.model2 <- lmer(log.sq.av~ pollen.treatment+virus.treatment +(1|Pulse), data = chiapv)
summary(iapv.model2)
anova(iapv.model2)
```
