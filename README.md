# ANOVA-and-posthoc
install.packages("lmerTest")
install.packages("nlme")
install.packages("multcomp")
install.packages('lsmeans')
install.packages("multcompView")
library(lsmeans)
library(multcomp)
library(nlme)
library(lme4)
library(lmerTest)
library(readr)
library(pastecs)
library(agricolae)
if(!require(nlme)){install.packages("nlme")}
install.packages("deplyr")
install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
install.packages ("tidyverse", dependencies = TRUE)
install.packages("tidyverse", type="source")
install.packages("car")
library(car)

datakulabio<-read.csv("Kula_bio_data-analysis.csv")
datakulabio
modeldatakulabio <- lm(Delaulled.grain.wt.per.pot ~ Treatment.ID, data=datakulabio)
anova(modeldatakulabio)
outmodeldatakulabio<- LSD.test(modeldatakulabio,"Treatment.ID", group=TRUE, p.adj="none")
print(outmodeldatakulabio)
ggqqplot(residuals(modeldatakulabio))
plot(modeldatakulabio, 1)
leveneTest(Total.grain.wt.pot ~ Treatment.ID, data=datakulabio)

#one way anova and random effect 
fit<-lmer(MC..lb.acre..12.. ~ Variety + (1|Year), data=redata2021)
anova(fit)
require(multcompView)
outfit<-LSD.test(fit,"Variety", group=TRUE, p.adj="none")
print(outfit)


summary(glht(fit,  linfct=mcp(habitat = "Tukey")))
marginal=lsmeans(fit, ~Variety)
CLD=cld(marginal, alpha=0.05, Letters=letters, adjust="tukey")
CLD

