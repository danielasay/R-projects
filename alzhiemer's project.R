##### Analysis of Data from EDSD data. Comparing groups and their MD and FA values. 
#### Includes linear model ANOVA


# Corona Radiata comparison

cr <- data.frame(Corona_Radiata)

head(cr)

#Load tapply data

tapply(cr$FA,cr$Group,mean)
tapply(cr$MD,cr$Group,mean)
tapply(cr$MMSE,cr$Group,mean)
tapply(cr$FA,cr$Group,sd)
tapply(cr$MD,cr$Group,sd)
tapply(cr$MMSE,cr$Group,sd)

#Build FA linear model and run ANOVA

cr_fa.lm <- lm(FA ~ Group + MMSE + Education + Age, data = cr)

anova(cr_fa.lm)

#Build MD linear model and run ANOVA

cr_md.lm <- lm(MD ~ Group + MMSE + Education + Age, data = cr)

anova(cr_md.lm)

#Run t test on MD group differences

mdval <- MD_values

res.ftest <- var.test(MD ~ Group, data = cr)
res.ftest

t.test(mdval$AD_MD, mdval$HC_MD, alternative = c("two.sided"), var.equal = TRUE)

#Load UF data and run tapply commands

uf <- data.frame(Uncinate_Fasciculus)

tapply(uf$FA,cr$Group,mean)
tapply(uf$MD,cr$Group,mean)
tapply(uf$MMSE,cr$Group,mean)
tapply(uf$FA,cr$Group,sd)
tapply(uf$MD,cr$Group,sd)
tapply(uf$MMSE,cr$Group,sd)

#Build FA Linear Model and run ANOVA-UF

uf_fa.lm <- lm(FA ~ Group + MMSE + Education + Age, data = uf)

anova(uf_fa.lm)

#Build MD Linear Model and run ANOVA-UF

uf_md.lm <- lm(MD ~ Group + MMSE + Education + Age, data = uf)

anova(uf_md.lm)

#Build MMSE Linear Model and run ANOVA on UF MD Data

mmse_uf_md.lm <- lm(MMSE ~ Group + MD + Education + Age, data = uf)

anova(mmse_uf_md.lm)

#Build MMSE Linear Model and run ANOVA on Uf FA Data

mmse_uf_fa.lm <- lm(MMSE ~ Group + FA + Education + Age, data = uf)

anova(mmse_uf_fa.lm)

#Build MMSE Linear Model and run ANOVA on CR FA Data

mmse_cr.lm <- lm(MMSE ~ Group + FA + Education + Age, data = cr)

anova(mmse_cr.lm)

#Build MMSE Linear Model and run ANOVA on CR MD Data

mmse_cr_md.lm <- lm(MMSE ~ Group + MD + Education + Age, data = cr)

anova(mmse_cr_md.lm)