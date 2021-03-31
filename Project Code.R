#Read in the data

library(readxl)
project <- read.csv("~/Documents/BYU/Fall 2019/Stat 230/milk_project.csv")
View(project)

#Make sure variables are factors

project$Brand <- factor(project$Brand)
project$Fat <- factor(project$Fat)

#Exploratory Analyses

tapply(project$Score,project$Brand,mean)
tapply(project$Score,project$Brand,sd)
tapply(project$Score,project$Fat,mean)
tapply(project$Score,project$Fat,sd)

#Exploratory boxplots

boxplot(Score ~ Brand, data=project)
boxplot(Score ~ Fat, data=project)

#Fit linear model and run ANOVA

anova(lm(Score ~ Fat + Brand + Brand*Fat, data = project))

#Plot Interactions for both variables

"Tastiness Score" <- project$Score
"Brand" <- project$Brand
"Fat Content" <- project$Fat

interaction.plot(`Fat Content`,Brand,project$Score,
                 col = c("orange", "blue"),
                 ylab = "Mean Tastiness Score", 
                 lty = 1,
                 main = "Interaction Plot of Fat Content",
                 leg.bty = "o")
interaction.plot(Brand,`Fat Content`,project$Score,
                 col = c("red", "purple"),
                 ylab = "Mean Tastiness Score",
                 lty = 1,
                 main = "Interaction Plot of Brand",
                 leg.bty = "o")

#T-test to compare means of nesqiuck whole and skim scores

neswhole <- nesquickwhole$Score
nesskim <- nesquickskim$Score

t.test(nesskim, neswhole, alternative = c("two.sided"), var.equal = TRUE)

#T-test to compare means of walmart and nesquick whole milk scores

walwhole <- walmartwhole$Score
neswhole <- nesquickwhole$Score

t.test(walwhole, neswhole, alternative = c("two.sided"), var.equal = TRUE)



