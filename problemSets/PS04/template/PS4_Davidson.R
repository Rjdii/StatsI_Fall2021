> ## Raymond Davidson 
> ## PS4

library(car)
data(Prestige)
help(Prestige)
library(stargazer)
library(emmeans)

#(a)

Prestige$professional <- ifelse(Prestige$type == 'prof', 1, 0)

#(b)

#Does income level mean the job is more prestigious? 

model1 <- lm(prestige ~ income, data = Prestige)
summary(model1)$coef

list1 <- list(income=0)

emmeans(model1, ~income, at=list1)

#Are professional jobs more prestigious? 

model2 <- lm(prestige ~ professional, data = Prestige)

summary(model2)$coef

list2 <- list(professional=30)

emmeans(model2, ~professional, at=list2)

modX <- lm(data = Prestige, professional ~ income)

stargazer(model1, model2, type = "text", title = "Regression Results")

print(Prestige$income)
