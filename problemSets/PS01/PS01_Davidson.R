#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

#Y - PC Exp. on H; X1 - PC Income; X2 - #ofRes. per 100k $Insecure; X3 - #ofPeopl per 1k in Urban Areas
str(expenditure)
expenditure[2:5]

####Q2.1

#The higher income p.c., the more is spent on housing.
scatter.smooth(expenditure$Y, expenditure$X1)
#How much is spent on housing p.c. and amount of people financially insecure.
scatter.smooth(expenditure$Y, expenditure$X2)
#Money spent on housing p.c. and amount of people living in urban areas.
scatter.smooth(expenditure$Y, expenditure$X3)
#Income p.c. and financial insecurity
scatter.smooth(expenditure$X1, expenditure$X2)
#Income p.c. and people living in urban areas: higher the income, the less urban dwellers.
scatter.smooth(expenditure$X1, expenditure$X3)
#Correlation between financial insecurity and urban population.
scatter.smooth(expenditure$X2, expenditure$X3)

##I would be able to gather more information if I new how to label the dots by state abbreviation and region, denoted by 4 different colors.

####Q2.2

#Region 4 has highest p.c. expenditure on housing.
barplot(expenditure$Y, expenditure$Region, col="light green")

###Q2.3

#I can't figure out how to change the points.
scatter.smooth(expenditure$Y, expenditure$X1, expenditure$Region)


