setwd('~/Desktop/Evolution/Tasks/Project/Data')
my_data1 <- read.csv("FemaleChoice.csv")
my_data2 <- read.csv("MaleCognitionColor.csv")
head(my_data1)
head(my_data2)

pdf('MatingSuccess1.pdf')
par(mfrow=c(1,3))
X <- my_data1[,"ThroatIntensity"]
Y <- my_data1[,4]
plot(X, Y)
LogReg <- glm(Y~X, family=binomial(link="logit"))
summary(LogReg)
predY <- predict(LogReg, data.frame(X=seq(from=0, to=3, by=0.1)), type="response")
lines(x=seq(from=0, to=3, by=0.1), exp(predY)/(1+exp(predY)), col='red')

cor(X, Y)

X <- my_data1[,"ThroatArea"]
Y <- my_data1[,4]
plot(X, Y)
LogReg <- glm(Y~X, family=binomial(link="logit"))
summary(LogReg)
predY <- predict(LogReg, data.frame(X=seq(from=0, to=5, by=0.1)), type="response")
lines(x=seq(from=0, to=5, by=0.1), exp(predY)/(1+exp(predY)), col='red')

X <- my_data1[,"ThroatCombined"]
Y <- my_data1[,4]
plot(X, Y)
LogReg <- glm(Y~X, family=binomial(link="logit"))
summary(LogReg)
predY <- predict(LogReg, data.frame(X=seq(from=0, to=7, by=0.1)), type="response")
lines(x=seq(from=0, to=7, by=0.1), exp(predY)/(1+exp(predY)), col='red')
dev.off()

pdf("MatingSuccess2.pdf")
X<- my_data2[, "MCPreThroatCombined_a"]
Y <- my_data2[, 3]
plot(X, Y)
LogReg <- glm(Y~X, family=binomial(link="logit"))
summary(LogReg)
predY <- predict(LogReg, data.frame(X=seq(from=0, to=7, by=0.1)), type="response")
lines(x=seq(from=0, to=7, by=0.1), exp(predY)/(1+exp(predY)), col='red')
dev.off()

pdf("MatingSuccess2b.pdf")
X <- my_data2[, "MCPreThroatCombined_b"]
Y <- my_data2[, 3]
plot(X,Y)
LogReg <- glm(Y~X, family=binomial(link="logit"))
summary(LogReg)
predY <- predict(LogReg, data.frame(X=seq(from=0, to=7, by=0.1)), type="response")
lines(x=seq(from=0, to=7, by=0.1), exp(predY)/(1+exp(predY)), col='red')
dev.off()

setwd('~/Desktop/Evolution/Tasks/Project/Data')
install.packages("readxl")
library("readxl")
my_data3 <- read_excel("PelvicSpineErections_Means_TTestData_DRYAD.xlsx")
my_data4 <- read_excel("PSEnearFleeLead_FleeLeadnearPSE_Data_PercentageCalculations_DRYAD.xlsx")
my_data5 <- read_excel("ColorData_Stickleback_DRYAD.xlsx")
my_data6 <- read_excel("BehavioralDATA_BORIS_WithPerMinCalculations_DRYAD.xlsx")
my_data7 <- read_excel("AllColorSpots__Stages1-4_ExperimentalANDControl_withlength_DRYAD.xlsx")
head(my_data3)
head(my_data4)
head(my_data5)
head(my_data6)
head(my_data7)


