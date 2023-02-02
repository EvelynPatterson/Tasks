setwd('~/Desktop/Evolution/Tasks/Task_02')
Data1 <- read.csv("beren.csv" , stringsAsFactors=F)
Data2 <- read.csv("cyrus.csv", stringsAsFactors=F)
write.csv (Data1, 'rawdata.csv' , quote=F)
Data1
head(Data1)
GlargleBrgle <- Data1
head(GlargleBrgle)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1 [1, ]
Data1 [2, ]
Data1 [1:3,]
Data1 [1:3, 4]
Data1 [1:5, 1:3]
Data1 [257, ]
Feeds <- which(Data1 [,9] == 'bottle')
berenMilk <- Data1 [Feeds ,]
head(berenMilk)
nrow(berenMilk)
Feeds <- which(Data1 [, 'event'] == 'bottle')
Feeds <- which(Data1$event == 'bottle')
dayID <- apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3 <- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds]~beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source('plotFxn02b.R')
pdf("r02b-cumulativeMilkByTime.pdf")
length(Data2)
nrow(Data2)
ncol(Data2)
colnames(Data2)
head(Data2)
Data2[1,]
Data2[2, ]
Data2[1:3,]
Data2[1:3,4]
Data2[1:5, 1:3]
Mass <- which(Data2[,9] == 'trait_mass')
cyrusMilk <- Data2[Mass, ]
head(cyrusMilk)
nrow(Data2)
Mass <- which(Data2[,'event'] == 'trait_mass')
Mass <- which(Data2$event=='trait_mass')
dayID <- apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin="2022-04-12")
Data2$age <- dateID-dateID[which(Data2$event == 'birth')]
head(Data2)
cyrus2 <- Data2
cyrus3 <- cyrus2[order(cyrus2$age), ]
write.csv(cyrus3, 'cyrus_new.csv', quote=F, row.names=FALSE)
Mass <- which(cyrus3$event== 'trait_mass')
avgMilk <- mean(cyrus3$value[Mass])
avgMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], mean)
varMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], var)
totalMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], sum)
numMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], length)
cor(cyrus3$value[Mass], cyrus3$age[Mass])
cor.test(cyrus3$value[Mass], cyrus3$age[Mass])
cyrusCor <- cor.test(cyrus3$value[Mass], cyrus3$age[Mass])
summary(cyrusCor)
cyrusANOVA <- aov(cyrus3$value[Mass]~ cyrus3$caregiver[Mass])
boxplot(cyrus3$value[Mass]~ cyrus3$caregiver[Mass], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalMass), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalMass), lty=2, col='red')
dev.off()
boxplot(cyrus3$value[Mass]~ cyrus3$caregiver[Mass], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in days", ylab="ounces of milk")

pdf("exampleplot.pdf")
boxplot(beren3$value[Feeds]~ beren3$caregiver[Feeds], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
dev.off()

plot(beren3$value[Mass], beren3$age[Mass], col="green", xlab="Age (days)", ylab="Mass(kg)")
points(cyrus3$age[Mass]/1000, cyrus3$value[Mass], col="red", pch=16, xlab="Age in days", ylab="Mass in kg")
title(("Cyrus and Beren Age vs Mass"), xlab="age (days)", ylab="Mass(kg)")