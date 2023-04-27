setwd('~/Desktop/Evolution/Tasks/Task_10')
x<-rnorm(100,mean=0,sd=2)
y<-x*5+2++runif(100,min=0,max=0.1)
#Intercept=2.052, x=4.999
lm(y~x)
linearmodel<-lm(y~x)
summary(linearmodel)
plot(linearmodel)
slope<-vector("numeric",100)
intercept<-vector("numeric",100)
z<-vector("numeric",100)

for(i in 1:100){
	x<-rnorm(100, mean=0, sd=2)
	z[i]<-runif(1,min=0.5,max=2)
	y<-x*5+2++runif(100,min=0,max=0.1)
	linearmodel<-lm(y~x)
	intercept[i]<-coef(linearmodel)[1]*z[i]+2
	slope[i]<-coef(linearmodel)[2]*z[i]
}

pdf("plot10.pdf")
plot(z,slope,xlab="z", ylab="Estimated Slope")
abline(lm(slope~z), col="blue")
dev.off()

#EC Monty Hall
n<-10000
prize<-sample(c("A","B","C"), size=n,replace=TRUE)
doorOpened<-ifelse(prize=="A", sample(c("B","C"), size=n, replace=TRUE), ifelse(prize=="B", "C", "B"))
doorUnopened<-ifelse(doorOpened=="B", "C", "B")
NotSwitchingWinChance<- sum(prize=="A")/n
SwitchingWinChance<- sum(prize==doorUnopened)/n
?barplot
WinChance<- c(NotSwitchingWinChance, SwitchingWinChance)
pdf("plot10_ec01.pdf")
barplot(WinChance, names.arg=c("Not Switching Doors", "Switching Doors"), ylab="Chance of Winning", main="Monty Hall Odds of Winning Grand Prize", col="blue")
dev.off()

#EC Meme
install.packages("meme")
library(meme)

png("meme10_1_ec02.png")
image<-"~/Desktop/this-is-fine.jpg"
meme(image, "Me during the entire semester of Evolution", size=2)
dev.off

png("meme10_2_ec02.png")
image<- "~/Desktop/4hdzm8.png"
meme(image, "Me when I figure out the R assignment", size=2)
dev.off()

png("meme10_3_ec02.png")
image<- "~/Desktop/Unknown.jpeg"
meme(image, "Dr. Mitchell when the class gets confused by irregualr verbs", size=1)
dev.off()

