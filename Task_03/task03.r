setwd("~/Desktop/Evolution/Tasks/Task_03")
5
A <- 5
A *2
B <- c(A, 5, 5)
B
B *2
'B' *2
B <- c(A,5,5)
B*2
C <- c(B, 6, 10)
C *6
D <- c(1,5,10,15,20)
sum(D)
mean(D)
min(D)
E <- c(6,7,8,9,10)
which(E==8)
which(E>8)
E[which(E>8)]
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

pdf('samples.pdf')
boxplot(Sample1, Sample2)
dev.off()

individual1 <- c("B", "A")
individual2 <- c("A", "A")
Gam1 <- sample(individual1, 1)
Gam2 <- sample(individual2, 1)
newBaby <- c(Gam1, Gam2)
isHet <- c()
for(i in 1:100)	{
	Gam1 <- sample(individual1, 1)
	Gam2 <- sample(individual2, 1)
	newBaby <- c(Gam1, Gam2)
	isHet [i] <- Gam1 == Gam2
}
sum(isHet)/length(isHet)
source("simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(PatGrandma, PatGrandpa)
Focus <- makeBaby(Brenda, Alan)
ToMom <- length(grep("mom", Focus))/length(Focus)
ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
quantile(ManySiblings)
mean(ManySiblings)

pdf("003_relatePlot.pdf")
hist(ManySiblings, col='green')
dev.off()

# You see a range of values in these analyses because genetics involves random chance. You are 50% related to your mother and 50% related to your father, because you are made up of half of each of their DNA. Similarity varies between siblings because it is not guaranteed that they will get the same 50% as you did. It is possible that you are more related to one sibling than another.

