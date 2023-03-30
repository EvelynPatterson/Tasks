setwd('~/Desktop/Evolution/Tasks/Task_08')
library(phytools)
#Questions 1-3
trees<- list()
births<- vector()
Fractions<-vector()
random<-c()
random2<-c()
treelist<-c()
for(i in 1:100){
	births[i]<- runif(1)
	Fractions[i]<- runif(1)
	trees[[i]]<- pbtree(n=100,b=births[i],d=births[i]*Fractions[i])
	random[[i]]<-births[i]
	random2[[i]]<-(Fractions[i])
	treelist[[i]]<- mean(trees[[i]]$edge.length)
}
pdf('trees_question1.pdf')
plot(trees[[i]])
dev.off()

#Question 4
#The net diversification increases as the log of the number of tips  increases.
sapply(trees,Ntip)
tips<- log(sapply(trees,Ntip))
random3<-unlist(random)
head(tips)
pdf('diversification&tips_question4.pdf')
plot(tips,xlab="log of tips",ylab="net diversification")
dev.off()

#Question 5
#In the graph, as speciation rate approaches 1, there is a  small decrease towards 0 in branch length. 
?cor
cor(tips,random3)
random4<-unlist(random2)
treelist2<-unlist(treelist)
plot(treelist2,random3,ylab="speciation rate", xlab="average branch length",pch=4)
pdf('speciation rate&average branch length_question5.pdf')
plot(treelist2,random3,ylab="speciation rate", xlab="average branch length",pch=1)
dev.off()

#Question 6
cor(treelist2,random4)
Tree<-trees[[which.max(tips)]]
pdf('tree_question6.pdf')
plot(Tree)
dev.off()

#Question 7
rates<-vector()
traits<-c()
meantraits<-c()
vartraits<-c()
for(i in 1:100){
	rates[i]<-runif(1)
	traits[[i]]<-fastBM(tree=Tree,sig2=rates[i])
	meantraits[[i]]<-mean(traits[[i]])
	vartraits[[i]]<-var(traits[[i]])
}
meantraits<-unlist(meantraits)
pdf('traits_question7.pdf')
plot(meantraits,rates)
dev.off()

#Question 8
cor(meantraits,rates)
#The correlation is -0.089, which tells us that there is a weak negative correlation. There is an inverse correlation.
vartraits<-unlist(vartraits)
pdf('vartraits_question8.pdf')
plot(vartraits,rates)
dev.off()

#Question 9
cor(vartraits,rates)
#The correlation is 0.740, which tells us that there is a strong positive correlation. A positive correlation happens when the changes in one variable relate to the same type of changes in the second variable.  
pdf('08_plot.pdf')
plot(traits[[1]],traits[[2]])
dev.off()

#Question 10
cor(traits[[1]],traits[[2]])
#The correlation is -0.1898, which tells us that there is a weak negative correlation. A negative correlation happens when one variable increases while the other decreases. 
traitMat<-cbind(traits[[1]],traits[[4]])

#Extra Credit
pdf('08_ec.pdf')
phylomorphospace(Tree,traitMat,xlab="Trait 1", ylab="Trait 2")
dev.off()