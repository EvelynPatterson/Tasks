setwd('~/Desktop/Evolution/Tasks/Task_05')
getwd()
install.packages("learnPopGen")
install.packages("adegenet")
install.packages("poppr")
install.packages("dplyr")
install.packages("hierfstat")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("scales")

library(adegenet)
library(poppr)
library(dplyr)
library(hierfstat)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)

lobster <- read.csv("~/Desktop/lobster_genotypes.csv")
lobster_wide <- reshape(lobster, idvar=c("ID", "Site"), timevar = "Locus", direction = "wide", sep = "")
colnames(lobster_wide) <- gsub("Genotype", "", colnames(lobster_wide))
snpgeno <- lobster_wide[, 3:ncol(lobster_wide)]
snps_to_remove <- c("25580", "32362", "41521", "53889", "65376", "8953", "21197", "15531", "22740", "28357", "33066", "51507", "53052", "53263", "21880", "22323", "22365")
snpgeno <- snpgeno[ , !colnames(snpgeno) %in% snps_to_remove]
ind <- as.character(lobster_wide$ID)
site <- as.character(lobster_wide$Site)
lobster_gen <- df2genind(snpgeno, ploidy=2, ind.names=ind, pop=site, sep="")
lobster_gen <- missingno(lobster_gen, type="geno", cutoff=0.20)
mlg(lobster_gen)
dups_lobster <- mlg.id(lobster_gen)
lob_dups <- c()
x<- 1
for(i in dups_lobster){
	if(length(dups_lobster[i])> 1){
		lob_dups[x]<- i
		x<- x+1
	}
}
lob_Nodups <- indNames(lobster_gen)[! indNames(lobster_gen) %in% lob_dups]
lobster_gen <- lobster_gen[lob_Nodups, ]
summary(lobster_gen$pop)
basic_lobster<- basic.stats(lobster_gen, diploid=TRUE)
Ho_lobster <- round(apply(basic_lobster$Ho, MARGIN=2, FUN=mean, na.rm=TRUE), digits=3)
He_lobster<- round(apply(basic_lobster$Hs, MARGIN=2, FUN=mean, na.rm=TRUE), digits=3)

pdf('005_plot01.pdf')
par(mar=c(4,4,1,1), las=1, mgp=c(2,0.25,0), tck=-0.005, cex.lab=1.25)
plot(He_lobster, Ho_lobster, xlab="expected H", ylab="observed H", pch=21, bg="gray", xlim=c(0.32, 0.405), ylim=c(0.32, 0.405))
abline(0,1,lty=3)
abline(lm(Ho_lobster~He_lobster), lty=2, lwd=1.25, col='red')
dev.off()

Fis <- apply(basic_lobster$Fis, MARGIN=2, FUN=mean, na.rm=TRUE)
lobster_gen_sub<- popsub(lobster_gen, sublist=c("Ale", "Ber", "Brd", "Pad", "Sar17", "Vig"))
lobster_fst <- genet.dist(lobster_gen_sub, method="WC84") %>% round(digits=3)
lab_order<- c("Ber", "Brd", "Pad", "Vig", "Sar17", "Ale")
fst.mat<- as.matrix(lobster_fst)
fst.mat1<- fst.mat[lab_order, ]
fst.mat2<- fst.mat1[, lab_order]
ind <- which(upper.tri(fst.mat2), arr.ind=TRUE)
fst.df <- data.frame(Site1= dimnames(fst.mat2)[[2]][ind[, 2]], 
					Site2= dimnames(fst.mat2)[[1]][ind[, 1]],
					Fst=fst.mat2[ ind ])
fst.df$Site1<- factor(fst.df$Site1, levels=unique(fst.df$Site1))
fst.df$Site2<- factor(fst.df$Site2, levels=unique(fst.df$Site2))
fst.df$Fst[fst.df$Fst<0]<- 0
fst.label<- expression(italic("F")[ST])
mid <- max(fst.df$Fst)/2

pdf('005_plot02.pdf')
ggplot(data=fst.df, aes(x=Site1, y=Site2, fill=Fst))+
	geom_tile(colour="black")+
	geom_text(aes(label=Fst), color="black", size=3)+
	scale_fill_gradient2(low="blue", mid="pink", high="red", 
	breaks=c(0,0.05,0.10,0.15))+
	scale_x_discrete(expand=c(0,0))+
	scale_y_discrete(expand=c(0,0), position="right")+
	theme(axis.text=element_text(colour="black", size=10, face="bold"),
		axis.title=element_blank(),
		panel.grid=element_blank(),
		panel.background=element_blank(),
		legend.position="right",
		legend.title=element_text(size=14, face="bold"),
		legend.text=element_text(size=10)
		)
dev.off()

pdf("learnPopGen1.pdf")
install.packages("coala")
library(coala)
library(learnPopGen)
coalescent.plot()
dev.off()


pdf("learnPopGen2.pdf")
coalescent.plot()
dev.off()

pdf("learnPopGen3.pdf")
coalescent.plot()
dev.off()
