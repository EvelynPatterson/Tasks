setwd('~/Desktop/Evolution/Tasks/Task_07')
install.packages("phytools")
install.packages("ape")
library(phytools)
library(ape)
text.string <- "(((((((cow, pig), whale),(bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree<- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
vert.tree
str(vert.tree)
tree<- read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric(read.tree('~/Desktop/Evolution/Tasks/Task_07/anolis.tre'))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges<- which(AnolisTree$edge[,2]<= Ntip(AnolisTree))
Lengths<- AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs<- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

?plot.phylo
plot(AnolisTree, type="phylogram", show.tip.label=FALSE, cex=0.25)
plot(AnolisTree, type="radial", cex=0.25)
eCols<- rep("black", 162)
(eCols[tipEdges]="red")
plot(AnolisTree, type="phylogram", cex=0.25, edge.col=eCols)

#A tree with no tip labels
pdf('notiplabels.pdf')
plot(AnolisTree, type="phylogram", use.edge.length=TRUE, node.pos=NULL, show.tip.label=F, show.node.label=FALSE, edge.color=NULL, edge.width=NULL, edge.lty=NULL, node.color=NULL, node.width=NULL, node.lty=NULL, font=3, cex=0.25, adj=NULL, srt=0, no.margin=FALSE, root.edge=FALSE, label.offset=0, underscore=FALSE, x.lim=NULL, y.lim=NULL, direction="rightwards", lab4ut=NULL, tip.color=par("col"), plot=TRUE, rotate.tree=0, open.angle=0, node.depth=1, align.tip.label=FALSE)
dev.off()

#A tree that is plotted as a circle, instead of facing left or right 
pdf('treecricle.pdf')
plot(AnolisTree, type="fan", use.edge.length=TRUE, node.pos=NULL, show.tip.label=F, show.node.label=FALSE, edge.color=NULL, edge.width=NULL, edge.lty=NULL, node.color=NULL, node.width=NULL, node.lty=NULL, font=3, cex=0.25, adj=NULL, srt=0, no.margin=FALSE, root.edge=FALSE, label.offset=0, underscore=FALSE, x.lim=NULL, y.lim=NULL, direction="rightwards", lab4ut=NULL, tip.color=par("col"), plot=TRUE, rotate.tree=0, open.angle=0, node.depth=1, align.tip.label=FALSE)
dev.off()

#A tree with the tips colored red instead of black
pdf('redtipstree.pdf')
plot(AnolisTree, type="phylogram", use.edge.length=TRUE, node.pos=NULL, show.tip.label=F,   show.node.label=FALSE, edge.color=NULL, edge.width=NULL, edge.lty=NULL, eCols[tipEdges], node.color=NULL, node.width=NULL, node.lty=NULL, adj=0.5, srt=0, no.margin=TRUE, root.edge=FALSE, label.offset=1, underscore=FALSE, x.lim=NULL, y.lim=NULL, direction="rightwards", lab4ut=NULL, tip.color='black', plot=TRUE, rotate.tree=0, open.angle=0, node.depth=1, align.tip.label=FALSE)
plot.phylo(AnolisTree, type="phylogram", cex=0.25, edge.color='red')
eCols<- rep("black", 162)
eCols[tipEdges]<- "red"
plot(AnolisTree, cex=0.25, edge.col=eCols)
str(AnolisTree)
dev.off()

shortedgelength<- which.min(Lengths)
shortedgelength
?drop.tip
AnolisTree2<-drop.tip(AnolisTree, tip=shortedgelength)
plot(AnolisTree2,cex=0.25)
Ntip(AnolisTree)
Ntip(AnolisTree2)

pdf('007_plot01.pdf')
ltt(AnolisTree)
abline(0,1,lwd=2, col='red', lty=2)
fit.bd(AnolisTree,b=NULL,d=NULL,rho=0.2)
dev.off()

setwd('~/Desktop/Evolution/Tasks/Task_07')
plot(tree,type="fan")

pdf('fossiltree.pdf')
data<- read.csv("svl.csv",stringsAsFactors=F, row.names=1)
svl<-setNames(data$svl,rownames(data))
Ancestors<-fastAnc(AnolisTree,svl,vars=TRUE, CI=TRUE)
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree,type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16,cex=0.25*svl[tree$tip.label])
nodelabels(pch=16,cex=0.25*Ancestors$ace)
obj<- contMap(AnolisTree,svl,plot=F)
plot(obj,legend=0.7*max(nodeHeights(AnolisTree)), sig=2,fsize=c(0.7,0.9))
dev.off()

fossilData<- data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)),tip1=c("Anolis_aliniger","Anolis_aliniger","Anolis_occultus","Anolis_ricordii","Anolis_cristatellus","Anolis_occultus"),tip2=c("Anolis_chlorocyanus","Anolis_coelestinus","Anolis_hendersoni","Anolis_cybotes","Anolis_angusticeps","Anolis_angusticeps"))
fossilNodes<-c()
nodeN<-c()
for (i in 1:nrow(fossilData)){
	Node<- fastMRCA(AnolisTree,fossilData[i, "tip1"],
	fossilData[i, "tip2"])
	fossilNodes[i]<-fossilData[i,"svl"]
	nodeN[i]<-Node
	names(fossilNodes)<-nodeN
}
Ancestors_withFossils<-fastAnc(AnolisTree,svl,anc.states=fossilNodes,CI=TRUE, var=TRUE)


