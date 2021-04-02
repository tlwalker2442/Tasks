setwd("C:\\Users\\tlwalker2442\\Desktop\\Evolution\\Tasks\\Task_08") 

library('phytools')
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label

Question 1: 82 tips, and branch lengths present (161 total) 

data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1) 
data
data[,1] 

Question 2: The object of the data is lizard snout-vent lengths, and there are 100 dimensions. 

svl <- setNames(data$svl, rownames(data))
svl

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE) 
Ancestors

Question 3: Where are the estimated values stored? A named vector containing the states at internal nodes 
What is the CI95 element? The upper & lower 95% confidence intervals 
Question 4: What are 2 assumptions made in the estimation of the ancestral states using fastAnc?

par(mac=c(0.1, 0.1, 0.1, 0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

tiplabels(pch=16, cex=0.25*svl[tree$tip.label])

nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))

Question 5: 
fossilNodes <- c()
nodeN <- c()
for(val in fossilData)
{if(val %% i == 1)
count = count+1}
print(Ancestors)

Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node

names(fossilNodes) <- nodeN

Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)

Question 7: 

Question 8-10: 
