trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2) 

Based on the graph, the populations were not that different from one another. 

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)

It should be 50%

ToMom <- length(grep("mom", Focus)) / length(Focus)

It should be 25% or less 

ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus) 

Relatedness among grandparents is not going to be equally related to Focus. 

Sibling_01 <- makeBaby(Brenda, Alan)

They may share some, alot, or very little. 

ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)

Probably very litte.

ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")

Why is there a range of values? 

HWE <- function(p)
{aa <- p^2
ab <- 2*p*(1-p)
bb <- (1-p)^2
return(c(aa=aa, ab=ab, bb=bb))}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from=0, to=1, by=0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")

As frequency of a allele increases, so does the frequency of aa individuals (vice versa). 
Time is not shown on this plot, but one can consider that as a allele increases, it would take time to do so. 

lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")

Matches Hardy-Weinberg

Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")

The frequency increased. 

install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=400, nrep=5, pause=0.01)
x <- genetic.drift(Ne=600, nrep=5, pause=0.01) 
x <- genetic.drift(Ne=800, nrep=5, pause=0.01) 
x <- genetic.drift(Ne=1000, nrep=5, pause=0.01) 
x <- genetic.drift(Ne=1200, nrep=5, pause=0.01) 

The patters were hard to see at first because the population was quite small, but the bigger the population gets, the frequency of A expands. 

PopSizes <- 5:50
Samples <- rep(PopSizes, 5) 
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt~Samples+0)

As the population size increases, the number of samples become very dispersed around 40 samples and it's hard to tell but it looks as if the samples stay below the line. As the population increases, the rate of drift becomes faster. 