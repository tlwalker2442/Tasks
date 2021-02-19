source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type = "1", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd=2, bty="n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h =1, s = 0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- (5, 0, 0, 35)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- (2, 3, 10, 30)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- (15, 4, 1, 25)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- (10, 10, 10, 10)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- (0, 0, 0, 40)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- c(10, 10, 10, 10)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Expectation <- c(10, 10, 10, 10)
Observed <- c(0, 0, 0, 40)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))

X^2 of all 10's = 0
X^2 of all 40 combos in 1 category = 120 
How does X^2 related to evenness of bars? The more evenness of bars, the lower X^2 is. 

setwd('C:\\Users\\tlwalker2442\\OneDrive\\Evolution (461)\\Task_04')
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
write.csv(results, 'rawdata.csv', quote=F)
calcChi(counts[1,])
source("http://jonsmitchell.com/code/fxn05.R")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)

When chi squared is high, one bar is predominant and the others are non-existant. 
When chi squared is low, the bars are more even. 
The higher the #, the more uneven the bars, the lower the #, the more even. 

Avg <- mean(Chisqs)

The average Chi-squred seems to fall between 0-50, which is well above 11.70. 
It doesn't seem to differ very much by background. 

backgroundAvgs <- tapply(Chisqs, results[,3], mean) 
propSig <- length(which(Chisqs > 11.70))/ length(Chisqs)
percSig <- round(100 * propSig)

Not only natural selection driving that #, but maybe drift as well. 

par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1) 
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1) 
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n") 
axis(2, at=1: length(backgrounds), labels=backgrounds) 
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for(i in backgrounds) {
Data <- Chisqs[which(results[,3] == i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter+1}
abline(v=11.70, lty=2, lwd=2, col='black')

Almost every one of the distributions seem to be quite far to the right of the critical value. The white and green do not go as far out as the others though. 

Simulation <- simDraws(10000) 
addHist(Y=7, Dat=Simulation, Color="lightgray") 
mtext(side=2, at=7, line=0, "simulated") 
abline(v=11.70, lty=2, lwd=2)

Just like the other distributions, almost the entire time there is a "significant" result, because the distribution makes up the majority of the right side of the distribution. 

Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))

Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))

Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))

Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))

Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel.sim.")

Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 0.25))

What evolutionary process(es) are at work in the lab-as-done-by-humans?
Natural selection, because the student(s) chose the toothpicks not at random the second time. The first time when the students chose them randomly, would be drift. Also drift because the population is smaller. 

What evolutionary process(es) are at work in the lab-as-simulated-by-the-computer?
Drift, because the simulation was carried out "blindly". 

What do the graphs tell us about the relative strength of the evolutionary processes the BIOL-112 students are simulating?
It seems to represent directional selection, as discussed in lecture. 

What tells you more about what processes occur here: comparing the student numbers to a single critical value (11.70), or comparing to the simulated numbers?
That based on directional selection, one trait is favored more than th other(s). 

Imagine you added the possibility for a toothpick to mutate into a different type. What would that do to the Chi-squared values? 
This could introduce a new allele into the population, which could change the frequency. Depending if the new allele is favored or not, will tell us how much chi^2 changes. If it isn't favored then probably won't change chi^2 much, but if it is, then we would probably see a difference. 
