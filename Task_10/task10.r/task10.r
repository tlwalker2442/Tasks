setwd("C:\\Users\\tlwalker2442\\Desktop\\Evolution\\Tasks\\Task_10")

install.packages("diversitree") 
library(diversitree)

transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_0 <- 0.2
extinction_0 <- 0.1

speciation_1 <- 0.2
extinction_1 <- 0.1

maxN <- 1e3
maxT <- 50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree)

stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)

Question 1: The frequency of state 1 was only higher than state 0 @ a diversification rate of 0.01. 
Frequencies <- c('State 0', 'State 1')

Colors <- c('pink', 'purple')

Data <- matrix(c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43, 0.32, 0.3, 0.43, 0.35, 0.35, 0.568), nrow = 2, ncol = 6, byrow=TRUE)

Data

Difference <- c(0.15, 0.1, 0.05, 0.03, 0.02, 0.01)

Freq1 <- c(0.32, 0.3, 0.43, 0.35, 0.35, 0.568)

Freq0 <- c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43)

pdf('Question1.pdf', height =6, width=6)

barplot(Data, names.arg=Difference,
main = 'Changes in Frequency of States based on Variation in R Values',
xlab = 'Difference in Diversification Rate',
ylab = 'Frequency',
beside=TRUE,
col = c('pink', 'purple')
)

legend('topright', Frequencies, fill = Colors)
dev.off()

Question 2: State 1 is never zero, but it comes very close if you increase the diverisification rate. 
Frequencies <- c('State 0', 'State 1')

Colors <- c('red', 'blue')

Data <- matrix(c(0.82, 0.8, 0.96, 0.85, 0.63, 0.9, 0.926, 0.923, 0.959, 0.955, 0.945, 0.968, 0.977, 0.963, 0.978, 0.984, 0.973, 0.18, 0.2, 0.04, 0.14, 0.37, 0.088, 0.074, 0.077, 0.041, 0.045, 0.055, 0.032, 0.023, 0.037, 0.022, 0.016, 0.027), nrow = 2, ncol = 17, byrow=TRUE)

Data

Difference <- c(0.05, 0.05, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.45, 0.45, 0.45)

pdf('Question2.pdf', height = 8, width = 8)

barplot(Data, names.arg=Difference,
main='How Close to Zero State 1 Gets When Transition Rate is Nonzero',
xlab='Difference in Diversification Rate',
ylab='Frequencies',
col=c('red', 'blue')
)

legend('topright', Frequencies, fill = Colors)
dev.off()
