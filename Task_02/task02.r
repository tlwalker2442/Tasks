setwd('C:\\Users\\tlwalker2442\\Desktop\\Evolution\\Tasks\\Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data$event == 'bottle')
Feeds <- which(Data[,'event'] == 'bottle')
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')] 
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),] 
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE) 

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds]
avgFeed <- tapply (beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length) 
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds]) 
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01) 
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red') 
pdf("r02b-totalMilkByDay.pdf", height=4, width=4) 
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01) 
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk") 
abline(h=mean(totalFeed), lty=2, col='red') 
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R") 
unique(beren3$event)
Question 01: 1 tells us the results, it is not a hypothesis, like the experiment has already been performed.
Question 02: It was only the time that Beren was at daycare and did not include the data from home. 
Hypothesis 1: amount of solids consumed vs walking skills, Does a variety of solid foods better his walking skills?
Hypothesis 2: introduction to solid food vs number of diapers, Will solid food increase the number of diapers used?
Hypothesis 3: naps vs walking, Do naps increase with the new skill of walking? 

Hypothesis 4: The amount of solids Beren consumes increases over time. 
setwd('C:\\Users\\tlwalker2442\\Desktop\\Evolution\\Tasks\\Task_02_c') 
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Feeds <- which(Data[,9] == 'solids')
berenFood <- Data[Feeds,] 
head(berenFood)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren4 <- Data
beren5 <- beren4[order(beren4$age),]
write.csv(beren5, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren5$event == "solids")
avgFood <- mean(beren5$value[Feeds]
avgFeed <- tapply(beren5$value[Feeds], beren5$age[Feeds], mean) 
varFeed <- tapply(beren5$value[Feeds], beren5$age[Feeds], var)
totalFeed <- tapply(beren5$value[Feeds], beren5$age[Feeds], sum)
numFeeds <- tapply(beren5$value[Feeds], beren5$age[Feeds], length)
cor(beren5$value[Feeds], beren5$age[Feeds])
cor.test(beren5$value[Feeds], beren5$age[Feeds])
berenCor <- cor.test(beren5$value[Feeds], beren5$age[Feeds])
summary(berenCor)
boxplot(beren5$value[Feeds] ~ beren5$bowel[Feeds] (xlab="number of bowel movements", ylab="amount of solids consumed")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="number of bowel movements", ylab="amount of solids consumed")
pdf("r02c-totalSolidsBowel.pdf", height=4, width=4)
plot(as.numeric(names(totalFeed)), totalFeed, type="c", pch=16, xlab="number of bowel movements", ylab="amount of solids consumed")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()