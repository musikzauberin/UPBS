# CHAPTER 5. LINEAR MODELS: REGRESSION

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
################################################

# This script uses genome size and morphology data to explore regression in R

# (1) Load the data
genome <- read.csv('../Data/GenomeSize.csv')

# (2) look at pairwise plots of all variables
pairs(genome)

# (3) identify the column numbers of five key variables and produce pairs plots
morpho <- c(4,7,8,12,14)
pairs(genome[, morpho], col=genome$Suborder)


# (4) Look at pairwise correlation coefficients and a correlation test
cor(genome[, morpho], use='pairwise')
cor.test(genome$GenomeSize, genome$TotalLength, use='pairwise')

# (5) log transform the key variables
genome$logGS <- log(genome$GenomeSize)
genome$logBW <- log(genome$BodyWeight)
genome$logTL <- log(genome$TotalLength)
genome$logFL <- log(genome$ForewingLength)
genome$logFA <- log(genome$ForewingArea)

str(genome)

# (6) get the columns of the transformed variables and repeat pairs and cor
logmorpho <- c(17,18,19,20,21)
# pair plots of log variables
pairs(genome[, logmorpho], col=genome$Suborder)
# correlation coefficient for log variables
cor(genome[, logmorpho], use='pairwise')

# (7) fit the null and genome size model for dragonflies
nullModDragon <- lm(logBW ~ 1, data=genome, subset=Suborder=='Anisoptera')
genomeSizeModelDragon <- lm(logBW ~ logGS, data=genome, subset=Suborder=='Anisoptera')
summary(genomeSizeModelDragon)
anova(genomeSizeModelDragon)

# residual sums of squares for the two models
sum(resid(nullModDragon)^2)
sum(resid(genomeSizeModelDragon)^2) 

# (8) fit the genome size model for damselflies
genomeSizeModelDamsel <- lm(logBW ~ logGS, data=genome, subset=Suborder=='Zygoptera')
summary(genomeSizeModelDamsel)
anova(genomeSizeModelDamsel)

# (9) Examine model diagnostics

# pdf("../Results/DiagModDragon.pdf")
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(genomeSizeModelDragon)
# dev.off()

# pdf("../Results/DiagModDamsel.pdf")
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(genomeSizeModelDamsel)
# dev.off()

# (10) Plot the model and add regression lines
pdf("../Results/DiagMod.pdf")
myCol <- c('red','blue')
plot(logBW ~ logGS, data=genome, col=myCol[Suborder], xlab='log Genome Size (pg)', ylab='log Body Weight (g)')
abline(genomeSizeModelDragon, col='red')
abline(genomeSizeModelDamsel, col='blue')
dev.off()

######################################################
# (11) Plot model and add regression lines using ggplot2 # Newly Added
library(ggplot2)

# basic plot
# differentiate Suborder by shape
p1 <- ggplot(genome, aes(x = logGS, y = logBW, shape = Suborder, color = Suborder)) +
  geom_point(size = 4, alpha = 0.6)
p1

# differentiate Suborder by color
p1 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, color = Suborder)) +
  geom_point() 
p1

# differentiate Suborder by color and shape
p1 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, color = Suborder, shape = Suborder)) +
  geom_point() 
p1


# manually adjust size and shape of plot character
p2 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, shape = Suborder, color = Suborder)) +
  geom_point(size = 3)  + # increase plot character size
  scale_shape_manual(values = c(1,3)) # change plot character shape
p2

# change plot background/theme
p3 <- p2 + theme_set(theme_bw())
p3

# set size of text for axis and for titles
text <- element_text(size = 14)
bold.16.text <- element_text(face = "bold", size = 16)

p4 <- p3 + labs(title = "Damselflies and Dragonflies", x = "Total Length / mm ", y = "Genome Size / pg ") +
  theme(title = bold.16.text, axis.title = bold.16.text, axis.text = text)

p4

# SAVE FILE AS A PDF:
pdf('../Results/GenomeSize_ggplot.pdf', height=5, width=6) #Open the pdf file

p4

dev.off() #Close the pdf file



