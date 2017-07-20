# CHAPTER 10. GENERALISED LINEAR MODELS

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
# Newly added: Boxplot and Barplot using ggplot2
################################################

# 1) Load the data
colonies <- read.csv("../Data/PracData.csv")
str(colonies)

# 2) Look at all the data
boxplot(ColonyCount ~ Treatment, data=colonies)

colonies$logCC <- log(colonies$ColonyCount + 1)
boxplot(logCC ~ Treatment, data=colonies)

# 3) Check the variation i colony count across treatments
tapply(colonies$ColonyCount, colonies$Treatment, min, na.rm=TRUE)
tapply(colonies$ColonyCount, colonies$Treatment, max, na.rm=TRUE)

# 4) Subset the data down to control and NG
coloniesCN <- subset(colonies, Treatment %in% c('Control', 'NG'), drop=TRUE)
str(coloniesCN)
# remove the unused levels
coloniesCN <- droplevels(coloniesCN)
str(coloniesCN)

# 5a) Look at the strains by treament
library(lattice)
# pdf('../Results/PracDataBoxplot.pdf', height=5, width=6) #Open the pdf file
bwplot(logCC ~ Strain | Treatment, data=coloniesCN)
# dev.off()

# alternatively, get a barplot
tab <- tapply(coloniesCN$ColonyCount, list(coloniesCN$Treatment,
	          coloniesCN$Strain), mean, na.rm=TRUE)
# pdf('../Results/PracDataBarplot.pdf', height=5, width=6)
barplot(tab, beside=TRUE)
# dev.off()

######################################################
# 5b) Plot boxplot using ggplot2 # Newly Added
library(ggplot2)

p1 <- ggplot(coloniesCN, aes(x = Strain, y = logCC, color = Treatment)) +   # color by Treatment
  stat_boxplot(geom = 'errorbar') +                                         # add whiskers to boxplots
  geom_boxplot(aes(fill = Treatment), lwd = 0.8) +                          
  # add boxplot, fill boxplot according to color of Treatment and lwd dictates thickness of outlines
  scale_colour_manual(values = c('darkred', 'midnightblue')) +              # manually choose color of outline and outliers
  scale_fill_manual(values = c('palevioletred3', 'skyblue4'))               # manually choose fill color
p1

# set text for axis
p2 <- p1 + labs(x = "Strains", y = "log Colony Count")

p2

# save plot as pdf
ggsave('../Results/PracDataBoxplot_ggplot.pdf', height=5, width=6) 

# OR multi-faceted plots
p3 <- p2 + facet_grid(Treatment ~ .)
p3

ggsave('../Results/PracDataBoxplot_ggplot2.pdf', height=5, width=6)

#############################################################
# 5b) Plot barplot using ggplot2 # Newly Added
# ggplots do not read matrix, need to convert data to dataframe of means
library(reshape2)
library(plyr)

# check out ?melt
melted <- melt(coloniesCN, id.vars=c("Student.ID", "Strain", "Treatment"), # identifying measures
               measure.vars=c("logCC"), na.rm = TRUE) # value = value of logCC, remove NAs
head(melted)

# find means
means <- ddply(melted, c("Strain", "Treatment"), summarise, # find mean of value/logCC grouped by strain and treatment
               mean=mean(value))

means

# find standard error to create error bars
means.sem <- ddply(melted, c("Strain", "Treatment"), summarise,
                   mean=mean(value), sem=sd(value)/sqrt(length(value)))
means.sem
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem) # find upper and lower bound
means.sem

# plot using ggplots
barplot1 <- ggplot(means, aes(x = Strain, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position=position_dodge(), # add barplots, unstacked/dodged barplot
           color = "black",                              # add black outlines
           lwd = 0.8)                                    # increase thickness of outline
barplot1

# plot errorbars
barplot2 <- barplot1 + geom_errorbar(aes(ymax=upper, ymin=lower),  # span error bar from upper to lower
                                     position=position_dodge(0.9), # unstacked, dodged barplot
                                     width = 0.5,                  # narrower errorbars
                                     lwd = 0.8,                    # increase thickness of lines
                                     data=means.sem)               # using data from means.sem

barplot2 

# lets change the colours
barplot3 <-  barplot2 + 
  scale_fill_manual(values = c('palevioletred3', 'skyblue4'))      # manually choose fill color
barplot3

# add mean values to barplots
mean.labels <- round(means$mean, 2)             # round means up to 2 dp for display
barplot4 <- barplot3 +
  geom_text(aes(label=mean.labels), vjust=1.2,  # vjust: adjust height of text from mean
            color="white",                      # color of text
            position = position_dodge(0.9),     # position of text from center
            size=3, fontface = "bold")          # size and bold text

barplot4

# set text and text size for axis
barplot5 <- barplot4 + labs(x = "Strains", y = "log Colony Count")

barplot5

# save plot as pdf
ggsave('../Results/PracDataBarplot_ggplot.pdf', height=5, width=6) #Open the pdf file

######################################################

# 6) A test linear model
modLM <- lm(logCC ~ Strain * Treatment, data=coloniesCN)
par(mfrow=c(2,2))
plot(modLM)

# 7) A Poisson glm
modPois <- glm(ColonyCount ~ Strain * Treatment, data=coloniesCN, family='poisson')
summary(modPois)

par(mfrow=c(2,2))
plot(modPois)

# 8) A quasipoisson glm
modQPois <- glm(ColonyCount ~ Strain * Treatment, data=coloniesCN, family='quasipoisson')
summary(modQPois)
anova(modQPois, test='F')
plot(modQPois)

# 9) Simplification?
drop.scope(modQPois)
modQPois2 <- update(modQPois, . ~ . - Strain:Treatment)
anova(modQPois, modQPois2, test='F')

# 10) predictions
df <- expand.grid(Strain=levels(coloniesCN$Strain), 
                  Treatment=levels(coloniesCN$Treatment))
predict(modQPois, newdata=df, type='response')


# 10) halo

table(Halo=colonies$HaloLawn, Strain=colonies$Strain, Treatment=colonies$Treatment)

