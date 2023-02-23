rm(list = ls())



##### need these libraries ########################################################################

library(reshape2)
library(ggplot2)
library(lme4)
library(plyr)
library(sjPlot)



##### some functions ##############################################################################

se <- function(x) {
	# standard standard error function
	sd(x)/sqrt(length(x))
}

meanX <- function(x) {
	# mean function with NAs removed
	mean(x, na.rm = TRUE)
}

meanplus <- function(x) { 
	# returns mean plus standard error
	mean(x) + se(x) 
}

meanminus <- function(x) { 
	# returns mean minus standard error
	mean(x) - se(x) 
}



##### get data ####################################################################################

# male sexual color signals and male cognitive performance data:

# read in male cognition and sexual signals file "MaleCognitionColor.csv"
d <- read.csv(file1 <- file.choose())



##### Change in Barrier Test Performance Over Time ################################################

# Figure 2A, Proportion Males Solving
quartz(width = 6, height = 6)
font <- 16
dEnter <- melt(d[c("Male.ID", "MaleEnter1", "MaleEnter2", "MaleEnter4", "MaleEnter7")],
	id.vars = c("Male.ID"))
cEnter <- ggplot(dEnter, aes(variable, value))
cEnter <- cEnter + 
	stat_summary(fun.y = mean, fun.ymin = meanminus, fun.ymax = meanplus, 
		geom = "pointrange", position = "identity", color = "black", size = 1.25) +
	stat_summary(fun.y = mean, geom = "line", position = "identity", 
		color = "black", size = 1.25, lty = 2, aes(group = 1)) + 
	ylab("Proportion Males Solving") + xlab("") +
	scale_x_discrete(labels = c("Day 1", "Day 2", "Day 4", "Day 7")) +
	theme_bw() +
	theme(axis.title = element_text(size = font), axis.text = element_text(size = font * 0.75)) +
	geom_text(aes(1, 0.99, label = "A"), size = font)
cEnter

# Figure 2B, Entries / Attempts
dEnterPerAttempt <- melt(d[c("Male.ID", "ASIN.SQRT.Solves.AttemptsDay1", 
	"ASIN.SQRT.Solves.AttemptsDay2", "ASIN.SQRT.Solves.AttemptsDay4", 
	"ASIN.SQRT.Solves.AttemptsDay7")],
	id.vars = c("Male.ID"))
cEnterPerAttempt <- ggplot(dEnterPerAttempt, aes(variable, value))
cEnterPerAttempt <- cEnterPerAttempt +
	stat_summary(fun.y = mean, fun.ymin = meanminus, fun.ymax = meanplus, 
		geom = "pointrange", position = "identity", color = "black", size = 1.25) +
	stat_summary(fun.y = mean, geom = "line", position = "identity", 
		color = "black", size = 1.25, lty = 2, aes(group = 1)) + 
	theme_bw() + 	
	scale_x_discrete(labels = c("Day 1", "Day 2", "Day 4", "Day 7")) +
	ylab("Entries / Attempts (arcsin square-root transformed)") + xlab("") +
	theme(axis.title = element_text(size = font), axis.text = element_text(size = font * 0.75)) +
	geom_text(aes(1, 0.59, label = "B"), size = font)
cEnterPerAttempt

# Figure 2C, Time to Enter
dTime <- melt(d[c("Male.ID", "LogTime1", "LogTime2", "LogTime4", "LogTime7")],
	id.vars = c("Male.ID"))
cTime <- ggplot(dTime, aes(variable, value))
cTime <- cTime +
	stat_summary(fun.y = mean, fun.ymin = meanminus, fun.ymax = meanplus, 
		geom = "pointrange", position = "identity", color = "black", size = 1.25) +
	stat_summary(fun.y = mean, geom = "line", position = "identity", 
		color = "black", size = 1.25, lty = 2, aes(group = 1)) + 
	theme_bw() + 	
	scale_x_discrete(labels = c("Day 1", "Day 2", "Day 4", "Day 7")) +
	ylab("Time to Enter (log ms)") + xlab("") +
	theme(axis.title = element_text(size = font), axis.text = element_text(size = font * 0.75)) +
	geom_text(aes(1, 5.6, label = "C"), size = font)
cTime


# repeated-measures ANOVA

# Proportion Males Solving
lmermodel <- glmer(value ~ as.numeric(variable) + (as.numeric(variable) | Male.ID), 
	family = "binomial", data = dEnter, 
	control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))  
lmermodel.0 <- glmer(value ~ (1 | Male.ID), 
	family = "binomial", data = dEnter, 
	control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))
anova(lmermodel, lmermodel.0)

# Entries / Attempts
lmermodel <- lmer(value ~ as.numeric(variable) + (as.numeric(variable) | Male.ID), 
	data = dEnterPerAttempt)  
lmermodel.0 <- lmer(value ~ (1 | Male.ID), data = dEnterPerAttempt) 
anova(lmermodel, lmermodel.0)

t.test(d$ASIN.SQRT.Solves.AttemptsDay1, d$ASIN.SQRT.Solves.AttemptsDay7, paired = TRUE)

# Time to Enter
lmermodel <- lmer(value ~ as.numeric(variable) + (as.numeric(variable) | Male.ID), data = dTime)
lmermodel.0 <- lmer(value ~ (1 | Male.ID), data = dTime)  
anova(lmermodel, lmermodel.0)


# create slope variables
x <- seq(1, 4)
for (i in 1:nrow(d)) {
	y <- c(d$LogTime1[i], d$LogTime2[i], d$LogTime4[i], d$LogTime7[i])
	d$TimeSlope[i] <- lm(y ~ x)$coefficients[2]
}

for (i in 1:nrow(d)) {
	y <- c(d$ASIN.SQRT.Solves.AttemptsDay1[i], d$ASIN.SQRT.Solves.AttemptsDay2[i], 
		d$ASIN.SQRT.Solves.AttemptsDay4[i], d$ASIN.SQRT.Solves.AttemptsDay7[i])
	d$ASINSlope[i] <- lm(y ~ x)$coefficients[2]
}



##### Cognition PCs, Table 1 ######################################################################

# PSA PCA
PC_PSA <- prcomp(~ MaleEnter1 + ASIN.SQRT.Solves.AttemptsDay1 + LogTime1, 
	data = d, scale. = TRUE, center = TRUE)
PC_PSA
summary(PC_PSA)
# plot of eigenvalues
plot(PC_PSA)

d$PC1_PSA <- PC_PSA$x[, 1] * (-1) 
# I multiply by -1 to swap the signs of the loadings to make it have a more intuitive direction,
# i.e., higher values of PC1 have better problem solving abiltiy this way.


# Learning Slopes PCA
PC_LearnSlopes <- prcomp(~ ASINSlope + TimeSlope, data = d, scale. = TRUE, center = TRUE)
PC_LearnSlopes
summary(PC_LearnSlopes)
# plot of eigenvalues
plot(PC_LearnSlopes)

d$PC1_LearnSlopes <- PC_LearnSlopes$x[, 1]



##### Color Score Aggregate Measures ##############################################################

d$AvgPreMCThroatCombined <- 
	apply(d[c("MCPreThroatCombined_a", "MCPreThroatCombined_b")], 1,
		function(x) mean(x, na.rm = TRUE))

d$AvgPreMCEyeIntensity <- apply(d[c("MCPreEyeIntensity_a", "MCPreEyeIntensity_b")], 1,
		function(x) mean(x, na.rm = TRUE))

d$AvgPostMCThroatCombined <- apply(d[c("MCPostThroatCombined_a", "MCPostThroatCombined_b")], 1,
		function(x) mean(x, na.rm = TRUE))

d$AvgPostMCEyeIntensity <- apply(d[c("MCPostEyeIntensity_a", "MCPostEyeIntensity_b")], 1,
		function(x) mean(x, na.rm = TRUE))

d$AvgMCThroatCombined <- 
	apply(d[c("AvgPreMCThroatCombined", "AvgPostMCThroatCombined")], 1,
		function(x) mean(x, na.rm = TRUE))

d$AvgMCEyeIntensity <- 
	apply(d[c("AvgPreMCEyeIntensity", "AvgPostMCEyeIntensity")], 1,
		function(x) mean(x, na.rm = TRUE))



##### Table 2, Relationship between male color signals and cognitive performance ##################

# PC1_PSA ~ Color Signals
model <- lm(PC1_PSA ~ Weight + LOGBM1 + CompleteNest + AvgMCThroatCombined + AvgMCEyeIntensity, data = d)
summary(model)

# Residuals from model that only includes neophobia, will need this later
d$inhib <- lm(PC1_PSA ~ LOGBM1, data = d)$residuals


# PC1_LearnSlopes ~ Color Signals
model <- lm(PC1_LearnSlopes ~ Weight + LOGBM1 + CompleteNest + AvgMCThroatCombined + AvgMCEyeIntensity, data = d)
summary(model)


# NumbertoSolve ~ Color Signals
model <- lm(NumbertoSolve ~ Weight + LOGBM1 + CompleteNest + AvgMCThroatCombined + AvgMCEyeIntensity, data = d)
summary(model)


##### get more data ###############################################################################

# female choice data, including male nest and courtship vigor data:

# read in female choice file "FemaleChoice.csv"
d_fem <- read.csv(file2 <- file.choose())

# merge this data with data about male cognitive performance
d_all <- merge(d_fem, d[c("Male.ID", "inhib", "PC1_PSA", "PC1_LearnSlopes", "NumbertoSolve", 
	"Weight", "LOGBM1", "CompleteNest")], 
	by.x = "MaleID", by.y = "Male.ID", suffixes = c(".F",".M"))
# this makes a new data frame with everything averaged by Male ID
d_all2 <- melt(d_all[c("MaleID", "Trial", "inhib", "PC1_PSA", "PC1_LearnSlopes", "NumbertoSolve", 
	"Weight.M", "LOGBM1", "CompleteNest", "NestArea", "CourtshipVigor.Sec")], 
id = c("MaleID", "Trial"))
d_all3 <- dcast(d_all2, MaleID ~ variable, meanX)



##### Table 3, Relationship between male nest area and cognitive performance ##################

model <- lm(PC1_PSA ~ Weight.M + LOGBM1 + CompleteNest + NestArea, data = d_all3)
summary(model)

model <- lm(PC1_LearnSlopes ~ Weight.M + LOGBM1 + CompleteNest + NestArea, data = d_all3)
summary(model)

model <- lm(NumbertoSolve ~ Weight.M + LOGBM1 + CompleteNest + NestArea, data = d_all3)
summary(model)



##### Table 4, Relationship between male courtship vigor and cognitive performance ################

model <- lm(PC1_PSA ~ Weight.M + LOGBM1 + CompleteNest + CourtshipVigor.Sec, data = d_all3)
summary(model)

model <- lm(PC1_LearnSlopes ~ Weight.M + LOGBM1 + CompleteNest + CourtshipVigor.Sec, data = d_all3)
summary(model)

model <- lm(NumbertoSolve ~ Weight.M + LOGBM1 + CompleteNest + CourtshipVigor.Sec, data = d_all3)
summary(model)



##### Table 5, Relationship between male cognitive performance and mating success #################

# I had to scale all the variables or glmer got really mad (this means the mean = 0 and sd = 1 for all variables)
scaled <- scale(d_all[c("PC1_PSA", "inhib", "PC1_LearnSlopes", "NumbertoSolve", 
	"Weight.M", "ThroatCombined", "EyeIntensity", "LOGBM1", "CourtshipVigor.Sec", "NestArea", 
	"CompleteNest")])

# add in trial information
scaled <- cbind(scaled, d_all[c("Enter.", "MaleID", "FemaleID", "Trial")])


# inhibitory control
Model1 <- glmer(Enter. ~ inhib + ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), 
	family = "binomial", data = scaled, 
	control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

Model1.null <- glmer(Enter.~ ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), 
	family = "binomial", data = scaled, 
	control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

anova(Model1, Model1.null)


# Learning Slopes
Model2 <- glmer(Enter. ~ PC1_LearnSlopes + ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), family = "binomial", data = scaled, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

Model2.null <- glmer(Enter. ~ ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), family = "binomial", data = scaled, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

anova(Model2, Model2.null)


# Number of Trials
Model3 <- glmer(Enter. ~ NumbertoSolve + ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), family = "binomial", data = scaled, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

Model3.null <- glmer(Enter. ~ ThroatCombined + EyeIntensity + Weight.M + LOGBM1 + NestArea + CourtshipVigor.Sec + CompleteNest + (1|MaleID) + (1|FemaleID), family = "binomial", data = scaled, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

anova(Model3, Model3.null)



##### Figure 3, Female choice and cognition #######################################################

# Generate data points for graph
# reorganize data
melted <- melt(scaled[c("MaleID", "Enter.", "inhib", "ThroatCombined", "EyeIntensity", "Weight.M", "LOGBM1", "NestArea", "CourtshipVigor.Sec", "CompleteNest")], id.vars = c("MaleID"))
# calculate means for 2 trials
means <- ddply(melted, c("MaleID", "variable"), summarise, mean = mean(value))
# reorganize again, back into original format
newd <- dcast(data = means, formula = MaleID ~ variable, fun.aggregate = sum, value.var = "mean")

# Marginal effect of inhibitory control on Enter, with average value for Enter plotted as data points
quartz(width = 6, height = 6)
font <- 16
c <- sjp.glmer(Model1, type = "eff", vars = c("inhib"), show.ci = TRUE, prnt.plot = FALSE, facet.grid = FALSE, geom.size = 2)
c$plot + xlab("Inhibitory Control") + ylab("Probability of Female Acceptance") + theme_bw() + theme(axis.title = element_text(size = font), 
		axis.text = element_text(size = font * 0.75), plot.title = element_text(size = 0)) +
  	scale_y_continuous(limits = c(-0.05, 1.05), breaks = c(0, 0.25, 0.50, 0.75, 1), 
  		labels = c("0%", "25%", "50%", "75%", "100%")) + 
  	# comment next line two lines and add last line if want to have jittered points instead (and not shaded points)
  	geom_point(data = newd, aes(inhib, Enter.), size = 5, pch = 19, alpha = 0.2) + 
  	geom_point(data = newd, aes(inhib, Enter.), size = 5, pch = 21)
	# geom_jitter(data = newd, aes(inhib, Enter.), size = 5, pch = 21, width = 0, height = 0.1)




