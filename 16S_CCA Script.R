# SCS meeting 11/20/2017

library(vegan)

#### import data ####
dat_com <- read.csv("F:/CCA/CCA_communityG_1120.csv", header=TRUE)
rownames(dat_com) <- dat_com[, 1]
dat_com <- dat_com[, -1] 
dat_env <- read.csv("F:/CCA/CCA_environmental_1121.csv", header=TRUE)
rownames(dat_env) <- dat_env[, 1]
dat_env <- dat_env[, -1]
dat_env <- dat_env[, -5] #remove InorganicN
dat_env <- dat_env[, -4] #ammonium
dat_env <- dat_env[, -3] #nitrate
dat_env <- dat_env[, -9]

mycca <- cca(dat_com ~ ., data=dat_env) # formula can be specified (writing out explicitly the variable names)
plot(mycca) # default
plot(mycca, display=c("sp", "wa", "cn"))
# plot(mycca, display=c("sp", "wa", "bp"))

ordistep(mycca)

dat_env <- dat_env[, -6] 
dat_env <- dat_env[, -6]
dat_env <- dat_env[, -6]

mycca2 <- cca(dat_com ~ ., data=dat_env)

vif.cca(mycca2)

anova(mycca2) #testing for overall significance of the model
summary(mycca2) #determining the overall constrained variation captured by the model
anova(mycca2, by="term") #environmental variable significance

summary(mycca2, axes=0)


plot(mycca2, type="none", scaling=3) #plot blank CCA that is scaled to sites (samples)          
points(mycca2, display="cn", col="blue", pch=2) #add environmental variables arrows
text(mycca2, display="bp", col="blue", cex=1) # add test to arrows
