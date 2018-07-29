# SCS meeting 11/20/2017

library(vegan)

#### import data ####
dat_com <- read.csv("F:/CCA/CCA_communityG_1120.csv", header=TRUE)
rownames(dat_com) <- dat_com[, 1]
dat_com <- dat_com[, -1] 
dat_env <- read.csv("F:/CCA/CCA_environmental_1120.csv", header=TRUE)
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
anova(mycca2, by="axis") #axis significance

summary(mycca2, axes=0)


plot(mycca2, type="none", scaling=3) #plot blank CCA that is scaled to sites (samples)          
points(mycca2, display="cn", col="blue", pch=2) #add environmental variables arrows
text(mycca2, display="bp", col="blue", cex=1) # add test to arrows


?plot.cca
plot(mycca, type="none")
points(mycca, display="sites")
# text(mycca, display="sites")
points(mycca, display="species", col="red", pch=16)
points(mycca, display="cn", col="blue", pch=3)
text(mycca, display="bp", col="blue", cex=0.55)
text(mycca, display="species", col="orange", cex=0.55)

cor(dat_env$Nitrate, dat_env$B.glucosidase_Activity)
cor(dat_env$Ammonium, dat_env$DCD)

# different scalings
# 2=species, 1=samples, 3=both, 0=raw
plot(mycca, type="none", scaling=3)
points(mycca, display="sites", scaling=3)
# text(mycca, display="sites")
points(mycca, display="species", col="red", pch=16, scaling=3)
points(mycca, display="cn", col="blue", pch=3, scaling=3)
text(mycca, display="species", col="blue", cex=0.75, scaling=3)

# add specific labels
# To be filled in...

# some other functionality of the package vegan
ordistep(mycca)
anova(mycca)
anova(mycca, by="margin")
anova(mycca, by="term")
anova(mycca, by="axis")

vif.cca(newcca) #variance inflation factor. This is supposed to be < 1.95, indicating that the variables are not redundant with each other

goodness(newcca, addprevious=TRUE, summ=TRUE)
inertcomp(mycca, stat="d")

mycca

newcca <- cca(dat_com ~ RM + Cover + Nitrate + Moisture + DCD + GLU, data =
                dat_env)
summary(newcca)
summary(newcca, axes=0)
plot(newcca, type="none")
plot(newcca, scaling=1)
points(newcca, scaling=1, display="cn")

plot(newcca, type="none")
plot(newcca, scaling=1, type="none")
text(newcca, display="bp", col="blue")

newcca
anova(newcca, by="term")

# 12/7/2017
summary(mycca) # to check the inertia explained
scores(newcca, display="bp") # to check relationship between variables and CCA1, CCA2
# to check the relationship between B.glucosidase_Activity and Nitrate
pairs(dat_env[, -c(1, 2, 6)]) # no strong linear relationship there
str(summary(mycca))$biplot
summary(mycca)

# adjust the plot

# adjust the scales (caution!)
plot(mycca, type="none", scaling=1)
points(mycca, display="sp", scaling=1, pch=16)
points(mycca, display="sp", scaling=2, pch=16, col="red")
points(mycca, display="sp", scaling=3, pch=16, col="blue")
points(scores(mycca, display="sp", scaling=3)*2, pch=16, col="green")

# color samples by a categorical variable
plot(mycca, type="none")
# points(mycca, display="wa")
points(scores(mycca, display="wa"), pch=16, col=dat_env$DCD)
legend("topleft",
       pch = rep(16, length(levels(dat_env$DCD))),
       col = 1:5, # as.numeric(levels(dat_env$DCD)),
       legend = levels(dat_env$DCD))
# double check: the color is correct
# text(mycca, display="wa")
# cbind(scores(mycca, display="wa"), dat_env$DCD)

# add environmental variables
mycca_copy <- mycca
mycca_copy$CCA$biplot <- mycca_copy$CCA$biplot[c(3, 4, 5, 6, 7, 8, 9, 10, 11), ]
text(mycca_copy, display="bp", cex=0.75)

# add labels for species
points(mycca, display="sp", pch=2)
idx.species <- c(3, 5)
text(scores(mycca, display="sp")[idx.species, ], 
     colnames(dat_com)[idx.species],
     cex=0.75)
