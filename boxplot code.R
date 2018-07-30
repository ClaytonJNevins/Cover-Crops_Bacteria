#Clayton Nevins
#Code for creating boxplots for soil B-glucosidase activity

library(ggplot2)

data_GLU <- read.csv("F:/M.Sc/Research and Data/SARE Project/Box Plots for 2016 Enzyme Data-R/2016 GLU NT.csv", header=TRUE)


ggplot(data_GLU,aes(x=DCD, y=Activity, fill=Treatment)) + geom_boxplot(color="black",alpha=0.2, outlier.shape=NA)

bg <- ggplot(data_GLU,aes(x=DCD, y=Activity, fill=Treatment)) + geom_boxplot(outlier.color=NA) + xlab("Decomposition Degree Day") + ylab("Soil Beta-glucosidase Activity") + theme_bw()

bg2 <- bg + theme(axis.text = element_text(color="black"))
bg2

bg3 <- bg2 + scale_fill_manual(breaks = c("Control", "CR", "HV", "HV/CR"), 
                               values=c("blue3", "red", "goldenrod", "green4"))

newbg <- bg3 + coord_cartesian(ylim=c(0,11)) + theme(axis.text = element_text(color="black"))
newbg

newbg1 <- newbg + geom_boxplot(outlier.shape = NA)
newbg1

#Finding Means for graph

byDCD <- split(data_GLU, paste(data_GLU$DCD))
byDCD2.7 <- byDCD[["DCD_02.7"]] 
byDCD2.7

means <- aggregate(Activity ~ Treatment, byDCD2.7, mean)
means

##

byDCD6.4 <- byDCD[["DCD_06.4"]] 
means <- aggregate(Activity ~ Treatment, byDCD6.4, mean)
means

byDCD10.5 <- byDCD[["DCD_10.5"]] 
means <- aggregate(Activity ~ Treatment, byDCD10.5, mean)
means

byDCD17.5 <- byDCD[["DCD_17.5"]] 
means <- aggregate(Activity ~ Treatment, byDCD17.5, mean)
means

byDCD27.9 <- byDCD[["DCD_27.9"]] 
means <- aggregate(Activity ~ Treatment, byDCD27.9, mean)
means
