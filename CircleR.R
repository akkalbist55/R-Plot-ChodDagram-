name=c(3,10,10,3,6,7,8,3,6,1,2,2,6)
feature=paste("feature ", c(1,1,2,2,2,2,2,3,3,3,3,3,3,3) , sep="")
dat <- data.frame(name,feature)
dat <- with(dat, table(name, feature))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(as.data.frame(dat), transparency = 0.5)
