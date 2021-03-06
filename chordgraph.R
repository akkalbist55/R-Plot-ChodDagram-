#Create data
name = c(3,10,10,3,6,7,8,3,6,1,2,2,6,10,2,3,3,10,4,5,9,10)
feature = paste("feature ", c(1,1,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5) , sep="")
dat <- data.frame(name,feature)
dat <- with(dat, table(name, feature))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(as.data.frame(dat), transparency = 0.5)

