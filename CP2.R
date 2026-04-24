knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, 
		      fig.height = 5, fig.keep = 'all', fig.path = "./figure/",
		      dev = 'png')


install.packages("R.utils")
library(R.utils)



filedest <- getwd()
bunzip2("repdata_data_StormData.csv.bz2", remove = FALSE, destname = filedest)
StormData <- read.csv("repdata_data_StormData.csv.bz2")

Names <- names(StormData)
StormData$EVTYPE <- as.factor(StormData$EVTYPE)
class(StormData$EVTYPE)
class(StormData$FATALITIES)

DeathEvents <- with(StormData, aggregate(FATALITIES~EVTYPE, FUN = 'sum' ))
DeathEvents100 <- DeathEvents[DeathEvents$FATALITIES > 100,]











