knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, 
		      fig.height = 5, fig.keep = 'all', fig.path = "./figure/",
		      dev = 'png')

install_load <- function(pkgs) {
	# Find which packages are not yet installed
	new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
	
	# Install missing ones
	if (length(new_pkgs)) {
		install.packages(new_pkgs, dependencies = TRUE)
	}
	
	# Load all packages
	sapply(pkgs, require, character.only = TRUE)
}

# Use the function
install_load(c("tidyverse"))

# 
# filedest <- getwd()
# bunzip2("repdata_data_StormData.csv.bz2", remove = FALSE, destname = filedest)
# SD <- read.csv("repdata_data_StormData.csv.bz2")
# 
# Names <- names(StormData)
# StormData$Event.Type <- as.factor(StormData$Event.Type)
# class(StormData$Event.Type)
# class(StormData$Deaths)
# 
# DeathEvents <- with(StormData, aggregate(Deaths~Event.Type, FUN = 'sum' ))
# DeathEvents100 <- DeathEvents[DeathEvents$Deaths > 100,]



StormDataOriginal <- data.table::fread("repdata_data_StormData.csv.bz2", 
				       select = c(2,7,8,23,24,25,26,27,28))
names_vect <- c("Date", "State", "Event.Type", "Deaths", 
		"Injuries", "Prop.Dam.Significand", "Prop.Dam.Exponent",
		"Crop.Dam.Significand", "Crop.Dam.Exponent")

StormData <- StormDataOriginal
colnames(StormData) <- names_vect

StormData$Date <- as.Date(StormData$Date, format = "%m/%d/%Y")
StormData$Event.Type <- as.factor(StormData$Event.Type)
StormData$Year <- with(StormData, format(Date, "%Y"))
StormData$Year <- as.numeric(SD$Year)

StormData <- StormData[StormData$Year >= 1993]


Dam.Exponent <- union(StormData$Prop.Dam.Exponent,StormData$Crop.Dam.Exponent)

Dam.Values <- c( 1e3, 1e6, 1, 1e9, 1e6, 1, 1, 1e5, 
		      1e6, 1, 1e4, 1e2, 1e3, 1e2, 1e7, 
		      1e2, 1, 10, 1e8, 1e3)
Dam.Multiplier <- cbind(Dam.Exponent, Dam.Values)

StormData <- left_join(StormData, Dam.Multiplier, 
		       join_by("Prop.Dam.Exponent" == "Dam.Exponent"), 
		       copy = TRUE)
colnames(StormData)[11] <- "Prop.Dam.Values"
StormData <- left_join(StormData, Dam.Multiplier, 
		join_by("Crop.Dam.Exponent" == "Dam.Exponent"),
		copy = TRUE)
colnames(StormData)[12] <- "Crop.Dam.Values"
StormData$Prop.Dam.Values <- as.numeric(StormData$Prop.Dam.Values)
StormData$Crop.Dam.Values <- as.numeric(StormData$Crop.Dam.Values)

StormData$Prop.Dam.Cost <- StormData$Prop.Dam.Significand * 
			StormData$Prop.Dam.Values

StormData$Crop.Dam.Cost <- StormData$Crop.Dam.Significand * 
	StormData$Crop.Dam.Values

StormData <- subset(StormData, 
		    select = -c( Prop.Dam.Significand, 
				 Prop.Dam.Exponent,
				 Crop.Dam.Significand, 
				 Crop.Dam.Exponent,
				 Prop.Dam.Values,
				 Crop.Dam.Values))

SD <- StormData[!rowSums(StormData[, c(4,5,7,8)]) == 0,]


"
SD.Agg <- with(StormData, aggregate(
	cbind(Deaths, Injuries, Prop.Dam.Cost, Crop.Dam.Cost) ~ Year, 
	FUN = sum))
SD.Agg$Prop.Dam.Cost <- SD.Agg$Prop.Dam.Cost / (1e9)
SD.Agg$Crop.Dam.Cost <- SD.Agg$Crop.Dam.Cost / (1e9)

SD.Agg$Tot.Dam.Cost <- SD.Agg$Prop.Dam.Cost + SD.Agg$Crop.Dam.Cost
SD.Agg$Casualties <- SD.Agg$Deaths + SD.Agg$Injuries


with(SD.Agg, plot(Year, Tot.Dam.Cost, type= "o", ylim = c(0, 15000)))
with(SD.Agg, lines(Year, Prop.Dam.Cost, type = "o", col = "red", pch = 17))
with(SD.Agg, lines(Year, Crop.Dam.Cost, type = "o", col = "green", pch = 12))


with(SD.Agg, plot(Year, Casualties, type= "o",ylim = c(0, 15000) ))
with(SD.Agg, lines(Year, Deaths, type = "o", col = "red", pch = 17))
with(SD.Agg, lines(Year, Injuries, type = "o", col = "green", pch = 12))
"


# SD.Tornado <- SD$Event.Type[grepl("tornado", SD$Event.Type, ignore.case = TRUE)]
# 
# SD01 <- SD$Event.Type[grepl("Astronomical Low Tide", SD$Event.Type, ignore.case = TRUE)]






#	Original event names unique
EventTypes <- unique(SD$Event.Type)
Event.Names <- unique(SD$Event.Type)
df0 <- EventTypes
#	48 acceptable variables as laid out in section 2.1.1 of the 
#	Storm Data Documentation. 
Events<- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood",
	"Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought",
	"Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill",
	"Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog",
	"Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind",
	"Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood",
	"Lightning C", "Marine Hail", "Marine High Wind", "Marine Strong Wind",
	"Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", 
	"Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
	"Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash",
	"Waterspout", "Wildfire", "Winter Storm", "Winter Weather") 

EventAction <- c("Low Tide", "Avalanche", "Blizzard", "Flood",
		 "Chill", "Flow", "Fog", "Smoke", "Drought",
		 "Dust Devil", "Storm", "Heat", "Chill",
		 "Flood", "Flood", "Frost", "Freeze", "Cloud", "Fog",
		 "Hail", "Heat", "Rain", "Snow", "Surf", "Wind",
		 "Hurricane", "Typhoon", "Storm", "Snow", "Flood",
		 "Lightning", "Hail", "Wind", "Wind",
		 "Wind", "Current", "Seiche", "Sleet", 
		 "Storm", "Surge","Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
		 "Depression", "Storm", "Tsunami", "Ash",
		 "Waterspout", "Wildfire", "Storm", "Weather") 


EventDescriptor <-  c("Astronomical", "Coastal",
		      "Cold", "Debris", "Dense", "Dense", "Drought",
		      "Dust", "Dust", "Excessive", "Extreme",
		      "Flash", "Funnel", "Freezing",
		      "Heavy", "Heavy", "High", "High",
		      "Ice", "Lake-Effect", "Lakeshore",
		      "Lightning C", "Marine", "Marine High", "Marine Strong",
		      "Marine Thunderstorm", "Rip", 
		      "Strong", "Thunderstorm", 
		      "Tropical", "Tropical", "Volcanic",
		      "Winter", "Winter") 



EventWords <- c("Astronomical", "Low", "Tide", "Avalanche", "Blizzard", "Coastal", 
		"Flood", "Cold", "/", "Wind", "Chill", "Debris", "Flow", "Dense", 
		"Fog", "Dense", "Smoke", "Drought", "Dust", "Devil", "Dust", 
		"Storm", "Excessive", "Heat", "Extreme", "Cold/Wind", "Chill",
		"Flash", "Flood", "Flood", "Frost/Freeze", "Funnel", "Cloud", 
		"Freezing", "Fog", "Hail", "Heat", "Heavy", "Rain", "Heavy", 
		"Snow", "High", "Surf", "High", "Wind", "Hurricane", "Typhoon", 
		"Ice", "Storm", "Lake-Effect", "Snow", "Lakeshore", "Flood",
		"Lightning", "C", "Marine", "Hail", "Marine", "High", "Wind", 
		"Marine", "Strong", "Wind", "Marine", "Thunderstorm", "Wind", 
		"Rip", "Current", "Seiche", "Sleet", "Storm", "Surge", "/", 
		"Tide", "Strong", "Wind", "Thunderstorm", "Wind", "Tornado", 
		"Tropical", "Depression", "Tropical", "Storm", "Tsunami", 
		"Volcanic", "Ash", "Waterspout", "Wildfire", "Winter", "Storm", 
		"Winter", "Weather") 

WordTable <- table(EventWords)

WT <- as.data.frame(WordTable)

WT1 <- WT[WT$Freq == 1, ]

df <- data.frame()
df_list <- list()

for(words in Events) {
	#print(words)
	
	df_list[[words]] <- data.frame(EventTypes[grepl(words, EventTypes, ignore.case = TRUE)])
	
	
	EventTypes <- EventTypes[grep(words, EventTypes, ignore.case = TRUE, invert = TRUE)]
	

	
	}




UniqueEW <- unique(EventWords)







#table(StormDataOriginal$PROPDMGEXP)