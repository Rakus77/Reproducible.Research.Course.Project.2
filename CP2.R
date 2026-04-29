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
install_load(c("tidyverse", "scales"))





StormDataOriginal <- data.table::fread("repdata_data_StormData.csv.bz2", 
				       select = c(2,7,8,23,24,25,26,27,28))
names_vect <- c("Date", "State", "Event.Type", "Deaths", 
		"Injuries", "Prop.Dam.Significand", "Prop.Dam.Exponent",
		"Crop.Dam.Significand", "Crop.Dam.Exponent")

StormData <- StormDataOriginal
colnames(StormData) <- names_vect

StormData$Date <- as.Date(StormData$Date, format = "%m/%d/%Y")
StormData$Year <- with(StormData, format(Date, "%Y"))
StormData$Year <- as.numeric(StormData$Year)

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

StormData$Prop.Dam.Cost <- StormData$Prop.Dam.Cost / (1e12)
StormData$Crop.Dam.Cost <- StormData$Crop.Dam.Cost / (1e12)

StormData <- subset(StormData, 
		    select = -c( Prop.Dam.Significand, 
				 Prop.Dam.Exponent,
				 Crop.Dam.Significand, 
				 Crop.Dam.Exponent,
				 Prop.Dam.Values,
				 Crop.Dam.Values))

SD <- StormData[!rowSums(StormData[, c(4,5,7,8)]) == 0,]





#A <- unique(SD$Event.Type[grepl("LIGHTNING|THUNDER|TSTM", SD$Event.Type )])



SD$Event.Type <- toupper(SD$Event.Type)


SD$Event.Type <- gsub(".*THUN.*|.*LIGHTNING.*|.*TSTM.*|.*LIGNTNING.*|.*LIGHTING.*|.*MICRO.*|.*DOWNBURST.*|.*TURBULENCE.*"
		      , "THUNDER/LIGHTNING", SD$Event.Type)
SD$Event.Type <- gsub(".*BLIZZARD.*", "BLIZZARD", SD$Event.Type)
SD$Event.Type <- gsub(".*SNOW.*|.*ICE.*|.*WINT.*|.*FROST.*|.*FREEZE.*|.*ICY.*|.*GLAZE.*|.*HEAVY MIX.*|.*FREEZING SPRAY.*", 
		      "ICE/SNOW", SD$Event.Type)
SD$Event.Type <- gsub(".*TORNADO.*|.*WATERSPOUT.*|.*DUST.*|.*GUSTNADO.*|.*LANDSPOUT.*|.*FUNNEL.*|.*TORNDAO.*", "TORNADO", SD$Event.Type)
SD$Event.Type <- gsub(".*HURR.*|.*TYPH.*", "HURRICANE", SD$Event.Type)
SD$Event.Type <- gsub(".*HAIL.*", "HAIL", SD$Event.Type)
SD$Event.Type <- gsub(".*FLOOD.*|.*MUD.*|.*SLIDE.*|.*DAM.*|.*RISING.*|.*EROSION.*|.*LANDSLUMP.*|.*DROWN.*|.*HIGH WATER.*"
		      , "FLOODS/DEBRIS FLOW", SD$Event.Type)
SD$Event.Type <- gsub(".*HEAT.*|.*DROUGHT.*|.*HYPER.*|.*WARM.*|.*DENSE SMOKE.*", "HEAT", SD$Event.Type)
SD$Event.Type <- gsub(".*RAIN.*|.*DRIZZ.*|.*SLEET.*|.*PRECIP.*|.*SHOWER.*|.*COOL AND WET.*|.*WETNESS.*", 
		      "RAIN/SLEET", SD$Event.Type)
SD$Event.Type <- gsub(".*FIRE.*", "WILDFIRE", SD$Event.Type)
SD$Event.Type <- gsub(".*FOG.*", "FOG", SD$Event.Type)
SD$Event.Type <- gsub(".*COLD.*|.*WIND.*|.*HYPO.*|.*LOW TEMP.*", "COLD/WIND", SD$Event.Type)
SD$Event.Type <- gsub(".*SURF.*|.*TIDE.*|.*COAST.*|.*WAVE.*|.*RIP CUR.*", "COASTAL/SURF/TIDE", SD$Event.Type)
SD$Event.Type <- gsub(".*TROPIC.*", "TROPICAL STORMS", SD$Event.Type)
SD$Event.Type <- gsub(".*OTHER.*|.*\\?.*|.*APACHE.*|.*ACCIDENT.*|.*URBAN.*|.*MISHAP.*|^HIGH$", "OTHER", SD$Event.Type)
SD$Event.Type <- gsub(".*SEA.*|.*SWELL.*|.*STORM SURGE.*", "MARINE/SEAS", SD$Event.Type)
SD$Event.Type <- gsub(".*AVALAN.*", "AVALANCHE", SD$Event.Type)

(unique(SD$Event.Type))



SD$Casualties <- SD$Deaths + SD$Injuries
SD$Dam.Cost <- (SD$Prop.Dam.Cost + SD$Crop.Dam.Cost)

SD.Agg.Cas <- aggregate(SD$Casualties~SD$Event.Type, FUN = sum)
names(SD.Agg.Cas) <- c("Event", "Casualties")
SD.Agg.Dam <- aggregate(SD$Dam.Cost~SD$Event.Type, FUN = sum)
names(SD.Agg.Dam) <- c("Event", "Damage.Cost")




SD.Agg.DI <- aggregate(cbind(SD$Deaths,SD$Injuries)~SD$Event.Type, FUN = sum)
names(SD.Agg.DI) <- c("Event", "Deaths", "Injuries")
SD.Agg.DI$Cas <- SD.Agg.DI$Deaths + SD.Agg.DI$Injuries
SD.Agg.DI <- slice_max(.data = SD.Agg.DI, order_by = SD.Agg.DI$Cas, n = 10)



SD.Agg.PC <- aggregate(cbind(SD$Prop.Dam.Cost,SD$Crop.Dam.Cost)~SD$Event.Type, FUN = sum)
names(SD.Agg.PC) <- c("Event", "Prop.Dam", "Crop.Dam")
SD.Agg.PC$Dam.Cost <- SD.Agg.PC$Crop.Dam + SD.Agg.PC$Prop.Dam
SD.Agg.PC <- slice_max(.data = SD.Agg.PC, order_by = SD.Agg.PC$Dam.Cost, n = 10)


SD.Agg.byCas <- pivot_longer(data = SD.Agg.DI,cols = c("Deaths", "Injuries"), 
			     names_to = "Casualty.Type", values_to = "Casualties")

SD.Agg.byCas$Casualty.Type <- factor(SD.Agg.byCas$Casualty.Type, 
				     levels = c("Injuries", "Deaths"))

SD.Agg.byDam <- pivot_longer(data = SD.Agg.PC,cols = c("Prop.Dam", "Crop.Dam"), 
			     names_to = "Damage.Type", values_to = "Damage.Cost")

SD.Agg.byDam$Damage.Type <- factor(SD.Agg.byDam$Damage.Type, 
				     levels = c("Prop.Dam", "Crop.Dam"))




k <- ggplot(SD.Agg.byCas, aes(x = reorder(Event, -Casualties, sum), y = Casualties, fill = Casualty.Type))+
	geom_bar(stat = "identity")+
	xlab("Weather Event")+
	ylab("Casualties")+
	theme(plot.title = element_text(hjust = 0.5) , 
	      axis.text.x = element_text(angle = 45, vjust = 1, 
	      				hjust = 1, size = 11), legend.position = "bottom", 
					legend.title = element_blank(), 
					axis.ticks.length.x = unit(0.5, "cm"))+
	labs(title = "Casualties by Weather Event")+
	ylim(0,30000)+
	scale_x_discrete(labels = c("Tornado", "Thunder\nLightning", 
			    "Heat", "Floods\nDebris Flow", 
			    "Ice\nSnow", "Cold\nWind", 
			    "Wildfire", "Coastal\nSurf/Tide", 
			    "Hurricane", "Fog"))+
	stat_summary(fun = sum, aes(label = after_stat(prettyNum(y, big.mark = ",")), group = Event), 
		     geom = "text", vjust = -0.5)

k

l <- ggplot(SD.Agg.byDam, aes(x = reorder(Event, -Damage.Cost, sum), y = Damage.Cost, fill = Damage.Type))+
	geom_bar(stat = "identity")+
	xlab("Weather Event")+
	ylab("Damages (trillions of dollars)")+
	theme(plot.title = element_text(hjust = 0.5) , 
	      axis.text.x = element_text(angle = 45, 
	      vjust = 1, hjust = 1, size = 11), legend.position = "bottom", 
	      legend.title = element_blank(),
	      axis.ticks.length.x = unit(0.5, "cm"))+
	ylim(0, 50)+
	labs(title = "Economic Impact by Weather Event")+
	scale_x_discrete(labels = c("Floods\nDebris Flow", "Tornado", "Hail",
			"Hurricane",  "Heat", "Thunder\nLightning",
 			"Ice\nSnow", "Cold\nWind",
 			"Wildfire", "Tropical \n Storms"))+
	stat_summary(fun = sum, aes(label = dollar(round(after_stat(y), 2)), group = Event), 
		     geom = "text", vjust = -0.5)

l






SD.State <- aggregate(cbind(SD$Deaths, SD$Injuries,SD$Casualties,
			    SD$Prop.Dam.Cost, SD$Crop.Dam.Cost, 
			    SD$Dam.Cost)~SD$State, FUN = sum)
names(SD.State) <- c("State", "Deaths", "Injuries", "Property.Damage",
		     "Crop.Damage", "Total.Damage")

m <- ggplot(SD.State, aes(x = reorder(Event, -Damage.Cost, sum), y = Damage.Cost, fill = Damage.Type))+
	geom_bar(stat = "identity")+
	xlab("Weather Event")+
	ylab("Damages (trillions of dollars)")+
	theme(plot.title = element_text(hjust = 0.5) , 
	      axis.text.x = element_text(angle = 45, 
	      			   vjust = 1, hjust = 1, size = 11), legend.position = "bottom", 
	      legend.title = element_blank(),
	      axis.ticks.length.x = unit(0.5, "cm"))+
	ylim(0, 50)+
	labs(title = "Economic Impact by Weather Event")+
	scale_x_discrete(labels = c("Floods\nDebris Flow", "Tornado", "Hail",
				    "Hurricane",  "Heat", "Thunder\nLightning",
				    "Ice\nSnow", "Cold\nWind",
				    "Wildfire", "Tropical \n Storms"))+
	stat_summary(fun = sum, aes(label = dollar(round(after_stat(y), 2)), group = Event), 
		     geom = "text", vjust = -0.5)

m



# 
# thunder_lightning_var <- unique(Event.Names[grepl(".*THUN.*|.*LIGHTNING.*|.*TSTM.*|.*LIGNTNING.*|
# 			      .*LIGHTING.*|.*MICRO.*|.*DOWNBURST.*", Event.Names )])
# blizzard_var <- unique(Event.Names[grepl(".*BLIZZARD.*", Event.Names )])
# ice_snow_var <- unique(Event.Names[grepl(".*SNOW.*|.*ICE.*|.*WINT.*|.*FROST.*|.*FREEZE.*|.*ICY.*|.*GLAZE.*", Event.Names )])
# tornado_var <- unique(Event.Names[grepl(".*TORNADO.*|.*WATERSPOUT.*|.*DUST.*|.*GUSTNADO.*|.*LANDSPOUT.*", Event.Names )])
# hurricane_var <- unique(Event.Names[grepl(".*HURR.*|.*TYPH.*", Event.Names )])
# hail_var <- unique(Event.Names[grepl(".*HAIL.*", Event.Names )])
# floods_debrisflow_var <- unique(Event.Names[grepl(".*FLOOD.*|.*MUD.*|.*SLIDE.*|.*DAM.*|.*RISING.*", Event.Names )])
# heat_var <- unique(Event.Names[grepl(".*HEAT.*|.*DROUGHT.*|.*HYPER.*|.*WARM.*", Event.Names )])
# rain_sleet_var <- unique(Event.Names[grepl(".*RAIN.*|.*DRIZZ.*|.*SLEET.*|.*PRECIP.*|.*SHOWER.*", Event.Names )])
# wildfire_var <- unique(Event.Names[grepl(".*FIRE.*", Event.Names )])
# fog_var <- unique(Event.Names[grepl(".*FOG.*", Event.Names )])
# cold_wind_var <- unique(Event.Names[grepl(".*COLD.*|.*WIND.*|.*HYPO.*", Event.Names )])
# coastal_surf_tide_var <- unique(Event.Names[grepl(".*SURF.*|.*TIDE.*|.*COAST.*|.*WAVE.*", Event.Names )])
# tropicalstorms_var <- unique(Event.Names[grepl(".*TROPIC.*", Event.Names )])
# other_var <- unique(Event.Names[grepl(".*OTHER.*|.*\\?.*|.*APACHE.*|.*ACCIDENT.*|.*URBAN.*|.*MISHAP.*", Event.Names )])
# marine_seas_var <- unique(Event.Names[grepl(".*SEA.*|.*SWELL.*|.*STORM SURGE.*", Event.Names )])



# f <- ggplot(SD.Agg.Cas, aes(x = reorder(Event, -Casualties), y = Casualties, fill = "red"))+
# 	geom_bar(stat = "identity")+
# 	xlab("Weather Event")+
# 	ylab("Casualties")+
# 	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6), legend.position = "none")+
# 	labs(title = "Casualties by Weather Event")
# 
# 
# f
# 
# h <- ggplot(SD.Agg.Dam, aes(x = reorder(Event, -Damage.Cost), y = Damage.Cost, fill = "red"))+
# 	geom_bar(stat = "identity")+
# 	xlab("Weather Event")+
# 	ylab("Cost of Damage (trillions of dollars)")+
# 	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6), legend.position = "none")+
# 	labs(title = "Economic Impact by Weather Event")
# 
# h



# A <- unique(SD$Event.Type[grepl("LIGHTNING", SD$Event.Type )])




# class(other_var)
# my_list <- list(other_var, tropicalstorms_var, coastal_surf_tide_var, cold_wind_var, fog_var, wildfire_var, rain_sleet_var,
# 		heat_var, floods_debrisflow_var, hail_var, hurricane_var, tornado_var, ice_snow_var, blizzard_var, 
# 		thunder_lightning_var)
# 
# 
# common_elem <- purrr::reduce(my_list, intersect)
# 
# x <- 0
# y <- 0
# 
# for(i in my_list){
# 	x <- length(i)
# 	y <- y + x
# 	
# }
# 
# intersect(thunder_lightning_var, cold_wind_var)


# SD$Event.Type <- gsub(".*THUNDERSTORMS.*", "THUNDERSTORM", SD$Event.Type)
# SD$Event.Type <- gsub(".*WINDS.*", "WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HAIL.*", "HAIL", SD$Event.Type)
# SD$Event.Type <- gsub(".*^TORNADO.*", "TORNADO", SD$Event.Type)
# SD$Event.Type <- gsub(".*^THUNDERSTORM WIND.*", "THUNDERSTORM WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^TSTM WIND.*", "THUNDERSTORM WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^ TSTM WIND.*", "THUNDERSTORM WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^THUNDERSTORMWINDS.*", "THUNDERSTORM WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^TSTMW.*", "THUNDERSTORM WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HURRICANE.*", "Hurricane (Typhoon)", SD$Event.Type)
# SD$Event.Type <- gsub(".*^TYPHOON.*", "Hurricane (Typhoon)", SD$Event.Type)
# SD$Event.Type <- gsub(".*^SNOW.*", "HEAVY SNOW", SD$Event.Type)
# SD$Event.Type <- gsub(".*^ICE STORM.*", "ICE STORM", SD$Event.Type)
# SD$Event.Type <- gsub(".*^FLASH FLOOD.*", "FLASH FLOOD", SD$Event.Type)
# SD$Event.Type <- gsub(".*^GUSTY WIND.*", "STRONG WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HIGH WIND.*", "HIGH WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HIGH  WIND.*", "HIGH WIND", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HEAVY SNOW.*", "HEAVY SNOW", SD$Event.Type)
# SD$Event.Type <- gsub(".*^FLOOD.*", "FLOOD", SD$Event.Type)
# SD$Event.Type <- gsub(".*DROUGHT.*", "DROUGHT", SD$Event.Type)
# SD$Event.Type <- gsub(".*^LIGHTNING.*", "LIGHTNING C", SD$Event.Type)
# SD$Event.Type <- gsub(".*FIRE.*", "WILDFIRE", SD$Event.Type)
# SD$Event.Type <- gsub(".*TROPICAL STORM.*", "TROPICAL STORM", SD$Event.Type)
# SD$Event.Type <- gsub(".*^WATERSPOUT.*", "WATERSPOUT", SD$Event.Type)
# SD$Event.Type <- gsub(".*MUDSLIDE.*|.*MUD SLIDE.*|.*MUD SLIDES.*|.*ROCK SLIDE.*|
# 		      .*FLOODING/EROSION.*|.*LANDSLIDES.*|.*EROSION.*", "DEBRIS FLOW", SD$Event.Type)
# SD$Event.Type <- gsub(".*DUST.*", "DUST DEVIL", SD$Event.Type)
# SD$Event.Type <- gsub(".*HEAT.*", "HEAT", SD$Event.Type)
# SD$Event.Type <- gsub(".*COASTAL FLOOD.*", "COASTAL FLOOD", SD$Event.Type)
# SD$Event.Type <- gsub(".*^HEAVY RAIN.*", "HEAVY RAIN", SD$Event.Type)
# SD$Event.Type <- gsub(".*^RIP CURRENT.*", "RIP CURRENT", SD$Event.Type)
# SD$Event.Type <- gsub(".*SURF.*", "HIGH SURF", SD$Event.Type)
# SD$Event.Type <- gsub(".*SNOW.*", "SNOW", SD$Event.Type)
# 
# 
# WordTable <- table(EventWords)
# 
# WT <- as.data.frame(WordTable)
# 
# WT1 <- WT[WT$Freq == 1, ]
# 
# df <- data.frame()
# df_list <- list()
# 
# for(words in Events) {
# 	#print(words)
# 	
# 	df_list[[words]] <- data.frame(EventTypes[grepl(words, EventTypes, ignore.case = TRUE)])
# 	
# 	
# 	EventTypes <- EventTypes[grep(words, EventTypes, ignore.case = TRUE, invert = TRUE)]
# 	
# 
# 	
# 	}
# 
# 
# 
# 
# UniqueEW <- unique(EventWords)
# 
# 
# 
# 
# 
# 

#table(StormDataOriginal$PROPDMGEXP)



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


# 
# #	Original event names unique
# EventTypes <- unique(SD$Event.Type)
# Event.Names <- unique(SD$Event.Type)
# df0 <- EventTypes
# #	48 acceptable variables as laid out in section 2.1.1 of the 
# #	Storm Data Documentation. 
# Events<- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood",
# 	   "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought",
# 	   "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill",
# 	   "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog",
# 	   "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind",
# 	   "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood",
# 	   "Lightning C", "Marine Hail", "Marine High Wind", "Marine Strong Wind",
# 	   "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", 
# 	   "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
# 	   "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash",
# 	   "Waterspout", "Wildfire", "Winter Storm", "Winter Weather") 
# 
# EventAction <- c("Low Tide", "Avalanche", "Blizzard", "Flood",
# 		 "Chill", "Flow", "Fog", "Smoke", "Drought",
# 		 "Dust Devil", "Storm", "Heat", "Chill",
# 		 "Flood", "Flood", "Frost", "Freeze", "Cloud", "Fog",
# 		 "Hail", "Heat", "Rain", "Snow", "Surf", "Wind",
# 		 "Hurricane", "Typhoon", "Storm", "Snow", "Flood",
# 		 "Lightning", "Hail", "Wind", "Wind",
# 		 "Wind", "Current", "Seiche", "Sleet", 
# 		 "Storm", "Surge","Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
# 		 "Depression", "Storm", "Tsunami", "Ash",
# 		 "Waterspout", "Wildfire", "Storm", "Weather") 
# 
# 
# EventDescriptor <-  c("Astronomical", "Coastal",
# 		      "Cold", "Debris", "Dense", "Dense", "Drought",
# 		      "Dust", "Dust", "Excessive", "Extreme",
# 		      "Flash", "Funnel", "Freezing",
# 		      "Heavy", "Heavy", "High", "High",
# 		      "Ice", "Lake-Effect", "Lakeshore",
# 		      "Lightning C", "Marine", "Marine High", "Marine Strong",
# 		      "Marine Thunderstorm", "Rip", 
# 		      "Strong", "Thunderstorm", 
# 		      "Tropical", "Tropical", "Volcanic",
# 		      "Winter", "Winter") 
# 
# 
# 
# EventWords <- c("Astronomical", "Low", "Tide", "Avalanche", "Blizzard", "Coastal", 
# 		"Flood", "Cold", "/", "Wind", "Chill", "Debris", "Flow", "Dense", 
# 		"Fog", "Dense", "Smoke", "Drought", "Dust", "Devil", "Dust", 
# 		"Storm", "Excessive", "Heat", "Extreme", "Cold/Wind", "Chill",
# 		"Flash", "Flood", "Flood", "Frost/Freeze", "Funnel", "Cloud", 
# 		"Freezing", "Fog", "Hail", "Heat", "Heavy", "Rain", "Heavy", 
# 		"Snow", "High", "Surf", "High", "Wind", "Hurricane", "Typhoon", 
# 		"Ice", "Storm", "Lake-Effect", "Snow", "Lakeshore", "Flood",
# 		"Lightning", "C", "Marine", "Hail", "Marine", "High", "Wind", 
# 		"Marine", "Strong", "Wind", "Marine", "Thunderstorm", "Wind", 
# 		"Rip", "Current", "Seiche", "Sleet", "Storm", "Surge", "/", 
# 		"Tide", "Strong", "Wind", "Thunderstorm", "Wind", "Tornado", 
# 		"Tropical", "Depression", "Tropical", "Storm", "Tsunami", 
# 		"Volcanic", "Ash", "Waterspout", "Wildfire", "Winter", "Storm", 
# 		"Winter", "Weather") 
# 



# SD.Agg <- with(StormData, aggregate(
# 	cbind(Deaths, Injuries, Prop.Dam.Cost, Crop.Dam.Cost) ~ Year, 
# 	FUN = sum))
# SD.Agg$Prop.Dam.Cost <- SD.Agg$Prop.Dam.Cost / (1e9)
# SD.Agg$Crop.Dam.Cost <- SD.Agg$Crop.Dam.Cost / (1e9)
# 
# SD.Agg$Tot.Dam.Cost <- SD.Agg$Prop.Dam.Cost + SD.Agg$Crop.Dam.Cost
# SD.Agg$Casualties <- SD.Agg$Deaths + SD.Agg$Injuries
# 
# 
# with(SD.Agg, plot(Year, Tot.Dam.Cost, type= "o", ylim = c(0, 15000)))
# with(SD.Agg, lines(Year, Prop.Dam.Cost, type = "o", col = "red", pch = 17))
# with(SD.Agg, lines(Year, Crop.Dam.Cost, type = "o", col = "green", pch = 12))
# 
# 
# with(SD.Agg, plot(Year, Casualties, type= "o",ylim = c(0, 15000) ))
# with(SD.Agg, lines(Year, Deaths, type = "o", col = "red", pch = 17))
# with(SD.Agg, lines(Year, Injuries, type = "o", col = "green", pch = 12))



# SD.Tornado <- SD$Event.Type[grepl("tornado", SD$Event.Type, ignore.case = TRUE)]
# 
# SD01 <- SD$Event.Type[grepl("Astronomical Low Tide", SD$Event.Type, ignore.case = TRUE)]

#StormData$Event.Type <- as.factor(StormData$Event.Type)










