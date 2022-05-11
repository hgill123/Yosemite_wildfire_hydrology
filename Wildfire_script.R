
install.packages("pracma")
library(pracma)
setwd("C:/Users/harri/OneDrive/Documents/School/21-22/Semester 2/ENRG 400/Wildfires/data/")

wildfire <- read.csv('NCIF_Wildfire_CA.csv')

get_CA_wildfire_data <- function(raw_wildfire_df) {
  relevant_cols <- c("ï..X", "Y", "CalculatedAcres", "ContainmentDateTime", "ControlDateTime",
                     "DailyAcres", "DiscoveryAcres", "FireCause", "FireCauseGeneral",
                     "FireCauseSpecific", "FireDiscoveryDateTime", "FireOutDateTime",
                     "IncidentName", "InitialLatitude", "InitialLongitude", "POOCounty",
                     "POOState", "FireBehaviorGeneral")
  wildfire <- raw_wildfire_df[relevant_cols]
  CA_wildfire <- subset(wildfire, POOState=="US-CA")
  CA_wildfire$FireDiscoveryDateTime <- as.Date(CA_wildfire$FireDiscoveryDateTime)
  CA_wildfire$ContainmentDateTime <- as.Date(CA_wildfire$ContainmentDateTime)
  CA_wildfire$ControlDateTime <- as.Date(CA_wildfire$ControlDateTime)
  CA_wildfire$FireOutDateTime <- as.Date(CA_wildfire$FireOutDateTime)
  # CA_wildfire <- subset(CA_wildfire, FireOutDateTime > "2021-05-06")
  return(CA_wildfire)
}

wildfire_df <- get_CA_wildfire_data(wildfire)
tuolumne_lat_long <- c(37.8731, -119.3591)
fire_haversine <- rep(NA, nrow(wildfire_df))
for(i in 1:nrow(wildfire_df)) {
  fire_lat_long <- c(wildfire[i, "InitialLatitude"], wildfire[i, "InitialLongitude"])
  fire_haversine[i] <- tryCatch(haversine(tuolumne_lat_long, fire_lat_long), error=function(err) NA)
}

wildfire_df$dist_from_tuolumne <- fire_haversine / 1.609
hist(wildfire_df[wildfire_df$dist_from_tuolumne<6000,]$dist_from_tuolumne)
hist(wildfire_df$FireDiscoveryDateTime, breaks=100)
hist(wildfire_df$FireOutDateTime, breaks=100)
hist(wildfire_df$dist_from_tuolumne)
plot(wildfire_df$dist_from_tuolumne,
     wildfire_df$CalculatedAcres,
     xlim=range(0:4000),
     ylim=range(0:3000))

# https://cdec.water.ca.gov/dynamicapp/QueryF?s=TUM&d=06-May-2022+08:25&span=12hours

tuolumne_turbidity_14 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_13_14.csv')
tuolumne_turbidity_15 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_14_15.csv')
tuolumne_turbidity_16 <- read.csv('TURBIDITY_TUOLUMNE MEADOWS_15_16.csv')
tuolumne_turbidity_17 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_16_17.csv')
tuolumne_turbidity_18 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_17_18.csv')
tuolumne_turbidity_19 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_18_19.csv')
tuolumne_turbidity_20 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_19_20.csv')
tuolumne_turbidity_21 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_20_21.csv')
tuolumne_turbidity_22 <- read.csv('TURBIDITY_TUOLUMNE_MEADOWS_21_22.csv')
tuolumne_turbidity <- rbind(tuolumne_turbidity_14,
                            tuolumne_turbidity_15,
                            tuolumne_turbidity_16,
                            tuolumne_turbidity_17,
                            tuolumne_turbidity_18,
                            tuolumne_turbidity_19,
                            tuolumne_turbidity_20,
                            tuolumne_turbidity_21,
                            tuolumne_turbidity_22)


tuolumne_flow_14 <- read.csv('FLOW_TUM_13_14.csv')
tuolumne_flow_15 <- read.csv('FLOW_TUM_14_15.csv')
tuolumne_flow_16 <- read.csv('FLOW_TUM_15_16.csv')
tuolumne_flow_17 <- read.csv('FLOW_TUM_16_17.csv')
tuolumne_flow_18 <- read.csv('FLOW_TUM_17_18.csv')
tuolumne_flow_19 <- read.csv('FLOW_TUM_18_19.csv')
tuolumne_flow_20 <- read.csv('FLOW_TUM_19_20.csv')
tuolumne_flow_21 <- read.csv('FLOW_TUM_20_21.csv')
tuolumne_flow_22 <- read.csv('FLOW_TUM_21_22.csv')
tuolumne_discharge <- rbind(tuolumne_flow_14,
                            tuolumne_flow_15,
                            tuolumne_flow_16,
                            tuolumne_flow_17,
                            tuolumne_flow_18,
                            tuolumne_flow_19,
                            tuolumne_flow_20,
                            tuolumne_flow_21,
                            tuolumne_flow_22)



tuolumne_hyrdology <- merge(tuolumne_discharge, tuolumne_turbidity, by = "DATE...TIME..PST.")
tuolumne_hyrdology = subset(tuolumne_hyrdology, select = c(DATE...TIME..PST., FLOW.CFS, TURB.W...NTU))
colnames(tuolumne_hyrdology) <- c("Date", "Flow", "Turbidity")
tuolumne_hyrdology$Flow <- as.numeric(tuolumne_hyrdology$Flow)
tuolumne_hyrdology$Turbidity <- as.numeric(tuolumne_hyrdology$Turbidity)
tuolumne_hyrdology$Dates <- as.Date(strptime(as.character(tuolumne_hyrdology$Date), "%m/%d/%Y"))

max_flow_by_date <- aggregate(Flow ~ Dates, data = tuolumne_hyrdology, max)
max_turbidity_by_date <- aggregate(Turbidity ~ Dates, data = tuolumne_hyrdology, max)
max(max_turbidity_by_date[as.numeric(max_turbidity_by_date$Turbidity) < 5,]$Turbidity)
plot(tuolumne_hyrdology[tuolumne_hyrdology$Turbidity<10,]$Turbidity, pch=19, cex=1)
plot(tuolumne_hyrdology$Dates, tuolumne_hyrdology$Flow)
plot(tuolumne_hyrdology$Dates, tuolumne_hyrdology$Turbidity)
plot(max_flow_by_date$Dates, max_flow_by_date$Flow, type = "l", lty = 1)
trimmed_max_turbidity_by_date <- max_turbidity_by_date[max_turbidity_by_date$Turbidity < 20, ]
plot(trimmed_max_turbidity_by_date$Dates, trimmed_max_turbidity_by_date$Turbidity, type = "l", lty = 1)

# plot fires found and hydrology by day
par(mfrow=c(3,1))
hist(wildfire_df$FireOutDateTime,
     breaks=100,
     xlab="Date",
     ylab="Frequency of Fires Put Out",
     yaxt='n',
     main='Frequency of CA Fires Put Out')
plot(tuolumne_hyrdology$Dates,
     tuolumne_hyrdology$Flow,
     xlab="Date",
     ylab="Flow (CFS)")
plot(tuolumne_hyrdology$Dates,
     tuolumne_hyrdology$Turbidity,
     xlab="Date",
     ylab="Turbidity (NTU)")


# SCU Lightning Complex Case Study
scu_coordinates <- c(37.439437, -121.30435)
cat("Miles from SCU Lightning Complex fire to Tuolumne Meadows: ", haversine(tuolumne_lat_long, scu_coordinates)/1.609)
scu_hydrology_subset <- tuolumne_hyrdology[tuolumne_hyrdology$Dates > as.Date('08/16/17', '%m/%d/%y'),]
par(mfrow=c(2,1))
plot(scu_hydrology_subset$Dates,
     scu_hydrology_subset$Flow,
     pch=20,
     cex=1,
     xlab="Date",
     ylab="Flow (CFS)")
title("Tuolumne Hydrology Surrounding SCU Fire")
scu_start_date <- as.Date('08/16/20', '%m/%d/%y')
scu_end_date <- as.Date('10/03/20', '%m/%d/%y')
polygon(c(scu_start_date, scu_end_date, scu_end_date, scu_start_date),
        c(0, 0, 1030, 1030),
        col=rgb(1, 0, 0,0.4))
plot(scu_hydrology_subset[scu_hydrology_subset$Turbidity <5,]$Dates,
     scu_hydrology_subset[scu_hydrology_subset$Turbidity <5,]$Turbidity,
     pch=20,
     cex=1,
     xlab="Date",
     ylab="Turbidity (NTU)")
polygon(c(scu_start_date, scu_end_date, scu_end_date, scu_start_date),
        c(0, 0, 5.2, 5.2),
        col=rgb(1, 0, 0,0.4))
legend("top", inset=-0.8, legend="SCU Lightning Complex Fire Duration",
       fill=rgb(1, 0, 0,0.4), horiz=TRUE, cex=0.8)

dim(scu_hydrology_subset[scu_hydrology_subset$Turbidity <5,])
dim(scu_hydrology_subset)
39345-41424
