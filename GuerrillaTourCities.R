################################################################################
##
##  Author:  Brendan McDougall
##  Proj Purpose: Revenue Streams for Musicians / Artists from Digital Media
##                Distribution & Live Performance
##  File Purpose: R-script for determining distance and drive times between
##                Cities of proposed Calif tour
##  Firm:  NSCI Consulting
##  Customer ID:  0001
##  Date:  4/8/17
##
##
################################################################################
##
##  System Info:  Windows 10, 64 bit, i7 processor, RStudio Version 0.98.1102
##                  R x64 3.1.2, git scm 1.9.5
##
################################################################################
##
## Revision History
##
##      4/8/17:  assembled file
##      4/9/17:  completed script for pulling travel data from googleMaps API
##      4/10/17: removed private key from google API requests; enables sharing            
##                  
##
##      
##
################################################################################
##
##  Methdology:
##  (1)     Load useful libraries
##  (2)     Specifiy and document environment
##  (3)     Execute Project:  determine distance / drive times between cities
##      
################################################################################
##
## Part (1):  Load useful R libraries
##
library(plyr); library(dplyr) # order dependent for machine learning
library(rCharts); library(maps); library(leaflet); library(rMaps)
library(gmapsdistance); library(placement); library(googleway)
library(xlsx)
##
################################################################################
##
##  (2)     Specifiy and document environment
##
setwd("E:\\Brendan\\Documents\\Education\\AHA\\MusicLabel")
curDir <- getwd()
fileList <- dir()
environment <- sessionInfo()
# View(curDir)
# View(fileList)
write(fileList, file = 'fileList.txt')
# writeLines(environment, con = 'environment.txt', sep = "\n", useBytes = FALSE)
writeLines(unlist(lapply(environment, paste, collapse=" ")),
           con = 'environment.txt', sep = "\n", useBytes = FALSE)
##
################################################################################
##
##  (3)     Execute Project:  Classification Project
##
################################################################################
##
## Load Data
##
cityData <- tbl_df(
    read.csv2('DigitalPlatformComparison.csv', sep = ",", 
              stringsAsFactors = FALSE, na.strings = "NA",
              header = TRUE,
              colClasses = c("character", "character", "character",
                             "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric")
    )
)
##
################################################################################
##
##  Pre-processing:  cast data as.numeric
##                      (SApply likely is more efficient way to recast data),
##                   geocode the Cities into Lat, Long for googleMaps API
cityData <- cityData %>%
    filter( nchar(cityData$City) > 0) %>%
    select(
        City, State, Univeristy, Meals, Lodging, Mileage, Total, Distance, Time, 
        latitude, longitude
    )  %>%
    mutate(
        Meals = as.numeric(Meals), Lodging = as.numeric(Lodging), 
        Lodging = as.numeric(Lodging), Total = as.numeric(Total),
        Distance = as.numeric(Distance), 
        Mileage = as.numeric(Mileage), Time = as.numeric(Time), 
        latitude = as.numeric(latitude), longitude = as.numeric(longitude)
    ) %>%
    mutate(
        latitude = 
            geocode_url( paste(City, State, sep="+"), auth ="standard_api",
                         privkey="",
                         clean=TRUE, add_date='today', verbose=TRUE)$lat,
        longitude = 
            geocode_url( paste(City, State, sep="+"), auth ="standard_api",
                         privkey="",
                         clean=TRUE, add_date='today', verbose=TRUE)$lng
        
    ) %>%
    arrange(desc(latitude), longitude) 
##
##
################################################################################
##
##
##  Sort dataFrame by latitude and longitude so travel is in a reasonable order
##  Call googleMap API to get distance and drive time between cites
##  Calculate mileage reimbursement using IRS business expense
##  Total the expense for meals, lodging, and driving to next city
cityData <- cityData %>%
    mutate(
        Distance = round(
            drive_time(
                address=paste(lag(cityData$latitude, default = cityData$latitude[1]),
                              lag(cityData$longitude, default = cityData$longitude[1]),
                              sep=","),
                dest=paste(cityData$latitude,cityData$longitude, sep=","),
                auth="standard_api",
                privkey="", clean=FALSE, 
                add_date='today', verbose=FALSE, travel_mode="driving",
                units="imperial")$dist_num,
            digits = 1),
        Mileage = round( 0.53 * Distance, digits = 2),
        Time = round(
            drive_time(
                address=paste(lag(cityData$latitude, default = cityData$latitude[1]),
                              lag(cityData$longitude, default = cityData$longitude[1]),
                              sep=","),
                dest=paste(cityData$latitude,cityData$longitude, sep=","),
                auth="standard_api",
                privkey="", clean=FALSE, 
                add_date='today', verbose=FALSE, travel_mode="driving",
                units="imperial")$time_hours,
            digits = 1            
        ),
        Total = Meals + Lodging + Mileage
    )   
##
## Calculate average expense and standard deviation of expense for this tour
##
meanExpense <- summarise(cityData, mean(Total))
stDevExpense <- summarise(cityData, sd(Total))

cat("Mean Expense = ", paste(round(meanExpense, digits = 2)), "with StdDev =",
    paste(round(stDevExpense, digits = 2)))

meanMeals <- summarise(cityData, mean(Meals))
stDevMeals <- summarise(cityData, sd(Meals))

cat("Mean Meals Expense = ", paste(round(meanMeals, digits = 2)), "with StdDev =",
    paste(round(stDevMeals, digits = 2)))

meanLodging <- summarise(cityData, mean(Lodging))
stDevLodging <- summarise(cityData, sd(Lodging))

cat("Mean Lodging Expense = ", paste(round(meanLodging, digits = 2)), "with StdDev =",
    paste(round(stDevLodging, digits = 2)))