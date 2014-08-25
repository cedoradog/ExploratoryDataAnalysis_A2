#Created by Camilo Dorado
#Created on August 19th/2014
#Last modification on August 24th/2014

library(data.table)
library(ggplot2)
#Set the working directory
setwd("d:/Users/Camilo/Desktop/DataScience/ExploratoryDataAnaL/A2")
#Download and unzip the data
dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
#Create a temporary directory and a placeholder file
tempDir <- tempdir()
tempFile <- tempfile(tmpdir = tempDir, fileext = ".zip")
#Download data into placeholder file
download.file(dataURL, tempFile)
#Extract the files to the current working directory
unzip(tempFile, exdir = ".", overwrite = T)
#Load the data
NEI <- readRDS("summarySCC_PM25.rds")
NEI <- transform(NEI, Pollutant = factor(Pollutant), 
                 type = factor(type))
NEI.dt <- data.table(NEI)
SCC <- readRDS("Source_Classification_Code.rds")

#1. Have total emissions from PM2.5 decreased in the United States from 1999 
#to 2008? Using the base plotting system, make a plot showing the total PM2.5
#emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#Prepare data to plot (in Mtons)
dataPlot1 <- NEI.dt[, sum(Emissions) / 1e6, by = year]
setnames(dataPlot1, 2, "AnnualEmissionMton")
yLimit <- ceiling(max(dataPlot1$AnnualEmissionMton))
xLimits <- range(dataPlot1$year)
#Open device to plot into a png file
png("plot1.png")
#Plot AnnualEmissionMton vs. Year
plot(dataPlot1, main = expression("PM"["2.5"]*" annual emmission"), 
     xlab = "Year", ylab = "Annual emission (Mton)",
     xaxp = c(xLimits, 3), 
     ylim = c(0, yLimit), yaxp = c(0, yLimit, yLimit))
#Add lines to help visualization
lines(dataPlot1, lwd = 2)
#Close plotting device
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == "24510") from 1999 to 2008? Use the base plotting system to make a
#plot answering this question.

#Prepare data to plot (in ktons)
dataPlot2 <- NEI.dt[fips == "24510", sum(Emissions) / 1e3, by = year]
setnames(dataPlot2, 2, "AnnualEmissionMton")
yLimit <- ceiling(max(dataPlot2$AnnualEmissionMton))
xLimits <- range(dataPlot2$year)
#Open device to plot into a png file
png("plot2.png")
#Plot AnnualEmissionMton vs. Year
plot(dataPlot2, main = expression("Baltimore City PM"["2.5"]*" annual emmission"), 
     xlab = "Year", ylab = "Annual emission (kton)",
     xaxp = c(xLimits, 3), 
     ylim = c(0, yLimit), yaxp = c(0, yLimit, yLimit))
#Add lines to help visualization
lines(dataPlot2, lwd = 2)
#Close plotting device
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#nonroad) variable, which of these four sources have seen decreases in emissions 
#from 1999 to 2008 for Baltimore City? Which have seen increases in emissions 
#from 1999 to 2008? Use the ggplot2 plotting system to make a plot answer this 
#question.

#Take the Baltimore City data excluding the 0 measures in Emissions
dataPlot3 <- NEI[NEI$fips == "24510" & NEI$Emissions > 0, 
                 c("Emissions", "type", "year")]
#Plot log(Emissions) vs. year, by each type, with the linear tendency and the CI 
#indicated by the linear model
plot3 <- qplot(year, log(Emissions, 10), data = dataPlot3, facets = . ~ type, 
              geom = c("point", "smooth"), method = "lm")
#Add labels and title
plot3 <- plot3 + labs(x = "Year", y = expression("log ("*PM[2.5]*" / ton)"), 
                      title = expression("Baltimore City PM"["2.5"]*" measures"))
#Open device to plot into a png file
png("plot3.png", width = 600, height = 300)
#Print the plot
plot3
#Close plotting device
dev.off()

#4. Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999 to 2008?

library(plyr)
#Select the SCC codes related w/ Coal (having "Coal" in its EI.Sector)
coalSCC <- SCC$SCC[grep("Coal", SCC$EI.Sector)]
#Select data w/ any of that SCC codes
dataPlot4 <- NEI[NEI$SCC %in% coalSCC & NEI$Emissions > 0,]
#Add the correponding EI.Sector
dataPlot4 <- join(dataPlot4, SCC[, c("SCC", "EI.Sector")], by = "SCC")
#Edit the EI.Sector variable
dataPlot4 <- transform(dataPlot4, EI.Sector = gsub("Fuel Comb - ", "", EI.Sector))
dataPlot4 <- transform(dataPlot4, EI.Sector = gsub(" - Coal", "", EI.Sector))
dataPlot4 <- transform(dataPlot4, EI.Sector = factor(EI.Sector))
#Plot the data by EI.Sector
plot4 <- ggplot(dataPlot4, aes(year, log(Emissions, 10))) + 
  geom_point(alpha = 0.2) +
  facet_grid(type ~ EI.Sector) + 
  geom_smooth(method = "lm", se = T) + 
  coord_cartesian(ylim = c(-3, 6)) +
  labs(x = "Year", y = expression("log ("*PM[2.5]*" / ton)"), 
       title = expression("Coal combustion - related  PM"["2.5"]*" measures"))
#Open device to plot into a png file
png("plot4.png")
#Print the plot
plot4
#Close plotting device
dev.off()

#5. How have emissions from motor vehicle sources changed from 1999 to 2008 in 
#Baltimore City?

#Select the SCC codes related w/ motor vehicles (its EI.Sector begins w/ "Mobile")
motorSCC <- SCC$SCC[grep("^Mobile", SCC$EI.Sector)]
#Select data w/ any of that SCC codes in Baltimore City
dataPlot5 <- NEI[NEI$SCC %in% motorSCC & NEI$fips == "24510" & NEI$Emissions > 0,]
#Add the correponding EI.Sector
dataPlot5 <- join(dataPlot5, SCC[, c("SCC", "EI.Sector")], by = "SCC")
#Edit the EI.Sector variable
dataPlot5 <- transform(dataPlot5, EI.Sector = gsub("Mobile - ", "", EI.Sector))
dataPlot5 <- transform(dataPlot5, EI.Sector = gsub("Non-Road.*", 
                                                   "Non-Road Equipment", EI.Sector))
dataPlot5 <- transform(dataPlot5, EI.Sector = gsub("On-Road.*", 
                                                   "On-Road Vehicles", EI.Sector))
dataPlot5 <- transform(dataPlot5, EI.Sector = factor(EI.Sector))
#Plot the data by EI.Sector
plot5 <- ggplot(dataPlot5, aes(year, log(Emissions, 10))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ EI.Sector, nrow = 2) + 
  geom_smooth(method = "lm", se = T) + 
  coord_cartesian(ylim = c(-5, 3)) +
  labs(x = "Year", y = expression("log ("*PM[2.5]*" / ton)"), 
       title = expression("Motor vehicles - related  PM"["2.5"]*
                            " measures in Baltimore City"))
#Open device to plot into a png file
png("plot5.png", width = 600)
#Print the plot
plot5
#Close plotting device
dev.off()

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions
#from motor vehicle sources in Los Angeles County, California (fips == "06037").
#Which city has seen greater changes over time in motor vehicle emissions?

#Select data w/ any of that SCC codes in Baltimore City and Los Angeles County
dataPlot6 <- NEI[NEI$SCC %in% motorSCC & NEI$fips %in% c("24510", "06037") &
                 NEI$Emissions > 0,]
#Edit the fips (city) variable
dataPlot6 <- transform(dataPlot6, fips = gsub("24510", "Baltimore City", fips))
dataPlot6 <- transform(dataPlot6, fips = gsub("06037", "Los Angeles County", fips))
#Plot the data by EI.Sector
plot6 <- ggplot(dataPlot6, aes(year, log(Emissions, 10))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ fips) + 
  geom_smooth(method = "lm", se = T) + 
  coord_cartesian(ylim = c(-5, 3)) +
  labs(x = "Year", y = expression("log ("*PM[2.5]*" / ton)"), 
       title = expression("Motor vehicles - related  PM"["2.5"]*
                            " measures"))
#Open device to plot into a png file
png("plot6.png", height = 300)
#Print the plot
plot6
#Close plotting device
dev.off()
