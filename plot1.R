#Created by Camilo Dorado
#Created on August 19th/2014
#Last modification on August 24th/2014

library(data.table)
#Assume that the working directory is properly selected
#setwd("d:/.../DataScience/ExploratoryDataAnaL/A2")
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