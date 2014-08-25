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
