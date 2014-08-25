#Created by Camilo Dorado
#Created on August 19th/2014
#Last modification on August 24th/2014

library(ggplot2)
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
SCC <- readRDS("Source_Classification_Code.rds")

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
