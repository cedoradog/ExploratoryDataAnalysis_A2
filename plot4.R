#Created by Camilo Dorado
#Created on August 19th/2014
#Last modification on August 24th/2014

library(ggplot2)
library(plyr)
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

#4. Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999 to 2008?

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
