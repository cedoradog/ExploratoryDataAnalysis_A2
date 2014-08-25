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
