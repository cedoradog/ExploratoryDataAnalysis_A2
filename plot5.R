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
