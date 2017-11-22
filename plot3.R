plot3 <- function() {
     library(ggplot2)
     library(dplyr)
     # Downloading and extracting file data if not already present
     if(!file.exists("./data")){
          message("Data not yet downloaded, will download first...")
          dir.create("./data")
          fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
          download.file(fileURL, dest="dataset.zip", mode="wb", method = "auto") 
          unzip("dataset.zip", exdir = "./data")
          file.remove("dataset.zip") # removing the archive
     }
     
     # Reading file to dataframe
     message("Reading data file...")
     NEI <- readRDS(file = "./data/summarySCC_PM25.rds")
     SCC <- readRDS(file = "./data/Source_Classification_Code.rds")

     # Subsetting Baltimore (fips 24510) emissons and year
     bEmi <- subset(NEI, fips == "24510", c(Emissions, year, type))
     bEmi <- select(NEI, c(year, type, Emissions), )
     
     bEmi <- NEI %>%
          select(fips, year, type, Emissions) %>%
          filter(fips == "24510") %>%
          group_by(year) %>%
          summarize(yEmissions = sum(Emissions))
     
     firstDF <- select(completeDF, 2:3, contains("mean"), contains("std"))
     
     qplot(year, Emissions, data = bEmissions, color = type, geom = "point", facets = type~.)
     
     
     #qplot(bEmissions$year, bEmissions$Emissions, data = bEmissions, color = type, geom = c("point", "smooth"))
     #qplot(displ, hwy, data = mpg, facets = . ~drv)
     
     library(dplyr)
     bEmissions <- select(NEI, )
     
     # Determining emissions per year per type
     
     # Calculating yearly emissions
     yEmissions <- with(bEmissions, tapply(Emissions, year, sum))
     
     # Creating PNG device
     png(file = "plot3.png", width = 480, height = 480, units = "px",
         bg = "white")
     
     library(ggplot2)
     
    
     
     # Closing device
     dev.off()
     message("Success! Plot saved as plot3.png to working directory.")
     
}