# Creating plot to see if total emissions from PM2.5 have decreased from 1999-2008
plot1 <- function() {
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

     # Calculating yearly emissions and converting to million (tons)
     yEmissions <- with(NEI, tapply(Emissions, year, sum))/1000000
     
     # Creating PNG device
     png(file = "plot1.png", width = 480, height = 480, units = "px",
         bg = "white")
     
     plot(x = names(yEmissions), y = yEmissions, type = "p", pch = 19, lwd = 5,
          xlab = "Year", ylab = "PM2.5 emissions (million tons)", main = "Total Emissions per Year")
     lines(names(yEmissions), yEmissions, col = "red", lty=3, lwd=2.5)
     abline(h = mean(yEmissions), col = "blue", lty = 3, lwd = 2.5)
     legend("topright", c("Yearly emission measurement", "Emission trend", "Average emissions"), 
            lty=c(0, 3, 3), pch = c(19,NA,NA), lwd=c(2.5, 2.5, 2.5), col=c("black", "red", "blue"))
     
     # Closing device
     dev.off()
     message("Success! Plot saved as plot1.png to working directory.")
     
}