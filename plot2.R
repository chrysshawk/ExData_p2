# Creating plot to determine if emissions have decresed in the Baltimore City,
# Maryland (fips 24510) from 1999-2008
plot2 <- function() {
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
     bEmissions <- subset(NEI, fips == "24510", c(Emissions, year))
     
     # Calculating yearly emissions
     yEmissions <- with(bEmissions, tapply(Emissions, year, sum))
     
     # Creating PNG device
     png(file = "plot2.png", width = 480, height = 480, units = "px",
         bg = "white")
     
     plot(x = names(yEmissions), y = yEmissions, type = "p", pch = 19, lwd = 5,
          xlab = "Year", ylab = "PM2.5 emissions (tons)", main = "Baltimore Total Emissions per Year")
     lines(names(yEmissions), yEmissions, col = "red", lty=3, lwd=2.5)
     abline(h = mean(yEmissions), col = "blue", lty = 3, lwd = 2.5)
     legend("bottomleft", c("Yearly emission measurement", "Emission trend", "Average emissions"), 
            lty=c(0, 3, 3), pch = c(19,NA,NA), lwd=c(2.5, 2.5, 2.5), col=c("black", "red", "blue"))
     
     # Closing device
     dev.off()
     message("Success! Plot saved as plot2.png to working directory.")
     
}