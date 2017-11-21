plot1 <- function() {
     # Downloading and extracting file data if not already present
     if(!file.exists("./data")){
          message("Data not yet downloaded, will download first...")
          dir.create("./data")
          fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
          download.file(fileURL, dest="dataset.zip", mode="wb", method = "auto") 
          unzip ("dataset.zip", exdir = "./data/")
          file.remove("dataset.zip") # removing the archive
     }
     
     # Reading file to dataframe
     message("Reading data file...")
     NEI <- readRDS(file = "./data/summarySCC_PM25.rds")
     SCC <- readRDS(file = "./data/Source_Classification_Code.rds")
     # NEI$year <- as.Date(as.character(NEI$year), "%Y")
     
     totalE <- tapply(NEI$Emissions, NEI$year, sum)
     meanE <- tapply(NEI$Emissions, NEI$year, mean)
     
}