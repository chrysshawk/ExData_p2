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

     # Subsetting Baltimore (fips 24510) and grouping by year and point type
     bEmi <- NEI %>%
          select(fips, year, type, Emissions) %>%
          filter(fips == "24510") %>%
          group_by(year, type) %>%
          summarize(Emissions = sum(Emissions))
     names(bEmi) <- make.names(c("Year", "Type", "Emissions"))

     # Creating PNG device
     #png(file = "plot3.png", width = 480, height = 480, units = "px",
     #    bg = "white")
     
     # Creating plot results using ggplot2
     plot3 <- ggplot(data = bEmi, aes(x=Year, y=Emissions)) +
          geom_line(aes(col = Type), size = 1) +
          geom_point(color = "white", size = 9) +
          geom_text(aes(col = Type, label = round(Emissions, 0)), size = 3) +
          labs(title = "Baltimore Emissions 1999-2008", y = "PM2.5 Emissions (tons)", x = "Year") +
          theme_classic() + 
          scale_x_continuous(breaks = unique(bEmi$Year)) #Aligning measurement years on x axis
     
     ggsave('plot3.png', plot = plot3, width = 7, height = 7)

     message("Success! Plot saved as plot3.png to working directory.")
     
}