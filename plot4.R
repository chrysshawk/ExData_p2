plot4 <- function() {
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
     message("Reading data files...")
     NEI <- readRDS(file = "./data/summarySCC_PM25.rds")
     SCC <- readRDS(file = "./data/Source_Classification_Code.rds")
     
     # Identifying Coal combustion-related sources from SCC
     sccCoal <- SCC %>%
          select(SCC, Short.Name) %>%
          filter(grepl("[Cc]oal", Short.Name))
     
     # Subsetting NEI based on coal related sources
     emiCoal <- NEI %>%
          select(SCC, year, type, Emissions) %>%
          filter(SCC %in% sccCoal$SCC) %>%
          group_by(year, SCC)
          #summarize(Emissaions = sum(Emissions))
     #names(emiCoal) <- make.names(c("Year", "Emission"))
     
     # Shows a greater amount of point measurements
     qplot(year, data = emiCoal, facets = type ~ ., geom = "histogram", binwidth = 3)
     ggplot(data = emiCoal, aes(x=Year, y=Emissions)) +
     
     # Looking at the intersect points
     coal99 <- subset(emiCoal, Year %in% 1999)
     coal08 <- subset(emiCoal, year %in% 2008)
     intSCC <- intersect(coal99$SCC, coal08$SCC)
     intCoal <- emiCoal %>%
          select(SCC, year, type, Emissions) %>%
          filter(SCC %in% intSCC) %>%
          group_by(year, type) %>%
          summarize(Total.Emissions = sum(Emissions),
                    Mean.Emissions = mean(Emissions),
                    count = length(SCC))
     ggplot(data = emiCoal)
          
     
     # Should I use only indicators for the measurements used in all years? (intersect)
     # Should I use means?
     # What about one chart per year; total emission, mean emission, development
     
     
          
     

     # Creating plot results using ggplot2
     plot3 <- ggplot(data = bEmi, aes(x=Year, y=Emissions)) +
          geom_line(aes(col = Type), size = 1) +
          geom_point(color = "white", size = 9) +
          geom_text(aes(col = Type, label = round(Emissions, 0)), size = 3) +
          labs(title = "Baltimore Emissions 1999-2008", y = "PM2.5 Emissions (tons)", x = "Year") +
          theme_classic() + 
          scale_x_continuous(breaks = unique(bEmi$Year)) #Aligning measurement years on x axis
     
     ggsave('plot4.png', plot = plot3, width = 7, height = 7)

     message("Success! Plot saved as plot4.png to working directory.")
     
}