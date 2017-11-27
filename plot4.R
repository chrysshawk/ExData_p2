# Creating a plot to see, across US, how emissions from coal combustion
# related sources have changed 1999-2008
plot4 <- function() {
     
     # Checking for and installing/initializing required packages
     if(!require(ggpubr)){
          install.packages("ggpubr")
     }
     if(!require(ggplot2)){
          install.packages("ggplot2")
     }
     if(!require(dplyr)){
          install.packages("dplyr")
     }
     library(ggpubr)
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
          filter(grepl("coal", Short.Name, ignore.case = TRUE)) %>% #filtering out coal
          filter(grepl("comb", Short.Name, ignore.case = TRUE)) # filtering out combustion
     
     # Merging dataframes to only view coal related data, & sorting df
     mCoal <- merge(sccCoal, NEI, by.x = "SCC", by.y = "SCC")
     mCoal <- mCoal[order(mCoal$year, mCoal$type),]
     
     # Plotting histogram of number of measurements in given period
     gMeasurements <- 
          ggplot(data = mCoal, aes(x = year, fill = type)) +
          geom_histogram(binwidth = 1) + 
          theme_light() +
          scale_x_continuous(breaks = unique(mCoal$year)) +
          labs(title = "(1) Increasing # US coal measurements", y = "Emission measurements", x = "Year")
     
     # See how average emissions have developed
     avgCoal <- mCoal %>%
          select(year, type, Emissions) %>%
          group_by(year, type) %>%
          summarize(AvgEmissions = mean(Emissions))
     
     # Plotting average emissions in given period
     gAvgEmission <- 
          ggplot(data = avgCoal, aes(x = year, y = AvgEmissions)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, lty = "dotted") +
          facet_grid(type~., scales = "free_y") +
          theme_light() +
          labs(title = "(2) Decreasing average emissions",
               y = "Average emissions per point",
               x = "Year")
     
     # Plotting total emissions in given period
     gSumEmissions <- 
          ggplot(data = mCoal, aes(x = year, y = Emissions)) +
          stat_summary(fun.y = sum, geom="bar", aes(fill = type), 
                       position = position_stack()) +
          theme_light() +
          scale_x_discrete(limits = unique(mCoal$year)) +
          labs(title = "(3) Decreasing total emissions",
               y = "Emissions (tons)",
               x = "Year")

     gPlots <- ggarrange(gMeasurements, gAvgEmission, gSumEmissions, ncol = 2, nrow = 2)
     
     ggsave('plot4.png', plot = gPlots, width = 10, height = 7)

     message("Success! Plot saved as plot4.png to working directory.")
     
}