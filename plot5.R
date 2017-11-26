#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
plot5 <- function() {
     
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
     
     # Identifying Motor vehicle (ON-ROAD) measurements in Baltimore (fips 24510)
     emiOnroad <- NEI %>%
          select(fips, SCC, Emissions, type, year) %>%
          filter(fips == "24510" & type == "ON-ROAD")
          
     # SCC Short.Name informs that the emission in emiOnroad are of 4 categories
     # Categorizing motor vehicle emission types
     emiType1 <- gsub(".........0$", "Total", emiOnroad$SCC)
     emiType2 <- gsub(".........B$", "Brakes", emiType1)
     emiType3 <- gsub(".........T$", "Transmission", emiType2)
     emiTypes <- gsub(".........X$", "Exhaust", emiType3)
     emiOnroad <- cbind(emiOnroad, emiTypes)
     
     # Plotting distribution of emission measurements
     gMotorVehicles <- 
          ggplot(data = emiOnroad, aes(x = year, fill = emiTypes)) +
          geom_histogram(binwidth = 2) +
          theme_light() +
          scale_x_continuous(breaks = unique(emiOnroad$year)) +
          labs(title = "(1) Motor vehicle measurements", 
               y = "# measurements", x = "Year")
     
     # Calculating average emissions
     avgOnroad <- emiOnroad %>%
          select(year, emiTypes, Emissions) %>%
          group_by(year) %>%
          summarize(avgEmission = mean(Emissions))
     
     # Plotting average emissions in given period
     gAvgEmissions <- 
          ggplot(data = avgOnroad, aes(x = year, y = avgEmission)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, lty = "dotted") +
          theme_light() +
          coord_cartesian(ylim = 0:3) +
          labs(title = "(2) Decreasing mean emissions",
               y = "Mean emissions (tons)",
               x = "Year")
     
     gTotalEmissions <-
          ggplot(data=emiOnroad, aes(x = year, y = Emissions)) +
          geom_col(aes(fill = emiTypes)) +
          theme_light() +
          scale_x_continuous(breaks = unique(emiOnroad$year)) +
          labs(title = "(3) Total measured onroad emissions", 
               y = "Emissions (tons)", x = "Year")
          

     gPlots <- ggarrange(gMotorVehicles, gAvgEmissions, gTotalEmissions,
                         ncol = 2, nrow = 2)
     
     ggsave('plot5.png', plot = gPlots, width = 10, height = 7)

     message("Success! Plot saved as plot5.png to working directory.")
     
}