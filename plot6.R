plot6 <- function() {
     
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
     
     # Identifying Motor vehicle (ON-ROAD) in Baltimore (24510) and LAC (06037)
     dEmi <- NEI %>%
          select(fips, SCC, Emissions, type, year) %>%
          filter(fips %in% c("24510", "06037") & type == "ON-ROAD") %>%
          mutate(County = ifelse(test = (fips == 24510),
                                 yes = "Baltimore",
                                 no = "Los Angeles County"))
     
     # Plotting measurement counts for period
     gMotorVehicles <- 
          ggplot(data = dEmi, aes(x=year, fill = County)) +
          geom_bar(position = position_dodge()) +
          theme_light() +
          scale_x_continuous(breaks = unique(dEmi$year)) +
          labs(title = "(1) Emission measurement count", 
               y = "# measurements", x = "Year")
     
     # Looking at variability of emissions
     gVariability <- 
          ggplot(data=dEmi, aes(x=year, y=Emissions, col = County)) +
          geom_point() +
          theme_light() +
          scale_x_continuous(breaks = unique(dEmi$year)) +
          labs(title = "(1) Emission measurement variability", 
               y = "Emissions (tons)", x = "Year")
     
     # Calculating average emissions
     avgEmi <- dEmi %>%
          select(year, County, Emissions) %>%
          group_by(year, County) %>%
          summarize(avgEmission = mean(Emissions))
     
     # Plotting average emissions in given period
     gAvgEmissions <- 
          ggplot(data = avgEmi, aes(x = year, y = avgEmission, col = County)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, lty = "dotted") +
          theme_light() +
          #coord_cartesian(ylim = 0:3) +
          labs(title = "(2) Variability in mean emissions",
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
     
     ggsave('plot6.png', plot = gPlots, width = 10, height = 7)

     message("Success! Plot saved as plot6.png to working directory.")
     
}