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
          geom_bar(position = position_dodge(), width = 2) +
          theme_light() +
          scale_x_discrete(limits = unique(dEmi$year)) +
          labs(title = "(1) Trend in # measurements", 
               y = "# measurements", x = "Year")
     
     # Calculating average emissions
     avgEmi <- dEmi %>%
          select(year, County, Emissions) %>%
          group_by(year, County) %>%
          summarize(avgEmission = mean(Emissions))
     
     # Plotting average emissions in given period
     gAvgEmissions <- 
          ggplot(data = avgEmi, aes(x = year, y = avgEmission, fill = County)) +
          geom_col() +
          geom_smooth(method = "lm", se = FALSE, lty = "dotted") +
          scale_x_discrete(limits = unique(avgEmi$year)) +
          theme_light() +
          facet_grid(County~., scales = "free_y") + 
          labs(title = "(2) Differences in average emissions",
               y = "Mean emissions (tons)",
               x = "Year")

     # Looking at variability of emissions
     gVariabilityLog <- 
          ggplot(data=dEmi, aes(x=County, y=Emissions, fill = County)) +
          theme_light() +
          geom_boxplot() +
          theme(axis.text.x = element_blank()) +
          facet_grid(.~year) +
          scale_y_log10() +
          labs(title = "(3) Emission measurement variation trend", 
               y = "Log10 Emissions (tons)", x = "County")

     gPlots <- ggarrange(gMotorVehicles, gAvgEmissions, gVariabilityLog,
                         ncol = 2, nrow = 2)
     
     ggsave('plot6.png', plot = gPlots, width = 7, height = 10)

     message("Success! Plot saved as plot6.png to working directory.")
     
}