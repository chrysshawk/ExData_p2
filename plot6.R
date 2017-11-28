# Comparing emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). 
# Plotting which city has seen greater changes over time in motor vehicle emissions
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
     
     # Identifying Onroad measurements in Baltimore (24510) and LAC (06037)
     dEmi <- NEI %>%
          select(fips, SCC, Emissions, type, year) %>%
          filter(fips %in% c("24510", "06037") & type == "ON-ROAD") %>%
          mutate(County = ifelse(test = (fips == 24510),
                                 yes = "Baltimore",
                                 no = "Los Angeles County"))
     
     # Calculating and plotting change in total emissions per County
     dSum <- dEmi %>% 
          select(County, Emissions, year) %>% 
          group_by(year, County) %>% 
          summarize(Emissions = sum(Emissions))
     
     gTotalEmissions <- 
          ggplot(data = dSum, aes(x=year, y=Emissions, fill=County)) +
          theme_light() + 
          theme(legend.position="none") +
          geom_col(width = 2) +
          geom_text(aes(label = round(Emissions, 0)), vjust = 2, 
                    size = 4) +
          facet_grid(County ~., scales = "free_y") + 
          scale_x_discrete(limits = unique(dEmi$year)) +
          labs(title = "(1) Change in measured MV emissions", 
               y = "PM2.5 emission (tons)", x = "Year")
     
     # Calculating and plotting change in average emissions
     avgEmi <- dEmi %>%
          select(year, County, Emissions) %>%
          group_by(year, County) %>%
          summarize(avgEmission = mean(Emissions))
     
     # Plotting average emissions in given period
     gAvgEmissions <- 
          ggplot(data = avgEmi, 
                 aes(x = year, y = avgEmission, fill = County)) +
          geom_col(width = 2) +
          geom_smooth(method = "lm", se = FALSE, lty = "dotted") +
          scale_x_discrete(limits = unique(avgEmi$year)) +
          theme_light() +
          theme(legend.position = "none") +
          facet_grid(County~., scales = "free_y") + 
          labs(title = "(2) Changes in average emissions",
               y = "PM2.5 Avg emissions (tons)",
               x = "Year")
     
     # Plotting measurement counts for period
     gMotorVehicles <- 
          ggplot(data = dEmi, aes(x=year, fill = County)) +
          geom_bar(position = position_dodge(), width = 2) +
          theme_light() +
          theme(legend.position = "none") +
          facet_grid(.~County) +
          scale_x_discrete(limits = unique(dEmi$year)) +
          labs(title = "(3) Changes in # measurements", 
               y = "# measurements", x = "Year")

     gPlots <- ggarrange(gTotalEmissions, gAvgEmissions, gMotorVehicles,
                         ncol = 1, nrow = 3)
     
     ggsave('plot6.png', plot = gPlots, width = 4, height = 10)

     message("Success! Plot saved as plot6.png to working directory.")
     
}