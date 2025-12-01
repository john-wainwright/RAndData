#---------------------------------------------------------------------------
# script for R and data workshop, Windsor BSG Training Course, December 2025
# John Wainwright  (john.wainwright@durham.ac.uk)
#---------------------------------------------------------------------------
#
#Set the working directory.  This should be the folder where this script is
#  located on *your* computer so you will need to change the path below
#Note that R always uses forward slashes (/) to show the path, whereas on
#  Windows the default is a backslash (\)
setwd ("c:/Path/To/This/Folder")
#Some would argue that setting the working directory in the script runs counter
#  to the aims of reproducible science (because the absolute folder structure is
#  unlikely to be the same on other machines.  The alternative approach is to add a 
#  note to use the RStudio Menu -- go to Session --> Set Working Directory -->
#  To Source File Location

#Set the location of the saved output (works if the whole script is run using
#  the Source option - don't run in interactive mode or you won't see the console
#  output)
#paste0 joins two or more strings of text together
sink (paste0 ("./results/results", Sys.Date (), ".txt"))

#Open the libraries we will use in the script
#The require funtion tells R to try to load in the library package.  If it is 
#  not already installed on the system, this loading will fail and return a 
#  FALSE value.  So the if statement tells R that if this occurs, it should 
#  install the library from CRAN and then load it using the library function.
#If you are working locally and know which packages you have, you only need the
#  library call.
if (!require (tidyverse)){
  install.packages ("tidyverse")
  library (tidyverse)
}

if (!require (gridExtra)){
  install.packages ("gridExtra")
  library (gridExtra)
}

if (!require (grid)){
  install.packages ("grid")
  library (grid)
}

#Read in the Durham Weather data file
durhamData <- read.csv ("./data/DurhamWeather1850_2025.csv",
                        header = TRUE)
#check the top of the dataset looks OK
head (durhamData)
#output some simple stats
summary (durhamData)

#show simple statistics using functions
mean (durhamData$Tmax, na.rm = TRUE)
median (durhamData$Tmax, na.rm = TRUE)
sd (durhamData$Tmax, na.rm = TRUE)
#add your own calculations here for the Tmin and rainfall data series:

#use the lubridate function make_date () to generate a new variable with the 
#  date of observation in sequence
durhamData <- durhamData %>%
  mutate (date = make_date (year, month, day))

#use ggplot to check the data
ggplot (data = durhamData) +
  geom_line (aes (x = date, y = Tmax))
#add your own plots here for the Tmin and rainfall data series:


#calculate annual statistics
durhamAnnual <- durhamData %>%
  group_by (year) %>%
  summarize (Tmax = mean (Tmax, na.rm = TRUE),
             Tmin = mean (Tmin, na.rm = TRUE),
             rainfall = sum (rainfall, na.rm = TRUE))

#use ggplot to check the data
ggplot (data = durhamAnnual) +
  geom_line (aes (x = year, y = Tmax, colour = "Tmax")) +
  geom_line (aes (x = year, y = Tmin, colour = "Tmin")) +
  labs (x = "year", y = "temperature [°C]", colour = "variable")

ggplot (data = durhamAnnual) +
  geom_line (aes (x = year, y = rainfall)) +
  labs (x = "year", y = "rainfall [mm]")

tail (durhamData)

#filter out the 2025 data to stop the odd values at the end
durhamAnnual <- durhamData %>%
  filter (year < 2025) %>%
  group_by (year) %>%
  summarize (Tmax = mean (Tmax, na.rm = TRUE),
             Tmin = mean (Tmin, na.rm = TRUE),
             rainfall = sum (rainfall, na.rm = TRUE))

#... and repeat our plots to check the results
ggplot (data = durhamAnnual) +
  geom_line (aes (x = year, y = Tmax, colour = "Tmax")) +
  geom_line (aes (x = year, y = Tmin, colour = "Tmin")) +
  labs (x = "year", y = "temperature [°C]", colour = "variable")

ggplot (data = durhamAnnual) +
  geom_line (aes (x = year, y = rainfall)) +
  labs (x = "year", y = "rainfall [mm]")

# use rowwise to calculate mean temperatures
durhamAnnual <- durhamAnnual %>%
  rowwise () %>%
  mutate (Tmean = (Tmin + Tmax) / 2.) %>%
  ungroup () 

ggplot (data = durhamAnnual) +
  geom_line (aes (x = year, y = Tmax, colour = "Tmax")) +
  geom_line (aes (x = year, y = Tmean, colour = "Tmean")) +
  geom_line (aes (x = year, y = Tmin, colour = "Tmin")) +
  labs (x = "year", y = "temperature [°C]", colour = "variable")

##Comparing with other long data series

#Reading in the CET and EWP datasets
CET <- read.table ("./data/meantemp_seasonal_totals.txt",
                   sep = "",
                   header = TRUE,
                   skip = 9,
                   na.strings = c ("NA", "-99.9"))

EWP <- read.table ("./data/HadEWP_monthly_totals.txt",
                   sep = "",
                   header = TRUE,
                   skip = 5,
                   na.strings = c ("NA", "-99.9"))

#Let's join these data together with the Durham data
combinedData <- CET %>%
  left_join (EWP, by = c ("Year")) %>%
  filter (Year < 2023) %>%
  mutate (temperature = Annual.x,
          precipitation = Annual.y) %>%
  select (c (Year, temperature, precipitation)) %>%
  left_join (durhamAnnual,
              by = c ("Year" = "year"))

#Combined plots
panel1 <- ggplot (data = combinedData) +
  geom_line (aes (x = Year, y = temperature, colour = "CET")) +
  geom_line (aes (x = Year, y = Tmean, colour = "Durham")) +
  labs (y = "temperature [°C]", colour = "variable") +
  theme (axis.title.x = element_blank (), 
         axis.text.x = element_blank ())

panel2 <- ggplot (data = combinedData) +
  geom_line (aes (x = Year, y = precipitation, colour = "EWP")) +
  geom_line (aes (x = Year, y = rainfall, colour = "Durham")) +
  labs (x = "year", y = "precipitation [mm]", colour = "variable")

panel1Grob <- ggplotGrob (panel1)
panel2Grob <- ggplotGrob (panel2)

grobCombined <- arrangeGrob (rbind (panel1Grob, panel2Grob,  
                                    size = "max"),
                             ncol = 1)
grid.newpage ()
grid.draw (grobCombined)

#Combined plots 2
panel1a <- ggplot (data = combinedData) +
  geom_point (aes (x = temperature, y = Tmean)) +
  geom_abline () +
  geom_smooth (aes (x = temperature, y = Tmean),
               method = "lm", formula = y ~ 0 + x) +
  annotate (geom = "text", x = 10.5, y = 10.2,
            label = "1:1", 
            colour = "black",
            size = 4) + 
  labs (x = "CET temperature [°C]",
        y = "Durham temperature [°C]") 

panel2a <- ggplot (data = combinedData) +
  geom_point (aes (x = precipitation, y = rainfall)) +
  geom_abline () +
  geom_smooth (aes (x = precipitation, y = rainfall),
               method = "lm", formula = y ~ 0 + x) +
  annotate (geom = "text", x = 1040, y = 990,
            label = "1:1", 
            colour = "black",
            size = 4) + 
  labs (x = "EWP precipitation [mm]", 
        y = "Durham precipitation [mm]")

panel1aGrob <- ggplotGrob (panel1a)
panel2aGrob <- ggplotGrob (panel2a)

grobCombined_a <- arrangeGrob (cbind (panel1aGrob, panel2aGrob,  
                                    size = "max"))
grid.newpage ()
grid.draw (grobCombined_a)

#Making predictions
temperatureModel1 <- lm (data = combinedData,
                         Tmean ~ temperature)
summary (temperatureModel1)

#Refit the model with no intercept
temperatureModel2 <- lm (data = combinedData,
                         Tmean ~ 0 + temperature)
summary (temperatureModel2)

#And for precipitation
precipitationModel1 <- lm (data = combinedData,
                           rainfall ~ precipitation)
summary (precipitationModel1)

#Refit the model with no intercept
precipitationModel2 <- lm (data = combinedData,
                         rainfall ~ 0 + precipitation)
summary (precipitationModel2)

#Retrodiction of the Durham data
temperature_CE <- data.frame (temperature = combinedData$temperature)
predTemperatureDurham <- predict (temperatureModel2, 
                                  temperature_CE, 
                                  se.fit = TRUE)

precipitation_EW <- data.frame (precipitation =
                                combinedData$precipitation)
predPrecipitationDurham <- predict (precipitationModel2,
                                    precipitation_EW, 
                                    se.fit = TRUE)

proxiesDurham <- data.frame (Year = combinedData$Year)
proxiesDurham <- cbind (proxiesDurham,
                        temperature = predTemperatureDurham$fit,
                        temperature_lwr = predTemperatureDurham$fit - 
                          2. * predTemperatureDurham$se.fit,
                        temperature_hgr = predTemperatureDurham$fit + 
                          2. * predTemperatureDurham$se.fit,
                        precipitation = predPrecipitationDurham$fit,
                        precipitation_lwr = predPrecipitationDurham$fit - 
                          2. * predPrecipitationDurham$se.fit,
                        precipitation_hgr = predPrecipitationDurham$fit + 
                          2. * predPrecipitationDurham$se.fit)

durham_panel1 <- ggplot (data = proxiesDurham) +
  geom_line (aes (x = Year, y = temperature), 
             colour = "red", alpha = 0.5) + 
  geom_ribbon (aes (x = Year, 
                    ymin = temperature_lwr, 
                    ymax = temperature_hgr),
               fill = "red", alpha = 0.5) +
  geom_point (data = durhamAnnual,
              aes (x = year, y = Tmean), 
              size = 1, colour = "darkgrey") +
  labs (x = "year",
        y = "predicted temperature [°C]") 

durham_panel2 <- ggplot (data = proxiesDurham) +
  geom_line (aes (x = Year, y = precipitation), 
             colour = "blue", alpha = 0.5) + 
  geom_ribbon (aes (x = Year, 
                    ymin = precipitation_lwr, 
                    ymax = precipitation_hgr),
               fill = "blue", alpha = 0.5) +
  geom_point (data = durhamAnnual,
              aes (x = year, y = rainfall), 
              size = 1, colour = "darkgrey") +
  labs (y = "predicted precipitation [mm]") +
  theme (axis.title.x = element_blank (), 
         axis.text.x = element_blank ())

durham_g1 <- ggplotGrob (durham_panel1)
durham_g2 <- ggplotGrob (durham_panel2)

grobCombinedDurham <- arrangeGrob (rbind (durham_g2, durham_g1, 
                                          size = "max"),
                                   ncol = 1)
windows ()
grid.newpage ()
grid.draw (grobCombinedDurham)

#save plot to file
ggsave (file = "./images/DurhamClimateComparisons.png",
        grobCombinedDurham)

#full specification
ggsave ("./images/DurhamClimateComparisons.jpg",
        plot = grobCombinedDurham,
        device = "jpg",
        width = 240,
        height = 240,
        units = "mm",
        dpi = 600)

#Climate change?  Looking at 30-year intervals
dateRanges <- c ("1850-1879", "1880-1909", "1910-1939",
                 "1939-1969", "1970-1999", "2000-2021")
durham30YearBlocks <- durhamAnnual %>%
  mutate (thirty_year = as.factor (dateRanges [trunc ((year - 1850) / 
                                                        30, 0) + 1]))

ggplot  (data = durham30YearBlocks) +
  geom_histogram (aes (x = rainfall)) +
  facet_grid (~thirty_year)

std.err <- function (x){
  output <- sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x)))
  return (output)
}

durhamRainStats1961_1990 <- durhamAnnual %>%
  filter (year >= 1961 & year <= 1990) %>%
  summarize (mean = mean (rainfall, na.rm = TRUE), 
             se = std.err (rainfall), 
             sd = sd (rainfall, na.rm = TRUE))
durhamTempStats1961_1990 <- durhamAnnual %>%
  filter (year >= 1961 & year <= 1990) %>%
  summarize (mean = mean (Tmean, na.rm = TRUE), 
             se = std.err (Tmean), 
             sd = sd (Tmean, na.rm = TRUE))

ggplot  (data = durham30YearBlocks) +
  geom_boxplot (aes (x = thirty_year, y = rainfall)) + 
  geom_abline (intercept = durhamRainStats1961_1990$mean, 
               slope = 0, linetype = "dashed", colour = "blue") +
  geom_abline (intercept = durhamRainStats1961_1990$mean + 
                 2 * durhamRainStats1961_1990$se, 
               slope = 0, linetype = "dotted", colour = "blue") +
  geom_abline (intercept = durhamRainStats1961_1990$mean - 
                 2 * durhamRainStats1961_1990$se, 
               slope = 0, linetype = "dotted", colour = "blue") +
  labs (x = "30-year periods", y = "rainfall  [mm]")

durhamRainAov <- aov (data = durham30YearBlocks, 
                      rainfall ~ thirty_year)
summary (durhamRainAov)
TukeyHSD (durhamRainAov, ordered = TRUE)

ggplot  (data = durham30YearBlocks) +
  geom_boxplot (aes (x = thirty_year, y = Tmean)) + 
  geom_abline (intercept = durhamTempStats1961_1990$mean, 
               slope = 0, linetype = "dashed", colour = "blue") +
  geom_abline (intercept = durhamTempStats1961_1990$mean + 
                 2 * durhamTempStats1961_1990$se, 
               slope = 0, linetype = "dotted", colour = "blue") +
  geom_abline (intercept = durhamTempStats1961_1990$mean - 
                 2 * durhamTempStats1961_1990$se, 
               slope = 0, linetype = "dotted", colour = "blue") +
  labs (x = "30-year periods", y = "mean air temperature  [°C]") 

durhamTempAov <- aov (data = durham30YearBlocks, 
                      Tmean ~ thirty_year)
summary (durhamTempAov)
TukeyHSD (durhamTempAov, ordered = TRUE)

#
#At end of script, close the output file
sink ()
