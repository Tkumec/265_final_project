library(tidyverse)
library(readxl)
library(lubridate)

##Read in PMW manual data, using readxl because there are many sheets
read_excel("data/PMW_manual.xlsx")

path <- "data/PMW_manual.xlsx"
path

pmw_data <- path %>% 
  excel_sheets() %>%
  set_names() %>%
  map_dfr(read_excel, path=path)


pmw_data <- pmw_data %>%
  mutate(dataset = "PMW")

##Read YMA data

path2 <- "data/YCWA_manual.xlsx"

ycwa_data <- path2 %>% 
  excel_sheets() %>%
  set_names() %>%
  map_dfr(read_excel, path=path2)

ycwa_data <- ycwa_data %>%
  mutate(dataset = "YCWA")

##Bind the tables

combined_welldata <- rbind(pmw_data, ycwa_data)

##Remove odd value
combined_welldata <- combined_welldata[-c(1),]

##1963 observation needs to go 
combined_welldata <- filter(combined_welldata, Date > 1980)


#####USGS streamflow data, tidying up

streamflow_data <- read.table("data/yuba_cfs_2016_2021.txt", sep="\t", header=TRUE)

streamflow_data <- select(streamflow_data, -c("X15678_00060_cd"))
streamflow_data <- streamflow_data[-c(1),]

streamflow_data <- rename(streamflow_data, discharge_cfs = X15678_00060)

streamflow_data <- streamflow_data %>%
  slice(which(row_number() %% 96 == 1))


##Fix datetime for USGS gauge data- Adding columns for each component

streamflow_data <- streamflow_data %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) 

combined_welldata <- combined_welldata %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Day = day(Date)) %>%
  mutate(Time = minute(Date))


##Create water year for well data

wtr_yr <- function(dates, start_month) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

##Add water year column
combined_welldata <- combined_welldata %>% 
  mutate(water_year = wtr_yr(Date, 10))

##Make character
combined_welldata$water_year <- as.character(combined_welldata$water_year)

##Add julian
combined_welldata$DayOfYear <- as.numeric(format(combined_welldata$Date, "%j"))

#
streamflow_data <- streamflow_data %>%
  mutate(water_year = wtr_yr(datetime, 10)) 
  
streamflow_data$water_year <- as.character(streamflow_data$water_year)



combined_welldata %>%
  ggplot(aes(x=Date, y=GWSE_ft, color=SWN)) +
  geom_point() +
  geom_line()




streamflow_data %>%
  ggplot(aes(x=datetime, y=discharge_cfs)) +
  geom_point() +
  facet_wrap(.~water_year, scales = "free") 



##Summaries
ave_wells <- combined_welldata %>% 
  group_by(Month, water_year) %>% 
  summarise(average = mean(GWSE_ft)) %>%
  filter(water_year <= 2021)
  

ave_wells %>%
  ggplot(aes(x=Month, y=average)) +
  geom_point() +
  geom_line() +
  facet_grid(~water_year)

##Fixing order of months
#Turn your 'treatment' column into a character vector
ave_wells$Month <- as.character(ave_wells$Month)
#Then turn it back into a factor with the levels in the correct order
ave_wells$Month <- factor(ave_wells$Month, levels=c(10,11,12,1,2,3,4,5,6,7,8,9))


