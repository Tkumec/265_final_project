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

#combined_welldata %>%
#  ggplot(aes(x=Date, y=GW_Below_RP, color = SWN)) +
#  geom_point() +
#  facet_wrap("SWN")

#####USGS streamflow data, tidying up

streamflow_data <- read.table("data/yuba_cfs_2016_2021.txt", sep="\t", header=TRUE)

streamflow_data <- select(streamflow_data, -c("X15678_00060_cd")) %>%
    streamflow_data[-c(1),]

streamflow_data <- rename(streamflow_data, discharge_cfs = X15678_00060)

##Fix datetime for USGS gauge data- Adding columns for each component

streamflow_data <- streamflow_data %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate(Time = minute(datetime))

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

combined_welldata <- combined_welldata %>% 
  mutate(wtr_yr = wtr_yr(Date, 10))

streamflow_data <- streamflow_data %>%
  mutate(wtr_yr = wtr_yr(datetime, 10))







