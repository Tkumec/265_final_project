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

##1963 observation needs to go 
exclude_1963 <- filter(combined_welldata, Date > 1980)

exclude_1963 %>%
  ggplot(aes(x=Date, y=GW_Below_RP, color = SWN)) +
  geom_point() +
  facet_wrap("dataset")

#####USGS streamflow data

raw_gauge_data <- read.table("data/yuba_cfs_2016_2021.txt", sep="\t", header=TRUE)

raw_gauge_data <- select(raw_gauge_data, -c("X15678_00060_cd"))
raw_gauge_data <- raw_gauge_data[-c(1),]

##Fix datetime - Adding columns for each component

raw_gauge_data <- raw_gauge_data %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate(Time = minute(datetime))

