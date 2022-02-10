library(tidyverse)
library(readxl)

##Read in PMW manual data, using readxl because there are many sheets
read_excel("data/PMW_manual.xlsx")

path <- "data/PMW_manual.xlsx"
path

pmw_data <- path %>% 
  excel_sheets() %>%
  set_names() %>%
  map_dfr(read_excel, path=path)

pmw_data <- test %>%
  select(-("...1"))

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

##Join the tables

combined_welldata <- rbind(pmw_data, ycwa_data)

