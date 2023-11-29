# Gerrit Bass August 2023
# EIM time series formatting script
# Takes WISKI exported time series data and formats it for upload to EIM

# SETUP ----------------------------------------------------------------------------------------------------------

#libraries
library(plyr)
library(tidyverse)

# make a list of all your files
data_files <- grep(list.files('data/paradise_creek/'), pattern='.log', invert=TRUE, value=TRUE)

#import EIM time series template
EIM_template <- read_csv("./data/templates/EIMTimeSeriesResultTemplate.csv")
#EIM_cols <- colnames(EIM_template)
# import dataframe with site names and corresponding serial numbers
serial_nums <- read.csv("./data/templates/serial_numbers.csv")

# FUNCTION -----------------------------------------------------------------------------------------------------
# This function formats data from each station and creates its own csv file for upload to EIM

Paradise_EIM_formatter <- function(file){
  
  paradise_file <- read.csv(paste('data/paradise_creek/',file, sep = ""), skip = 15, check.names = FALSE)
  
  station <- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  # filtering logger list by site to get specific serial number
  serial_num <- serial_nums %>% 
    filter(Location_ID == station)
  
  # rename columns so they can rbind with the EIM template
  paradise_data <- paradise_file %>% 
    dplyr::rename("Start_Date" = 1,
                  "Start_Time" = 2,
                  "Result_Value" = 3) 
  
  paradise_eim <- rbind.fill(EIM_template, paradise_data) %>%
    mutate(`State of value` = NULL,
           Study_ID = "WQC-2020-PaloCD-00129",
           Location_ID = station,
           `Study-Specific_Location_ID` = station,
           Field_Collector = "ConsDistrict",
           Matrix = "Water",
           Source = "Fresh/Surface Water",
           Field_Collection_Type = "Measurement",
           Parameter_Name = "Temperature, water",
           Result_Unit = "deg C",
           Result_Method = "TEMPLOGGER",
           Instrument_ID = serial_num$Instrument_ID[1],
           Result_Value = na_if( Result_Value, "---")) %>%
    drop_na(any_of("Result_Value"))
    
  paradise_eim[is.na(paradise_eim)] <- ""
  
  
  write_csv(paradise_eim, paste("outputs/",station,".csv",sep = ""))
}


# runs function on files to format them for EIM
lapply(data_files, Paradise_EIM_formatter)


#TEST ------------------------------------------------------------------------------------------------------
## work space to develop code to go in the function without running it as a function

#import data file
paradise_file <- read.csv(paste('data/paradise_creek/',data_files[9], sep = ""), skip = 15, check.names = FALSE)

#import list of serial numbers that correspond to sites
serial_nums <- read.csv("./data/templates/serial_numbers.csv")

#grab station name from file name
station1<- sapply(strsplit(data_files[9],"_"), `[`, 1) 

# filtering logger list by site to get specific serial number
serial_num <- serial_nums %>% 
  filter(Location_ID == station1)

# rename columns so they can rbind with the EIM template
paradise_data <- paradise_file %>% 
  dplyr::rename("Start_Date" = 1,
         "Start_Time" = 2,
         "Result_Value" = 3,
         "Result_Data_Qualifier" = 4) 
  
paradise_eim <- rbind.fill(EIM_template, paradise_data) %>%
  mutate(`State of value` = NULL,
         Study_ID = "WQC-2020-PaloCD-00129",
         Location_ID = station1,
         `Study-Specific_Location_ID` = station1,
         Field_Collector = "ConsDistrict",
         Matrix = "Water",
         Source = "Fresh/Surface Water",
         Field_Collection_Type = "Measurement",
         Parameter_Name = "Temperature, water",
         Result_Unit = "deg C",
         Result_Method = "TEMPLOGGER",
         Instrument_ID = serial_num$Instrument_ID[1],
         Result_Value = na_if( Result_Value, "---")) %>% # convert "---" values to NA
  drop_na(any_of("Result_Value")) # remove rows with no value

paradise_eim[is.na(paradise_eim)] <- "" # convert NA values left from columns that don't need to be filled to blank strings

write_csv(paradise_eim, paste("outputs/",station1,".csv",sep = ""))






