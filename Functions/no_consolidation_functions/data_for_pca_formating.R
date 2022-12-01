
# 
# library(xlsx)
# library(dplyr)
# 
# 
# 


data_for_pca <-function(){

  
  CTD_data <- read.xlsx("..//..//CTD_data//CTD_B_downcast_12-15_clean.xlsx", sheetIndex=1, header= TRUE)
  
  Chloropyll_data <- read.xlsx("..//..//Palmer_Station_Metadata//Chlorophyll_B.xlsx", sheetIndex=1, header= TRUE)
  # Dissolved_Inorganic_Nutrients_data <- read.xlsx("Palmer_Station_Metadata\\Dissolved_Inorganic_Nutrients_B.xlsx", sheetIndex=1, header= TRUE)
  
  
  
  # I added 2013-12-27 data point at 30m this will need to be address in the future for dissolved inorganic phosphate silicate and nitrate
  # I also added 2012-11-27 data point at 20m for Phosphate because the previous data point was -999
  Dissolved_Inorganic_Nutrients_data <- read.xlsx("..//..//Palmer_Station_Metadata//Dissolved_Inorganic_Nutrients_B_27_12_2013_at_30m_phosphate_B_27_11_2012_20m.xlsx", sheetIndex=1, header= TRUE)
  
  Primary_Production_B_data <- read.xlsx("..//..//Palmer_Station_Metadata//Primary_Production_B.xlsx", sheetIndex=1, header= TRUE)
  
  

  
  # Dates that were used in metagenomics
  sample_dates <- c("2012-11-27", "2012-11-30", "2012-12-10", "2012-12-17", "2013-02-08", "2013-02-15", "2013-12-27", "2014-01-23", "2014-02-03", "2014-02-10", "2014-02-28", "2014-03-04", "2014-12-01", "2014-12-11", "2015-01-12", "2015-01-19", "2015-02-09", "2015-02-23", "2015-03-09")
  
  
  
  
  
  # Filter rows out by dates
  Chloropyll_crop <- Chloropyll_data %>% filter(Date %in% sample_dates)
  
  # 2013-12-27 is missing
  Dissolved_Inorganic_Nutrients_crop <- Dissolved_Inorganic_Nutrients_data %>% filter(Date %in% sample_dates)
  
  # Had to convert the dates to a character
  Primary_Production_B_data$Date <- as.character(Primary_Production_B_data$Date)
  Primary_Production_B_data_crop <- Primary_Production_B_data %>% filter(Date %in% sample_dates)
  
  
  
  
  
  # Using a pipe function to manipulate data 
  CTD_crop <- CTD_data%>%
    # using a mutate function to add a summer stage based off of month data was collected
    mutate(
      Summer_Stage = if_else(str_detect(Date, "-10-") | str_detect(Date, "-11-"), "Early Summer", if_else(str_detect(Date, "-12-") | str_detect(Date, "-01-"), "Mid Summer", "Late Summer")),
    )
  
  
  
  
  # For some reason the Dates are doubles so I convert to a Char 
  CTD_data$Date <- as.character(CTD_data$Date)
  
  # Filter rows out by dates
  CTD_crop <- CTD_crop %>% filter(CTD_data$Date %in% sample_dates)
  
  
  
  
  # Now that the dates have been cropped I am converting the dates from a character to a "date" data type
  CTD_crop$Date <- as.Date(CTD_crop$Date)
  
  
  
  
  # combining the data sets
  data_for_pca <- cbind(CTD_crop[,3:9],Chloropyll_crop[,3:4])
  
  data_for_pca <- cbind(data_for_pca, Dissolved_Inorganic_Nutrients_crop [,3:5])
  
  data_for_pca <- cbind(data_for_pca, Primary_Production_B_data_crop [,3:4])
  


return(data_for_pca)

}









CTD_crop <-function(){
  
  
  CTD_data <- read.xlsx("..//..//CTD_data//CTD_B_downcast_12-15_clean.xlsx", sheetIndex=1, header= TRUE)
  
  Chloropyll_data <- read.xlsx("..//..//Palmer_Station_Metadata//Chlorophyll_B.xlsx", sheetIndex=1, header= TRUE)
  # Dissolved_Inorganic_Nutrients_data <- read.xlsx("Palmer_Station_Metadata\\Dissolved_Inorganic_Nutrients_B.xlsx", sheetIndex=1, header= TRUE)
  
  
  
  # I added 2013-12-27 data point at 30m this will need to be address in the future for dissolved inorganic phosphate silicate and nitrate
  # I also added 2012-11-27 data point at 20m for Phosphate because the previous data point was -999
  Dissolved_Inorganic_Nutrients_data <- read.xlsx("..//..//Palmer_Station_Metadata//Dissolved_Inorganic_Nutrients_B_27_12_2013_at_30m_phosphate_B_27_11_2012_20m.xlsx", sheetIndex=1, header= TRUE)
  
  Primary_Production_B_data <- read.xlsx("..//..//Palmer_Station_Metadata//Primary_Production_B.xlsx", sheetIndex=1, header= TRUE)
  
  
  
  
  # Dates that were used in metagenomics
  sample_dates <- c("2012-11-27", "2012-11-30", "2012-12-10", "2012-12-17", "2013-02-08", "2013-02-15", "2013-12-27", "2014-01-23", "2014-02-03", "2014-02-10", "2014-02-28", "2014-03-04", "2014-12-01", "2014-12-11", "2015-01-12", "2015-01-19", "2015-02-09", "2015-02-23", "2015-03-09")
  
  
  
  
  
  # Filter rows out by dates
  Chloropyll_crop <- Chloropyll_data %>% filter(Date %in% sample_dates)
  
  # 2013-12-27 is missing
  Dissolved_Inorganic_Nutrients_crop <- Dissolved_Inorganic_Nutrients_data %>% filter(Date %in% sample_dates)
  
  # Had to convert the dates to a character
  Primary_Production_B_data$Date <- as.character(Primary_Production_B_data$Date)
  Primary_Production_B_data_crop <- Primary_Production_B_data %>% filter(Date %in% sample_dates)
  
  
  
  
  
  # Using a pipe function to manipulate data 
  CTD_crop <- CTD_data%>%
    # using a mutate function to add a summer stage based off of month data was collected
    mutate(
      Summer_Stage = if_else(str_detect(Date, "-10-") | str_detect(Date, "-11-"), "Early Summer", if_else(str_detect(Date, "-12-") | str_detect(Date, "-01-"), "Mid Summer", "Late Summer")),
    )
  
  
  
  
  # For some reason the Dates are doubles so I convert to a Char 
  CTD_data$Date <- as.character(CTD_data$Date)
  
  # Filter rows out by dates
  CTD_crop <- CTD_crop %>% filter(CTD_data$Date %in% sample_dates)
  
  
  
  
  # Now that the dates have been cropped I am converting the dates from a character to a "date" data type
  CTD_crop$Date <- as.Date(CTD_crop$Date)
  
  return(CTD_crop)
  
}






