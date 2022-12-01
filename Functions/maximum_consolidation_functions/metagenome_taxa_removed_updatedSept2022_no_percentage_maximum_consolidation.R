

library(xlsx)
library(dplyr)




metagenome_taxa_removed <- function(){
  
  
  Metagenome <- read.xlsx("..//..//18s_Metagnome//18s_andMetagenome_taxaCounts_updatedSept2022_no_percentage.xlsx", sheetIndex=5, header= FALSE)
  
  
  
  # converting dates to readable dates
  Metagenome[2,6:20]<-as.character(openxlsx::convertToDate(Metagenome[2,6:20]))
  
  
  
  # Creating a df of dates and of taxa+counts
  Metagenome_Taxa_Dates <- Metagenome[2,5:20]
  Metagenome_Taxa_counts<- Metagenome[5:16,5:20]
  
  
  
  
  # Turing all the counts into double data type
  Metagenome_Taxa_counts_double<-lapply(Metagenome_Taxa_counts[,2:ncol(Metagenome_Taxa_counts)], as.numeric)
  
  # making it a data frame
  Metagenome_Dates_Counts<-as.data.frame(Metagenome_Taxa_counts_double,check.names=FALSE)
  
  
  
  # Adding dates as header
  colnames(Metagenome_Dates_Counts)<-c(unlist(Metagenome_Taxa_Dates[2:ncol(Metagenome_Taxa_Dates)]))
  
  
  
  # adding more metagenome tags to help identify differences
  
  # I shouldn't need theses
  
  # names(Metagenome_Dates_Counts)[1] <- "2012-11-27.metagenome"
  # names(Metagenome_Dates_Counts)[3] <- "2012-11-30.metagenome"
  # names(Metagenome_Dates_Counts)[4] <- "2012-12-10.metagenome"
  # names(Metagenome_Dates_Counts)[5] <- "2012-12-17.metagenome"
  # names(Metagenome_Dates_Counts)[6] <- "2013-02-08.metagenome"
  # names(Metagenome_Dates_Counts)[8] <- "2013-02-15.metagenome"
  
  
  
  
  # Adding Taxa
  Metagenome_Dates_Counts$Taxa<-c(unlist(Metagenome_Taxa_counts[1]))
  
  Metagenome_Dates_Counts <- Metagenome_Dates_Counts %>%
    select(Taxa, everything())
  
  
  
  
  # Shouldnt need this
  
  # # removing the taxa discussed 
  # newdata <- Metagenome_Dates_Counts[ !(Metagenome_Dates_Counts$Taxa %in% c("Opisthokonta.Holozoa","Eukaryota;other","Opisthokonta.otherOpisthokonta")), ]
  # 
  # # reorders the numbers
  # newdata<- slice(newdata)
  # 
  # 
  # 
  # # overwriting the Metagenome_Dates_Counts to uninclude taxa discussed
  # Metagenome_Dates_Counts<-newdata
  # 
  
  return(Metagenome_Dates_Counts)
  
}






m<-metagenome_taxa_removed()


















































