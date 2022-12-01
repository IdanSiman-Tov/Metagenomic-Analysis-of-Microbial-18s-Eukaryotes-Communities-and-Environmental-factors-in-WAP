
library(xlsx)
library(dplyr)

source("..//..//Functions//maximum_consolidation_functions//metagenome_taxa_removed_updatedSept2022_no_percentage_maximum_consolidation.R")
source("..//..//Functions//My_Functions.R")


Metagenome_average_counts<-metagenome_taxa_removed()

# # Using an external xlsx file i made of percentages of taxa instead of counts
# Metagenome_percent_counts <- read.xlsx("Metagenome_percent_counts.xlsx", sheetIndex=1, header= TRUE)
# 
# Metagenome_average_counts_intermidiary<- Metagenome_average_counts
# 
# Metagenome_average_counts_intermidiary[1:27,2:20]<-Metagenome_percent_counts[1:27,3:20]
# 
# Metagenome_percent_counts<-Metagenome_average_counts_intermidiary


Metagenome_percent_counts<- Percentage_Function(Metagenome_average_counts)




# Turing the df into a table for ggplot of percentages
Metagenome_Graph_data<-setNames(melt(Metagenome_percent_counts), c('Taxa', 'Date', 'Counts'))






# Adding the Metagenome data to the PCA df


Metagenome_Graph_data_alphabetical <- Metagenome_Graph_data[order(Metagenome_Graph_data$Taxa),]







# CREATING A DF FOR EACH TAXA

# Archaeplastida.Chlorophyta.and.otherArchaeplastida

Archaeplastida.Chlorophyta.and.otherArchaeplastida<-function(){
  Archaeplastida.Chlorophyta.and.otherArchaeplastida <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Archaeplastida.Chlorophyta.and.otherArchaeplastida")
  Archaeplastida.Chlorophyta.and.otherArchaeplastida <- Archaeplastida.Chlorophyta.and.otherArchaeplastida[3]
  colnames(Archaeplastida.Chlorophyta.and.otherArchaeplastida)<-"Archaeplastida.Chlorophyta.and.otherArchaeplastida"
  return(Archaeplastida.Chlorophyta.and.otherArchaeplastida)
}



# Cryptophyceae.Geminigera.and.otherCryptophyceae

Cryptophyceae.Geminigera.and.otherCryptophyceae<-function(){
  Cryptophyceae.Geminigera.and.otherCryptophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Cryptophyceae.Geminigera.and.otherCryptophyceae")
  Cryptophyceae.Geminigera.and.otherCryptophyceae <- Cryptophyceae.Geminigera.and.otherCryptophyceae[3]
  colnames(Cryptophyceae.Geminigera.and.otherCryptophyceae)<-"Cryptophyceae.Geminigera.and.otherCryptophyceae"
  return(Cryptophyceae.Geminigera.and.otherCryptophyceae)
}





# Excavata.Discoba.Jakobida

Excavata.Discoba.Jakobida<-function(){
  Excavata.Discoba.Jakobida <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Excavata.Discoba.Jakobida")
  Excavata.Discoba.Jakobida <- Excavata.Discoba.Jakobida[3]
  colnames(Excavata.Discoba.Jakobida)<-"Excavata.Discoba.Jakobida"
  return(Excavata.Discoba.Jakobida)
}


# Haptophyta.Phaeocystis.and.non-Phaeocystis

Haptophyta.Phaeocystis.and.non_Phaeocystis<-function(){
  Haptophyta.Phaeocystis.and.non_Phaeocystis <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Haptophyta.Phaeocystis.and.non-Phaeocystis")
  Haptophyta.Phaeocystis.and.non_Phaeocystis <- Haptophyta.Phaeocystis.and.non_Phaeocystis[3]
  colnames(Haptophyta.Phaeocystis.and.non_Phaeocystis)<-"Haptophyta.Phaeocystis.and.non-Phaeocystis"
  return(Haptophyta.Phaeocystis.and.non_Phaeocystis)
}







# Picozoa.Picomonadida

Picozoa.Picomonadida<-function(){
  Picozoa.Picomonadida <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Picozoa.Picomonadida")
  Picozoa.Picomonadida <- Picozoa.Picomonadida[3]
  colnames(Picozoa.Picomonadida)<-"Picozoa.Picomonadida"
  return(Picozoa.Picomonadida)
}




# Alveolata.Dinoflagellata

Alveolata.Dinoflagellata<-function(){
  Alveolata.Dinoflagellata <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Alveolata.Dinoflagellata")
  Alveolata.Dinoflagellata <- Alveolata.Dinoflagellata[3]
  colnames(Alveolata.Dinoflagellata)<-"Alveolata.Dinoflagellata"
  return(Alveolata.Dinoflagellata)
}




# Rhizaria.Cercozoa

Rhizaria.Cercozoa<-function(){
  Rhizaria.Cercozoa <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Rhizaria.Cercozoa")
  Rhizaria.Cercozoa <- Rhizaria.Cercozoa[3]
  colnames(Rhizaria.Cercozoa)<-"Rhizaria.Cercozoa"
  return(Rhizaria.Cercozoa)
}





# SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae

SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae<-function(){
  SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae")
  SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae <- SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae[3]
  colnames(SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae)<-"SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae"
  return(SAR_unclassified.and.otherStramenopiles.and.otherAlveolata.and.Ochrophyta.and.Phaeophyceae)
}




# Stramenopiles.MAST_2.and.MAST_3

Stramenopiles.MAST_2.and.MAST_3<-function(){
  Stramenopiles.MAST_2.and.MAST_3 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.MAST-2.and.MAST-3")
  Stramenopiles.MAST_2.and.MAST_3 <- Stramenopiles.MAST_2.and.MAST_3[3]
  colnames(Stramenopiles.MAST_2.and.MAST_3)<-"Stramenopiles.MAST-2.and.MAST-3"
  return(Stramenopiles.MAST_2.and.MAST_3)
}



# Stramenopiles.Diatomea.all

Stramenopiles.Diatomea.all<-function(){
  Stramenopiles.Diatomea.all <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.all")
  Stramenopiles.Diatomea.all <- Stramenopiles.Diatomea.all[3]
  colnames(Stramenopiles.Diatomea.all)<-"Stramenopiles.Diatomea.all"
  return(Stramenopiles.Diatomea.all)
}






# Stramenopiles.Dictyochophyceae.all

Stramenopiles.Dictyochophyceae.all<-function(){
  Stramenopiles.Dictyochophyceae.all <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.all")
  Stramenopiles.Dictyochophyceae.all <- Stramenopiles.Dictyochophyceae.all[3]
  colnames(Stramenopiles.Dictyochophyceae.all)<-"Stramenopiles.Dictyochophyceae.all"
  return(Stramenopiles.Dictyochophyceae.all)
}






# Eukaryota;other

Eukaryota_other<-function(){
  Eukaryota_other <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Eukaryota;other")
  Eukaryota_other <- Eukaryota_other[3]
  colnames(Eukaryota_other)<-"Eukaryota;other"
  return(Eukaryota_other)
}


