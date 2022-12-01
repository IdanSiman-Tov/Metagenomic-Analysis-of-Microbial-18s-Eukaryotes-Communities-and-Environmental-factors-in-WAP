
library(xlsx)
library(dplyr)

source("..//..//Functions//medium_consolidation_functions//metagenome_taxa_removed_updatedSept2022_no_percentage_medium_consolidation.R")
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


# Haptophyta.Phaeocystis

Haptophyta.Phaeocystis<-function(){
  Haptophyta.Phaeocystis <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Haptophyta.Phaeocystis")
  Haptophyta.Phaeocystis <- Haptophyta.Phaeocystis[3]
  colnames(Haptophyta.Phaeocystis)<-"Haptophyta.Phaeocystis"
  return(Haptophyta.Phaeocystis)
}






# Haptophyta.non_Phaeocystis

Haptophyta.non_Phaeocystis<-function(){
  Haptophyta.non_Phaeocystis <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Haptophyta.non_Phaeocystis")
  Haptophyta.non_Phaeocystis <- Haptophyta.non_Phaeocystis[3]
  colnames(Haptophyta.non_Phaeocystis)<-"Haptophyta.non_Phaeocystis"
  return(Haptophyta.non_Phaeocystis)
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





# SAR_unclassified.and.otherStramenopiles.and.otherAlveolata

SAR_unclassified.and.otherStramenopiles.and.otherAlveolata<-function(){
  SAR_unclassified.and.otherStramenopiles.and.otherAlveolata <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "SAR_unclassified.and.otherStramenopiles.and.otherAlveolata")
  SAR_unclassified.and.otherStramenopiles.and.otherAlveolata <- SAR_unclassified.and.otherStramenopiles.and.otherAlveolata[3]
  colnames(SAR_unclassified.and.otherStramenopiles.and.otherAlveolata)<-"SAR_unclassified.and.otherStramenopiles.and.otherAlveolata"
  return(SAR_unclassified.and.otherStramenopiles.and.otherAlveolata)
}




# Stramenopiles.MAST_2.and.MAST_3

Stramenopiles.MAST_2.and.MAST_3<-function(){
  Stramenopiles.MAST_2.and.MAST_3 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.MAST-2.and.MAST-3")
  Stramenopiles.MAST_2.and.MAST_3 <- Stramenopiles.MAST_2.and.MAST_3[3]
  colnames(Stramenopiles.MAST_2.and.MAST_3)<-"Stramenopiles.MAST-2.and.MAST-3"
  return(Stramenopiles.MAST_2.and.MAST_3)
}



# Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea

Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea<-function(){
  Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea")
  Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea <- Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea[3]
  colnames(Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea)<-"Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea"
  return(Stramenopiles.Diatomea.Bacillariophytina.and.Coscinodiscophytina.and.otherDiatomea)
}





# Stramenopiles.Diatomea.ME-Euk-FW10

Stramenopiles.Diatomea.ME_Euk_FW10<-function(){
  Stramenopiles.Diatomea.ME_Euk_FW10 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.ME-Euk-FW10")
  Stramenopiles.Diatomea.ME_Euk_FW10 <- Stramenopiles.Diatomea.ME_Euk_FW10[3]
  colnames(Stramenopiles.Diatomea.ME_Euk_FW10)<-"Stramenopiles.Diatomea.ME-Euk-FW10"
  return(Stramenopiles.Diatomea.ME_Euk_FW10)
}


# Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae

Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae<-function(){
  Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae")
  Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae <- Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae[3]
  colnames(Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae)<-"Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae"
  return(Stramenopiles.Dictyochophyceae.Dictyochales.and.Florenciellales.and.otherDictyochophyceae)
}




# Stramenopiles.Dictyochophyceae.Pedinellales

Stramenopiles.Dictyochophyceae.Pedinellales<-function(){
  Stramenopiles.Dictyochophyceae.Pedinellales <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.Pedinellales")
  Stramenopiles.Dictyochophyceae.Pedinellales <- Stramenopiles.Dictyochophyceae.Pedinellales[3]
  colnames(Stramenopiles.Dictyochophyceae.Pedinellales)<-"Stramenopiles.Dictyochophyceae.Pedinellales"
  return(Stramenopiles.Dictyochophyceae.Pedinellales)
}



# Stramenopiles.Ochrophyta.other

Stramenopiles.Ochrophyta.other<-function(){
  Stramenopiles.Ochrophyta.other <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Ochrophyta.other")
  Stramenopiles.Ochrophyta.other <- Stramenopiles.Ochrophyta.other[3]
  colnames(Stramenopiles.Ochrophyta.other)<-"Stramenopiles.Ochrophyta.other"
  return(Stramenopiles.Ochrophyta.other)
}


# Stramenopiles.Phaeophyceae

Stramenopiles.Phaeophyceae<-function(){
  Stramenopiles.Phaeophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Phaeophyceae")
  Stramenopiles.Phaeophyceae <- Stramenopiles.Phaeophyceae[3]
  colnames(Stramenopiles.Phaeophyceae)<-"Stramenopiles.Phaeophyceae"
  return(Stramenopiles.Phaeophyceae)
}






# Eukaryota;other

Eukaryota_other<-function(){
  Eukaryota_other <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Eukaryota;other")
  Eukaryota_other <- Eukaryota_other[3]
  colnames(Eukaryota_other)<-"Eukaryota;other"
  return(Eukaryota_other)
}


