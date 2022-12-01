
library(xlsx)
library(dplyr)

source("..//..//Functions//no_consolidation_functions//metagenome_taxa_removed.R")
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

# Archaeplastida.Chlorophyta

Archaeplastida.Chlorophyta<-function(){
  Archaeplastida.Chlorophyta <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Archaeplastida.Chlorophyta")
  Archaeplastida.Chlorophyta <- Archaeplastida.Chlorophyta[3]
  colnames(Archaeplastida.Chlorophyta)<-"Archaeplastida.Chlorophyta"
  return(Archaeplastida.Chlorophyta)
}



# Archaeplastida.otherArchaeplastida

Archaeplastida.otherArchaeplastida<-function(){
  Archaeplastida.otherArchaeplastida <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Archaeplastida.otherArchaeplastida")
  Archaeplastida.otherArchaeplastida <- Archaeplastida.otherArchaeplastida[3]
  colnames(Archaeplastida.otherArchaeplastida)<-"Archaeplastida.otherArchaeplastida"
  return(Archaeplastida.otherArchaeplastida)
}



# Cryptophyceae.Cryptomonadales.Geminigera

Cryptophyceae.Cryptomonadales.Geminigera<-function(){
  Cryptophyceae.Cryptomonadales.Geminigera <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Cryptophyceae.Cryptomonadales.Geminigera")
  Cryptophyceae.Cryptomonadales.Geminigera <- Cryptophyceae.Cryptomonadales.Geminigera[3]
  colnames(Cryptophyceae.Cryptomonadales.Geminigera)<-"Cryptophyceae.Cryptomonadales.Geminigera"
  return(Cryptophyceae.Cryptomonadales.Geminigera)
}


# Cryptophyceae.otherCryptophyceae

Cryptophyceae.otherCryptophyceae<-function(){
  Cryptophyceae.otherCryptophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Cryptophyceae.otherCryptophyceae")
  Cryptophyceae.otherCryptophyceae <- Cryptophyceae.otherCryptophyceae[3]
  colnames(Cryptophyceae.otherCryptophyceae)<-"Cryptophyceae.otherCryptophyceae"
  return(Cryptophyceae.otherCryptophyceae)
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






# IT CANT FIND IT FOR SOME REASON
# Haptophyta.non-Phaeocystis

# Haptophyta.non-Phaeocystis <- Metagenome_Graph_data_alphabetical %>% 
#   filter(Taxa == "Haptophyta.non-Phaeocystis")
# Haptophyta.non-Phaeocystis <- Haptophyta.non-Phaeocystis[3]
# colnames(Haptophyta.non-Phaeocystis)<-"Haptophyta.non-Phaeocystis"


# Opisthokonta.Holozoa

Opisthokonta.Holozoa<-function(){
  Opisthokonta.Holozoa <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Opisthokonta.Holozoa")
  Opisthokonta.Holozoa <- Opisthokonta.Holozoa[3]
  colnames(Opisthokonta.Holozoa)<-"Opisthokonta.Holozoa"
  return(Opisthokonta.Holozoa)
}


# Opisthokonta.otherOpisthokonta

Opisthokonta.otherOpisthokonta<-function(){
  Opisthokonta.otherOpisthokonta <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Opisthokonta.otherOpisthokonta")
  Opisthokonta.otherOpisthokonta <- Opisthokonta.otherOpisthokonta[3]
  colnames(Opisthokonta.otherOpisthokonta)<-"Opisthokonta.otherOpisthokonta"
  return(Opisthokonta.otherOpisthokonta)
}


# Picozoa.otherPicozoa

Picozoa.otherPicozoa<-function(){
  Picozoa.otherPicozoa <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Picozoa.otherPicozoa")
  Picozoa.otherPicozoa <- Picozoa.otherPicozoa[3]
  colnames(Picozoa.otherPicozoa)<-"Picozoa.otherPicozoa"
  return(Picozoa.otherPicozoa)
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



# Alveolata.otherAlveolata

Alveolata.otherAlveolata<-function(){
  Alveolata.otherAlveolata <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Alveolata.otherAlveolata")
  Alveolata.otherAlveolata <- Alveolata.otherAlveolata[3]
  colnames(Alveolata.otherAlveolata)<-"Alveolata.otherAlveolata"
  return(Alveolata.otherAlveolata)
}



# Rhizaria.Cercozoa

Rhizaria.Cercozoa<-function(){
  Rhizaria.Cercozoa <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Rhizaria.Cercozoa")
  Rhizaria.Cercozoa <- Rhizaria.Cercozoa[3]
  colnames(Rhizaria.Cercozoa)<-"Rhizaria.Cercozoa"
  return(Rhizaria.Cercozoa)
}



# Rhizaria.otherRhizaria

Rhizaria.otherRhizaria<-function(){
  Rhizaria.otherRhizaria <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Rhizaria.otherRhizaria")
  Rhizaria.otherRhizaria <- Rhizaria.otherRhizaria[3]
  colnames(Rhizaria.otherRhizaria)<-"Rhizaria.otherRhizaria"
  return(Rhizaria.otherRhizaria)
}



# SAR_unclassified

SAR_unclassified<-function(){
  SAR_unclassified <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "SAR_unclassified")
  SAR_unclassified <- SAR_unclassified[3]
  colnames(SAR_unclassified)<-"SAR_unclassified"
  return(SAR_unclassified)
}



# Stramenopiles.MAST-2

Stramenopiles.MAST_2<-function(){
  Stramenopiles.MAST_2 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.MAST-2")
  Stramenopiles.MAST_2 <- Stramenopiles.MAST_2[3]
  colnames(Stramenopiles.MAST_2)<-"Stramenopiles.MAST-2"
  return(Stramenopiles.MAST_2)
}



# Stramenopiles.MAST-3

Stramenopiles.MAST_3<-function(){
  Stramenopiles.MAST_3 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.MAST-3")
  Stramenopiles.MAST_3 <- Stramenopiles.MAST_3[3]
  colnames(Stramenopiles.MAST_3)<-"Stramenopiles.MAST-3"
  return(Stramenopiles.MAST_3)
}



# Stramenopiles.Diatomea.Bacillariophytina

Stramenopiles.Diatomea.Bacillariophytina<-function(){
  Stramenopiles.Diatomea.Bacillariophytina <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.Bacillariophytina")
  Stramenopiles.Diatomea.Bacillariophytina <- Stramenopiles.Diatomea.Bacillariophytina[3]
  colnames(Stramenopiles.Diatomea.Bacillariophytina)<-"Stramenopiles.Diatomea.Bacillariophytina"
  return(Stramenopiles.Diatomea.Bacillariophytina)
}




# Stramenopiles.Diatomea.Coscinodiscophytina

Stramenopiles.Diatomea.Coscinodiscophytina<-function(){
  Stramenopiles.Diatomea.Coscinodiscophytina <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.Coscinodiscophytina")
  Stramenopiles.Diatomea.Coscinodiscophytina <- Stramenopiles.Diatomea.Coscinodiscophytina[3]
  colnames(Stramenopiles.Diatomea.Coscinodiscophytina)<-"Stramenopiles.Diatomea.Coscinodiscophytina"
  return(Stramenopiles.Diatomea.Coscinodiscophytina)
}



# Stramenopiles.Diatomea.otherDiatomea

Stramenopiles.Diatomea.otherDiatomea<-function(){
  Stramenopiles.Diatomea.otherDiatomea <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.otherDiatomea")
  Stramenopiles.Diatomea.otherDiatomea <- Stramenopiles.Diatomea.otherDiatomea[3]
  colnames(Stramenopiles.Diatomea.otherDiatomea)<-"Stramenopiles.Diatomea.otherDiatomea"
  return(Stramenopiles.Diatomea.otherDiatomea)
}



# Stramenopiles.Diatomea.ME-Euk-FW10

Stramenopiles.Diatomea.ME_Euk_FW10<-function(){
  Stramenopiles.Diatomea.ME_Euk_FW10 <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Diatomea.ME-Euk-FW10")
  Stramenopiles.Diatomea.ME_Euk_FW10 <- Stramenopiles.Diatomea.ME_Euk_FW10[3]
  colnames(Stramenopiles.Diatomea.ME_Euk_FW10)<-"Stramenopiles.Diatomea.ME-Euk-FW10"
  return(Stramenopiles.Diatomea.ME_Euk_FW10)
}


# Stramenopiles.Dictyochophyceae.Dictyochales

Stramenopiles.Dictyochophyceae.Dictyochales<-function(){
  Stramenopiles.Dictyochophyceae.Dictyochales <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.Dictyochales")
  Stramenopiles.Dictyochophyceae.Dictyochales <- Stramenopiles.Dictyochophyceae.Dictyochales[3]
  colnames(Stramenopiles.Dictyochophyceae.Dictyochales)<-"Stramenopiles.Dictyochophyceae.Dictyochales"
  return(Stramenopiles.Dictyochophyceae.Dictyochales)
}


# Stramenopiles.Dictyochophyceae.otherDictyochophyceae

Stramenopiles.Dictyochophyceae.otherDictyochophyceae<-function(){
  Stramenopiles.Dictyochophyceae.otherDictyochophyceae <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.otherDictyochophyceae")
  Stramenopiles.Dictyochophyceae.otherDictyochophyceae <- Stramenopiles.Dictyochophyceae.otherDictyochophyceae[3]
  colnames(Stramenopiles.Dictyochophyceae.otherDictyochophyceae)<-"Stramenopiles.Dictyochophyceae.otherDictyochophyceae"
  return(Stramenopiles.Dictyochophyceae.otherDictyochophyceae)
}


# Stramenopiles.Dictyochophyceae.Florenciellales

Stramenopiles.Dictyochophyceae.Florenciellales<-function(){
  Stramenopiles.Dictyochophyceae.Florenciellales <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.Dictyochophyceae.Florenciellales")
  Stramenopiles.Dictyochophyceae.Florenciellales <- Stramenopiles.Dictyochophyceae.Florenciellales[3]
  colnames(Stramenopiles.Dictyochophyceae.Florenciellales)<-"Stramenopiles.Dictyochophyceae.Florenciellales"
  return(Stramenopiles.Dictyochophyceae.Florenciellales)
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



# Stramenopiles.otherStramenopiles

Stramenopiles.otherStramenopiles<-function(){
  Stramenopiles.otherStramenopiles <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Stramenopiles.otherStramenopiles")
  Stramenopiles.otherStramenopiles <- Stramenopiles.otherStramenopiles[3]
  colnames(Stramenopiles.otherStramenopiles)<-"Stramenopiles.otherStramenopiles"
  return(Stramenopiles.otherStramenopiles)
}



# Eukaryota;other

Eukaryota_other<-function(){
  Eukaryota_other <- Metagenome_Graph_data_alphabetical %>% 
    filter(Taxa == "Eukaryota;other")
  Eukaryota_other <- Eukaryota_other[3]
  colnames(Eukaryota_other)<-"Eukaryota;other"
  return(Eukaryota_other)
}





