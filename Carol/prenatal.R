# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

setwd("D:/projetos/consultoria/Carol")

library(microdatasus)
library(tidyverse)
library(openxlsx)


dados = read.csv('./dados/dados_prenatal.csv', sep = ";", 
                 check.names = F,  fileEncoding = "Latin1") 

write.xlsx(dados, './dados/dados_prenatal.xls')

