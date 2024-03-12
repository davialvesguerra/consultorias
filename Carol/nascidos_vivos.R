setwd("D:/projetos/consultoria/Carol")
library(tidyverse)


dir_dados = "./dados/nascidos vivos/"

#criar os meses nos .csv



for(filename in list.files(dir_dados)){
  dados = read.csv(paste0(dir_dados,filename), encoding = "UTF-8")
  match = str_extract(filename, "(?<=_)[0-9]+")
  
  dados['i_mes_obito'] = match
  
  write.csv(dados, paste0(dir_dados,filename), row.names = F)
  
}


#juntar os dados em um só arquivo
dados = read.csv('./dados/nascidos vivos/2018_01.csv')

n_files = length(list.files(dir_dados))

for(filename in list.files(dir_dados)[2:n_files]){
  dados_local = read.csv(paste0(dir_dados,filename), encoding = "UTF-8")
  dados = rbind(dados, dados_local)
  
}

write.csv(dados, "./dados/dados_nascidos_vivos.csv", row.names = F)


