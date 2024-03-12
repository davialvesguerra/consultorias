# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

setwd("D:/projetos/consultoria/Carol")

library(microdatasus)
library(tidyverse)

dados <- fetch_datasus(year_start = 2018, year_end = 2019, uf = "DF", information_system = "SIM-DO")
dados <- process_sim(dados)

dados <- fetch_datasus(year_start = 2018, year_end = 2019, uf = "DF", information_system = "SINASC")
dados <- process_sinasc(dados, municipality_data = FALSE)





dir_dados = "./dados/mortalidade/"

#criar os meses nos .csv

for(filename in list.files(dir_dados)){
  dados = read.csv(paste0(dir_dados,filename), encoding = "UTF-8")
  match = str_extract(filename, "(?<=_)[0-9]+")
  
  dados['i_mes_obito'] = match
  
  write.csv(dados, paste0(dir_dados,filename), row.names = F)
  
}


#juntar os dados em um só arquivo
dados = read.csv('./dados/mortalidade/2018_01.csv')

n_files = length(list.files(dir_dados))

for(filename in list.files(dir_dados)[2:n_files]){
  dados_local = read.csv(paste0(dir_dados,filename), encoding = "UTF-8")
  dados = rbind(dados, dados_local)
  
}

write.csv(dados, "./dados/dados_mortalidade.csv", row.names = F)

dados_mortalidade_infantil = dados %>% 
  filter(i_faixa_etaria %in% c("01_04_anos","00_<_1_ano"))

write.csv(dados_mortalidade_infantil, "./dados/dados_mortalidade_infantil.csv", row.names = F)


dados_mortalidade_infantil = read.csv('./dados/dados_mortalidade_infantil.csv', encoding = "UTF-8")

dados_mortalidade_infantil %>% 
  group_by(i_ano_obito, i_mes_obito) %>% 
  summarise(cont = n()) %>% 
  mutate('i_data_obito' = paste("01",i_mes_obito,i_ano_obito,sep = "/")) %>% 
  mutate('i_data_obito' = dmy(i_data_obito)) %>% 
  ggplot(aes(x = i_data_obito, y = cont))+
    geom_line()+
    theme_bw()


dados_mortalidade_infantil %>% 
  group_by(i_ano_obito, ) %>% 
  summarise(cont = n()) %>% 
  mutate('i_data_obito' = year(i_ano_obito)) %>% 
  ggplot(aes(x = i_ano_obito, y = cont))+
  geom_line()+
  theme_bw()


table(dados_mortalidade_infantil$i_sexo)
table(dados_mortalidade_infantil$i_desc_raca_cor, dados_mortalidade_infantil$i_ano_obito)[,1:2] %>% 
  chisq.test()

write.xlsx(dados_mortalidade_infantil, './dados/dados_mortalidade_infantil.xls')


