library(tidyverse)
library(data.table)

criar_tabela = function(data, coluna){
  
  nome_coluna = coluna
  
  tabela <- data %>% 
    rename('var'= coluna) %>% 
    select(var) %>% 
    group_by(var) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>% 
    mutate(freq_relativa = str_replace(str_c(round((freq/sum(freq))*100,2),"%"),'\\.',','))
  
  
  colnames(tabela) = c(nome_coluna, "Frequência absoluta", "Frequência  relativa")
  tabela
}

calculate_prais_winsten_summary_simples = function(dados, grupo){
  
  formula = as.formula(paste('log_taxa_mort', "~", 'ano'))
  pw = prais_winsten(formula, data=dados, index='ano') %>%
  summary

  data.frame(coeficiente = pw$coefficients[2, 1],
             std_error = pw$coefficients[2, 2],
             est_ = pw$coefficients[2, 3],
             p_valor = pw$coefficients[2, 4],
             grupo = grupo)  
}

calculate_prais_winsten_summary <- function(data, group_var, response_var, time_var) {
  # Pré-alocando o data.table
  categorias = unique(data[[group_var]])
  # pw_data_cor = data.table(coeficiente=numeric(length(categorias)), 
  #                          std_error=numeric(length(categorias)), 
  #                          est_=numeric(length(categorias)), 
  #                          p_valor=numeric(length(categorias)), 
  #                          grupo=categorias)
  # 
  # Usando lapply para iterar sobre as categorias
  results = lapply(categorias, function(categoria) {
    
    print("####################")
    print(categoria)
    
    filtered_data = data[data[[group_var]] == categoria,]
    formula = as.formula(paste(response_var, "~", time_var))
    pw = prais_winsten(formula, data=filtered_data, index=time_var) %>%
      summary
    
    list(coeficiente = pw$coefficients[2, 1],
         std_error = pw$coefficients[2, 2],
         est_ = pw$coefficients[2, 3],
         p_valor = pw$coefficients[2, 4],
         grupo = categoria)
  })
  
  # Combinando os resultados
  rbindlist(results)
}

sumarizar_dados_por_ano <- function(dados, col_ano, col_var_aux) {
  dados %>% 
    group_by(ano = .data[[col_ano]], grupo = .data[[col_var_aux]]) %>%
    summarise(freq = n(), .groups = 'drop') 
}


unir_mortalidade_nascidos_vivos  = function(data_mortalidade, data_nascidos_vivos, categorias_filtradas=c("")){
  merge(data_mortalidade, data_nascidos_vivos, by =c("ano", "grupo"), all = TRUE) %>% 
    mutate(freq.x = replace_na(freq.x,0),
           taxa_mort = 1000*freq.x/freq.y,
           log_taxa_mort = log(taxa_mort)) %>% 
  filter(!(grupo %in% categorias_filtradas))
}

unir_mortalidade_nascidos_vivos_simples  = function(data_mortalidade, data_nascidos_vivos, categorias_filtradas=c(""), colunas_em_comum = c("ano", "grupo")){
  merge(data_mortalidade, data_nascidos_vivos, by = colunas_em_comum, all = TRUE) %>% 
    mutate(freq.x = replace_na(freq.x,0),
           taxa_mort = 1000*freq.x/freq.y,
           log_taxa_mort = log(taxa_mort)) 
}

pegar_dados_mortalidade_nascidos_vivos_2018_2022 = function(dados_mortalidade_nascidos_vivos){
  
  dados_mortalidade_nascidos_vivos %>% 
    select(-c(freq.y,log_taxa_mort)) %>% 
    pivot_wider(names_from = c("ano"), values_from = c("taxa_mort","freq.x")) %>% 
    select(grupo, freq.x_2018, taxa_mort_2018,
           freq.x_2022, taxa_mort_2022) %>%
    na.omit()
}

pegar_dados_mortalidade_nascidos_vivos_total = function(dados_mortalidade_nascidos_vivos){
  
  dados_mortalidade_nascidos_vivos %>% 
    select(-c(freq.y,log_taxa_mort)) %>% 
    pivot_wider(names_from = c("ano"), values_from = c("taxa_mort","freq.x")) %>% 
    select(grupo, freq.x_2018, taxa_mort_2018,
           freq.x_2019, taxa_mort_2019,
           freq.x_2020, taxa_mort_2020,
           freq.x_2021, taxa_mort_2021,
           freq.x_2022, taxa_mort_2022) %>%
    na.omit()
}


criar_tabela_prais_winsten_summary = function(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten){
  
  merge(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten, by = c("grupo"), all = TRUE) %>% 
    na.omit() %>%
    mutate(coeficiente = as.numeric(coeficiente),
           std_error = as.numeric(std_error),
           p_valor = round(as.numeric(p_valor), 3),
           VPA = round(-1 + (10**coeficiente), 3),
           IC_sup = round(-1 + 10**(qt(0.975, 4) * std_error), 2),
           IC_inf = round(-1 + 10**(qt(0.025, 4) * std_error), 2),
           tendencia = ifelse(coeficiente < 0, "↓", "↑"),
           tendencia = ifelse(p_valor < 0.05, tendencia, "-"),
           IC = paste(IC_inf, IC_sup, sep = " - ")) %>%
    select(grupo,
           freq.x_2018, taxa_mort_2018,
           freq.x_2022, taxa_mort_2022,
           p_valor, VPA, IC, tendencia)
  
  
}


criar_tabela_prais_winsten_summary_total = function(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten){
  
  merge(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten, by = c("grupo"), all = TRUE) %>% 
    na.omit() %>%
    mutate(coeficiente = as.numeric(coeficiente),
           std_error = as.numeric(std_error),
           p_valor = round(as.numeric(p_valor), 3),
           VPA = round(-1 + (10**coeficiente), 3),
           IC_sup = round(-1 + 10**(qt(0.975, 4) * std_error), 2),
           IC_inf = round(-1 + 10**(qt(0.025, 4) * std_error), 2),
           tendencia = ifelse(coeficiente < 0, "↓", "↑"),
           tendencia = ifelse(p_valor < 0.05, tendencia, "-"),
           IC = paste(IC_inf, IC_sup, sep = " - ")) %>%
    select(grupo,
           freq.x_2018, taxa_mort_2018,
           freq.x_2019, taxa_mort_2019,
           freq.x_2020, taxa_mort_2020,
           freq.x_2021, taxa_mort_2021,
           freq.x_2022, taxa_mort_2022,
           p_valor, VPA, IC, tendencia)
  
  
}

criar_tabela_prais_winsten_summary_total_simples = function(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten){
  
  cbind(dados_mortalidade_nascidos_vivos_2018_2022, dados_prais_winsten) %>% 
    na.omit() %>%
    mutate(coeficiente = as.numeric(coeficiente),
           std_error = as.numeric(std_error),
           p_valor = round(as.numeric(p_valor), 3),
           VPA = round(-1 + (10**coeficiente), 3),
           IC_sup = round(-1 + 10**(qt(0.975, 4) * std_error), 2),
           IC_inf = round(-1 + 10**(qt(0.025, 4) * std_error), 2),
           tendencia = ifelse(coeficiente < 0, "↓", "↑"),
           tendencia = ifelse(p_valor < 0.05, tendencia, "-"),
           IC = paste(IC_inf, IC_sup, sep = " - ")) %>%
    select(grupo,
           freq.x_2018, taxa_mort_2018,
           freq.x_2019, taxa_mort_2019,
           freq.x_2020, taxa_mort_2020,
           freq.x_2021, taxa_mort_2021,
           freq.x_2022, taxa_mort_2022,
           p_valor, VPA, IC, tendencia)
  
  
}
