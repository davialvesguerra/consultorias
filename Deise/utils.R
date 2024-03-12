
criar_tabela = function(data, coluna, is_nome_coluna = FALSE, nome_coluna="",is_ordem_vars=FALSE, ordem_vars = c()){
  
  
  if(!is_nome_coluna) nome_coluna = coluna
  
  tabela <- data %>% 
    rename('var'= coluna) %>% 
    select(var) %>% 
    group_by(var) %>%
    filter(!var %in% c('')) %>% 
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>% 
    mutate(freq_relativa = str_replace(str_c(round((freq/sum(freq))*100,2),"%"),'\\.',','))
  
  if(is_ordem_vars){
  
    tabela = tabela %>%
      mutate(var = factor(var, ordem_vars)) %>% 
      arrange(var)
  }
  
  
  colnames(tabela) = c(nome_coluna, "Frequência \\ absoluta", "Frequência \\ relativa")
  tabela
}





barras = function(data, coluna, save=FALSE, dir_="", xlab = FALSE, nome_coluna="", is_ordem_vars=FALSE, ordem_vars = c()){
  
  if(!xlab) nome_coluna = coluna

    
  tabela <- data %>% 
    rename('var'= coluna) %>% 
    select(var) %>% 
    group_by(var) %>%
    summarise(freq = n()) %>%
    mutate(freq_relativa = round((freq/sum(freq))*100,2))
  
  if(!is_ordem_vars) ordem_vars = unique(tabela$var)  
  
  tabela = tabela %>% 
    mutate(var = factor(var, ordem_vars))
  
  porcentagens <- str_c(tabela$freq_relativa, '%') %>%
    str_replace('\\.',',')
  
  graf = tabela %>% 
    ggplot(aes(x=var, y=freq_relativa, label=porcentagens)) +
    geom_bar(stat="identity", fill="#006633") +
    geom_text(vjust = -0.5, size = 4) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,25),
                       labels = str_c(seq(0,100,25),'%')
    )+
    labs(x=nome_coluna, y="Frequência") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=10),
          axis.text = element_text(colour = "black", size=7.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
  
  if(save){
    if (!dir.exists(dir_)) dir.create(dir_)
    
    ggsave(paste0(dir_,"/",nome_coluna,".png"), graf)
  }
  
  graf
  
}


# Sort word frequencies in descending order and select top 10 words

detect_word_in_text <- function(word, texto) {
  str_detect(texto, paste0("\\b", word, "\\b"))
}

calculate_word_frequency <- function(data, column, top_n) {
  words <- preprocess_text(data, column)
  words = words[words != ""]
  word_freq <- table(words)
  word_freq_sort <- sort(word_freq, decreasing = TRUE)[1:(top_n)]
  top_words_freq <- names(word_freq_sort)
  
  texto_coluna_resposta = sapply(1:length(data[[column]]),
                                 function(x){
                                   preprocess_text2(data, column, x)
                                 })
  
  

  
  palavras_por_usuario <- sapply(top_words_freq, function(word) {
    sapply(texto_coluna_resposta, function(texto) detect_word_in_text(word, texto)) %>%
      sum
  }) %>%
    sort(decreasing = TRUE)
  
  return(palavras_por_usuario)
}

preprocess_text2 <- function(data, column, indice) {
  column_text <- data[[column]][indice]
  
  # Create a corpus from the text data
  corpus <- Corpus(VectorSource(column_text))
  
  # Preprocess the corpus
  words <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(iconv), to = "ASCII//TRANSLIT") %>%
    tm_map(removeWords, stopwords("portuguese")) %>%
    tm_map(as.character) %>%
    unlist()
  
  return(unlist(words['content']))
}


preprocess_text <- function(data, column) {
  column_text <- data[[column]]
  
  # Create a corpus from the text data
  corpus <- Corpus(VectorSource(column_text))
  
  # Preprocess the corpus
  words <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(iconv), to = "ASCII//TRANSLIT") %>%
    tm_map(removeWords, stopwords("portuguese")) %>%
    tm_map(as.character) %>%
    unlist() %>%
    strsplit("\\W+") %>%
    unlist()
  
  return(words)
}
