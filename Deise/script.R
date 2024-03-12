pacman::p_load('tidyverse')

setwd('D:/projetos/consultoria/Deise')

data1 = read.csv('./PESQUISA - PROFESSORES CURSISTAS - CENTRO DE VIVÊNCIAS LÚDICAS – OFICINAS PEDAGÓGICAS (CVLOPs).csv')
data1 = data1[,4:dim(data1)[2]]

nome_cols = c("Sexo", "Naturalidade", "Cor", "Idade","Fez curso de magistério",
              "Instituição de magistério",'Possui graduação', 'Tipo de graduação',
              'Instituição de graduação','Possui especialização Latu Sensus',
              'Instituição do Latu Sensus', 'Tipo do Latu Sensus',
              'Possui mestrado', 'Instituição do mestrado', 'Tipo do mestrado',
              'Possui doutorado', 'Instituição do doutorado', 'Tipo do doutorado',
              'Possui pós-doutorado', 'Instituição do pós-doutorado', 'Tipo do pós-doutorado', 
              "Possui pós-doutorado em outra área",
              "Tempo de docência na educação pública", "Tempo de docência na educação privada",
              "Tempo de docência no SEEDF", "Coordenação regional de ensino",
              "Modalidade de ensino atual", "Outros locais de trabalho",
              "Carga horária de trabalho no SEEDF", "Vínculo institucional no SEEDF",
              "É professor readaptado",
              "Realizou alguns dos cursos das oficinas pedagógicas da SEEDF",
              "Oficina pedagógica em que realizou o curso","Oficina pedagógica em que está realizando o curso",
              "Curso realizado atualmente", "Outros cursos realizados","Tempo em que participa do curso",
              "Principais razões em fazer o curso", "Atividades que contribuiram para o trabalho docente",
              "Referências utilizadas","O que representa a ludicidade","Significado de ser professor cursista")


colnames(data1)[1:length(nome_cols)] = nome_cols




barras = function(data, coluna, nome_coluna){
  
  tabela <- data %>% 
    rename('var'= coluna) %>% 
    select(var) %>% 
    group_by(var) %>%
    summarise(freq = n()) %>%
    mutate(freq_relativa = round((freq/sum(freq))*100,2))
  
  
  porcentagens <- str_c(tabela$freq_relativa, '%') %>%
    str_replace('\\.',',')
  
  tabela %>% 
    ggplot(aes(x=var, y=freq_relativa, label=porcentagens)) +
    geom_bar(stat="identity", fill="#A11D21") +
    geom_text(vjust = -0.5, size = 4) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,25),
                       labels = str_c(seq(0,100,25),'%')
    )+
    labs(x=nome_coluna, y="Frequência") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
  
}

barras(data1, 'Sexo', 'Sexo')
barras(data1, 'Cor', 'Cor')
barras(data1, nome_cols[5], nome_cols[5])
















