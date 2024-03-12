setwd('D:/projetos/consultoria/Deise')
pacman::p_load('tidyverse')
library(tm)
library(SnowballC)
library(wordcloud)
source('script.R')

data1 = read.csv('./dados_discentes_originais.csv', encoding = "UTF-8")
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
              "Professor readaptado",
              "Realizou alguns dos cursos das oficinas pedagógicas da SEEDF",
              "Oficina pedagógica em que realizou o curso","Oficina pedagógica em que está realizando o curso",
              "Curso realizado atualmente", "Outros cursos realizados","Tempo em que participa do curso",
              "Principais razões em fazer o curso", "Atividades que contribuiram para o trabalho docente",
              "Referências utilizadas","O que representa a ludicidade","Significado de ser professor cursista")


colnames(data1)[1:length(nome_cols)] = nome_cols


# fazer coluna com os dados de pós graduação
# fazer tratamento na coluna "outros cursos realizados"


col_abertas = c("Referências utilizadas",
                "O que representa a ludicidade",
                "Significado de ser professor cursista",
                "Principais razões em fazer o curso",
                "Atividades que contribuiram para o trabalho docente")

col_fechadas = c("Sexo", "Cor", "Idade","Fez curso de magistério",
                 "Instituição de magistério",'Possui graduação', 'Tipo de graduação','Instituição de graduação',
                 "Tempo de docência na educação privada","Tempo de docência na educação pública",
                 'Possui especialização Latu Sensus','Instituição do Latu Sensus', 'Tipo do Latu Sensus',
                 'Possui mestrado', 'Instituição do mestrado', 'Tipo do mestrado',
                 'Possui doutorado', 'Instituição do doutorado', 'Tipo do doutorado',
                 'Possui pós-doutorado', 'Instituição do pós-doutorado', 'Tipo do pós-doutorado', 
                 "Possui pós-doutorado em outra área",
                 "Tempo de docência no SEEDF", "Coordenação regional de ensino",
                 "Modalidade de ensino atual", 
                 "Vínculo institucional no SEEDF",
                 "Professor readaptado",
                 "Realizou alguns dos cursos das oficinas pedagógicas da SEEDF",
                 "Oficina pedagógica em que está realizando o curso",
                 "Curso realizado atualmente", "Tempo em que participa do curso",
                 "Principais razões em fazer o curso")




data1["Sexo"] = unlist(sapply(data1["Sexo"], function(x) str_remove(x, "\\.")))
data1['Possui graduação'] = "Sim"



utils_cols = c(col_fechadas, col_abertas)

data1 = data1[utils_cols]

write.csv(data1, file = "data_discentes.csv")






data2 = read.csv('./dados_docentes_originais.csv', encoding = "UTF-8")
data2 = data2[,4:dim(data2)[2]]

nome_cols_docentes = c(nome_cols[1:32],
                       "Qual oficina pedagógica realizou o curso",
                       "Quanto tempo como professor em formação nos cursos da oficina",
                       "É professor formador no mesmo local que realizou a oficina",
                       "Local em que atua como professor formador",
                       "Tempo de atuação como professor formador",
                       "Tempo como professor formador na oficina atual",
                       "5 razões para ser professor formador",
                       "Qual atividade contribui mais como professor formador",
                       "Atividade que dificulta a atuação como professor formador",
                       "Qual referencial teórico utilizado",
                       "Significado de ludicidade",
                       "Significade de oferecer a ludicidade na formação dos professores",
                       "Significade de ser professor formador")

colnames(data2) = nome_cols_docentes

write.csv(data2, file = "data_docentes.csv")















