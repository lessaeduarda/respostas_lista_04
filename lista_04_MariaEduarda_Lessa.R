# Lista 4 
# Professor: Davi Moreira
# Disciplina: Análise de Dados
# Aluna: Maria Eduarda R. N. Lessa


### Questão 1:

# Link do repositório: 
# https://github.com/lessaeduarda/respostas_lista_04 


### Questão 2: 

# Definir diretório:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/respostas_lista04")

## 2.1 - Use a função read_xlsx do pacote readxl para carregar os dados do PNUD;

# Instalar pacote readxl:
install.packages("readxl")

# Requerer o pacote:
require(readxl)

# Carregar a base de dados através da função "read_xlsx":
dados_PNUD <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx")

## 2.2 - Não deve haver docente com mais de 70 anos ou com menos de 18 anos;

# Instalar pacote "dplyr":
install.packages("dplyr")

# Instalar pacote para usar o pipe:
install.packages("magrittr")

# Requerer pacote:
library(magrittr)

# Requerer pacote:
library(dplyr)

# Carregar base de dados:
load("docentes_pe_censo_escolar_2016.RData")

# Filtrar idade (mínima de 18 e máxima de 70), chamar nova base de "docentes_pe_selec":
docentes_pe_selec <- docentes_pe %>% filter(NU_IDADE >= 18, 
                                            NU_IDADE <= 70)

# Checar filtro através das estatísticas descritivas da nova base: 
summary(docentes_pe_selec$NU_IDADE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   32.00   39.00   39.96   48.00   70.00 

## 2.3 - Não deve haver aluno com mais de 25 anos ou com menos de 1 ano;

# Carregar base de dados:
load("matricula_pe_censo_escolar_2016.RData")

# Filtrar idade (mínima de 1 e máxima de 25), chamar nova base de "matricula_pe_selec":
matricula_pe_selec <- matricula_pe %>% filter(NU_IDADE >= 1, 
                                              NU_IDADE <= 25)

# Checar filtro através das estatísticas descritivas da nova base:
summary(matricula_pe_selec$NU_IDADE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   12.00   11.63   15.00   25.00 


## 2.4 - Apresente estatísticas descritivas do número de alunos por docente nos municípios do Estado;

# Requerer pacote "tidyr":
require(tidyr)

# Criar nova base ("matricula_por_municipio"); selecionar apenas 2 variáveis da base original; 
# agrupar o número de matrículas por município e sumarizar em uma nova coluna com o resultado:
matricula_por_municipio <- matricula_pe_selec %>% select(ID_MATRICULA,CO_MUNICIPIO) %>% group_by(CO_MUNICIPIO) %>% summarise(Mat_Muni = n())

# Criar nova base ("docentes_por_municipio"); selecionar apenas 2 variáveis da base original,
# agrupar o número de docentes por município e sumarizar em uma nova coluna com o resultado:
docentes_por_municipio <- docentes_pe_selec %>% select(CO_PESSOA_FISICA, CO_MUNICIPIO) %>% group_by(CO_MUNICIPIO) %>% summarise(Doc_Muni = n()) 

# Criar nova base(matricula_por_docente) através da união de outras duas (matricula_por_municipio, docentes_por_municipio)
# e adicionar variável (que é igual ao número de alunos sobre o número de docentes): 
matricula_por_docente <- inner_join(matricula_por_municipio, docentes_por_municipio) %>% mutate(Mat_Doc = Mat_Muni / Doc_Muni)

# Estatísticas descritivas da variável Mat_Doc (quarta coluna), da base "matricula_por_docente":
summary(matricula_por_docente [,4])
  
#    Mat_Doc     
# Min.   :4.431  
# 1st Qu.:5.464  
# Median :5.945  
# Mean   :6.043  
# 3rd Qu.:6.584  
# Max.   :9.557


## 2.5 - Apresente o município com maior número de alunos por docente e seu IDHM;

# Filtrar base do PNUD para resultados dos municípios de Pernambuco (UF 26) mais recentes (2010); selecionar apenas 2 variáveis da base original 
# (IDHM e Codmun7); renomear variável (de Codmun7 para CO_MUNICIPIO); unir nova base à "matricula_por_docente", classificar a variável "Mat_Doc"
# em ordem decrescente. Nomear nova base "aluno_docente_IDHM":
aluno_docente_IDHM <- dados_PNUD %>% filter(UF == 26, ANO == 2010
                                       )%>% select(Codmun7,IDHM
                                                   ) %>% rename(CO_MUNICIPIO = Codmun7
                                                                ) %>% inner_join(matricula_por_docente
                                                                                 ) %>% arrange(desc(Mat_Doc))

# Checar primeiros resultados:
head(aluno_docente_IDHM)
# A tibble: 6 x 5
# CO_MUNICIPIO  IDHM Mat_Muni Doc_Muni Mat_Doc
#    <dbl>      <dbl>    <int>    <int>   <dbl>
# 1   2615805   0.519     6986      731    9.56  
# 2   2615102   0.545     1882      210    8.96
# 3   2609154   0.487     6391      714    8.95
# 4   2607505   0.51      7277      894    8.14
# 5   2607000   0.523     6138      772    7.95
# 6   2606309   0.595     2082      269    7.74

# Município 2615805 (TUPANATINGA) é o que tem o maior número de alunos por docente e IDHM (em 2010) de 0.519.

## 2.6 - Faça o teste do coeficiente de correlação linear de pearson e apresente sua resposta;

# Atribuir "Aluno_por_Docente" à variável "Mat_Doc"(número de matrículas / número de docentes) da base "aluno_docente_IDHM":
Aluno_por_Docente <- aluno_docente_IDHM$Mat_Doc

# Atribuir "IDHM" à variável "IDHM" da base "aluno_docente_IDHM":
IDHM <- aluno_docente_IDHM$IDHM

# Calcular a correlação entre as duas variáveis:
cor(Aluno_por_Docente,IDHM)
# [1] -0.5057435

## 2.7 - Seu script deve salvar a base de dados criada para o cálculo em formato .RData;

# Salvar nova base em .RData:
save(aluno_docente_IDHM, file = "alunos_docentes_IDHM.RData")

### Questão 3: 

# Requerer pacote "ggplot2":
require(ggplot2)

# Usar função "ggplot" para gerar um scatterplot de alunos por docente (eixo x) e IDHM (eixo y):
ggplot(aluno_docente_IDHM, aes(x=Aluno_por_Docente, y=IDHM)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
