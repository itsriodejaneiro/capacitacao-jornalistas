rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)

# Pacotes -----------------------------------------------------------------
## Caso não tenha os pacotes instalados, use:
# install.packages("dplay")
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(readr)

# Base de dados -----------------------------------------------------------

base <- read_csv("Dados/all_#FechadoComBolsonaro2022_2021-06-14 00:00:00_1623963801.csv",
                 col_types = cols(.default = "c"))

resultados <- read_csv("Dados/Handles-.fechadocombolsonaro2022-1623963801-resultados.csv")


# Analise Exploratoria ----------------------------------------------------

## Vamos usar o R Base e o Dplyr para responder algumas perguntas.

# 1) Qual foi o período da coleta?
## Para isso, vamos pegar o ultimo dia de coleta e primeiro para calcular.

periodo <- max(as.Date(base$date)) - min(as.Date(base$date))

# as.Date()

# 2) Quantos registros tem a base de tweets?

n_tweets <- nrow(base)

# 3) Quantos usuários únicos tem na base tweets?

usuarios_unicos <- nrow(distinct(as.data.frame(base$username)))

# 4) Média e desvio padrão de tweets por usuário?
## Vamos pegar primeiro o total de tweets:

total_tweets <- base %>% 
  group_by(username) %>% 
  summarise(N = n())

media_tweets <- round(mean(total_tweets$N), 1) # Media
desvio_tweets <- round(sd(total_tweets$N), 2) # Desvio Padrao

# 5) Quem é bot? Considerar acima de 70%.

base_pegabot <- resultados %>% 
  mutate(Resultado = cut(x = `Análise Total`, 
                         breaks = c(0,.70, Inf),
                         labels = c("Baixa Probabilidade",
                                    "Alta Probabilidade"))) %>% 
  select(`Perfil Twitter`, Resultado)

# 6) Contar quantos bots (porcentagem em relação ao total de usuários)?

bots <- base_pegabot %>% 
  group_by(Resultado) %>% 
  summarise(N = n())

# 7) Indicar os cinco perfis (@) que tuitaram mais vezes

tuiteiros <- base %>% 
  group_by(username) %>% 
  summarise(N = n()) %>% 
  arrange(desc(N)) %>% 
  slice_head(n = 5)

tuiteiros$username <- paste0("@", tuiteiros$username) # Se quiser colocar @

# 8) Indicar os cinco perfis (@) que receberam mais curtidas em seus tweets.

curtidos <- base %>% 
  select(username, tweet, nlikes) %>% 
  arrange(desc(as.numeric(nlikes))) %>%
  slice_head(n = 5)
          

# 9) Indicar a quantidade de tweets e retweets: - Atenção

ntweets <- base %>% 
  distinct(username, tweet) %>% 
  nrow()

nretweets <- sum(as.numeric(base$nretweets))
  
# 10) Apresentar uma visualização gráfica da evolução desses tweets no tempo,
# indicando o dia com mais tweets contendo a hashtag.

Linha_Temporal <- base %>% 
  group_by(as.Date(date)) %>% 
  summarise(Ocorrencias = n())

Gr1 <- ggplot(Linha_Temporal, aes(x = `as.Date(date)`, y = Ocorrencias)) +
  geom_line(color = "red", size = 0.75) +
  labs(title = "CPI da Covid", subtitle = "Perído: 03/05-01/06",
       x = "", y = "N° de tweets") +
  theme_light()


