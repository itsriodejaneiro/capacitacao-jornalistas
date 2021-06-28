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

base <- read_csv("./capacitacao-jornalistas-main/Dados/all_#FechadoComBolsonaro2022_2021-06-14 00:00:00_1623963801.csv",
                 col_types = cols(.default = "c"))

resultados <- read_csv("./capacitacao-jornalistas-main/Dados/Handles-.fechadocombolsonaro2022-1623963801-resultados.csv")


# Analise Exploratoria ----------------------------------------------------

## Vamos usar o R Base e o Dplyr para responder algumas perguntas.

# 1) Qual foi o período da coleta?
## Para isso, vamos pegar o ultimo dia de coleta e primeiro para calcular.

periodo_horas <- difftime(max(base$date), min(base$date), units = "h")
periodo_horas

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
  top_n(5, N)

tuiteiros$username <- paste0("@", tuiteiros$username) # Se quiser colocar @

# 8) Indicar os cinco perfis (@) que receberam mais curtidas em seus tweets.

curtidos <- base %>% 
  select(username, tweet, nlikes) %>% 
  arrange(desc(as.numeric(nlikes))) %>%
  top_n(5, as.numeric(nlikes))
          
# 9) Indicar a quantidade de tweets e retweets na base - Atenção

nretweets <- base %>%
  filter(retweet == 'True') %>%
  nrow()
nretweets

ntweets <- base %>%
  filter(retweet == 'False') %>%
  nrow()
ntweets

# Calcular a porcentagem de cada um
library(scales)
percent(ntweets/n_tweets)
percent(nretweets/n_tweets)

# 10) Apresentar uma visualização gráfica da evolução desses tweets no tempo,
# indicando o dia com mais tweets contendo a hashtag.

linha_temporal <- base %>% 
  group_by(as.Date(date)) %>% 
  summarise(ocorrencias = n())

plot <- ggplot(linha_temporal, aes(x = `as.Date(date)`, y = ocorrencias)) +
  geom_line(color = "red", size = 0.75) +
  labs(title = "CPI da Covid", subtitle = "Período: 14/06 à 17/06",
       x = "", y = "N° de tweets") +
  theme_light()
plot

