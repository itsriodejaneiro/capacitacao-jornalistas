## 0. Instalar e carregar o pacote ####
rm(list = ls())
install.packages("rtweet")
library(rtweet)
library(dplyr)

## 1. Acessar a API do Twitter #####

appname <- "<seu app>"
key <- "<sua key>"
secret <- "<sua secret>"
access_token <- "<seu access token>"
access_secret <- "<sua access secret>"

token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

token

## 2. Pesquisar por termo com search_tweets()  #####
# a função retorna até 18000 registros a cada 15 minutos

data <- search_tweets(q = "#VotoImpresso", # termo a ser buscado (podemos usar AND e/ou OR para buscar por mais termos)
                      n = 1000, # número de registros
                      type = 'recent', # buscar pelos mais recentes
                      include_rts = FALSE, # para incluir RTs, ajustar para TRUE
                      token = token)

## 3. Pesquisar em tempo real com stream_tweets() #####
# a função retorna os dados no mesmo formato de search_tweets()

# Exemplo, fazendo a busca dentro de uma janela de 30 segundos
stream_data <- stream_tweets("#CPIdaCovid", 
                            timeout = 30, # tempo para permancer coletanto
                            token = token)

## 4. Coletar dados sobre os usuários presentes na coleta pelo termo  #####
# removendo (com distinct()) linhas duplicadas

users_data <- users_data(data) %>% 
  distinct(user_id, .keep_all = TRUE)

## 5. Coletar seguindo/seguidores de determinado usuário #####
# as funções retornam os ids dos usuários

pegabots_followers <- get_followers('pegabots', token = token)
nrow(pegabots_followers) # Verificar quantos registros a função retornou
pegabots_following <- get_friends('pegabots', token = token)
nrow(pegabots_following)

## 6. Coletar dados de usuários #####

# Podemos consultar por @ ou id de um usuário específico
user <- lookup_users('pegabots', token = token)

# Ou podemos passar uma lista de usuários
# por exemplo, buscando aqui por dados de usuários seguidores do @pegabots
followers_data <- lookup_users(pegabots_followers$user_id, token = token)


## 7. Coletar dados de tweets de usuários específicos ##### 
# podemos coletar de mais de um passando um vetor de usuários

# Exemplo, tweets do pegabots
pegabots_tl <- get_timeline("pegabots", 
                   n = 100, # 3200 é número máximo permitido pelo Twitter
                   token = token)
pegabots_tl


## 8. Consultar trending topcis #####

# Listar opções de lugares para filtrar os trending topics
tt_available <- trends_available(token = token)
tt_available

# Busca trendigs em todo mundo
  tt_worldwide <- get_trends(woeid = 1, # id worldwide
                        token = token)

# Busca trendigs do Brasil
tt_brasil <- get_trends(woeid = 23424768, # id do Brasil
                        exclude_hashtags = FALSE, # para excluir as hashtags, ajustar para TRUE
                        token = token)
