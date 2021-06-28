rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)
paleta_its <- c("#A3248B","#54318C","#255C8E","#00A6B7","#E1F1FD", "#8a8a8a")

# 0. Pacotes -----------------------------------------------------------------
install.packages('igraph')
install.packages('ggraph')
install.packages('readr')
install.packages('tidyverse')

library('igraph')
library('ggraph')
library('readr')
library('tidyverse')
library('ggplot2')
library('scales')

# Leitura das bases ----------------------------------------------------

base <- read_csv("./capacitacao-jornalistas-main/Dados/all_#FechadoComBolsonaro2022_2021-06-14 00:00:00_1623963801.csv",
                 col_types = cols(.default = "c"))

base_pegabot <- read_csv("./capacitacao-jornalistas-main/Dados/Handles-.fechadocombolsonaro2022-1623963801-resultados.csv")

# Verificar quais perfis são 'bots' (+70%)
base_pegabot <- base_pegabot %>% 
  mutate(Resultado = cut(x = `Análise Total`, 
                         breaks = c(0,.70, Inf),
                         labels = c("Baixa Probabilidade",
                                    "Alta Probabilidade"))) %>% 
  select(`Perfil Twitter`, Resultado)

# Construção da rede de RTs --------------------------------------------

## Vamos passar o tweet por uma regex para extrair o autor do tweet original
# Testando:
str_match('RT @bolsomito_2: @taoquei1 Vem voto auditavel #FechadoComBolsonaro2022', 
          "(RT|via)((?:[[:blank:]:]\\W*@\\w+))")

# Agora sim, vamos construir uma tabela com os RTs
# indicando o usuário autor do tweet e o usuário que fez o RT
rts <- filter(base, str_detect(tweet, "(RT|via)((?:[[:blank:]:]\\W*@\\w+)+)")) %>% # procura por tweets que tenham RT ou via no texto
  select(tweet, username) %>% # seleciona a coluna tweet
  mutate(fonte = str_match(tweet, "(RT|via)((?:[[:blank:]:]\\W*@\\w+))")[,3]) %>% # cria uma coluna fonte para indicar a fonte do tweet
  mutate(fonte = gsub(" ", "", fonte, fixed = TRUE)) %>% # limpa o espaço em branco da coluna fonte
  mutate(username = paste0('@', username)) %>% # aqui é só um ajuste para adicionar '@' ao username, estava sem
  select(fonte, username)

head(rts) # mostra as primeiras linhas dessa tabela

# E a partir dessa tabela, construímos a rede:
rt_graph <- graph_from_data_frame(rts, directed = TRUE, vertices = NULL)
gsize(rt_graph) # verificar a qntd de nós
gorder(rt_graph) # verificar a qntd de arestas
summary(rt_graph)
## Legenda do igraph
# D ou U: rede direcionada ou não 
# N: named graph - quando os nós tem um atributo nome (no nosso caso sim)
# W: rede com pesos - quando as arestas têm um atributo peso (no nosso caso não)
# B: grede bipartite - quando os nós têm um atributo para especificar seu tipo 
# seguido da qntd de nós e de arestas

# Análises exploratórias --------------------------------------------------

# 1) Como é a distribuição de grau nessa rede? #####

# Vamos criar uma tabela com essa info
# a função degree_distribution() já faz esse cálculo
dist_grau <- data.frame(y = degree_distribution(rt_graph), x = 1:length(degree_distribution(rt_graph)))

# Plot do resultado
ggplot(dist_grau) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand=c(0,0), trans="sqrt") +
  labs(x = "Grau", 
       y = "Densidade (sqrt)", 
       title = "Distribuição de grau da rede de RTs") +
  theme_minimal()

# 2) Quais são os usuários mais centrais #####

# Centralidade de grau de entrada
grau_in <- degree(rt_graph, mode = 'in') # grau de entrada
grau_in <- data.frame(screen_name = names(grau_in), 
                      grau_in = grau_in, row.names = NULL)

top10_in <- grau_in %>%
  arrange(-grau_in) %>%
  top_n(10); top10_in

# Centralidade de grau de saída
grau_out <- degree(rt_graph, mode = 'out') # grau de SAÍDA
grau_out <- data.frame(screen_name = names(grau_out), 
                       grau_out = grau_out, row.names = NULL)

top10_out <- grau_out %>%
  arrange(-grau_out) %>%
  top_n(10); top10_out

# Centralidade de grau de intermediação
bet <- betweenness(rt_graph, directed = F, weights = NA, normalized = T)
bet <- data.frame(screen_name = names(bet), 
                  bet = bet, row.names = NULL)

top10_bet <- bet %>%
  arrange(-bet) %>%
  top_n(10); top10_bet

# 3) Quantos componentes tem a rede #####

componentes <- components(rt_graph) # ou clusters(rt_graph)
componentes[3]

rt_graph_dg <- decompose.graph(rt_graph)

rt_graph_1 <- igraph::simplify(rt_graph_dg[[1]])

# 4) Qual o volume de interações envolvendo ao menos um 'bot' #####

# Filtrar todos os nós que são bots
nos_bots <- base_pegabot %>%
  filter(Resultado == 'Alta Probabilidade')

# Filtrar interações de envolvam algum bot (na fonte ou username)
interacoes_bots <- rts %>%
  filter(fonte %in% nos_bots$`Perfil Twitter` |
           username %in% nos_bots$`Perfil Twitter`)
nrow(interacoes_bots) # número de interações
percent(nrow(interacoes_bots)/nrow(rts)) # porcentagem em relação ao total

# 5) Quantos 'bots' interagiram com pelo menos um dos usuários mais centrais #####

# primeiro temos que reunir uma lista desses usuários mais centrais
usuarios_centrais <- c(top10_in$screen_name,
                       top10_out$screen_name,
                       top10_bet$screen_name) %>%
  unique()

# como no item 4, filtramos as interacoes que envolvem bots
# mas tb filtramos a partir disso, as interacoes que envolvem os usuarios centrais
interacoes_centrais_bots <- rts %>%
  filter(fonte %in% nos_bots$`Perfil Twitter` |
           username %in% nos_bots$`Perfil Twitter`) %>%
  filter(fonte %in% usuarios_centrais |
           username %in% usuarios_centrais)

nrow(interacoes_centrais_bots) # número de interações
percent(nrow(interacoes_centrais_bots)/nrow(rts)) # porcentagem em relação ao total

# 6) Pelo menos um plot da rede... #####

# Como é uma rede grande para plotar por aqui, vamos fazer alguns ajutes:
# Ajustar o tamanho dos nós no grafo. Mínimo = 50 e máximo = ao valor do nó com maior grau
V(rt_graph_1)$node_size <- unname(ifelse(degree(rt_graph_1)[V(rt_graph_1)] > 50, degree(rt_graph_1), 50)) 
# Ajustar a cor. Nós com grau até 50 ficam em azul, maior que 50 ficam em rosa
V(rt_graph_1)$color <- unname(ifelse(degree(rt_graph_1)[V(rt_graph_1)] > 50, paleta_its[1], paleta_its[3])) 

# E plotar
ggraph(rt_graph_1, layout = 'stress') + 
  geom_edge_link0(edge_colour = paleta_its[6], edge_width = 0.1, edge_alpha = 0.5)+
  geom_node_point(aes(size = V(rt_graph_1)$node_size),
                  color = V(rt_graph_1)$color, alpha = .7) + 
  coord_fixed() +
  theme_graph() +
  theme(legend.position="none")
