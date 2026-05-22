setwd("C:\\Users\\rysio\\Desktop\\Informatyka\\R\\MetodyAnalizyDanych\\Projekt2-Klasyfikacja")
df <- read.csv("lol.csv", header = T)

library(dplyr)
blueWins <- df |> select('blueWins')

# Odrzucamy jako cechę 'gameId' jako nieistotną i blueWins jako zbyteczna do wstępnej analizy
# oraz odrzucamy dragons i heralds jako że cecha eliteMonsters jest ich liniową kombinacją
df <- df |>
  select(!c('blueWins', 'gameId', ends_with('EliteMonsters'), ends_with('Deaths')))

df <- df %>% mutate(across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x))))
glimpse(df)

if(!exists('draw_graphs')){
  source('CorrelationWisualization.R')
}

df <- draw_graphs(df)

# Odrzucamy cechy związane z exp i gold, jako że wynikają z pozostałych zmiennych
df <- df |>
  select(!c(ends_with('PerMin'), ends_with('diff'), ends_with('AvgLevel'), ends_with('TotalExperience')))

draw_graph(cor(df), 'WykresBezGoldExp.png')

df <- df %>%
  mutate(
    blueKA = blueKills + blueAssists,
    redKA  = redKills + redAssists
  ) %>%
  select(-blueKills, -blueAssists, -redKills, -redAssists)

draw_graph(cor(df), 'WykresZ_KA.png')

if(!exists('delete_outliners')){
  source('Outliners.R')
}

df <- delete_outliners(df)