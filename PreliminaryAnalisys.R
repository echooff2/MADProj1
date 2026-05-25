# setwd("C:\\Users\\rysio\\Desktop\\Informatyka\\R\\MetodyAnalizyDanych\\Projekt2-Klasyfikacja")

draw_historgram <- function(df, feature, name){
  p <- ggplot(df.frame(feature), aes(x=feature)) +
    geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
    geom_density(color = "darkgrey", lwd = 1.5, fill="gray", alpha = 0.5) +
    ggtitle(name) +
    theme(plot.title = element_text(size=15, hjust=0.5))
  ggsave(paste0("plots\\histograms\\hist_", name, ".png"))
}

draw_historgrams <- function(df){
  for (col_nr in 1:ncol(df)) {
    feature <- df[, col_nr]
    name <- names(df)[col_nr]
    draw_historgram(df, feature, name)
  }
}

delete_quasi_const <- function(df){
  nzv_cols <- nearZeroVar(df)
  print(colnames(df)[nzv_cols])
  
  df <- df |> 
    select(!colnames(df)[nzv_cols])
  
  return(df)
}

delete_outliers <- function(df){
  # Funkcja zastępująca skrajne wartości na podstawie percentyla
  czysc_skrajne <- function(x, p) {
    gorny_prog <- quantile(x, probs = 1 - (p / 100), na.rm = TRUE)
    
    ifelse(x > gorny_prog, NA, x)
  }
  percentage = 1
  
  # Zastosowanie dla wszystkich kolumn z wardami
  df <- df %>%
    mutate(
      across(
        c(
          blueWardsPlaced, blueWardsDestroyed,
          redWardsPlaced, redWardsDestroyed
          ), ~ czysc_skrajne(.x, percentage)
      )
    )
  
  # Sprawdzenie wyniku
  print(colSums(is.na(df)))
  
  df <- na.omit(df)
  return(df)
}

do_preliminary_analisys <- function(to_scale = T, my_path = ""){
  if( my_path != "")
    setwd(my_path)
  
  df <- read.csv("lol.csv", header = T)
  
  library(dplyr)
  blueWins <- df |> select('blueWins')
  
  # Odrzucamy jako cechę 'gameId' jako nieistotną i blueWins jako zbyteczna do wstępnej analizy
  df <- df |>
    select(!c('blueWins', 'gameId'))
  
  library(psych)
  options(digits = 6)
  describe(df)[, c("mean", "median", "min", "max", "sd", "skew")]
  
  library(caret)
  delete_quasi_const(df)
  
  # Odrzucamy dragons i heralds jako że cecha eliteMonsters jest ich liniową kombinacją
  df <- df |>
    select(!c(ends_with('EliteMonsters')))
  
  
  if(!exists('draw_graphs')){
    source('CorrelationWisualization.R')
  }
  
  df <- draw_graphs(df)
  
  df <- df %>%
    mutate(
      blueKA = blueKills + blueAssists,
      redKA  = redKills + redAssists
    ) %>%
    select(-blueKills, -blueAssists, -redKills, -redAssists)
  
  draw_graph(cor(df), 'WykresZ_KA.png')
  
  df <- cbind(df, blueWins)
  df <- delete_outliers(df)
  
  blueWins <- df |> select('blueWins')
  df <- df |> select(!c('blueWins'))
  
  draw_historgrams(df)
  if(to_scale)
    scale(df)
  return(df)
}