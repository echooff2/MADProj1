draw_historgram <- function(df, feature, name){
  p <- ggplot(data.frame(feature), aes(x=feature)) +
    geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
    geom_density(color = "darkgrey", lwd = 1.5, fill="gray", alpha = 0.5) +
    ggtitle(name) +
    theme(plot.title = element_text(size=15, hjust=0.5))
  ggsave(paste0("Plots/histograms/hist_", name, ".png"))
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

# OMÓWIENIE ZMIENNYCH:
# a) to_draw_graphs przyjmuje wektor zmiennych True/False, gdzie:
#   1) oznacza rysowanie histogramów i boxplotów
#   2) oznacza rysowanie macierzy korelacji
#   3) oznacza ryzowanie wykresów do analizy PCA
# b) to_scale, zmienna True/False, oznaczająca, czy dopuszczamy skalowanie;
#     brak skalowania oznacza brak pca
# c) my_path pozwala przekazać ścieżkę do folderu, w którym pracujemy
do_preliminary_analisys <- function(to_draw_graphs = c(F, F, F), to_scale = T, my_path = ""){
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
  df <- delete_quasi_const(df)
  
  if(!exists('draw_corr_matrix')){
    source('CorrelationWisualization.R')
  }
  
  df <- delete_correlated_draw_corr_matrixes(df, to_draw_graphs[2])
  
  # Odrzucamy: 
  # eliteMonsters jako że ta cecha jest liniową kombinacją cech dragons i heralds
  # goldPerMin jako że goldDiff jest różnicą wartości goldPerMin pomiędzy 2 drużynami
  # totalExperience analogicznie do goldPerMin
  df <- df |>
    select(!c(ends_with('EliteMonsters'), ends_with('GoldPerMin'), ends_with('TotalExperience')))
  
  df <- df %>%
    mutate(
      blueKA = blueKills + blueAssists,
      redKA  = redKills + redAssists
    ) %>%
    select(-blueKills, -blueAssists, -redKills, -redAssists)
  
  if(to_draw_graphs[2])
    draw_corr_matrix(cor(df), 'WykresZ_KA.png')
  
  # Za pomocą PCA łączymy goldDiff i experienceDiff w jedną zmienną
  if(to_scale){
    if(!exists('reduce_dim_pca')){
      source('PCA.R')
    }
    df <- reduce_dim_pca(df, c(ends_with('diff')), 1, "diff", to_draw_graphs[3])
    if(to_draw_graphs[2])
      draw_corr_matrix(cor(df), 'WykresBezDiff.png')
  }
  
  # Usuwamy wszystkie rekordy, które zawierają 
  df <- cbind(df, blueWins)
  df <- delete_outliers(df)
  
  if(to_draw_graphs[2])
    draw_corr_matrix(cor(df), 'WykresZblueWins.png')
  
  print(colnames(df))
  
  if(to_draw_graphs[1])
    draw_historgrams(df)
  if(to_scale)
    scale(df)
  
  return(df)
}

# histograms, corr_matrixes
do_preliminary_analisys(c(F, T, T))
