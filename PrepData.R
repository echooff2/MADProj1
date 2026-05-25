library(dplyr)
prep_data <- function(df) {
  blueWins <- df |> select("blueWins")

  # Odrzucamy jako cechę "gameId" jako nieistotną i blueWins jako zbyteczna do wstępnej analizy
  # oraz odrzucamy dragons i heralds jako że cecha eliteMonsters jest ich liniową kombinacją
  df <- df |>
    select(!c("blueWins", "gameId", ends_with("EliteMonsters")))

  if (!exists("read_correlated")) {
    source("CorrelationWisualization.R")
  }
  count <- 1
  m <- cor(df, method = "pearson")

  count <- count + 1
  df <- df |>
    select(!all_of(sub("Kills$", "Deaths", read_correlated(m))))

  # Odrzucamy cechy związane z exp i gold, jako że wynikają z pozostałych zmiennych
  df <- df |>
    select(!c(ends_with("PerMin"), ends_with("diff"), ends_with("AvgLevel"), ends_with("TotalExperience")))

  df <- df |>
    mutate(
           blueKA = blueKills + blueAssists,
           redKA  = redKills + redAssists
    ) |>
    select(-blueKills, -blueAssists, -redKills, -redAssists)

  df <- df |> mutate(across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x))))
  glimpse(df)


  if (!exists("delete_outliners")) {
    source("Outliners.R")
  }

  df$blueWins <- blueWins$blueWins

  df <- delete_outliners(df)
  df
}
