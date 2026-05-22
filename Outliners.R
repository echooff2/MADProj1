delete_outliners <- function(df, percentage = 1){
  # Funkcja zastępująca skrajne wartości na podstawie percentyla
  czysc_skrajne <- function(x, p) {
    gorny_prog <- quantile(x, probs = 1 - (p / 100), na.rm = TRUE)
    
    ifelse(x > gorny_prog, NA, x)
  }
  
  # Zastosowanie dla wszystkich kolumn numerycznych
  df <- df %>%
    mutate(across(where(is.numeric), ~ czysc_skrajne(.x, percentage)))
  
  # Sprawdzenie wyniku
  print(colSums(is.na(df)))
  
  df <- na.omit(df)
  return(df)
}