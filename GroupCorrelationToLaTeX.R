library(dplyr)
library(tidyr)
library(kableExtra)

group_correlations_to_latex <- function(M, file_path = NULL) {
  
  # 1. Convert matrix to long format and remove duplicates
  cor_df <- as.data.frame(as.table(M)) %>%
    rename(Var1 = Var1, Var2 = Var2, CorValue = Freq) %>%
    mutate(AbsCor = abs(CorValue)) %>%
    filter(as.numeric(Var1) < as.numeric(Var2))
  
  # 2. Assign Polish labels
  cor_df <- cor_df %>%
    mutate(Category = case_when(
      AbsCor >= 0.0 & AbsCor < 0.2 ~ "Bardzo słaba",
      AbsCor >= 0.2 & AbsCor < 0.4 ~ "Słaba",
      AbsCor >= 0.4 & AbsCor < 0.7 ~ "Umiarkowana",
      AbsCor >= 0.7 & AbsCor < 0.9 ~ "Silna",
      AbsCor >= 0.9 & AbsCor <= 1.0 ~ "Bardzo silna"
    )) %>%
    mutate(Pair = paste0(Var1, " -- ", Var2, " (", round(CorValue, 2), ")"))
  
  # 3. Pivot to wide format
  table_data <- cor_df %>%
    group_by(Category) %>%
    mutate(row_id = row_number()) %>%
    ungroup() %>%
    select(Category, Pair, row_id) %>%
    pivot_wider(names_from = Category, values_from = Pair) %>%
    select(-row_id)
  
  # Ensure all categories are present as columns
  target_cols <- c("Bardzo słaba", "Słaba", "Umiarkowana", "Silna", "Bardzo silna")
  for(col in target_cols) {
    if(!(col %in% names(table_data))) table_data[[col]] <- ""
  }
  
  table_data <- table_data[, target_cols]
  # Replace NA with empty strings for a cleaner LaTeX look
  table_data[is.na(table_data)] <- ""
  
  # 4. Generate LaTeX Table
  latex_output <- kable(table_data, 
                        format = "latex", 
                        booktabs = TRUE, 
                        caption = "Podział cech według siły korelacji",
                        align = "l") %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  # 5. Saving to file
  if (!is.null(file_path)) {
    # Ensure the file has a .tex extension if not provided
    if (!grepl("\\.txt$", file_path)) {
      file_path <- paste0(file_path, ".txt")
    }
    
    writeLines(as.character(latex_output), file_path)
    message(paste("Tabela została zapisana do pliku:", file_path))
  }
  
  return(latex_output)
}