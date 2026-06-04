# ------------- Zdefiniuj parametry -----------------
# TP - True Positive,   FN - False Negative,
# TN - True Negative,   FP - False Positive,
# name - nazwa bez rozszerzenia
draw_confusion_matrix <- function(TP, FN, TN, FP, name, decimal_digits = 2){
  library(ggplot2)
  library(dplyr)
  library(ggtext)
  
  # ============================================================
  # PARAMETRY ROZMIARÓW - EDYTUJ TUTAJ
  # ============================================================
  
  # Szerokości kolumn
  col_width_top_header <- 0.25   # Kolumna 1 (lewa scalona)
  col_width_normal <- 1     # Kolumny 2-5 (zwykłe)
  
  # Wysokości wierszy
  row_height_top_header <- 0.25  # Wiersz 1 (górny scalony)
  row_height_normal <- 0.5    # Wiersze 2-5 (zwykłe)
  
  # ============================================================
  # PARAMETRY PROCENTOWE
  # ============================================================
  precision <- round(TP/(TP + FP), decimal_digits)
  negative_pred_val <- round(TN/(TN + FN), decimal_digits)
  sensitivity <- round(TP/(TP + FN), decimal_digits)
  specificity <- round(TN/(TN + FP), decimal_digits)
  accuracy <- round((TP + TN)/(TP + TN + FP + FN), decimal_digits)
  
  # ============================================================
  # OBLICZANIE POZYCJI NA PODSTAWIE ROZMIARÓW (POPRAWIONE)
  # ============================================================
  
  # Szerokości i wysokości dla każdej kolumny/wiersza
  col_widths <- c(col_width_top_header, col_width_normal, col_width_normal, col_width_normal, col_width_normal)
  row_heights <- c(row_height_top_header, row_height_normal, row_height_normal, row_height_normal, row_height_normal)
  
  # Krawędzie kolumn (kumulatywne)
  x_edges <- cumsum(c(0, col_widths))
  total_width <- sum(col_widths)
  
  # Środki kolumn
  x_positions <- x_edges[1:5] + col_widths / 2
  
  # Krawędzie wierszy od góry (kumulatywne)
  y_edges_from_top <- cumsum(c(0, row_heights))
  total_height <- sum(row_heights)
  
  # Środki wierszy (odwrócone, żeby wiersz 1 był na górze)
  y_positions <- total_height - (y_edges_from_top[1:5] + row_heights / 2)
  
  # ============================================================
  # SIATKA KOMÓREK
  # ============================================================
  
  cells <- expand.grid(col = 1:5, row = 1:5) |>
    mutate(
      x = x_positions[col],
      y = y_positions[row],
      w = col_widths[col],
      h = row_heights[row]
    )
  
  # Komórki do usunięcia
  missing_cells <- data.frame(
    row = c(1, 1, 1, 1, 1, 2, 3, 4, 5, 2, 2, 5),
    col = c(1, 2, 3, 4, 5, 1, 1, 1, 1, 2, 5, 2)
  )
  
  # Tekst i style
  cells <- cells |>
    mutate(
      label = case_when(
        row == 2 & col == 3 ~ "**Blue Wins**",
        row == 2 & col == 4 ~ "**Red Wins**",
        row == 3 & col == 2 ~ "**Blue Wins**",
        row == 4 & col == 2 ~ "**Red Wins**",
        
        row == 3 & col == 3 ~ paste0("", TP),
        row == 3 & col == 4 ~ paste0("", FP),
        row == 4 & col == 3 ~ paste0("", FN),
        row == 4 & col == 4 ~ paste0("", TN),
        
        row == 5 & col == 3 ~ paste0("**Precisionl** <br>", precision * 100, "%"),
        row == 5 & col == 4 ~ paste0("**Negative Predictive<br>Value**<br>", negative_pred_val * 100, "%"),
        row == 3 & col == 5 ~ paste0("**Sensitivity** <br>", sensitivity * 100, "%"),
        row == 4 & col == 5 ~ paste0("**Specificity** <br>", specificity * 100, "%"),
        row == 5 & col == 5 ~ paste0("**Accuracy** <br>", accuracy * 100, "%"),
        
        TRUE ~ ""
      ),
      
      # ------------------ TŁO KOMÓREK ------------------
      fill = case_when(
        # 1. Nagłówki klas (kolory marki Blue/Red)
        # Nagłówek "Blue Wins" w kolumnie predykcji (wiersz 2, kol 3)
        # oraz w wierszu actual (wiersz 3, kol 2) - dostosuj do swojej struktury!
        (row == 2 & col == 3) | (row == 3 & col == 2) ~ "#dbeafe", # Blue Wins nagłówki
        (row == 2 & col == 4) | (row == 4 & col == 2) ~ "#fee2e2", # Red Wins nagłówki
        
        # 2. Główne wyróżnienie: Accuracy
        row == 5 & col == 5 ~ "#fef3c7", # Złoty
        
        # 3. Trafienia - poprawne predykcje (główna przekątna)
        (row == 3 & col == 3) | (row == 4 & col == 4) ~ "#dcfce7", # Jasny zielony
        
        # 4. Błędy - niepoprawne predykcje (poza przekątną)
        (row == 3 & col == 4) | (row == 4 & col == 3) ~ "#ffedd5", # Jasny pomarańczowy
        
        # 5. Pozostałe metryki (Sensitivity, Specificity, Precision, NPV)
        row == 5 | col == 5 ~ "#f1f5f9", # Jasny slate
        
        # Reszta (puste komórki, nagłówki "Predicted"/"Actual")
        TRUE ~ "white"
      ),
      
      # ------------------ KOLOR TEKSTU ------------------
      text_color = case_when(
        # 1. Nagłówki klas
        (row == 2 & col == 3) | (row == 3 & col == 2) ~ "#1d4ed8", # Blue Wins tekst
        (row == 2 & col == 4) | (row == 4 & col == 2) ~ "#b91c1c", # Red Wins tekst
        
        # 2. Accuracy
        row == 5 & col == 5 ~ "#a16207", # Ciemny złoty
        
        # 3. Trafienia
        (row == 3 & col == 3) | (row == 4 & col == 4) ~ "#15803d", # Ciemny zielony
        
        # 4. Błędy
        (row == 3 & col == 4) | (row == 4 & col == 3) ~ "#c2410c", # Ciemny pomarańczowy
        
        # 5. Pozostałe metryki
        row == 5 | col == 5 ~ "#475569", # Ciemny slate
        
        # Reszta
        TRUE ~ "black"
      ),
      
      fontface = case_when(
        row %in% 3:4 & col %in% 3:4 ~ "bold",
        TRUE ~ "plain"
      ),
      
      font_size = case_when(
        row %in% 3:4 & col %in% 3:4 ~ 5,
        TRUE ~ 4
      )
    )
  
  cells_to_draw <- anti_join(cells, missing_cells, by = c("row", "col"))
  
  # ============================================================
  # SCALONE KOMÓRKI 
  # ============================================================
  
  merged_cells <- data.frame(
    # Top Header: kolumny 3-4, wiersz 1
    x = c(
      (x_positions[3] + x_positions[4]) / 2,    # Środek między kol 3 i 4
      x_positions[1]                            # Kolumna 1
    ),
    y = c(
      y_positions[1],                          # Wiersz 1
      (y_positions[3] + y_positions[4]) / 2    # Środek między wierszem 3 i 4
    ),
    w = c(
      col_width_normal * 2,                     # Szerokość = 2 kolumny
      col_width_top_header                      # Szerokość = 1 kolumna header
    ),
    h = c(
      row_height_top_header,                    # Wysokość = 1 wiersz header
      row_height_normal * 2                     # Wysokość = 2 wiersze
    ),
    label      = c("Predicted", "Actual"),
    fill       = c("white", "white"),
    text_color = c("black", "black"),
    fontface   = c("plain", "plain"),
    angle      = c(0, 90)
  )
  
  # ============================================================
  # RYSOWANIE
  # ============================================================
  
  p <- ggplot() +
    # Zwykłe komórki
    
    geom_tile(
      data = cells_to_draw,
      aes(x = x, y = y, width = w, height = h, fill = fill),
      color = "black", linewidth = 0.7
    ) +
    # Scalone komórki
    geom_tile(
      data = merged_cells,
      aes(x = x, y = y, width = w, height = h, fill = fill),
      color = "black", linewidth = 0.7
    ) +
    scale_fill_identity() +
    
    # Tekst dla zwykłych komórek
    geom_richtext(
      data = cells_to_draw,
      aes(x = x, y = y, label = label, color = text_color, size = font_size),
      fill = NA,            # Bez tła
      label.color = NA,     # Bez ramki wokół tekstu
      size = 5
    ) +
    scale_size_identity() +
    # Tekst dla scalonych komórek
    geom_text(
      data = merged_cells,
      aes(x = x, y = y, label = label, color = text_color, fontface = fontface, angle = angle),
      #size = 4, lineheight = 1.1
      size = 6, lineheight = 1.1
    ) +
    scale_color_identity() +
    
    coord_fixed() +
    xlim(0, total_width) +
    ylim(0, total_height) +
    theme_void()
  
  # Zapis
  ggsave(paste0("./Plots/confusion_matrixes/",name,".png"), p, width = 10, height = 10, dpi = 300, bg = "transparent", create.dir = TRUE)
}

if (sys.nframe() == 0L) {
  draw_confusion_matrix(3891, 123, 253, 4000, "custom_table")
}
