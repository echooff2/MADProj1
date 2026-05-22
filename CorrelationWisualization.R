library(corrplot)

draw_graph <- function(M,filename){
  # Porządkujemy macierz (ważne: zrób to przed definiowaniem kolorów!)
  # corrplot domyślnie robi hclust wewnątrz, ale lepiej mieć posortowaną macierz M
  ord <- corrMatOrder(M, order = "hclust")
  M_ord <- M[ord, ord]
  
  # Definiujemy kolory tekstów TYLKO dla górnego trójkąta (w tym przekątna)
  # Musimy użyć tej samej logiki, której używa corrplot przy rysowaniu
  text_colors_vector <- ifelse(abs(M_ord) > 0.6, "white", "black")
  upper_text_colors <- text_colors_vector[upper.tri(M_ord, diag = TRUE)]
  
  # Rysujemy wykres
  my_colors <- colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(200)
  
  png(paste0('./Plots/',filename), width = 1500, height = 1500, res = 150, type = "cairo", bg="white")
  
  # Rysujemy GÓRĘ (kwadraty z liczbami)
  corrplot(M_ord, 
           type = "upper", 
           method = "color", 
           col = my_colors, 
           addCoef.col = upper_text_colors, 
           tl.pos = "lt",                   
           number.cex = 1 - dim(M)[1]/60, 
           number.digits = 2,
           tl.cex = 0.6, 
           tl.col = "black",
           diag = TRUE)                     # Diagonala z kolorem
  
  # Nakładamy DÓŁ (elipsy bez liczb)
  corrplot(M_ord, 
           type = "lower", 
           method = "ellipse", 
           col = my_colors,
           add = TRUE,                      # KLUCZ: nakłada na istniejący wykres
           diag = FALSE,                    # Nie rysuj drugi raz diagonali
           tl.pos = "n")                    # Nie rysuj drugi raz nazw
  dev.off()
}

read_correlated <- function(M){ 
  n <- dim(M)[1]
  coll_names <- c()
  for (i in 2:(n-1)){
    for (j in (i+1):n){
      if(abs(M[i, j]) >= 0.9){
        coll_names <- c(coll_names, rownames(M)[i])
        cat(rownames(M)[i], " ", colnames(M)[j], " ", M[i, j], "\n")
      }
    }
  }
  return(unique(coll_names))
}

draw_graphs <- function(df, name='WykresKorelacji', extension='.png'){
  count <- 1
  M <- cor(df, method = "pearson")
  draw_graph(M, paste0(name, toString(count), extension))
  
  count <- count + 1
  df <- df |>
    select(!all_of(sub("Kills$", "Deaths", read_correlated(M))))
  M <- cor(df, method = "pearson")
  draw_graph(M, paste0(name, toString(count), extension))
  return(df)
}