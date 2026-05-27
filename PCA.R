draw_pca_graph <- function(pca_result, name){
  # add file extension
  name <- paste0(name, '.png')
  png(paste0('./Plots/PCA/biplot_',name), width = 1500, height = 1500, res = 150, type = "cairo", bg="white")
  biplot(pca_result)
  dev.off()
  
  # Scree plot
  png(paste0('./Plots/PCA/screeplot_',name), width = 1500, height = 1500, res = 150, type = "cairo", bg="white")
  screeplot(pca_result, type = "lines", main = "Scree Plot")
  dev.off()
  
  # Calculate explained variance
  std_dev <- pca_result$sdev
  pr_var <- std_dev^2
  prop_variance <- pr_var / sum(pr_var)
  
  # Plot cumulative variance to find the threshold
  png(paste0('./Plots/PCA/CumulativeVar_',name), width = 1500, height = 1500, res = 150, type = "cairo", bg="white")
  plot(cumsum(prop_variance), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       ylim = c(min(prop_variance[1], 0.9), 1), # force seing min line of 0.9
       type = "b")
  abline(h = 0.90, col = "red", lty = 2) # Reference line at 90%
  dev.off()
}

# Omówienie zmiennych:
# a) df reprezentuje data frame
# b) cols_to_del reprezentuje nazwy zmiennych, które chcemy połaczyć przez PCA
# c) n ilość wektorów, które chcemy otrzymać po zastosowaniu PCA
# d) name to przedrostek nazwy kolumn powstałych po PCA
# e) to_draw_graphs zmienna True/False; mówi, czy rysować wykresy
reduce_dim_pca <- function(df, cols_to_del, n, name = "", to_draw_graphs = T){
  if(n > dim(df)[1])
    error("Ilość kolumn po PCA (parametr n) nie może być większa od ilości
          kolumn przekazanych do PCA")
  
  pca_result <- prcomp(df |> select(cols_to_del), center = TRUE, scale. = TRUE)
  
  # View summary of importance
  print(summary(pca_result))
  
  if(to_draw_graphs)
    draw_pca_graph(pca_result, name)
  
  # 1. Assign names to the PCA matrix columns first
  if(name != "")
    if(!endsWith(name, "_"))
      name <- paste0(name, "_")
  colnames(pca_result$x)[1:n] <- paste0(name,"PCA_Component_", 1:n)
  
  df <- cbind(
    df |> select(!all_of(cols_to_del)),
    pca_result$x[, 1:n, drop = FALSE]
  )
  
  return(df)
}