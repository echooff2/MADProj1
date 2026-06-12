draw_tree_plot <- function(tree_res, name, file_name) {
  png(paste("Plots/tree_plots/", file_name, ".png", sep = ""), width = 800, height = 600, res = 80)

  par(
    bg = "white",
    mar = c(3, 3, 5, 3),
    cex.main = 1.6,
    font.main = 2
  )
  plot(tree_res, type = "uniform", col = "transparent")
  u <- par("usr")
  rect(u[1], u[3], u[2], u[4], col = "#EBEBEB", border = NA)
  grid(nx = NULL, ny = NULL, col = "white", lty = 1, lwd = 1.5)
  par(new = TRUE)
  plot(tree_res, type = "uniform", col = "#2166AC", lwd = 4)
  text(
    tree_res,
    cex = 0.95,
    col = "black",
    font = 2,
    pretty = 0,
    bg = "white"
  )
  par(mar = c(4, 4, 6, 4))
  title(main = name, cex.main = 1.8)

  dev.off()
}

draw_tree_cv_plot <- function(tree_res.cv, tree_res) {
  df <- data.frame(
    size = tree_res.cv$size,
    deviance = tree_res.cv$dev
  )
  print(df)

  p <- ggplot(df, aes(x = size, y = deviance)) +
    geom_line(color = "#2166AC", linewidth = 1) +
    geom_point(color = "#2166AC", size = 3) +
    ggtitle("Błąd CV dla różnych rozmiarów drzewa") +
    labs(
      x = "Tree size",
      y = "Deviance"
    ) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18)
    )

  print(tree_res)

  ggsave(
    "Plots/tree_plots/tree_cv.png",
    plot = p,
    width = 8.5,
    height = 6,
    dpi = 80,
    create.dir = TRUE
  )
}

do_classification_tree <- function(draw_plots = F, seed = 23, split = NULL, use_synth_data = F) {
  set.seed(seed)

  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }

  if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
  }

  if (!exists("draw_roc_plot")) {
    source("ROC.R")
  }

  ########## uruchomic przed całym skryptem ##############
  synth_data <- NULL
  if (use_synth_data) {
    dfnt <- do_preliminary_analisys(generate_syntetic_data = use_synth_data)
    df <- dfnt$real
    synth_data <- dfnt$synth
  }
  df$blueWins <- as.factor(df$blueWins)

  library(tree)
  tree(df)

  if (is.null(split)) {
    split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
  }
  train_data <- df[split, ]
  test <- df[-split, ]
  ########## uruchomic przed całym skryptem ##############
  # if (use_synth_data) {
  #   # library(synthpop)
  #   # synth_data <- syn(df, method = "cart", cart.minbucket = 10, seed = 67)
  #   test <- synth_data$syn
  # }

  summary(train_data)
  tree_res <- tree(blueWins ~ ., data = train_data, model = TRUE)

  tree_res2 <- tree(blueWins ~ ., data = train_data, model = TRUE)
  tree_res.cv <- cv.tree(tree_res2, FUN = prune.misclass, K = 10)

  for (i in 2:5) {
    tree_res.cv$dev <- tree_res.cv$dev + cv.tree(tree_res2, FUN = prune.misclass, K = 10)$dev
  }
  print(tree_res.cv)
  tree_res.cv$dev <- tree_res.cv$dev / 5
  if (draw_plots) {
    draw_tree_cv_plot(tree_res.cv, tree_res)
    draw_tree_plot(tree_res, "Drzewo klasyfikacyjne", "classification_tree")
  }

  summary(tree_res)

  min_dev_indices <- which(tree_res.cv$dev == min(tree_res.cv$dev))
  best_size <- min(tree_res.cv$size[min_dev_indices])
  small_tree <- prune.tree(tree_res2, best = best_size)

  if (use_synth_data) {
    synth_data$blueWins <- as.factor(synth_data$blueWins)

    predict_probs <- predict(tree_res, newdata = synth_data)[, 2]
    predict_test <- predict(tree_res, newdata = synth_data, type = "class")

    dir.create("csv", showWarnings = FALSE, recursive = TRUE)
    write.csv(predict_test, "csv/synth_data_tree.csv")

    output <- as.data.frame(predict_probs)
    output$test_class_tree <- synth_data$blueWins
    return(output)
  }

  predict_probs <- predict(tree_res, newdata = test)[, 2]
  predict_test <- predict(tree_res, newdata = test, type = "class")
  confusion_matrix <- table(Predicted = predict_test, Actual = test$blueWins)
  confusion_matrix
  summary(predict_test)

  library(caret)
  confusionMatrix(predict_test, test$blueWins)

  cat(paste0("Wybrany rozmiar drzewa (min. CV deviance): ", best_size, " liści\n"))
  if (draw_plots) {
    draw_tree_plot(small_tree, "Przycięte drzewo klasyfikacyjne", "trimmed_classification_tree")
  }

  # 2. Robimy prognozy dla obu modeli na zbiorze testowym
  pred_small <- predict(small_tree, newdata = test, type = "class")
  pred_large <- predict(tree_res2, newdata = test, type = "class")

  if (use_synth_data) {
    t <- synth_data$syn
    t$PredictedBlueWins <- pred_small
    dir.create("csv", recursive = TRUE, showWarnings = FALSE)
    write.csv(t, file = "csv/synth_data_tree.csv")
  }

  # 3. Liczymy Accuracy dla obu
  acc_small <- mean(pred_small == test$blueWins)
  acc_large <- mean(pred_large == test$blueWins)
  acc_normal <- mean(predict_test == test$blueWins)

  # 4. Wyświetlamy wynik starcia
  cat(paste0(
    "Accuracy przyciętego tree_res2 drzewa: ", round(acc_small * 100, 2), "%\n",
    "Accuracy tree_res2 drzewa: ", round(acc_large * 100, 2), "%\n",
    "Accuracy tree_res drzewa: ", round(acc_normal * 100, 2), "%\n"
  ))

  tn <- confusion_matrix["0", "0"]
  tp <- confusion_matrix["1", "1"]
  fp <- confusion_matrix["0", "1"]
  fn <- confusion_matrix["1", "0"]

  if (use_synth_data) {
    conf_mat_name <- "classification_tree_confusion_matrix_synth"
    roc_plot_name <- "Drzewo klas. synth"
  } else {
    conf_mat_name <- "classification_tree_confusion_matrix"
    roc_plot_name <- "Drzewo klasyfikacyjne"
  }


  if (draw_plots) {
    draw_confusion_matrix(tp, fn, tn, fp, conf_mat_name, decimal_digits = 4)
    draw_roc_plot(test$blueWins, predict_probs, roc_plot_name)
  }

  output <- as.data.frame(predict_probs)
  output$test_class_tree <- test$blueWins

  return(output) # returns data for mixed model
}

if (sys.nframe() == 0L) {
  use_synth_data = F
  dfnt <- do_preliminary_analisys(generate_syntetic_data = use_synth_data)
  df <- dfnt
  df$blueWins <- as.factor(df$blueWins)
  t <- do_classification_tree(draw_plots = TRUE, use_synth_data = TRUE)
}
