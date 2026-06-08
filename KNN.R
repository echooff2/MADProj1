draw_k_accuracy_plot <- function(df, best_k) {
  ver_line_name = paste("k:", best_k)
  p <- ggplot(df, aes(x = k, y = Accuracy)) +
      ggtitle("Zależność accuracy od parametru k") +
      geom_line(
        aes(color = "Accuracy", linetype = "Accuracy"),
        linewidth = 1.1
      ) +
      geom_vline(
        aes(
          xintercept = best_k,
          color = "k = 79",
          linetype = "k = 79"
        ),
        linewidth = 1.1
      ) +
      scale_color_manual(
        name = "Legenda",
        values = c("Accuracy" = "#2166AC", "k = 79" = "#B2182B")
      ) +
      scale_linetype_manual(
        values = c("Accuracy" = "solid", "k = 79" = "dashed")
      ) +
      guides(linetype = "none") +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)
      )

  print(p)
  ggsave("Plots/other/k_accuracy.png", plot = p,
         width = 8, height = 6, dpi = 80, create.dir = TRUE)
}


do_knn <- function(draw_plots = F) {
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }

  if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
  }

  if (!exists("draw_roc_plot")) {
    source("ROC.R")
  }

  df <- do_preliminary_analisys(c(F, F, F))


  library(caret)
  library(class)

  set.seed(23)

  # =========================
  # Podział danych 80/20
  # =========================

  #train_index <- createDataPartition(
  #    df$blueWins,
  #    p = 0.7,
  #    list = FALSE
  #)

  split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
  train_data <- df[split, ]
  test_data <- df[-split, ]

  use_synth_data = F
  if (use_synth_data){
    library(synthpop)
    synth_data <- syn(df, method = "cart", cart.minbucket = 10, seed = 67)
    test_data <- synth_data$syn
  }


  #train_data <- df[train_index, ]
  #test_data  <- df[-train_index, ]

  # =========================
  # Dobór najlepszego k
  # =========================

  train_data_cv <- train_data
  train_data_cv$blueWins <- factor(train_data_cv$blueWins)

  ctrl <- trainControl(
    method = "cv",
    number = 10
  )

  knn_cv <- caret::train(
      blueWins ~ .,
      data = train_data_cv,
      method = "knn",
      tuneGrid = data.frame(k = seq(1, 101, by = 2)),
      trControl = ctrl
  )

  best_k <- knn_cv$bestTune$k

  cat("Najlepsze k =", best_k, "\n")

  # =========================
  # Przygotowanie danych
  # =========================

  train_labels <- train_data$blueWins
  test_labels  <- test_data$blueWins

  train_features <- subset(train_data, select = -blueWins)
  test_features  <- subset(test_data, select = -blueWins)

  train_labels_factor <- factor(train_labels, levels = c(0, 1))

  # =========================
  # Klasyfikacja końcowa
  # =========================

  knn_pred <- knn(
      train = train_features,
      test = test_features,
      cl = train_labels_factor,
      k = best_k,
      prob = TRUE
  )

  # Przewidziane klasy
  predicted_classes <- as.numeric(as.character(knn_pred))

  # Prawdopodobieństwa klasy 1
  raw_prob <- attr(knn_pred, "prob")

  predicted_probabilities <- ifelse(
      knn_pred == "1",
      raw_prob,
      1 - raw_prob
  )

  # =========================
  # Macierz pomyłek
  # =========================

  conf_matrix <- table(
      Actual = test_labels,
      Predicted = predicted_classes
  )

  print(conf_matrix)

  # =========================
  # TP, TN, FP, FN
  # =========================

  TP <- sum(test_labels == 1 & predicted_classes == 1)
  TN <- sum(test_labels == 0 & predicted_classes == 0)
  FP <- sum(test_labels == 0 & predicted_classes == 1)
  FN <- sum(test_labels == 1 & predicted_classes == 0)

  cat("TP =", TP, "\n")
  cat("TN =", TN, "\n")
  cat("FP =", FP, "\n")
  cat("FN =", FN, "\n")

  # =========================
  # Metryki
  # =========================

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)

  cat("Accuracy =", round(accuracy, 4), "\n")
  cat("Precision =", round(precision, 4), "\n")
  cat("Recall =", round(recall, 4), "\n")
  cat("Specificity =", round(specificity, 4), "\n")

  if (use_synth_data) {
      conf_mat_name = "KNN_synth"
      roc_plot_name = "KNN synth"
  }
  else {
      conf_mat_name = "KNN"
      roc_plot_name = "KNN"
  }

  if (draw_plots) {
      draw_confusion_matrix(TP, FN, TN, FP, conf_mat_name, decimal_digits = 4)
      draw_roc_plot(test_labels, predicted_probabilities, roc_plot_name)
      draw_k_accuracy_plot(knn_cv$results, best_k)
  }

  #test na lable z danymi
  output <- as.data.frame(predicted_probabilities)
  output$test_class_knn<-test_labels

  return(output) # returns data for mixed model
}

if (sys.nframe() == 0L) {
  do_knn(draw_plots = T)
}
