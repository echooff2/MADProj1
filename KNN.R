draw_k_accuracy_plot <- function(df, best_k) {
  ver_line_name <- paste("k =", best_k)
  
  p <- ggplot(df, aes(x = k, y = Accuracy)) +
    ggtitle("Zależność accuracy od parametru k") +
    geom_line(
      aes(color = "Accuracy", linetype = "Accuracy"),
      linewidth = 1.1
    ) +
    geom_vline(
      aes(
        xintercept = best_k,
        color = ver_line_name,   # Tutaj przekaże np. "k = 5"
        linetype = ver_line_name
      ),
      linewidth = 1.1
    ) +
    scale_color_manual(
      name = "Legenda",
      # Używamy setNames, aby kluczem była wartość zmiennej ver_line_name
      values = setNames(c("#2166AC", "#B2182B"), c("Accuracy", ver_line_name))
    ) +
    scale_linetype_manual(
      name = "Legenda",
      values = setNames(c("solid", "dashed"), c("Accuracy", ver_line_name))
    ) +
    # Usunąłem guides(linetype = "none"), jeśli chcesz, aby linetype też był w legendzie
    # Jeśli chcesz ukryć część legendy, upewnij się, że name w obu scale_ jest takie samo
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18)
    )
  
  print(p)
  ggsave("Plots/other/k_accuracy.png",
    plot = p,
    width = 8, height = 6, dpi = 80, create.dir = TRUE
  )
}


run_knn_once <- function(df, seed, use_synth_data = FALSE, split = NULL) {
  set.seed(seed)

  if (is.null(split)) {
    split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
  }
  train_data <- df[split, ]
  test_data <- df[-split, ]

  if (use_synth_data) {
    library(synthpop)
    synth_data <- syn(df, method = "cart", cart.minbucket = 10, seed = 67)
    test_data <- synth_data$syn
  }

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

  train_labels <- train_data$blueWins
  test_labels <- test_data$blueWins

  train_features <- subset(train_data, select = -blueWins)
  test_features <- subset(test_data, select = -blueWins)

  train_labels_factor <- factor(train_labels, levels = c(0, 1))

  knn_pred <- knn(
    train = train_features,
    test = test_features,
    cl = train_labels_factor,
    k = best_k,
    prob = TRUE
  )

  predicted_classes <- as.numeric(as.character(knn_pred))

  if (use_synth_data) {
    t <- test_data
    t$PredictedBlueWins <- predicted_classes
    dir.create("csv", recursive = TRUE, showWarnings = FALSE)
    write.csv(t, file = "csv/synth_data_knn.csv")
  }

  raw_prob <- attr(knn_pred, "prob")

  predicted_probabilities <- ifelse(
    knn_pred == "1",
    raw_prob,
    1 - raw_prob
  )

  conf_matrix <- table(
    Actual = test_labels,
    Predicted = predicted_classes
  )

  TP <- sum(test_labels == 1 & predicted_classes == 1)
  TN <- sum(test_labels == 0 & predicted_classes == 0)
  FP <- sum(test_labels == 0 & predicted_classes == 1)
  FN <- sum(test_labels == 1 & predicted_classes == 0)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  print('Single KNN done!')

  list(
    best_k = best_k,
    data_train = train_features,
    train_labels = train_labels_factor,
    knn_model = knn_cv,
    knn_cv_results = knn_cv$results,
    conf_matrix = conf_matrix,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    test_labels = test_labels,
    predicted_probabilities = predicted_probabilities,
    predicted_classes = predicted_classes
  )
}


average_knn_cv_results <- function(runs) {
  all_results <- do.call(rbind, lapply(runs, `[[`, "knn_cv_results"))
  averaged <- aggregate(
    cbind(Accuracy, Kappa) ~ k,
    data = all_results,
    FUN = mean
  )
  best_k <- averaged$k[which.max(averaged$Accuracy)]
  list(results = averaged, best_k = best_k)
}

predict_synth <- function(runs, synth_data){
  acc <- vapply(runs, function(x) x$accuracy, numeric(1))
  best_i <- which.max(acc)
  
  best_run   <- runs[[best_i]]
  best_model <- best_run$knn_model
  best_train <- best_run$data_train
  train_labels <- best_run$train_labels
  best_k <- best_run$best_k
  
  synth_data$blueWins <- as.factor(synth_data$blueWins)
  
  print(predict(best_model, newdata = synth_data))
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE 
  )
  
  print(dim(best_train))
  print(dim(synth_data))
  
  blueWins <- synth_data |> select(c('blueWins'))
  synth_data <- synth_data |> select(!c('blueWins'))
  
  knn_pred <- knn(
    train = best_train,
    test = synth_data,
    cl = train_labels,
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
  
  dir.create("csv", showWarnings = FALSE, recursive = TRUE)
  write.csv(predicted_classes, "csv/synth_data_knn.csv")
  
  output <- as.data.frame(predicted_probabilities)
  output$test_class_knn <- blueWins
  return(output)
}

do_knn <- function(draw_plots = F, seed = 23, split = NULL, use_synth_data = FALSE){
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }

  if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
  }

  if (!exists("draw_roc_plot")) {
    source("ROC.R")
  }

  if(use_synth_data){
    res <- do_preliminary_analisys(c(F, F, F), generate_syntetic_data = T)
    df  <- res$real
    synth_data <- res$synth
    synth_data$blueWins <- as.factor(synth_data$blueWins)
  }
  else{
    df <- do_preliminary_analisys(c(F, F, F))
  }

  library(caret)
  library(class)

  n_runs <- if (draw_plots) 5 else 1
  seeds <- 23 + seq_len(n_runs) - 1

  runs <- lapply(seeds, function(seed) {
    run_knn_once(df, seed, split = if (n_runs == 1) split else NULL)
  })
  
  if(use_synth_data){
    return(predict_synth(runs, synth_data))
  }

  result <- runs[[1]]

  if (draw_plots) {
    TP <- mean(vapply(runs, `[[`, numeric(1), "TP"))
    TN <- mean(vapply(runs, `[[`, numeric(1), "TN"))
    FP <- mean(vapply(runs, `[[`, numeric(1), "FP"))
    FN <- mean(vapply(runs, `[[`, numeric(1), "FN"))

    accuracy <- mean(vapply(runs, `[[`, numeric(1), "accuracy"))
    precision <- mean(vapply(runs, `[[`, numeric(1), "precision"))
    recall <- mean(vapply(runs, `[[`, numeric(1), "recall"))
    specificity <- mean(vapply(runs, `[[`, numeric(1), "specificity"))

    cv_avg <- average_knn_cv_results(runs)
    best_k <- cv_avg$best_k

    cat("Wyniki uśrednione z", n_runs, "uruchomień KNN\n")
    cat("Najlepsze k (średnia CV) =", best_k, "\n")
    cat("TP =", round(TP, 2), "\n")
    cat("TN =", round(TN, 2), "\n")
    cat("FP =", round(FP, 2), "\n")
    cat("FN =", round(FN, 2), "\n")
    cat("Accuracy =", round(accuracy, 4), "\n")
    cat("Precision =", round(precision, 4), "\n")
    cat("Recall =", round(recall, 4), "\n")
    cat("Specificity =", round(specificity, 4), "\n")
  } else {
    TP <- result$TP
    TN <- result$TN
    FP <- result$FP
    FN <- result$FN
    accuracy <- result$accuracy
    precision <- result$precision
    recall <- result$recall
    specificity <- result$specificity
    best_k <- result$best_k

    print(result$conf_matrix)
    cat("Najlepsze k =", best_k, "\n")
    cat("TP =", TP, "\n")
    cat("TN =", TN, "\n")
    cat("FP =", FP, "\n")
    cat("FN =", FN, "\n")
    cat("Accuracy =", round(accuracy, 4), "\n")
    cat("Precision =", round(precision, 4), "\n")
    cat("Recall =", round(recall, 4), "\n")
    cat("Specificity =", round(specificity, 4), "\n")
  }

  if (draw_plots) {
    draw_confusion_matrix(
      round(TP), round(FN), round(TN), round(FP),
      "KNN",
      decimal_digits = 4
    )

    roc_runs <- lapply(runs, function(run) {
      list(
        real_classes = run$test_labels,
        probabilities = run$predicted_probabilities
      )
    })
    draw_averaged_roc_plot(roc_runs, "roc_knn")
    draw_k_accuracy_plot(cv_avg$results, best_k)
  }

  output <- as.data.frame(result$predicted_probabilities)
  output$test_class_knn <- result$test_labels

  return(output)
}

if (sys.nframe() == 0L) {
  do_knn(draw_plots = TRUE, use_synth_data = FALSE)
}
