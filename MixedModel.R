run_mixed_avg_once <- function(seed, gen_synth_data = FALSE) {
  if (!exists("do_tensor_flow_neuralNet")) {   #seed=23 #gen_synth_data = TRUE
    source("tensorflow.R")
  }
  if (!exists("do_classification_tree")) {
    source("ClassificationTree.R")
  }
  if (!exists("do_knn")) {
    source("KNN.R")
  }

  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  set.seed(seed)
  df <- do_preliminary_analisys()
  split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))

  Tensor_prob <- do_tensor_flow_neuralNet(seed = seed, split = split, use_synth_data = gen_synth_data  )
  Classification_tree_prob <- do_classification_tree(seed = seed, split = split, use_synth_data = gen_synth_data)
  Tensor_prob <- as.data.frame(Tensor_prob)
  Classification_tree_prob <- as.data.frame(Classification_tree_prob)
  KNN_prob <- do_knn(seed = seed, split = split, use_synth_data = gen_synth_data)
  KNN_prob <- as.data.frame(KNN_prob)

  library(dplyr)
  Output <- cbind(Tensor_prob, KNN_prob)
  Output <- cbind(Output, Classification_tree_prob)
  Output_class <- as.numeric(Output$test_class_nn$blueWins)  
  Output <- Output |> select(!c("test_class_nn", "test_class_tree", "test_class_knn"))
  names(Output) <- c("probability_vector", "predicted_probabilities", "predict_probs")

  Output$Sum <- Output$probability_vector + Output$predicted_probabilities + Output$predict_probs
  Output$Sum <- Output$Sum / 3

    pred_class <- ifelse(Output$Sum >= 0.5, 1, 0)
    actual_class <- Output_class
    probabilities <- Output$Sum

  cm <- table(
    Predicted = factor(pred_class, levels = c(0, 1)),
    Actual = factor(actual_class, levels = c(0, 1))
  )

  TP <- ifelse(is.na(cm["1", "1"]), 0, as.numeric(cm["1", "1"]))
  TN <- ifelse(is.na(cm["0", "0"]), 0, as.numeric(cm["0", "0"]))
  FP <- ifelse(is.na(cm["1", "0"]), 0, as.numeric(cm["1", "0"]))
  FN <- ifelse(is.na(cm["0", "1"]), 0, as.numeric(cm["0", "1"]))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  list(
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    accuracy = accuracy,
    test_class = actual_class,
    probabilities = probabilities
  )
}


do_mixed_model_avg <- function(draw_plots = TRUE) {
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
  }
  if (!exists("draw_averaged_roc_plot")) {
    source("ROC.R")
  }

  n_runs <- if (draw_plots) 5 else 1
  seeds <- if (draw_plots) c(23, 67, 69, 123, 98) else 23

  real_runs <- lapply(seeds, function(seed) {
    run_mixed_avg_once(seed, gen_synth_data = F)
  })

  if (draw_plots) {
    TP <- mean(vapply(real_runs, `[[`, numeric(1), "TP"))
    TN <- mean(vapply(real_runs, `[[`, numeric(1), "TN"))
    FP <- mean(vapply(real_runs, `[[`, numeric(1), "FP"))
    FN <- mean(vapply(real_runs, `[[`, numeric(1), "FN"))
    accuracy <- mean(vapply(real_runs, `[[`, numeric(1), "accuracy"))

    cat("Wyniki uśrednione z", n_runs, "uruchomień modelu hybrydowego śr. (dane rzeczywiste)\n")
    cat("TP =", round(TP, 2), "\n")
    cat("TN =", round(TN, 2), "\n")
    cat("FP =", round(FP, 2), "\n")
    cat("FN =", round(FN, 2), "\n")
    cat("Accuracy =", round(accuracy, 4), "\n")

    draw_confusion_matrix(
      round(TP), round(FN), round(TN), round(FP),
      "Mixed_AVG_real_data",
      decimal_digits = 4
    )

    roc_runs <- lapply(real_runs, function(run) {
      list(
        real_classes = run$test_class,
        probabilities = run$probabilities
      )
    })
    draw_averaged_roc_plot(roc_runs, "Model hybrydowy śr. (R)")

    synth_runs <- lapply(seeds, function(seed) {
      run_mixed_avg_once(seed, en_synth_data = T)
    })

    TP_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "TP"))
    TN_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "TN"))
    FP_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "FP"))
    FN_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "FN"))
    accuracy_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "accuracy"))

    cat("Wyniki uśrednione z", n_runs, "uruchomień modelu hybrydowego śr. (dane syntetyczne)\n")
    cat("TP =", round(TP_synth, 2), "\n")
    cat("TN =", round(TN_synth, 2), "\n")
    cat("FP =", round(FP_synth, 2), "\n")
    cat("FN =", round(FN_synth, 2), "\n")
    cat("Accuracy =", round(accuracy_synth, 4), "\n")

    draw_confusion_matrix(
      round(TP_synth), round(FN_synth), round(TN_synth), round(FP_synth),
      "Mixed_AVG_synthetic_data",
      decimal_digits = 4
    )

    roc_synth_runs <- lapply(synth_runs, function(run) {
      list(
        real_classes = run$test_class,
        probabilities = run$probabilities
      )
    })
    draw_averaged_roc_plot(roc_synth_runs, "Model hybrydowy śr. (S)")
  } else {
    result <- real_runs[[1]]
    pred_class <- ifelse(result$probabilities >= 0.5, 1, 0)
    cm <- table(
      Predicted = pred_class,
      Actual = result$test_class
    )
    print(cm)
    cat("TP =", result$TP, "\n")
    cat("TN =", result$TN, "\n")
    cat("FP =", result$FP, "\n")
    cat("FN =", result$FN, "\n")
    cat("Accuracy =", round(result$accuracy, 4), "\n")
  }

  invisible(real_runs)
}


if (sys.nframe() == 0L) {
  do_mixed_model_avg(draw_plots = TRUE, test_on_synth_data = TRUE )
}
