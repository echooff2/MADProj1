flatten_class_column <- function(x) {
  if (is.data.frame(x)) {
    as.numeric(x[[1]])
  } else {
    as.numeric(x)
  }
}


build_hybrid_output <- function() {
  if (!exists("do_tensor_flow_neuralNet")) {
    source("tensorflow.R")
  }
  if (!exists("do_classification_tree")) {
    source("ClassificationTree.R")
  }
  if (!exists("do_knn")) {
    source("KNN.R")
  }

  Tensor_prob <- do_tensor_flow_neuralNet()
  Classification_tree_prob <- do_classification_tree()
  Tensor_prob <- as.data.frame(Tensor_prob)
  Classification_tree_prob <- as.data.frame(Classification_tree_prob)
  KNN_prob <- do_knn()
  KNN_prob <- as.data.frame(KNN_prob)

  library(dplyr)
  Output <- cbind(Tensor_prob, KNN_prob)
  Output <- cbind(Output, Classification_tree_prob)
  Output_class <- data.frame(
    test_class_nn = flatten_class_column(Output$test_class_nn)
  )
  Output <- Output |> select(!c("test_class_nn", "test_class_tree", "test_class_knn"))
  names(Output) <- c("prob_nn", "prob_knn", "prob_tree")

  list(
    output = as.matrix(Output),
    output_class = as.matrix(Output_class),
    output_df = Output,
    output_class_df = Output_class
  )
}


create_mixed_model <- function() {
  keras_model_sequential() %>%
    layer_dense(units = 32, activation = "elu", input_shape = c(3)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 8, activation = "softplus") %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = "sigmoid") %>%
    compile(
      loss = "binary_crossentropy",
      optimizer = "rmsprop",
      metrics = c("BinaryAccuracy")
    )
}


run_mixed_nn_once <- function(hybrid_data, seed, use_synth_data = FALSE) {
  set.seed(seed)
  library(keras3)
  library(tensorflow)

  output <- hybrid_data$output
  output_class <- hybrid_data$output_class

  split <- sample(seq_len(nrow(output)), round(0.7 * nrow(output)))
  train_data <- as.matrix(output[split, ])
  train_class <- as.matrix(output_class[split, ])
  test_data <- as.matrix(output[-split, ])
  test_class <- as.matrix(output_class[-split, ])

  model <- create_mixed_model()
  model %>% fit(
    train_data,
    train_class,
    epochs = 150,
    batch_size = 50,
    shuffle = TRUE,
    validation_split = 0.10
  )

  if (use_synth_data) {
    output_df <- hybrid_data$output_df
    output_class_df <- hybrid_data$output_class_df
    combined <- cbind(output_df, output_class_df)

    library(synthpop)
    synth_data <- syn(combined, method = "cart", cart.minbucket = 10, seed = 67)
    test_synth <- synth_data$syn

    synthetic_class <- test_synth |> dplyr::select("test_class_nn")
    test_synth <- test_synth |> dplyr::select(!c("test_class_nn"))
    test_synth <- as.matrix(test_synth)
    synthetic_class <- as.matrix(synthetic_class)

    probability_vector_test <- predict_on_batch(model, test_synth)
    test_class <- synthetic_class
  } else {
    probability_vector_test <- predict_on_batch(model, test_data)
  }

  probability_vector_test <- as.numeric(probability_vector_test)
  pred_class <- ifelse(probability_vector_test >= 0.5, 1, 0)

  cm <- table(
    Predicted = factor(pred_class, levels = c(0, 1)),
    Actual = factor(test_class, levels = c(0, 1))
  )

  TP <- ifelse(is.na(cm["1", "1"]), 0, as.numeric(cm["1", "1"]))
  TN <- ifelse(is.na(cm["0", "0"]), 0, as.numeric(cm["0", "0"]))
  FP <- ifelse(is.na(cm["1", "0"]), 0, as.numeric(cm["1", "0"]))
  FN <- ifelse(is.na(cm["0", "1"]), 0, as.numeric(cm["0", "1"]))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  list(
    model = model,
    probability_vector_test = probability_vector_test,
    test_class = test_class,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    accuracy = accuracy
  )
}


do_mixed_model_neural <- function(draw_plots = FALSE) {
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
  }
  if (!exists("draw_averaged_roc_plot")) {
    source("ROC.R")
  }

  hybrid_data <- build_hybrid_output()

  n_runs <- if (draw_plots) 5 else 1
  seeds <- if (draw_plots) c(23, 67, 69, 123, 98) else 23

  real_runs <- lapply(seeds, function(seed) {
    run_mixed_nn_once(hybrid_data, seed, use_synth_data = FALSE)
  })

  result <- real_runs[[1]]
  summary(result$model)

  if (draw_plots) {
    TP <- mean(vapply(real_runs, `[[`, numeric(1), "TP"))
    TN <- mean(vapply(real_runs, `[[`, numeric(1), "TN"))
    FP <- mean(vapply(real_runs, `[[`, numeric(1), "FP"))
    FN <- mean(vapply(real_runs, `[[`, numeric(1), "FN"))
    accuracy <- mean(vapply(real_runs, `[[`, numeric(1), "accuracy"))

    cat("Wyniki uśrednione z", n_runs, "uruchomień modelu hybrydowego MLP (dane rzeczywiste)\n")
    cat("TP =", round(TP, 2), "\n")
    cat("TN =", round(TN, 2), "\n")
    cat("FP =", round(FP, 2), "\n")
    cat("FN =", round(FN, 2), "\n")
    cat("Accuracy =", round(accuracy, 4), "\n")

    draw_confusion_matrix(
      round(TP), round(FN), round(TN), round(FP),
      "Mixed_NN_real_data",
      decimal_digits = 4
    )

    roc_runs <- lapply(real_runs, function(run) {
      list(
        real_classes = run$test_class,
        probabilities = run$probability_vector_test
      )
    })
    draw_averaged_roc_plot(roc_runs, "Model hybrydowy MLP(R)")

    synth_runs <- lapply(seeds, function(seed) {
      run_mixed_nn_once(hybrid_data, seed, use_synth_data = TRUE)
    })

    TP_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "TP"))
    TN_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "TN"))
    FP_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "FP"))
    FN_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "FN"))
    accuracy_synth <- mean(vapply(synth_runs, `[[`, numeric(1), "accuracy"))

    cat("Wyniki uśrednione z", n_runs, "uruchomień modelu hybrydowego MLP (dane syntetyczne)\n")
    cat("TP =", round(TP_synth, 2), "\n")
    cat("TN =", round(TN_synth, 2), "\n")
    cat("FP =", round(FP_synth, 2), "\n")
    cat("FN =", round(FN_synth, 2), "\n")
    cat("Accuracy =", round(accuracy_synth, 4), "\n")

    draw_confusion_matrix(
      round(TP_synth), round(FN_synth), round(TN_synth), round(FP_synth),
      "Mixed_NN_synthetic_data",
      decimal_digits = 4
    )

    roc_synth_runs <- lapply(synth_runs, function(run) {
      list(
        real_classes = run$test_class,
        probabilities = run$probability_vector_test
      )
    })
    draw_averaged_roc_plot(roc_synth_runs, "Model hybrydowy MLP(S)")
  } else {
    pred_class <- ifelse(result$probability_vector_test >= 0.5, 1, 0)
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

  invisible(result)
}


if (sys.nframe() == 0L) {
  do_mixed_model_neural(draw_plots = TRUE)
}
