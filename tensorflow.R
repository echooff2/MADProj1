# keras i tensorflow nie wspierają najnowszych wersji py:
library(keras3) # 3.11 działa i na tym testuje
library(tensorflow)
# install_tensorflow()       #<-obie poniższe instrukcje do utworzenia pakietów py
# keras3::install_keras()    #(może zajęć im trochę czasu, tak kilka minut na jedną, może dosłownie wyglądać jakby padło)

# ostatnia komenda wywołująca funkcję tym pliku jest zakomendowana atm.

run_tf_once <- function(seed, use_synth_data = F, split = NULL) {
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  set.seed(seed)
  library(dplyr)
  library(keras3)
  library(tensorflow)

  dfnt <- do_preliminary_analisys(generate_syntetic_data = use_synth_data)
  if(use_synth_data){
  df <- dfnt$real
  synth_data <- dfnt$synth
  }
  else
  {df<-dfnt}
  blueWins <- df |> select("blueWins")
  df <- df |> select(!c("blueWins"))
  df <- as.matrix(df)

  if (is.null(split)) {
    split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
  }
  train_df_data <- df[split, ]
  train_df_class <- blueWins[split, ]
  test_df_data <- df[-split, ]
  test_df_class <- blueWins[-split, ]

  model <- keras_model_sequential() %>% # 16 dla prelim, 38 dla raw
    layer_dense(units = 128, activation = "elu", input_shape = c(16)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64, activation = "softplus") %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = "sigmoid") %>%
    compile(
      loss = "binary_crossentropy",
      optimizer = "rmsprop",
      metrics = c("BinaryAccuracy")
    )

  model %>% fit(train_df_data, train_df_class,
    epochs = 100, batch_size = 100,
    shuffle = TRUE, validation_split = 0.20
  )

  if (use_synth_data) {
    test_df_class <- synth_data |> select("blueWins")
    synth_data <- synth_data |> select(!c("blueWins"))
    synth_data <- as.matrix(synth_data)
    probability_vector_test <- predict_on_batch(model, synth_data)
    probability_vector_test <- as.data.frame(probability_vector_test)$V1
  } else {
    probability_vector_test <- predict_on_batch(model, test_df_data)
    test_df_class <- as.data.frame(test_df_class)
    if (!"blueWins" %in% names(test_df_class)) {
      names(test_df_class) <- "blueWins"
    }
    probability_vector_test <- as.numeric(probability_vector_test)
  }

  
  #plot_draw
  pred_class <- ifelse(probability_vector_test > 0.5, 1, 0)
  cm <- table(
    Predicted = factor(pred_class, levels = c(0, 1)),
    Actual = factor(test_df_class$blueWins, levels = c(0, 1))
  )

  TP <- ifelse(is.na(cm["1", "1"]), 0, as.numeric(cm["1", "1"]))
  TN <- ifelse(is.na(cm["0", "0"]), 0, as.numeric(cm["0", "0"]))
  FP <- ifelse(is.na(cm["1", "0"]), 0, as.numeric(cm["1", "0"]))
  FN <- ifelse(is.na(cm["0", "1"]), 0, as.numeric(cm["0", "1"]))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  list(
    model = model,
    probability_vector_test = probability_vector_test,
    test_df_class = test_df_class,
    TP = TP,
    TN = TN,
    
    FP = FP,
    FN = FN,
    accuracy = accuracy
  )
}

# kompiluje, trenuje i zwraca wektor probabilistyczny ze testowego datasetu.
do_tensor_flow_neuralNet <- function(draw_plots = F, use_synth_data = F, seed = 23, split = NULL, cross_val=F) {
  n_runs <- if (cross_val) 5 else 1                         #use_synth_data = T
  seeds <- if (cross_val) c(23, 67, 69, 123, 98) else seed  #cross_val = T

  runs <- lapply(seeds, function(s) {
    run_tf_once(s, use_synth_data, split = if (n_runs == 1) split else NULL)
  })
  
  result <- runs[[1]]
  summary(result$model)

  if (draw_plots) {
    TP <- mean(vapply(runs, `[[`, numeric(1), "TP"))
    TN <- mean(vapply(runs, `[[`, numeric(1), "TN"))
    FP <- mean(vapply(runs, `[[`, numeric(1), "FP"))
    FN <- mean(vapply(runs, `[[`, numeric(1), "FN"))
    accuracy <- mean(vapply(runs, `[[`, numeric(1), "accuracy"))

    cat("Wyniki uśrednione z", n_runs, "uruchomień MLP\n")
    cat("TP =", round(TP, 2), "\n")
    cat("TN =", round(TN, 2), "\n")
    cat("FP =", round(FP, 2), "\n")
    cat("FN =", round(FN, 2), "\n")
    cat("Accuracy =", round(accuracy, 4), "\n")

    if (!exists("draw_confusion_matrix")) {
      source("TableWisualization.R")
    }
    if (!exists("draw_roc_plot")) {
      source("ROC.R")
    }

    if (use_synth_data) {
      draw_confusion_matrix(
        round(TP), round(FN), round(TN), round(FP),
        "neural_network_synthetic_data",
        decimal_digits = 4
      )
      roc_plot_name <- "Sieć MLP (S)"
    } else {
      draw_confusion_matrix(
        round(TP), round(FN), round(TN), round(FP),
        "neural_network_real_data",
        decimal_digits = 4
      )
      roc_plot_name <- "Sieć MLP (R)"
    }

    roc_runs <- lapply(runs, function(run) {
      list(
        real_classes = run$test_df_class$blueWins,
        probabilities = run$probability_vector_test
      )
    })
    draw_averaged_roc_plot(roc_runs, roc_plot_name)
  }

  output <- data.frame(probability_vector = result$probability_vector_test)
  if (use_synth_data) {
    t <- data.frame(x=output$probability_vector)
    t$x <- ifelse(t$x>0.5,1,0)
    t$x<-as.factor(t$x)
    dir.create("csv", recursive = TRUE, showWarnings = FALSE)
    write.csv(t, file = "csv/synth_data_tensorflow_mlp.csv")
  }
  output$test_class_nn <- result$test_df_class
  

  
  return(output)
}

if (sys.nframe() == 0L) {
  do_tensor_flow_neuralNet(draw_plots = T,cross_val=T)
}
