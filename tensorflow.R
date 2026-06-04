                   #keras i tensorflow nie wspierają najnowszych wersji py:
library(keras3)    #3.11 działa i na tym testuje
library(tensorflow)
#install_tensorflow()       #<-obie poniższe instrukcje do utworzenia pakietów py
#keras3::install_keras()    #(może zajęć im trochę czasu, tak kilka minut na jedną, może dosłownie wyglądać jakby padło)

#ostatnia komenda wywołująca funkcję tym pliku jest zakomendowana atm. 

#kompiluje, trenuje i zwraca wektor probabilistyczny ze testowego datasetu.
do_tensor_flow_neuralNet<-function(draw_plots = F){
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  set.seed(23) 
  library(keras3)    
  library(tensorflow)
  
  df <- do_preliminary_analisys()
  blueWins <- df |> select('blueWins')
  df <- df |> select(!c('blueWins'))
  df <- as.matrix(df)
  
  split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
  train_df_data <- df[split, ]
  train_df_class <- blueWins[split, ]
  test_df_data <- df[-split, ]
  test_df_class<- blueWins[-split, ]
  
  model <- keras_model_sequential() %>%                       #16 dla prelim, 38 dla raw
    layer_dense(units = 128, activation = 'elu', input_shape = c(16)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64, activation = 'softplus') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = 'sigmoid') %>%
    compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('BinaryAccuracy')
    )
  
  model %>% fit(train_df_data, train_df_class, epochs=100, batch_size=100,shuffle = TRUE,validation_split = 0.20)
  probability_vector_test <- predict_on_batch(model, test_df_data)
  summary(model)
  if (draw_plots) {
    # MACIERZ POMYLEK
    pred_class <- ifelse(probability_vector_test > 0.5, 1, 0)
    cm <- table(
      Predicted = pred_class,
      Actual = test_df_class
    )
    TN <- cm["0","0"]
    FN <- cm["0","1"]
    FP <- cm["1","0"]
    TP <- cm["1","1"]
    if (!exists("draw_confusion_matrix")) {
      source("TableWisualization.R")
    }
    draw_confusion_matrix(TP, FN, TN, FP, "neural_network")
    
    #ROC
    if (!exists("draw_roc_plot")) {
      source("ROC.R")
    }
    draw_roc_plot(test_df_class, probability_vector_test, "Sieci neuronowe")
  }
  output <- as.data.frame(probability_vector_test)
  output <- output %>% rename_at('V1', ~'probability_vector')
  output$test_class_nn<-test_df_class
  
  return(output)
}


prob_vector_test <- do_tensor_flow_neuralNet(T)
#mean(score)
#summary(score)


if (sys.nframe() == 0L) {
  do_tensor_flow_neuralNet(draw_plots = T)
}

