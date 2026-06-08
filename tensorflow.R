                   #keras i tensorflow nie wspierają najnowszych wersji py:
library(keras3)    #3.11 działa i na tym testuje
library(tensorflow)
#install_tensorflow()       #<-obie poniższe instrukcje do utworzenia pakietów py
#keras3::install_keras()    #(może zajęć im trochę czasu, tak kilka minut na jedną, może dosłownie wyglądać jakby padło)

#ostatnia komenda wywołująca funkcję tym pliku jest zakomendowana atm. 

#kompiluje, trenuje i zwraca wektor probabilistyczny ze testowego datasetu.
do_tensor_flow_neuralNet<-function(draw_plots = F, use_synth_data=F, random_seed=F){
  if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
  }
  if(!random_seed){
  set.seed(23)}
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
  if(use_synth_data){
    df <- do_preliminary_analisys()
    library(synthpop)
    synth_data <- syn(df, method = "cart", cart.minbucket = 10, seed = 67)
    test <- synth_data$syn
    
    test_df_class <- test |> select('blueWins')
    test <- test |> select(!c('blueWins'))
    test <- as.matrix(test)
    probability_vector_test <- predict_on_batch(model, test)
    probability_vector_test <- as.data.frame(probability_vector_test)
    probability_vector_test <-probability_vector_test$V1
  }
  else
  {
  probability_vector_test <- predict_on_batch(model, test_df_data)
  test_df_class <- as.data.frame(test_df_class)
  test_df_class <- test_df_class %>% rename_at('test_df_class', ~'blueWins')
  }
  summary(model)
  if (draw_plots) {
    # MACIERZ POMYLEK
    pred_class <- ifelse(probability_vector_test > 0.5, 1, 0)
    cm <- table(
      Predicted = pred_class,
      Actual = test_df_class$blueWins
    )
    TN <- cm["0","0"]
    FN <- cm["0","1"]
    FP <- cm["1","0"]
    TP <- cm["1","1"]
    if (!exists("draw_confusion_matrix")) {
      source("TableWisualization.R")
    }
    if(use_synth_data){
    draw_confusion_matrix(TP, FN, TN, FP, "neural_network_synthetic_data", decimal_digits = 4)
    if (!exists("draw_roc_plot")) {
      source("ROC.R")
    }
      draw_roc_plot(test_df_class$blueWins, probability_vector_test, "Sieć MLP (S)")  
    }
    else{
    draw_confusion_matrix(TP, FN, TN, FP, "neural_network_real_data", decimal_digits = 4)
    if (!exists("draw_roc_plot")) {
      source("ROC.R")
    }
    draw_roc_plot(test_df_class$blueWins, probability_vector_test, "Sieć MLP (R)") 
    }
    }
  
  #proszę nie pytać dlaczego tak jest, info o nazwach kolumn trzeba jakoś usunąć
  if(use_synth_data){
  output<-as.data.frame(probability_vector_test)
  output$V1 <-probability_vector_test
  output <- output %>% rename_at('probability_vector_test', ~'probability_vector')
  output$test_class_nn<-test_df_class
  }
  else{
  output<-as.data.frame(probability_vector_test)
  output <- output %>% rename_at('V1', ~'probability_vector')
  output$test_class_nn<-test_df_class
  }
  
  return(output)
}

do_tf_valcycle<-function(synthetic=F){
  if(synthetic)
  {prob_vector_test_1 <- do_tensor_flow_neuralNet(F,T,T)} #<-Syntetyczne
  else
  {prob_vector_test_1 <- do_tensor_flow_neuralNet(F,F,T)} #<-realne
  prob_vector_test_1$probability_vector <- ifelse(prob_vector_test_1$probability_vector >=0.5, 1, 0)
  prob_vector_test_1$accuracy <- ifelse(prob_vector_test_1$probability_vector ==prob_vector_test_1$test_class_nn, 1, 0)
  acc_1 <-sum(prob_vector_test_1$accuracy)/length(prob_vector_test_1$accuracy)  
 }

##dane realne, + walidacja, okropne wiem
acc_1<-do_tf_valcycle(F)
acc_2<-do_tf_valcycle(F)
acc_3<-do_tf_valcycle(F)
acc_4<-do_tf_valcycle(F)
acc_5<-do_tf_valcycle(F)
total_acc<-(acc_1+acc_2+acc_3+acc_4+acc_5)/5
print(total_acc) #<- to po cv-kfolds 


##dane syntetyczne
acc_1<-do_tf_valcycle(T)
acc_2<-do_tf_valcycle(T)
acc_3<-do_tf_valcycle(T)
acc_4<-do_tf_valcycle(T)
acc_5<-do_tf_valcycle(T)
total_acc<-(acc_1+acc_2+acc_3+acc_4+acc_5)/5
print(total_acc) #<- to po cv-kfolds 

prob_vector_test <- do_tensor_flow_neuralNet(T,T,F) #<-syntetyczne
#mean(score)
#summary(score)


if (sys.nframe() == 0L) {
  do_tensor_flow_neuralNet(draw_plots = T)
}

