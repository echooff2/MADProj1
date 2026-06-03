                   #keras i tensorflow nie wspierają najnowszych wersji py:
library(keras3)    #3.11 działa i na tym testuje
library(tensorflow)
#install_tensorflow()       #<-obie poniższe instrukcje do utworzenia pakietów py
#keras3::install_keras()    #(może zajęć im trochę czasu, tak kilka minut na jedną, może dosłownie wyglądać jakby padło)

df<-do_preliminary_analisys(c(F, F, F))
blueWins <- df |> select('blueWins')
df <- df |> select(!c('blueWins'))
df <- as.matrix(df)
blueWins<-as.matrix(blueWins)

#test dla danych bez analizy wstępnej
#df <- read.csv("lol.csv", header = T)
#blueWins <- df |> select('blueWins')
#df <- df |>
#  select(!c('blueWins', 'gameId'))
#df<-scale(df)

inp <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
training_df_data <- df[inp==1,]
test_df_data <- df[inp==2, ] #
training_df_class <- blueWins[inp==1,]
test_df_class <- blueWins[inp==2,]

#kompiluje, trenuje i zwraca wektor probabilistyczny ze testowego datasetu.
do_tensor_flow_neuralNet<-function(train_df_data,train_df_class,test_df_data,test_df_class){
  set.seed(69) 
  library(keras3)    
  library(tensorflow)
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
  
  model %>% fit(train_df_data, train_df_class, epochs=50, batch_size=100)
  probability_vector_test <- predict_on_batch(model, test_df_data)
  summary(model)
  return(probability_vector_test)
}


prob_vector_test <- do_tensor_flow_neuralNet(training_df_data,training_df_class,test_df_data,test_df_class)
#mean(score)
#summary(score)

# MACIERZ POMYLEK
pred_class <- ifelse(prob_vector_test > 0.5, 1, 0)
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
draw_roc_plot(test_df_class, prob_vector_test, "Sieci neuronowe")