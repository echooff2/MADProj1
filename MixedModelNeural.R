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
Classification_tree_prob <- do_classification_tree() #trzeba odpalić Train<-df[-split, ] w tamtym pliku, nwm dlaczego 
Tensor_prob <- as.data.frame(Tensor_prob)           #od 77 lini do 84
Classification_tree_prob <- as.data.frame(Classification_tree_prob)
KNN_prob<-do_knn()
KNN_prob<-as.data.frame(KNN_prob)      

if (!exists("do_preliminary_analisys")) {
  source("PreliminaryAnalisys.R")
}

library(dplyr)
Output <- cbind(Tensor_prob,KNN_prob)
Output <- cbind(Output,Classification_tree_prob)
Output_class<- Output |> select('test_class_nn')
Output <- Output |> select(!c('test_class_nn'))
Output <- Output |> select(!c('test_class_tree'))
Output <- Output |> select(!c('test_class_knn'))

Output<-as.matrix(Output)
Output_class<-as.matrix(Output_class)


#poniżej sieć neuronowa
split <- sample(seq_len(nrow(Output)), round(0.7 * nrow(Output)))
train_data <- Output[split, ]
train_class <- Output_class[split, ]
test_data <- Output[-split, ]
test_class<- Output_class[-split, ]

train_data <- as.matrix(train_data)
train_class <- as.matrix(train_class)
test_data <- as.matrix(test_data)
test_class<- as.matrix(test_class)


library(keras3)    
library(tensorflow)

set.seed(23)
Model <- keras_model_sequential() %>%                     
  layer_dense(units = 32, activation = 'elu', input_shape = c(3)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 8, activation = 'softplus') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid') %>%
  compile(
    loss = 'binary_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('BinaryAccuracy')
  )


#\nnet/
Model %>% fit(train_data, train_class, epochs=150, batch_size=50,shuffle = TRUE,validation_split = 0.10)
probability_vector_test <- predict_on_batch(Model, test_data)


# MACIERZ POMYLEK
pred_class <- ifelse(probability_vector_test >=0.5, 1, 0)  #<-nnet
cm <- table(
  Predicted = pred_class,
  Actual = test_class) #<-nnet
TN <- cm["0","0"]
FN <- cm["0","1"]
FP <- cm["1","0"]
TP <- cm["1","1"]
if (!exists("draw_confusion_matrix")) {
  source("TableWisualization.R")
}
draw_confusion_matrix(TP, FN, TN, FP, "Mixed_NN_real_data", decimal_digits = 4)

#ROC
if (!exists("draw_roc_plot")) {
  source("ROC.R")
}
draw_roc_plot(test_class, probability_vector_test, "Model hybrydowy MLP(R)") #<-nnet


### wykonaj poniższe by zrobić mix sieci na syntetycznych danych
Output<-as.data.frame(Output)
Output_class<-as.data.frame(Output_class)
Output<-cbind(Output,Output_class)
library(synthpop)
synth_data <- syn(Output, method = "cart", cart.minbucket = 10, seed = 67)
test <- synth_data$syn

synthetic_class<- test |> select('test_class_nn')
test <- test |> select(!c('test_class_nn'))

test<-as.matrix(test)
synthetic_class<-as.matrix(synthetic_class)
probability_vector_test <- predict_on_batch(Model, test)


pred_class <- ifelse(probability_vector_test >=0.5, 1, 0)  #<-nnet
cm <- table(
  Predicted = pred_class,
  Actual = synthetic_class) #<-nnet
TN <- cm["0","0"]
FN <- cm["0","1"]
FP <- cm["1","0"]
TP <- cm["1","1"]
if (!exists("draw_confusion_matrix")) {
  source("TableWisualization.R")
}
draw_confusion_matrix(TP, FN, TN, FP, "Mixed_NN_synthetic_data", decimal_digits = 4)

#ROC
if (!exists("draw_roc_plot")) {
  source("ROC.R")
}
draw_roc_plot(synthetic_class, probability_vector_test, "Model hybrydowy MLP(S)") 



