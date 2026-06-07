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
Output_class<- Output |> select('test_class_knn')
Output <- Output |> select(!c('test_class_nn'))
Output <- Output |> select(!c('test_class_tree'))
Output <- Output |> select(!c('test_class_knn'))

Output$Sum<-Output$probability_vector+Output$predicted_probabilities+Output$probability_vector


# MACIERZ POMYLEK
Output$Sum <-Output$Sum/3
pred_class <- ifelse(Output$Sum >=0.5, 1, 0)    #<-śr ważona
cm <- table(
  Predicted = pred_class,
  Actual = Output_class$test_class_knn) #<-śr ważona
TN <- cm["0","0"]
FN <- cm["0","1"]
FP <- cm["1","0"]
TP <- cm["1","1"]
if (!exists("draw_confusion_matrix")) {
  source("TableWisualization.R")
}
draw_confusion_matrix(TP, FN, TN, FP, "Mixed_AVG_real_data ")

#ROC
if (!exists("draw_roc_plot")) {
  source("ROC.R")
}
draw_roc_plot(Output_class$test_class_knn, Output$Sum, "Model hybrydowy śr. (R)") #<-śr.waż


### wykonaj poniższe by zrobić mix sr.ważonej na syntetycznych danych
Output <- Output |> select(!c('Sum'))
Output<-cbind(Output,Output_class)
library(synthpop)
synth_data <- syn(Output, method = "cart", cart.minbucket = 10, seed = 67)
test <- synth_data$syn
synthetic_class <- test |> select('test_class_knn')
test <- test |> select(!c('test_class_knn'))
test$Sum<-test$probability_vector+test$predicted_probabilities+test$probability_vector

test$Sum <-test$Sum/3
pred_class <- ifelse(test$Sum >=0.5, 1, 0)    #<-śr ważona
cm <- table(
  Predicted = pred_class,
  Actual = synthetic_class$test_class_knn) #<-śr ważona
TN <- cm["0","0"]
FN <- cm["0","1"]
FP <- cm["1","0"]
TP <- cm["1","1"]
if (!exists("draw_confusion_matrix")) {
  source("TableWisualization.R")
}
draw_confusion_matrix(TP, FN, TN, FP, "Mixed_AVG_synthetic_data ")

#ROC
if (!exists("draw_roc_plot")) {
  source("ROC.R")
}
draw_roc_plot(synthetic_class$test_class_knn, test$Sum, "Model hybrydowy śr. (S)") #<-śr.waż


