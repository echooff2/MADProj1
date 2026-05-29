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
df <- read.csv("lol.csv", header = T)
blueWins <- df |> select('blueWins')
df <- df |>
  select(!c('blueWins', 'gameId'))
df<-scale(df)


inp <- sample(2, nrow(df), replace = TRUE, prob = c(0.6, 0.4))
training_df_data <- df[inp==1,]
test_df_data <- df[inp==2, ] #
training_df_class <- blueWins[inp==1,]
test_df_class <- blueWins[inp==2,]

score<-NULL

for(x in 1:100){
model <- keras_model_sequential() %>%                       #16 dla prelim, 38 dla raw
  layer_dense(units = 128, activation = 'elu', input_shape = c(38)) %>%
  layer_dropout(rate = 0.5) %>%
#  layer_dense(units = 128, activation = 'relu') %>%
#  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64, activation = 'softplus') %>%
  layer_dropout(rate = 0.5) %>%
#  layer_dense(units = 8, activation = 'softmax') %>%
#  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid') %>%
  compile(
    loss = 'binary_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('BinaryAccuracy')
  )


#summary(model)
#print(x)
model %>% fit(training_df_data, training_df_class, epochs=50, batch_size=100)
  score_add = model %>%evaluate(test_df_data,test_df_class)
  score_add<-as.data.frame(score_add)
  score <-c(score,score_add$BinaryAccuracy)
}

mean(score)
summary(score)

