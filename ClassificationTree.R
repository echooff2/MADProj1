set.seed(23)

if (!exists("do_preliminary_analisys")) {
  source("PreliminaryAnalisys.R")
}

if (!exists("draw_confusion_matrix")) {
  source("TableWisualization.R")
}

if (!exists("draw_roc_plot")) {
    source("ROC.R")
}

df <- do_preliminary_analisys()
df$blueWins <- as.factor(df$blueWins)

library(tree)
tree(df)

split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
train <- df[split, ]
test <- df[-split, ]
summary(train)
tree_res <- tree(blueWins ~ ., train)

tree_res2 <- tree(blueWins ~ ., train)
tree_res.cv <- cv.tree(tree_res2, FUN = prune.tree)

for (i in 2:5) {
  tree_res.cv$dev <- tree_res.cv$dev + cv.tree(tree_res2, FUN = prune.tree)$dev
}
tree_res.cv$dev <- tree_res.cv$dev / 5
png("tree_cv.png", width = 800, height = 600)
plot(tree_res.cv)
print(tree_res)
dev.off()


png("tree.png", width = 800, height = 600)
plot(tree_res)
text(tree_res)
dev.off()

summary(tree_res)

predict_probs <- predict(tree_res, newdata = test)[, 2]
predict_test <- predict(tree_res, newdata = test, type = "class")
confusion_matrix <- table(Predicted = predict_test, Actual = test$blueWins)
confusion_matrix
summary(predict_test)

library(caret)
confusionMatrix(predict_test, test$blueWins)

small_tree <- prune.tree(tree_res2, best = 2)
png("small_tree.png", width = 800, height = 600)
plot(small_tree)
text(small_tree)
dev.off()

# 2. Robimy prognozy dla obu modeli na zbiorze testowym
pred_small <- predict(small_tree, newdata = test, type = "class")
pred_large <- predict(tree_res2, newdata = test, type = "class")

# 3. Liczymy Accuracy dla obu
acc_small <- mean(pred_small == test$blueWins)
acc_large <- mean(pred_large == test$blueWins)
acc_normal <- mean(predict_test == test$blueWins)

# 4. Wyświetlamy wynik starcia
cat(paste0(
  "Accuracy przyciętego tree_res2 drzewa: ", round(acc_small * 100, 2), "%\n",
  "Accuracy tree_res2 drzewa: ", round(acc_large * 100, 2), "%\n",
  "Accuracy tree_res drzewa: ", round(acc_normal * 100, 2), "%\n"
))

tn <- confusion_matrix["0", "0"]
tp <- confusion_matrix["1", "1"]
fp <- confusion_matrix["0", "1"]
fn <- confusion_matrix["1", "0"]

draw_confusion_matrix(tp, fn, tn, fp, "classification_tree_confusion_matrix")

draw_roc_plot(test$blueWins, predict_probs, "Drzewo klasyfikacyjne")

for (x in prob_vector_test) {
    print(x)
}