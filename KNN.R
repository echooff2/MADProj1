if (!exists("do_preliminary_analisys")) {
    source("PreliminaryAnalisys.R")
}

if (!exists("draw_confusion_matrix")) {
    source("TableWisualization.R")
}

if (!exists("draw_roc_plot")) {
    source("ROC.R")
}

df <- do_preliminary_analisys(c(F, F, F))


library(caret)
library(class)

set.seed(123)

# =========================
# Podział danych 80/20
# =========================

train_index <- createDataPartition(
    df$blueWins,
    p = 0.8,
    list = FALSE
)

train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

# =========================
# Dobór najlepszego k
# =========================

train_data_cv <- train_data
train_data_cv$blueWins <- factor(train_data_cv$blueWins)

ctrl <- trainControl(
    method = "cv",
    number = 10
)

knn_cv <- caret::train(
    blueWins ~ .,
    data = train_data_cv,
    method = "knn",
    tuneGrid = data.frame(k = seq(1, 101, by = 2)),
    trControl = ctrl
)

best_k <- knn_cv$bestTune$k

cat("Najlepsze k =", best_k, "\n")

# =========================
# Przygotowanie danych
# =========================

train_labels <- train_data$blueWins
test_labels  <- test_data$blueWins

train_features <- subset(train_data, select = -blueWins)
test_features  <- subset(test_data, select = -blueWins)

train_labels_factor <- factor(train_labels, levels = c(0, 1))

# =========================
# Klasyfikacja końcowa
# =========================

knn_pred <- knn(
    train = train_features,
    test = test_features,
    cl = train_labels_factor,
    k = best_k,
    prob = TRUE
)

# Przewidziane klasy
predicted_classes <- as.numeric(as.character(knn_pred))

# Prawdopodobieństwa klasy 1
raw_prob <- attr(knn_pred, "prob")

predicted_probabilities <- ifelse(
    knn_pred == "1",
    raw_prob,
    1 - raw_prob
)

# =========================
# Macierz pomyłek
# =========================

conf_matrix <- table(
    Actual = test_labels,
    Predicted = predicted_classes
)

print(conf_matrix)

# =========================
# TP, TN, FP, FN
# =========================

TP <- sum(test_labels == 1 & predicted_classes == 1)
TN <- sum(test_labels == 0 & predicted_classes == 0)
FP <- sum(test_labels == 0 & predicted_classes == 1)
FN <- sum(test_labels == 1 & predicted_classes == 0)

cat("TP =", TP, "\n")
cat("TN =", TN, "\n")
cat("FP =", FP, "\n")
cat("FN =", FN, "\n")

# =========================
# Metryki
# =========================

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)

cat("Accuracy =", round(accuracy, 4), "\n")
cat("Precision =", round(precision, 4), "\n")
cat("Recall =", round(recall, 4), "\n")
cat("Specificity =", round(specificity, 4), "\n")

draw_confusion_matrix(TP, FN, TN, FP, "KNN")

draw_roc_plot(test_labels, predicted_probabilities, "KNN")