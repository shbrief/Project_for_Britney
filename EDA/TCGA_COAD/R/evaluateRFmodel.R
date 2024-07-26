#' Function to preprocess data
#' 
#' @param data A sample score matrix. RAVs (rows) x samples (column).
#' @param min Integer(1). The minimum number of samples detecting the feature.
#' Default is 0.
#' 
preprocess_data <- function(data, min = 0) {
    keep_features <- rowSums(data > 0) >= min
    data <- data[keep_features, , drop=FALSE]
    return(data)
}

#' Evaluate Random Forest classification model using ROC curve
#' 
#' @import caret
#' @import pROC
#' 
#' @param data A sample score matrix. RAVs (rows) x samples (column).
#' @param labels A vector of outcomes. The number of samples and the length of 
#' labels should be same.
#' @param min Integer(1). The minimum number of samples detecting the feature.
#' Default is 0.
#' @param p The percentage of data that goes to training. Between 0 and 1. Default is 0.8.
#' @param aucOnly If `TRUE`, this function returns only the AUC. 
#' 
evaluateRFmodel <- function(data, labels, p = 0.8, min = 0, aucOnly = FALSE) {
    
    ## Sanity check
    if (ncol(data) != length(labels)) {
        msg <- "The number of samples and the length of labels are different."
        stop(msg)
    }
    
    ## Preprocess data
    data <- preprocess_data(data, min = min)
    
    ## Split data into training and test sets
    train_index <- createDataPartition(labels, p = p, list = FALSE)
    train_data <- data[,train_index,drop=FALSE] |> t()
    train_labels <- labels[train_index]
    test_data <- data[,-train_index,drop=FALSE] |> t()
    test_labels <- labels[-train_index]
    
    ## Binary or Multinomial Logit
    lvs <- levels(train_labels)
    nlvs <- nlevels(train_labels)
    if (nlvs == 2) {
        train_metric <- "Accuracy"
    } else if (nlvs > 2) {
        train_metric <- "MultinomialLogitMetric"
    }
    
    ## Train random forest model with nested cross-validation
    rf_model <- train(
        train_data, train_labels,
        method = "rf", # random forest
        metric = train_metric,
        trControl = trainControl(
            method = "repeatedcv", # repeating cross-validation
            number = 5, # number of re-sampling iterations
            repeats = 5,
            search = "random"),
        tuneLength = 20
    )
    
    ## Evaluate model performance on test set
    predictions <- predict(rf_model, newdata = test_data, type = "prob")
    
    ## Generate ROC curve
    if (nlvs == 2) {
        roc_obj <- roc(response = factor(test_labels, levels = lvs),
                       predictor = predictions[, 2],
                       levels = rev(lvs))
        if (isTRUE(aucOnly)) {
            return(roc_obj$auc)
        } else {
            plot(roc_obj, print.auc = TRUE, main = "ROC Curve") # plot ROC curve
        }
    } else {
        roc_obj <- multiclass.roc(response = factor(test_labels, levels = lvs),
                                  predictor = predictions)
        if (isFALSE(aucOnly)) {
            msg <- "A multiclass AUC is a mean of several auc and cannot be plotted."
            message(msg)
        }
        return(roc_obj$auc)
    }
}