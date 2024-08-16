#' Function to preprocess data
#' 
#' @param data A sample score matrix. RAVs (rows) x samples (column).
#' @param min Integer(1). The minimum number of samples detecting the feature.
#' Default is 0.
#' 
preprocess_data <- function(data, min = 0) {
    keep_features <- rowSums(data > 0) >= min
    data <- data[keep_features, , drop = FALSE]
    return(data)
}


#' Function to calculate AUC for a single bootstrap sample
#' 
#' @param data A sample score matrix. RAVs (rows) x samples (column).
#' @param labels A vector of outcomes. The number of samples and the length of 
#' labels should be same.
#' @param indices 
#' @param p The percentage of data that goes to training. Between 0 and 1. Default is 0.8.
#' 
calc_auc <- function(data, labels, indices, p, aucOnly) {
    boot_data <- data[, indices, drop = FALSE]
    boot_labels <- labels[indices]
    
    ## Split data into training and test sets
    train_index <- createDataPartition(boot_labels, p = p, list = FALSE)
    train_data <- boot_data[, train_index, drop = FALSE] |> t()
    train_labels <- boot_labels[train_index]
    test_data <- boot_data[, -train_index, drop = FALSE] |> t()
    test_labels <- boot_labels[-train_index]
    
    ## Binary or Multinomial Logit
    lvs <- levels(train_labels)
    nlvs <- nlevels(train_labels)
    train_metric <- if (nlvs == 2) "Accuracy" else "MultinomialLogitMetric"
    
    ## Train random forest model
    rf_model <- train(
        train_data, train_labels,
        method = "rf",
        metric = train_metric,
        trControl = trainControl(
            method = "cv",
            number = 5,
            search = "random"),
        tuneLength = 5
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
#' @param n_bootstrap Integer. Number of bootstrap iterations. Default is 10.
#' 
evaluateRFmodel <- function(data, 
                            labels, 
                            p = 0.8, 
                            min = 0, 
                            aucOnly = FALSE,
                            n_bootstrap = 10) {
    
    ## Sanity check
    if (ncol(data) != length(labels)) {
        msg <- "The number of samples and the length of labels are different."
        stop(msg)
    }
    
    ## Preprocess data
    data <- preprocess_data(data, min = min)
    
    ## Perform bootstrap
    if (!aucOnly) {n_bootstrap <- 1} # For plotting, no bootstrap
    boot_results <- replicate(n_bootstrap, {
        indices <- unlist(lapply(unique(labels), function(lbl) {
            sample(which(labels == lbl), size = sum(labels == lbl), replace = TRUE)
        }))
        calc_auc(data, labels, indices = indices, p, aucOnly)
    })
    
    ## Calculate mean AUC and confidence interval
    mean_auc <- mean(boot_results)
    ci_auc <- quantile(boot_results, c(0.025, 0.975))
    
    if (aucOnly) {
        return(list(mean_auc = mean_auc, 
                    ci_lower = ci_auc[1], 
                    ci_upper = ci_auc[2], 
                    boot_results = boot_results))
    } else {
        plot(roc_obj, print.auc = TRUE, main = "ROC Curve") # plot ROC curve
    }
}