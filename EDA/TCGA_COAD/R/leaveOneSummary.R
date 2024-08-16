## Function to create summary table without one variable at a time
leaveOneSummary <- function(data, labels, all_vars) {
    summary_table <- data.frame(
        Variables = character(),
        Mean_AUC = numeric(),
        CI_Lower = numeric(),
        CI_Upper = numeric(),
        stringsAsFactors = FALSE
    )
    
    ## Calculate AUC
    for (i in seq_along(all_vars)) {
        vars_to_use <- all_vars[-i]
        data_subset <- data[vars_to_use, ]
        eval_result <- evaluateRFmodel(data_subset, labels, p = 0.8, min = 0, 
                                       aucOnly = TRUE, n_bootstrap = 10)
        summary_table <- rbind(summary_table, 
                               data.frame(Removed_Variable = all_vars[i],
                                          Mean_AUC = eval_result$mean_auc,
                                          CI_Lower = eval_result$ci_lower,
                                          CI_Upper = eval_result$ci_upper))
    }
    
    ## Sort the table by Mean_AUC in descending order
    summary_table <- summary_table[order(-summary_table$Mean_AUC), ]
    
    return(summary_table)
}