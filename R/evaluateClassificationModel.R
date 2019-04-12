#' Evaluate binary classification models.
#'
#' Evaluate binary classification model outputs based on the predicted probabilities and actual values.
#' For evaluation purposes, the Confusion Matrix, ROC Curve, and AUC are returned.
#'
#' @import dplyr
#' @import caret
#'
#' @param actual Vector of actual binary classes being predicted (y).
#' @param pred_prob Vector of predicted probabilities by the model (y-hat).
#' @param cutoff Cutoff threshold to be set. Default value is 0.5.
#'
#' @return None
#'
#' @examples
#' evaluateClassificationModel(data_df$Actual, data_df$Predicted_Probabilities)
#'
#' @export

evaluateClassificationModel <- function(actual, pred_prob, cutoff=0.5){
  results_df <- data.frame(Actual=actual, Pred_Prob=pred_prob)
  results_df$Pred_Class <- ifelse(results_df$Pred_Prob >= cutoff, levels(actual)[2], levels(actual)[1])
  results_df$Pred_Class <- factor(results_df$Pred_Class, levels=levels(results_df$Actual))
  message("Confusion Matrix:")
  print(confusionMatrix(reference=results_df$Actual, data=results_df$Pred_Class, positive = levels(actual)[2]))
  message("Printing ROC Curve...")
  result_roc <- roc(response=results_df$Actual, predictor=results_df$Pred_Prob)
  plot(result_roc, print.auc=TRUE)
}
