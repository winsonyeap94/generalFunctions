#' Combined Train-Test evaluation of regression (continuous output prediction) models.
#'
#' Evaluate regression (continuous output prediction) model outputs based on predictions and actual values (both Train & Test).
#' This version only allows evaluation on temporal data (timestamp-based data) with a DateTime index.
#' For evaluation purposes, the MAPE, MAE, RMSE, and R2 values are returned.
#'
#' @import dplyr
#' @import xts
#' @import dygraphs
#' @import ggplot2
#'
#' @param train_time Vector of DateTime (POSIXct) for training period to be used as index for plotting.
#' @param train_actual Actual values for training data of the parameter being predicted (y).
#' @param train_predicted Predicted values for training data of the parameter being predicted (y-hat).
#' @param test_time Vector of DateTime (POSIXct) for testing period to be used as index for plotting.
#' @param test_actual Actual values for test data of the parameter being predicted (y).
#' @param test_predicted Predicted values for test data of the parameter being predicted (y-hat).
#' @param title Title to be added to the plots. Defaults to \emph{NULL} with no title included.
#' @param lim_val Single numeric value as limit value for plotting (note for ggplot2 objects lims() is used).
#'  This does not affect the evaluation metric calculations.
#' @param calcMAPE \emph{TRUE/FALSE} to indicate whether MAPE should be calculated or not. MAPE is not calculatable if there are 0's in the
#'  actual values (results in \emph{Inf} due to division by 0). Defaults to \emph{TRUE}.
#' @param scatterplot \emph{TRUE/FALSE}, whether Actual vs Predicted scatterplot should be shown.
#' @param trendplot \emph{TRUE/FALSE}, whether Trend Plot of Actual & Predicted should be shown.
#' @param residualplot \emph{TRUE/FALSE}, whether histogram of residuals (Predicted - Actual) should be shown.
#' @seealso \code{\link{evaluateRegressionModel}}
#'
#' @return None
#'
#' @examples
#' combinedRegressionEvaluation(train_df$DateTime, train_df$Actual, train_df$Predicted, test_df$DateTime, test_df$Actual, test_df$Predicted, title="Evaluation of Regression Model")
#'
#' @export

combinedRegressionEvaluation <- function(train_time, train_actual, train_predicted, test_time, test_actual, test_predicted, title=NULL, lim_val=NULL, calcMAPE=TRUE, scatterplot=TRUE, trendplot=TRUE, residualplot=TRUE){
  results_df <- data.frame(DateTime = train_time, Actual = as.numeric(train_actual), Predicted = as.numeric(train_predicted), Group = "Train")
  results_df <- rbind(results_df, data.frame(DateTime = test_time, Actual = as.numeric(test_actual), Predicted = as.numeric(test_predicted), Group = "Test"))
  results_df$Error <- results_df$Predicted - results_df$Actual

  # Calculating results metrics
  message("Performance Metrics:")
  if (calcMAPE == TRUE){
    print(results_df %>% group_by(Group) %>% summarise(MAE = mean(abs(Error), na.rm=TRUE),
                                                       MAPE = mean(abs(Error/Actual), na.rm=TRUE),
                                                       RMSE = sqrt(mean(Error ^ 2, na.rm=TRUE)),
                                                       R2 = cor(Actual, Predicted, use='pairwise.complete.obs')^2))
  } else {
    print(results_df %>% group_by(Group) %>% summarise(MAE = mean(abs(Error), na.rm=TRUE),
                                                       RMSE = sqrt(mean(Error ^ 2, na.rm=TRUE)),
                                                       R2 = cor(Actual, Predicted, use='pairwise.complete.obs')^2))
  }

  ## Scatterplot
  if (scatterplot == TRUE){
    g <- ggplot(results_df, aes(x=Actual, y=Predicted, col=Group, fill=Group)) + geom_point(size=3, alpha=0.5) + geom_smooth(method='lm') +
      geom_abline(intercept=0, slope=1, size=2, col='seagreen2', alpha=0.5) +
      labs(title=paste0(title,' : Predicted vs Actual')) + theme_bw() + theme(legend.position='top', legend.title=element_blank())
    if (is.null(lim_val)){
      g <- g + lims(x=c(min(min(results_df$Actual),min(results_df$Predicted)), max(max(results_df$Actual),max(results_df$Predicted))),
                    y=c(min(min(results_df$Actual),min(results_df$Predicted)), max(max(results_df$Actual),max(results_df$Predicted))))
    } else {
      g <- g + lims(x=c(0,lim_val), y=c(0,lim_val))
    }
    print(g)
  }

  ## Trend Plot
  if (trendplot == TRUE){
    train_trend <- xts(results_df %>% filter(Group=="Train") %>% dplyr::select(Predicted, Actual) %>% rename(`Train Predicted` = Predicted, `Train Actual` = Actual), results_df$DateTime[which(results_df$Group=="Train")])
    test_trend <- xts(results_df %>% filter(Group=="Test") %>% dplyr::select(Predicted, Actual) %>% rename(`Test Predicted` = Predicted, `Test Actual` = Actual), results_df$DateTime[which(results_df$Group=="Test")])
    trend <- cbind(train_trend, test_trend)
    dyObj <- dygraph(trend, main=title)
    print(dyObj)
  }

  ## Residual Plot
  if (residualplot == TRUE){
    g <- ggplot(results_df, aes(x=Error, fill=Group)) + geom_histogram(aes(y=..density..), col='black', alpha=0.25) + geom_density(alpha=0.5) +
      labs(x='Error (Predicted - Actual)', y='Frequency', title=paste0(title,' Residual Plot')) +
      theme_bw() + theme(plot.title=element_text(hjust=0.5), legend.position='top', legend.title=element_blank())
    print(g)
  }

}
