#' Evaluate regression (continuous output prediction) models.
#'
#' Evaluate regression (continuous output prediction) model outputs based on predictions and actual values.
#' This version only allows evaluation on temporal data (timestamp-based data) with a DateTime index.
#' For evaluation purposes, the MAPE, MAE, RMSE, and R2 values are returned.
#'
#' @import dplyr
#' @import xts
#' @import dygraphs
#' @import ggplot2
#'
#' @param time Vector of DateTime (POSIXct) to be used as index for plotting.
#' @param actual Actual values of the parameter being predicted (y).
#' @param predicted Predicted values of the parameter being predicted (y-hat).
#' @param title Title to be added to the plots. Defaults to \emph{NULL} with no title included.
#' @param lim_val Single numeric value as limit value for plotting (note for ggplot2 objects lims() is used).
#'  This does not affect the evaluation metric calculations.
#' @param calcMAPE \emph{TRUE/FALSE} to indicate whether MAPE should be calculated or not. MAPE is not calculatable if there are 0's in the
#'  actual values (results in \emph{Inf} due to division by 0). Defaults to \emph{TRUE}.
#' @param scatterplot \emph{TRUE/FALSE}, whether Actual vs Predicted scatterplot should be shown.
#' @param trendplot \emph{TRUE/FALSE}, whether Trend Plot of Actual & Predicted should be shown.
#' @param residualplot \emph{TRUE/FALSE}, whether histogram of residuals (Predicted - Actual) should be shown.
#' @param scale_attributes Dataframe object that details the Mean and Std_Dev of variables obtained from \code{scale_data()}.
#' @seealso \code{\link{scale_data}}
#'
#' @return None
#'
#' @examples
#' evaluateRegressionModel(data_df$DateTime, data_df$Actual, data_df$Predicted, title="Evaluation of Regression Model")
#'
#' @export

evaluateRegressionModel <- function(time, actual, predicted, title=NULL, lim_val=NULL, calcMAPE = TRUE, scatterplot=TRUE, trendplot=TRUE, residualplot=TRUE, scale_attributes=NULL){

  if (!is.null(scale_attributes)){
    if (nrow(scale_attributes) > 1){
      message("Invalid 'scale_attributes' provided. scale_attributes should be a single row dataframe with Mean and StdDev of the predicted (y) variable.")
      break
    } else {
      mean <- scale_attributes$Mean
      stddev <- scale_attributes$StdDev
      actual <- actual * stddev + mean
      predicted <- predicted * stddev + mean
    }
  }

  results_df <- data.frame(DateTime = time, Actual = as.numeric(actual), Predicted = as.numeric(predicted))
  results_df$Predicted[which(results_df$Predicted < 0)] <- 0
  results_df$Error <- results_df$Predicted - results_df$Actual

  mae <- mean(abs(results_df$Error), na.rm=TRUE)
  rmse <- sqrt(mean(results_df$Error^2, na.rm=TRUE))
  r2 <- 1 - sum(results_df$Error^2, na.rm=TRUE) / sum(results_df$Actual - mean(results_df$Actual, na.rm=TRUE), na.rm=TRUE) # R2 = 1 - SSE/SST

  message(paste0("MAE : ", mae))
  if (calcMAPE == TRUE){
    mape <- mean(abs(results_df$Error / results_df$Actual), na.rm=TRUE) * 100
    message(paste0("MAPE : ", mape, "%"))
  }
  message(paste0("RMSE : ", rmse))
  message(paste0("R-squared : ", r2))

  ## Scatterplot
  if (scatterplot == TRUE){
    g <- ggplot(results_df, aes(x=Actual, y=Predicted)) + geom_point(size=3, alpha=0.5) + geom_smooth(method='lm') +
      geom_abline(intercept=0, slope=1, size=2, col='seagreen2', alpha=0.5) +
      labs(title=paste0(title,' : Predicted vs Actual')) + theme_bw()
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
    trend <- xts(results_df %>% dplyr::select(Predicted, Actual), results_df$DateTime)
    dyObj <- dygraph(trend, main=title)
    print(dyObj)
  }

  ## Residual Plot
  if (residualplot == TRUE){
    g <- ggplot(results_df, aes(x=Error)) + geom_histogram(fill='grey', col='black') +
      labs(x='Error (Predicted - Actual)', y='Frequency', title=paste0(title,' Residual Plot')) +
      theme_bw() + theme(plot.title=element_text(hjust=0.5))
    print(g)
  }
}
