% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinedRegressionEvaluation.R
\name{combinedRegressionEvaluation}
\alias{combinedRegressionEvaluation}
\title{Combined Train-Test evaluation of regression (continuous output prediction) models.}
\usage{
combinedRegressionEvaluation(train_time, train_actual, train_predicted,
  test_time, test_actual, test_predicted, title = NULL, lim_val = NULL,
  calcMAPE = TRUE, scatterplot = TRUE, trendplot = TRUE,
  residualplot = TRUE)
}
\arguments{
\item{train_time}{Vector of DateTime (POSIXct) for training period to be used as index for plotting.}

\item{train_actual}{Actual values for training data of the parameter being predicted (y).}

\item{train_predicted}{Predicted values for training data of the parameter being predicted (y-hat).}

\item{test_time}{Vector of DateTime (POSIXct) for testing period to be used as index for plotting.}

\item{test_actual}{Actual values for test data of the parameter being predicted (y).}

\item{test_predicted}{Predicted values for test data of the parameter being predicted (y-hat).}

\item{title}{Title to be added to the plots. Defaults to \emph{NULL} with no title included.}

\item{lim_val}{Single numeric value as limit value for plotting (note for ggplot2 objects lims() is used).
This does not affect the evaluation metric calculations.}

\item{calcMAPE}{\emph{TRUE/FALSE} to indicate whether MAPE should be calculated or not. MAPE is not calculatable if there are 0's in the
actual values (results in \emph{Inf} due to division by 0). Defaults to \emph{TRUE}.}

\item{scatterplot}{\emph{TRUE/FALSE}, whether Actual vs Predicted scatterplot should be shown.}

\item{trendplot}{\emph{TRUE/FALSE}, whether Trend Plot of Actual & Predicted should be shown.}

\item{residualplot}{\emph{TRUE/FALSE}, whether histogram of residuals (Predicted - Actual) should be shown.}
}
\value{
None
}
\description{
Evaluate regression (continuous output prediction) model outputs based on predictions and actual values (both Train & Test).
This version only allows evaluation on temporal data (timestamp-based data) with a DateTime index.
For evaluation purposes, the MAPE, MAE, RMSE, and R2 values are returned.
}
\examples{
combinedRegressionEvaluation(train_df$DateTime, train_df$Actual, train_df$Predicted, test_df$DateTime, test_df$Actual, test_df$Predicted, title="Evaluation of Regression Model")

}
\seealso{
\code{\link{evaluateRegressionModel}}
}
