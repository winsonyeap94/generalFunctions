#' Improved Correlation Plotting
#'
#' Improvised correlation plotting which includes ordering of variables, text angle adjustment, significnat level t-test, and
#' font size adjustment.
#'
#' Runs using the \code{corrplot::corrplot()} function.
#'
#' @import corrplot
#'
#' @param correlation_matrix Correlation matrix obtained from \code{stats::cor()}.
#' @param order_type Character, the ordering method of the correlation matrix.
#' \itemize{
#'  \item "original" for original order.
#'  \item "AOE" for angular order of the eigenvectors.
#'  \item "FPC" for first principal component order.
#'  \item "hclust" for hierarchical clustering order (default).
#'  \item "alphabet" for alphabetical order.
#' }
#' @param text_angle Numeric, angle (degrees) for the text to be adjusted for better visualisation. Defaults to 45.
#' @param sig_level Numeric, significant level for p-value to be considered significant, whereby it would be coloured. Insignifcant
#'  p-values would not be coloured. Defaults to 0.05.
#' @param number_font_size Numeric, font size. Defaults to 1.
#'
#' @return None
#'
#' @examples
#' cor_mat <- cor(data)
#' plot_correlation(cor_mat)
#'
#' @export

plot_correlation <- function(correlation_matrix, order_type = "hclust", text_angle = 45, sig_level = 0.05, number_font_size = 1){
  # Matrix of the p-value of the correlation
  corr_pval_test <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    return(p.mat)
  }
  pval_matrix <- corr_pval_test(correlation_matrix)
  # Plotting correlation
  color_palette <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(correlation_matrix, order = order_type, method="color", col = color_palette(200),
           addCoef.col = "black", addCoefasPercent = TRUE, diag = TRUE, # addCoef.col: Adds coefficient of correlation
           p.mat = pval_matrix, sig.level = sig_level, insig = "blank", # Color only those with significant p-value
           tl.col = "black", tl.srt = text_angle,
           number.font = number_font_size)
}
