#' Partial Forward-Filling
#'
#' Wrapper function around the \code{zoo::na.locf} function which only forward fills until last observation.
#' This is useful to prevent tail NAs from being forward filled.
#'
#' @import zoo
#'
#' @param x Vector object to be forward-filled until last observation.
#'
#' @return Vector of forward-filled data (until last observation).
#'
#' @examples
#' nalocf_partial(x)
#' data_df <- sapply(data_df, function(x) nalocf_partial(x))
#'
#' @export

nalocf_partial <- function(x){
  if ((sum(is.na(x), na.rm=TRUE) >= 1) & (sum(is.na(x), na.rm=TRUE) != length(x))){
    last_idx <- last(which(!is.na(x)))
  } else {
    last_idx <- length(x)
  }
  x[seq(1, last_idx)] <- zoo::na.locf(x[seq(1, last_idx)], na.rm=FALSE)
  return(x)
}
