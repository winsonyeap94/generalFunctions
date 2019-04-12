#' Mapping column names of a dataframe to a dictionary dataframe.
#'
#' @param input_df Dataframe object to have its column names remapped.
#' @param map_df Dictionary dataframe which consists of two columns (existing and new colnames to be converted to).
#' @param existing_colname String input of the column name in \code{map_df} that tells which column refers to the
#'     existing column name.
#' @param new_colname String input of the column name in \code{map_df} that tells which column refers to the new
#'     column name to be mapped to.
#' @return Dataframe with column names remapped to new column names.
#' @examples
#' data_df <- mapColumnNames(data_df, dictionary_df, "Existing_Colnames", "New_Colnames")
#' @export

mapColumnNames <- function(input_df, map_df, existing_colname, new_colname){
  colnames(input_df)[!is.na(match(colnames(input_df), map_df[[existing_colname]]))] <-
    as.character(map_df[[new_colname]][match(colnames(input_df), map_df[[existing_colname]])[!is.na(match(colnames(input_df), map_df[[existing_colname]]))]])
  return(input_df)
}
