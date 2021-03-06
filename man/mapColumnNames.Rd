% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapColumnNames.R
\name{mapColumnNames}
\alias{mapColumnNames}
\title{Mapping column names of a dataframe to a dictionary dataframe.}
\usage{
mapColumnNames(input_df, map_df, existing_colname, new_colname)
}
\arguments{
\item{input_df}{Dataframe object to have its column names remapped.}

\item{map_df}{Dictionary dataframe which consists of two columns (existing and new colnames to be converted to).}

\item{existing_colname}{String input of the column name in \code{map_df} that tells which column refers to the
existing column name.}

\item{new_colname}{String input of the column name in \code{map_df} that tells which column refers to the new
column name to be mapped to.}
}
\value{
Dataframe with column names remapped to new column names.
}
\description{
Mapping column names of a dataframe to a dictionary dataframe.
}
\examples{
data_df <- mapColumnNames(data_df, dictionary_df, "Existing_Colnames", "New_Colnames")
}
