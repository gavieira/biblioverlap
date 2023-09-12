#' @title Take two dataframes and checks if any of them are empty
#'
#' @description
#' This function takes a pair of dataframes as parameters and checks if any of them are empty (i.e. have 0 rows).
#'
#' If at least one of the dataframes are empty, returns TRUE. Else, returns FALSE.
#'
#' It is intended to help the document matching functions deal with datasets that have only DOI (or DOIless) records.
#'
#' @param df1 - A data.frame object
#' @param df2 - A second data.frame object
#'
#' @return a boolean value
# @export
#'
# @examples
#' df1 <- data.frame(A = sample(1:10)) #non-empty dataframe
#' df2 <- data.frame() #empty dataframe
#' df3 <- data.frame(A = c('test1', 'test2')) #non-empty dataframe
#' any_empty_dfs(df1, df2) #TRUE, as df2 is empty
#' any_empty_dfs(df1, df3) #FALSE, as there are no empty dfs in the arguments

any_empty_dfs <- function(df1, df2){
  empty_dfs <- sapply(list(df1, df2), function(db) nrow(db) == 0)
  return( any(empty_dfs) )
}



#' Subsetting bibliographic database records to use in matching procedures
#'
#' @param db - dataframe containing all fields from the bibliographic database records
#'
#' @return a subset of the database containing only relevant fields for the doi matching procedure
# @export
#'
# @examples
subset_db_for_doi_match <- function(db) {
  db %>%
    dplyr::filter(!is.na(DI) & score < 1) %>%
    dplyr::select(index,DI)
}


#' Subsetting bibliographic database records to use in matching procedures
#'
#' @param df - a dataframe from a bibliographic database
#'
#' @return a subset df with all needed data for the specified matching procedure
# @export
#'
# @examples
subset_db_for_score_match <- function(df) {
  df %>%
    dplyr::filter(is.na(DI) & score < 1) %>%
    dplyr::select(index,TI,PY,SO,AU)
}



#' Converts a matrix to sparseMatrix format - used in score matching procedure
#'
#' @param matrix - A matrix object
#'
#' @description
#' This is a helper function to both calc_distance_score_matrix() and calc_exact_score_matrix()
#'
#'
#' @return a matrix in sparseMatrix format, where negative values have been replaced by 0
#' @export
#'
# @examples
convert_to_sparse_matrix <- function(matrix) {
  matrix[is.na(matrix) | matrix < 0] <- 0 # Replacing NA and negative values with zeroes
  return( Matrix::Matrix(matrix, sparse = TRUE) ) #Returning sparse_matrix (way more memory efficient)
}