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
