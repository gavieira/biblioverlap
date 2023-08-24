any_empty_dfs <- function(df1, df2){
  empty_dfs <- sapply(list(df1, df2), function(db) nrow(db) == 0)
  return( any(empty_dfs) )
}
