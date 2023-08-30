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
    dplyr::select(index,TI,PY,J9,AU)
}

#' Converts a matrix to sparseMatrix format
#'
#' @param matrix - A matrix object
#'
#' @description
#' This is a helper function to both calc_distance_score_matrix() and calc_exact_score_matrix()
#'
#'
#' @return a matrix in sparseMatrix format, where negative values have been replaced by 0
# @export
#'
# @examples
convert_to_sparse_matrix <- function(matrix) {
  matrix[is.na(matrix) | matrix < 0] <- 0 # Replacing NA and negative values with zeroes
  return( methods::as(matrix, "sparseMatrix") ) #Returning sparse_matrix (way more memory efficient)
}


#' Calculates score matrix for a pair of databases based on edit (levenshtein) distance
#'
#' @param db1_col - Column from db1 with values to be compared
#' @param db2_col - Column from db2 with values to be compared
#' @param max_score - Max score value (starting score value)
#' @param penalty - Penalty for each increase in edit distance
#' @param n_threads - Number of threads used for matrix calculation
#'
#' @return a sparseMatrix object containing score values
# @export
#'
# @examples
calc_distance_score_matrix <- function(db1_col, db2_col, max_score, penalty, n_threads) {
  stringdist_matrix <- stringdist::stringdistmatrix(db1_col, db2_col, method = "lv", nthread = n_threads)
  stringdist_matrix <- max_score - stringdist_matrix * penalty
  return( convert_to_sparse_matrix(stringdist_matrix) )
}


#' Calculates score matrix for a pair of databases based on exact match
#'
#' @param db1_col - Column from db1 with values to be compared
#' @param db2_col - Column from db2 with values to be compared
#' @param max_score - Max score value
#'
#' @return a sparseMatrix object containing score values
# @export
#'
# @examples
calc_exact_score_matrix <- function(db1_col, db2_col, max_score) {
  exact_matrix <- outer(db1_col, db2_col, FUN = "==") + 0
  exact_matrix <- max_score * exact_matrix
  return( convert_to_sparse_matrix(exact_matrix) )
}




#' Get the max score value for each row of the final score matrix
#'
#' @param final_score_matrix - matrix with final score values
#'
#' @return a list containing match data
# @export
#'
# @examples
get_max_score_matches <- function(final_score_matrix) {
  db1_ids <- as.numeric(rownames(final_score_matrix))
  max_scores <- apply(final_score_matrix, 1, max)
  db2_pos <- apply(final_score_matrix, 1, which.max)
  db2_names <- as.numeric(colnames(final_score_matrix))
  db2_ids <- sapply(db2_pos, function(x) db2_names[x])
  score_matches <- Map(function(vec1, vec2, vec3) { list('db1_id' = vec1,
                                                         'score' = vec2,
                                                         'db2_id' = vec3) },
                       db1_ids, max_scores, db2_ids)
  score_matches <- unname(score_matches)
  score_matches <- Filter(function(x) x$score != 0, score_matches)
  return( score_matches )
}




#' Calculating the final score matrix and extracting the highest scoring matches
#'
#' @description
#' This function calculates each component of the score in a different score matrix, then sums those in order to obtain the final score.
#' Finally, it extracts the highest score match for each row of the final score matrix.
#'
#'
#' @param db1 - First bibliographic database in the comparison
#' @param db2 - Second bibliographic database in the comparison
#' @param n_threads - number of (logical) cores to be used in the matching
#' @param ti_penalty - penalty applied for each increment in Title's levenshtein distance
#' @param ti_max - max score value for Title
#' @param so_penalty - penalty applied for each increment in Source's levenshtein distance
#' @param so_max - max score value for Source
#' @param au_penalty - penalty applied for each increment in Author's levenshtein distance
#' @param au_max - max score value for Author
#' @param py_max - max score value for Publication Year
#' @param score_cutoff - minimum final score for a valid match between two documents
#'
#' @return a list containing match data
# @export
#'
# @examples
get_score_matches <- function(db1, db2, n_threads,
                              ti_penalty, ti_max,
                              so_penalty, so_max,
                              au_penalty, au_max,
                              py_max, score_cutoff) {
  db1 <- subset_db_for_score_match(db1)
  db2 <- subset_db_for_score_match(db2)
  if ( any_empty_dfs(db1, db2) ) {
    print('There is a db with only DOI records')
    return( list() )
  }
  ti_matrix <- calc_distance_score_matrix(db1$TI, db2$TI, max_score = ti_max, penalty = ti_penalty, n_threads = n_threads)
  au_matrix <- calc_distance_score_matrix(db1$AU, db2$AU, max_score = au_max, penalty = au_penalty, n_threads = n_threads)
  so_matrix <- calc_distance_score_matrix(db1$SO, db2$SO, max_score = so_max, penalty = so_penalty, n_threads = n_threads)
  py_matrix <- calc_exact_score_matrix(db1$PY, db2$PY, max_score = py_max)
  final_score_matrix <- ti_matrix + au_matrix + so_matrix + py_matrix
  final_score_matrix[final_score_matrix < score_cutoff] <- 0 #Removing results below cutoff
  dimnames(final_score_matrix) <- list(db1$index, db2$index) #Adding db indexes to each matrix dimension
  score_matches <- get_max_score_matches(final_score_matrix)
  return(list(score_matches, final_score_matrix))
}
