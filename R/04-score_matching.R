#' Calculates score matrix for a pair of databases based on edit (Levenshtein) distance
#'
#' @param db1_col - Column from db1 with values to be compared
#' @param db2_col - Column from db2 with values to be compared
#' @param max_score - Max score value (starting score value)
#' @param penalty - Penalty for each increase in edit distance
#' @param n_threads - Number of threads used for matrix calculation
#'
#' @return a sparseMatrix object containing score values
#'
#' @keywords internal
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
#'
#' @keywords internal
calc_exact_score_matrix <- function(db1_col, db2_col, max_score) {
  exact_matrix <- outer(db1_col, db2_col, FUN = "==") + 0
  exact_matrix <- max_score * exact_matrix
  return( convert_to_sparse_matrix(exact_matrix) )
}


#' Get the max score value for each row of the final score matrix
#'
#' @param final_score_matrix - matrix with final score values
#' @param db1_index - db1 index column
#' @param db2_index - db2 index column
#'
#' @details
#' This function uses the base 'apply()' to obtain max_value and max_col_index for each row in the 'final_score_matrix' object.
#'
#' Such approach coerces the 'final_score_matrix' from sparse (dgCMatrix) to a regular (dense) matrix. This can generate warnings regarding vector allocation when working with large datasets.
#'
#' To avoid adding more dependencies to this package, we have chosen to keep this function working with the base 'apply()', since the increased RAM usage caused by this coercion is temporary and won't surpass total RAM usage in the score_matrix calculation.
#'
#' Long story short, if the computer has enough RAM to calculate the score matrices, it will have enough RAM to run this function, so we chose to use only base functions in it.
#'
#' @return a list containing match data
#'
#' @keywords internal
extract_score_matches <- function(final_score_matrix, db1_index, db2_index) {
  db1_ids <- db1_index # db1 index vector
  max_scores <- apply(final_score_matrix, 1, max) # max score value for each row (i.e. for each db1_record)
  db2_pos <- apply(final_score_matrix, 1, which.max) # column position of the max score (i.e. db2 match position)
  db2_ids <- sapply(db2_pos, function(x) db2_index[x]) # db2 match index vector (extracted from db2_index using db2_pos)
  score_matches <- Map(function(vec1, vec2, vec3) { list('db1_id' = vec1,
                                                         'score' = vec2,
                                                         'db2_id' = vec3) },
                       db1_ids, max_scores, db2_ids) # generating match list
  score_matches <- unname(score_matches) #removing names from list
  score_matches <- Filter(function(x) x$score != 0, score_matches) # removing records where score == 0 (all scores below cutoff are converted to 0 by the get_score_matches() function)
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
#' @param ti_penalty - penalty applied for each increment in Title's Levenshtein distance
#' @param ti_max - max score value for Title
#' @param so_penalty - penalty applied for each increment in Source's Levenshtein distance
#' @param so_max - max score value for Source
#' @param au_penalty - penalty applied for each increment in Author's Levenshtein distance
#' @param au_max - max score value for Author
#' @param py_max - max score value for Publication Year
#' @param score_cutoff - minimum final score for a valid match between two documents
#'
#' @return a list containing match data
#'
#' @keywords internal
score_matching <- function(db1, db2, n_threads,
                              ti_penalty, ti_max,
                              so_penalty, so_max,
                              au_penalty, au_max,
                              py_max, score_cutoff) {
  db1 <- subset_db_for_score_match(db1)
  db2 <- subset_db_for_score_match(db2)
  if ( any_empty_dfs(db1, db2) ) {
    print('There is a db with only DOI records')
    return( list(list(), list()) )
  }
  ti_matrix <- calc_distance_score_matrix(db1$TI, db2$TI, max_score = ti_max, penalty = ti_penalty, n_threads = n_threads)
  au_matrix <- calc_distance_score_matrix(db1$AU, db2$AU, max_score = au_max, penalty = au_penalty, n_threads = n_threads)
  so_matrix <- calc_distance_score_matrix(db1$SO, db2$SO, max_score = so_max, penalty = so_penalty, n_threads = n_threads)
  py_matrix <- calc_exact_score_matrix(db1$PY, db2$PY, max_score = py_max)

  final_score_matrix <- ti_matrix + au_matrix + so_matrix + py_matrix
  final_score_matrix[final_score_matrix < score_cutoff] <- 0 #Results below cutoff are converted to 0
  #dimnames(final_score_matrix) <- list(paste0('db1_', db1$index),
  #                                     paste0('db2_',db2$index)) #Adding db indexes to each matrix dimension
  score_matches <- extract_score_matches(final_score_matrix, db1$index, db2$index)
  return(list(score_matches, final_score_matrix))
}
