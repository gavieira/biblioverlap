


#' Updating db2 matched records
#'
#' @param db1 - First bibliographic database in the comparison
#' @param db2 - Second bibliographic database in the comparison
#' @param match_list - List containing all matching documents between db1 and db2
#'
#' @return db2 with updated uuid and score values
# @export
#'
# @examples
update_db2_matches <- function(db1, db2, match_list) {
  for (match in match_list) {
    db1_index <- match$db1_id
    db2_index <- match$db2_id
    score <- match$score
    db2[db2_index,'score'] <- score #Updating the score value of the record
    db2[db2_index,'UUID'] <- db1[db1_index,'UUID'] #Inheriting uuid from db1's matching record
  }
  return(db2)
}




inherit_uuid_col <- function(db_list, internal_db_list) {
  for (name in names(db_list)) {
    db_list[[name]]$UUID <- internal_db_list[[name]]$UUID
  }
  return(db_list)
}



#' My Custom List Object
#'
#' This is an example list object containing sample data.
#'
#' @format A named list.
#' @field doi Description of the field.
#' @field title Description of the field.
#' @field pubyear Description of the field.
#' @field authors Description of the field.
#' @field source Description of the field.
#'
#' @export
matching_fields <- list(DI = 'DI',
                      TI = 'TI',
                      PY = 'PY',
                      AU = 'AU',
                      SO = 'SO')
#matching_fields <- list(DI = 'Lens.ID',
#                      TI = 'Title',
#                      PY = 'Publication.Year',
#                      AU = 'Author.s',
#                      SO = 'Source.Title')


#' Obtains document overlap between databases from a named list
#'
#' @param db_list - list of dataframes containing the sets of bibliographic data
#' @param db_order - order of the databases
#' @param matching_fields - Column names used in the matching
#' @param n_threads - number of (logical) cores used in the matching procedures
#' @param ti_penalty - penalty applied for each increment in Title's levenshtein distance
#' @param ti_max - max score value for Title
#' @param so_penalty - penalty applied for each increment in Source's levenshtein distance
#' @param so_max - max score value for Source
#' @param au_penalty - penalty applied for each increment in Author's levenshtein distance
#' @param au_max - max  score value for Author
#' @param py_max - max score value for Publication Year
#' @param score_cutoff - minimum final score for a valid match between two documents
#'
#' @return a modified version of db_list where matching documents share the same UUID
#' @export
#'
# @examples
biblioverlap <- function(db_list, db_order = names(db_list), matching_fields = matching_fields, n_threads = parallel::detectCores(),
                        ti_penalty = 0.1, ti_max = 0.6,
                        so_penalty = 0.1, so_max = 0.3,
                        au_penalty = 0.1, au_max = 0.3,
                        py_max = 0.3, score_cutoff = 1) {
  #db_list <- lapply(db_list, function(db) db %>% rownames_to_column(var = 'index') ) #Creating column that will keep the index of the original db row even when splitting the data for doi and score matching
  db_list <- removing_duplicates(db_list, matching_fields)
  internal_db_list <- data_preprocessing(db_list, matching_fields)
  combs <- utils::combn(db_order, 2) #Getting ordered db pairwise combinations
  matches <- list()
  score_matrices <- list()
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll match the documents based on DOI and score, and then modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each list of matches
    db1 <- internal_db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- internal_db_list[[db2_name]] #Getting db2 by name from the db_list
    print(paste('Matching by DOI for pair', comb_name))
    doi_matches <- doi_matching(db1, db2, n_threads = n_threads) #Obtaining matches by DOI
    print(paste('Matching by SCORE for pair', comb_name))
    score_matches <- score_matching(db1, db2, n_threads = n_threads,
                                       ti_penalty, ti_max,
                                       so_penalty, so_max,
                                       au_penalty, au_max,
                                       py_max, score_cutoff) #Obtaining matches by score
    final_score_matrix <- score_matches[[2]]
    score_matches <- score_matches[[1]]
    print('Updating matched documents in db2')
    #matches[[comb_name]] <- c(doi_matches, score_matches)
    all_matches <- c(doi_matches, score_matches)
    score_matrices[[comb_name]] <- final_score_matrix
    matches[[comb_name]] <- lapply(all_matches, function(lst) {
      lst$db1 <- db1_name
      lst$db2 <- db2_name
      return(lst) } )
    internal_db_list[[db2_name]] <- update_db2_matches(db1, db2, matches[[comb_name]]) #Saving modified db2 to db_list
    summary <- get_matching_summary(internal_db_list)
  }
  db_list <- inherit_uuid_col(db_list, internal_db_list)
  summary <- get_matching_summary(internal_db_list)
  return (list(db_list = db_list,
               summary = summary,
               internal_db_list = internal_db_list,
               matches = matches,
               score_matrices = score_matrices)) #Returning db_list and matches
}
