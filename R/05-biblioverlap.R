#' Check if user has input valid dataset names
#'
#' @description
#' Function that checks if dataset names (provided as names to elements of db_list) are valid (not NULL, NA or empty string.
#' If all names are valid, returns them. Throws an error otherwise.
#'
#'
#' @param db_list - db_list used as input for [biblioverlap]
#' @param matching_fields - column names used in the matching procedure by [`biblioverlap`]
#'
#' @return a boolean value. TRUE if check succeeds, otherwise FALSE.
#'
#' @noRd
#'
check_dataset_names <- function(db_list) {
  names <- names(db_list)
  if (is.null(names) || any(sapply(names, function(x) x == "" | is.na(x) | is.null(x)))) {
    stop("Invalid value for dataset names. Please correct it and resubmit your data.")
  }
  return(names)
}

#' Check if user has input column names present in all datasets
#'
#' @description
#' Function that checks if colnames (provided through the matching_fields argument of [`biblioverlap`]) are present in all datasets
#' Raises an error if check fails
#'
#'
#' @param db_list - db_list used as input for [biblioverlap]
#' @param matching_fields - db_list used as input for [biblioverlap]
#'
#' @return dataset names
#'
#' @noRd
#'
check_dataset_cols <- function(db_list, matching_fields) {
  column_names <- unlist(matching_fields)
  lapply(1:length(db_list), function(index) {
    all_present <- all(column_names %in% colnames( db_list[[index]] ))

    if (!all_present) {
      stop(paste0("Not all specified columns are present in the dataframe '", names(db_list)[index],"'."))
    }

    return(all_present)
  })
}


#' Updating db2 matched records
#'
#' @param db1 - First bibliographic database in the comparison
#' @param db2 - Second bibliographic database in the comparison
#' @param match_list - List containing all matching documents between db1 and db2
#'
#' @return db2 with updated uuid and score values
#'
#' @noRd
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



#' Adding UUID column to original data provided by user
#'
#' @description
#' This is a helper function to [biblioverlap::biblioverlap()] that extracts the UUID column from the internal data used for document matching and adds it to the original data provided by the user.
#'
#' @param unique_db_list  - db_list after duplicate cleaning process by [biblioverlap::removing_duplicates()]
#' @param internal_db_list - internal_db_list used by [biblioverlap::biblioverlap()] for document matching
#'
#' @return a db_list with unique records that
#'
#' @noRd
inherit_uuid_col <- function(unique_db_list, internal_db_list) {
  for (name in names(unique_db_list)) {
    unique_db_list[[name]]$UUID <- internal_db_list[[name]]$UUID
  }
  return(unique_db_list)
}


#' Obtaining a table summary of biblioverlap's matching results
#'
#' @param internal_db_list - internal_db_list used by [biblioverlap::biblioverlap()] for document matching
#'
#' @return a table summary  of the matching procedure results
#' @importFrom rlang .data
#'
#' @noRd
get_matching_summary_df <- function(internal_db_list) {
  #Getting dataframes
  all_data <- do.call(rbind, internal_db_list) #Saving all data into a single df
  matched_data <-  all_data %>%
    dplyr::filter(duplicated(.data$UUID)) %>% #Filtering only rows with duplicated UUID (docs with match)
    dplyr::distinct(.data$UUID, .keep_all = TRUE) #And then obtaining only one (the first) occurrence of each matched document
  #Getting values
  summary <- list()
  summary$total <- nrow(all_data)
  summary$distinct <- nrow(all_data %>% dplyr::distinct(.data$UUID, .keep_all = TRUE))
  summary$overlap <- summary$total - summary$distinct
  summary$matched <- nrow(matched_data)
  summary$unmatched <- summary$distinct - summary$matched
  summary$matched_id <- nrow(matched_data %>% dplyr::filter(!is.na(DI)))
  summary$matched_score <- nrow(matched_data %>% dplyr::filter(is.na(DI))) #USES DI column
  summary_df <- data.frame(doc_subset = names(summary), n_docs = unlist(summary), row.names = NULL)
  #Getting dataframe
  categories <- c('total', 'distinct/overlap', 'distinct/overlap', 'distinct', 'distinct', 'matched', 'matched')
  doc_subset_levels <- c('total',  'overlap', 'distinct', 'unmatched', 'matched',  'matched_id', 'matched_score' )
  final_summary_df <- summary_df %>%
    dplyr::mutate("doc_subset" = factor(.data$doc_subset, levels = doc_subset_levels)) %>%
    dplyr::mutate("category" = factor(categories, levels = unique(categories)), .after = .data$doc_subset) %>%
    dplyr::group_by(.data$category) %>%
    dplyr::mutate("perc_inside_category" = round(.data$n_docs / sum(.data$n_docs) * 100, 1))
  return(final_summary_df)
}


#' Document-level matching of bibliographic datasets
#'
#'
#' @description
#' This function identifies document overlap between bibliographic datasets and records it through the use of Universally Unique Identifiers (UUID).
#'
#'
#' @param db_list - named list of dataframes containing the sets of bibliographic data. Names are used to identify sets throughout the matching process.
#' @param matching_fields - Five column names used in the matching. Should be universal across all datasets and provided as a named list with the following names: **DI** (unique identifier), **TI** (document title), **PY** (publication year), **SO** (publication source) and **AU** (Authors). Default values come from [The Lens scholar field definition](https://support.lens.org/knowledge-base/scholar-field-definition/).
#' @param n_threads - number of (logical) cores used in the matching procedures. Default: 1
#' @param ti_max - max score value for Title. Default: 0.6
#' @param ti_penalty - penalty applied for each increment in Title's Levenshtein distance. Default: 0.1
#' @param so_max - max score value for Source. Default: 0.3
#' @param so_penalty - penalty applied for each increment in Source's Levenshtein distance. Default: 0.1
#' @param au_max - max  score value for Author. Default: 0.3
#' @param au_penalty - penalty applied for each increment in Author's Levenshtein distance. Default: 0.1
#' @param py_max - max score value for Publication Year. Default: 0.3
#' @param score_cutoff - minimum final score for a valid match between two documents. Default: 1
#'
#' @return a list object containing:
#'
#' (i) `db_list`: a modified version of db_list where matching documents share the same UUID
#'
#' (ii) `summary`: a summary of the results of the matching procedure
#'
#' @details
#' In this procedure, any duplicates in the same dataset are removed. Then, Universally Unique Identifiers (UUID) are attributed to each record. If a match is found between two documents in a pairwise comparison, the UUID of the record from the first dataset is copied to the record on the second.
#'
#' All preprocessing and modifications to the dataset are performed in a copy of the original data, which is used internally by the program. After all pairwise comparisons are completed, the UUID data is added as a new column in the original data.
#'
#' Thus, the `db_list` returned by this function contains the same fields provided by the user plus the UUID column with the overlap information. This allows for further analysis using other fields (e.g. 'number of citations' or 'document type').
#'
#' @note
#' In its internal data, the program will attempt to split the AU (Author) field to extract only the first author, for which it will calculate the Levenshtein distance.
#'
#' It assumes that the AU field is ";" (semicolon) separated. Thus, in order to correctly perform the matching procedure to when another separator is being applied to this field, the user can either: (i) change the separator to semicolon; or (ii) create a new column containing only the first author.
#'
#'
#' @export
#'
#' @examples
#' #Example list of input dataframes
#' lapply(ufrj_bio_0122, head, n=1)
#'
#' #List of columns for matching (identical to biblioverlap()'s defaults)
#' matching_cols <- list(DI = 'DOI',
#'                       TI = 'Title',
#'                       PY = 'Publication Year',
#'                       AU = 'Author/s',
#'                       SO = 'Source Title')
#'
#' #Running document-level matching procedure (first two dataframes)
#' biblioverlap_results <- biblioverlap(ufrj_bio_0122[1:2], matching_fields = matching_cols)
#'
#' #Taking a look at the matched db_list
#' lapply(biblioverlap_results$db_list, head, n=1)
#'
#' #Taking a look at the matching results summary
#' biblioverlap_results$summary
#'
biblioverlap <- function(db_list, matching_fields = default_matching_fields, n_threads = 1,
                        ti_penalty = 0.1, ti_max = 0.6,
                        so_penalty = 0.1, so_max = 0.3,
                        au_penalty = 0.1, au_max = 0.3,
                        py_max = 0.3, score_cutoff = 1) {
  #db_list <- lapply(db_list, function(db) db %>% rownames_to_column(var = 'index') ) #Creating column that will keep the index of the original db row even when splitting the data for doi and score matching
  check_dataset_cols(db_list, matching_fields) #Checking if datasets have the colnames specified in 'matching fields'
  db_names <- check_dataset_names(db_list) #db_list dataset names
  db_list <- removing_duplicates(db_list, matching_fields)
  internal_db_list <- data_preprocessing(db_list, matching_fields)
  combs <- utils::combn(db_names, 2) #Getting ordered db pairwise combinations
  matches <- list()
  score_matrices <- list()
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll match the documents based on DOI and score, and then modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each list of matches
    db1 <- internal_db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- internal_db_list[[db2_name]] #Getting db2 by name from the db_list
    message(paste('Matching by DOI for pair', comb_name))
    doi_matches <- doi_matching(db1, db2, n_threads = n_threads) #Obtaining matches by DOI
    message(paste('Matching by SCORE for pair', comb_name))
    score_matches <- score_matching(db1, db2, n_threads = n_threads,
                                       ti_penalty, ti_max,
                                       so_penalty, so_max,
                                       au_penalty, au_max,
                                       py_max, score_cutoff) #Obtaining matches by score
    final_score_matrix <- score_matches[[2]]
    score_matches <- score_matches[[1]]
    message('Updating matched documents in db2')
    all_matches <- c(doi_matches, score_matches)
    score_matrices[[comb_name]] <- final_score_matrix
    matches[[comb_name]] <- lapply(all_matches, function(lst) {
      lst$db1 <- db1_name
      lst$db2 <- db2_name
      return(lst) } )
    internal_db_list[[db2_name]] <- update_db2_matches(db1, db2, matches[[comb_name]]) #Saving modified db2 to db_list
    summary <- get_matching_summary_df(internal_db_list)
  }
  db_list <- inherit_uuid_col(db_list, internal_db_list)
  final_db_list <- list(db_list = db_list,
                        summary = summary) #Getting db_list and summary into final results
  #final_db_list$internal_db_list <- internal_db_list #For debugging
  #final_db_list$matches <- matches #For debugging
  #final_db_list$score_matrices <- score_matrices #For debugging
  return (final_db_list) #Returning db_list and summary
}


#' Merge biblioverlap's results into a single dataframe
#'
#' @param db_list - list of matched dataframes (with UUID column added by [`biblioverlap`])
#' @param filter_distinct - boolean value determining whether to return only the subset containing distinct records (TRUE) or to keep overlapping records between datasets (FALSE). Default: FALSE
#'
#' @return a single dataframe containing data from db_list, featuring an additional 'SET_NAME' column to indicate from which dataset each record came
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'
#' #Running document-level matching procedure for two datasets
#' biblioverlap_results <- biblioverlap(ufrj_bio_0122[1:2])
#'
#' #Obtaining the results as a single dataframe (including overlapping records)
#' all_data <- merge_results(biblioverlap_results$db_list)
#'
#' #Checking number of total rows and overlapping documents are in the dataframe
#' nrow(all_data)
#' sum(duplicated(all_data$UUID))
#'
#' #Obtaining only unique records as a single dataframe
#' distinct_data <- merge_results(biblioverlap_results$db_list, filter_distinct = FALSE)
#'
#' #Checking number of total rows and overlapping documents are in the dataframe
#' nrow(distinct_data)
#' sum(duplicated(distinct_data$UUID))
#'
merge_results <- function(db_list, filter_distinct = FALSE){
  db_list <- lapply(db_list, function(df) dplyr::mutate_all(df, as.character))
  df <- dplyr::bind_rows(db_list, .id =  'SET_NAME') #Joining all info in a single table, while also adding a new column (SET_NAME) with the name of the set that record comes from
  columns_to_front <- c("SET_NAME", "UUID") # Specifying the names of the columns to be moved to the front
  df <- df[c(columns_to_front, setdiff(names(df), columns_to_front))] # Rearrange columns
  if (filter_distinct) {
    df <- dplyr::distinct(df, .data$UUID, .keep_all = TRUE)
  }

  return(df)
}
