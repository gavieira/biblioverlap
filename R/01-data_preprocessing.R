#' Removes duplicates from each bibliographic dataset
#'
#' @param db_list - input list containing a dataframe for each set
#' @param matching_fields - list containing the names of the columns to be used in the matching procedure
#'
#' @return  a list containing a duplicate-free dataframe for each set of bibliographic data
#'
removing_duplicates <- function(db_list, matching_fields) {
  doi_col <- dplyr::sym(matching_fields$DI)
  no_dups <- lapply(db_list, function(db) {
    db <- db[!duplicated(db), ] # Identify and remove duplicate rows
    db %>%
      dplyr::mutate(dplyr::across(!!doi_col, trimws)) %>% #Removing whitespace from doi column
      dplyr::mutate(dplyr::across(!!doi_col, ~ ifelse(. == "" | is.null(.), NA, .) ) ) %>% #Converting empty/null values to NA from doi column
      dplyr::filter(!duplicated(!!doi_col, incomparables = NA)) #Keeping only unique DOIs (duplicated DOIs are common - though not frequent - in bibliographical databases)
    return(db)
  })
  return (no_dups)
}


#' Processes bibliographic data for the downstream matching procedure
#'
#' @param db_list - list containing a dataframe for each set
#' @param matching_fields - list containing the names of the columns to be used in the matching procedure
#'
#' @return  a list containing a (modified) dataframe for each set
#'
data_preprocessing <- function(db_list, matching_fields) {
  preprocessed_data <- lapply(db_list, function(db) {
    db %>%
      dplyr::select( dplyr::all_of(unlist( matching_fields )) ) %>% #Retrieves only relevant fields and automatically renames them to the list names (WoS' field names)
      dplyr::mutate(AU = sub(";.*$", "", AU)) %>% #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
      dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) %>% #Converting all character fields to uppercase
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>% #Converting all character fields to US-ASCII (removing accentuation) - IT EXPECTS UTF-8!!!!!!
      dplyr::mutate_all(trimws) %>% #Removing whitespace from all values
      dplyr::mutate_all(~ifelse(. == "" | is.null(.), NA, .) ) %>% #Converting empty/null values to NA
      dplyr::mutate(score = 0) %>% #This column will be used to exclude documents that have already been matched to another database from subsequent comparisons, reducing execution time
      #tibble::rownames_to_column(var = 'index') %>% #Adding a column with the index of row
      dplyr::mutate(index = 1:nrow(.)) %>%
      dplyr::mutate(UUID = sapply(1:dplyr::n(), uuid::UUIDgenerate)) %>% #Generating UUIDs for each row
      dplyr::relocate(index, UUID, DI, TI, PY, AU, SO, score)

  } )
  return(preprocessed_data)
}
