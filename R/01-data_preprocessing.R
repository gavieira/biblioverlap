
#' Title
#'
#' @param db_list - blabla
#' @param matching_fields -blabla
#'
#' @return blabla
#' @export
#'
# @examples
generating_db_col <- function(db_list, matching_fields) {
  for (i in names(db_list)) {
    db_list[[i]]$DB <- i
  }
  return(db_list)
}

#select_stuff <- function(db_list, matching_fields) {
#  lapply(db_list, function(db) {
#  db %>%
#    dplyr::select(dplyr::all_of(unname(unlist(matching_fields)))) %>%
#    dplyr::mutate_all(trimws) %>% #Removing whitespace from all values
#    dplyr::mutate_all(~ifelse(. == "" | is.null(.), NA, .) ) #Converting empty/null values to NA
#    })
#}


removing_duplicates <- function(db_list, matching_fields) {
  doi_col <- dplyr::sym(matching_fields$DI)
  no_dups <- lapply(db_list, function(db) {
    #db <- as.data.frame(db)
    db %>%
      dplyr::distinct() %>% #Removing all rows that are completely identical in the same database
      dplyr::mutate(dplyr::across(!!doi_col, trimws)) %>% #Removing whitespace from doi column
      dplyr::mutate(dplyr::across(!!doi_col, ~ ifelse(. == "" | is.null(.), NA, .) ) ) %>% #Converting empty/null values to NA from doi column
      dplyr::filter(!duplicated(!!doi_col, incomparables = NA)) #Keeping only unique DOIs (duplicated DOIs are not uncommon)
    return(db)
  })
  return (no_dups)
}


#' Processes bibliographic data for the downstream matching procedure
#'
#' @param db_list - list containing a dataframe for each set
#' @param matching_fields - list containing the names of the columns used in the matching procedure
#'
#' @return  a list containing a (modified) dataframe for each set
#' @export
#'
#  @examples
data_preprocessing <- function(db_list, matching_fields) {
  preprocessed_data <- lapply(db_list, function(db) {
    #row.names(db) <- NULL #Removing the default rownames (useful for records from bibliometrix's convert2db function)
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
      #dplyr::bind_cols(index = 'index') %>% #Adding a column with the index of row
      dplyr::mutate(UUID = sapply(1:dplyr::n(), uuid::UUIDgenerate)) %>% #Generating UUIDs for each row
      dplyr::relocate(index, UUID, DI, TI, PY, AU, SO, score)

  } )
  return(preprocessed_data)
}

#data_preprocessing <- function(db_list, matching_fields) {
#  preprocessed_data <- lapply(db_list, function(db) {
#    row.names(db) <- NULL #Removing the default rownames added by bibliometrix's convert2db function
#    db %>%
#      #dplyr::select( !!{{ matching_fields }} )
#      #dplyr::select(!!dplyr::sym(matching_fields[['doi']]))
#      dplyr::select(dplyr::all_of(unname(unlist(matching_fields)))) %>%
#      #dplyr::select(!!{{ matching_fields$doi }}, !!{{ matching_fields$title }}, !!{{ matching_fields$pubyear }},
#                    #!!{{ matching_fields$authors }}, !!{{ matching_fields$source }} ) %>%
#      #dplyr::mutate(AU = stringr::str_extract( !!{{ matching_fields$authors }}, "^[^;]+") ) %>% #Getting only the first author from the 'AU' column; regex extracts the substring up to the first ";"
#      #dplyr::rowwise() %>%
#      #dplyr::mutate(Ar = sub(";.*$", "", AU ) ) %>% #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
#      #dplyr::mutate(AU = sub(";.*$", "", get(matching_fields$authors))) %>% #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
#      dplyr::mutate(AU = sub(";.*$", "", !!dplyr::sym(matching_fields$authors))) %>% #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
#      #dplyr::mutate(Author = sub(";.*$", "", !!{{ matching_fields$authors }}) ) %>% #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
#      dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) %>% #Converting all character fields to uppercase
#      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>% #Converting all character fields to US-ASCII (removing accentuation)
#      dplyr::mutate_all(trimws) %>% #Removing whitespace from all values
#      dplyr::mutate_all(~ifelse(. == "" | is.null(.), NA, .) ) %>% #Converting empty/null values to NA
#      dplyr::mutate(score = 0) %>% #This column will be used to exclude documents that have already been matched to another database from subsequent comparisons, reducing execution time
#      ##dplyr::mutate(UUID = sapply(1:nrow(db), uuid::UUIDgenerate), .before = matching_fields$doi) %>% #Generating UUIDs for each row
#      dplyr::filter(!duplicated(!!{{matching_fields$doi}}, incomparables = NA)) %>% #Keeping only unique DOIs (some bases have duplicated DOIs)
#      tibble::rownames_to_column(var = 'index') %>% #Adding a column with the index of row
#      dplyr::mutate(UUID = sapply(1:dplyr::n(), uuid::UUIDgenerate), .before = !!{{ matching_fields$doi }}) #Generating UUIDs for each row
#  } )
#  return(preprocessed_data)
#}
