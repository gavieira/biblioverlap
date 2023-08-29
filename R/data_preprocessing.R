#' Processes bibliographic data for the downstream matching procedure
#'
#' @param db_list - list containing a dataframe for each set
#' @param matching_fields - list containing the names of the columns used in the matching procedure
#'
#' @return  a list containing a (modified) dataframe for each set
#  @export
#'
#  @examples
data_preprocessing <- function(db_list, matching_fields) {
  preprocessed_data <- lapply(db_list, function(db) {
    row.names(db) <- NULL #Removing the default rownames added by bibliometrix's convert2db function
    db %>%
      dplyr::select({{ matching_fields$doi }}, {{ matching_fields$title }}, {{ matching_fields$pubyear }},
                    {{ matching_fields$authors }}, {{ matching_fields$source }} ) %>%
      dplyr::mutate(AU = stringr::str_extract({{matching_fields$authors}}, "^[^;]+") ) %>% #Getting only the first author from the 'AU' column; regex extracts the substring up to the first ";"
      dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) %>% #Converting all character fields to uppercase
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>% #Converting all character fields to US-ASCII (removing accentuation)
      dplyr::mutate_all(trimws) %>% #Removing whitespace from all values
      dplyr::mutate_all(~ifelse(. == "" | is.null(.), NA, .) ) %>% #Converting empty/null values to NA
      dplyr::mutate(score = 0) %>% #This column will be used to exclude documents that have already been matched to another database from subsequent comparisons, reducing execution time
      #mutate(UUID = sapply(1:nrow(db), UUIDgenerate), .before = DI) %>% #Generating UUIDs for each row
      dplyr::filter(!duplicated(matching_fields$doi, incomparables = NA))%>% #Keeping only unique DOIs (some bases have duplicated DOIs)
      tibble::rownames_to_column(var = 'index') %>% #Adding a column with the index of row
      dplyr::mutate(UUID = sapply(1:dplyr::n(), uuid::UUIDgenerate), .before = {{ matching_fields$doi}}) #Generating UUIDs for each row
  } )
  return(preprocessed_data)
}
