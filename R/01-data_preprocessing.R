#' Removes duplicates from each bibliographic dataset
#'
#' @param db_list - input list containing a dataframe for each set
#' @param matching_fields - list containing the names of the columns to be used in the matching procedure
#'
#' @return  a list containing a (duplicate-free) dataframe for each set
#'
#' @noRd
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
#' @return  a list containing a (pre-processed) dataframe for each set
#'
#' @noRd
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
      dplyr::mutate(index = 1:nrow(.)) %>%
      dplyr::mutate(UUID = sapply(1:dplyr::n(), uuid::UUIDgenerate)) %>% #Generating UUIDs for each row
      dplyr::relocate(index, UUID, DI, TI, PY, AU, SO, score)

  } )
  return(preprocessed_data)
}


#' Merge multiple input files from the same source
#'
#' @details
#' It is fairly common to retrieve data from a single bibliographic database in small chunks. Thus, this function is designed to merge multiple files from the same source into a single file while also removing duplicate records.
#'
#'
#' @param input_files - an array containing the path to all input files
#' @param sep - field separator. Default: comma (',')
#' @param quote - quote type used for character fields. Default: Double quotes ('"')
#'
#' @return a single dataframe with all unique records from the input files
#' @export
#'
#' @examples
#'
#' ## Generating tempfiles
#' tempfile1 <- tempfile(fileext = ".csv")
#' tempfile2 <- tempfile(fileext = ".csv")
#' write.csv(ufrj_bio_0122$Biochemistry, file = tempfile1, row.names = FALSE)
#' write.csv(ufrj_bio_0122$Genetics, file = tempfile2, row.names = FALSE)
#'
#' ## Testing function
#' merged_files <- merge_input_files(c(tempfile1, tempfile2))
#' dim(merged_files)
#' head(merged_files)
#'
merge_input_files <- function(input_files,
                              sep = ",",
                              quote = '"') {
  df_list <- lapply(input_files, function(input_file) {
    utils::read.csv(input_file,
             sep = sep,
             quote = quote,
             strip.white = TRUE,
             check.names = FALSE) })
  tryCatch({
    df <- do.call(rbind, df_list)
  }, error = function(err) {
    stop('Failed to merge files. Are they from the same database and/or have the same columns?') }
  )
  df[] <- lapply(df, function(col) { #Cleaning data (one column at a time)
    col <- trimws(as.character(col)) # Removing leading and trailing whitespaces
    col[which(col == "" | is.null(col))] <- NA  # Convert empty or null values to NA
    return(col)
  })
  df <- df[!duplicated(df), ]   # Removing duplicate records

  return( df )
}
