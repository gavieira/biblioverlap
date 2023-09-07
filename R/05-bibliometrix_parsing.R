#' Wrapper function to call bibliometrix's convert2df() for a given set of records
#'
#' @param db_data - list containing 3 elements: file (either a single file or vector with multiple files containing bibliographic records), dbsource (bibliographic source), and format (file format)
#'
#' @return a database featuring all records from the files specified, formatted according to bibliometrix fields
#' @seealso [bibliometrix::convert2df()]
#'
# @export
#'
# @examples
convert2df_wrapper <- function(db_data) {
  bibliometrix_df <- bibliometrix::convert2df(db_data$file,
                                dbsource = db_data$dbsource,
                                format = db_data$format )
  return(bibliometrix_df)
}



#' Function to convert list of database files into db_lists using bibliometrix
#'
#' @param dataset_list - a list of the files in each dataset. Each is a list containing 3 elements: *file* (either a single file or vector with multiple files containing bibliographic records), *dbsource* (bibliographic source), and *format* (file format)
#' Example: datasets <- list(dimensions = list(file = 'path/to/dimensions.csv', dbsource = 'dimensions', format = 'csv'), scopus = list(file = c('path/to/scopus/1.bib', 'path/to/scopus/2.bib'), dbsource = 'scopus', format = 'bibtex'), wos = list(file = c('path/to/wos/1.bib','path/to/wos/2.bib'), dbsource = 'wos', format = 'bibtex'), 'lens' = list(file = 'path/to/lens.csv', dbsource = 'lens', format = 'csv') )
#'
#'
#' @param n_threads - number of (logical) cores used in the parsing process. By default, uses the number of cores dectected by parallel:detectCores(). Internally, however, this parameter will not exceed the number datasets provided, since going above this threshold would not increase performance.
#'
#'
#' @return a db_list
#' @export
#'
# @examples
bibliometrix_parsing <- function(dataset_list, n_threads = parallel::detectCores() ) {
  n_sets <- length(dataset_list)
  if (n_threads > n_sets) {
    n_threads <- n_sets
  }
  db_list <- parallel::mclapply(dataset_list, function(db_data) convert2df_wrapper(db_data), mc.cores = n_threads )
  return(db_list)
}
