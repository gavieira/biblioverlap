#' Document matching using unique identifiers for two bibliographic datasets
#'
#' @description
#' This function receives two lists of unique identifiers (e.g. DOI - Digital Object Identifier) and returns a list containing the matches of list1 against list2
#' This step of the matching is also given a "fake score" that will be used as a tag to identify and exclude matched documents from subsequent doi matching steps, increasing overall efficiency of the whole process
#'
#' @param db1 - First bibliographic database in the comparison
#' @param db2 - Second bibliographic database in the comparison
#' @param n_threads - number of (logical) cores to be used in the matching
#' @param doi_score - custom score value to help identify DOI matches in subsequent comparisons
#'
#' @return a list containing match data
#'
#' @keywords internal
doi_matching <- function(db1, db2, n_threads, doi_score = 2) {
  subset_db1 <- subset_db_for_doi_match(db1) #Extracting DOI info from db1
  subset_db2 <- subset_db_for_doi_match(db2) #Extracting DOI info from db2
  if ( any_empty_dfs(subset_db1, subset_db2) ) {
    print('There is a db with no DOI records')
    return( list() )
  }
  cl <- parallel::makeCluster(n_threads) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  parallel::clusterExport(cl, varlist = c("subset_db1", "subset_db2", "doi_score"), envir = environment()) #Exporting the variables received by the function to the cluster
  doi_matches <- parallel::parLapplyLB(cl,  1:nrow(subset_db1),  function(i) {

    match <- match(subset_db1[i, 'DI'], subset_db2$DI, incomparables = c(NULL, NA, ''))
    if (!is.na(match)) {
      return( list('db1_id' = as.numeric(subset_db1[i, 'index']),
                   'score' = doi_score,
                   'db2_id' = as.numeric(subset_db2[match, 'index'])) ) }
  }) #Calculating which elements in list2 correspond to the DOIs in list1
  parallel::stopCluster(cl) #Stopping the cluster
  doi_matches <- Filter(function(x) !is.null(x), doi_matches) #Removing any null elements from list
  return(doi_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}
