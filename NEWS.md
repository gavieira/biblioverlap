# biblioverlap (development version)
* Minor documentation updates
* Used `dplyr::bind_rows()` instead of `rbind()` in the ShinyApp's `get_merged_db_list()` to allow merging of datasets with different rows (and probably from different databases)
* Added `tryCatch()` to ShinyApp's `merge_input_files()` to raise an error in case the rbind fails (possibly because of divergences in column names/number) and warn the user that the files provided are not compatible for merging.


# biblioverlap 1.0.3

* Updated ShinyApp's `calculate_results()` to avoid crashes if user input has problems
* Fixed [issue](https://github.com/gavieira/biblioverlap/issues/4) regarding duplicate removal in ShinyApp's 'Merge Files' tab 
* Added functions that raise errors if the following coditions are not met:
  - All datasets have valid names
  - All column names are found in all datasets
