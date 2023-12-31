##### Reading raw lens data
##Data was downloaded from The Lens (https://www.lens.org/)

# Specifying path (inside project root)
path <- 'data-raw'

# Getting csv file list
csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Getting csv filenames (without extension)
csv_names <- sapply(csv_files, function(file) {
  name <- sub("-.*", "", basename(file))
  capitalized_name <- paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), sep = '')
  return(capitalized_name)
      }
  )


# Creating a list of dataframes by reading each file with read.csv()
df_list <- lapply(csv_files, read.csv, strip.white = TRUE, check.names = FALSE)

# Using csv filenames as element names in the dataframe list
names(df_list) <- csv_names


#Function to keep only the first n authors of a column
remove_excess_authors <- function(df, au_col, sep = ';', max_au = 3) {
  cleaned_au <- sapply(unlist(df[au_col]), function(author_rec) {
    split_au <- unlist(strsplit(author_rec, sep)) #Spliting the authors using the separator
    ifelse(length(split_au) > max_au, paste(split_au[1:max_au], collapse = sep), author_rec) #If document has more authors than max_au, keep only the first authors. Else, keep all authors.
  })
  df[au_col] <- cleaned_au #Updating the original dataframe with the 'cleaned' author column
  return(df) #Returning the modified dataframe
}

# Selecting columns to clean and reduce size of package data
ufrj_bio_0122 <- lapply(df_list, function(df) {
  `%>%` <- magrittr::`%>%` #Defining the pipe operator for this specific function to avoid loading the entire 'magrittr' library
  df %>%
    dplyr::mutate(`Date Published` = as.Date(`Date Published`)) %>% #Converting 'Date.Published' into 'Date' type
    dplyr::filter(`Date Published` >= '2022-01-01', `Date Published` <= '2022-01-31')  %>% #Selecting only january records
    dplyr::select(`Lens ID`, DOI, Title, `Publication Year`, `Source Title`, `Author/s`, `Publication Type`, `Citing Works Count`, `Open Access Colour`) %>% #Selecting relevant columns
    remove_excess_authors(au_col = 'Author/s') %>% #Keeps only the three first authors
    dplyr::mutate(Title = gsub("<.*?>", "", Title)) %>% #Removes html tags, common in the Title field
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ iconv(., to = "UTF-8"))) #Making sure that all character fields are UTF-8 encoded
})

#Saving as package data
usethis::use_data(ufrj_bio_0122, compress = 'xz')
