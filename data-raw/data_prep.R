##### Reading raw lens data
##Data was downloaded from


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
df_list <- lapply(csv_files, read.csv)

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
test_data <- lapply(df_list, function(df) {
  `%>%` <- magrittr::`%>%` #Defining the pipe operator for this specific function to avoid loading the entire 'magrittr' library
  df %>%
    dplyr::mutate(Date.Published = as.Date(Date.Published)) %>% #Converting 'Date.Published' into 'Date' type
    dplyr::filter(Date.Published >= '2022-01-01', Date.Published <= '2022-01-31')  %>% #Selecting only january records
    dplyr::select(Lens.ID, DOI, Title, Publication.Year, Source.Title, Author.s, Publication.Type, Citing.Works.Count, Open.Access.Colour) %>% #Selecting relevant columns
    remove_excess_authors(au_col = 'Author.s') %>% #Keeps only the three first authors
    dplyr::mutate(Title = gsub("<.*?>", "", Title)) %>% #Removes html tags, common in the Title field
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ iconv(., to = "UTF-8"))) %>% #Making sure that all character fields are UTF-8 encoded
    dplyr::rename( DI = DOI, TI = Title, PY = Publication.Year, SO = Source.Title, AU = Author.s )
})


## code to prepare `data_prep` dataset goes here

usethis::use_data(test_data, overwrite = TRUE, compress = 'xz')



##Generating duplicated dataset for internal testing

dups <- lapply(test_data, function(df) rbind(df,df))

all_dups <- lapply(dups, function(db) {
  db %>%
  dplyr::filter(DI != '') %>%
  dplyr::filter(duplicated(DI) | duplicated(DI, fromLast = TRUE)) %>%
  dplyr::arrange(DI)
  })

View(all_dups$Biochemistry)

View(lapply(test_data, function(db) {
  db %>%
    dplyr::distinct()
}
  ))

usethis::use_data(c(test_data, internal = TRUE, overwrite = TRUE, compress = 'xz')

