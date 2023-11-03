#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Variables for unquoted use in dplyr
#' @description
#' Internally, biblioverlap uses specific field names to refer to the data it processes. They are added here for unquoted use in dplyr.
#'
#' @name global variables
#' @keywords internal
utils::globalVariables(c('index','score', 'AU', 'DI', 'J9', 'PY', 'TI', 'SO', '.', 'UUID'))

#' Default matching fields for [biblioverlap()]
#'
#' Each element in the named list corresponds to a column that will be used to match documents between bibliographic datasets.
#'
#' In total, there are five columns that need to be specified. The default values come from column names in [The Lens](https://www.lens.org/).
#'
#' @field doi Unique identifier (e.g. DOI). Default: 'DOI'
#' @field title Document title. Default: 'Title'
#' @field pubyear Publication year. Default: 'Publication Year'
#' @field authors Authors. Default: 'Author/s'
#' @field source Publication source. Default: 'Source Title'
#'
#' @keywords internal
default_matching_fields <- list(DI = 'DOI',
                                TI = 'Title',
                                PY = 'Publication Year',
                                AU = 'Author/s',
                                SO = 'Source Title')

#' UFRJ-affiliated documents from biological sciences disciplines (January 2022)
#'
#' @description
#' Data obtained from [The Lens Scholarly Search](https://www.lens.org/lens/search/scholar/list?q=) in September 6, 2023.
#'
#' The original data contained all documents from four major biological sciences fields published in the year 2022 by at least one author affiliated to the Universidade Federal do Rio de Janeiro (UFRJ). The data was then subsampled to documents published exclusively in January 2022 to reduce package size.
#'
#' The biological disciplines featured in this dataset are [Biochemistry](https://www.lens.org/lens/search/scholar/list?collectionId=212653), [Genetics](https://www.lens.org/lens/search/scholar/list?collectionId=212658), [Microbiology](https://www.lens.org/lens/search/scholar/list?collectionId=212657) and [Zoology](https://www.lens.org/lens/search/scholar/list?collectionId=212655).
#'
#'
#' @name ufrj_bio_0122
#' @encoding UTF-8
#'
#' @format ## `ufrj_bio_0122`
#' A named list with 4 elements. Each element is a dataframe that contains the following fields:
#' \describe{
#'   \item{Lens ID}{Unique identifier given to each record in The Lens database}
#'   \item{DOI}{Digital Object Identifier}
#'   \item{Title}{Document title}
#'   \item{Publication Year}{Document publication year}
#'   \item{Source Title}{Source (e.g. journal) where the document has been published}
#'   \item{Author/s}{Document authors}
#'   \item{Publication Type}{Type of the document (e.g. 'journal article', 'book chapter', etc...)}
#'   \item{Citing Works Count}{Total number of citations received by document at the time of data recovery}
#'   \item{Open Access Colour}{Type of open access (e.g. gold, bronze, green, etc...)}
#' }
#'
#' @source <https://www.lens.org>
#' @keywords datasets
"ufrj_bio_0122"
