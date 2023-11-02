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
utils::globalVariables(c('index','score', 'AU', 'DI', 'J9', 'PY', 'TI', 'SO', '.', 'UUID'))

#' Default matching fields for [biblioverlap::biblioverlap()]
#'
#' Each element in the list corresponds to a column that will be used to match documents between bibliographic datasets
#'
#' @format A named list
#' @field doi Unique identifier (e.g. DOI)
#' @field title Document title
#' @field pubyear Publication year
#' @field authors Authors
#' @field source Publication source
#'
#' @export
default_matching_fields <- list(DI = 'DOI',
                                TI = 'Title',
                                PY = 'Publication Year',
                                AU = 'Author/s',
                                SO = 'Source Title')

#' ufrj_bio_0122
#'
#' Data obtained from [The Lens Scholarly Search](https://www.lens.org)
#' Report ...
#' @name ufrj_bio_0122
#' @encoding UTF-8
#'
#' @format ## `ufrj_bio_0122`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#'
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"ufrj_bio_0122"

