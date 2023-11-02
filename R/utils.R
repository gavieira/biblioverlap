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
default_matching_fields <- list(DI = 'DI',
                                TI = 'TI',
                                PY = 'PY',
                                AU = 'AU',
                                SO = 'SO')


