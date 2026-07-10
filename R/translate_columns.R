#' Translate one or more columns in a data frame
#'
#' Creates bilingual translations for specified character columns
#' using a translation lookup generated from unique values in each
#' column. Translations can either overwrite the original column
#' or be stored in newly created columns.
#'
#' @param data A data frame or tibble.
#' @param columns Character vector of column names to translate.
#' @param source_language Source language code. Defaults to `"en"`.
#' @param target_language Target language code. Defaults to `"fr"`.
#' @param overwrite Logical. If `TRUE` (default), replace the
#'   original column with bilingual text. If `FALSE`, create a
#'   new column instead.
#' @param suffix Suffix applied to new bilingual columns when
#'   `overwrite = FALSE`. Defaults to `"_bi"`.
#'
#' @return A data frame with translated columns.
#'
#' @details
#' For each specified column:
#' \enumerate{
#'   \item Unique non-missing values are extracted.
#'   \item Values are translated using
#'         \code{polyglotr::google_translate()}.
#'   \item A bilingual string is constructed in the form
#'         `"English text / French text"`.
#'   \item The bilingual values are joined back to the
#'         original data frame.
#' }
#'
#' This approach minimizes translation calls by translating only
#' unique values rather than every row.
#'
#' @examples
#' df <- tibble::tibble(
#'   fieldNotes = c(
#'     "Near reef",
#'     "Near reef",
#'     "Deep basin"
#'   ),
#'   locationRemarks = c(
#'     "Good visibility",
#'     "Moderate visibility",
#'     "Good visibility"
#'   )
#' )
#'
#' translate_columns(
#'   df,
#'   columns = c(
#'     "fieldNotes",
#'     "locationRemarks"
#'   )
#' )
#'
#' translate_columns(
#'   df,
#'   columns = "fieldNotes",
#'   overwrite = FALSE
#' )
#'
#' @seealso
#' \code{\link{make_bilingual_lookup}}
#'
#' @export
translate_columns <- function(
  data,
  columns,
  source_language = "en",
  target_language = "fr",
  overwrite = TRUE,
  suffix = "_bi"
) {
  result <- data

  for (col in columns) {
    lookup <- make_bilingual_lookup(
      result[[col]],
      source_language = source_language,
      target_language = target_language
    )

    names(lookup)[1] <- col

    result <- result |>
      dplyr::left_join(
        lookup,
        by = col,
        relationship = "many-to-one"
      )

    if (overwrite) {
      result[[col]] <- dplyr::coalesce(
        result$bilingual,
        result[[col]]
      )
    } else {
      result[[paste0(col, suffix)]] <- dplyr::coalesce(
        result$bilingual,
        result[[col]]
      )
    }

    result <- result |>
      dplyr::select(-translated, -bilingual)
  }

  result
}