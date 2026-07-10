#' Fix garbled UT8 translations from polyglotr::google_translate()
#'
#' @param x garbled French output from polyglotr::google_translate()
#'
#' @returns
#' @export
#'
#' @examples
fix_encoding <- function(x) {
  iconv(
    iconv(x, from = "UTF8", to = "windows-1252"),
    from = "UTF8",
    to = "UTF8"
  )
}
