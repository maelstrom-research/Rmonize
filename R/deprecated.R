#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [madshapR_website()] instead of `madshapR_help()`.
#'
#' @name deprecated
#' @keywords internal
#' @import dplyr
#' @importFrom lifecycle deprecate_warn
#' @export
Rmonize_help <- function(...) {
  
  deprecate_warn(
    "1.0.2", "Rmonize_help()", "Rmonize_website()")
  
  # Unquote-splice to avoid argument matching
  
  Rmonize_website(...)
  
}
