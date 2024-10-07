#' @title
#' Call to online documentation
#'
#' @description
#' Direct call to the online documentation for the package, which includes a 
#' description of the latest version of the package, vignettes, user guides, 
#' and a reference list of functions and help pages.
#'
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' Rmonize_website()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
Rmonize_website <- function(){
  
  browseURL("https://maelstrom-research.github.io/Rmonize-documentation/")

}

#' @title
#' Call to online documentation to download templates
#'
#' @description
#' Direct call to online documentation to download templates.
#'
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' Rmonize_templates()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
Rmonize_templates <- function(){
  
  browseURL("https://maelstrom-research.github.io/Rmonize-documentation/articles/a-Glossary-and-templates.html")

}

#' @title
#' Objects to provide illustrative use case
#'
#' @description
#' Illustrative input datasets, input data dictionaries, DataSchema, 
#' Data Processing Elements, and other objects to provide use case of Rmonize.
#'
#' @format ## `list`
#' A list with 12 elements (data frames and lists) providing example objects 
#' for testing the package:
#'
#' \describe{
#'   \item{dataset_study1}{Example Dataset for a study named "study1", labelled with its data dictionary}
#'   \item{dataset_study2}{Example Dataset for a study named "study2", labelled with its data dictionary}
#'   \item{dataset_study3}{Example Dataset for a study named "study3", labelled with its data dictionary}
#'   \item{dataset_study4}{Example Dataset for a study named "study4", labelled with its data dictionary}
#'   \item{dataset_study5}{Example Dataset for a study named "study5", labelled with its data dictionary}
#'   \item{T3_PAE_use_case_2024_08_21}{Example Data Processing Elements}
#'   \item{T2_PAE_use_case_2024_08_21}{Example DataSchema}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' library(dplyr)
#' 
#' glimpse(Rmonize_examples$DataSchema)
#'
#' }
"Rmonize_examples"

