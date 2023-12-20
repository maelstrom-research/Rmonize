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
#' Rmonize_help()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
Rmonize_help <- function(){

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
#' Demo objects to provide illustrative examples
#'
#' @description
#' Demo input datasets, input data dictionaries, DataSchema, 
#' Data Processing Elements, and other objects to provide illustrative examples 
#' of objects used by Rmonize.
#'
#' @format ## `list`
#' A list with 13 elements (data frames and lists) providing example objects 
#' for testing the package:
#'
#' \describe{
#'   \item{data_processing_elements - final}{Example Data Processing Elements}
#'   \item{data_processing_elements - with error}{Example 
#'   Data Processing Elements containing errors}
#'   \item{data_processing_elements - work in progress}{Example incomplete
#'   Data processing Element}
#'   \item{dataschema - final}{Example DataSchema}
#'   \item{pooled_harmonized_dataset}{Example pooled harmonized dataset}
#'   \item{harmonized_dossier}{Example of harmonized dossier}
#'   \item{harmonized_dossier_summary}{Example harmonized variables summary}
#'   \item{data_dict_MELBOURNE}{Example Data dictionary for Melbourne dataset}
#'   \item{data_dict_PARIS}{Example Data dictionary for Paris dataset}
#'   \item{data_dict_TOKYO}{Example Data dictionary for Tokyo dataset}
#'   \item{dataset_MELBOURNE}{Example Dataset for Melbourne}
#'   \item{dataset_PARIS}{Example Dataset for Paris}
#'   \item{dataset_TOKYO}{Example Dataset for Tokyo}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' 
#' glimpse(Rmonize_DEMO$`dataschema - final`)
#'
#' }
"Rmonize_DEMO"

