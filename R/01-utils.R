#' @title
#' Call to package website
#'
#' @description
#' Direct call to the package website, which includes an overview of the 
#' Rmonize process, vignettes and user guides, a reference list of functions 
#' and help pages, and package updates.
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
#' Example objects to provide an illustrative use case
#'
#' @description
#' Example input datasets, input data dictionaries, DataSchema,
#' Data Processing Elements, harmonized output, and summary report.
#'
#' @format ## `list`
#' A list with elements (data frames and lists) providing example objects
#' for using the package:
#' \describe{
#' \item{original_dataset_study1}{Example original dataset from .sav file for "study1"}
#' \item{original_dataset_study2}{Example original dataset from .sav file for "study2"}
#' \item{original_dataset_study3}{Example original dataset from .csv file for "study3"}
#' \item{original_dataset_study4}{Example original dataset from .xlsx file for "study4"}
#' \item{original_dataset_study5}{Example original dataset from .xlsx file for "study5"}
#' \item{original_data_dict_study4}{Example original data dictionary from .xlsx file, for "study4"}
#' \item{original_data_dict_study5}{Example original data dictionary from .xlsx file, for "study5"}
#' \item{input_dataset_study1}{Example input dataset ready for processing, for "study1"}
#' \item{input_dataset_study2}{Example input dataset ready for processing, for "study2"}
#' \item{input_dataset_study3}{Example input dataset ready for processing, for "study3"}
#' \item{input_dataset_study4}{Example input dataset ready for processing, for "study4"}
#' \item{input_dataset_study5}{Example input dataset ready for processing, for "study5"}
#' \item{DataSchema}{Example DataSchema}
#' \item{Data_Processing_Element_no errors}{Example Data Processing Elements containing no errors}
#' \item{Data_Processing_Element_with errors}{Example Data Processing Elements containing errors}
#' \item{harmonized_dossier}{Example harmonized dossier}
#' \item{pooled_harmonized_dataset}{Example pooled harmonized dataset}
#' \item{summary_report_harmonized_dossier}{Example summary report of harmonized dossier}
#' }
#'
#' @examples
#' {
#'
#' library(dplyr)
#' glimpse(Rmonize_examples$`DataSchema`)
#' 
#'
#' }
"Rmonize_examples"

