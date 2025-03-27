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
#'   \item{dataset_study1}{Dataset example for a study named "study1", labelled with its data dictionary}
#'   \item{dataset_study2}{Dataset example for a study named "study2", labelled with its data dictionary}
#'   \item{dataset_study3}{Dataset example for a study named "study3", labelled with its data dictionary}
#'   \item{dataset_study4}{Dataset example for a study named "study4", labelled with its data dictionary}
#'   \item{dataset_study5}{Dataset example for a study named "study5", labelled with its data dictionary}
#'   \item{Data Processing Elements}{Example Data Processing Elements}
#'   \item{DataSchema}{Example DataSchema}
#'   \item{harmo_dataset_study1}{Harmonized dataset example , labelled with its harmonized data dictionary}
#'   \item{harmo_dataset_study2}{Harmonized dataset example , labelled with its harmonized data dictionary}
#'   \item{harmo_dataset_study3}{Harmonized dataset example , labelled with its harmonized data dictionary}
#'   \item{harmo_dataset_study4}{Harmonized dataset example , labelled with its harmonized data dictionary}
#'   \item{harmo_dataset_study5}{Harmonized dataset example , labelled with its harmonized data dictionary}
#'   \item{harmo_dataset_pooled}{Harmonized pooled dataset example, labelled with its harmonized data dictionary}
#'   \item{summary - harmonized datasets}{Example of harmonized datasets summary}
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


#' @title
#' Objects to provide illustrative use case
#'
#' @description
#' Illustrative input datasets, input data dictionaries, DataSchema, 
#' Data Processing Elements to provide use case of Rmonize.
#'
#' @format ## `list`
#' A list with elements (data frames and lists) providing example objects 
#' for testing the package:
#'
#' \describe{
#'   \item{original_dataset_study1a}{Dataset example for a study named "study1a"}
#'   \item{original_dataset_study2a}{Dataset example for a study named "study2a"}
#'   \item{original_dataset_study3a}{Dataset example for a study named "study3a"}
#'   \item{original_dataset_study4}{Dataset example for a study named "study4"}
#'   \item{original_dataset_study5}{Dataset example for a study named "study5"}
#'   \item{original_data_dict_study4}{Data dictionary example for a study named "study5"}
#'   \item{original_data_dict_study5}{Data dictionary example for a study named "study5"}
#'   \item{DPE_use_case_no_errors}{Example Data Processing Elements containing no errors}
#'   \item{DPE_use_case_with_errors}{Example Data Processing Elements containing errors}
#'   \item{DataSchema_use_case}{Example DataSchema}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' library(dplyr)
#' 
#' glimpse(Rmonize_examples_v1.1.0.9400$DataSchema_use_case)
#'
#' }
"Rmonize_examples_v1.1.0.9400"


