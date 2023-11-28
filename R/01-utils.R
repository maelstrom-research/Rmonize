#' @title
#' Call the help center for full documentation
#'
#' @description
#' This function is a direct call to the documentation in the repository hosting
#' the package. The user can access the description of the latest version of the
#' package, the vignettes, and the list of functions.
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
#' Call the help center to the download template page
#'
#' @description
#' This function is a direct call to the documentation in the repository hosting
#' the package, more specifically to the download template section. 
#' The user can also access the description of the latest version of the
#' package, the vignettes, and the list of functions.
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
#' Built-in material allowing the user to test the package with demo data
#'
#' @description
#' Built-in tibbles and lists allowing the user to test the package with demo
#' material for harmonization.
#'
#' @format ## `list`
#' A list with 15 elements used for testing the package (data frames and lists):
#'
#' \describe{
#'   \item{data_processing_elements - final}{Data Processing Elements}
#'   \item{data_processing_elements - with error}{Data Processing Elements
#'   containing errors}
#'   \item{data_processing_elements - work in progress}{Data processing
#'   element in construction}
#'   \item{dataschema}{DataSchema used, combined with data processing
#'   elements and input material}
#'   \item{harmonized_dossier}{harmonized dossier for testing purpose}
#'   \item{harmonized_dossier_summary}{harmonized variables summary for testing 
#'   purpose}
#'   \item{pooled_harmonized_dataset}{pooled harmonized dataset for testing 
#'   purpose}
#'   \item{data_dict_MELBOURNE}{Data dictionary of Melbourne 
#'   dataset}
#'   \item{data_dict_PARIS}{Data dictionary of Paris dataset}
#'   \item{data_dict_TOKYO}{Data dictionary of Tokyo dataset}
#'   \item{dataset_MELBOURNE}{Dataset of Melbourne}
#'   \item{dataset_PARIS}{Dataset of Paris}
#'   \item{dataset_TOKYO}{Dataset of Tokyo}
#'   ...
#' }
#'
#' @examples
#' {
#'
#'  print(Rmonize_DEMO$`dataschema - final`)
#'
#' }
"Rmonize_DEMO"

