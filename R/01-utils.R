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
#' download_templates()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
download_templates <- function(){
  
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
#'   \item{DEMO_data_processing_elements - final}{Data Processing Elements}
#'   \item{DEMO_data_processing_elements - with error}{Data Processing Elements
#'   containing errors}
#'   \item{DEMO_data_processing_elements - work in progress}{Data processing
#'   element in construction}
#'   \item{DEMO_dataschema}{DataSchema used, combined with data processing
#'   elements and input material}
#'   \item{pooled_harmonized_dataset}{pooled harmonized dataset for testing 
#'   purpose}
#'   \item{harmonized_dossier}{harmonized dossier for testing purpose}
#'   \item{summary_var_harmo}{harmonized variables summary for testing purpose}
#'   \item{dd_MELBOURNE_1_format_maelstrom}{Data dictionary (1) of Melbourne 
#'   dataset}
#'   \item{dd_MELBOURNE_2_format_maelstrom}{Data dictionary (2) of Melbourne 
#'   dataset}
#'   \item{dd_PARIS_format_maelstrom}{Data dictionary of Paris dataset}
#'   \item{dd_TOKYO_format_maelstrom}{Data dictionary of Tokyo dataset}
#'   \item{dataset_MELBOURNE_1}{Dataset of Melbourne (1)}
#'   \item{dataset_MELBOURNE_2}{Dataset of Melbourne (2)}
#'   \item{dataset_PARIS}{Dataset of Paris}
#'   \item{dataset_TOKYO}{Dataset of Tokyo}
#'   ...
#' }
#'
#' @examples
#' {
#'
#'  print(DEMO_files_harmo$`dataschema - final`)
#'
#' }
"DEMO_files_harmo"

