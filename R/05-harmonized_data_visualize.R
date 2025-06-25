#' @title
#' Generate a web-based visual report for a harmonized dossier
#'
#' @description
#' Generates a visual report of a harmonized dossier in an HTML bookdown
#' document, with summary figures and statistics for each harmonized variable.
#' The report outputs can be grouped by a categorical variable.
#'
#' @details
#' A harmonized dossier is a named list containing one or more data frames,
#' which are harmonized datasets. A harmonized dossier is generally the
#' product of applying processing to a dossier object. The name of each
#' harmonized dataset (data frame) is taken from the reference input dataset.
#' A harmonized dossier also contains the DataSchema and
#' Data Processing Elements used in processing as attributes.
#' 
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param bookdown_path A character string identifying the folder path where 
#' the bookdown report files will be saved.
#' @param harmonized_dossier_summary A list which identifies an existing 
#' summary produced by [harmonized_dossier_summarize()] of the harmonized 
#' variables. 
#' Using this parameter can save time in generating the visual report.
#'
#' @returns
#' A folder containing files for the bookdown document. To open the bookdown document 
#' in a browser, open 'docs/index.html', or use [bookdown_open()] with the 
#' folder path.
#'
#' @seealso
#' [dataset_visualize()]
#' [bookdown_open()]
#'
#' @examples
#' {
#' 
#' library(fs)
#' 
#' # Use Rmonize_examples to run examples.
#' # Perform data processing
#' 
#' harmonized_dossier <- Rmonize_examples$`harmonized_dossier`
#' harmonized_dossier_summary <- Rmonize_examples$`summary_report_harmonized_dossier`
#' 
#' # Create a folder where the visual report will be placed
#' 
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#' 
#' # Generate the visual report
#' harmonized_dossier_visualize(
#'   harmonized_dossier = harmonized_dossier,
#'   bookdown_path = bookdown_path,
#'   harmonized_dossier_summary = harmonized_dossier_summary)
#'   
#' # To open the file in a browser, open 'bookdown_path/docs/index.html'.
#' # Or use bookdown_open(bookdown_path) function.
#' 
#' }
#'
#' @import dplyr haven fs
#' @importFrom rlang .data
#'
#' @export
harmonized_dossier_visualize <- function(
    harmonized_dossier,
    bookdown_path,
    harmonized_dossier_summary = NULL){

  # check args
  render <- 'html'

  # [GF] to test
  if(!is_dossier(harmonized_dossier))
    stop(call. = FALSE,
         'Input parameter `harmonized_dossier` must be a harmonized dossier.')
  
  if(!is.character(bookdown_path))
    stop(call. = FALSE,
         '`bookdown_path` must be a character string.')

  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(harmonized_dossier = harmonized_dossier)

  if(is.null(harmonized_dossier_summary)){
    harmonized_dossier_summary <-
      harmonized_dossier_summarize(harmonized_dossier)
  }

  names(harmonized_dossier_summary) <-
    str_replace(names(harmonized_dossier_summary),"Harmonization overview",
    "Overview")
  # names(harmonized_dossier_summary) <- 
  #   str_replace(names(harmonized_dossier_summary),"Data dictionary summary",
  #   "Harmonized Data dictionary summary")
  # names(harmonized_dossier_summary) <- 
  #   str_replace(names(harmonized_dossier_summary),"Data dictionary assessment",
  #   "Harmonized Data dictionary assessment")
  # names(harmonized_dossier_summary) <- 
  #   str_replace(names(harmonized_dossier_summary),"Dataset assessment",
  #   "Harmonized Dataset assessment")
  # names(harmonized_dossier_summary) <- 
  #   str_replace(names(harmonized_dossier_summary),"Variables summary \\(all\\)",
  #   "Harmonized Variables summary (all)")
  
  # [GF] WARNING : Categories in data dictionary en doublon
  harmonized_dossier_summary$`Variables summary (all)` <- 
    harmonized_dossier_summary$`Variables summary (all)` %>%
    select(1:"Non-valid categories",-"Harmonization status","Harmonization status",everything())

  dataset_visualize(
    dataset = pooled_harmonized_dataset,
    group_by = attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
    bookdown_path = bookdown_path,
    dataset_summary = harmonized_dossier_summary)
  
}

