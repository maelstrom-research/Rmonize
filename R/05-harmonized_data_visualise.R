#' @title
#' Generate a web-based bookdown visual report of a harmonized dossier
#'
#' @description
#' Generates a visual report for a dataset in an HTML bookdown document. The 
#' report provides figures and descriptive statistics for each variable to 
#' facilitate the assessment of input data. Statistics and figures are generated 
#' according to variable data type. The report can be used to help assess 
#' data structure, coherence across elements, and taxonomy or 
#' data dictionary formats. The summaries and figures provide additional 
#' information about variable distributions and descriptive statistics. 
#' The charts and tables are produced based on their data type. The variable can 
#' be grouped using `group_by` parameter, which is a (categorical) column in the 
#' dataset. The user may need to use [as.factor()] in this context. To fasten 
#' the process (and allow recycling object in a workflow) the user can feed the 
#' function with a `.summary_var`, which is the output of the function 
#' [dataset_summarize()] of the column(s) `col` and  `group_by`. The summary 
#' must have the same parameters to operate. 
#'
#' @details
#' A harmonized dossier must be a named list containing at least one data frame 
#' or data frame extension (e.g. a tibble), each of them being 
#' harmonized dataset(s). It is generally the product of applying harmonization 
#' processing to a dossier object. The name of each tibble will be use as the 
#' reference name of the dataset. A harmonized dossier has four attributes :
#' `Rmonize::class` which is "harmonized_dossier" ; `Rmonize::DataSchema` 
#' (provided by user) ; `Rmonize::Data Processing Elements` ; 
#' `Rmonize::harmonized_col_id` (provided by user) which refers to the column 
#' in each dataset which identifies unique combination observation/dataset. 
#' This id column name is the same across the dataset(s), the DataSchema and 
#' the Data Processing Elements (created by using 'id_creation') and is used to 
#' initiate the process of harmonization.
#' 
#' A taxonomy is classification scheme that can be defined for variable 
#' attributes. If defined, a taxonomy must be a data frame like object. It must 
#' be compatible with (and is generally extracted from) an Opal environment. To 
#' work with certain functions, a valid taxonomy must contain at least the 
#' columns 'taxonomy', 'vocabulary', and 'terms'. In addition, the taxonomy
#' may follow Maelstrom research taxonomy, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#' 
#' The valueType is a property of a variable and is required in certain 
#' functions to determine the handling of the variables. The valueType refers 
#' to the OBiBa-internal type of a variable. It is specified in a data 
#' dictionary in a column `valueType` and can be associated with variables as 
#' attributes. Acceptable valueTypes include 'text', 'integer', 'decimal', 
#' 'boolean', datetime', 'date'). The full list of OBiBa valueType 
#' possibilities and their correspondence with R data types are available using
#' [madshapR::valueType_list].
#'
#' @param harmonized_dossier List of tibble(s), each of them being 
#' harmonized dataset.
#' @param bookdown_path A character string identifying the folder path where the 
#' bookdown report will be saved.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param harmonized_dossier_summary A list which is the summary of the 
#' harmonized dossier.
#' @param harmonized_col_id  A character string identifying the name of the 
#' column identifier of the dataset and will be the concatenation of 
#' id column value and dataset name. NULL by default.
#' @param dataschema A list of tibble(s) representing metadata of an 
#' associated harmonized dossier.
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#' @param dataschema_apply whether to apply the datashema to each 
#' harmonized dataset. The resulting tibble will have for each column its 
#' associated meta data as attributes. The factors will be preserved. 
#' FALSE by default.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. FALSE by default.
#' The visual element will be grouped and displayed by this column.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A bookdown folder containing files in the specified output folder. To
#' open the file in browser, open 'docs/index.html'.
#' Or use [bookdown_open()]
#'
#' @seealso
#' [pooled_harmonized_dataset_create()]
#' [dataset_summarize()]
#' [bookdown_open()]
#'
#' @examples
#' {
#' 
#' # You can use our demonstration files to run examples
#' 
#' library(dplyr)
#' library(fs)
#' 
#' harmonized_dossier <- Rmonize_DEMO$harmonized_dossier
#' harmonized_dossier_summary <- Rmonize_DEMO$harmonized_dossier_summary
#' 
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#' 
#' harmonized_dossier_visualize(
#'   harmonized_dossier,
#'   bookdown_path = bookdown_path,
#'   harmonized_dossier_summary = harmonized_dossier_summary)
#' 
#' # To open the file in browser, open 'bookdown_path/docs/index.html'.
#' # Or use bookdown_open(bookdown_path) function
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
    group_by = attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`,
    harmonized_dossier_summary = NULL,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Element`,
    valueType_guess = FALSE,
    taxonomy = NULL){

  # check args
  render <- 'html'
  
  # tests
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')
  
  # test if group exists
  if(!is.null(group_by)){
    
    # test if harmonized_col_dataset exists
    bind_rows(
      harmonized_dossier %>% lapply(function(x) x %>%
                                      mutate(across(everything(),as.character)))) %>%
      select(all_of(group_by))
  }
  
  # creation of pooled_harmonized_dataset
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(
      harmonized_dossier = harmonized_dossier,
      harmonized_col_dataset = group_by,
      add_col_dataset = TRUE,
      data_proc_elem = data_proc_elem,
      dataschema = dataschema)
  
  if(is.null(harmonized_dossier_summary)){
    harmonized_dossier_summary <-
      dataset_summarize(
        dataset = pooled_harmonized_dataset,
        group_by = 
          attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
        taxonomy = taxonomy, 
        valueType_guess = valueType_guess)}
  
  dataset_visualize(
    dataset = pooled_harmonized_dataset,
    group_by = attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
    bookdown_path = bookdown_path,
    taxonomy = taxonomy,
    valueType_guess = valueType_guess,
    dataset_summary = harmonized_dossier_summary)
  
}
