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
    harmonized_col_id = attributes(harmonized_dossier)$`Rmonize::harmonized_col_id`,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    dataschema_apply = FALSE,
    valueType_guess = FALSE,
    taxonomy = NULL){

  # check args
  render <- 'html'
  
  # tests
  as_dossier(harmonized_dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,'`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.logical(dataschema_apply))
    stop(call. = FALSE,'`dataschema_apply` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')
  
  bookdown_path <- str_squish(bookdown_path)
  path_to <- path_abs(bookdown_path)
  
  if(dir_exists(path_to)){stop(call. = FALSE,
"The path folder already exists. 
Please provide another name folder or delete the existing one.")}
  
  # group_by = attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(
      harmonized_dossier = harmonized_dossier,
      harmonized_col_dataset = group_by,
      harmonized_col_id = harmonized_col_id,
      dataschema = dataschema,
      dataschema_apply = dataschema_apply)
  
  # test if group_by column exists
  if(is.null(group_by)){
    
    warning(call. = FALSE,
            '\nThe harmonized_col_dataset is not present in your DataSchema.',
            '\nThe names of each dataset in your dossier have been be used instead, and an',
            '\nadditional variable has been created to avoid loosing information.',
            bold("\n\nUseful tip:\n"),
            'To avoid this warning, we recommend to add this variable in your DataSchema.')
    
    col_dataset <- harmonized_dossier
    for(i in names(col_dataset)){
      # stop()}
      col_dataset[[i]] <- 
        col_dataset[[i]] %>% 
        mutate(`Rmonize::harmonized_col_dataset` = i,
               # `Rmonize::harmonized_col_dataset` = as_category(`Rmonize::harmonized_col_dataset`))
               `Rmonize::harmonized_col_dataset` = 
                 Rmonize:::as_category(.data$`Rmonize::harmonized_col_dataset`)) %>%
        select('Rmonize::harmonized_col_dataset')
    }
    
    col_dataset <- bind_rows(col_dataset)
    
    pooled_harmonized_dataset <- 
      pooled_harmonized_dataset %>%
      bind_cols(col_dataset)
    
    group_by = 'Rmonize::harmonized_col_dataset'
  }
  
  if(is.null(harmonized_dossier_summary)){
    dossier_summary <- 
      dataset_summarize(
        dataset = pooled_harmonized_dataset,
        group_by = group_by,
        taxonomy = taxonomy,
        valueType_guess = valueType_guess,
        dataset_name = 'pooled_harmonized_dossier')}
  
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Harmonization Overview",
                "Overview")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Harmonized Data dictionary summary",
                "Data dictionary summary")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Harmonized Data dictionary assessement",
                "Data dictionary assessment")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Harmonized Dataset assessment",
                "Dataset assessment")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Harmonized Variables summary \\(all\\)",
                "Variables summary (all)")
  
  dataset_visualize(
    dataset = pooled_harmonized_dataset,
    group_by = group_by,
    bookdown_path = bookdown_path,
    taxonomy = taxonomy,
    valueType_guess = valueType_guess,
    dataset_summary = harmonized_dossier_summary,
    dataset_name = 'pooled_harmonized_dataset')
  
  # if(is.null(attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`$name)){}
  
}
