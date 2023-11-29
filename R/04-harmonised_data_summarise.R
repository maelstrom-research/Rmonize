#' @title
#' Generate a report and summary of a harmonized dossier
#'
#' @description
#' Assesses and summarizes the content and structure of a harmonized dossier 
#' (list of harmonized datasets) and reports potential issues to facilitate 
#' the assessment of input data. The report can be used to help assess data 
#' structure, presence of fields, coherence across elements, and taxonomy or 
#' data dictionary formats. The summary provides additional information about 
#' variable distributions and descriptive statistics. This report is compatible 
#' with Excel and can be exported as an Excel spreadsheet.
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
#' A DataSchema defines the harmonized variables to be generated, and also 
#' represents metadata of an associated harmonized dossier. It must be a list 
#' of data frame or data frame extension (e.g. a tibble) objects with elements 
#' named "Variables" (required) and "Categories" (if any). The "Variables" 
#' element must contain at least the `name` column, and the "Categories" element 
#' must contain at least the `variable` and `name` columns to be usable in any 
#' function. To be considered as a minimum workable DataSchema, in "Variables" 
#' the `name` column must also have unique and non-null entries, and in 
#' "Categories" the combination of `variable` and `name` columns must also be 
#' unique.
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
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param dataschema A list of tibble(s) representing metadata of an 
#' associated harmonized dossier.
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. FALSE by default.
#'
#' @returns
#' A list of tibbles of report for each harmonized dataset.
#'
#' @examples
#' {
#' 
#' harmonized_dossier <- Rmonize_DEMO$harmonized_dossier
#'
#' # summary harmonization
#' harmonized_dossier_summarize(harmonized_dossier)
#'
#' }
#'
#' @import dplyr stringr tidyr haven
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
harmonized_dossier_summarize <- function(
    harmonized_dossier,
    group_by = attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Element`,
    taxonomy = NULL,
    valueType_guess = FALSE){

  # tests
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (TRUE by default)')

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
  
  report <-
    dataset_summarize(
      dataset = pooled_harmonized_dataset,
      group_by = 
        attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
      taxonomy = taxonomy, 
      valueType_guess = valueType_guess)
  
  names(report) <- str_replace(names(report),"Overview",
                              "Harmonization Overview")   
  names(report) <- str_replace(names(report),"Data dictionary summary",
                              "Harmonized Data dictionary summary")
  names(report) <- str_replace(names(report),"Data dictionary assessment",
                              "Harmonized Data dictionary assessement")
  names(report) <- str_replace(names(report),"Dataset assessment",
                              "Harmonized Dataset assessment")
  names(report) <- str_replace(names(report),"Variables summary \\(all\\)",
                              "Harmonized Variables summary (all)")

  return(report)
}
