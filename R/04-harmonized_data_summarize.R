#' @title
#' Generate an assessment report and summary of a harmonized dossier
#'
#' @description
#' Assesses and summarizes the content and structure of a harmonized dossier and 
#' generates reports of the results. This function can be used to evaluate data 
#' structure, presence of specific fields, coherence across elements, and 
#' data dictionary formats, and to summarize additional information about 
#' variable distributions and descriptive statistics.
#' 
#' @details
#' A harmonized dossier is a named list containing one or more data frames, 
#' which are harmonized datasets. A harmonized dossier is generally the 
#' product of applying processing to a dossier object The name of each 
#' harmonized dataset (data frame) is taken from the reference input dataset. 
#' A harmonized dossier also contains the DataSchema and 
#' Data Processing Elements used in processing as attributes.
#' 
#' A DataSchema is the list of core variables to generate across datasets and 
#' related metadata. A DataSchema object is a list of data frames with elements 
#' named 'Variables' (required) and 'Categories' (if any). The 'Variables' 
#' element must contain at least the `name` column, and the 'Categories' 
#' element must contain at least the `variable` and `name` columns to be usable 
#' in any function. In 'Variables' the `name` column must also have unique 
#' entries, and in 'Categories' the combination of `variable` and `name` columns 
#' must also be unique. 
#' 
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#' 
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#' 
#' The valueType is a declared property of a variable that is required in 
#' certain functions to determine handling of the variables. Specifically, 
#' valueType refers to the 
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/variables-data.html#value-types). 
#' The valueType is specified in a data dictionary in a column 'valueType' and 
#' can be associated with variables as attributes. Acceptable valueTypes 
#' include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full 
#' list of OBiBa valueType possibilities and their correspondence with R data 
#' types are available using [valueType_list].
#'
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param group_by A character string identifying the column in each 
#' harmonized dataset to use as a grouping variable. Summary elements will be 
#' grouped by this column.
#' @param dataschema A DataSchema object.
#' @param data_proc_elem A Data Processing Elements object.
#' @param taxonomy An optional data frame identifying a variable 
#' classification schema.
#' @param valueType_guess Whether the output should be generated based on more 
#' precise valueType inferred from the data. FALSE by default 
#' (will use the valueType declared).
#'
#' @returns
#' A list of data frames containing overall assessment reports and summaries 
#' grouped by harmonized dataset.
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
  
  harmonized_dossier_summary <-
    dataset_summarize(
      dataset = pooled_harmonized_dataset,
      group_by = 
        attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
      taxonomy = taxonomy, 
      valueType_guess = valueType_guess)
  
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),"Overview",
                "Harmonization Overview")   
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),"Data dictionary summary",
                "Harmonized Data dictionary summary")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),"Data dictionary assessment",
                "Harmonized Data dictionary assessement")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),"Dataset assessment",
                "Harmonized Dataset assessment")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),"Variables summary \\(all\\)",
                "Harmonized Variables summary (all)")

  return(harmonized_dossier_summary)
}
