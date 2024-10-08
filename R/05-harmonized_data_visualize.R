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
#' The valueType is a declared property of a variable that is required in 
#' certain functions to determine handling of the variables. Specifically, 
#' valueType refers to the 
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/dev/variables-data.html#value-types). 
#' The valueType is specified in a data dictionary in a column 'valueType' and 
#' can be associated with variables as attributes. Acceptable valueTypes 
#' include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full 
#' list of OBiBa valueType possibilities and their correspondence with R data 
#' types are available using [valueType_list]. The valueType can be used to 
#' coerce the variable to the corresponding data type.
#'
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal//), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#' 
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param bookdown_path A character string identifying the folder path where 
#' the bookdown report files will be saved.
#' @param pooled_harmonized_dataset A data frame containing the pooled harmonized dataset.
#' @param group_by A character string identifying the column in the dataset
#' to use as a grouping variable. Elements will be grouped by this 
#' column.
#' @param harmonized_dossier_summary A list which identifies an existing 
#' summary produced by [harmonized_dossier_summarize()] of the harmonized 
#' variables. 
#' Using this parameter can save time in generating the visual report.
#' @param dataschema A DataSchema object.
#' @param data_proc_elem A Data Processing Elements object.
#' @param add_col_dataset Whether to add an extra column to each 
#' harmonized dataset. The resulting data frame will have an additional column 
#' and its data dictionary will be updated accordingly adding categories for 
#' this variable if necessary. FALSE by default.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. FALSE by default.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A folder containing files for the bookdown site. To open the bookdown site 
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
#' # use Rmonize_examples provided by the package
#' library(stringr)
#' 
#' # perform data processing
#' pooled_harmonized_dataset <-
#'   Rmonize_examples[str_detect(names(Rmonize_examples),"harmo_dataset_pooled")][[1]]
#' summary_harmo  <- Rmonize_examples[str_detect(names(Rmonize_examples),"summary")][[1]]
#' 
#'  # create a folder where the visual report will be palced
#' library(fs)
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#' 
#' # generate the visual report
#' harmonized_dossier_visualize(
#'   pooled_harmonized_dataset = pooled_harmonized_dataset,
#'   harmonized_dossier_summary = summary_harmo,
#'   bookdown_path = bookdown_path)
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
    harmonized_dossier = NULL,
    bookdown_path,
    pooled_harmonized_dataset = NULL,
    group_by = attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`,
    harmonized_dossier_summary = NULL,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Element`,
    add_col_dataset = FALSE,
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
    stop(call. = FALSE,
         '`bookdown_path` must be a character string.')
  
  if(!is.logical(add_col_dataset))
    stop(call. = FALSE,
         '`add_col_dataset` must be TRUE or FALSE (TRUE by default)')
  
  # test if group exists
  if(!is.null(group_by)){
    
    # test if harmonized_col_dataset exists
    bind_rows(
      as.list(harmonized_dossier) %>% lapply(function(x) x %>%
                                      mutate(across(everything(),as.character)))) %>%
      select(all_of(group_by))
  }
  
  # creation of pooled_harmonized_dataset
  if(is.null(pooled_harmonized_dataset)){
    pooled_harmonized_dataset <- 
      suppressWarnings(pooled_harmonized_dataset_create(
        harmonized_dossier = harmonized_dossier,
        harmonized_col_dataset = group_by,
        dataschema = dataschema,
        data_proc_elem = data_proc_elem,
        add_col_dataset = TRUE))}

  if(is.null(harmonized_dossier_summary)){
    harmonized_dossier_summary <-
      harmonized_dossier_summarize(
        harmonized_dossier = harmonized_dossier,
        group_by = group_by,
        dataschema = dataschema,
        data_proc_elem = data_proc_elem,
        pooled_harmonized_dataset = pooled_harmonized_dataset,
        taxonomy = taxonomy,
        valueType_guess = valueType_guess)
    }
  
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
  
  dataset_visualize(
    dataset = pooled_harmonized_dataset,
    group_by = attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
    bookdown_path = bookdown_path,
    taxonomy = taxonomy,
    valueType_guess = valueType_guess,
    dataset_summary = harmonized_dossier_summary)
  
}
