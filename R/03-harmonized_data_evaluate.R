#' @title
#' Generate an assessment report for a harmonized dossier
#'
#' @description
#' Assesses the content and structure of a harmonized dossier and generates 
#' reports of the results. This function can be used to evaluate data structure, 
#' presence of specific fields, coherence across elements, and data dictionary 
#' formats.
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal//), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#' 
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataschema A DataSchema object.
#' @param is_dataschema_mlstr
#'  Whether the output DataSchema should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. TRUE by default.
#'
#' @returns
#' A list of data frames containing assessment reports for each harmonized dataset.
#'
#' @examplesOK
#' {
#' 
#' # use Rmonize_DEMO provided by the package
#' library(stringr)
#' 
#' # perform data processing
#' dossier            <- Rmonize_examples[str_detect(names(Rmonize_examples),"dataset")]
#' dataschema         <- Rmonize_examples$DataSchema
#' data_proc_elem     <- Rmonize_examples$`Data Processing Elements`
#' harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' eval_harmo         <- harmonized_dossier_evaluate(harmonized_dossier)
#' 
#' glimpse(eval_harmo)
#' 
#' }
#'
#' @import dplyr stringr tidyr haven
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
harmonized_dossier_evaluate <- function(
    harmonized_dossier,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    taxonomy = NULL,
    is_dataschema_mlstr = TRUE){

  # future dev 
  # assess harmonized data dictionary
  # exclude impossible from the evaluation
  
  # tests
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(!is.logical(is_dataschema_mlstr))
    stop(call. = FALSE,
         '`is_dataschema_mlstr` must be TRUE or FALSE (FALSE by default)')
  
  # creation of pooled_harmonized_dataset
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(harmonized_dossier)

  report_list <-
    dataset_evaluate(
      dataset = pooled_harmonized_dataset,
      data_dict = dataschema,
      taxonomy = taxonomy,
      is_data_dict_mlstr = is_dataschema_mlstr)

  report_list <-
    report_list %>%
    lapply(function(x){
      
        names(x) <- str_replace(names(x),"Data dictionary summary",
                                "Harmonized Data dictionary summary")
        names(x) <- str_replace(names(x),"Data dictionary assessment",
                                "Harmonized Data dictionary assessement")
        names(x) <- str_replace(names(x),"Dataset assessment",
                                "Harmonized Dataset assessment")
    return(x)
    })

  return(report_list)
}

#' @title
#' Generate an assessment report for Data Processing Elements
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Assesses the content and structure of a Data Processing Elements object and 
#' generates reports of the results. This function can be used to evaluate data 
#' structure, presence of specific fields, coherence across elements, and data 
#' dictionary formats.
#'
#' @details
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
#' [Opal environment](https://www.obiba.org/pages/products/opal//), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @param data_proc_elem A Data Processing Elements object.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A list of data frames containing assessment reports.
#'
#' @examplesOK
#' {
#' 
#' # use Rmonize_DEMO provided by the package
#' 
#' data_proc_elem <- Rmonize_examples$`Data Processing Elements`
#' data_proc_elem_evaluate(data_proc_elem)
#' 
#' }
#'
#' @import dplyr fabR
#' @importFrom rlang .data
#' @importFrom crayon bold
#'
#' @export
data_proc_elem_evaluate <- function(data_proc_elem, taxonomy = NULL){

  data_proc_elem <- 
    as_data_proc_elem(data_proc_elem) %>%
    add_index("Row number", .force = TRUE) 
  
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  message(
"- DATA PROCESSING ASSESSMENT ------------------------------------------------")

  # creation of the structure of the report
  report <- list()
  
  report$`Data Processing Elements summary` <- data_proc_elem
  
  test_names_harmo_rule <-
    test_duplicated_rule <-
    test_possible_ruling <- 
    # ...
    tibble("Row number" = as.integer())
  
  message("    Assess the rule category declared")
  test_names_harmo_rule  <-
    data_proc_elem %>%
    mutate(
      value = 
        ifelse(
        .data$`Mlstr_harmo::rule_category` %in% c(
          "add_variable",
          "case_when",
          "direct_mapping",
          "id_creation",
          "impossible",
          "merge_variable",
          "operation",
          "other",
          "paste",
          "recode",
          "rename",
          "undetermined"),NA_character_,.data$`Mlstr_harmo::rule_category`)) %>%
    dplyr::filter(!is.na(.data$`value`)) %>%
    mutate(condition = "[ERR] - Rule category name doesn't exist") %>%
    select("Row number","value","condition") 
  
  report$`Data Processing Elements assessment` <-
    test_names_harmo_rule %>%
    bind_rows(test_duplicated_rule) %>%
    bind_rows(test_possible_ruling) %>%
    
    select("Row number", matches("value"), matches("condition")) %>%
    arrange(.data$`Row number`) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    distinct() %>% tibble
    
  message("    Generate report")
  
  if(nrow(report$`Data Processing Elements assessment`) == 0){
    message("\n    The Data Processing Elements contains no error/warning.")
    report$`Data Processing Elements assessment` <- NULL
  }
  
  message(bold(
    "
  - WARNING MESSAGES (if any): --------------------------------------------\n"))
  
  return(report)
  
  # futur dev
  #   dossier_name <- tibble(dossier = as.character(), dataset = as.character())
  #   for(i in names(dossier)) for(j in names(dossier[[i]])){
  #     dossier_name <- dossier_name %>% add_row(dossier = i, dataset = j)}
  #
  #   dpe_name <- tibble(dossier = as.character(), dataset = as.character())
  #   for(i in names(data_proc_elem)) for(j in names(data_proc_elem[[i]])){
  #     dpe_name <- dpe_name %>% add_row(dossier = i, dataset = j)}
  #
  #   no_dpe <-
  #     anti_join(dossier_name, dpe_name,by = c("dossier", "dataset")) %>%
  #     group_by(.data$`dossier`) %>%
  #     summarise(dataset = paste(dataset, collapse = " - ")) %>%
  #     ungroup() %>%
  #     unite("value", .data$`dossier`, .data$`dataset`, sep = " : ") %>%
  #     summarise(value = paste(.data$`value`, collapse = " \n")) %>% pull
  #
  #   no_dossier <-
  #     anti_join(dpe_name, dossier_name,by = c("dossier", "dataset")) %>%
  #     group_by(.data$`dossier`) %>%
  #     summarise(dataset = paste(dataset, collapse = " - ")) %>%
  #     ungroup() %>%
  #     unite("value", .data$`dossier`, .data$`dataset`, sep = " : ") %>%
  #     summarise(value = paste(.data$`value`, collapse = " \n")) %>% pull
  #
  #   if(inner_join(dossier_name, dpe_name,by = c("dossier", "dataset")) %>%
  #   nrow == 0){
  #     stop(
  #       "
  # The harmonization process has been interupted because some mismatch between
  # dataset(s) and Data Processing Elements have been found. Plese make
  # sure Data Processing Elements (such as input_dataset) match names
  # of your dataset(s)
  #
  # input dataset(s) names:\n",no_dpe,"\n",
  #       "\n and input_dataset(s) in dataprocessing elements:\n",
  #        no_dossier,
  #       "\n\n" )}
  #
  #   if(nchar(no_dpe)){
  #     warning(
  #       "\nNo Data Processing Elements found for:\n",no_dpe,
  #       "\nThese dataset will not be harmonized.\n", call. = FALSE)
  #
  #   }
  #
  #   if(nchar(no_dossier)){
  #     warning(
  #       "\nNo dataset found for:\n",no_dossier,
  #       "\nThese Data Processing Elements have not been processed.\n",
  #            call. = FALSE)
  #
  #   }

}

#' @title
#' Generate an assessment report for a DataSchema
#'
#' @description
#' Assesses the content and structure of a DataSchema object and generates 
#' reports of the results. This function can be used to evaluate data structure, 
#' presence of specific fields, coherence across elements, and data dictionary 
#' formats.
#'
#' @details
#' A DataSchema is the list of core variables to generate across datasets and 
#' related metadata. A DataSchema object is a list of data frames with elements 
#' named 'Variables' (required) and 'Categories' (if any). The 'Variables' 
#' element must contain at least the `name` column, and the 'Categories' 
#' element must contain at least the `variable` and `name` columns to be usable 
#' in any function. In 'Variables' the `name` column must also have unique 
#' entries, and in 'Categories' the combination of `variable` and `name` columns 
#' must also be unique. 
#' 
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal//), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @param dataschema A DataSchema object.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A list of data frames containing assessment reports.
#'
#' @examplesOK
#' {
#'
#' # use Rmonize_DEMO provided by the package
#' 
#' library(dplyr)
#' 
#' dataschema <- Rmonize_examples$`DataSchema`
#' eval_dataschema <- dataschema_evaluate(dataschema)
#' 
#' glimpse(eval_dataschema)
#' 
#' }
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @export
dataschema_evaluate <- function(dataschema, taxonomy = NULL){

  # dataschema <-
  #   as_dataschema(dataschema,as_dataschema_mlstr = TRUE) %>%
  #   as_data_dict_mlstr()

  report <- data_dict_evaluate(dataschema,taxonomy,is_data_dict_mlstr = TRUE)

  names(report) <- str_replace(names(report),"Data dictionary summary",
                               "Harmonized Data dictionary summary")
  names(report) <- str_replace(names(report),"Data dictionary assessment",
                               "Harmonized Data dictionary assessement")
  
  #   dossier_name <- tibble(dossier = as.character(), dataset = as.character())
  #   for(i in names(dossier)) for(j in names(dossier[[i]])){
  #     dossier_name <- dossier_name %>% add_row(dossier = i, dataset = j)}
  #
  #   dpe_name <- tibble(dossier = as.character(), dataset = as.character())
  #   for(i in names(data_proc_elem)) for(j in names(data_proc_elem[[i]])){
  #     dpe_name <- dpe_name %>% add_row(dossier = i, dataset = j)}
  #
  #   no_dpe <-
  #     anti_join(dossier_name, dpe_name,by = c("dossier", "dataset")) %>%
  #     group_by(.data$`dossier`) %>%
  #     summarise(dataset = paste(dataset, collapse = " - ")) %>%
  #     ungroup() %>%
  #     unite("value", .data$`dossier`, .data$`dataset`, sep = " : ") %>%
  #     summarise(value = paste(.data$`value`, collapse = " \n")) %>% pull
  #
  #   no_dossier <-
  #     anti_join(dpe_name, dossier_name,by = c("dossier", "dataset")) %>%
  #     group_by(.data$`dossier`) %>%
  #     summarise(dataset = paste(dataset, collapse = " - ")) %>%
  #     ungroup() %>%
  #     unite("value", .data$`dossier`, .data$`dataset`, sep = " : ") %>%
  #     summarise(value = paste(.data$`value`, collapse = " \n")) %>% pull
  #
  #   if(inner_join(dossier_name, dpe_name,by = c("dossier", "dataset")) %>%
  #       nrow == 0){
  #     stop(
  #       "
  # The harmonization process has been interupted because some mismatch between
  # dataset(s) and Data Processing Elements have been found. Plese make
  # sure Data Processing Elements (such as input_dataset) match names
  # of your datasets
  #
  # dossier(s) and input dataset(s) names:\n",no_dpe,"\n",
  #       "\n and input_dataset(s) in dataprocessing elements:\n",
  #          no_dossier,
  #       "\n\n" )}
  #
  #   if(nchar(no_dpe)){
  #     warning(
  #       "\nNo Data Processing Elements found for:\n",no_dpe,
  #       "\nThese dataset will not be harmonized.\n", call. = FALSE)
  #
  #   }
  #
  #   if(nchar(no_dossier)){
  #     warning(
  #       "\nNo dataset found for:\n",no_dossier,
  #       "\nThese Data Processing Elements have not been processed.\n",
  #          call. = FALSE)
  #
  #   }



  return(report)
}
