#' @title
#' Generate a report of a harmonized dossier (list of harmonized datasets)
#'
#' @description
#' Assesses the content and structure of a harmonized dossier object (list of 
#' harmonized datasets) and reports possible issues in the datasets and 
#' data dictionaries  to facilitate assessment of input data. The report can be 
#' used to help assess data structure, presence of fields, coherence across 
#' elements, and taxonomy or data dictionary formats. This report is compatible 
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
#' @param harmonized_dossier List of tibble(s), each of them being 
#' harmonized dataset.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A list of report(s), each of them being tibble(s) ('Overview and summary)
#' for each harmonized dataset.
#'
#' @examples
#' {
#' 
#' harmonized_dossier_evaluate(DEMO_files_harmo$harmonized_dossier)
#' 
#' }
#'
#' @import dplyr stringr tidyr haven
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
harmonized_dossier_evaluate <- function(harmonized_dossier, taxonomy = NULL){

  # future dev 
  # assess harmonized data dictionary
  # exclude impossible from the evaluation
  
  # check on arguments
  as_dossier(harmonized_dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  dataschema <- 
    attributes(harmonized_dossier)$`Rmonize::DataSchema` %>%
    as_dataschema(as_dataschema_mlstr = TRUE) %>%
    as_data_dict_mlstr()

  report_list <-
    dossier_evaluate(harmonized_dossier, as_data_dict_mlstr = TRUE)

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
#' Generate a quality assessment report of a Data Processing Elements
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Function that assesses the content and structure of a 
#' Data Processing Elements object and reports possible issues to facilitate 
#' assessment of input data. The report can be used to help assess 
#' data structure, presence of fields, coherence across elements, and taxonomy 
#' or data dictionary formats. This report is compatible with Excel and can be 
#' exported as an Excel spreadsheet.
#'
#' @details
#' A Data Processing Elements contains the rules and metadata that will be used 
#' to perform harmonization of input datasets in accordance with the DataSchema. 
#' It must be a data frame or data frame extension (e.g. a tibble) and it must 
#' contain certain columns which participate to the process, including the 
#' `dataschema_variable`, `input_dataset`,`input_variables`, 
#' `Mlstr_harmo::rule_category` and  `Mlstr_harmo::algorithm`. The mandatory 
#' first processing element must be "id_creation" in 
#' `Mlstr_harmo::rule_category` followed by the name of the column taken as 
#' identifier of each dataset to initiate the process of harmonization.
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
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A list of tibbles of report for one Data Processing Elements.
#'
#' @examples
#' {
#' 
#' # use DEMO_files_harmo provided by the package
#' 
#' data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final`   
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
    madshapR::add_index("Row number", .force = TRUE) 
  
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
    filter(!is.na(.data$`value`)) %>%
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
#' Generate a quality assessment report of the DataSchema
#'
#' @description
#' Assesses the content and structure of a DataSchema and reports potential
#' issues to facilitate the assessment of input data. The report can be used to 
#' help assess data structure, presence of fields, coherence across elements, 
#' and taxonomy or data dictionary formats. This report is compatible with Excel 
#' and can be exported as an Excel spreadsheet.
#'
#' @details
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
#' @param dataschema A list of tibble(s) representing metadata of an 
#' associated harmonized dossier.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A list of tibbles of report for the DataSchema.
#'
#' @examples
#' {
#'
#' # use DEMO_files_harmo provided by the package
#' 
#' library(dplyr)
#' library(madshapR) # data_dict_filter
#' 
#' dataschema <- 
#'   DEMO_files_harmo$`dataschema - final` %>%
#'   data_dict_filter("name == 'adm_unique_id'")
#'   
#' dataschema_evaluate(dataschema)
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

  report <- data_dict_evaluate(dataschema,taxonomy,as_data_dict_mlstr = TRUE)

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
