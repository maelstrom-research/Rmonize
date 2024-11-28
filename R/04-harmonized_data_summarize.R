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
#' [Opal environment](https://www.obiba.org/pages/products/opal//), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
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
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#'
#' @returns
#' A list of data frames containing overall assessment reports and summaries 
#' grouped by harmonized dataset.
#'
#' @examples
#' {
#' 
#' # use Rmonize_examples provided by the package
#' library(dplyr)
#' library(stringr)
#' 
#' # perform data processing
#' harmonized_dossier <-
#'   Rmonize_examples[str_detect(names(Rmonize_examples),"harmonized_dossier")][[1]]
#' 
#' summary_harmo  <- harmonized_dossier_summarize(harmonized_dossier)
#' 
#' glimpse(summary_harmo)
#'
#' }
#'
#' @import dplyr stringr tidyr
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
harmonized_dossier_summarize <- function(harmonized_dossier){

  # creation of pooled_harmonized_dataset
  pooled_harmonized_dataset <- 
    suppressWarnings(pooled_harmonized_dataset_create(harmonized_dossier))
  
  # suppressWarnings(pooled_harmonized_dataset_create(
  #   harmonized_dossier = harmonized_dossier,harmonized_col_dataset = NULL))
  
  harmonized_dossier_summary <-
    dataset_summarize(
      dataset = pooled_harmonized_dataset,
      group_by = attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`)
  
  ## change content

  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Overview","Harmonization overview")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Dataset assessment","Harmo dataset assessment")
  names(harmonized_dossier_summary) <- 
    str_replace(names(harmonized_dossier_summary),
                "Data dictionary assessment","Harmo data dictionary assessment")

  harmonized_dossier_summary <- 
    harmonized_dossier_summary %>%
    lapply(function(x){
      names(x) <- str_replace(names(x),
                              "Data dictionary assessment","Harmo data dictionary assessment")
      names(x) <- str_replace(names(x),
                              "Dataset assessment","Harmo dataset assessment")
      return(x)
    })
  
  # add the harmo_status in the summary
  
  harmonized_col_id      <- attributes(harmonized_dossier)$`Rmonize::harmonized_col_id`
  harmonized_col_dataset <- attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`
  data_proc_elem         <- attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`
  dataschema             <- attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  dataschema_labels <- data_dict_trim_labels(dataschema)
  
  data_proc_elem <-
    data_proc_elem %>%
    bind_rows(tibble(
      "Rmonize::error_status" = as.character(),
      "Rmonize::warning_status" = as.character())) %>%
    select(
      "name_var" = "dataschema_variable",
      "Harmonization status" = "Mlstr_harmo::status", 
      "group_index" = "input_dataset", 
      "Rmonize::error_status", 
      "Rmonize::warning_status") %>%
    group_by(pick("group_index")) %>% left_join(
      dataschema_labels$Variables %>% select("name_var" = "name","Variable name"),
      by = "name_var")
  
  harmo_status <-
    data_proc_elem %>% 
    group_split() %>%
    lapply(function(x){
    
      has_error = !all(is.na(x[["Rmonize::error_status"]]))
      if(has_error){
        x["Rmonize::error_status"] <-
          x["Rmonize::error_status"] %>%
          mutate(
            "Rmonize::error_status" = ifelse(
              is.na(.data$`Rmonize::error_status`),
              .data$`Rmonize::error_status`,"**ERROR**"))}
      
      has_warning = all(is.na(x[["Rmonize::warning_status"]]))
      if(has_warning){
        x["Rmonize::warning_status"] <-
          x["Rmonize::warning_status"] %>%
          mutate(
            "Rmonize::warning_status" = ifelse(
              is.na(.data$`Rmonize::warning_status`),
              .data$`Rmonize::warning_status`,"(warning)"))}
      
      x = x %>%
        mutate(
          "Harmonization status" = ifelse(
            (is.na(.data$`Rmonize::error_status`) & 
              is.na(.data$`Rmonize::warning_status`)) ,
            .data$`Harmonization status`, NA_character_)) %>%
        unite("Harmonization status",
              c("Harmonization status",
                "Rmonize::error_status","Rmonize::warning_status"),sep = "", na.rm = TRUE)
        
      return(x)}) %>% bind_rows() %>%
  
    mutate(group_index = ifelse(.data$`Variable name` == harmonized_col_id,NA,.data$`group_index`)) %>%
    mutate(group_index = ifelse(.data$`Variable name` == harmonized_col_dataset,NA,.data$`group_index`)) %>%
    filter(!is.na(.data$`group_index`)) %>%
    group_by(.data$`group_index`) %>%
    mutate(group_index = cur_group_id())
  

  name_group_short <- 
    dataschema_labels$Variables[dataschema_labels$Variables$name == harmonized_col_dataset,"Variable name"][[1]]
  group_var_name <- paste0("Grouping variable: ",name_group_short)
  
  for(i in names(harmonized_dossier_summary)[
    str_detect(names(harmonized_dossier_summary),"(V|v)ariable")]){
    # stop()}

    harmonized_dossier_summary[[i]] <-
      harmonized_dossier_summary[[i]] %>%
      left_join(
        harmonized_dossier_summary[[i]] %>%
          mutate(group_index = !!as.name(group_var_name)) %>% 
          select(any_of(group_var_name),"group_index","Variable name") %>%
          mutate(group_index = ifelse(str_detect(.data$`group_index`,"(all)"),NA,.data$`group_index`)) %>%
          filter(!is.na(.data$`group_index`)) %>%
          group_by(.data$`group_index`) %>%
          mutate(group_index = cur_group_id()) %>%
          left_join(harmo_status,by = c('group_index', 'Variable name')),
        
        by = c(group_var_name, "Variable name")) %>% 
      select('Index',!!group_var_name,"Variable name","Variable label",
             "Harmonization status",everything(),-"group_index") %>%
      mutate("Harmonization status" = replace_na(.data$`Harmonization status`,"complete")) %>% select(-"name_var")}

  # add harmonization statuses in the dataset assessment
  # harmonized_dossier_summary[['Harmo dataset assessment']] <- 
  #   harmonized_dossier_summary[['Harmo dataset assessment']] %>% 
  #   bind_rows(tibble('Variable name' = as.character()))
  
 # if has error, modify the message for the study affected
  # has_error <- 
  #   harmonized_dossier_summary[['Variables summary (all)']] %>%
  #   filter(str_detect(.data$`Harmonization status`,"ERROR")) %>%
  #   select("Category codes and labels long" = starts_with("Grouping")) %>% 
  #   inner_join(dataschema_labels[["Categories"]],by = "Category codes and labels long") %>%
  #   select(Value = "Category codes and labels long","name")
  #   
  
  # if(nrow(has_error) > 0){

    # harmonized_dossier_summary[['Harmo dataset assessment']] <- 
    #   harmonized_dossier_summary[['Harmo dataset assessment']] %>%
#       mutate(
#         'Variable name' = 
#           ifelse(.data$`Value` %in% has_error$`name` & str_detect(
#             .data$`Harmo dataset assessment` , 
# "Grouping variable has empty group \\(no participant\\)"),
# "(all)",.data$`Variable name`), 

#         'Harmo dataset assessment' = 
#           ifelse(.data$`Value` %in% has_error$`name` & str_detect(
#             .data$`Harmo dataset assessment` , 
# "Grouping variable has empty group \\(no participant\\)"),
# "[ERROR] - The Data Processing Elements contain 'error' statuses for this study.", # [GF] to validate
#           .data$`Harmo dataset assessment`),

        # 'Value' = 
        # ifelse(.data$`Value` %in% has_error$`name` & str_detect( 
        #   .data$`Harmo dataset assessment` , "The Data Processing Elements"),
        #   has_error$name,
        #   .data$`Value`))
    
    # }
  
  # has_undet <- 
  #   harmonized_dossier_summary[['Variables summary (all)']] %>%
  #   filter(str_detect(.data$`Harmonization status`,"undetermined")) %>%
  #   select("Variable name", "Category codes and labels long" = starts_with("Grouping")) %>% 
  #   inner_join(dataschema_labels[["Categories"]],by = "Category codes and labels long") %>% 
  #   select("Variable name", "Value" = "name") %>%
  #   arrange(pick("Value")) %>%
  #   group_by(pick("Value")) %>%
  #   slice(1:4) %>% 
  #   mutate("Variable name" = ifelse(
  #     row_number() == 4,
  #     '...', 
  #     .data$`Variable name`)) %>%
  #   reframe("Variable name" = paste0(.data$`Variable name`,collapse = " ; ")) %>%
  #   mutate(
  #     'Harmo dataset assessment' = "[INFO] - The data Processing Elements contain 'undetermined' statuses.")
  
  
    # harmonized_dossier_summary[['Harmo dataset assessment']] <- 
    #   harmonized_dossier_summary[['Harmo dataset assessment']] %>%
    #   bind_rows(has_undet)
    # 
    # all_impossible <- 
    #   harmonized_dossier_summary[['Variables summary (all)']] %>%
    #   select("Variable name", 
    #          "Harmonization status",
    #          "Category codes and labels long" = starts_with("Grouping")) %>%
    #   inner_join(dataschema_labels[["Categories"]],by = "Category codes and labels long") %>% 
    #   filter(str_detect(.data$`Harmonization status`,"impossible")) %>%
    #   group_by(pick("name")) %>%
    #   count(.data$`name`) %>%
    #   mutate(
    #     count_dataschema_var = nrow(dataschema$Variables)-1- .data$`n`) %>%
    #   dplyr::filter(.data$`count_dataschema_var` == 0) %>%
    #   select("Value" = "name") %>%
    #   mutate(
    #     "Variable name" = "(all)",
    #     'Harmo dataset assessment' = "[INFO] - The data Processing Elements contain 'impossible' statuses only.") %>%
    #   ungroup
    # 
    # harmonized_dossier_summary[['Harmo dataset assessment']] <- 
    #   harmonized_dossier_summary[['Harmo dataset assessment']] %>%
    #   bind_rows(all_impossible)
    
    # if(nrow(harmonized_dossier_summary[['Harmo dataset assessment']]) == 0){
    #   harmonized_dossier_summary[['Harmo dataset assessment']] <- 
    #     harmonized_dossier_summary[['Harmo dataset assessment']] %>% 
    #     mutate(
    #       'Variable name' = "(all)", 
    #       'Harmo dataset assessment' = "[INFO] - No error/warning detected.") # [GF] to validate
    #   }
    
    # replace the name of the datasets in the grouping variable column
    # for(i in names(harmonized_dossier_summary)[
    #   str_detect(names(harmonized_dossier_summary),"(V|v)ariable")]){
    #   
    #   names(harmonized_dossier_summary[[i]]) <- 
    #     str_replace(names(harmonized_dossier_summary[[i]]),group_var_name,"Harmonized dataset")
    #   # names(x) <- str_replace(names(x),"Variable name","Harmonized dataset")  # [GF] Dataschema variable name? 
    #   
    #   harmonized_dossier_summary[[i]] <- 
    #     harmonized_dossier_summary[[i]] %>%
    #     left_join(
    #       dataschema_labels$Categories %>% select(
    #         "name_var" = "name",
    #         "Harmonized dataset" = "Category codes and labels long"),
    #       by = "Harmonized dataset") %>%
    #     mutate("Harmonized dataset" = ifelse(is.na(.data$`name_var`),.data$`Harmonized dataset`,.data$`name_var`)) %>%
    #     mutate("Quality assessment comment" = 
    #              str_replace_all(.data$`Quality assessment comment`, "\\[INFO\\] - Identifier variable\\.", # [GF] wording to validate
    #              "[INFO] - Harmonized identifier variable.")) %>%
    #     mutate("Quality assessment comment" =
    #              str_replace_all(.data$`Quality assessment comment`, "\\[INFO\\] - Grouping variable\\.",   # [GF] wording to validate
    #              "[INFO] - Harmonized dataset identifier variable.")) %>%
    #     mutate("Quality assessment comment" = 
    #              str_replace_all(.data$`Quality assessment comment`, "\\[INFO\\] - Empty group\\.",        # [GF] wording to validate
    #                              "[ERROR] - 'error' statuses for this study.")) %>%
    #     select(-"name_var")}
    # 
    # 
    # study_names <-  
    #   dataschema_labels[["Categories"]] %>%
    #   dplyr::filter(.data$`variable` == harmonized_col_dataset) %>%
    #   pull("name")
    # 
    # names(harmonized_dossier_summary$`Harmonization overview`) <- 
    #   c("Harmonization overview","(all)",study_names)
    #   
    # index <- 
    #   harmonized_dossier_summary$`Harmonization overview`[1] %>%
    #   add_index %>% 
    #   dplyr::filter(.data$`Harmonization overview` == "    Rows") %>% 
    #   pull("index") 
    # 
    # harmonized_dossier_summary$`Harmonization overview`[
    #   index,c(3:ncol(harmonized_dossier_summary$`Harmonization overview`))] <- 
    #   as.list(study_names)
    # 
    # harmonized_dossier_summary$`Harmonization overview` <- 
    #   harmonized_dossier_summary$`Harmonization overview` %>%
    #   mutate(across(everything(),~str_replace_all(.,"\\(empty\\)","(error)")))        # [GF] wording to validate
    # 
    # harmonized_dossier_summary$`Harmonization overview` <- 
    #   harmonized_dossier_summary$`Harmonization overview` %>%
    #   mutate(
    #     "Harmonization overview" <- str_replace_all(.data$`Harmonization overview`,
    #       "    Grouping variable","    Harmonized identifier variable"))        # [GF] wording to validate
    
  return(harmonized_dossier_summary)
}
