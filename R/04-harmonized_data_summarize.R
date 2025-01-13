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

  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") %>%
      str_squish()
    x = x[!is.na(x)]
    
    return(x)}
  
  # creation of pooled_harmonized_dataset
  pooled_harmonized_dataset <- 
    suppressWarnings(pooled_harmonized_dataset_create(harmonized_dossier))
  
  harmo_data_dict <- 
    attr(pooled_harmonized_dataset,"madshapR::Data dictionary")
  
  harmonized_col_id      <- toString(attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_id`)
  harmonized_col_dataset <- toString(attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`)
  
  data_proc_elem         <- attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`
  dataschema             <- attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  # remove empty groups, because of non presence of the dataset name
  if(harmonized_col_dataset != "Rmonize::harmonized_col_dataset"){
    
    dataset_present <- names(harmonized_dossier)
    
    if(has_categories(harmo_data_dict)){
      
      dataset_present <- 
        data_proc_elem %>% 
        dplyr::filter(input_dataset %in% dataset_present & dataschema_variable == harmonized_col_dataset) %>%
        pull("Mlstr_harmo::algorithm") %>% extract_var
      
      harmo_data_dict[['Categories']] <- 
        harmo_data_dict[['Categories']] %>%
        dplyr::filter(!(variable == harmonized_col_dataset & !(name %in% dataset_present)))
    
      pooled_harmonized_dataset <- 
        pooled_harmonized_dataset %>%
        data_dict_apply(harmo_data_dict)
    }
  }
    
  harmonized_dossier_summary <-
    dataset_summarize(
      dataset = pooled_harmonized_dataset,
      data_dict = harmo_data_dict,
      group_by = attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`,
      valueType_guess = FALSE)
  
  harmo_data_dict <- 
    harmo_data_dict %>%  
    data_dict_trim_labels()

  harmonized_col_id_short <-
    harmo_data_dict$Variables$`Variable name`[
      harmo_data_dict$Variables$`name` ==
        attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_id`]
  
  harmonized_col_dataset_short <-
    harmo_data_dict$Variables$`Variable name`[
      harmo_data_dict$Variables$`name` ==
        attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset`]
  
  group_var_name <- paste0("Grouping variable: ",harmonized_col_dataset_short)
  
  if(has_categories(harmo_data_dict)){
    harmo_data_dict$`Categories` <- 
      harmo_data_dict$`Categories` %>%
      left_join(
        harmo_data_dict$`Variables` %>% select("variable" = "name", "Variable name"),
        by = "variable")}
  
  # suppressWarnings(pooled_harmonized_dataset_create(
  #   harmonized_dossier = harmonized_dossier,harmonized_col_dataset = NULL))

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


  # add harmonization statuses in the dataset assessment
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
      harmo_data_dict$Variables %>% select("name_var" = "name","Variable name"),
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
    group_by(.data$`group_index`) %>%
    mutate(group_index = cur_group_id())
  
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
             "Harmonization status",everything(),-"group_index",-"name_var") %>%
      mutate("Harmonization status" = replace_na(.data$`Harmonization status`,"complete")) # %>% select(-"name_var") 
    
    # %>% select(any_of(group_var_name),"Harmonization status",`Variable name`)
    }

  
  ########### GENERAL MESSAGES in Harmo dataset assessment sheet
  
  # if Rmonize::harmonized_col_dataset, suppress the comment corresponding to 
  # dataset and data dict assessment
  if(harmonized_col_dataset == "Rmonize::harmonized_col_dataset"){
    
    harmonized_dossier_summary[['Harmo dataset assessment']] <-
      harmonized_dossier_summary[['Harmo dataset assessment']] %>%
      bind_rows(tibble(
        'Variable name' = as.character(),
        'Harmo dataset assessment' = as.character())) %>% 
      filter(!(`Variable name` == harmonized_col_dataset_short &
                 str_detect(`Harmo dataset assessment`,"Variable names contain special characters, contain spaces, or begin with a number")))

    harmonized_dossier_summary[['Harmo data dictionary assessment']] <-
      harmonized_dossier_summary[['Harmo data dictionary assessment']] %>%
      bind_rows(tibble(
        'Variable name' = as.character(),
        'Harmo data dictionary assessment' = as.character())) %>% 
      filter(!(`Variable name` == harmonized_col_dataset_short &
                 str_detect(`Harmo data dictionary assessment`,"Variable names contain special characters, contain spaces, or begin with a number"))) 
        
  }
  
  # if harmonized_col_dataset has a constant value (meaning only one study), 
  # suppress the comment corresponding to dataset assessment
  if(length(harmonized_dossier) == 1){
    
    harmonized_dossier_summary[['Harmo dataset assessment']] <-
      harmonized_dossier_summary[['Harmo dataset assessment']] %>%
      bind_rows(tibble(
        'Variable name' = as.character(),
        'Harmo dataset assessment' = as.character())) %>% 
      filter(!(`Variable name` == harmonized_col_dataset_short &
                 str_detect(`Harmo dataset assessment`,"Variable has a constant value")))
  }
  
  # [GF] FURTHER DISCUSSIONS
  # if(length(harmonized_dossier) == 1 & FALSE){
  #   
  #   imposs_status <- 
  #     harmonized_dossier_summary$`Variables summary (all)` %>% 
  #     select("Variable name", "Harmonization status") %>% 
  #     dplyr::filter(`Harmonization status` == 'impossible')
  #   
  #   harmonized_dossier_summary[['Harmo dataset assessment']] <-
  #     harmonized_dossier_summary[['Harmo dataset assessment']] %>%
  #     bind_rows(tibble(
  #       'Variable name' = as.character(),
  #       'Harmo dataset assessment' = as.character())) %>% 
  #     filter(!(`Variable name` %in% imposs_status$`Variable name` &
  #                str_detect(`Harmo dataset assessment`,"Empty variable")))
  # }



  ### HANDLE ERRORS
  # has_error <-
  # harmonized_dossier_summary[['Variables summary (all)']] %>%
  # filter(str_detect(.data$`Harmonization status`,"ERROR")) %>%
  # select("Value" = starts_with("Grouping")) %>%
  #   mutate(
  #     "Variable name" = "(all)", 
  #     'Harmo dataset assessment' = "[ERROR] - There were errors in the processing of this dataset.") %>%
  #   distinct
  # 
  
  has_error <-
    harmonized_dossier_summary[['Variables summary (all)']] %>%
    filter(str_detect(.data$`Harmonization status`,"ERROR")) %>%
    select("Categories in data dictionary long" = starts_with("Grouping")) %>%
    inner_join(harmo_data_dict[["Categories"]],by = "Categories in data dictionary long") %>%
    select(Value = "Categories in data dictionary long","Variable name") %>%
    mutate(
      Value2 = Value, 
      'Harmo dataset assessment' = '[ERROR] - There were errors in the processing of this dataset.') %>%
    select(Value = starts_with("Grouping"),"Variable name","Value2","Harmo dataset assessment") 
  
  if(nrow(has_error) > 0){

    harmonized_dossier_summary[['Harmo dataset assessment']] <-
      harmonized_dossier_summary[['Harmo dataset assessment']] %>%
      bind_rows(tibble("Index" = as.character())) %>%
      select("Index",everything()) %>%
      rowwise %>%
      mutate(
        'Harmo dataset assessment' =
          ifelse(.data$`Variable name` %in% has_error$`Variable name` & str_detect(
            .data$`Harmo dataset assessment` ,
            "Grouping variable has empty group \\(group with no participants\\)"),
            "[ERROR] - There were errors in the processing of this dataset.",
            .data$`Harmo dataset assessment`),
        'Index' =
          ifelse(.data$`Variable name` %in% has_error$`Variable name` & str_detect(
            .data$`Harmo dataset assessment` ,
            "There were errors in the processing of this dataset"),
            NA_character_,.data$`Index`),
        'Variable name' =
          ifelse(.data$`Variable name` %in% has_error$`Variable name` & str_detect(
            .data$`Harmo dataset assessment` ,
            "There were errors in the processing of this dataset"),
            "(all)",.data$`Variable name`)) %>%
          ungroup}
  
  
  if(nrow(has_error) > 0){
    # if only one harmo dataset, remove empty variable information in the dataset assessment
    # [GF] comment: may bug if dataset has 0 rows because there is no participants at the first place
    if(length(harmonized_dossier) == 1){
      harmonized_dossier_summary[['Harmo dataset assessment']] <-
        harmonized_dossier_summary[['Harmo dataset assessment']] %>% 
        dplyr::filter(!(
          .data$`Variable name` == "(all)" & 
            str_detect(`Harmo dataset assessment`,"The dataset has 0 rows"))) %>%
        bind_rows(has_error %>% mutate("Variable name" = '(all)', "Value" = .data$`Value2`) %>% select(-Value2))
    }
    
    # [GF] comment: may bug if dataset has 0 rows because there is no participants at the first place
    # harmonized_dossier_summary[['Harmo dataset assessment']] <- 
    #   harmonized_dossier_summary[['Harmo dataset assessment']] %>%
    #   dplyr::filter(!(
    #     str_detect(.data$`Harmo dataset assessment`,
    #                "Grouping variable has empty group \\(group with no participants\\)")))
    
    # for(i in names(harmonized_dossier_summary)[
    #   str_detect(names(harmonized_dossier_summary),"(V|v)ariable")]){
    #   # stop()}
    #   
    #   harmonized_dossier_summary[[i]] <-
    #     harmonized_dossier_summary[[i]] %>%
    #     mutate("Quality assessment comment" = 
    #              ifelse(str_detect(.data$`Harmonization status`,"ERROR"),"[ERROR] - Error in the process.",
    #                     .data$`Quality assessment comment`))
    #   
    #   harmonized_dossier_summary[[i]] <-
    #     harmonized_dossier_summary[[i]] %>%
    #     mutate("Quality assessment comment" = 
    #              ifelse(str_detect(.data$`Quality assessment comment`,"Empty group."),"[INFO] - The dataset has 0 rows.",
    #                     .data$`Quality assessment comment`))
    #   }
  }
  
  ### HANDLE UNDETERMINED
  has_undet <-
    harmonized_dossier_summary[['Variables summary (all)']] %>%
    filter(str_detect(.data$`Harmonization status`,"undetermined")) %>%
    select("Value" = starts_with("Grouping")) %>%
    mutate(
      "Variable name" = "(all)", 
      'Harmo dataset assessment' = "[INFO] - The Data Processing Elements contain 'undetermined' statuses.") %>%
    distinct
  
  # if only one harmo dataset, remove empty variable information in the 
  # dataset assessment
  
  if(nrow(has_undet) > 0){
    
    harmonized_dossier_summary[['Harmo dataset assessment']] <-
      harmonized_dossier_summary[['Harmo dataset assessment']] %>%
      bind_rows(has_undet)
  
    if(length(harmonized_dossier) == 1){
      harmonized_dossier_summary[['Harmo dataset assessment']] <-
        harmonized_dossier_summary[['Harmo dataset assessment']] %>%
        dplyr::filter(!(
          .data$`Variable name` == has_undet$`Variable name` & 
            str_detect(`Harmo dataset assessment`,"Empty variable")))}
    
      # for(i in names(harmonized_dossier_summary)[
      #   str_detect(names(harmonized_dossier_summary),"(V|v)ariable")]){
      # stop()}
      
      # harmonized_dossier_summary[[i]] <-
      #   harmonized_dossier_summary[[i]] %>%
      #   mutate("Quality assessment comment" = 
      #            ifelse(str_detect(.data$`Harmonization status`,"undetermined"),"[INFO] - Undetermined status.",
      #                   .data$`Quality assessment comment`))
      
      # }
  }
  

  

  

  # all_impossible <-
  #   harmonized_dossier_summary[['Variables summary (all)']] %>%
  #   select("Variable name",
  #          "Harmonization status",
  #          "Categories in data dictionary long" = starts_with("Grouping")) %>%
  #   inner_join(harmo_data_dict[["Categories"]],by = "Categories in data dictionary long") %>%
  #   filter(str_detect(.data$`Harmonization status`,"impossible")) %>%
  #   group_by(pick("name")) %>%
  #   count(.data$`name`) %>%
  #   mutate(
  #     count_dataschema_var = nrow(dataschema$Variables)-1- .data$`n`) %>%
  #   dplyr::filter(.data$`count_dataschema_var` == 0) %>%
  #   select("Value" = "name") %>%                                             # [GF] FUTURE DEV TO DISCUSS
  #   mutate(
  #     "Variable name" = "(all)",
  #     'Harmo dataset assessment' = "[INFO] - The data Processing Elements contain 'impossible' statuses only.") %>%
  #   ungroup
  #
  # harmonized_dossier_summary[['Harmo dataset assessment']] <-
  #   harmonized_dossier_summary[['Harmo dataset assessment']] %>%
  #   bind_rows(all_impossible)
  
    ########### MESSAGES in Harmo dataset assessment column
    
    # replace the name of the datasets in the grouping variable column
    for(i in names(harmonized_dossier_summary)[
      str_detect(names(harmonized_dossier_summary),"(V|v)ariable")]){
      # stop()}

      # names(harmonized_dossier_summary[[i]]) <-
      #   str_replace(names(harmonized_dossier_summary[[i]]),group_var_name,"Grouping variable") # [GF] QUESTION : to validate. "Harmonized dataset name" ?
      # names(x) <- str_replace(names(x),"Variable name","Dataschema variable name")  # [GF] FUTURE DEV  
      
      harmonized_dossier_summary[[i]] <-
        harmonized_dossier_summary[[i]] %>%
        # left_join(
        #   harmo_data_dict$Categories %>% select(
        #     "name_var" = "name",
        #     "Grouping variable" := "Categories in data dictionary long"),
        #   by = "Grouping variable") %>%
        rowwise() %>%
        mutate("Quality assessment comment" =
                 str_replace_all(.data$`Quality assessment comment`, "\\[INFO\\] - Identifier variable\\.",
                 "[INFO] - Harmonized identifier variable.")) %>%
        
        # [GF] COMMENT : when the harmo status is paste, we do keep '[INFO] - Variable has a constant value', which is normal.
        mutate("Quality assessment comment" =
                 ifelse(harmonized_col_dataset %in% "Rmonize::harmonized_col_dataset",
                        str_replace_all(
                          .data$`Quality assessment comment`, "\\[INFO\\] - Grouping variable\\.",NA_character_),
                        str_replace_all(
                          .data$`Quality assessment comment`, "\\[INFO\\] - Grouping variable\\.","[INFO] - Harmonized dataset identifier variable."))) %>%
        mutate("Quality assessment comment" =
                 str_replace_all(.data$`Quality assessment comment`, "\\[INFO\\] - Empty group\\.",        
                                 "[ERROR] - Error in the process.")) %>%
        ungroup() # %>% select(-"name_var")
      }


    
    # rename the groups by the name of the datasets if 
    dataset_names <-
      harmo_data_dict[["Categories"]] %>%
      dplyr::filter(.data$`Variable name` == harmonized_col_dataset_short) %>%
      pull(first_label_get(harmo_data_dict)['Categories']) %>% str_trunc(31)

    names(harmonized_dossier_summary$`Harmonization overview`) <-
      c("Harmonization overview","(all)",dataset_names)

    index <-
      harmonized_dossier_summary$`Harmonization overview`[1] %>%
      add_index %>%
      dplyr::filter(.data$`Harmonization overview` == "    Rows") %>%
      pull("index")

    harmonized_dossier_summary$`Harmonization overview`[
      index,c(3:ncol(harmonized_dossier_summary$`Harmonization overview`))] <-
      as.list(dataset_names)

    # harmonized_dossier_summary$`Harmonization overview` <-
    #   harmonized_dossier_summary$`Harmonization overview` %>%
    #   mutate(across(everything(),~str_replace_all(.,"\\(empty\\)","(error)")))        # [GF] FUTURE DEV

    harmonized_dossier_summary$`Harmonization overview` <-
      harmonized_dossier_summary$`Harmonization overview` %>%
      mutate(
        "Harmonization overview" = str_replace_all(.data$`Harmonization overview`,
          "    Identifier variable","    Harmonized identifier variable"))  
    
    harmonized_dossier_summary$`Harmonization overview` <-
      harmonized_dossier_summary$`Harmonization overview` %>%
      mutate(
        "Harmonization overview" = str_replace_all(.data$`Harmonization overview`,
         "    Grouping variable","    Harmonized dataset variable"))  
    
    # suppress "Rmonize::harmonized_col_dataset" everywhere needed
    # in overview
    if(harmonized_col_dataset == "Rmonize::harmonized_col_dataset" & FALSE){
      
      variable_label <-              'Dataset names (added to the data dictionary as a categorical variable).' # [GF] Text to validate
      assessment_comment <- '[INFO] - Dataset names (added to the data dictionary as a categorical variable).'
      
      harmonized_dossier_summary$`Harmonization overview` <-
        harmonized_dossier_summary$`Harmonization overview` %>%
        filter(.data$`Harmonization overview` != "    Harmonized dataset variable")
      
      # in data dictionary assessment
      harmonized_dossier_summary[['Harmo data dictionary assessment']] <-
        harmonized_dossier_summary[['Harmo data dictionary assessment']] %>%
        bind_rows(tibble("Variable name" = as.character())) %>%
        filter(.data$`Variable name` != harmonized_col_dataset_short)

      # in dataset assessment
      harmonized_dossier_summary[['Harmo dataset assessment']] <-
        harmonized_dossier_summary[['Harmo dataset assessment']] %>%
        bind_rows(tibble("Variable name" = as.character())) %>%
        mutate("Variable name" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, '(all)', .data$`Variable name`))

      # in Variable summary (all)
      harmonized_dossier_summary[['Variables summary (all)']] <-
        harmonized_dossier_summary[['Variables summary (all)']] %>%
        mutate("Variable label" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, variable_label, .data$`Variable label`)) %>% 
        mutate("Quality assessment comment" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, assessment_comment, .data$`Quality assessment comment`)) %>% # [GF] to validate.
        mutate("Variable name" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, '(all)', .data$`Variable name`))
      # filter(.data$`Variable name` != harmonized_col_dataset_short)

      # in Categorical variable summary
      harmonized_dossier_summary[['Categorical variable summary']] <-
        harmonized_dossier_summary[['Categorical variable summary']] %>%
        mutate("Variable label" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, variable_label, .data$`Variable label`)) %>% 
        mutate("Quality assessment comment" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, assessment_comment, .data$`Quality assessment comment`)) %>% # [GF] to validate.
        mutate("Variable name" = ifelse(
          .data$`Variable name` == harmonized_col_dataset_short, '(all)', .data$`Variable name`))
      # filter(.data$`Variable name` != harmonized_col_dataset_short)
    }
    
    
    # [GF] to check.
    if(nrow(harmonized_dossier_summary[['Harmo dataset assessment']]) == 0){
      
      harmonized_dossier_summary[['Harmo dataset assessment']] <-
        harmonized_dossier_summary[['Harmo dataset assessment']] %>%
        mutate(
          'Index' = NA_character_,
          'Variable name' = "(all)",
          'Harmo dataset assessment' = "[INFO] - No errors/warnings detected.")}
    
    # arrange by index.
    harmonized_dossier_summary[['Harmo dataset assessment']] <- 
      harmonized_dossier_summary[['Harmo dataset assessment']] %>% 
      arrange("Index")
    
    # suppress empty sheets.
    harmonized_dossier_summary <- harmonized_dossier_summary[vapply(X = harmonized_dossier_summary,
                                                                    FUN = function(x) sum(nrow(x)) > 0,
                                                                    FUN.VALUE = logical(1))]
  return(harmonized_dossier_summary)
}
