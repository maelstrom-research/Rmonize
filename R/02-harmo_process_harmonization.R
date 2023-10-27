#' @title
#' Generate harmonized dataset(s) and annotated Data Processing Elements
#'
#' @description
#' Reads the DataSchema and Data Processing Elements objects to generate 
#' harmonized dataset(s) and annotated Data Processing Elements object with 
#' harmonization statuses and any processing errors. The function uses 
#' the DataSchema and Data Processing Elements specifications to process 
#' input variables into output harmonized variables for each dataset.
#' Documentation of each data processing action is generated in the console to 
#' support the identification of errors and correction of 
#' the Data Processing Elements file and objects, as needed. An 
#' annotated Data Processing Elements is also produced, providing harmonization 
#' statuses (complete/impossible) for each DataSchema variable in input 
#' dataset, which can be used to create a summary of the harmonization 
#' potential of the DataSchema variables across input dataset(s).
#'
#' @details
#' A dossier must be a named list containing at least one data frame or
#' data frame extension (e.g. a tibble), each of them being datasets.
#' The name of each tibble will be use as the reference name of the dataset.
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
#' @param dossier List of tibble(s), each of them being datasets to be 
#' harmonized.
#' @param dataschema A list of tibble(s) representing metadata of an 
#' associated harmonized dossier.
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#'
#' @returns
#' A list of tibbles of each harmonized dataset that has been harmonized from 
#' input datataset.
#'
#' @examples
#' {
#' 
#' # You can use our demonstration files to run examples
#' 
#' library(dplyr)
#' library(madshapR) # data_dict_filter
#' 
#' dataset_MELBOURNE_1 <- DEMO_files_harmo$dataset_MELBOURNE_1[1]
#' dossier <- dossier_create(list(dataset_MELBOURNE_1))
#' 
#' dataschema <- 
#'   DEMO_files_harmo$`dataschema - final` %>%
#'   data_dict_filter('name == "adm_unique_id"')
#' 
#' data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final` %>%
#'   dplyr::filter(dataschema_variable == 'adm_unique_id',
#'          input_dataset == 'dataset_MELBOURNE_1')
#' 
#' # perform harmonization
#' harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#' @importFrom rlang is_error
#' @importFrom rlang is_warning
#'
#' @export
harmo_process <- function(dossier, dataschema = NULL, data_proc_elem){
  
  # future dev
  # si le vT n'existe pas dans le Data Processing Elements, aller le chercher 
  # dans le DataSchema
  # controle de version ?
  
  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") 
    x = x[!is.na(x)]
    
    return(x)
  }
  
  data_proc_elem <- as_data_proc_elem(data_proc_elem)
  
  data_proc_elem$dataschema_variable <- 
    extract_var(data_proc_elem$dataschema_variable)
  
  # extraction of Data Processing Elements
  dpe <-
    data_proc_elem %>%
    rename(
      output_variable = "dataschema_variable",
      script          = "Mlstr_harmo::algorithm",
      rule_category   = "Mlstr_harmo::rule_category") %>%
    mutate(
      `script` = replace_na(.data$`script`, "undetermined"),
      `rule_category` = replace_na(.data$`rule_category`, "undetermined"),
      `rule_category` = ifelse(
        .data$`script` == "undetermined","undetermined",
        .data$`rule_category`),
      `script` = ifelse(
        .data$`rule_category` == "undetermined","undetermined",
        .data$`script`),
      `input_variables` = ifelse(
        .data$`rule_category` == "undetermined","__BLANK__",
        .data$`input_variables`))
  
  
  harmonized_col_id <- 
    unique(dpe[dpe$`rule_category` %in% 'id_creation',][[
          'output_variable']])
  
  dpe <- dpe %>%
    group_by(.data$`input_dataset`) %>%
    group_split() %>%
    as.list
  names(dpe) <- sort(unique(bind_rows(dpe)$input_dataset))
  
  if(is.null(dataschema)){
    dataschema <- dataschema_extract(data_proc_elem) 
  }else{
    vars <- extract_var(unique(data_proc_elem$dataschema_variable))
    dataschema <- 
      data_dict_filter(
        dataschema,filter_var = 
          paste0(c("name %in% c('",paste0(vars,collapse = "','"),"')"),
                 collapse = "")) %>%
      as_dataschema(as_dataschema_mlstr = TRUE)} 
  
  
  if(length(intersect(names(dossier), names(dpe))) == 0){
    stop(call. = FALSE, 'The dataset list to be harmonized is empty.
    
This usually means that your dataset names in the Data Processing Elements 
(in the column `dataset_input`) do not match the names in your dossier list. 
Please correct elements and reprocess.')
    
  }
  
  # selection of needed columns
  for(i in names(dossier)){
    # stop()}
  
    create_id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    var_id <- extract_var(create_id_row[['input_variables']])
    
    names_in_dpe <- 
      str_squish(unlist(strsplit(
        dpe[[i]] %>% 
          filter(
            .data$`input_variables` != '__BLANK__') %>%
          pull(.data$`input_variables`),split = ";"))) %>%
      unique %>% 
      extract_var
    
    dossier[[i]] <- as_dataset(dossier[[i]],col_id = var_id)
    
    dossier[[i]] <- 
      dossier[[i]] %>% select(col_id(dossier[[i]]),any_of(!! names_in_dpe))
  }
  
  # rid of data dictionary
  dossier <- 
    as_dossier(dossier) %>% 
    lapply(dataset_zap_data_dict)
  
  # intersection of dossier and dpe
  dossier <- dossier[intersect(names(dossier), names(dpe))]
  dpe <- dpe_init <- dpe[intersect(names(dossier), names(dpe))]
  
  # gather all information by dataset to be harmonized.
  harmonized_dossier <- harmonized_dossier_init <- 
    dpe %>% lapply(function(x){
      tbl <- tibble(!!! dataschema[['Variables']]$name, .rows = 0)
      names(tbl) <- dataschema[['Variables']]$name
      tbl <- tbl %>%
        select(all_of(harmonized_col_id), everything())
      return(tbl)})
  
  # creation of id
  for(i in names(harmonized_dossier)){
    # stop()}
    create_id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    var_id <- extract_var(create_id_row[['input_variables']])
    
    harmonization_id <-
      dossier[[i]][var_id] %>%
      mutate(across(all_of(var_id),as.character)) %>%
      rename_with(
        .cols = all_of(var_id), ~ create_id_row$output_variable)
    
    harmonized_dossier[[i]] <-
      harmonized_dossier[[i]] %>% bind_rows(harmonization_id)
    harmonized_dossier[[i]] <- harmonized_dossier_init[[i]] <-
      as_dataset(
        harmonized_dossier[[i]],
        col_id = create_id_row$output_variable)
  }
  
  message(crayon::bold(
"- Data Processing Elements: ------------------------------------------------"))
  
  # recast <- tibble()
  harmonization_report <- harmonization_report_init <- 
    tibble(
      output_variable = as.character(),
      input_dataset = as.character(),
      input_variables = as.character(),
      rule_category = as.character(),
      script = as.character(),
      `Rmonize::r_script` = as.character())
  
  harmonized_dossier <- harmonized_dossier_init
  dpe <- dpe_init
  harmonization_report <- harmonization_report_init
  
  for (i in names(harmonized_dossier)) {
    # stop()}
    
    message(str_sub(paste0("\n",
"--harmonization of : ",
crayon::bold(i)," -----------------------------------------------------"),1,81))
    
    create_id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    dataset_id <- extract_var(create_id_row[['input_variables']])
    input_dataset <- dossier[[i]]
    
    message(str_sub(paste0(
      str_trunc(paste0(
        "    processing ","1","/",nrow(dpe[[i]])," : ",
        harmonized_col_id),width = 49,ellipsis = '[...]'),
      "                                       "),1,50),
      crayon::bold("id created"))
    
    harmonization_report <-
      harmonization_report %>%
      bind_rows(
        create_id_row %>%
          mutate(
            `Rmonize::r_script` = paste0(
              "`",extract_var(.data$input_dataset),"`"," %>% \n",
              "  select('",!! harmonized_col_id,"' = '",
              dataset_id,"')")))
    
    # exclusion of id_creation rule for the rest of the process
    
    idx <- 1
    
    for(j in dpe[[i]][['output_variable']][!dpe[[i]]$`output_variable` %in% 
                                           harmonized_col_id]){
      # stop()}
      
      process_rule  <- dpe[[i]][dpe[[i]]$output_variable == j,]
      idx <- idx + 1
      
      r_script <-
        # Rmonize:::harmo_parse_process_rule(
        harmo_parse_process_rule(
          process_rule_slice = process_rule,
          input_dataset = input_dataset,
          r_script = TRUE)
      
      col <-
        # Rmonize:::harmo_parse_process_rule(
          harmo_parse_process_rule(
          process_rule_slice = process_rule, 
          input_dataset = input_dataset, 
          r_script = FALSE)
      
      if(!is_error(col)){
        vT_test <- fabR::silently_run(
          as_valueType(unique(col[[j]]),
                       dataschema[['Variables']] %>%
                         filter(.data$`name` == j) %>%
                         pull(.data$`valueType`)))
        
        if(is_error(attributes(vT_test)$condition))
          col <- attributes(vT_test)$condition}

      error_status <- NULL
      warning_status <- NULL
      
      if(is_error(col)){
        
        message(str_sub(paste0(
          str_trunc(paste0(
            "    processing ",idx,"/",nrow(dpe[[i]])," : ",
            process_rule$`output_variable`),width = 49,ellipsis = '[...]'),
          "                                       "),1,50),
          crayon::bold("**ERROR**"))
        
        error_status <- conditionMessage(col)
        
      }else{
        
        col <-
          col %>%
          rename_with(.cols = !! dataset_id,.fn = ~ harmonized_col_id) %>%
          mutate(across(!!harmonized_col_id, as.character))
        
        harmonized_dossier[[i]] <-
          harmonized_dossier[[i]] %>%
          select(- !! j) %>%
          full_join(col,by = harmonized_col_id)
        
        
        if(is_warning(attributes(col)$`Rmonize::warning`)){
          
          message(str_sub(paste0(
            str_trunc(paste0(
              "    processing ",idx,"/",nrow(dpe[[i]])," : ",
              process_rule$`output_variable`),width = 49,ellipsis = '[...]'),
            "                                       "),1,50),
            crayon::bold("**Warning(s)**"))
          
          warning_status <- conditionMessage(attributes(col)$`Rmonize::warning`)
          
        }else{
          
          status <- process_rule$`Mlstr_harmo::status`
          
          message(str_sub(paste0(
            str_trunc(paste0(
              "    processing ",idx,"/",nrow(dpe[[i]])," : ",
              process_rule$`output_variable`),width = 49,ellipsis = '[...]'),
            "                                       "),1,50),
            ifelse(status %in% 
                     c("undetermined",NA),crayon::bold(status),status)) 
          
        }
      }
      
      harmonization_report <-
        harmonization_report %>%
        bind_rows(
          process_rule %>%
            mutate(
              `Rmonize::error_status`   = error_status,
              `Rmonize::warning_status` = warning_status,
              `Rmonize::r_script` = 
                ifelse(.data$`rule_category` %in% c("undetermined",NA),
                       .data$`rule_category`, r_script),
              
              `Rmonize::r_script` = 
                str_replace_all(
                  .data$`Rmonize::r_script`, "input_dataset",.data$`input_dataset`),
              
              `Rmonize::r_script` = 
                str_replace(
                  .data$`Rmonize::r_script`, 
                  paste0("select\\('",var_id,"','",j,"'\\)$"),
                  paste0("select('",harmonized_col_id,"' = '",
                         var_id,"','",j,"')"))))
    }
  }
  
  message(crayon::bold("\n
- CREATION OF HARMONIZED DATA DICTIONARY : --------------------------------\n"))
  
  data_proc_elem <- 
    harmonization_report %>%
    rename(
      "dataschema_variable"        = "output_variable" ,
      "Mlstr_harmo::algorithm"     = "script"          ,
      "Mlstr_harmo::rule_category" = "rule_category"   )
  
  for(i in names(harmonized_dossier)){
    # stop()}
    
    message(crayon::bold(i)," : done")
    input_data_proc_elem <-
      data_proc_elem %>%
      rename("name" = "dataschema_variable") %>%
      dplyr::filter(.data$`input_dataset` == !! i) %>%
      select('name', 
             'Mlstr_harmo::rule_category',
             'Mlstr_harmo::algorithm',
             'Rmonize::r_script',
             'Mlstr_harmo::status',
             'Mlstr_harmo::status_detail',
             'Mlstr_harmo::comment')
    
    harmo_data_dict <- dataschema
    harmo_data_dict[['Variables']][['Mlstr_harmo::rule_category']] <- NULL
    harmo_data_dict[['Variables']][['Mlstr_harmo::algorithm']] <- NULL
    harmo_data_dict[['Variables']][['Rmonize::r_script']] <- NULL
    harmo_data_dict[['Variables']][['Mlstr_harmo::comment']] <- NULL
    harmo_data_dict[['Variables']][['Mlstr_harmo::status']] <- NULL
    harmo_data_dict[['Variables']][['Mlstr_harmo::status_detail']] <- NULL
    
    harmo_data_dict[['Variables']] <-
      harmo_data_dict[['Variables']] %>%
      full_join(input_data_proc_elem, by = 'name')
    
    harmonized_dossier[[i]] <- 
      valueType_adjust(
        from = harmo_data_dict,
        to = as_dataset(harmonized_dossier[[i]])) %>%
      dataset_zap_data_dict() %>%
      data_dict_apply(harmo_data_dict)
  }
  
  nb_error <- if(!is.null(harmonization_report[['Rmonize::error_status']])){
    sum(!is.na(harmonization_report[['Rmonize::error_status']]))
  }else{0}
  
  if(nb_error > 0){
    message(
      "\n
------------------------------------------------------------------------------\n
Your harmonization process contains errors. The tables with error(s) have not 
been generated to avoid any version confusion. When harmonization elements and 
rules are corrected, rerun the process with the updated Data Processing Elements 
to generate complete harmonized tables.\n",
      
      crayon::bold("\n\nUseful tip: "),
"If you identified errors and want to correct them later, you can specify
'undetermined' in the column 'Mlstr_harmo::algorithm' of your problematic 
Data Processing Elements(s). Such Data Processing Elements will be ignored.\n")
    
    for(i in names(harmonized_dossier)){
      # stop()}
      
      nb_error_i <- nrow(dplyr::filter(
        harmonization_report[harmonization_report$input_dataset %in% i,],
        !is.na(.data$`Rmonize::error_status`)))
      
      if(nb_error_i > 0) harmonized_dossier[[i]] <- 
          harmonized_dossier[[i]] %>% slice(0)
    }
    
  }
  
  nb_undet <-
    nrow(dplyr::filter(harmonization_report,
                       is.na(.data$`Mlstr_harmo::status`) |
                       str_detect(.data$`Mlstr_harmo::status`, "undetermined")))
  
  if(nb_undet > 0){
    message(
      "\n
------------------------------------------------------------------------------\n
The harmonization process contains 'undetermined' statuses or harmonization 
rules. These variables will appear as empty columns in any harmonized tables 
generated. When harmonization statuses and rules are determined, rerun the 
process with the updated Data Processing Elements to generate complete 
harmonized tables.")}
  
  
  if(nb_error > 0){
    message(
      "\n
------------------------------------------------------------------------------\n
Your harmonization is done. Please check if everything worked correctly.\n")    
    
  }
  
  harmonized_dossier <- 
    as_harmonized_dossier(
      harmonized_dossier,dataschema,data_proc_elem,harmonized_col_id)
  
  message(crayon::bold(
    "
- WARNING MESSAGES (if any): ----------------------------------------------\n"))
  
  return(harmonized_dossier)
  
}

#' @title
#' Internal function that generates code for harmonized dataset(s)
#'
#' @description
#' Generates code for harmonized dataset(s).
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#' @param input_dataset A tibble identifying the input table to be harmonized.
#' @param r_script script extracted form Data Processing Elements.
#'
#' @returns
#' A tibble identifying a harmonized dataset
#'
#' @import dplyr
#' @importFrom rlang .data
#' @noRd
harmo_parse_process_rule <- function(
    process_rule_slice, 
    input_dataset,
    r_script){

  if(is.na(process_rule_slice$`rule_category`))
    process_rule_slice$`rule_category` <- 'undetermined'
  
  process_rule_slice <- 
    process_rule_slice %>%
    mutate(
      to_eval_test =
        # paste0("try(harmo_process_",
        paste0("try(Rmonize:::harmo_process_",
               process_rule_slice$`rule_category`,
               "(process_rule_slice %>% slice(",row_number(),
               ")), silent = TRUE)" ))

  process_script <-
    paste0("`input_dataset` %>% \n",
           eval(parse(text = process_rule_slice$to_eval_test)), " %>%\n",
           "  select(",toString(paste0("'",col_id(input_dataset),"'")),",'",
                       process_rule_slice$output_variable,"')")

  if(r_script) return(process_script)

  process_script <- 

    tryCatch(
      
      {
        
        eval(parse(text = process_script))
        
      },
      error = function(e) {
        # Choose a return value in case of error
        return(e)
      },
      warning = function(w) {
        
        process_script <- eval(parse(text = process_script))
        attributes(process_script)$`Rmonize::warning` <- w
        
        # Choose a return value in case of warning
        return(process_script)
      }
    )
  
  return(process_script)
}

#' @title
#' Internal function for 'add_variable' in the process of harmonization
#'
#' @description
#' Processes 'add_variable' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#' @noRd
harmo_process_add_variable <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = "left",
      to_eval_test     = paste0(
        .data$`output_table`," %>% left_join(\n",
        "  ",.data$`input_dataset`," %>% \n",
        "  mutate(\n",
        "  '",.data$`output_variable`,"' = ",.data$`replacement`,") %>% \n")
      ) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'case_when' in the process of harmonization
#'
#' @description
#' Processes 'case_when' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_case_when <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    # DEMO_files_harmo$`data_processing_elements - final` %>%
    # dplyr::filter(`Mlstr_harmo::rule_category` == 'case_when') %>%
    # slice(1) %>%
    # select(
    #   output_variable = dataschema_variable,
    #   script = `Mlstr_harmo::algorithm`) %>%
    # mutate(script ='case_when( "j\'ai aucune idée, mais _$ca va" ~ NA ; ELSE ~ $var)') %>%
    mutate(
      replacement      =
        .data$`script` %>% str_squish(),
      replacement      =
        str_replace_all(.data$`replacement`,"case_when\\(","case_when(\n     "),
      replacement      =
        str_replace_all(.data$`replacement`,";",",\n     "),
      replacement      =
        str_replace_all(.data$`replacement`,"ELSE","TRUE"),
      # replacement      =
      #   str_replace_all(.data$`replacement`,"\\$",
      #                   paste0(.data$`input_dataset`,"$")),
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'direct_mapping' in the process of harmonization
#'
#' @description
#' Processes 'direct_mapping' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_direct_mapping <- function(process_rule_slice){
  
  process_script_to_eval <-
    process_rule_slice %>%
    # process_rule %>%
    mutate(
      replacement      = .data$`input_variables`,
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'id_creation' in the process of harmonization
#'
#' @description
#' Creates id in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_id_creation <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = .data$`input_variables`,
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'impossible' in the process of harmonization
#'
#' @description
#' Processes 'impossible' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_impossible <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = NA_character_,
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'merge' in the process of harmonization
#'
#' @description
#' Processes 'merge' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_merge_variable <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      var_to_add   =
        harmo_process_add_variable(
          process_rule_slice %>% mutate(script = "left")),
      var_to_merge = harmo_process_case_when(process_rule_slice),
      var_to_merge =
        .data$`var_to_merge` %>% gsub('^.+?%>%(.*)', "\\1"),
      replacement  =
        paste0(.data$`var_to_add`," %>%",.data$`var_to_merge`),
      replacement  =
        str_replace_all(.data$`replacement`,"\\.old",".x"),
      replacement  =
        str_replace_all(.data$`replacement`,"\\.new",".y"),
      to_eval_test =
        paste0(
          .data$`output_table`," %>% left_join(\n",
          "  ",.data$`input_dataset`," %>% \n",
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,") %>% \n"
          )) %>%
    pull(.data$`to_eval_test`)
  
  return(process_script_to_eval$to_eval_test)
}

#' @title
#' Internal function for 'operation' in the process of harmonization
#'
#' @description
#' Processes 'operation' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_operation <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      =
        .data$`script` %>% str_squish(),
      replacement      =
        .data$`replacement` %>%
        str_replace_all("\\$",paste0(.data$`input_dataset`,"$")),
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'other' in the process of harmonization
#'
#' @description
#' Processes 'other' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_other <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = .data$`script` %>% str_squish(),
      to_eval_test =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'paste' in the process of harmonization
#'
#' @description
#' Processes 'paste' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_paste <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = .data$`script`,
      replacement      = str_squish(.data$`replacement`),
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,"
          )")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'recode' in the process of harmonization
#'
#' @description
#' Processes 'recode' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr 
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_recode <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    # process_rule %>%
    mutate(
      input_variables  = str_remove_all(.data$`input_variables`,'`'),
      replacement      =
        .data$`script`,
      replacement      =
        str_replace_all(.data$`replacement`,"\'","{madshapR::apostrophy}"),
      replacement      =
        str_replace_all(.data$`replacement`,",","{madshapR::coma}"),
      replacement      =
        gsub("^recode\\("," , '",.data$`replacement`),
      replacement      =
        gsub("ELSE","else ",.data$`replacement`),
      replacement      =
        gsub(")$","')",.data$`replacement`),
      replacement      =
        paste0("car::recode(\n      var = .$`",
               .data$`input_variables`,"`",.data$`replacement`),
      replacement      =
        str_replace_all(.data$`replacement`,"fun::",""),
      replacement      =
        str_replace_all(.data$`replacement`,",",",\n      recodes = "),
      
    replacement      =
      str_replace_all(.data$`replacement`,
                "car::recode\\(","car::recode(\n      as.numeric = FALSE ,"),
    replacement      =
      str_replace_all(.data$`replacement`,"\\{madshapR::apostrophy\\}","\\\\'"),
    replacement      =
      str_replace_all(.data$`replacement`,"\\{madshapR::coma\\}",","),
      
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`input_variables`,
          "' = stringr::str_squish(`",.data$`input_variables`,"`), \n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'rename' in the process of harmonization
#'
#' @description
#' Processes 'rename' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_rename <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = .data$`script`,
      to_eval_test     =
        paste0(
          .data$`output_table`," %>% \n",
          "  rename('",.data$`output_variable`,"' = '",
          .data$`input_variables`,"')")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'undetermined' in the process of harmonization
#'
#' @description
#' Processes 'undetermined' in the process of harmonization.
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
#' @param process_rule_slice A one-rowed tibble, identifying the occurring
#' Data Processing Elements. 
#'
#' @returns
#' A character string of R code for harmonizing a specific column in
#' an input dataset. That line code is the transcription of the
#' harmonisation rule in the Data Processing Elements.
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @noRd
harmo_process_undetermined <- function(process_rule_slice){

  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = NA_character_,
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Generate a summary of the annotated Data Processing Elements in the console
#'
#' @description
#' Reads annotated Data Processing Elements to list processes, any errors, and 
#' an overview of each harmonization rule. This summary report can be used to 
#' asses harmonization and provide insight on the data structure, fields to 
#' investigate, and coherence across elements.
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
#' @param harmonized_dossier List of tibble(s), each of them being 
#' harmonized dataset.
#'
#' @returns
#' No object returned, the function sends messages in the console, showing
#' errors in the harmonization process.
#'
#' @examples
#' {
#'
#' harmonized_dossier <- DEMO_files_harmo$harmonized_dossier
#' show_harmo_error(harmonized_dossier)
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#' @importFrom utils capture.output
#'
#' @export
show_harmo_error <- function(harmonized_dossier){

  # list of primary error in the Data Processing Elements.
  # print the list of error + the index

  # check input
  as_harmonized_dossier(harmonized_dossier)
  
  data_proc_elem <- 
    as_data_proc_elem(
      attributes(harmonized_dossier)[['Rmonize::Data Processing Elements']])

  for(i in names(harmonized_dossier)){
    # stop()}

    if(nrow(harmonized_dossier[[i]]) > 0){
      tbl <- 
        harmonized_dossier[[i]] %>% 
        summarise(across(everything(), ~all(is.na(.)))) %>%
        pivot_longer(everything()) %>%
        filter(.data$`value` == TRUE) %>%
        select('dataschema_variable' = 'name','empty' = 'value')
      dpe <- 
        data_proc_elem %>% 
        filter(.data$`input_dataset` == i & 
             !(.data$`Mlstr_harmo::rule_category` %in% 
                   c('impossible','undetermined'))) %>%
        select('dataschema_variable') %>% distinct
      
      empty_col <- inner_join(tbl,dpe, by = 'dataschema_variable') 
      status <- 'Warning: The variable generated is NA'
      
      data_proc_elem <- 
        data_proc_elem %>%
        mutate(
          `Mlstr_harmo::status` =
            ifelse(
              .data$`dataschema_variable` %in% empty_col$dataschema_variable &
              .data$`input_dataset` == i,
              status, .data$`Mlstr_harmo::status`))
    }
    
  } 
  
  report_log <-
    data_proc_elem %>%
    select(
      "dataschema_variable", "input_dataset",
      "Mlstr_harmo::algorithm",
      "Mlstr_harmo::rule_category", "Rmonize::r_script",
      matches("Rmonize::error_status"),
      matches("Rmonize::warning_status")) %>%
    bind_rows(tibble(
      "Rmonize::error_status" = as.character(),
      "Rmonize::warning_status" = as.character())) %>%
    dplyr::filter(!is.na(.data$`Rmonize::error_status`) |
                    !is.na(.data$`Rmonize::warning_status`)) %>%
    unite(col = "Rmonize::status",
          "Rmonize::error_status","Rmonize::warning_status",
          na.rm = TRUE) %>% 
    mutate(
      var = paste0(.data$`dataschema_variable`, " in ",
                   .data$`input_dataset`, " \n"),
      rule = paste0(.data$`Mlstr_harmo::algorithm`))

  if(nrow(report_log) > 0){
    message(crayon::bold(
"\n
- ERROR/WARNING STATUS DETAILS: -------------------------------------------\n"),
"\nHere is the list of the errors  and warnings encountered in the process 
of harmonization:\n")
    for(i in seq_len(nrow(report_log))){
      message(
"---------------------------------------------------------------------------\n")
      message(crayon::bold(report_log$var[i]))
      message(crayon::bold("\nRule Category:"))
      message(report_log$`Mlstr_harmo::rule_category`[i])
      message(crayon::bold("\nAlgorithm:"))
      message(report_log$rule[i])
      message(crayon::bold("\nR Script:"))
      message(report_log$`Rmonize::r_script`[i])
      message(crayon::green(crayon::bold("\nR Error/Warning:")))
      message(crayon::green(report_log$`Rmonize::status`[i]))
    }
  }
  message(crayon::bold(
"\n\n- STATUS SUMMARY: ----------------------------------------------------\n"))
  message(paste(capture.output({print(

    report_log %>% count(.data$`Mlstr_harmo::rule_category`) %>%
      full_join(data_proc_elem %>%
                  count(.data$`Mlstr_harmo::rule_category`),
                by = "Mlstr_harmo::rule_category") %>%
      mutate(
        n.x = replace_na(.data$`n.x`,0),
        success = trunc((1 - .data$`n.x`/.data$`n.y`)*100)
        
        ) %>%
      arrange(.data$`success`) %>%
      mutate(success = paste0(.data$`success`," %")) %>%
      rename(total_nb_errors_warnings = "n.x", total_nb_rules = "n.y") %>%
      mutate_all(as.character)

  )}), collapse = "\n"))

  return(message(""))
}

#' @title
#' Extract and create the DataSchema from a Data Processing Elements
#'
#' @description
#' Creates the DataSchema in the Maelstrom Research formats (with 'Variables' 
#' and 'Categories' in separate tibbles and standard columns in each) from any
#' Data Processing Elements.
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
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#'
#' @returns
#' A list of tibble(s), 'Variables' and 'Categories' (if any), each of them 
#' being the two elements of the DataSchema.
#'
#' @examples
#' {
#'
#' # You can use our demonstration files to run examples
#'
#' dataschema_extract(
#'   data_proc_elem = DEMO_files_harmo$`data_processing_elements - final`)
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
dataschema_extract <- function(data_proc_elem){

  data_proc_elem <- as_data_proc_elem(data_proc_elem)

  ## creation of the DataSchema in a list provided from the 
  ## dataprocessing element variables

  dataschema <- list(
    Variables =
      data_proc_elem[
        c('dataschema_variable','valueType')] %>%
      rename(name = 'dataschema_variable') %>%
      distinct) %>%
    as_dataschema(as_dataschema_mlstr = TRUE)

#   warning(call. = FALSE,
# "Your project does not include the DataSchema. An automatic DataSchema 
#  and harmonized data dictionary have been created based on your 
#  Data Processing Elements." )

  return(dataschema)
}

#' @title
#' Validate and coerce any object as a Data Processing Elements
#'
#' @description
#' Validates the input object as a valid Data Processing Elements and coerces 
#' it with the appropriate Rmonize::class attribute. This function mainly 
#' helps validate input within other functions of the package but could be used 
#' to check if an object is valid for use in a function.
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
#' @param object A potential Data Processing Elements to be coerced.
#'
#' @returns
#' A tibble, identifying a Data Processing Elements.
#'
#' @examples
#' {
#'
#' # You can use our demonstration files to run examples
#'
#' as_data_proc_elem(DEMO_files_harmo$`data_processing_elements - final`)
#' }
#'
#' @import dplyr fabR tidyr
#' @importFrom rlang .data
#'
#' @export
as_data_proc_elem <- function(object){

  object_init <- object
  
  if(is.data.frame(object)){
    
    # if('Mlstr_harmo::algorithm' %in% names(object)){
      object <- 
        object %>%
        rename_with(
          .cols = everything(),
          .fn = ~ case_when(
            . == 'ss_table'            ~ 'input_dataset',
            . == 'ss_variables'        ~ 'input_variables',
            . == 'harmo_rule'          ~ 'Mlstr_harmo::algorithm',
            . == 'harmo_status'        ~ 'Mlstr_harmo::status',
            . == 'harmo_status_detail' ~ 'Mlstr_harmo::status_detail',
            . %in% c('status'       ,
                     'status_detail',
                     'comment'      ,
                     'algorithm'    ,
                     'rule_category')  ~ paste0('Mlstr_harmo::',.),
            TRUE                       ~ .))
      
    # }
  }
  
  if(sum(names(object) %in% 
         c('dataschema_variable',
           'input_dataset',
           'input_variables',
           'Mlstr_harmo::rule_category',
           'Mlstr_harmo::algorithm'
           )) == 5 & is.data.frame(object)){
    
    # up to date object_init 
    if(sum(names(object_init) %in% 
           c('ss_table','ss_variables','harmo_rule')) > 0 ){
    
      warning(call. = FALSE,
"\n\nSome column names in your Data Processing Elements have been deprecated 
since the last version of the package. To avoid this warning, we recommend to 
change column names into the newest ones as defined by Maelstrom standards :

     Old column names                New column names
  ======================   >   =============================
  
    ss_table               >     input_dataset
    ss_variables           >     input_variables
    rule_category          >     Mlstr_harmo::rule_category
    harmo_rule             >     Mlstr_harmo::algorithm
    harmo_status           >     Mlstr_harmo::status
    harmo_status_detail    >     Mlstr_harmo::status_detail
    comment                >     Mlstr_harmo::comment
   
   
Please refer to documentation.")
    
    }
    

    # no NA in dataschema_variable
    if(sum(is.na(unique(object$dataschema_variable))) > 0)
      stop(call. = FALSE,
'The column `dataschema_variable` of your Data Processing Elements must not 
contain NA values.')
    
    # no NA in input_dataset
    if(sum(is.na(unique(object$input_dataset))) > 0)
      stop(call. = FALSE,
'The column `input_dataset` of your Data Processing Elements must not 
contain NA values.')
    
    # id_creation rule must exist
    object %>%
      group_by(.data$`input_dataset`) %>% group_split() %>%
      lapply(function(x){
      col_id <- 
        x[x$`Mlstr_harmo::rule_category` %in% 'id_creation',][['dataschema_variable']]
      
      if(length(col_id) == 0)
        stop(
          call. = FALSE,
'In ',unique(x$input_dataset),': \n\n',
'In the column `Mlstr_harmo::rule_category` of your Data Processing Elements, 
"id_creation" rule is missing. The harmonization process cannot start because 
this step initiates the process for each of your processed datasets. 
Your first processing element must be "id_creation" in `Mlstr_harmo::rule_category` 
followed by the name of the column taken as identifier of dataset in your 
dossier.')
      
      if(length(col_id)  > 1)
        stop(
          call. = FALSE,
'In ',unique(x$input_dataset),': \n\n',
'In the column `Mlstr_harmo::rule_category` of your Data Processing Elements, 
"id_creation" rule is not unique. The harmonization process cannot start because 
this step initiates the process and must be unique for each of your processed 
dataset(s). Your first processing element must be "id_creation" in 
`Mlstr_harmo::rule_category` followed by the name of the column taken as identifier of 
dataset in your dossier.')
    })
    
  nb_col_id <- 
    unique(
      object[
        object$`Mlstr_harmo::rule_category` %in% 'id_creation',][[
          'dataschema_variable']])
  
  if(length(nb_col_id) > 1)
    stop(
      call. = FALSE,
'In ',unique(object$input_dataset),': \n\n',
'In the column `Mlstr_harmo::rule_category` of your Data Processing Elements, 
"id_creation" is not unique across the harmonized dataset(s) to generate. 
The harmonization process cannot start because this step initiates the process 
for each of your processed datasets and must be the same variable across the 
generated harmonized dataset(s). Your first processing element must be 
"id_creation" in `Mlstr_harmo::rule_category` followed by the name of the column taken 
identifier of dataset in your dossier.')
  
  # harmo status must be either NA, complete, impossible or undetermined
  

  # step cleaning : addition of index
  if(is.null(object[['index']])){
    object <-
      object %>%
      fabR::add_index()}
  
  # step cleaning : addition of missing columns
  object <-
    object %>%
    bind_rows(tibble(
      `valueType` = as.character(),
      `Mlstr_harmo::status` = as.character(),
      `Mlstr_harmo::status_detail` = as.character(),
      `Mlstr_harmo::comment` = as.character()))
  
  # step cleaning : addition of Mlstr_harmo::status.
  object <-
    object %>%  
    mutate(
      `Mlstr_harmo::status` = case_when(
        if_any(c("input_variables",
                 "Mlstr_harmo::rule_category",
                 "Mlstr_harmo::algorithm"), ~ . == "undetermined") ~ 
          'undetermined',
        if_any(c("Mlstr_harmo::rule_category",
                 "Mlstr_harmo::algorithm"), ~ . == "impossible")   ~ 
          'impossible',
        is.na(`Mlstr_harmo::status`)                               ~ 
          'complete',
        TRUE                                                       ~ 
          .data$`Mlstr_harmo::status`))
    
  # step cleaning : addition of Mlstr_harmo::status_detail.
  object <-
    object %>%  
    mutate(
      `Mlstr_harmo::status_detail` = case_when(
        if_any(c("input_variables",
                 "Mlstr_harmo::rule_category",
                 "Mlstr_harmo::algorithm"), ~ . == "undetermined") ~ 
          'undetermined',
        is.na(`Mlstr_harmo::status_detail`)                        ~ 
          'unknown',
        TRUE                                                       ~ 
          .data$`Mlstr_harmo::status_detail`))
  
    # if all test pass:
    object <- 
      object %>%
      select('index',
             'dataschema_variable',
             'valueType',
             'input_dataset',
             'input_variables',
             'Mlstr_harmo::rule_category',
             'Mlstr_harmo::algorithm',
             'Mlstr_harmo::status',
             'Mlstr_harmo::status_detail',
             'Mlstr_harmo::comment',
             everything())
    
    attributes(object)$`Rmonize::class` <- "data_proc_elem"
    return(object)

  }

  # dataschema_variable column must exist
  if(is.null(object[['dataschema_variable']])){
    stop(call. = FALSE,
"Column 'dataschema_variable'in your Data Processing Elements is missing.")}
  
  # input_dataset column must exist
  if(is.null(object[['input_dataset']])){
    stop(call. = FALSE,
"Column 'input_dataset' in your Data Processing Elements is missing.")}
  
  # input_variables column must exist
  if(is.null(object[['input_variables']])){
    stop(call. = FALSE,
"Column 'input_variables' in your Data Processing Elements is missing.")}
  
  # Mlstr_harmo::rule_category column must exist
  if(is.null(object[['Mlstr_harmo::rule_category']])){
    stop(call. = FALSE,
"Column 'Mlstr_harmo::rule_category' in your Data Processing Elements is missing.")}
  
  # Mlstr_harmo::algorithm column must exist
  if(is.null(object[['Mlstr_harmo::algorithm']])){
    stop(call. = FALSE,
"Column 'Mlstr_harmo::algorithm' in your Data Processing Elements is missing.")}
  
  # else
  stop(call. = FALSE,
"\n\nThis object is not a Data Processing Elements as defined by 
Maelstrom standards, which must be a tibble containing at least these columns:
  `dataschema_variable`
  `input_dataset`
  `input_variables`
  `Mlstr_harmo::rule_category`
  `Mlstr_harmo::algorithm`
  
Please refer to documentation.")

  return(object)
}

#' @title
#' Validate and coerce any object as the DataSchema
#'
#' @description
#' Validates the input object as a valid DataSchema and coerces it with the 
#' appropriate Rmonize::class attribute. This function mainly helps validate 
#' input within other functions of the package but could be used to check if an 
#' object is valid for use in a function.
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
#' @param object A potential DataSchema (list of tibble) to be coerced.
#' @param as_dataschema_mlstr Whether the output DataSchema should have a 
#' minimal DataSchema structure or additional attributes associated 
#' with additional capabilities for Maelstrom and integrated workflows, 
#' such as Opal environments. FALSE by default.
#'
#' @returns
#' A list of tibble(s), 'Variables' and 'Categories' (if any), each of them 
#' being the two elements of the DataSchema.
#'
#' @examples
#' {
#'
#' # You can use our demonstration files to run examples
#'
#' as_dataschema(DEMO_files_harmo$`dataschema - final`)
#' 
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
as_dataschema <- function(object, as_dataschema_mlstr = FALSE){

  if(!is.logical(as_dataschema_mlstr))
    stop('`as_dataschema_mlstr` must be TRUE or FALSE (FALSE by default)')

  if(is_data_dict(object)){
    
    if(as_dataschema_mlstr == TRUE){
      if(is_data_dict_mlstr(object)){
        object <- as_data_dict_mlstr(object)
        attributes(object)$`Rmonize::class` <- "dataSchema_mlstr"
        return(object)}
    }else{
      object <- as_data_dict(object)
      attributes(object)$`Rmonize::class` <- "dataschema"      
      return(object)
    }
  }
  
  stop(call. = FALSE,
         "

The DataSchema contains errors or does not conform to the required structure, 
Please refer to documentation.\n\n",
crayon::bold("Useful tip:\n"),
"Use dataschema_evaluate(dataschema) to get an assessment of your DataSchema")
  
}

#' @title
#' Validate and coerce any object as a DataSchema with Maelstrom restrictions
#'
#' @description
#' Validates the input object as a valid DataSchema compliant with formats used 
#' in Maelstrom Research ecosystem, including Opal, and returns it with the 
#' appropriate Rmonize::class attribute. This function mainly helps validate 
#' input within other functions of the package but could be used to check if an 
#' object is valid for use in a function.
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
#' @param object A potential Maelstrom formatted DataSchema to be coerced.
#'
#' @returns
#' A list of tibble(s), each of them identifying the DataSchema.
#'
#' @examples
#' {
#'
#' # You can use our demonstration files to run examples
#'
#' as_dataschema_mlstr(DEMO_files_harmo$`dataschema - final`)
#' 
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
as_dataschema_mlstr <- function(object){

  object <- as_dataschema(object,as_dataschema_mlstr = TRUE)
  # object <- as_data_dict_mlstr(object)
  # 
  # # if all test pass:
  # attributes(object)$`Rmonize::class` <- "dataSchema_mlstr"

  return(object)
}


#' @title
#' Validate and coerce any object as an harmonized dossier
#'
#' @description
#' Validates the input object as a valid harmonized dossier and coerces it with 
#' the appropriate Rmonize::class attribute. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if an object is valid for use in a function.
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
#' @param object A potential harmonized dossier to be coerced.
#' @param dataschema A list of tibble(s) representing metadata of an 
#' associated harmonized dossier.
#' @param data_proc_elem A tibble, identifying the input 
#' Data Processing Elements.
#' @param harmonized_col_id A character string identifying the name of the 
#' column present in every dataset as identifier of the dataset.
#'
#' @returns
#' A list of tibble(s), each of them identifying the harmonized dataset.
#'
#' @examples
#' {
#' 
#' as_harmonized_dossier(object = DEMO_files_harmo$harmonized_dossier)
#'   
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
as_harmonized_dossier <- function(
    object, 
    dataschema = NULL,
    data_proc_elem = NULL,
    harmonized_col_id = NULL){

  # future devs
  # generate all missing from given parameters
  
  as_dossier(object)

  # check the DataSchema
  if(is.null(dataschema)) dataschema <- 
      attributes(object)$`Rmonize::DataSchema`
  dataschema <- as_dataschema(dataschema,as_dataschema_mlstr = TRUE)
  
  # check the presence of same column names in the dataset(s)
  same_names <- 
    all(
      sapply(
        sapply(
          object %>% lapply(function(x) attributes(x)$`names`),
          `%in%`,
          dataschema$`Variables`$`name`),
        identical,
        TRUE))
  
  if(!same_names) stop(call. = FALSE,
"All of your column(s) in the harmonized dataset(s) must be in the DataSchema 
name list of variables.")
  
  # check the Data Processing Elements
  if(is.null(data_proc_elem)) 
    data_proc_elem <- attributes(object)$`Rmonize::Data Processing Elements`
  data_proc_elem <- as_data_proc_elem(data_proc_elem)
  
  # check the presence of same column id in the dataset(s). If yes, assignation
  if(is.null(harmonized_col_id)) 
    harmonized_col_id <- attributes(object)$`Rmonize::harmonized_col_id`
  object <- 
    object %>% lapply(function(x) as_dataset(x, col_id = harmonized_col_id))

  # if all checks TRUE
  attributes(object)$`Rmonize::class` <- "harmonized_dossier"
  attributes(object)$`Rmonize::DataSchema` <- dataschema
  attributes(object)$`Rmonize::Data Processing Elements` <- data_proc_elem
  attributes(object)$`Rmonize::harmonized_col_id` <- harmonized_col_id

  return(object)
}


#' @title
#' Generate the pooled dataset from harmonized dataset(s) in a dossier
#'
#' @description
#' Generates the pooled harmonized dataset from harmonized dataset(s) in a 
#' dossier. The pooled dataset has two columns which can be declared by 
#' the user (unique_col_dataset and unique_col_id).
#' The first column refers to the name of each dataset which is the name of each
#' tibble in the dossier. The second column refers to the column id in each 
#' harmonized dataset and  which identifies unique combination (concatenated)
#' observation/dataset. These two columns are added to ensure every information 
#' is safe during the process. The pooled harmonized dataset comes with its 
#' data dictionary which is the harmonized dossier DataSchema, to which the 
#' two extra columns are added.
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
#' @param harmonized_dossier List of tibble(s), each of them being 
#' harmonized dataset.
#' @param unique_col_dataset A character string identifying the name the column 
#' referring each dataset names. NULL by default.
#' @param unique_col_id  A character string identifying the name of the 
#' column identifier of the dataset and will be the concatenation of 
#' id column value and dataset name. NULL by default.
#'
#' @returns
#' A tibble, which is the pooled harmonized dataset from a harmonized dossier.
#' 
#' @examples
#' {
#'
#' harmonized_dossier <- DEMO_files_harmo$harmonized_dossier
#' 
#' pooled_harmonized_dataset_create(
#'  harmonized_dossier,unique_col_dataset = 'adm_unique_id')
#'   
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @export
pooled_harmonized_dataset_create <- function(
    harmonized_dossier,
    unique_col_dataset = NULL,
    unique_col_id = NULL){
  
  # check args
  as_dossier(harmonized_dossier)
  as_dataset(harmonized_dossier[[1]], col_id = unique_col_dataset)
  as_dataset(harmonized_dossier[[1]], col_id = unique_col_id)
  
  dataschema <- 
    attributes(harmonized_dossier)$`Rmonize::DataSchema` %>%
    as_dataschema(as_dataschema_mlstr = TRUE) %>%
    as_data_dict_mlstr()
  
  harmonized_col_id <- 
    attributes(harmonized_dossier)$`Rmonize::harmonized_col_id`
  
  if(is.null(harmonized_dossier[["Rmonize::pooled_harmonized_dataset"]])){
    
    pooled_harmonized_dataset <- bind_rows(harmonized_dossier) 
    
  }else{
    
    pooled_harmonized_dataset <-
    harmonized_dossier[["Rmonize::pooled_harmonized_dataset"]]
    
  }
  
  pooled_harmonized_dataset <-
    pooled_harmonized_dataset %>%
    data_dict_apply(dataschema)
  
  attributes(pooled_harmonized_dataset)$`Rmonize::class` <- 
    "pooled_harmonized_dataset"
  
  attributes(pooled_harmonized_dataset)$`Rmonize::unique_col_id` <- 
    unique_col_id
  
  attributes(`pooled_harmonized_dataset`)$`Rmonize::unique_col_dataset` <- 
    unique_col_dataset

  return(pooled_harmonized_dataset)
}


#' @title
#' Test if an object is a valid Maelstrom dataSchema
#'
#' @description
#' Tests if the input object is a valid dataSchema compliant with formats 
#' used in Maelstrom Research ecosystem, including Opal. This function mainly 
#' helps validate input within other functions of the package but could be used 
#' to check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [dataschema_evaluate()].
#'
#' @param object A potential Maelstrom formatted DataSchema to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files_harmo provided by the package
#'
#' dataschema <- DEMO_files_harmo$`dataschema - final`
#' is_dataschema_mlstr(dataschema)
#' is_dataschema_mlstr(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_dataschema_mlstr <- function(object){
  
  object <- object
  # if only the tibble is given in parameter
  test <- silently_run(try(as_dataschema_mlstr(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
}

#' @title
#' Test if an object is a valid DataSchema
#'
#' @description
#' Tests if the input object is a valid DataSchema. This function mainly 
#' helps validate input within other functions of the package but could be used 
#' to check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [dataschema_evaluate()].
#'
#' @param object A potential DataSchema to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files_harmo provided by the package
#'
#' dataschema <- DEMO_files_harmo$`dataschema - final`
#' is_dataschema(dataschema)
#' is_dataschema(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_dataschema <- function(object){
  
  object <- object
  # if only the tibble is given in parameter
  test <- silently_run(try(as_dataschema(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
  
}

#' @title
#' Test if an object is a valid Data Processing Elements
#'
#' @description
#' Tests if the input object is a valid Data Processing Elements. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [data_proc_elem_evaluate()].
#'
#' @param object A potential DataSchema to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files_harmo provided by the package
#'
#' data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final`
#' is_data_proc_elem(data_proc_elem)
#' is_data_proc_elem(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_data_proc_elem <- function(object){
  
  object <- object
  # if only the tibble is given in parameter
  test <- silently_run(try(as_data_proc_elem(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
  
}
