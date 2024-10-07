#' @title
#' Generate harmonized dataset(s) and associated metadata
#'
#' @description
#' Reads a DataSchema and Data Processing Elements to generate a 
#' harmonized dossier from input dataset(s) in a dossier and associated 
#' metadata. The function 
#' has one argument that can optionally be declared by the user 
#' (`unique_col_dataset`). It refers to the columns which contains name of 
#' each harmonized dataset. These two columns are added to ensure that there 
#' is always a unique entity identifier when datasets are pooled.
#'
#' @details
#' A dossier is a named list containing one or more data frames, which are 
#' input datasets. The name of each data frame in the dossier will be used as 
#' the name of the associated harmonized dataset produced by [harmo_process()].
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
#' @param object Data frame(s) or list of data frame(s) containing input 
#' dataset(s).
#' @param dataschema A DataSchema object.
#' @param data_proc_elem A Data Processing Elements object.
#' @param harmonized_col_dataset A character string identifying the column 
#' to use for dataset names. NULL by default.
#' @param harmonized_col_id A character string identifying the name of the 
#' column present in every dataset to use as a dataset identifier. 
#' NULL by default.
#' @param .debug Allow user to test the inputs before processing harmonization.
#' @param dossier `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A list of data frame(s), containing harmonized dataset(s). The DataSchema 
#' and Data Processing Elements are preserved as attributes of the 
#' output harmonized dossier.
#'
#' @examples
#' {
#' 
#' # Use Rmonize_examples to run examples.
#'  
#' library(dplyr)
#' library(stringr)
#' 
#' # use Rmonize_examples provided by the package
#' library(stringr)
#' 
#' # perform data processing
#' dossier            <- Rmonize_examples[str_detect(names(Rmonize_examples),"dataset")]
#' dataschema         <- Rmonize_examples$DataSchema
#' data_proc_elem     <- Rmonize_examples$`Data Processing Elements`
#' harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' glimpse(harmonized_dossier$dataset_study1)
#' 
#' }
#'
#' @import dplyr tidyr fabR stringr
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom rlang is_error
#' @importFrom rlang is_warning
#' @importFrom crayon bold
#'
#' @export
harmo_process <- function(
    object = NULL,
    dataschema = attributes(dossier)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(dossier)$`Rmonize::Data Processing Elements`,
    harmonized_col_dataset = attributes(dossier)$`Rmonize::harmonized_col_dataset`,
    harmonized_col_id = attributes(dossier)$`Rmonize::harmonized_col_id`,
    .debug = FALSE,
    dossier = object
    ){
  
  # future dev
  # si le vT n'existe pas dans le Data Processing Elements, aller le chercher 
  # dans le DataSchema
  # controle de version ?
  
  if(is.null(object)) object <- dossier
  if(is.null(dossier)) dossier <- object
  
  if(.debug == FALSE){
    
    # check arguments
    if(is.null(object) | is.null(dataschema) | is.null(data_proc_elem))
      stop(call. = FALSE,
"

`object`, `dataschema` and `data_proc_elem` are mandatory and must be provided.
If you want to allow the code to run, you can specify 
harmo_process(object, dataset, dataprocelem, .debug = TRUE) to test your
input elements before processing harmonization.")

  }
  
  add_index_dossier <- function(dossier){
    
    dossier <- as_dossier(dossier)
    dataset_names <- names(dossier)
    
    col_ids <- 
      dossier %>% lapply(function(x) col_id(x)) %>% unique
    

    
    if(length(col_ids) > 1 | is.null(col_ids[[1]])){
      for(i in names(dossier)){
        # stop()}
        
        if(!("Rmonize::index" %in% names(dossier[[i]]))){
          dossier[[i]] <- 
            dossier[[i]] %>%
            add_index(name_index = "Rmonize::index",.force = TRUE) %>%
            mutate(`Rmonize::index` = paste0(i,".",.data$`Rmonize::index`)) %>%
            as_dataset(col_id = "Rmonize::index")          
        }else{
          dossier[[i]] <- 
            dossier[[i]] %>%
            as_dataset(col_id = "Rmonize::index")          
        }
      }}
    return(dossier)
  }
    
  data_proc_elem_get <- function(dossier){
    
    bind_dossier_names <- 
      dossier %>% 
      lapply(function(x) mutate(x[1,], across(everything(),as.character))) %>%
      bind_rows() %>%
      names
    
    total_len <- 
      dossier %>% 
      lapply(function(x) nrow(x)) %>%
      unlist() %>% sum
      # bind_rows() %>%
    
    bind_dossier <- tibble(.rows = total_len)
    
    for(i in seq_along(bind_dossier_names)){
      # stop()}
      
      cols_to_bind <- 
        dossier %>%
        lapply(function(x) select(x,any_of(bind_dossier_names[[i]])))
        
      bound_cols <- silently_run(bind_rows(cols_to_bind))
      
      if(class(bound_cols)[[1]] == 'try-error'){
        bound_cols <- 
          cols_to_bind %>%
          lapply(function(x) mutate(x, across(everything(),as.character))) %>%
          bind_rows() %>%
          valueType_self_adjust()}
      
        bind_dossier <- bind_dossier %>% bind_cols(bound_cols)
    }
        
    dataschema <- data_dict_extract(bind_dossier)
      
    data_proc_elem <-
      tibble(
        index = as.integer(),
        dataschema_variable = as.character(),
        input_dataset = as.character(),
        valueType = as.character(),
        input_variables = as.character(),
        `Mlstr_harmo::rule_category` = as.character(),
        `Mlstr_harmo::algorithm` = as.character())
    
    for(i in names(dossier)){
      # stop()}
      data_proc_elem_i <- 
        dataschema$Variables %>%
        rename("dataschema_variable" = "name") %>%
        mutate(
          input_dataset = i,
          input_variables = case_when(
            .data$`dataschema_variable` %in% names(dossier[[i]]) ~ .data$`dataschema_variable`,
            TRUE ~ "__BLANK__",
          ),
          `Mlstr_harmo::rule_category` = case_when(
            col_id(dossier[[i]]) == .data$dataschema_variable ~ "id_creation",
            .data$`dataschema_variable` %in% names(dossier[[i]]) ~ "direct_mapping",
            TRUE  ~ "impossible"
          ),
          `Mlstr_harmo::algorithm` = .data$`Mlstr_harmo::rule_category`) %>%
        add_index(.force = TRUE)  %>%
        select(
          "index","dataschema_variable","valueType",
          "input_dataset","input_variables",
          "Mlstr_harmo::rule_category",
          "Mlstr_harmo::algorithm")
      
      data_proc_elem <- bind_rows(data_proc_elem,data_proc_elem_i)
      
    }
      
    data_proc_elem <- as_data_proc_elem(data_proc_elem)
    return(data_proc_elem)
  }

  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") %>%
      str_squish()
    x = x[!is.na(x)]
    
    return(x)}
  
  # check arguments
  
  if(is_data_proc_elem(object)) 
    stop(call. = FALSE,'
This object is a Data Processing Elements. 
Please write harmo_process(data_proc_elem = my_object) instead.')
  
  if(is_dataschema(object)) 
    stop(call. = FALSE,'
This object is a DataSchema. 
Please write harmo_process(dataschema = my_object) instead.')
  
  if(is.null(object) & is.null(data_proc_elem) & is.null(dataschema))
    stop(call. = FALSE,"At least one element is missing.")
  
  if(!is.null(data_proc_elem))
    if(is.null(data_proc_elem[['input_dataset']]) | 
       is.null(data_proc_elem[['Mlstr_harmo::rule_category']]))
      if(is.null(data_proc_elem[['rule_category']]))
        as_data_proc_elem(data_proc_elem)
  
  # create dossier from data proc elem.
  if(is.null(object) & !is.null(data_proc_elem)){
    
    data_proc_elem <- as_data_proc_elem(data_proc_elem)
    name_datasets <- unique(data_proc_elem$input_dataset)
    object <- lapply(as.list(name_datasets),function(x) tibble())
    names(object) <- name_datasets
    
    for(i in name_datasets){
      # stop()}
      
      input_vars <- 
        unique(extract_var(
          data_proc_elem %>% 
            dplyr::filter(.data$`input_dataset` == i & 
                            .data$`input_variables` != '__BLANK__') %>%
            select("input_variables") %>%
            separate_longer_delim(cols = "input_variables",";") %>%
            pull("input_variables")))
      
      object[[i]] <- 
        as_tibble(data.frame(matrix(nrow=0,ncol=length(input_vars)))) %>%
        mutate(across(everything(),as.character))
      
      names(object[[i]]) <- input_vars
    }
    
    # harmonized_col_id <- extract_var(
    #   data_proc_elem %>%
    #     dplyr::filter(.data$`Mlstr_harmo::rule_category` == "id_creation") %>%
    #     pull("dataschema_variable") %>% unique)
    
    return(
      harmo_process(
        object = object,
        dataschema = dataschema,
        data_proc_elem = data_proc_elem,
        harmonized_col_dataset = harmonized_col_dataset,
        harmonized_col_id = harmonized_col_id,
        dossier = dossier,
        .debug = .debug))

  }
  
  # if only a dataSchema is present
  if(is.null(object) & is.null(data_proc_elem) & !is.null(dataschema)){
    
    input_vars <- dataschema$Variables$name
    object <- as_tibble(data.frame(matrix(nrow=0,ncol=length(input_vars))))
    names(object) <- input_vars
    
    object <- valueType_adjust(from = dataschema,to = object)
    
    return(
      harmo_process(
        object = object,
        dataschema = dataschema,
        data_proc_elem = data_proc_elem,
        harmonized_col_dataset = harmonized_col_dataset,
        harmonized_col_id = harmonized_col_id,
        dossier = dossier,
        .debug = .debug))
  }
    
  if(is_dataset(dossier)) dossier <- list(dossier)
  
  if(is.null(names(dossier)))
    if(length(dossier) == 1 & !is.null(data_proc_elem))
      if(length(unique(data_proc_elem$input_dataset)) == 1 &
         !is.na(unique(data_proc_elem$input_dataset)))
        names(dossier) <- unique(data_proc_elem$input_dataset)
  
  if(is.null(names(dossier))){
    fargs <- as.list(match.call(expand.dots = TRUE))
    
    if(!is.null(fargs$object)){
      names(dossier) <- make_name_list(as.character(fargs['object']), dossier)
    }else{
      names(dossier) <- make_name_list(as.character(fargs['dossier']), dossier)
    }    
  }

  
  # check arguments. make sure col_id is harmonizable if exists
  dossier <- dossier_create(dossier)
  
  if(!is.null(data_proc_elem))
    if(length(dossier) == 1)
      if(length(unique(data_proc_elem$input_dataset)) == 1 |
         is.na(unique(data_proc_elem$input_dataset)))
        data_proc_elem$input_dataset <- names(dossier)
  
  if(!is.null(harmonized_col_id)){
    dossier <- 
      dossier %>% 
      lapply(function(x) 
        x %>% 
          as_dataset(harmonized_col_id)
    )}

  # check arguments
  # if(is.null(dataschema) & is.null(data_proc_elem) & !is.null(harmonized_col_id)){
  #   dossier <- 
  #     as_harmonized_dossier(
  #       dossier,
  #       harmonized_col_id = harmonized_col_id,
  #       harmonized_col_dataset = harmonized_col_dataset)  
  # return(harmo_process(dossier))}
  
  
  if(!is.null(data_proc_elem))
    if(length(dossier) == 1)
      if(length(unique(data_proc_elem$input_dataset)) == 1 &
         length(str_subset(data_proc_elem$`Mlstr_harmo::rule_category`,'id_creation')) == 0){
        
        if(is.null(harmonized_col_id)) 
          dossier <- add_index_dossier(dossier)
        
        id_creation <- data_proc_elem_get(dossier %>% lapply(function(x) x[1]))
        data_proc_elem <- bind_rows(id_creation,data_proc_elem)
         }

  # check arguments
  if(is.null(data_proc_elem)){
    dossier <- add_index_dossier(dossier)
    data_proc_elem <- data_proc_elem_get(dossier)
  }else{
    data_proc_elem <- as_data_proc_elem(data_proc_elem)
  }

  if(is.null(dataschema)){
    dataschema <- dataschema_extract(data_proc_elem)
  }else{
    dataschema <- as_dataschema_mlstr(dataschema)}
  
  # clean dataschema_variable and input dataset names
  data_proc_elem$dataschema_variable <- 
    extract_var(data_proc_elem$dataschema_variable)
  
  data_proc_elem$input_dataset <- 
    extract_var(data_proc_elem$input_dataset)
  
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
  
  if(is.null(harmonized_col_id)){
    harmonized_col_id <- 
      unique(dpe[
        dpe$`rule_category` %in% 'id_creation',][[
          'output_variable']])}
  
  # test if harmonized_col_id exists in dpe and dataschema
  if(! harmonized_col_id %in% dataschema$Variables$name){
    
    dataschema_col_id <- data_dict_extract(dossier[[1]][1])
    dataschema$Variables <-
      bind_rows(dataschema_col_id$Variables,dataschema$Variables)
    
#     stop(call. = FALSE,
# '\n\nThe harmonized_col_id `',harmonized_col_id,'`',
# '\nmust be present in your DataSchema and in the Data Processing Elements.')
    
  }

  
  # test if harmonized_col_id exists in dpe and dpe
  if(! harmonized_col_id %in% data_proc_elem$dataschema_variable)
    stop(message('ERROR 101'))
#     stop(call. = FALSE,
# '\n\nThe harmonized_col_id `',harmonized_col_id,'`',
# '\nmust be present in your DataSchema and in the Data Processing Elements.')

  dpe <- dpe %>%
    group_by(.data$`input_dataset`) %>%
    group_split() %>%
    as.list
  names(dpe) <- sort(unique(bind_rows(dpe)$input_dataset))

  vars <- extract_var(unique(data_proc_elem$dataschema_variable))
  dataschema <- 
    data_dict_filter(
      dataschema,filter_var = 
        paste0(c("name %in% c('",paste0(vars,collapse = "','"),"')"),
               collapse = "")) %>%
    as_dataschema(as_dataschema_mlstr = TRUE)
  
  if(!is.null(harmonized_col_dataset)){
    
    # test if harmonized_col_dataset exists
    if(! harmonized_col_dataset %in% dataschema$Variable$name)
      stop(call. = FALSE,
'\n\nIf declared, the harmonized_col_dataset `',harmonized_col_dataset,'`',
'\nmust be present in your DataSchema and in the Data Processing Elements.')
    
  }
  
  if(length(intersect(names(dossier), names(dpe))) == 0){
    stop(call. = FALSE, 
'The dataset list to be harmonized is empty.
    
This usually means that your dataset names in the Data Processing Elements 
(in the column `dataset_input`) do not match the names in your dossier list. 
Please correct elements and reprocess.')
    
  }
  
  # intersection of dossier and dpe
  dossier <- dossier[intersect(names(dossier), names(dpe))]
  
  # dossier <<- dossier
  # dpe <<- dpe
  
  # selection of needed columns
  for(i in names(dossier)){
    # stop()}
  
    create_id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    var_id <- extract_var(create_id_row[['input_variables']])
    
    names_in_dpe <- 
      str_squish(unlist(strsplit(
        dpe[[i]] %>% 
          dplyr::filter(
            .data$`input_variables` != '__BLANK__') %>%
          pull(.data$`input_variables`),split = ";"))) %>%
      unique %>% 
      extract_var
    
    dossier[[i]] <- try(as_dataset(dossier[[i]],col_id = var_id),silent = TRUE)
    
    if(class(dossier[[i]])[1] == 'try-error'){
      
      stop(message('ERROR 102'))
# stop(call. = FALSE, 
# 'In your Data Processing Elements, the input variable `',var_id,'` does not 
# exists in the input dataset `',create_id_row$`input_dataset`,'`.
# 
# This element is mandatory for the data processing to be initiated. 
# Please correct elements and reprocess.')
      
      
    }
    
    dossier[[i]] <- 
      dossier[[i]] %>% 
      select(all_of(col_id(dossier[[i]])),any_of(!! names_in_dpe))
  }
  
  # rid of data dictionary
  dossier <- 
    as_dossier(dossier) %>% 
    lapply(dataset_zap_data_dict)
  
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
    
    dossier[[i]][var_id] <- 
      dossier[[i]][var_id] %>% 
      mutate(across(all_of(var_id),as.character)) %>%
      mutate(across(all_of(var_id) , ~ paste0('{',row_number(.),'}::',.)))
    
    harmonization_id <-
      dossier[[i]][var_id] %>%
      rename_with(
        .cols = all_of(var_id), ~ create_id_row$output_variable)
    
    harmonized_dossier[[i]] <-
      harmonized_dossier[[i]] %>% bind_rows(harmonization_id)
    
    harmonized_dossier[[i]] <- harmonized_dossier_init[[i]] <-
      as_dataset(
        harmonized_dossier[[i]],
        col_id = create_id_row$output_variable)
  }
  
  message(bold(
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
bold(i)," -----------------------------------------------------"),1,81))
    
    create_id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    dataset_id <- extract_var(create_id_row[['input_variables']])
    input_dataset <- dossier[[i]]
    
    message(str_sub(paste0(
      str_trunc(paste0(
        "    processing ","1","/",nrow(dpe[[i]])," : ",
        harmonized_col_id),width = 49,ellipsis = '[...]'),
      "                                       "),1,50),
      bold("id created"))
    
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
      
      harmo_parse_process_rule <- 
      
      r_script <-
        get("harmo_parse_process_rule", envir = asNamespace("Rmonize"))(
          process_rule_slice = process_rule,
          input_dataset = input_dataset,
          r_script = TRUE)
      
      col <-
        get("harmo_parse_process_rule", envir = asNamespace("Rmonize"))(
          process_rule_slice = process_rule, 
          input_dataset = input_dataset, 
          r_script = FALSE)
      
      if(!is_error(col)){
        vT_test <- fabR::silently_run(
          as_valueType(unique(col[[j]]),
                       dataschema[['Variables']] %>%
                         dplyr::filter(.data$`name` == j) %>%
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
          bold("**ERROR**"))
        
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
            bold("**Warning(s)**"))
          
          warning_status <- conditionMessage(attributes(col)$`Rmonize::warning`)
          
        }else{
          
          status <- process_rule$`Mlstr_harmo::status`
          
          message(str_sub(paste0(
            str_trunc(paste0(
              "    processing ",idx,"/",nrow(dpe[[i]])," : ",
              process_rule$`output_variable`),width = 49,ellipsis = '[...]'),
            "                                       "),1,50),
            ifelse(status %in% 
                     c("undetermined",NA),bold(status),status)) 
          
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
  
  
  # creation of id
  for(i in names(harmonized_dossier)){
    # stop()}
  
    id_row <- 
      dpe[[i]][dpe[[i]]$`output_variable` %in% harmonized_col_id,]
    
    dossier[[i]][id_row$input_variables] <- 
      dossier[[i]][id_row$input_variables] %>% 
      mutate(
        across(all_of(id_row$input_variables) , ~ 
        str_remove_all(.,pattern = '^[^\\}\\:\\:]*\\}\\:\\:')))
    
    harmonized_dossier[[i]][id_row$output_variable] <-
      harmonized_dossier[[i]][id_row$output_variable] %>% 
      mutate(
        across(all_of(id_row$output_variable) , ~ 
        str_remove_all(.,pattern = '^[^\\}\\:\\:]*\\}\\:\\:')))
  }
  
  data_proc_elem <- 
    harmonization_report %>%
    rename(
      "dataschema_variable"        = "output_variable" ,
      "Mlstr_harmo::algorithm"     = "script"          ,
      "Mlstr_harmo::rule_category" = "rule_category"   )
  
  harmonized_dossier <- 
    as_harmonized_dossier(
      harmonized_dossier,dataschema,data_proc_elem,
      harmonized_col_id,harmonized_col_dataset,
      harmonized_data_dict_apply = TRUE)
  
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
      
      bold("\n\nUseful tip: "),
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
  
  message(bold(
    "
- WARNING MESSAGES (if any): ----------------------------------------------\n"))
  
  return(harmonized_dossier)
  
}

#' @title
#' Internal function that generates code for harmonized dataset
#'
#' @description
#' Generates code for harmonized dataset.
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
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
#' Data Processing Elements. 
#' @param input_dataset A data frame identifying the input table to be 
#' harmonized.
#' @param r_script R Script extracted form Data Processing Elements.
#'
#' @returns
#' A data frame identifying a harmonized dataset
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
    # Rmonize_examples$`Data Processing Elements` %>%
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
      replacement      = str_remove_all(.data$`input_variables`,'`'),
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = `",.data$`replacement`,"`)")) %>%
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
      replacement      =
        .data$`script` %>% str_squish(),
      to_eval_test     =
        paste0(
          "  mutate(\n",
          "  '",.data$`output_variable`,"' = ",.data$`replacement`,")")) %>%
    pull(.data$`to_eval_test`)

  return(process_script_to_eval)
}

#' @title
#' Internal function for 'dataset_id_creation' in the process of harmonization
#'
#' @description
#' Creates dataset id in the process of harmonization.
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
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
harmo_process_dataset_id_creation <- function(process_rule_slice){
  
  process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      =
        .data$`script` %>% str_squish(),
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
      replacement      = 
        .data$`script` %>% str_squish(),
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
        .data$`script` %>% str_squish(),
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' The Data Processing Elements specifies the algorithms used to process input 
#' variables into harmonized variables in the DataSchema format. It is also 
#' contains metadata used to generate documentation of the processing. 
#' A Data Processing Elements object is a data frame with specific columns 
#' used in data processing: `dataschema_variable`, `input_dataset`, 
#' `input_variables`, `Mlstr_harmo::rule_category` and `Mlstr_harmo::algorithm`. 
#' To initiate processing, the first entry must be the creation of a harmonized 
#' primary identifier variable (e.g., participant unique ID).
#'
#' @param process_rule_slice A one-rowed data frame, identifying the occurring
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
#' Print a summary of data processing in the console
#'
#' @description
#' Reads a harmonized dossier, product of [harmo_process()], to list processes, 
#' any errors, and an overview of each harmonization rule. The output printed 
#' in the console can help in correcting any errors that occurred during 
#' data processing.
#'
#' @details
#' A harmonized dossier is a named list containing one or more data frames, 
#' which are harmonized datasets. A harmonized dossier is generally the 
#' product of applying processing to a dossier object The name of each 
#' harmonized dataset (data frame) is taken from the reference input dataset. 
#' A harmonized dossier also contains the DataSchema and 
#' Data Processing Elements used in processing as attributes.
#'
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param show_warnings Whether the function should print warnings or not. 
#' TRUE by default.
#'
#' @returns
#' Nothing to be returned. The function prints messages in the console, 
#' showing any errors in the processing.
#'
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#'  
#' library(dplyr)
#' library(stringr)
#' 
#' # perform data processing
#' dossier            <- Rmonize_examples[str_detect(names(Rmonize_examples),"dataset")]
#' dataschema         <- Rmonize_examples$DataSchema
#' data_proc_elem     <- Rmonize_examples$`Data Processing Elements`
#' harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' # show error(s) on the console
#' show_harmo_error(harmonized_dossier)
#' 
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#' @importFrom crayon bold
#' @importFrom crayon green
#' @importFrom utils capture.output
#'
#' @export
show_harmo_error <- function(harmonized_dossier, show_warnings = TRUE){

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
        dplyr::filter(.data$`value` == TRUE) %>%
        select('dataschema_variable' = 'name','empty' = 'value')
      dpe <- 
        data_proc_elem %>% 
        dplyr::filter(.data$`input_dataset` == i & 
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
    mutate(error = !is.na(.data$`Rmonize::error_status`),
           warning = !is.na(.data$`Rmonize::warning_status`)) %>%
    unite(col = "Rmonize::status",
          "Rmonize::error_status","Rmonize::warning_status",
          na.rm = TRUE) %>%
    mutate(
      var = paste0(.data$`dataschema_variable`, " in ",
                   .data$`input_dataset`, " \n"),
      rule = paste0(.data$`Mlstr_harmo::algorithm`))

  if(show_warnings == FALSE){
    report_log <-
      report_log %>%
      dplyr::filter(.data$warning != TRUE)
  }
  
  if(nrow(report_log) > 0){
    message(bold(
paste0("\n
- ERROR",ifelse(show_warnings == TRUE,"/WARNING",""),
" STATUS DETAILS: -------------------------------------------\n")),
paste0("\nHere is the list of the errors",ifelse(show_warnings == TRUE," and warnings",""),
" encountered in the process 
of harmonization:\n"))
    
    for(i in seq_len(nrow(report_log))){
      message(
"---------------------------------------------------------------------------\n")
      message(bold(report_log$var[i]))
      message(bold("\nRule Category:"))
      message(report_log$`Mlstr_harmo::rule_category`[i])
      message(bold("\nAlgorithm:"))
      message(report_log$rule[i])
      message(bold("\nR Script:"))
      message(report_log$`Rmonize::r_script`[i])
      message(green(bold("\nR Error/Warning:")))
      message(green(report_log$`Rmonize::status`[i]))
    }
  }
  
  report_log <-  
    report_log %>% 
    group_by(.data$`error`,.data$`warning`) %>%
    count(.data$`Mlstr_harmo::rule_category`) %>%
    full_join(data_proc_elem %>%
                count(.data$`Mlstr_harmo::rule_category`),
              by = "Mlstr_harmo::rule_category") %>%
    ungroup %>%
    mutate(
      n.x = replace_na(.data$`n.x`,0),
      `Total number of errors`   =ifelse(.data$`error` %in% TRUE,.data$`n.x`,0),
      `Total number of warnings` =ifelse(.data$`warning` %in% TRUE,.data$`n.x`,0), 
      success = trunc((1 - .data$`Total number of errors`/.data$`n.y`)*100)) %>%
    arrange(.data$`success`) %>%
    mutate(success = paste0(.data$`success`," %")) %>%
    rename(`Total number of algorithms` = "n.y") %>%
    # mutate_all(as.character) %>%
    select(-'error',-'warning',-'n.x')
  
  if(sum(report_log$`Total number of errors`) == 0)
    report_log$`Total number of errors` <- NULL
  
  if(sum(report_log$`Total number of warnings`) == 0)
    report_log$`Total number of warnings` <- NULL
  
  message(bold(
"\n\n- STATUS SUMMARY: ----------------------------------------------------\n"))
  message(paste(capture.output({print(
    report_log %>%
      mutate(across(everything(), as.character))
  )}), collapse = "\n"))

  return(message(""))
}

#' @title
#' Generate a DataSchema based on Data Processing Elements
#'
#' @description
#' Generates a DataSchema from a Data Processing Elements.
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
#' @param data_proc_elem A Data Processing Elements object.
#' 
#' @returns
#' A list of data frame(s) named 'Variables' and (if any) 'Categories', with 
#' `Rmonize::class` 'dataschema'.
#'
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#' library(dplyr)
#'
#' dataschema <- dataschema_extract(Rmonize_examples$`Data Processing Elements`)
#' glimpse(dataschema)
#' 
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
#' Validate and coerce as a Data Processing Elements object
#'
#' @description
#' Checks if an object is a valid Data Processing Elements and returns it with 
#' the appropriate `Rmonize::class` attribute. This function mainly helps 
#' validate inputs within other functions of the package but could be used 
#' separately to ensure that an object has an appropriate structure.
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
#' @param object A potential Data Processing Elements object to be coerced.
#'
#' @returns
#' A data frame with `Rmonize::class` 'data_proc_elem'.
#'
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#' library(dplyr)
#'
#' data_proc_elem <- as_data_proc_elem(Rmonize_examples$`Data Processing Elements`)
#' 
#' head(data_proc_elem)
#' 
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
      group_by(.data$`input_dataset`) %>% group_split() %>% as.list() %>%
      lapply(function(x){
      col_id <- 
        x[x$`Mlstr_harmo::rule_category` %in% 'id_creation',][['dataschema_variable']]
      
      if(length(col_id) == 0)
        stop(call. = FALSE,
'In ',unique(x$input_dataset),': \n\n',
'In the column `Mlstr_harmo::rule_category` of your Data Processing Elements, 
"id_creation" rule is missing. The harmonization process cannot start because 
this step initiates the process for each of your processed datasets. 
Your first processing element must be "id_creation" in `Mlstr_harmo::rule_category` 
followed by the name of the column taken as identifier of dataset in your 
dossier.')
      
      if(length(col_id)  > 1)
        stop(call. = FALSE,
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
    stop(call. = FALSE,
'In ',unique(object$input_dataset),': \n\n',
'In the column `Mlstr_harmo::rule_category` of your Data Processing Elements, 
"id_creation" is not unique across the harmonized dataset(s) to generate. 
The harmonization process cannot start because this step initiates the process 
for each of your processed datasets and must be the same variable across the 
generated harmonized dataset(s). Your first processing element must be 
"id_creation" in `Mlstr_harmo::rule_category` followed by the name of the column taken 
identifier of dataset in your dossier.')
  
  # harmo status must be either NA, complete, impossible or undetermined

  # step cleaning : addition of missing columns
  object <-
    object %>%
    bind_rows(tibble(
      `valueType` = as.character(),
      `Mlstr_harmo::status` = as.character(),
      `Mlstr_harmo::status_detail` = as.character(),
      `Mlstr_harmo::comment` = as.character()))

  
  # step cleaning : addition of `Mlstr_harmo::rule_category`.
  object <-
    object %>%
    mutate(
      `Mlstr_harmo::rule_category` = case_when(
        if_any(c("input_variables",
                 "Mlstr_harmo::rule_category",
                 "Mlstr_harmo::algorithm"), ~ . == "undetermined") ~ 
          'undetermined',
        is.na(`Mlstr_harmo::rule_category`)                        ~ 
          'undetermined',
        TRUE                                                       ~ 
          `Mlstr_harmo::rule_category`))
  
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
  
  # step cleaning : id_creation must be the first entry
  object <-
    object %>%
    group_by(.data$`input_dataset`) %>%
    group_modify(.f = ~ 
                bind_rows(
                  dplyr::filter(., `Mlstr_harmo::rule_category` %in% 'id_creation'),
                  dplyr::filter(.,!`Mlstr_harmo::rule_category` %in% 'id_creation')))
    
  if(is.null(object[['index']])){
    object <-
      object %>%
      add_index()}

  # step cleaning : addition of index
  if(is.null(object[['index']])){
    object <-
      object %>%
      add_index()}
  
  # if all test pass:
  object <- 
    object %>%
    ungroup %>%
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
Maelstrom standards, which must be a data frame containing at least these columns:
  `dataschema_variable`
  `input_dataset`
  `input_variables`
  `Mlstr_harmo::rule_category`
  `Mlstr_harmo::algorithm`
  
Please refer to documentation.")

  return(object)
}

#' @title
#' Validate and coerce as a DataSchema object
#'
#' @description
#' Checks if an object is a valid DataSchema and returns it with the appropriate 
#' `Rmonize::class` attribute. This function mainly helps validate inputs within 
#' other functions of the package but could be used separately to ensure that an 
#' object has an appropriate structure.
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
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param object A potential DataSchema object to be coerced.
#' @param as_dataschema_mlstr Whether the output DataSchema should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. FALSE by default.
#'
#' @returns
#' A list of data frame(s) named 'Variables' and (if any) 'Categories', 
#' with `Rmonize::class` 'dataschema'.
#'
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#' library(dplyr)
#'
#' dataschema <- as_dataschema(Rmonize_examples$`DataSchema`)
#' glimpse(dataschema)
#' 
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
as_dataschema <- function(object, as_dataschema_mlstr = FALSE){

  if(!is.logical(as_dataschema_mlstr))
    stop(call. = FALSE,
         '`as_dataschema_mlstr` must be TRUE or FALSE (FALSE by default)')

  if(is_data_dict(object)){
    
    if(as_dataschema_mlstr == TRUE){
      if(is_data_dict_mlstr(object)){
        object <- as_data_dict_mlstr(object,name_standard = FALSE)
        attributes(object)$`Rmonize::class` <- "dataschema_mlstr"
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
bold("Useful tip:\n"),
"Use dataschema_evaluate(dataschema) to get an assessment of your DataSchema")
  
}

#' @title
#' Validate and coerce as a DataSchema object with specific format restrictions
#'
#' @description
#' Checks if an object is a valid DataSchema with specific format restrictions 
#' for compatibility with other Maelstrom Research software and returns it with 
#' the appropriate `Rmonize::class` attribute. This function mainly helps validate 
#' inputs within other functions of the package but could be used separately to 
#' ensure that an object has an appropriate structure.
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
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param object A potential DataSchema object to be coerced.
#'
#' @returns
#' A list of data frame(s) named 'Variables' and (if any) 'Categories', with 
#' `Rmonize::class` 'dataschema_mlstr'.
#'
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#' library(dplyr)
#'
#' dataschema_mlstr <- as_dataschema_mlstr(Rmonize_examples$`DataSchema`)
#' glimpse(dataschema_mlstr)
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
  attributes(object)$`Rmonize::class` <- "dataschema_mlstr"

  return(object)
}


#' @title
#' Validate and coerce as a harmonized dossier object
#'
#' @description
#' Checks if an object is a valid harmonized dossier and returns it with the 
#' appropriate `Rmonize::class` attribute. This function mainly helps validate 
#' inputs within other functions of the package but could be used separately to 
#' ensure that an object has an appropriate structure. The function 
#' has two arguments that can optionally be declared by the user 
#' (`unique_col_dataset` and `unique_col_id`). `unique_col_dataset` refers to 
#' the columns which contains name of each harmonized dataset. `unique_col_id` 
#' refers to the column in harmonized datasets which identifies unique 
#' combinations of observation/dataset. These two columns are added to ensure 
#' that there is always a unique entity identifier when datasets are pooled.
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
#' @param object A A potential harmonized dossier object to be coerced.
#' @param dataschema A DataSchema object.
#' @param data_proc_elem A Data Processing Elements object.
#' @param harmonized_col_id A character string identifying the name of the 
#' column present in every dataset to use as a dataset identifier.
#' @param harmonized_col_dataset A character string identifying the column 
#' to use for dataset names.
#' @param harmonized_data_dict_apply Whether to apply the dataschema to each 
#' harmonized dataset. FALSE by default.
#'
#' @returns
#' A list of data frame(s), containing harmonized dataset(s). 
#' The DataSchema and Data Processing Elements are preserved as attributes of 
#' the output harmonized dossier.
#'
#' @examples
#' {
#' 
#' # Use Rmonize_examples to run examples.
#'  
#' library(dplyr)
#' library(stringr)
#' 
#' # perform data processing
#' dossier            <- Rmonize_examples[str_detect(names(Rmonize_examples),"dataset")]
#' dataschema         <- Rmonize_examples$DataSchema
#' data_proc_elem     <- Rmonize_examples$`Data Processing Elements`
#' harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' harmonized_dossier <- as_harmonized_dossier(harmonized_dossier)
#' 
#' glimpse(harmonized_dossier$dataset_study1)
#'   
#' }
#'
#' @import dplyr fabR
#' @importFrom rlang .data
#'
#' @export
as_harmonized_dossier <- function(
    object, 
    dataschema = attributes(object)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(object)$`Rmonize::Data Processing Elements`,
    harmonized_col_id = attributes(object)$`Rmonize::harmonized_col_id`,
    harmonized_col_dataset = attributes(object)$`Rmonize::harmonized_col_dataset`,
    harmonized_data_dict_apply = FALSE){

  silently_run(dossier_create(object))
  
  if(!is.logical(harmonized_data_dict_apply))
    stop(call. = FALSE,
         '`harmonized_data_dict_apply` must be TRUE or FALSE (TRUE by default)')

  # check the id column 
  if(is.null(harmonized_col_id))
    stop(message('ERROR 103'))
  #   stop(call. = FALSE,
  #        '`harmonized_col_id` must be provided')
  
  # check if col_id exists
  bind_rows(
    object %>% lapply(function(x) x %>% 
                        mutate(across(everything(),as.character)))) %>% 
    select(all_of(harmonized_col_id))
  
  # check the DataSchema
  if(is.null(dataschema)){
    dataschema <- data_dict_extract(bind_rows(as.list(object)))
    dataschema$Variables <- 
      dataschema$Variables %>%
      select(-starts_with("Mlstr_harmo::"),-starts_with("Rmonize::"))}

  dataschema <- as_dataschema(dataschema,as_dataschema_mlstr = TRUE)
  
  # check the DPE
  if(is.null(data_proc_elem)){

    ## creation of the DPE from the variables in the DataSchema
    data_proc_elem <- 
      tibble(input_dataset = names(object)) %>% 
      cross_join(tibble(
        input_variables = dataschema$Variables$name,
        valueType = dataschema$Variables$valueType)) %>%
      mutate(
        dataschema_variable = .data$`input_variables`,
        `Mlstr_harmo::rule_category` = ifelse(
          .data$`input_variables` == harmonized_col_id, 
          'id_creation','direct_mapping'),
        `Rmonize::r_script` = 
          ifelse(
            .data$`input_variables` == harmonized_col_id,
          paste0(
          .data$`input_dataset`, 
          " %>% \n  select('",.data$`input_variables`,
          "' = '",.data$`input_variables`,"')"),
          paste0(
            .data$`input_dataset`, 
            " %>% \n  mutate('",.data$`input_variables`,
            "' = `",.data$`input_variables`,"`) %>% ",
            "\n  select('",harmonized_col_id,"','",.data$`dataschema_variable`,"')")),
        `Mlstr_harmo::algorithm` = .data$`Mlstr_harmo::rule_category`)
  }
  
  data_proc_elem <- as_data_proc_elem(data_proc_elem)
  dataschema <- as_data_dict_mlstr(dataschema)
  
  # Warn user that the dataschema should have a categorical variable if
  # harmonized_col_dataset is provided.
  
  if(!is.null(harmonized_col_dataset)){

    # test if harmonized_col_dataset exists
    bind_rows(
      as.list(object) %>% lapply(function(x) x %>% 
                          mutate(across(everything(),as.character)))) %>% 
      select(all_of(harmonized_col_dataset))
    
  }
  
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
  
  if(!same_names) 
    stop(call. = FALSE,
"All of your column(s) in the harmonized dataset(s) must be in the DataSchema 
name list of variables.")
  
  
  # Assignation of col_id in the datasets
  object <- 
    object %>% lapply(function(x) as_dataset(x, col_id = harmonized_col_id))
  
  if(harmonized_data_dict_apply == TRUE){
    
    message(bold("\n
- CREATION OF HARMONIZED DATA DICTIONARY : --------------------------------\n"))
    

    
    for(i in names(object)){
      # stop()}
      
      harmo_data_dict <- dataschema
      harmo_data_dict[['Variables']][['Mlstr_harmo::rule_category']] <- NULL
      harmo_data_dict[['Variables']][['Mlstr_harmo::algorithm']] <- NULL
      harmo_data_dict[['Variables']][['Rmonize::r_script']] <- NULL
      harmo_data_dict[['Variables']][['Mlstr_harmo::comment']] <- NULL
      harmo_data_dict[['Variables']][['Mlstr_harmo::status']] <- NULL
      harmo_data_dict[['Variables']][['Mlstr_harmo::status_detail']] <- NULL
      
      input_data_proc_elem <-
        data_proc_elem %>%
        rename("name" = "dataschema_variable") %>%
        dplyr::filter(.data$`input_dataset` == !! i) %>%
        select('name', 
               'Mlstr_harmo::rule_category',
               'Mlstr_harmo::algorithm',
               matches('^Rmonize::r_script$'),
               'Mlstr_harmo::status',
               'Mlstr_harmo::status_detail',
               'Mlstr_harmo::comment')
      
      harmo_data_dict[['Variables']] <-
        harmo_data_dict[['Variables']] %>%
        full_join(input_data_proc_elem, by = 'name')
      
      object[[i]] <- 
        valueType_adjust(
          from = harmo_data_dict,
          to = as_dataset(object[[i]])) %>%
        dataset_zap_data_dict() %>%
        data_dict_apply(harmo_data_dict)
      
      message(bold(i)," : done")
    }
  }
  
  # if all checks TRUE
  attributes(object)$`Rmonize::class` <- "harmonized_dossier"
  attributes(object)$`Rmonize::DataSchema` <- dataschema
  attributes(object)$`Rmonize::Data Processing Elements` <- data_proc_elem
  attributes(object)$`Rmonize::harmonized_col_id` <- harmonized_col_id
  
  if(!is.null(harmonized_col_dataset)){
    attributes(object)$`Rmonize::harmonized_col_dataset` <- harmonized_col_dataset
  }
  return(object)
}

#' @title
#' Generate a pooled harmonized dataset from a harmonized dossier
#' 
#' @description
#' Generates a pooled harmonized dataset from a harmonized dossier. The function 
#' has two arguments that can optionally be declared by the user 
#' (`unique_col_dataset` and `unique_col_id`). `unique_col_dataset` refers to 
#' the columns which contains name of each harmonized dataset. `unique_col_id` 
#' refers to the column in harmonized datasets which identifies unique 
#' combinations of observation/dataset. These two columns are added to ensure 
#' that there is always a unique entity identifier when datasets are pooled.
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
#' @param harmonized_dossier A list containing the harmonized dataset(s).
#' @param harmonized_col_id A character string identifying the name of the 
#' column present in every dataset to use as a dataset identifier.
#' @param harmonized_col_dataset A character string identifying the column 
#' to use for dataset names.
#' @param add_col_dataset Whether to add an extra column to each 
#' harmonized dataset. The resulting data frame will have an additional column 
#' and its data dictionary will be updated accordingly adding categories for 
#' this variable if necessary. FALSE by default.
#' @param dataschema A DataSchema object.
#' @param data_proc_elem A Data Processing Elements object.
#'
#' @returns
#' A data frame containing the pooled harmonized dataset.
#' 
#' @examples
#' {
#'
#' # Use Rmonize_examples to run examples.
#'  
#' library(dplyr)
#' library(stringr)
#' 
#' # perform data processing
#' dossier            <- Rmonize_examples[str_detect(names(Rmonize_examples),"dataset")]
#' dataschema         <- Rmonize_examples$DataSchema
#' data_proc_elem     <- Rmonize_examples$`Data Processing Elements`
#' harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)
#' 
#' # create the pooled harmonized dataset from the harmonized dossier
#' pooled_harmonized_dataset <- 
#'   pooled_harmonized_dataset_create(
#'     harmonized_dossier,
#'     harmonized_col_id = 'adm_unique_id')
#'   
#' glimpse(pooled_harmonized_dataset)
#'   
#' }
#'
#' @import dplyr fabR
#' @importFrom rlang .data
#'
#' @export
pooled_harmonized_dataset_create <- function(
    harmonized_dossier,
    harmonized_col_dataset = attributes(harmonized_dossier)$`Rmonize::harmonized_col_dataset`,
    harmonized_col_id = attributes(harmonized_dossier)$`Rmonize::harmonized_col_id`,
    add_col_dataset = FALSE,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`){
  
  if(!is.logical(add_col_dataset))
    stop(call. = FALSE,
         '`add_col_dataset` must be TRUE or FALSE (TRUE by default)')
  
  # check harmonized dossier (except harmonized_col_dataset)
  as_harmonized_dossier(
    object = harmonized_dossier,
    harmonized_col_id = harmonized_col_id,
    dataschema = dataschema,
    data_proc_elem = data_proc_elem)

  pooled_harmonized_dataset <- 
    bind_rows(harmonized_dossier) %>%
    data_dict_apply(dataschema)

  col_dataset <- 
    tibble(.rows = nrow(pooled_harmonized_dataset))
  
  if(is.null(harmonized_col_dataset)){
    if(add_col_dataset == TRUE){
      # if the harmonized_col_dataset is null 
      harmonized_col_dataset <- 'Rmonize::harmonized_col_dataset'
    }
  }
  
  # test whether the harmonized_col_dataset exists and is categorical (
  # NA dont exists but to add
  # TRUE exists and categorical
  # FALSE exists not categorical
  # NULL is not provided)
  
  bool_test <- 
    if(!is.null(harmonized_col_dataset)){
      toString(
        pooled_harmonized_dataset %>% reframe(
          across(any_of(harmonized_col_dataset),  is_category))) %>%
        as.logical %>% 
        as_any_boolean == FALSE}else{NULL}
  
  if(!is.null(bool_test)){
    
    if(bool_test %in% FALSE){} # nothing to warn. the col exists and is categorical
    
    if(bool_test %in% TRUE){
      warning(call. = FALSE,
'\nThe harmonized_col_dataset `',harmonized_col_dataset,'` you declared is not ',
'categorical in your DataSchema.',
'\nThe correspondant categories have been be created in your pooled harmonized dataset.',
bold("\n\nUseful tip:\n"),
'To avoid this warning, we recommend to add the categories in your DataSchema.')

  }
  
    if(is.na(bool_test) & add_col_dataset == TRUE) {
      warning(call. = FALSE,
'\nAn additional variable `',harmonized_col_dataset,'` has been created
with harmonized dataset names as values for each harmonized dataset.',
bold("\n\nUseful tip:\n"),
'To avoid this message, we recommend to set `harmonized_col_dataset` as a 
categorical variable DataSchema, or to set `add_col_dataset` = FALSE.')
    
      col_dataset <- harmonized_dossier
    
      for(i in names(col_dataset)){
        # stop()}
        col_dataset[[i]] <- 
          col_dataset[[i]] %>% 
          mutate(`Rmonize::harmonized_col_dataset` = i) %>%
          select(!!harmonized_col_dataset := 'Rmonize::harmonized_col_dataset')}
      
      col_dataset <- bind_rows(col_dataset)
      
    }
    
    if(is.na(bool_test) & add_col_dataset == FALSE) {
      stop(call. = FALSE,
'\nThe harmonized_col_dataset `',harmonized_col_dataset,'` you declared is not ',
'in your DataSchema.',
bold("\n\nUseful tip:\n"),
'To avoid this error, we recommend to set `harmonized_col_dataset` as a 
categorical variable DataSchema, or to set `add_col_dataset` = FALSE.')
    
    }
  }
  
  pooled_harmonized_dataset <-
    pooled_harmonized_dataset %>%
    bind_cols(col_dataset) %>%
    mutate(across(any_of(harmonized_col_dataset), as_category))

  attributes(pooled_harmonized_dataset)$`Rmonize::class` <- 
    "pooled_harmonized_dataset"
  
  attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_id` <- 
    harmonized_col_id
  
  if(!is.null(harmonized_col_dataset)){
    attributes(pooled_harmonized_dataset)$`Rmonize::harmonized_col_dataset` <- 
      harmonized_col_dataset}

  return(pooled_harmonized_dataset)
}

#' @title
#' Test for a valid DataSchema object with specific format restrictions
#'
#' @description
#' Tests if an object is a valid DataSchema object with specific format 
#' restrictions for compatibility with other Maelstrom Research software. This 
#' function mainly helps validate input within other functions of the package 
#' but could be used to check if an object is valid for use in a function.
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
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @seealso
#' For a better assessment, please use [dataschema_evaluate()].
#'
#' @param object A potential DataSchema object to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use Rmonize_examples provided by the package
#'
#' is_dataschema_mlstr(Rmonize_examples$`DataSchema`)
#' is_dataschema_mlstr(Rmonize_examples$`Data Processing Elements`)
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
#' Test for a valid DataSchema object
#'
#' @description
#' Tests if the input is a valid DataSchema object. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [dataschema_evaluate()].
#'
#' @param object A potential DataSchema object to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use Rmonize_examples provided by the package
#'
#' is_dataschema(Rmonize_examples$`DataSchema`)
#' is_dataschema(Rmonize_examples$`Data Processing Elements`)
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
#' Test for a valid Data Processing Elements object
#'
#' @description
#' Tests if the input is a valid Data Processing Elements object. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if an object is valid for use in a function.
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
#' @param object A potential Data Processing Elements object to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use Rmonize_examples provided by the package
#'
#' data_proc_elem <- Rmonize_examples$`Data Processing Elements`
#' is_data_proc_elem(Rmonize_examples$`Data Processing Elements`)
#' is_data_proc_elem(Rmonize_examples$`DataSchema`)
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
