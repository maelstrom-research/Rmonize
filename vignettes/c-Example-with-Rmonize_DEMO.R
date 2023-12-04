## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = T,
  results = "hide",
  eval = FALSE)


## ----eval=FALSE---------------------------------------------------------------
#  # To install Rmonize:
#  install.packages('Rmonize')
#  
#  library(Rmonize)
#  # If you need help with the package, please use:
#  Rmonize_help()
#  
#  # Downloadable templates are available here
#  Rmonize_templates()
#  
#  # Demo files are available here, along with an online demonstration process
#  Rmonize_DEMO()
#  

## -----------------------------------------------------------------------------
#  
#  names(Rmonize_DEMO)
#  # To see examples
#  # View(Rmonize_DEMO$data_dict_TOKYO)   # A data dictionary
#  # View(Rmonize_DEMO$dataset_TOKYO)                      # A datasets
#  # View(Rmonize_DEMO$`data_processing_elements - final`) # A Data Processing Elements
#  # View(Rmonize_DEMO$`dataschema - final`)               # A DataSchema
#  

## -----------------------------------------------------------------------------
#  # Associate metadata from data dictionary to the the data
#  
#  dataset_MELBOURNE <- data_dict_apply(
#    dataset = Rmonize_DEMO$dataset_MELBOURNE,
#    data_dict = Rmonize_DEMO$data_dict_MELBOURNE)
#  
#  dataset_PARIS <- data_dict_apply(
#    dataset = Rmonize_DEMO$dataset_PARIS,
#    data_dict = Rmonize_DEMO$data_dict_PARIS)
#  
#  dataset_TOKYO <- data_dict_apply(
#    dataset = Rmonize_DEMO$dataset_TOKYO,
#    data_dict = Rmonize_DEMO$data_dict_TOKYO)
#  

## -----------------------------------------------------------------------------
#  # Group the dataset in a dossier object
#  # NB: The names of the datasets in the dossier must match the column
#  # input_dataset in the Data Processing Elements
#  
#  dossier <- dossier_create(
#    dataset_list = list(
#      dataset_MELBOURNE,
#      dataset_PARIS,
#      dataset_TOKYO))
#  
#  dataschema <- Rmonize_DEMO$`dataschema - final`
#  data_proc_elem <- Rmonize_DEMO$`data_processing_elements - final`
#  

## -----------------------------------------------------------------------------
#  
#  # Data processing
#  
#  harmonized_dossier <- harmo_process(dossier, dataschema, data_proc_elem)
#  show_harmo_error(harmonized_dossier)
#  

## -----------------------------------------------------------------------------
#  
#  # These reports can be downloaded as Excel files
#  harmonized_dossier_evaluate <- harmonized_dossier_evaluate(harmonized_dossier)
#  harmonized_dossier_summary <- harmonized_dossier_summarise(harmonized_dossier)
#  
#  bookdown_path <- paste0('tmp/',basename(tempdir()))
#  
#  harmonized_dossier_visualize(
#    harmonized_dossier,
#    bookdown_path = bookdown_path,
#    group_by = 'adm_study')
#  

