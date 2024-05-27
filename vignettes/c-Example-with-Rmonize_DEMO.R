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
#  Rmonize_website()
#  
#  # Downloadable templates are available here
#  Rmonize_templates()
#  
#  # Demo files are available here, along with an online demonstration process
#  Rmonize_DEMO
#  

## -----------------------------------------------------------------------------
#  # To see contents
#  names(Rmonize_DEMO)
#  print(Rmonize_DEMO$dataset_TOKYO)	                     # An input dataset
#  print(Rmonize_DEMO$data_dict_TOKYO)             	     # An input data dictionary
#  print(Rmonize_DEMO$`data_processing_elements - final`) # A Data Processing Elements
#  print(Rmonize_DEMO$`dataschema - final`)	             # A DataSchema
#  

## -----------------------------------------------------------------------------
#  # as_dataschema and as_data_proc_elem will check the structure of object and
#  # assign attributes to them.
#  
#  dataschema <- as_dataschema(Rmonize_DEMO$`dataschema - final`)
#  data_proc_elem <- as_data_proc_elem(Rmonize_DEMO$`data_processing_elements - final`)
#  

## -----------------------------------------------------------------------------
#  # Associate metadata from input data dictionaries to the input datasets.
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
#  
#  # Group the datasets into a dossier object.
#  # NB: The names of the datasets in the dossier must match the column
#  # input_dataset in the Data Processing Elements
#  
#  dossier <- dossier_create( dataset_list = list(
#    dataset_MELBOURNE,
#    dataset_PARIS,
#    dataset_TOKYO))
#  

## -----------------------------------------------------------------------------
#  
#  harmonized_dossier <- harmo_process(
#      dossier,
#      dataschema,
#      data_proc_elem)
#  

## -----------------------------------------------------------------------------
#  
#  show_harmo_error(harmonized_dossier)
#  

## -----------------------------------------------------------------------------
#  # Evaluate and summarize a harmonized dossier containing multiple harmonized datasets.
#  
#  harmonized_dossier_evaluation <- harmonized_dossier_evaluate(harmonized_dossier)
#  harmonized_dossier_summary <- harmonized_dossier_summarize(harmonized_dossier)
#  

## -----------------------------------------------------------------------------
#  # place your harmonized dossier in a folder. This folder name is mandatory, and
#  # must not previously exist.
#  
#  bookdown_path <- paste0('tmp/',basename(tempdir()))
#  
#  harmonized_dossier_visualize(harmonized_dossier, bookdown_path)
#  
#  # Open the visual report in a browser.
#  bookdown_open(bookdown_path)
#  

## -----------------------------------------------------------------------------
#  
#  # Generate one pooled harmonized dataset from a harmonized dossier
#  pooled_harmonized_dataset <-
#    pooled_harmonized_dataset_create(harmonized_dossier)
#  

## -----------------------------------------------------------------------------
#  
#  # Extract the harmonized data dictionary for one harmonized dataset.
#  
#  harmonized_TOKYO_dd <- data_dict_extract(harmonized_dossier$dataset_TOKYO)
#  

## -----------------------------------------------------------------------------
#  
#  library(fabR)
#  ## Examples of exporting objects as Excel files.
#  
#  # write_excel_allsheets(harmonized_dossier, "myfile.xlsx")
#  # write_excel_allsheets(harmonized_dossier_summary, "myfile.xlsx")
#  # write_excel_allsheets(harmonized_TOKYO_dd, "myfile.xlsx")
#  

