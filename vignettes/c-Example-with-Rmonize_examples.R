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
#  # Example files are available here, along with an online illustrative process
#  Rmonize_examples
#  

## -----------------------------------------------------------------------------
#  # To see contents
#  names(Rmonize_examples)
#  print(Rmonize_examples$dataset_study1)             # An input dataset
#  print(Rmonize_examples$`Data Processing Elements`) # A Data Processing Elements
#  print(Rmonize_examples$`DataSchema`)	             # A DataSchema
#  

## -----------------------------------------------------------------------------
#  # as_dataschema and as_data_proc_elem will check the structure of object and
#  # assign attributes to them.
#  
#  dataschema <- as_dataschema(Rmonize_examples$`Data Processing Elements`)
#  data_proc_elem <- as_data_proc_elem(Rmonize_examples$`DataSchema`)
#  

## -----------------------------------------------------------------------------
#  
#  dataset_study1 <- Rmonize_examples$dataset_study1
#  dataset_study2 <- Rmonize_examples$dataset_study2
#  dataset_study3 <- Rmonize_examples$dataset_study3
#  dataset_study4 <- Rmonize_examples$dataset_study4
#  dataset_study5 <- Rmonize_examples$dataset_study5
#  
#  
#  

## -----------------------------------------------------------------------------
#  
#  # Group the datasets into a dossier object.
#  # NB: The names of the datasets in the dossier must match the column
#  # input_dataset in the Data Processing Elements
#  
#  dossier <- dossier_create(
#    dataset_list = list(
#      dataset_study1,
#      dataset_study2,
#      dataset_study3,
#      dataset_study4,
#      dataset_study5))
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
#  harmonized_data_dict_study1 <- data_dict_extract(harmonized_dossier$dataset_study1)
#  

## -----------------------------------------------------------------------------
#  
#  library(fabR)
#  ## Examples of exporting objects as Excel files.
#  
#  # write_excel_allsheets(harmonized_dossier, "myfile.xlsx")
#  # write_excel_allsheets(harmonized_dossier_summary, "myfile.xlsx")
#  # write_excel_allsheets(harmonized_data_dict_study1, "myfile.xlsx")
#  

