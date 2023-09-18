## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  echo = TRUE)


## ---- eval = TRUE, echo = FALSE-----------------------------------------------
source('datatables.R')


## -----------------------------------------------------------------------------
#  
#  dataset_MELBOURNE_1 <- DEMO_files_harmo$dataset_MELBOURNE_1
#  dataset_MELBOURNE_2 <- DEMO_files_harmo$dataset_MELBOURNE_2
#  dataset_PARIS <- DEMO_files_harmo$dataset_PARIS
#  dataset_TOKYO <- DEMO_files_harmo$dataset_TOKYO
#  
#  # create the dossier
#  dossier <- dossier_create(list(
#    dataset_MELBOURNE_1, dataset_MELBOURNE_2, dataset_PARIS, dataset_TOKYO))
#  
#  dataschema <- DEMO_files_harmo$`dataschema - final`
#  data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final`
#  
#  # perform harmonization
#  harmo_process(dossier,dataschema,data_proc_elem)
#  

## -----------------------------------------------------------------------------
#  
#  data_proc_elem <-
#    DEMO_files_harmo$`data_processing_elements - with error`
#  
#  # perform harmonization
#  harmonized_dossier <- harmo_process(dossier, dataschema, data_proc_elem)
#  
#  # use the function to help correcting elements of the harmonization;
#  show_harmo_error(harmonized_dossier)
#  

