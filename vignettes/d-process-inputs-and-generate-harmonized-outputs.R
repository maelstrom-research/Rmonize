## ----include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Load relevant packages
# library(Rmonize)
# library(tidyverse) # Collection of R packages for data science

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get the input datasets
# dataset_study1 <- Rmonize_examples$input_dataset_study1
# dataset_study2 <- Rmonize_examples$input_dataset_study2
# dataset_study3 <- Rmonize_examples$input_dataset_study3
# dataset_study4 <- Rmonize_examples$input_dataset_study4
# dataset_study5 <- Rmonize_examples$input_dataset_study5
# 
# # Get the DataSchema
# dataschema <- Rmonize_examples$DataSchema
# 
# # Get the Data Processing Elements
# dpe_with_errors <- Rmonize_examples$Data_Processing_Elements_with_errors
# # This version contains some examples of potential processing errors.

## ----eval=FALSE----------------------------------------------------------------------------------
# # Create an input dossier
# input_dossier <- dossier_create(list(
#   dataset_study1,
#   dataset_study2,
#   dataset_study3,
#   dataset_study4,
#   dataset_study5))
# 
# # Run processing function
# harmonized_dossier_with_errors <- harmo_process(
#   object = input_dossier,
#   dataschema = dataschema,
#   data_proc_elem = dpe_with_errors,
#   harmonized_col_dataset = 'adm_study_id') # Identifies the harmonized variable to use as dataset identifiers

## ----eval=FALSE----------------------------------------------------------------------------------
# # To identify processing errors to correct in the DPE
# show_harmo_error(
#   harmonized_dossier_with_errors,
#   show_warnings = TRUE) # Can be informative, but can also be turned off, e.g., if there are known warnings produced by processing algorithms

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get corrected DPEs with changes made based on error messages
# dpe_no_errors <- Rmonize_examples$Data_Processing_Elements_no_errors %>%
#   as_data_proc_elem()
# 
# # Run processing function
# harmonized_dossier <- harmo_process(
#   object = input_dossier,
#   dataschema = dataschema,
#   data_proc_elem = dpe_no_errors,
#   harmonized_col_dataset = 'adm_study_id' # Identifies the harmonized variable to use as dataset identifiers
# )
# 
# # Confirm there are no errors
# show_harmo_error(
#   harmonized_dossier,
#   show_warnings = TRUE
# )

## ----eval=FALSE----------------------------------------------------------------------------------
# # Save the harmonized dossier as R file
# saveRDS(harmonized_dossier, "harmonized_dossier.rds")

