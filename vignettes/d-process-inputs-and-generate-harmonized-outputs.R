## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # Load relevant packages
# library(Rmonize)
# library(tidyverse) # Collection of R packages for data science

## ----eval=FALSE---------------------------------------------------------------
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

## ----eval=FALSE---------------------------------------------------------------
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

## ----fig.cap="Subset of processing information printed in the console, including messages about errors in running individual algorithms.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig4_fig01.png")

## ----eval=FALSE---------------------------------------------------------------
# # To identify processing errors to correct in the DPE
# show_harmo_error(
#   harmonized_dossier_with_errors,
#   show_warnings = TRUE) # Can be informative, but can also be turned off, e.g., if there are known warnings produced by processing algorithms

## ----fig.cap="Subset of output from show_harmo_error() printed in the console.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig4_fig02.png")

## ----fig.cap="Example of locating the errors in the DPE document.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig4_fig03.png")

## ----eval=FALSE---------------------------------------------------------------
# # Get corrected DPEs with changes made based on error messages
# dpe_no_errors <- Rmonize_examples$`Data_Processing_Elements_no errors` %>%
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

## ----eval=FALSE---------------------------------------------------------------
# # Save the harmonized dossier as R file
# saveRDS(harmonized_dossier, "harmonized_dossier.rds")

