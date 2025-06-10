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
# # See available examples
# names(Rmonize_examples)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get the input datasets
# dataset_study1 <- Rmonize_examples$input_dataset_study1
# dataset_study2 <- Rmonize_examples$input_dataset_study2
# dataset_study3 <- Rmonize_examples$input_dataset_study3
# dataset_study4 <- Rmonize_examples$input_dataset_study4
# dataset_study5 <- Rmonize_examples$input_dataset_study5

## ----eval=FALSE----------------------------------------------------------------------------------
# # Group multiple datasets into a dossier.
# # IMPORTANT: The names of the datasets in the dossier must match the column
# # input_dataset in the Data Processing Elements.
# # These will also be used in documentation of the harmonization outputs.
# 
# input_dossier <- dossier_create(list(
#   dataset_study1,
#   dataset_study2,
#   dataset_study3,
#   dataset_study4,
#   dataset_study5))

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get a DataSchema
# dataschema <- Rmonize_examples$DataSchema

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get the Data Processing Elements
# dpe <- Rmonize_examples$Data_Processing_Elements_no_errors
# 
# # Get the Data Processing Elements for a single dataset (study1)
# dpe_study1 <- dpe %>%
#   filter(input_dataset == "dataset_study1")

## ----eval=FALSE----------------------------------------------------------------------------------
# # Run processing function on all five datasets
# harmonized_dossier <- harmo_process(
#   object = input_dossier,
#   dataschema = dataschema,
#   data_proc_elem = dpe,
#   harmonized_col_dataset = 'adm_study_id') # Identifies the harmonized variable to use as dataset identifiers
# 
# # Run processing function on a single dataset
# harmonized_dossier_study1 <- harmo_process(
#   object = dataset_study1,
#   dataschema = dataschema,
#   data_proc_elem = dpe_study1,
#   harmonized_col_dataset = 'adm_study_id') # Identifies the harmonized variable to use as dataset identifiers

## ----eval=FALSE----------------------------------------------------------------------------------
# # Produce a summary report of the harmonized datasets and variables
# summary_report_harmonized_dossier <- harmonized_dossier_summarize(harmonized_dossier)
# 
# # Produce a visual report of the harmonized datasets and variables
# # You must specify a folder to contain the visual report files, and the folder name must not already exist.
# # WARNING: This script creates a folder 'tmp'.
# bookdown_path <- paste0('tmp/',basename(tempdir()))
# 
# harmonized_dossier_visualize(
#   harmonized_dossier,
#   bookdown_path = bookdown_path)
# 
# # Open the visual report in a browser.
# bookdown_open(bookdown_path)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Generate one pooled harmonized dataset from a harmonized dossier
# pooled_harmonized_dataset <- pooled_harmonized_dataset_create(
#   harmonized_dossier = harmonized_dossier,
#   harmonized_col_dataset = "adm_study_id")

## ----eval=FALSE----------------------------------------------------------------------------------
# # Extract the data dictionary for one dataset
# data_dictionary_study1 <- data_dict_extract(harmonized_dossier$dataset_study1)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Save the harmonized dossier as an R file to preserve all metadata
# saveRDS(harmonized_dossier, "my_dossier.rds")
# 
# # Export a harmonized dataset to another file format
# library(haven)
# write_sav(harmonized_dossier$dataset_study1, "my_spss_file.sav")
# 
# # Export a summary as an Excel file
# library(fabR)
# write_excel_allsheets(summary_report_harmonized_dossier, "my_summary_report.xlsx")

