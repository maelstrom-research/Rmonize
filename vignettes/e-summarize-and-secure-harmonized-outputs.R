## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # Load relevant packages
# library(Rmonize)
# library(fabR) # Additional functions to help with importing data
# library(tidyverse) # Collection of R packages for data science

## ----eval=FALSE---------------------------------------------------------------
# # Load harmonized dossier
# harmonized_dossier <- Rmonize_examples$harmonized_dossier
# 
# # See information about the harmonized dossier in the RStudio viewer
# View(harmonized_dossier)
# 
# # See additional attributes associated with the harmonized dossier
# summary(attributes(harmonized_dossier))

## ----fig.cap="Information about the example harmonized dossier provided in the RStudio viewer.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig5_fig01.png")

## ----fig.cap="Additional Rmonize attributes are also associated with the harmonized dossier.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig5_fig02.png")

## ----eval=FALSE---------------------------------------------------------------
# # Produce dataset and variable summaries
# summary_report_harmonized_dossier <- harmonized_dossier_summarize(harmonized_dossier)
# # Note: This report is also provided in Rmonize_examples$summary_report_harmonized_dossier.
# 
# # Export to Excel
# write_excel_allsheets(summary_report_harmonized_dossier, "summary_report_harmonized_dossier.xlsx")

## ----fig.cap="Information about the example harmonized dossier summary report provided in the RStudio viewer.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig5_fig03.png")

## ----eval=FALSE---------------------------------------------------------------
# # Produce a visual report of the harmonized datasets and variables
# # You must specify a folder to contain the visual report files, and the folder name must not already exist.
# # WARNING: This script creates a folder 'temp'.
# bookdown_path <- paste0('temp/',basename(tempdir()))
# 
# harmonized_dossier_visualize(
#   harmonized_dossier,
#   bookdown_path = bookdown_path)
# 
# # Open the visual report in a browser.
# bookdown_open(bookdown_path)

## ----fig.cap="a) the Overview page", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig5_fig04a.png")

## ----fig.cap="b) summary figure for the variable lsb_alc_binge_m_preg.", out.width="80%", fig.align="center",echo=FALSE----
knitr::include_graphics("images/vig5_fig04b.png")

## ----eval=FALSE---------------------------------------------------------------
# # Extract the data dictionary
# data_dict_harmonized_dossier <- harmonized_dossier %>%
#   lapply(data_dict_extract)

## ----eval=FALSE---------------------------------------------------------------
# # Generate one pooled harmonized dataset from a harmonized dossier
# pooled_harmonized_dataset <- pooled_harmonized_dataset_create(
#   harmonized_dossier = harmonized_dossier,
#   harmonized_col_dataset = "adm_study_id")

## ----eval=FALSE---------------------------------------------------------------
# # Export harmonized datasets without metadata to Excel
# for(i in names(harmonized_dossier)){
#   filename <- paste0(
#     "outputs/harmonized_datasets/harmonized_",i,".xlsx")
#   write_excel_allsheets(harmonized_dossier[[i]],filename)}
# 
# # Export harmonized data dictionaries to Excel
# for(i in names(data_dict_harmonized_dossier)){
#   filename <- paste0(
#     "outputs/harmonized_data_dictionaries/harmonized_data_dictionary_",i,".xlsx")
#   write_excel_allsheets(data_dict_harmonized_dossier[[i]],filename)}

