## ----include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Load relevant packages
# library(Rmonize)
# library(madshapR) # Additional functions to work with data dictionaries
# library(fabR) # Additional functions to help with importing data
# 
# library(tidyverse) # Collection of R packages for data science

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get original datasets that were read from different file formats
# 
# # Datasets from .sav files that contain variable metadata
# original_dataset_study1 <- Rmonize_examples$original_dataset_study1
# original_dataset_study2 <- Rmonize_examples$original_dataset_study2
# 
# # Dataset from .csv file, with no data dictionary
# original_dataset_study3 <- Rmonize_examples$original_dataset_study3
# 
# # Datasets from .xlsx files, with data dictionaries in separate files
# original_dataset_study4 <- Rmonize_examples$original_dataset_study4
# original_dataset_study5 <- Rmonize_examples$original_dataset_study5

## ----eval=FALSE----------------------------------------------------------------------------------
# # Existing metadata (e.g., from datasets from .sav files) can be extracted as data dictionaries
# original_dd_study1 <- data_dict_extract(original_dataset_study1)
# original_dd_study2 <- data_dict_extract(original_dataset_study2)
# 
# # If data dictionaries are provided as separate files, assign them separately
# original_dd_study4 <- Rmonize_examples$original_data_dictionary_study4
# original_dd_study5 <- Rmonize_examples$original_data_dictionary_study5
# 
# # A minimal data dictionary can be extracted from any dataset, but it provides very limited information
# extracted_dd_study3 <- data_dict_extract(original_dataset_study3)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Confirm that the dataset is a tibble
# is.data.frame(original_dataset_study4) # TRUE
# 
# # Evaluate dataset only
# dataset_evaluate(original_dataset_study4)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Make the data dictionary a list containing a data frame named 'Variables'
# modified_dd_study4 <- list(
#   "Variables" = original_dd_study4)
# 
# # Rename columns using standardized names
# modified_dd_study4$Variables <- modified_dd_study4$Variables %>%
#   rename(name = Variable,
#          label = Label,
#          valueType = `Data type`)
# 
# # Evaluate data dictionary only
# data_dict_evaluate(modified_dd_study4)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Get the accepted 'valueType' values and their R equivalents
# madshapR::valueType_list
# 
# # Recode variable valueTypes
# compatible_dd_study4 <- modified_dd_study4
# 
# compatible_dd_study4$Variables <- compatible_dd_study4$Variables %>%
#   mutate(valueType = case_match(
#     valueType,
#     "character" ~ "text",
#     "numeric" ~ "decimal",
#     "integer" ~ "integer"
#   ))
# 
# # Use function data_dict_expand() to create `Categories`
# ?madshapR::data_dict_expand() # See the function documentation
# 
# # Rename column `Category codes` with a standardized name for easy processing
# compatible_dd_study4$Variables <- compatible_dd_study4$Variables %>%
#   rename(`Categories::label` = `Category codes`)
# 
# # Create 'Categories' data frame
# compatible_dd_study4 <- data_dict_expand(
#   data_dict = compatible_dd_study4,
# ) %>%
#   as_data_dict_mlstr() # Ensure correct formatting
# 
# # Correctly code categories that indicate types of missing values
# compatible_dd_study4$Categories <- compatible_dd_study4$Categories %>%
#   mutate(
#     missing = ifelse(
#       label %in% c("Don't know", "Prefer not to answer"), TRUE, FALSE)
#   )
# 
# # Rerun the data dictionary evaluation to confirm the corrections were made
# data_dict_evaluate(compatible_dd_study4)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Format the dataset
# compatible_dataset_study4 <- as_dataset(original_dataset_study4, col_id = "ID")
# 
# # Evaluate the dataset and data dictionary together, as separate objects
# # Tip: Assign the output to an object for easier viewing.
# dataset_evaluation_study4 <- dataset_evaluate(
#   dataset = compatible_dataset_study4,
#   data_dict = compatible_dd_study4)
# 
# # View the new informational messages in the RStudio viewer
# View(dataset_evaluation_study4[["Dataset assessment"]])
# 
# # Note: the informational messages about duplicated rows are a side effect
# # of a synthesized dataset with few variables. Rows are more likely to have
# # the same values.

## ----eval=FALSE----------------------------------------------------------------------------------
# # Correct a variable name in the data dictionary to match the dataset
# formatted_dd_study4 <- compatible_dd_study4
# formatted_dd_study4$Variables <- formatted_dd_study4$Variables %>%
#   mutate(name = ifelse(name == "marital", "marital_v1", name))
# formatted_dd_study4$Categories <- formatted_dd_study4$Categories %>%
#   mutate(variable = ifelse(variable == "marital", "marital_v1", variable))
# 
# # Adjust two variable valueTypes in the dataset to match the data dictionary
# formatted_dataset_study4 <- compatible_dataset_study4 %>%
#   mutate(
#     # First convert "NA" strings to NA empty strings
#     across(c(drink_four_preg_v1, drink_four_preg_v3), ~ na_if(.x, "NA")),
#     # Convert the variables to integer
#     across(c(drink_four_preg_v1, drink_four_preg_v3), as.integer))

## ----eval=FALSE----------------------------------------------------------------------------------
# # Associate a dataset with its data dictionary (only possible when there are no
# # errors in the previous evaluation)
# dataset_with_dd_study4 <- data_dict_apply(
#   dataset = formatted_dataset_study4,
#   data_dict = formatted_dd_study4)
# 
# # If you want, evaluate the dataset and data dictionary together
# # (confirms that there are no errors, but otherwise provides same information)
# dataset_evaluate(dataset_with_dd_study4)

## ----eval=FALSE----------------------------------------------------------------------------------
# # Summarize a dataset with an associated data dictionary
# summary_dataset_with_dd_study4 <- dataset_summarize(
#   dataset = dataset_with_dd_study4)
# 
# # View the summary outputs
# View(summary_dataset_with_dd_study4)
# summary_dataset_with_dd_study4$`Numerical variable summary` %>% View()
# 
# # Export summaries as Excel file
# fabR::write_excel_allsheets(summary_dataset_with_dd_study4, "summary_dataset_with_dd_study4.xlsx")

## ----eval=FALSE----------------------------------------------------------------------------------
# # Produce a visual report of the dataset and variables
# # You must specify a folder to contain the visual report files, and the folder name must not already exist.
# # WARNING: This script creates a folder 'tmp'.
# bookdown_path <- paste0('tmp/',basename(tempdir()))
# 
# dataset_visualize(
#   dataset = dataset_with_dd_study4,
#   bookdown_path = bookdown_path)
# 
# # Open the visual report in a browser.
# bookdown_open(bookdown_path)
# # Or open 'bookdown_path/docs/index.html'.

## ----eval=FALSE----------------------------------------------------------------------------------
# # Prepare a version of the cleaned validated input dataset (with associated data dictionary)
# input_dd_study4 <- formatted_dd_study4
# input_dataset_study4 <- formatted_dataset_study4 %>%
#   # Erroneous values noted in summary reports can be removed
#   mutate(age_v1 = ifelse(age_v1>100, NA, age_v1)) %>%
#   # If desired, specify the column that provides unique IDs
#   as_dataset(col_id = "ID") %>%
#   # Associate the data dictionary
#   data_dict_apply(data_dict = input_dd_study4)
# 
# # Prepare a version of the cleaned validated input dataset as an R file
# saveRDS(input_dataset_study4, "input_dataset_study4.rds")

