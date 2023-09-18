## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = T,
  results = "hide")


## ---- eval=FALSE--------------------------------------------------------------
#  # To install the R package:
#  # install.packages('Rmonize')
#  
#  library(Rmonize)
#  
#  #if you need help with the package, please use:
#  Rmonize_help()
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  names(DEMO_files_harmo)
#  
#  # To see examples:
#  # View(DEMO_files_harmo$dd_TOKYO_format_maelstrom_tagged) # A data dictionary
#  # View(DEMO_files_harmo$dataset_TOKYO)                    # A datasets
#  
#  # View(DEMO_files_harmo$`data_processing_elements - final`)
#                                                      # A Data Processing Elements
#  # View(DEMO_files_harmo$`dataschema - final`)       # A DataSchema
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # Associate metadata from data dictionary to the the data
#  dataset_MELBOURNE_1 <- data_dict_apply(
#    dataset = DEMO_files_harmo$dataset_MELBOURNE_1,
#    data_dict = DEMO_files_harmo$dd_MELBOURNE_1_format_maelstrom)
#  
#  dataset_MELBOURNE_2 <- data_dict_apply(
#    dataset = DEMO_files_harmo$dataset_MELBOURNE_2,
#    data_dict = DEMO_files_harmo$dd_MELBOURNE_2_format_maelstrom)
#  
#  dataset_PARIS <- data_dict_apply(
#    dataset = DEMO_files_harmo$dataset_PARIS,
#    data_dict = DEMO_files_harmo$dd_PARIS_format_maelstrom)
#  
#  dataset_TOKYO <- data_dict_apply(
#    dataset = DEMO_files_harmo$dataset_TOKYO,
#    data_dict = DEMO_files_harmo$dd_TOKYO_format_maelstrom)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # group the datasets in a dossier object
#  # NB: the name of the datasets in the dossier must match the column
#  # input_dataset in the Data Processing Elements
#  
#  dossier <- dossier_create(
#    dataset_list = list(
#      dataset_MELBOURNE_1,
#      dataset_MELBOURNE_2,
#      dataset_PARIS,
#      dataset_TOKYO))
#  
#  dataschema <- DEMO_files_harmo$`dataschema - final`
#  data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final`
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # Process harmonization
#  harmonized_dossier <- harmo_process(dossier, dataschema, data_proc_elem)
#  show_harmo_error(harmonized_dossier)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # Assess the harmonization (this report can be downloaded as an excel file)
#  harmonized_dossier_evaluate <- harmonized_dossier_evaluate(harmonized_dossier)
#  # View(harmonized_dossier_evaluate)
#  
#  # Produce summary statistics (this summary can be downloaded as an excel file)
#  harmonized_dossier_summary <- harmonized_dossier_summarise(harmonized_dossier)
#  # View(harmonized_dossier_summary)
#  
#  # Generate visual report (a web-based interactive application)
#  bookdown_path <- paste0('tmp/',basename(tempdir()))
#  # print(bookdown_path)
#  harmonized_dossier_visualize(
#    harmonized_dossier,
#    bookdown_path = bookdown_path,
#    overwrite = TRUE)
#  
#  # bookdown_open(bookdown_path) # open the report in browser
#  # file.remove(bookdown_path)
#  

