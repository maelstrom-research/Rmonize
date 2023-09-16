# ## code to prepare `valueType_list` dataset goes here
library(Rmonize)
library(madshapR)
library(fabR)
library(tidyverse)
DEMO_files_harmo <-
  file_index_create('../DEMO/DEMO_V1_Rmonize/') %>%
  file_index_read()
# 
# 
# simple harmo
dataset_MELBOURNE_1 <- DEMO_files$dataset_MELBOURNE_1[1]
dossier <- dossier_create(list(dataset_MELBOURNE_1))
# Associate metadata from data dictionary to the the data
# dataset_MELBOURNE_1 <- data_dict_apply(
#   dataset = DEMO_files$dataset_MELBOURNE_1,
#   data_dict = DEMO_files$dd_MELBOURNE_1_format_maelstrom)
#
# dataset_MELBOURNE_2 <- data_dict_apply(
#   dataset = DEMO_files$dataset_MELBOURNE_2,
#   data_dict = DEMO_files$dd_MELBOURNE_2_format_maelstrom)
#
# dataset_PARIS <- data_dict_apply(
#   dataset = DEMO_files$dataset_PARIS,
#   data_dict = DEMO_files$dd_PARIS_format_maelstrom)
#
# dataset_TOKYO <- data_dict_apply(
#   dataset = DEMO_files$dataset_TOKYO,
#   data_dict = DEMO_files$dd_TOKYO_format_maelstrom_tagged)
#
# dossier <- dossier_create(dataset_list = list(dataset_MELBOURNE_1,dataset_MELBOURNE_2,dataset_PARIS,dataset_TOKYO))

dataschema <- 
  DEMO_files_harmo$`dataschema - final` %>%
  data_dict_filter('name == "adm_unique_id"')

data_proc_elem <- DEMO_files_harmo$`data_processing_elements - final` %>%
  dplyr::filter(dataschema_variable == 'adm_unique_id',
                input_dataset == 'dataset_MELBOURNE_1')

# perform harmonization
harmonized_dossier <-
 harmo_process(dossier,dataschema,data_proc_elem)

pooled_harmonized_dataset <-
  pooled_harmonized_dataset_create(harmonized_dossier)

summary_var_harmo <-
  dataset_summarize(
  dataset = pooled_harmonized_dataset,
  valueType_guess = TRUE)

DEMO_files_harmo$pooled_harmonized_dataset <- pooled_harmonized_dataset
DEMO_files_harmo$harmonized_dossier        <- harmonized_dossier
DEMO_files_harmo$summary_var_harmo         <- summary_var_harmo

DEMO_files_harmo$dd_MELBOURNE_1_format_maelstrom         <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
DEMO_files_harmo$dd_MELBOURNE_2_format_maelstrom         <- DEMO_files$dd_MELBOURNE_2_format_maelstrom
DEMO_files_harmo$dd_PARIS_format_maelstrom               <- DEMO_files$dd_PARIS_format_maelstrom
DEMO_files_harmo$dd_TOKYO_format_maelstrom               <- DEMO_files$dd_TOKYO_format_maelstrom

DEMO_files_harmo$dataset_MELBOURNE_1         <- DEMO_files$dataset_MELBOURNE_1
DEMO_files_harmo$dataset_MELBOURNE_2         <- DEMO_files$dataset_MELBOURNE_2
DEMO_files_harmo$dataset_PARIS               <- DEMO_files$dataset_PARIS
DEMO_files_harmo$dataset_TOKYO               <- DEMO_files$dataset_TOKYO

usethis::use_data(DEMO_files_harmo, overwrite = TRUE)



