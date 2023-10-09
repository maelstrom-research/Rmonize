
# Rmonize (current development version 1.0.0.0000)

# Rmonize 1.0.0

Functions to support rigorous retrospective data harmonization
processing, evaluation, and documentation across datasets in a dossier
based on Maelstrom Research guidelines. The package includes the core
functions to evaluate and format the main inputs that define the
harmonization process, apply specified processing rules to generate
harmonized data, diagnose processing errors, and summarize and evaluate
harmonized outputs.

This is still a work in progress, so please let us know if you used a
function before and is not working any longer.

## Helper functions and objects

- `Rmonize_help()` Call the help center for full documentation
- `dowload_templates()` Call the help center to the download template
  page
- `DEMO_files_harmo` Built-in material allowing the user to test the
  package with demo data

## Assess and manipulate input files

- `as_data_proc_elem()` Validate and coerce any object as a Data
  Processing Elements
- `as_dataschema()`, `as_dataschema_mlstr()` Validate and coerce any
  object as the DataSchema
- `as_harmonized_dossier()` Validate and coerce any object as an
  harmonized dossier
- `dataschema_extract()` Extract and create the DataSchema from a data
  processing elements

## Data processing

- `harmo_process()` Generate harmonized dataset(s) and annotated Data
  Processing Elements. This function internally runs other functions,
  which are :

- `harmo_parse_process_rule()`,
  `harmo_process_add_variable()`,`harmo_process_case_when()`,
  `harmo_process_direct_mapping()`,`harmo_process_id_creation()`,
  `harmo_process_impossible()`,`harmo_process_merge_variable()`,
  `harmo_process_operation()`,`harmo_process_other()`,
  `harmo_process_paste()`,`harmo_process_recode()`,
  `harmo_process_rename()`,`harmo_process_undetermined()`

- `pooled_harmonized_dataset_create()` Generate the pooled dataset from
  harmonized datasets in a dossier

## Evaluation of the harmonization process

- `show_harmo_error()` Generate a summary of the annotated Data
  Processing Elements
- `data_proc_elem_evaluate()`,`dataschema_evaluate()`,
  `harmonized_dossier_evaluate()`,`harmonized_dossier_summarise()`,
  `harmonized_dossier_visualize()` Generate a quality assessment reports
  and summary statistics of inputs and outputs.

## import from madshapR package:

- Shape and prepare input (datasets and data dictionaries) :

`as_data_dict()`,`is_data_dict()`,
`as_data_dict_mlstr()`,`is_data_dict_mlstr()`,
`as_dataset()`,`is_dataset()`, `as_dossier()`,`is_dossier()`,
`as_taxonomy()`

- Extract and manipulate information from input :

`data_extract()`,`data_dict_extract()`,
`data_dict_apply()`,`dataset_zap_data_dict()`,`dossier_create()`
`valueType_adjust()`

- Assess input data :

`dataset_evaluate()`, `data_dict_evaluate()`,`dossier_evaluate()`,
`dataset_summarize()`,`dossier_summarize()`

- Visualize input data :

`bookdown_template()`,`bookdown_render()`,`bookdown_open()`,
`dataset_visualize()`
