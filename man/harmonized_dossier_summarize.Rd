% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-harmonized_data_summarize.R
\name{harmonized_dossier_summarize}
\alias{harmonized_dossier_summarize}
\title{Generate an assessment report and summary of a harmonized dossier}
\usage{
harmonized_dossier_summarize(harmonized_dossier)
}
\arguments{
\item{harmonized_dossier}{A list containing the harmonized dataset(s).}
}
\value{
A list of data frames containing overall assessment reports and summaries
grouped by harmonized dataset.
}
\description{
Assesses and summarizes the content and structure of a harmonized dossier and
generates reports of the results. This function can be used to evaluate data
structure, presence of specific fields, coherence across elements, and
data dictionary formats, and to summarize additional information about
variable distributions and descriptive statistics.
}
\details{
A harmonized dossier is a named list containing one or more data frames,
which are harmonized datasets. A harmonized dossier is generally the
product of applying processing to a dossier object. The name of each
harmonized dataset (data frame) is taken from the reference input dataset.
A harmonized dossier also contains the DataSchema and
Data Processing Elements used in processing as attributes.

A DataSchema is the list of core variables to generate across datasets and
related metadata. A DataSchema object is a list of data frames with elements
named 'Variables' (required) and 'Categories' (if any). The 'Variables'
element must contain at least the \code{name} column, and the 'Categories'
element must contain at least the \code{variable} and \code{name} columns to be usable
in any function. In 'Variables' the \code{name} column must also have unique
entries, and in 'Categories' the combination of \code{variable} and \code{name} columns
must also be unique.

The Data Processing Elements specifies the input elements and processing algorithms
to generate harmonized variables in the DataSchema formats. It is also
contains metadata used to generate documentation of the processing.
A Data Processing Elements object is a data frame with specific columns
used in data processing: \code{dataschema_variable}, \code{input_dataset},
\code{input_variables}, \code{Mlstr_harmo::rule_category} and \code{Mlstr_harmo::algorithm}.
To initiate processing, the first entry must be the creation of a harmonized
primary identifier variable (e.g., participant unique ID).

A taxonomy is a classification schema that can be defined for variable
attributes. A taxonomy is usually extracted from an
\href{https://www.obiba.org/pages/products/opal//}{Opal environment}, and a
taxonomy object is a data frame that must contain at least the columns
\code{taxonomy}, \code{vocabulary}, and \code{terms}. Additional details about Opal
taxonomies are
\href{https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html}{available online}.

The valueType is a declared property of a variable that is required in
certain functions to determine handling of the variables. Specifically,
valueType refers to the
\href{https://opaldoc.obiba.org/en/dev/variables-data.html#value-types}{OBiBa data type of a variable}.
The valueType is specified in a data dictionary in a column 'valueType' and
can be associated with variables as attributes. Acceptable valueTypes
include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full
list of OBiBa valueType possibilities and their correspondence with R data
types are available using \code{valueType_list}. The valueType can be used to
coerce the variable to the corresponding data type.
}
\examples{
\donttest{
# Use Rmonize_examples to run examples.
library(dplyr)

# Perform data processing
harmonized_dossier <- Rmonize_examples$`harmonized_dossier`
summary_harmo <- harmonized_dossier_summarize(harmonized_dossier)

glimpse(summary_harmo)
}

}
