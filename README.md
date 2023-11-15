
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rmonize

<!-- badges: start -->

[![R-CMD-check](https://github.com/maelstrom-research/Rmonize/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maelstrom-research/Rmonize/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Overview

Harmonizing data (evaluating and processing, where relevant, data items
from different studies or datasets under a common format) is essential
to <b style="color:red">epidemiology research</b> but can be
methodologically and technically challenging. Rmonize was developed by
<a href="https://www.maelstrom-research.org/" target="_blank"> Maelstrom
Research</a> to address some of the key challenges faced and support a
streamlined, reusable, and well documented harmonization pipeline. The
current documentation provides a starting point to use the package.

# Pipeline

<b style="color:red">(figure to be discussed)</b>

<img src="man/figures/fig_readme.png" 
style="width: 50%; margin: 0 auto; display: flex; justify-content: center;">

<br>

To support organized and well documented harmonization, Rmonize includes
functions to prepare and validate the required input datasets and
metadata, generate harmonized datasets and metadata, evaluate data
processing, and produce reports to help assess harmonized data content
and quality.  
Data processing in Rmonize depends on three external user-provided
elements: the input datasets, DataSchema (list of core variables to
generate across datasets), and Data Processing Elements. The DataSchema
and Data Processing Elements are prepared in spreadsheets that can be
easily modified and shared outside of R. Templates can be downloaded
here. Rmonize uses two underlying packages,
<a href="https://cran.r-project.org/web/packages/madshapR/index.html" 
target="_blank">madshapR</a> and
<a href="https://cran.r-project.org/web/packages/fabR/index.html" 
target="_blank">fabR</a>, which include many functions to work with data
and metadata. Functions required by Rmonize are automatically loaded and
accessible to the user without loading madshapR and fabR, but these
packages include additional useful functions for data processing.

# Get started

## Install the package

``` r
# To install the R package:
install.packages('Rmonize')

library(Rmonize)
# If you need help with the package, please use:
Rmonize_help()
```

## Annexes

[Glossary and templates](articles/a-Glossary-and-templates.html)

[How to fill the Data Processing
Elements](articles/b-The-Data-processing-elements.html)

[Example with DEMO files](articles/c-Example-with-DEMO-files.html)
