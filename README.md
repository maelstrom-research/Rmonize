
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rmonize

<!-- badges: start -->

[![R-CMD-check](https://github.com/maelstrom-research/Rmonize/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maelstrom-research/Rmonize/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Background

Combining and co-analyzing data in a dossier offers potential advantages
for addressing research questions, but data items collected in a dossier
must first be made suitably equivalent, i.e., harmonized. This process
of data harmonization is essential but challenging to implement in a
rigorous and transparent way. To help address these challenges,
<a href="https://www.maelstrom-research.org/" target="_blank"> Maelstrom
Research</a> developed guidelines for rigorous
<a href="https://maelstrom-research.org/page/maelstrom-guidelines" target="_blank">
retrospective data harmonization</a>. An overview of the iterative steps
of this process is shown in Figure 1.

<figure id="id">
<img
src="https://www.maelstrom-research.org/assets/images/HarmoStepsNew.png"
class="class" style="width:50.0%;height:50.0%"
alt="Figure 1. Iterative harmonization steps." />
<figcaption aria-hidden="true">Figure 1. Iterative harmonization
steps.</figcaption>
</figure>

# Rmonize

The **Rmonize** R package addresses practical aspects of data
harmonization processing (guidelines Step 3) and facilitates evaluation
(Step 4) and documentation (Step 5) of harmonization products. It was
developed to meet the needs of Maelstrom’s collaborative harmonization
initiatives, focusing on harmonization of individual participant data
from population-based cohort studies, but is intended as a general
resource for diverse harmonization efforts. **Rmonize** is compatible
with the Maelstrom OBiBa software suite and other commonly used data
management and statistical software.

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

[Understand the workflow](articles/b-Understand-the-workflow.html)

[How to fill the Data Processing
Elements](articles/c-The-Data-processing-elements.html)

[Example with DEMO files](articles/d-Example-with-DEMO-files.html)
