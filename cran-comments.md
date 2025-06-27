## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 note ✖

# Submission for CRAN : Rmonize 2.0.0

> side note 1: dear people of CRAN, please note that fabR, madshapR,
Rmonize, mlstrOpalr, and BanffIT belong to a suite that is going to be 
updated. fabR (2.1.1) has already been updated. 
madshapR (2.0.0) has already been updated.
madshapR, Rmonize, mlstrOpalr and BanffIT will not be retrocompatible, 
as we discussed with all our community, update all the codes and tested 
for 3 months, along with git monitoring and .onAttach() message. 
Ultimately we are thrilled to see the next release published in CRAN. 
Best Regards, Guillaume FABRE.

> side note 2: dear people of CRAN, please note that some examples of the 
madshapR package take some time to be processed. The examples generate 
complexe summaries and visual reports that cannot be shorten. 
These functions have been put in \donttest{ (these are the same in madshapR package)
Best Regards, Guillaume FABRE.

## Superseded object.

| previous version (1.1.0 and older) | version 2.0.0          |
|------------------------------------|------------------------|
| Rmonize_DEMO                       | Rmonize_examples       |

## Superseded parameters.

In functions show_harmo_error(), harmonized_dossier_evaluate(),
harmonized_dossier_summarize() and harmonized_dossier_visualize(),
The parameters have been simplified into one and only "dossier"
https://github.com/maelstrom-research/Rmonize/issues/110
https://github.com/maelstrom-research/Rmonize/issues/109
https://github.com/maelstrom-research/Rmonize/issues/108
https://github.com/maelstrom-research/Rmonize/issues/98
https://github.com/maelstrom-research/Rmonize/issues/93
https://github.com/maelstrom-research/Rmonize/issues/92


## Superseded function behaviors and/or output structures.

In `harmonized_dossier_evaluate()` and `harmonized_dossier_summarize()`,
the columns generated in the outputs have been renamed.

## Bug fixes and improvements

## Enhancements in the assessment, the summary and the visual reports!

* The assessment and summary reports had some updates, such as renamed columns 
and bug corrections.
https://github.com/maelstrom-research/Rmonize/issues/104
https://github.com/maelstrom-research/Rmonize/issues/103
https://github.com/maelstrom-research/Rmonize/issues/89
https://github.com/maelstrom-research/Rmonize/issues/88
https://github.com/maelstrom-research/Rmonize/issues/87
https://github.com/maelstrom-research/Rmonize/issues/86
https://github.com/maelstrom-research/Rmonize/issues/85
https://github.com/maelstrom-research/Rmonize/issues/84
https://github.com/maelstrom-research/Rmonize/issues/68
https://github.com/maelstrom-research/Rmonize/issues/21

* The visual reports have been improved, including better visual outputs and
color palettes, and new features such as total number of rows next to the bar charts.

https://github.com/maelstrom-research/Rmonize/issues/57
https://github.com/maelstrom-research/Rmonize/issues/53
https://github.com/maelstrom-research/Rmonize/issues/49
https://github.com/maelstrom-research/Rmonize/issues/48
https://github.com/maelstrom-research/Rmonize/issues/39
https://github.com/maelstrom-research/Rmonize/issues/37
https://github.com/maelstrom-research/Rmonize/issues/33
https://github.com/maelstrom-research/Rmonize/issues/32
https://github.com/maelstrom-research/Rmonize/issues/29

