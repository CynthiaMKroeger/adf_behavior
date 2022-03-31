Description of ADF Behavior Repository Contents
================
Cynthia M Kroeger
31 March 2022

This repository contains supporting materials for the manuscript that
describes our work evaluating the Effects of Alternate Day Fasting
versus Daily Calorie Restriction on Dietary Restraint, Self-efficacy,
and Appetite.

## Description of Repository Contents

The types of supporting materials in this repository include:

-   .R files with code for multiple imputation methods, statistical
    analyses, and figure generation
-   .csv files of all SAS results
-   .pdf files of surveys used
-   .txt file with licensing information

## .R files

**File names for, descriptions of, and authors of all R code used for
this project:**

imputation.R

-   This file contains the code used to impute missing data.
-   Authors: Hiroki Naganobori, Cynthia M Kroeger

clean\_imputed\_data.R

-   This file contains the code used to clean the imputed dataset.
-   Author: Cynthia M Kroeger

LMEModelSummary.R ImpLMEModelSummary.R

-   These files contain the code used to define the imputation synthesis
    model terms for the longitudinal study design.
-   Author: Hiroki Naganobori

imputation\_model.R

-   This file contains the code used to run statistical models on the
    full imputed dataset.
-   Authors: Hiroki Naganobori, Cynthia M Kroeger

baseline\_statistics.R

-   This file contains the code used to run baseline analyses that are
    presented in Table 1 of the manuscript.
-   Authors: Chen Lyu, Cynthia M Kroeger

figures.R

-   This file contains the code used to generate figures. Outputs used
    to generate figures were obtained from statistical analyses
    completed in SAS, with methods described in manuscript.
-   Authors: Chen Lyu, Cynthia M Kroeger

## A Note on How to Replicate this Study

Because the data are clinical, they are not publicly available. A formal
agreement with university ethics would need to be made to obtain the
data. Further, longitudinal analyses were conducted using SAS, with the
methods used described in the manuscript. All results from SAS are
included in this repository to improve transparency and assist with any
potential meta-analyses including this study.

The main reasons for sharing .R code are to improve methods transparency
and provide a potential resource for the field. When originally
conducting the multiple imputation for this study, there were no clear
methods for how to code the integration of analyses from all imputed
data sets, for this kind of longitudinal design. Cynthia M Kroeger and
Hiroki Naganobori scoured many written texts to work out the contents of
ImpLMEModelSummary.R. Perhaps others may find the code useful to think
about, replicate, and/or edit to accommodate their own analysis needs,
if better resources are not already available.

## License Information

These materials are licensed under the Creative Commons Attribution
Share Alike 4.0.

**File name for license:** LICENSE.txt

## Recommendations for Citation of Supporting Materials

Please use the following to cite any of the supporting materials herein:

Kroeger CM. Research materials and analysis code for manuscript: Alternate day fasting
versus calorie restriction for health behavior: Randomized clinical trial. 2022. DOI: 10.5281/zenodo.6400587. [![DOI](https://zenodo.org/badge/476200394.svg)](https://zenodo.org/badge/latestdoi/476200394)

## Contact Information for Corresponding Author

Cynthia M. Kroeger, PhD, Postdoctoral Fellow, Charles Perkins Centre,
Central Clinical School, Faculty of Medicine and Health, The University
of Sydney. <cynthia.kroeger@sydney.edu.au>
