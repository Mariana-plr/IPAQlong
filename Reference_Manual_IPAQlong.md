---
title: "Package 'IPAQlong'"
date: "March 23, 2022"
output: 
  html_document:
    toc: true 
    toc_depth: 3  
    toc_float: true
    
---
#

**Package:** IPAQlong  
**Type:** Package  
**Title:** Calculates the Scores for the ‘International Physical Activity   Questionnaire (IPAQ)’ Long Form  
**Version:** 0.1.0  
**Author:** Mariana Ponce-de-Leon  
**Maintainer:** Mariana Ponce-de-Leon <mariana.ponce-de-leon@outlook.com>  
**Description:** Calculates the scores for the 'International Physical Activity Questionnaire (IPAQ)' long form, based on the "Guidelines for the data processing and analysis of the IPAQ" <https://sites.google.com/site/theipaq/home>.   
**License:** MIT + file LICENSE  
**Encoding:** UTF-8  
**LazyData:** true  
**RoxygenNote:** 7.1.2  
**Suggests:**   
    *knitr*,  
    *rmarkdown*  
**Imports:** 
    *dplyr*



# `ipaq_scores`


## Description

ipaq_scores() calculates the continuous and categorical scores for the 'International Physical Activity Questionnaire (IPAQ)'
 long form.


## Usage

```r
ipaq_scores(data, truncate = F)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     A data frame object containing 25 columns with the replies to the IPAQ long format (parts 1-4). Yes/no replies should be coded as yes-1, no-0. Time should be in minutes. 
`truncate`     |     Logical vector. If TRUE all walking, moderate and vigorous time variables are truncated following the IPAQ short rule. Variables exceeding 180 minutes are truncated to be equal to 180 minutes. Default FALSE.


## Value

A data frame object with the continuous (metabolic equivalent of task minutes (MET-min)/week) and categorical scores (low, moderate, high). Returns NA for cases with missing values.


## References

The IPAQ Group (2005). Guidelines for Data Processing and Analysis of the International Physical Activity Questionnaire. Retrieved from <https://sites.google.com/site/theipaq/home>


# `ipaq_subScores`


## Description

ipaq_subscores() calculates the domain and intensity sub scores for the 'International Physical Activity Questionnaire (IPAQ)'
 long form.


## Usage

```r
ipaq_subScores(data, truncate = F)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     A data frame object containing 25 columns with the replies to the IPAQ long format (parts 1-4). Yes/no replies should be coded as yes-1, no-0. Time should be in minutes.
`truncate`     |     Logical vector. If TRUE all walking, moderate and vigorous time variables are truncated following the IPAQ short rule. Variables exceeding 180 minutes are truncated to be equal to 180 minutes.Default FALSE.


## Value

A data frame object with the domain (work, transportation, domestic, leisure) and intensity (walking, moderate, vigorous) sub scores in metabolic equivalent of task minutes (MET-min)/week. Sub scores are calculated for all cases, even in the presence of missing values.


## References

The IPAQ Group (2005). Guidelines for Data Processing and Analysis of the International Physical Activity Questionnaire. Retrieved from <https://sites.google.com/site/theipaq/home>


