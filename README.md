# Master's Data Analysis Project

This repository contains the data analysis developed as part of my Master's degree in Statistical Modelling and Data Analysis.

## Overview

The goal of this work is to explore and model production data through a combination of descriptive, inferential, and predictive techniques.  
Analyses include descriptive statistics, correlation assessment, normality testing, multivariate modelling of production cost and profit, and investigation of the main factors influencing the certification (“DOP” label) of animals.  
All analyses were performed using **R**.

## Files

- **Analisedescritivadissert.R** – Script performing the descriptive analysis of the dataset (summary statistics, data visualization, and initial exploration).
- **correlacao.R** – Correlation analysis between key production and performance variables.
- **normalidade.R** and **kruskal.R** – Tests of data normality and non-parametric comparisons using the Kruskal–Wallis test.
- **modelcaocustototaldia.R** and **modelopesomatintervalo.R** – Multivariate modelling of total production cost and weight variation across time intervals.
- **outlier.R** – Outlier detection and evaluation of model robustness.
- **modelacaorlm.R** – Linear modelling of net profit.
- **GLM and logistic models (additional scripts)** – Implemented to identify characteristics associated with animals earning the “DOP” certification label.  
  Further details and parallel analyses can be found in the published dissertation.

## Requirements

- R (≥ 4.0)
- Packages: `tidyverse`, `ggplot2`, `dplyr`, `readr`, `car`, `MASS`, and `glmnet`

## Author

Ana Januário  
University of Évora  
[Dissertação 2021](https://www.rdpc.uevora.pt/handle/10174/29316)

