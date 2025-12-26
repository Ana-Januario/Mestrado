# Mestrado

Data analysis and modeling project developed as part of my Master's degree in Statistical Modelling and Data Analysis using R.

## 📋 Project Overview

This repository contains all the work, analyses, and models developed during my Master's degree program. The project focuses on statistical modeling and data analysis using R programming language.

## 📁 Repository Structure

```
Mestrado/
│
├── data/               # Data files
│   ├── raw/           # Original, immutable data
│   └── processed/     # Cleaned and processed data
│
├── scripts/           # R analysis scripts
│   └── example_analysis.R
│
├── reports/           # Analysis reports and documentation
│   └── report_template.Rmd
│
├── figures/           # Generated plots and visualizations
│
├── models/            # Saved statistical models
│
├── .gitignore        # Git ignore file for R projects
├── Mestrado.Rproj    # RStudio project file
└── README.md         # This file
```

## 🚀 Getting Started

### Prerequisites

- R (version 4.0 or higher recommended)
- RStudio (recommended IDE)
- Required R packages:
  - `tidyverse`
  - `ggplot2`
  - `dplyr`
  - `readr`
  - `knitr`
  - `rmarkdown`

### Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/Ana-Januario/Mestrado.git
   ```

2. Open the project in RStudio by double-clicking `Mestrado.Rproj`

3. Install required packages:
   ```r
   install.packages(c("tidyverse", "ggplot2", "dplyr", "readr", "knitr", "rmarkdown"))
   ```

## 📊 Usage

### Running Analysis Scripts

1. Navigate to the `scripts/` directory
2. Open the desired R script
3. Run the script in RStudio or from the command line:
   ```r
   source("scripts/example_analysis.R")
   ```

### Generating Reports

1. Open an R Markdown file from the `reports/` directory
2. Click "Knit" in RStudio or use:
   ```r
   rmarkdown::render("reports/report_template.Rmd")
   ```

## 📝 Workflow

1. **Data Import**: Place raw data in `data/raw/`
2. **Data Cleaning**: Process data and save to `data/processed/`
3. **Analysis**: Create analysis scripts in `scripts/`
4. **Visualization**: Generate figures and save to `figures/`
5. **Modeling**: Build models and save to `models/`
6. **Reporting**: Document findings in `reports/`

## 🤝 Contributing

This is a personal academic project. However, suggestions and feedback are welcome!

## 📄 License

This project is part of academic work. Please contact the author for usage permissions.

## 👤 Author

Ana Januário

## 📧 Contact

For questions or collaborations, please open an issue in this repository.

---

*Last updated: 2025*
