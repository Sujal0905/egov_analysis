# E-Governance Data Analysis: UN E-Government Development Index (EGDI) 2024

Week 1 task: data acquisition and preliminary analysis in R.

**Dataset:** UN DESA E-Government Development Index 2024 (193 UN Member States)
Source: https://publicadministration.un.org/egovkb/Data-Center

## Contents
| Path | Description |
|---|---|
| `egov_analysis.R` | Full R script: import, cleaning, summary statistics, plots, Wilcoxon test |
| `data/egdi_2024_raw.csv` | Input data in the layout of the UN download |
| `data/egdi_2024_clean.csv` | Cleaned data written by the script |
| `figures/` | Five PNG charts produced by the script |
| `EGovernance_Week1_Report.docx` | Report with explanations and embedded code |

## How to run
```r
install.packages(c("readr", "dplyr", "ggplot2", "stringr"))
source("egov_analysis.R")   # run from the project folder
```

## What the script does
1. Imports the CSV and inspects structure
2. Cleans names, recodes the MSQ flag, sets ordered factors, validates ranges and duplicates
3. Computes summary statistics overall, by EGDI group and by MSQ status; locates India
4. Creates five ggplot2 visualizations
5. Runs a Wilcoxon rank-sum test and saves the cleaned dataset
