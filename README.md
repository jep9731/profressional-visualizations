# 📊 Professional Visualizations: Imaging Projects

This repository contains professional data visualizations and analytics related to clinical imaging and diagnostic data from the ADC Imaging Core and related projects. The scripts and notebooks focus on MRI, PET, longitudinal tracking, and amyloid harmonization.

---

## 📘 Project Summaries

### 🧠 `ADC_IC_MRIQC_Y3.ipynb`
Visualizes MRIQC (MRI Quality Control) group-level metrics for Year 3 imaging data.

**Data Files Required:**
- `group_T1w.tsv`
- `group_bold.tsv`

**Tools Used:** `pandas`, `seaborn`, `matplotlib`

**Run:**
```bash
pip install pandas seaborn matplotlib
jupyter notebook ADC_IC_MRIQC_Y3.ipynb
```

### 🧠 `ADC_IC_PET_Reads.ipynb`
Summarizes PET scan reads using a master clinical diagnosis sheet.

**Date File Required:**
- `imaging_diagnosis_mastersheet_2024-03-05.xlsx`

**Tools Used:** `pandas`, `seaborn`, `openpyxl`

**Run:**
```bash
pip install pandas seaborn matplotlib openpyxl
jupyter notebook ADC_IC_PET_Reads.ipynb
```

### 📈 `Longitudinal_scans.R`
Tracks scan frequency and types across time for longitudinal participants.

**Data File Required:**
- `imaging_diagnosis_mastersheet_2024-06-10.xlsx`

**R packages**: `readxl`, `dplyr`, `ggplot2`, `lubridate`

**Run:**
1. Load script
2. Ensure Excel file is in data/
3. Execute script

### 🧠 `amyloid_reads.R`

Merges and harmonizes multiple external amyloid PET datasets from GAAIN and NPDKA sources.

**Data Files Required:**

- `AlzheimersDiseaseRes-AmyloidPETmetricsADC_DATA_LABELS_2024-10-28_1324.csv`
- `2024-12-09T17_58_12.809Z_clinical_core_export.csv`
- `SCAN Amyloid PET (MRI-free) GAAIN analysis data.csv`
- `SCAN Amyloid PET (MRI-free) NPDKA analysis data.csv`

**R Packages:** `readr`, `dplyr`, `ggplot2`, `stringr`

**Run in RStudio:**
1. Load script
2. Place all data files in data/
3. Run top to bottom

## 📫 Contact

For questions or collaboration inquiries, please contact [Joshua Pasaye](https://github.com/jep9731).

---

Let me know if you'd like this README formatted for RMarkdown or Jupyter Book, or if you want an automatic file-checking script added for each project!
