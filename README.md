# Applied Statistics Coursework

## Table of Contents
- [Applied Statistics Coursework](#applied-statistics-coursework)
  - [Table of Contents](#table-of-contents)
  - [About](#about)
    - [Requirements](#requirements)
    - [Dataset](#dataset)
  - [Structure](#structure)

## About
This is the coursework for Applied Statistics module.

### Requirements
R 4.2.1

### Dataset
Chicago Crime rate
Kaggle: https://www.kaggle.com/datasets/chicago/chicago-crime 
Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2
Paper: https://ieeexplore.ieee.org/abstract/document/8768367
FBI: https://ucr.fbi.gov/nibrs/2011/resources/nibrs-offense-codes/view


## Structure
GitHub structure can be found bellow. This will be updated as the project develops.
```
DataMiningCoursework
│   README.md
│
└─── Code
│   │   code.R
│   
└─── Data
│    │   Chicago_Crime_Lite.csv
│    │   └───   Lite version of the dataset (only 10k rows)
│    │   liteData.R
│    │   └───   Code that generated lite dataset
│    
└─── Documents
│    └─── LaTeX
│        │    bibliography.bib
│        │    └───   Authors (for references) are added here
│        │ 
│        │    Report.tex
│        │    └───   Report in .tex file (.tex generates other items as well but they are not important)
│        │
│        │    Report.pdf
│        │    └───   Report as .pdf file
│        │
│        └─── images
│        │    │   header.jpg
│        │    │   └───   BCU img that is used in the header of the report
│        │    │
│        │    │   logo.png
│        │    │   └───   Logo of BCU that is displayed at the begining of the report
│        │
│        └─── Sections
│                └─── Executive_Summary
│                │   │   executive_summary.tex
│                │
│                └─── Introduction
│                │   │   introduction.tex
│                │
│                └─── Literature_Review
│                │   │   literature_review.tex
│                │
│                └─── Methodology
│                │   │   methodology.tex
│                │
│                └─── Dataset_Pre-processing
│                │   │   dataset_pre-processing.tex
│                │
│                └─── Datasets
│                │   │   datasets.tex
│                │
│                └─── Results_And_Discussion
│                │   │   results_and_Discussion.tex
│                │
│                └─── Conclusions
│                │   │   conclusions.tex
│                │
│                └─── References
│                │   │   references.tex
│                │
│                └─── Section1
│                    │   section1.tex
```