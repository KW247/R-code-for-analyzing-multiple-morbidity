This document provides an explanation of the RStudio code file. 

The data used in this file comes from the ECLS trials, and the analysis focuses on changes in multiple morbidity between baseline and the two-year follow-up. Chi-square tests and logistic regression analyses are conducted for each morbidity to assess the impact of specific morbidities on patients and lung cancer screening.

Since the dataset from the ECLS trial is not publicly available, all attached files only show the code without the data.

This R document is divided into five parts, explained as follows:

    Part 1 includes the data cleaning process, such as removing duplicate blood records, merging medication records with questionnaire timing, and random group assignment. It also includes categorization of age and smoking pack-years in the demographic data. This section forms the basis for all subsequent parts.

    Part 2 deals with the baseline data. This section analyzes the baseline data, including calculations of the proportion of participants with and without multiple morbidity, and random group assignment data. Chi-square tests and logistic regression are performed for the morbidities.

    Part 3 deals the two-year follow-up data. The analysis in Part 3 is similar to that in Part 2.

    Part 4 is mainly for visualization. It includes:
        Calculating the prevalence of each morbidity at baseline and at the two-year follow-up, and analyzing the combined data from these two time points.
        Creating and exporting a heatmap of the correlations between morbidities at both time points.

    Part 5 focuses on calculating the number of new and recovered cases for each morbidity in the second year.

All the code runs in R version 4.3.2.
The necessary packages must be installed before running the code, and all packages are installed at the beginning of Part 1.

August 12, 2024.