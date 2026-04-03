Data Analysis Home work 2026

Software used: I made use of R version 4.5.2(2026)

Homework Description
This homework involves data analysis, preprocessing, outlier detection, and handling missing values,and Testing the mechanism of missingness using Little's MCAR test and Implementing multiple imputation using Predictive Mean Matching (PMM)

My Data Description 
This is a clinical data in CSV format containing various measurements including:
 - `record_id`: Unique identifier for each sample
 - `outcome`: Result or classification variable
 - Hormones: Levels of multiple hormones 
 - Lipids: Lipid profile indicators (lipids1 to lipids5)
 - Carbohydrate Metabolism: Metabolic indicators
 - Lipid Peroxidation: Markers such as lipid_pero1 to lipid_pero5
 - Antioxidants: Antioxidant levels and indices

It has the name ”DataSet_No_Details.csv” with source file  (C:/Users/рс/Desktop/nwaokestanley/DataSet_No_Details.csv)

Procedures Used and why I used it
  
1. Little's MCAR Test
Procedure: Statistical test to check the pattern of missing data.
Why used: To determine if missing values happen completely at random (MCAR) or not. This result helps decide the correct way to handle missing information.

2. Multiple Imputation using PMM (Predictive Mean Matching)
Procedure: This is a Method to fill in empty or missing values in the dataset.
Why used: To replace missing entries with estimated values, so no data is wasted and analysis results remain accurate. PMM was chosen because it preserves the original distribution and structure of the data.
 
3. Local Outlier Factor (LOF) Analysis
Procedure: I used algorithm to detect unusual data points, using  k  nearest neighbors to measure how different a point is from others around it.
Why used: To identify outliers (values that are far from normal). Outliers can skew analysis and make results wrong, so they need to be found first.
 
4. Data Visualization
Procedure: I created different charts and graphs including:
- Missing data patterns plot
- Density plot (compare original vs imputed data)
- Boxplots (for all variables and lipid measurements)
- LOF Histogram, LOF Scatterplot, LOF Boxplot
Why used: To clearly see patterns, I checked if imputation worked correctly, and 	visually show where outliers are located for easy understanding.

Summary of the tools I used and what it did:

base R: Loaded the dataset into R
Dplyr : Removed unwanted columns
Visdat: Created missing data heatmap
Naniar: Performed Little's MCAR test
Mice: Imputed missing values using PMM
ggplot2: Created outlier detection plots
Dbscan: Calculated LOF scores for multivariate outliers
