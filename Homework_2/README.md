Explanation for HOME-WORK 2 

Description of  the assignment
I used the clinical dataset called  data_for_analysis from practical 2 class , which includes measurements of hormone levels, lipid profiles, carbohydrate metabolism, lipid peroxidation indicators, and antioxidant indices. 

I utilized R version 4.5.2 for my study. 

Description of the steps in my analysis 
I performed two analysis with and without lipids 5
 I made use of the following software tools, which are taken from the collection of R packages for data manipulation,  and statistical analysis: 

dplyr: used to eliminate superfluous columns from the data set. 
readr: Used to read data and its part of tidyverse
tidyr: used t create  tidy data

Conclusion of the analysis
The analysis of continuous variables using the Shapiro-Wilk test revealed that most variables were non-normally distributed across outcome categories, often showing skewness or outliers, which required the use of appropriate summary statistics tailored to each variable’s distribution. Descriptive statistics highlighted notable differences in central tendency and variability among outcome groups for several variables, suggesting potential associations with the outcome. Finally, missing values in the lipids5 variable were successfully addressed through median imputation, improving dataset completeness without substantially altering the overall distribution patterns or key findings.
