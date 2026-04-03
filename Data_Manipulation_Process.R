# ======================================================
# MY COMPLETE DATA MANIPULATION WITH MCAR TEST & LOF ANALYSIS

# ======================================================

#--------------------START--------------------------------
# Get current working directory
getwd()

# Set your file path (adjust as needed)
data_path <- "C:/Users/рс/Desktop/nwaokestanley/DataSet_No_Details.csv"

#----------------READ DATASET----------------------------
df <- read.csv(data_path)

# Display structure
str(df)

# Load libraries
library(dplyr)

#----------------DATA PREPARATION------------------------
# Remove unwanted columns
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2",
                    "an_index_23", "outcome", "factor_eth", "factor_h", 
                    "factor_pcos", "factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))

# Keep factor variables separately (optional)
factor_df <- df %>% select(record_id, outcome, factor_eth, factor_h, 
                           factor_pcos, factor_prl)

# Check structure
str(MD_df)

#--------------IDENTIFY MISSING VALUES--------------------
# Total NAs
sum(is.na(MD_df))

# NA counts per column
colSums(is.na(MD_df))

# Percentage of missing data
na_stats <- colMeans(is.na(MD_df)) * 100
print(na_stats)

# Columns with <=35% missing
na_stats_filtered <- na_stats[na_stats <= 35]
data.frame(
  Column = names(na_stats_filtered),
  NA_Percent = na_stats_filtered
)

# Columns with >35% missing
na_stats_filtered_1 <- na_stats[na_stats > 35]
data.frame(
  Column = names(na_stats_filtered_1),
  NA_Percent = na_stats_filtered_1
)

#--------------VISUALIZE MISSING DATA---------------------
library(visdat)
vis_miss(MD_df)

library(naniar)
gg_miss_var(MD_df)

#--------------REMOVE HIGH MISSING COLUMNS----------------
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13", "hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))

# ======================================================
# HOMEWORK: LITTLE'S MCAR TEST AND FOR EXTRA POINTS
# ======================================================
# Hypotheses:
# H₀: Data is Missing Completely at Random (MCAR)
# H₁: Data is NOT MCAR (MAR or MNAR)
#
# Decision:
# p-value > 0.05 → Fail to reject H₀ → Data is MCAR
# p-value ≤ 0.05 → Reject H₀ → Data is NOT MCAR
# ======================================================

# Select only numeric columns
numeric_data <- handle_MD_df %>% select(where(is.numeric))

# Perform Little's MCAR Test
cat("\n========== LITTLE'S MCAR TEST RESULTS ==========\n")
mcar_result <- mcar_test(numeric_data)
print(mcar_result)

# Interpretation
p_value <- mcar_result$p.value

cat("\n========== INTERPRETATION ==========\n")
if (p_value > 0.05) {
  cat(sprintf("p-value = %.4f > 0.05\n", p_value))
  cat(" Fail to reject H₀\n")
  cat("CONCLUSION: The data is Missing Completely at Random (MCAR).\n")
  cat("Multiple imputation is appropriate.\n")
} else {
  cat(sprintf("p-value = %.4f ≤ 0.05\n", p_value))
  cat(" Reject H₀\n")
  cat("CONCLUSION: The data is NOT MCAR (it is either MAR or MNAR).\n")
  cat("Consider more sophisticated missing data handling methods.\n")
}

#--------------IMPUTATION WITH MICE------------------------
library(mice)
library(ggplot2)

# Method: Predictive Mean Matching (PMM)
cat("\n========== PERFORMING MULTIPLE IMPUTATION ==========\n")
imputed_object <- mice(handle_MD_df, m = 5, method = "pmm", print = FALSE)
imputed_handle_MD_df_final <- complete(imputed_object)

#--------------FIX: REMOVE ROWS WITH MISSING VALUES FOR LOF--------

# We need to remove any rows that still have NAs
cat("\n========== CHECKING FOR MISSING VALUES ==========\n")
cat("Missing values before removal:", sum(is.na(imputed_handle_MD_df_final)), "\n")

# Remove rows with any missing values
imputed_clean <- na.omit(imputed_handle_MD_df_final)
cat("Missing values after removal:", sum(is.na(imputed_clean)), "\n")
cat("Rows removed:", nrow(imputed_handle_MD_df_final) - nrow(imputed_clean), "\n")
cat("Final dataset size:", nrow(imputed_clean), "rows ×", ncol(imputed_clean), "columns\n")

#--------------DENSITY PLOT COMPARISON---------------------
# Use hormone10_generated if it exists
if ("hormone10_generated" %in% names(handle_MD_df)) {
  density_plot <- ggplot() +
    geom_density(data = handle_MD_df, aes(x = hormone10_generated, fill = "Original"), alpha = 0.5, na.rm = TRUE) +
    geom_density(data = imputed_clean, aes(x = hormone10_generated, fill = "Imputed"), alpha = 0.5, na.rm = TRUE) +
    labs(title = "Density Plot: Original vs. Imputed (hormone10_generated)") +
    scale_x_continuous(limits = c(0, 2)) +
    theme_minimal()
  print(density_plot)
} else {
  cat("Column 'hormone10_generated' not found. Using first numeric column.\n")
  first_num <- names(handle_MD_df)[sapply(handle_MD_df, is.numeric)][1]
  
  # Create data frame for plotting
  orig_data <- data.frame(value = handle_MD_df[[first_num]], Method = "Original")
  imp_data <- data.frame(value = imputed_clean[[first_num]], Method = "Imputed")
  plot_data <- rbind(orig_data, imp_data)
  
  density_plot <- ggplot(plot_data, aes(x = value, fill = Method)) +
    geom_density(alpha = 0.5, na.rm = TRUE) +
    labs(title = paste("Density Plot:", first_num, "- Original vs. Imputed")) +
    theme_minimal()
  print(density_plot)
}

#--------------OUTLIER DETECTION-------------------------
library(tidyr)

# Boxplot for lipid variables
lipid_cols <- c("lipids1", "lipids2", "lipids3", "lipids4", "lipids5")
lipid_cols_exist <- lipid_cols[lipid_cols %in% names(imputed_clean)]

if (length(lipid_cols_exist) > 0) {
  outliers_data <- imputed_clean %>%
    select(all_of(lipid_cols_exist)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  
  lipid_boxplot <- ggplot(outliers_data, aes(x = variable, y = value)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    labs(title = "Outlier Detection - Lipid Variables",
         x = "Variables", y = "Value") +
    theme_minimal()
  print(lipid_boxplot)
}

# Boxplots for all numeric variables
all_boxplots <- imputed_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.color = "red") +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  labs(title = "Boxplots for Outlier Detection (All Numeric Variables)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
print(all_boxplots)

#--------------SUMMARY STATISTICS AFTER IMPUTATION--------
cat("\n========== SUMMARY AFTER IMPUTATION ==========\n")
cat("Original missing values:", sum(is.na(handle_MD_df)), "\n")
cat("Imputed missing values (before cleanup):", sum(is.na(imputed_handle_MD_df_final)), "\n")
cat("Final dataset (after removing rows with NAs):", nrow(imputed_clean), "rows\n")

# Display first few rows of cleaned imputed data
cat("\nFirst 6 rows of cleaned imputed data:\n")
head(imputed_clean)

# ======================================================
# LOCAL OUTLIER FACTOR (LOF) ANALYSIS - FIXED
# For dataset: imputed_clean (no NAs)
# ======================================================

cat("\n")
cat("==================================================\n")
cat("     LOCAL OUTLIER FACTOR (LOF) ANALYSIS\n")
cat("==================================================\n")

# Install and load dbscan if needed
if (!require("dbscan")) install.packages("dbscan")
library(dbscan)

# Prepare data for LOF (use the cleaned imputed dataset with no NAs)
lof_dataset <- imputed_clean

# Select only numeric columns
lof_data <- lof_dataset %>% select(where(is.numeric))

# Remove any columns with zero variance (constant values)
constant_cols <- sapply(lof_data, function(x) sd(x, na.rm = TRUE) == 0)
if(any(constant_cols)) {
  cat("\n Removing constant columns:", names(lof_data)[constant_cols], "\n")
  lof_data <- lof_data %>% select(-which(constant_cols))
}

# Scale the data (important for LOF algorithm)
lof_data_scaled <- scale(lof_data)


cat("\n Checking for NAs in scaled data:", sum(is.na(lof_data_scaled)), "\n")

cat("\n Dataset for LOF Analysis:")
cat("\n   • Observations:", nrow(lof_data_scaled))
cat("\n   • Variables:", ncol(lof_data_scaled))

# ---------- CALCULATING LOF FACTORS ----------
cat("\n\n Calculating Local Outlier Factor (LOF)...\n")

# Determine minPts value (number of neighbors) - using minPts instead of k
minPts_value <- min(20, floor(nrow(lof_data_scaled) / 5))
cat("   • Using minPts =", minPts_value, "neighbors\n")

# Calculate LOF scores using minPts (newer dbscan syntax)
lof_scores <- lof(lof_data_scaled, minPts = minPts_value)

# Add LOF scores to dataset
lof_dataset$LOF_Score <- lof_scores

# Determine outliers (LOF > 1.5 is typically considered outlier)
threshold <- 1.5
lof_dataset$Is_Outlier <- lof_scores > threshold

# LOF Summary Statistics
cat("\n LOF Score Statistics:")
cat("\n   • Min:", round(min(lof_scores), 4))
cat("\n   • 1st Quartile:", round(quantile(lof_scores, 0.25), 4))
cat("\n   • Median:", round(median(lof_scores), 4))
cat("\n   • Mean:", round(mean(lof_scores), 4))
cat("\n   • 3rd Quartile:", round(quantile(lof_scores, 0.75), 4))
cat("\n   • Max:", round(max(lof_scores), 4))

# Outlier detection results
n_outliers <- sum(lof_dataset$Is_Outlier)
pct_outliers <- round(n_outliers / nrow(lof_dataset) * 100, 2)

cat("\n Outlier Detection Results:")
cat("\n   • Total observations:", nrow(lof_dataset))
cat("\n   • Outliers detected (LOF > 1.5):", n_outliers)
cat("\n   • Outlier percentage:", pct_outliers, "%")

# Top outliers
cat("\n\n Top 10 Outliers by LOF Score:\n")
top_outliers <- lof_dataset %>%
  arrange(desc(LOF_Score)) %>%
  select(where(is.numeric), LOF_Score, Is_Outlier) %>%
  head(10)
print(top_outliers)

# ---------- VISUALIZING RESULTS ----------

# Visualization 1: Histogram of LOF factors
cat("\n Creating LOF Histogram...\n")

histogram_plot <- ggplot(lof_dataset, aes(x = LOF_Score)) +
  geom_histogram(aes(fill = ..count..), bins = 40, color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = 1, color = "blue", linetype = "dotted", size = 0.8) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  annotate("text", x = threshold + 0.3, y = Inf, 
           label = paste("Outlier Threshold (", threshold, ")", sep = ""),
           hjust = 0, vjust = 2, color = "red", size = 4) +
  annotate("text", x = 1, y = Inf, label = "Normal (LOF = 1)", 
           hjust = 0.5, vjust = 2, color = "blue", size = 4) +
  labs(title = "Local Outlier Factor (LOF) Distribution",
       subtitle = paste("minPts =", minPts_value, "neighbors |", n_outliers, "outliers (", pct_outliers, "%)"),
       x = "LOF Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(histogram_plot)

# Visualization 2: Bivariate scatterplot with outliers
if(ncol(lof_data) >= 2) {
  cat("\n Creating Bivariate Scatterplot...\n")
  
  # Select first two numeric variables for scatterplot
  var_x <- names(lof_data)[1]
  var_y <- names(lof_data)[2]
  
  scatter_plot <- ggplot(lof_dataset, aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_point(aes(color = Is_Outlier, size = LOF_Score), alpha = 0.6) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                       labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
    scale_size_continuous(range = c(1, 6)) +
    labs(title = "Bivariate Scatterplot with LOF Outliers",
         subtitle = paste("Red points = outliers (LOF > 1.5) | Size = LOF Score"),
         x = var_x, y = var_y,
         color = "Status", size = "LOF Score") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(scatter_plot)
} else {
  cat("\n Not enough variables for bivariate scatterplot (need at least 2)\n")
}

# Visualization 3: Boxplot of LOF scores by outlier status
cat("\n Creating LOF Boxplot...\n")

boxplot_lof <- ggplot(lof_dataset, aes(x = Is_Outlier, y = LOF_Score, fill = Is_Outlier)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red"),
                    labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "darkred", size = 1) +
  labs(title = "LOF Score Distribution: Normal vs Outlier",
       subtitle = paste("Threshold =", threshold),
       x = "Observation Status", y = "LOF Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(boxplot_lof)

# ======================================================
# FINAL SUMMARY
# ======================================================

cat("\n")
cat("==================================================\n")
cat("                 FINAL SUMMARY\n")
cat("==================================================\n")

cat("\n 1. LITTLE'S MCAR TEST:")
if (p_value > 0.05) {
  cat("\n      → Data is MCAR (p =", round(p_value, 4), ")")
} else {
  cat("\n      → Data is NOT MCAR (p =", round(p_value, 4), ")")
}

cat("\n\n 2. MULTIPLE IMPUTATION:")
cat("\n      → Method: PMM (Predictive Mean Matching)")
cat("\n      → Original missing values:", sum(is.na(handle_MD_df)))
cat("\n      → After imputation (clean):", sum(is.na(imputed_clean)))

cat("\n\n 3. LOCAL OUTLIER FACTOR (LOF):")
cat("\n      → minPts neighbors:", minPts_value)
cat("\n      → Outliers detected:", n_outliers, "(", pct_outliers, "%)")
cat("\n      → LOF score range:", round(min(lof_scores), 3), "to", round(max(lof_scores), 3))

cat("\n\n 4. VISUALIZATIONS CREATED:")
cat("\n      → Missing data patterns")
cat("\n      → Density plot (Original vs Imputed)")
cat("\n      → Traditional boxplots (Lipids + All variables)")
cat("\n      → LOF Histogram with threshold")
cat("\n      → LOF Scatterplot with outliers")
cat("\n      → LOF Boxplot")

cat("\n\n==================================================\n")
cat("              ANALYSIS COMPLETED!\n")
cat("==================================================\n")