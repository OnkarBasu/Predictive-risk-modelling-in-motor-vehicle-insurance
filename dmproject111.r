packages <- c(
  "tidyverse", "naniar", "caret", "missRanger", "dplyr",
  "corrplot", "ggplot2", "scales", "randomForest", "gam", "gamlss",
  "caTools", "MASS", "ranger", "statmod", "forcats", "recipes", "glmnet",
  "pscl", "mgcv", "cplm", "car", "parallel", "DescTools", "xgboost", 
  "Metrics", "scales", "tweedie"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


invisible(lapply(packages, install_if_missing))
lapply(packages, library, character.only = TRUE)

set.seed(123)

insurance_df <- read.csv('Motor vehicle insurance data.csv', sep=";")
hist(rowSums(is.na(insurance_df)), breaks = 50, main = "Missing values per row")

sum(rowSums(is.na(insurance_df)) > 0) # no. of rows with at least 1 missing value
sum(rowSums(is.na(insurance_df)) == 0) # no. of complete rows

missing_cols_summary <- insurance_df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = 'col', values_to = 'n_missing') %>%
  filter(n_missing > 0)
missing_cols_summary

# plot for missingness
# summary per feature
missing_summary <- insurance_df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = Missing_Count / nrow(insurance_df) * 100)

# bar plot with missing percentages as labels
p <- ggplot(missing_summary, aes(x = reorder(Feature, -Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Missing_Percent, 2), "%")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    y = "Count of missing values",
    x = "") +
  theme_minimal() +
  ylim(0, max(missing_summary$Missing_Count) * 1.15)

p


sapply(insurance_df, class)

# we have dates stored as characters -> change into date type
colnames(insurance_df)

columns_to_fix <- c("Length", "Value_vehicle", "R_Claims_history", "Premium", "Cost_claims_year")

for (col in columns_to_fix) {
  insurance_df[[col]] <- gsub(",", ".", insurance_df[[col]])
  
  insurance_df[[col]] <- as.numeric(insurance_df[[col]])
}

column_types <- sapply(insurance_df, class)

column_types_df <- data.frame(
  Column = names(column_types),
  DataType = as.character(column_types)
)

insurance_df <- insurance_df %>%
  mutate(
    Date_start_contract = as.Date(Date_start_contract, format = '%d/%m/%Y'),
    Date_last_renewal = as.Date(Date_last_renewal, format = '%d/%m/%Y'),
    Date_next_renewal = as.Date(Date_next_renewal, format = '%d/%m/%Y'),
    Date_birth = as.Date(Date_birth, format = '%d/%m/%Y'),
    Date_driving_licence = as.Date(Date_driving_licence, format = '%d/%m/%Y'),
    Date_lapse = as.Date(Date_lapse, format = '%d/%m/%Y')
  )


missings_data <- insurance_df %>%
  filter(is.na(Length) | is.na(Type_fuel))

# still need to inspect variable Length
insurance_df$missing_Length  <- as.integer(is.na(insurance_df$Length))


# check relationship with categorical variables
# from here we can see that Length missingness is almost entirely explained by variable Type_risk -> it is not randomly missing, it is missing structurally
# the missingness is almost entirely concentrated in policies covering motorbikes and agricultural vehicles (Type_risk = 1 and 4).
# note: explain what could be the reason
# note: this is almost fully observed for cars and vans and highly missing for motorbikes and agricultural vehicles -> this variable is acting as proxy for vehicle class, which is not a direct risk factor
# for other vars: missing Length vehicles more ofthen located in area = 1 (urban); vehicles with missing Length almost always have no second driver
# (when recorded - around 11 percent of policies have a second driver, compared to 0.6 percent when it is not recorded).

vars_cat <- c("Type_risk", "Area", "Second_driver", "Payment", "Distribution_channel")

lapply(vars_cat, function(v) {
  print(v)
  print(round(prop.table(table(insurance_df$missing_Length, insurance_df[[v]]), 2), 3))
  cat("\n")
})

cat_summary <- insurance_df %>%
  dplyr::select(all_of(vars_cat), missing_Length) %>%
  pivot_longer(cols = all_of(vars_cat), names_to = "variable", values_to = "value") %>%
  group_by(variable, value, missing_Length) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable, missing_Length) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(variable, missing_Length, value)

cat_summary <- cat_summary %>%
  mutate(prop_percent = round(prop * 100, 1)) %>%
  dplyr::select(-prop)

cat_summary


check_missing_assoc_cat <- function(var) {
  df <- insurance_df %>% dplyr::select(missing_Length, all_of(var))
  
  contingency_table <- table(df[[var]], df$missing_Length)
  
  chi_sq_result <- chisq.test(contingency_table)
  
  tibble(
    variable = var,
    test_used = "Chi-Squared",
    p_value = chi_sq_result$p.value,
    total_observations = sum(contingency_table)
  )
}

res_cat <- purrr::map_dfr(vars_cat, check_missing_assoc_cat)
res_cat


# now, let's see how it looks with the numeric vars of interest
# looking at the table we can say that cars with missing Length are:
# 1. older (mean production year around 2000 compared to 2005)
# 2. cheaper - mean value of the vehicle is around 8k euros compared to around 19.k euros
# 3. less powerful - 31 HP vs 99 HP
# 4. much lighter - 494 kg vs 1267 kg
# what this means: missing-length vehicles look like small, old, lower value (likely mopeds, light motorcycles, etc) -> type_fuel would also be irrelevant or harder to classify for these
# again, this confirms that missing Length and Type_fuel are NOT RANDOM, they are structurally missing for a specific class of vehicles ->  MAR (missing at random)
vars_num <- c("Year_matriculation", "Value_vehicle", "Power", "Weight")

insurance_df %>%
  group_by(missing_Length) %>%
  summarise(across(all_of(vars_num), ~mean(.x, na.rm = TRUE)))


# note: this Shapiro Wilk test cannot be applied because dataset is large
# when normality cannot be assumed - non parametric alternative
check_missing_assoc <- function(var) {
  df <- insurance_df %>% dplyr::select(missing_Length, all_of(var))
  
  w <- wilcox.test(df[[var]] ~ df$missing_Length)
  
  # summarize group means
  means <- df %>%
    group_by(missing_Length) %>%
    # use median for a better comparison with Wilcoxon, but mean is also fine for effect size
    summarise(
      #mean_value = mean(.data[[var]], na.rm = TRUE),
      median_value = median(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  tibble(
    variable = var,
    test_used = "Wilcoxon",
    p_value = w$p.value,
    #mean_missing0 = means$mean_value[means$missing_Length == 0],
    #mean_missing1 = means$mean_value[means$missing_Length == 1],
    median_missing0 = means$median_value[means$missing_Length == 0],
    median_missing1 = means$median_value[means$missing_Length == 1]
  )
}

res <- purrr::map_dfr(vars_num, check_missing_assoc)
res


# --- Numerical Outlier Analysis ---

numeric_cols <- names(select_if(insurance_df, is.numeric))
outlier_summary <- list()
insurance_df_no_outliers <- insurance_df

for (col in numeric_cols) {
  if (n_distinct(insurance_df[[col]]) < 10) next
  
  Q1 <- quantile(insurance_df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(insurance_df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- insurance_df[[col]][(insurance_df[[col]] < lower_bound) | (insurance_df[[col]] > upper_bound)]
  num_outliers <- length(na.omit(outliers))
  
  if (num_outliers > 0) {
    outlier_summary[[col]] <- num_outliers
  }
  
  insurance_df_no_outliers <- insurance_df_no_outliers %>%
    filter(.data[[col]] >= lower_bound & .data[[col]] <= upper_bound)
}

if (length(outlier_summary) > 0) {
  print(as.data.frame(t(bind_rows(outlier_summary))))
}

range_summary <- insurance_df_no_outliers %>%
  summarise(across(all_of(numeric_cols),
                   list(Min = ~min(.x, na.rm = TRUE),
                        Max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"),
               names_pattern = "(.*)_(.*)")

print(range_summary)

# --- Boxplots ---
for (col in numeric_cols) {
  if (n_distinct(insurance_df[[col]]) < 10) next
  
  p <- ggplot(insurance_df, aes_string(y = col)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 1) +
    labs(title = paste("Boxplot of", col), y = col) +
    theme_minimal()
  
  print(p)
}

categorical_cols <- names(select_if(insurance_df, is.factor))

for (col in categorical_cols) {
  cat(paste("Frequency table for column:", col, "\n"))
  print(table(insurance_df[[col]]))
  cat("\n----------------------------------------\n")
}

idf1 <- insurance_df
idf3 <- insurance_df

# assumption: all policies have exactly 1 year of exposure
# since exposure = 1 for all policies, N_claims_year is already the frequency rate
# no offset needed in GLM models

# Date_start_contract to Date_next_renewal should span ~1 year

# verify exposure assumption:
idf1 <- idf1 %>%
  mutate(
    contract_days = as.numeric(difftime(Date_next_renewal, Date_last_renewal, units = "days")),
    exposure_years = contract_days / 365.25
  )

summary(idf1$exposure_years)
hist(idf1$exposure_years, breaks = 50, main = "exposure distribution", 
     xlab = "years", col = "#63B8FF")

# flag problematic records
exposure_issues <- idf1 %>%
  filter(exposure_years < 0.9 | exposure_years > 1.1) %>%
  dplyr::select(ID, Date_last_renewal, Date_next_renewal, exposure_years, Lapse, Date_lapse)

cat("records with non-standard exposure:", nrow(exposure_issues), 
    "(", round(100 * nrow(exposure_issues) / nrow(idf1), 2), "%)\n")

# Lapse = number of policies cancelled this year 
# this affects your exposure calculation 

# check lapse patterns
table(idf1$Lapse)
prop.table(table(idf1$Lapse))

# for policies with Lapse > 0 and Date_lapse exists:
lapsed_this_year <- idf1 %>%
  filter(Lapse > 0, !is.na(Date_lapse)) %>%
  mutate(
    days_active = as.numeric(difftime(Date_lapse, Date_last_renewal, units = "days")),
    actual_exposure = days_active / 365.25
  )
View(lapsed_this_year)

# check if claims occurred BEFORE lapse
lapsed_with_claims <- lapsed_this_year %>%
  filter(N_claims_year > 0)

cat("lapsed policies with claims:", nrow(lapsed_with_claims), "\n")
cat("mean exposure for lapsed policies:", 
    round(mean(lapsed_this_year$actual_exposure, na.rm = TRUE), 3), "years\n")

# --- Feature Engineering ---
idf3_engineered <- idf3 %>%
  mutate(
    Driver_Age = floor(as.numeric(interval(Date_birth, Date_start_contract) / years(1))),
    Driving_Experience = floor(as.numeric(interval(Date_driving_licence, Date_start_contract) / years(1))),
    
    Vehicle_Age = year(Date_start_contract) - Year_matriculation,
    Power_to_Weight_Ratio = ifelse(Weight > 0, Power / Weight, 0),
    
    Contract_Duration = as.numeric(Date_next_renewal - Date_start_contract),
    Days_Since_Last_Renewal = as.numeric(Date_start_contract - Date_last_renewal),
    Product_Holding_Ratio = ifelse(Max_policies > 0, Max_products / Max_policies, 0),
    
    Has_Claimed_In_History = ifelse(N_claims_history > 0, 1, 0),
    Is_New_Customer = ifelse(Seniority == 0, 1, 0)
  )

idf3 <- idf3_engineered

# add exposure calculation
idf3 <- idf3 %>%
  mutate(
    # exposure from last renewal to next renewal (standard contract period)
    exposure_days = as.numeric(difftime(Date_next_renewal, Date_last_renewal, units = "days")),
    exposure_years = exposure_days / 365.25,
    
    #fFor lapsed policies, actual exposure is shorter
    exposure_years_actual = case_when(
      Lapse > 0 & !is.na(Date_lapse) ~ 
        as.numeric(difftime(Date_lapse, Date_last_renewal, units = "days")) / 365.25,
      TRUE ~ exposure_years
    ),
    
    # floor at 1 day minimum to avoid division by zero
    exposure_years_actual = pmax(exposure_years_actual, 1/365.25),
    
    # calculate annualised claim rate
    claim_rate_annual = N_claims_year / exposure_years_actual
  )

exposure_summary <- idf3 %>%
  summarise(
    n_total = n(),
    mean_exposure = mean(exposure_years_actual, na.rm = TRUE),
    median_exposure = median(exposure_years_actual, na.rm = TRUE),
    min_exposure = min(exposure_years_actual, na.rm = TRUE),
    max_exposure = max(exposure_years_actual, na.rm = TRUE),
    pct_full_year = mean(exposure_years_actual >= 0.95 & exposure_years_actual <= 1.05, na.rm = TRUE) * 100
  )

print(exposure_summary)

# exposure by lapse status
exposure_by_lapse <- idf3 %>%
  group_by(Lapsed = Lapse > 0) %>%
  summarise(
    n = n(),
    mean_exposure = mean(exposure_years_actual, na.rm = TRUE),
    mean_claims = mean(N_claims_year, na.rm = TRUE),
    mean_annual_rate = mean(claim_rate_annual, na.rm = TRUE)
  )

print(exposure_by_lapse)

round(exposure_by_lapse$mean_exposure[exposure_by_lapse$Lapsed == TRUE], 3) # lapsed policies have mean exposure of 0.261 years
round(1 / exposure_by_lapse$mean_exposure[exposure_by_lapse$Lapsed == TRUE], 1) # Without adjustment, their frequency would be underestimated by ~ 3.8 times

glimpse(idf3 %>% dplyr::select(Driver_Age, Driving_Experience, Vehicle_Age, 
                               Power_to_Weight_Ratio, Has_Claimed_In_History,
                               exposure_years_actual, claim_rate_annual))


# note: this specific code is dedicated to making previously assessed missigness meaningful
# for that: create missingness indicator variables
# for var length was already created
idf3$Type_Fuel_Missing  <- as.integer(is.na(insurance_df$Type_fuel))

nzv_index <- nearZeroVar(idf3, saveMetrics = FALSE)
cols_nzv <- names(idf3)[nzv_index]
cols_nzv

# drop vars 'Is_New_Customer' and 'Type_Fuel_Missing', since they offer almost no useful signal
idf3 <- idf3 %>% dplyr::select(-c(Is_New_Customer, Type_Fuel_Missing))

length_impute_vars <- c(
  "Type_risk", "Power", "Cylinder_capacity", "Weight",
  "Value_vehicle", "N_doors", "Vehicle_Age", "Power_to_Weight_Ratio",
  "Area", "Driver_Age", "Driving_Experience", "missing_Length"
)

length_data <- idf3 %>% dplyr::select(all_of(length_impute_vars), Length)

length_imputed <- missRanger(
  length_data,
  formula = Length ~ .,
  pmm.k = 5,
  num.trees = 100,
  seed = 123
)

length_imputed %>%
  filter(missing_Length == 1) %>%
  dplyr::select(Length, everything()) %>%
  View()

idf_imp <- idf3
idf_imp$Length <- length_imputed$Length

ggplot(idf3, aes(x = Length)) +
  geom_histogram(binwidth = 0.2, fill="blue", alpha=0.5) +
  labs(title = "Original Length Distribution")

ggplot(idf_imp, aes(x = Length)) +
  geom_histogram(binwidth = 0.2, fill="green", alpha=0.5) +
  labs(title = "Imputed Length Distribution")

summary(idf3$Length)
summary(idf_imp$Length)

idf_imp$Type_fuel <- fct_na_value_to_level(idf_imp$Type_fuel, "Unknown")

all_cols <- colnames(idf3)

cols_to_check <- setdiff(all_cols, "Date_lapse")

original_rows <- nrow(idf3)

idf3_cleaned <- idf3 %>%
  filter(across(all_of(cols_to_check), ~ !is.na(.)))

new_rows <- nrow(idf3_cleaned)
cat("Number of rows after removing NAs from all columns:", new_rows, "\n")
cat("Total rows removed:", original_rows - new_rows, "\n\n")
idf3 <- idf3_cleaned


columns_to_log <- c(
  "Cost_claims_year",
  "Weight",
  "Cylinder_capacity",
  "Power",
  "N_claims_history"
)

idf_imp <- idf_imp %>%
  mutate(across(all_of(columns_to_log), ~ log1p(.), .names = "{.col}_log"))


original_cols_to_drop <- c(
  #"Cost_claims_year",have it
  "Weight",
  "Cylinder_capacity",
  "Power",
  "N_claims_history"
)

idf_imp <- idf_imp %>%
  dplyr::select(-all_of(original_cols_to_drop))

# drop the temporary exposure_days var
idf_imp <- idf_imp %>%
  dplyr::select(-exposure_days)

glimpse(idf_imp)

idf_imp <- idf_imp %>%
  dplyr::select(-Days_Since_Last_Renewal)

#idf_imp <- idf_imp %>%
#dplyr::select( -ID)

# drop date columns (BUT keep exposure_years_actual and claim_rate_annual)
idf_imp <- idf_imp %>%
  dplyr::select(-Date_start_contract,-Date_last_renewal,-Date_next_renewal,-Date_birth,-Date_driving_licence)

idf_imp1 <- idf_imp # save to have cost_claims_year for later

idf_imp <- idf_imp %>%
  dplyr::select(-Cost_claims_year)

numerical_to_plot <- c()
categorical_to_plot <- c()
for (col_name in names(idf_imp)) {
  col_data <- idf_imp[[col_name]]
  if (is.numeric(col_data) && n_distinct(col_data) > 10) {
    numerical_to_plot <- c(numerical_to_plot, col_name)
  }
  else if (is.factor(col_data) || is.character(col_data) || (is.numeric(col_data) && n_distinct(col_data) <= 10)) {
    categorical_to_plot <- c(categorical_to_plot, col_name)
  }
}

# --- Histogram ---
for (col in numerical_to_plot) {
  p <- ggplot(idf_imp, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", col), y = "Frequency") +
    theme_minimal()
  print(p)
}

# --- Bar Charts ---
for (col in categorical_to_plot) {
  
  p <- ggplot(idf_imp, aes_string(x = paste0("as.factor(", col, ")"))) +
    geom_bar(fill = "orange", alpha = 0.8) +
    labs(title = paste("Frequency of", col), x = col, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


all_predictors <- setdiff(names(idf_imp), c("N_claims_year", "Cost_claims_year", "Lapse", "Date_lapse", "Date_start_contract", "Date_last_renewal", "Date_next_renewal", "Date_birth", "Date_driving_licence"))

numerical_predictors <- c()
categorical_predictors <- c()

for (col_name in all_predictors) {
  col_data <- idf_imp[[col_name]]
  
  if (is.numeric(col_data) && n_distinct(col_data, na.rm = TRUE) > 10) {
    numerical_predictors <- c(numerical_predictors, col_name)
  }
  else if (is.factor(col_data) || is.character(col_data) || (is.numeric(col_data) && n_distinct(col_data, na.rm = TRUE) <= 10)) {
    categorical_predictors <- c(categorical_predictors, col_name)
  }
}


idf_imp_plot <- idf_imp %>%
  mutate(Claims_Group = case_when(
    N_claims_year == 0 ~ "0 Claims",
    N_claims_year == 1 ~ "1 Claim",
    N_claims_year >= 2 ~ "2+ Claims"
  ))

idf_imp_plot$Claims_Group <- factor(idf_imp_plot$Claims_Group, levels = c("0 Claims", "1 Claim", "2+ Claims"))


for (col in numerical_predictors) {
  p <- ggplot(idf_imp_plot, aes_string(x = "Claims_Group", y = col, fill = "Claims_Group")) +
    geom_boxplot(show.legend = FALSE) +
    labs(title = paste(col, "by Number of Claims"),
         x = "Number of Claims",
         y = col) +
    theme_minimal()
  
  print(p)
}

for (col in categorical_predictors) {
  
  p <- ggplot(idf_imp_plot, aes_string(x = paste0("as.factor(", col, ")"), fill = "Claims_Group")) +
    geom_bar(position = "fill") +
    labs(title = paste("Proportion of Claims by", col),
         x = col,
         y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

idf_imp_plot <- idf_imp %>%
  mutate(N_claims_year = as.numeric(as.character(N_claims_year))) %>%
  
  mutate(Claims_Group = case_when(
    N_claims_year == 0 ~ "0 Claims",
    N_claims_year == 1 ~ "1 Claim",
    N_claims_year >= 2 ~ "2+ Claims"
  )) %>%
  
  mutate(Claims_Group = factor(Claims_Group, levels = c("0 Claims", "1 Claim", "2+ Claims"))) %>%
  
  filter(!is.na(Claims_Group))

# --- Predictors ---
# Excluded: Lapse, Date_lapse, Cost_claims_year_log (outcomes)

numerical_predictors <- c(
  "Seniority", "Premium", "R_Claims_history", "Value_vehicle", "Length",
  "Driver_Age", "Driving_Experience", "Vehicle_Age", "Power_to_Weight_Ratio",
  "Contract_Duration", "Product_Holding_Ratio", "Weight_log",
  "Cylinder_capacity_log", "Power_log", "N_claims_history_log"
)

categorical_predictors <- c(
  "Distribution_channel", "Policies_in_force", "Max_policies", "Max_products",
  "Payment", "Type_risk", "Area", "Second_driver", "N_doors",
  "Type_fuel", "missing_Length", "Has_Claimed_In_History"
)


for (col in numerical_predictors) {
  
  p <- ggplot(idf_imp_plot, aes_string(x = "Claims_Group", y = col, fill = "Claims_Group")) +
    geom_violin(trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.7, show.legend = FALSE, outlier.shape = NA) +
    labs(title = paste("Distribution of", col, "by Claim Group"),
         x = "Number of Claims",
         y = col) +
    theme_minimal()
  
  print(p)
}

for (col in categorical_predictors) {
  
  p <- ggplot(idf_imp_plot, aes_string(x = paste0("as.factor(", col, ")"), fill = "Claims_Group")) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste("Proportion of Claims by", col),
         x = col,
         y = "Proportion",
         fill = "Claim Group") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

idf_imp <- idf_imp1 %>%
  mutate(unique_row_id = row_number())

# --- Modelling ---

# --- global split --- NOTE: USE FOR ALL, NOT JUST FOR FREQUENCY

set.seed(123)
zero_indicator <- ifelse(idf_imp$N_claims_year > 0, 1, 0)
train_idx <- createDataPartition(as.factor(zero_indicator), p = 0.75, list = FALSE)
train_global <- idf_imp[train_idx, ]
test_global  <- idf_imp[-train_idx, ]

# helper f-ion for metrics
compute_metrics <- function(y_true, y_pred) {
  rmse <- sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
  mae  <- mean(abs(y_true - y_pred), na.rm = TRUE)
  list(RMSE = rmse, MAE = mae)
}

# ---- FREQUENCY MODELS ----

forbidden_cols_freq <- c(
  "Cost_claims_year_log",  # this one's severity response
  "Cost_claims_year",
  "Lapse", # causes low exposure - would be leakage
  "Date_lapse", # same reason
  "exposure_years_actual", # not use as predictor - use as offset
  "claim_rate_annual" # derived from response - would be leakage
)

train_freq_temp <- train_global %>% 
  mutate(across(where(is.character), as.factor))

test_freq_temp <- test_global %>%
  mutate(across(where(is.character), as.factor))

# extract ID and exposure beforee removing cols
train_id <- train_freq_temp$ID
test_id <- test_freq_temp$ID

train_unique_id <- train_freq_temp$unique_row_id
test_unique_id <- test_freq_temp$unique_row_id

train_exposure <- train_freq_temp$exposure_years_actual
test_exposure <- test_freq_temp$exposure_years_actual

train_rate_annual <- train_freq_temp$claim_rate_annual
test_rate_annual <- test_freq_temp$claim_rate_annual

train_exposure <- pmax(train_exposure, 0.1) # minimum 36 days
test_exposure <- pmax(test_exposure, 0.1) # minimum 36 days

train_rate_annual <- train_freq_temp$N_claims_year / train_exposure
test_rate_annual <- test_freq_temp$N_claims_year / test_exposure

# remove forbidden cols
train_freq <- train_freq_temp %>% 
  dplyr::select(-all_of(forbidden_cols_freq), -exposure_years, -ID, -unique_row_id)

test_freq <- test_freq_temp %>%
  dplyr::select(-all_of(forbidden_cols_freq), -exposure_years, -ID, -unique_row_id)

# verify alignment
cat("train_freq rows:", nrow(train_freq), "\n")
cat("train_exposure length:", length(train_exposure), "\n")
cat("test_freq rows:", nrow(test_freq), "\n")
cat("test_exposure length:", length(test_exposure), "\n")

# check multicollinearity
vif(lm(N_claims_year ~ ., data = train_freq))

# --- Poisson GLM ---

poisson_model <- glm(
  N_claims_year ~ . - Vehicle_Age - Year_matriculation - Weight_log + offset(log(train_exposure)),
  data = train_freq,
  family = poisson(link = "log")
)
summary(poisson_model)

pred_poisson <- predict(poisson_model, 
                        newdata = test_freq %>% mutate(train_exposure = test_exposure),
                        type = "response")

# evaluate against annual rates (not raw counts)
metrics_poisson <- compute_metrics(test_rate_annual, pred_poisson)
dispersion_poisson <- sum(residuals(poisson_model, type="pearson")^2) / poisson_model$df.residual

cat("Poisson - RMSE:", round(metrics_poisson$RMSE, 4), # fairly high, since influenced by outliers
    " MAE:", round(metrics_poisson$MAE, 4), #  on average, predictions are off by less than 1 claim per year
    " Dispersion:", round(dispersion_poisson, 4), "\n") # underdispresion, NB won't help

summary(test_rate_annual)

# convert predictions to counts for the actual exposure
pred_counts <- pred_poisson * test_exposure
actual_counts <- test_freq_temp$N_claims_year[1:length(test_exposure)]

metrics_final <- compute_metrics(actual_counts, pred_counts)

cat("final model performance (count scale):\n")
cat("RMSE:", round(metrics_final$RMSE, 4), "claims\n")
cat("MAE:", round(metrics_final$MAE, 4), "claims\n")

# recall that we have lots of zeros
zero_rate <- mean(train_freq$N_claims_year == 0)
round(zero_rate*100,2) # % of zeros in the train set; in test set should be the same, since stratified split was done

# extract significant predictors from Poisson (p < 0.05)
poisson_summary <- summary(poisson_model)$coefficients
sig_vars <- rownames(poisson_summary)[poisson_summary[, "Pr(>|z|)"] < 0.05]
sig_vars <- sig_vars[sig_vars != "(Intercept)"]

cat("Significant predictors:", length(sig_vars), "\n")
print(sig_vars)

# fit ZIP with only significant predictors
formula_zip_sig <- as.formula(paste(
  "N_claims_year ~", 
  paste(sig_vars, collapse = " + "), 
  "+ offset(log(train_exposure)) | 1"
))

zip_model <- zeroinfl(formula_zip_sig, data = train_freq, dist = "poisson")

pred_zip <- predict(zip_model, 
                    newdata = test_freq %>% mutate(train_exposure = test_exposure),
                    type = "response")

metrics_zip <- compute_metrics(test_rate_annual, pred_zip)

cat("ZIP (sig predictors) - RMSE:", round(metrics_zip$RMSE, 4),
    " MAE:", round(metrics_zip$MAE, 4), "\n")

# check AIC for all available models
aic_values <- data.frame(
  Model = c("Full Poisson", "ZIP (sig preds)"),
  AIC  = c(
    AIC(poisson_model),
    AIC(zip_model)
  )
)
print(aic_values)

# ---- Random forest ---- 

# add annual rates to training data
train_freq_rf <- train_freq %>%
  mutate(claim_rate_annual = train_rate_annual)

rf_preds <- setdiff(names(train_freq_rf), c("N_claims_year", "claim_rate_annual"))

rf_formula <- as.formula(
  paste("claim_rate_annual ~", paste(rf_preds, collapse = " + "))
)

p <- length(rf_preds)

set.seed(123)
rf_freq <- ranger(
  formula = rf_formula,
  data = train_freq_rf,
  num.trees = 500,
  mtry = floor(sqrt(p)),
  min.node.size = 10,
  replace = TRUE,
  num.threads = detectCores() - 1,
  importance = "none",
  respect.unordered.factors = "order",
  keep.inbag = TRUE
)

# predict annual rates
pred_rf <- predict(rf_freq, data = test_freq)$predictions
pred_rf_initial <- pred_rf

# evaluate on same scale as Poisson/ZIP
metrics_rf <- compute_metrics(test_rate_annual, pred_rf)

cat("RF (annual rates) - RMSE:", round(metrics_rf$RMSE, 4), "\n")
cat("RF (annual rates) - MAE:", round(metrics_rf$MAE, 4), "\n")

r_sq <- rf_freq$r.squared
r_sq_oob <- rf_freq$r.squared * 100
r_sq_oob

format_aic <- function(aic) {
  format(round(aic, 0), big.mark = ",")
}

# comparison table
model_comparison <- data.frame(
  Model = c("Poisson GLM", "ZIP (sig preds)", "Random Forest"),
  RMSE = c(metrics_poisson$RMSE, metrics_zip$RMSE, metrics_rf$RMSE),
  MAE  = c(metrics_poisson$MAE, metrics_zip$MAE, metrics_rf$MAE),
  AIC  = c(format_aic(AIC(poisson_model)),
           format_aic(AIC(zip_model)),
           "-"),
  R_sqrd_OOB = c("-", "-", paste0(round(r_sq_oob, 2), "%"))
)

print(model_comparison)

# check RF performance by claim bucket
df_perf <- data.frame(
  actual = test_rate_annual,
  predicted = pred_rf
)

df_perf$bucket <- cut(df_perf$actual, 
                      breaks = c(-1, 0, 1, 2, 5, 10, Inf),
                      labels = c("0", "0-1", "1-2", "2-5", "5-10", "10+"))

df_summary <- df_perf %>%
  group_by(bucket) %>%
  summarise(
    n = n(),
    mean_actual = mean(actual),
    mean_predicted = mean(predicted),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted))
  )

print(df_summary) # high raates are severly underpredicted
# BUT: high-rate policies (10+) are only 42 cases, which is extremely little in comparison to the actual set

residuals_rf <- test_rate_annual - pred_rf

# predicted vs actual
ggplot(df_perf, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5, color = "#63B8FF") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "RF: Predicted vs actual annual rates",
       x = "actual annual rate", 
       y = "predicted annual rate") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

# residuals distribution
ggplot(data.frame(residuals = residuals_rf), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "#63B8FF", color = "black") +
  labs(title = "Residuals Distribution - Random Forest", 
       x = "Residual", y = "Count") +
  theme_minimal()

# try again with RF

# --- RF with High claim adjustment ---

# identify high predicted claims
high_claim_threshold <- 3
high_idx <- pred_rf >= high_claim_threshold

# train auxiliary RF on high-claim policies only
if(sum(high_idx) > 0){
  train_high <- train_freq_rf %>% filter(claim_rate_annual >= high_claim_threshold)
  
  rf_high <- ranger(
    claim_rate_annual ~ ., 
    data = train_high %>% dplyr::select(all_of(rf_preds), claim_rate_annual),
    num.trees = 500,
    mtry = floor(sqrt(length(rf_preds))),
    min.node.size = 5,
    replace = TRUE,
    num.threads = detectCores() - 1
  )
  
  # update predictions for high-claim test cases
  pred_rf[high_idx] <- predict(rf_high, data = test_freq[high_idx, rf_preds])$predictions
}

# metrics
metrics_rf_adj <- compute_metrics(test_rate_annual, pred_rf)
cat("RF (high-claim adj) - RMSE:", round(metrics_rf_adj$RMSE, 4), "\n")
cat("RF (high-claim adj) - MAE:", round(metrics_rf_adj$MAE, 4), "\n")

# bucket summary
df_perf_adj <- data.frame(
  actual = test_rate_annual,
  predicted = pred_rf
)

df_perf_adj$bucket <- cut(
  df_perf_adj$actual,
  breaks = c(-1, 0, 1, 2, 5, 10, Inf),
  labels = c("0", "0-1", "1-2", "2-5", "5-10", "10+"),
  right = TRUE,
  include.lowest = TRUE
)

df_summary_adj <- df_perf_adj %>%
  group_by(bucket) %>%
  summarise(
    n = n(),
    mean_actual = mean(actual),
    mean_predicted = mean(predicted),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted))
  )

print(df_summary_adj)

# residuals
residuals_rf_adj <- test_rate_annual - pred_rf

# prepare df for plot comparison
df_plot <- data.frame(
  actual = rep(test_rate_annual, 2),
  predicted = c(pred_rf_initial, pred_rf),  # initial RF, updated RF
  Model = rep(c("RF initial", "RF updated"), each = length(test_rate_annual))
)

# predicted vs actual plot (comparison)
ggplot(df_plot, aes(x = actual, y = predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Predicted vs actual annual rates: RF comparison",
    x = "Actual annual rate",
    y = "predicted annual rate",
    color = "Model"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  scale_color_manual(values = c("blue", "red"))

# HERE: frequency predictions for loss calculation

frequency_results <- data.frame(
  # identifier for merging
  unique_row_id = test_unique_id, # since ID can be duplicated
  ID = test_id,
  
  # actual values
  actual_claims = test_freq_temp$N_claims_year,
  actual_exposure = test_exposure,
  actual_rate = test_rate_annual,
  
  # predictions (annual rates)
  pred_rf_rate = pred_rf_initial,
  pred_poisson_rate = pred_poisson,
  pred_zip_rate = pred_zip,
  
  # predictions (expected counts for actual exposure)
  pred_rf_count = pred_rf_initial * test_exposure,
  pred_poisson_count = pred_poisson * test_exposure,
  pred_zip_count = pred_zip * test_exposure
)

write.csv(frequency_results, "frequency_predictions.csv", row.names = FALSE)

# ---- TWEEDIE EXPECTED LOSS ----

forbidden_cols_loss <- c(
  "Lapse", 
  "Date_lapse", 
  "Payment", 
  "Premium", 
  "N_claims_year",
  "claim_rate_annual",
  "Cost_claims_year"  # Keep only the log version
)

train_loss <- train_global %>% 
  dplyr::select(-all_of(forbidden_cols_loss)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

test_loss <- test_global %>% 
  dplyr::select(-all_of(forbidden_cols_loss)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

test_loss_ids <- test_loss$ID
test_loss_unique_ids <- test_loss$unique_row_id

# VIF check
vif(lm(Cost_claims_year_log ~ . - Year_matriculation - Weight_log - ID - unique_row_id, 
       data = train_loss))

# inital model
set.seed(123)
model_cpglm.0 <- cpglm(
  Cost_claims_year_log ~ . - Year_matriculation - Weight_log - ID - unique_row_id,
  data = train_loss,
  link = "log"
)

y_test <- test_loss$Cost_claims_year_log
preds_0 <- predict(model_cpglm.0, newdata = test_loss, type = "response")
p <- model_cpglm.0$p
phi <- model_cpglm.0$phi

MAE <- mean(abs(y_test - preds_0))
RMSE <- sqrt(mean((y_test - preds_0)^2))
cat("Model 0 - MAE:", round(MAE, 2), "RMSE:", round(RMSE, 2), "\n")

# model 1 - reduced
set.seed(123)
model_cpglm_final <- cpglm(
  Cost_claims_year_log ~ . - Year_matriculation - Weight_log - Distribution_channel
  - N_doors - Type_fuel - Length - Power_to_Weight_Ratio - Cylinder_capacity_log 
  - Max_products - ID - unique_row_id,
  data = train_loss,
  link = "log"
)

summary(model_cpglm_final)

preds <- predict(model_cpglm_final, newdata = test_loss, type = "response")

# metrics on log scale
mae_cpglm <- mean(abs(test_loss$Cost_claims_year_log - preds))
rmse_cpglm <- sqrt(mean((test_loss$Cost_claims_year_log - preds)^2))
rank_corr <- cor(test_loss$Cost_claims_year_log, preds, method = "spearman")

cat("\n--- Final Tweedie Results (Log Scale) ---\n")
cat(sprintf("MAE: %.2f\n", mae_cpglm))
cat(sprintf("RMSE: %.2f\n", rmse_cpglm))
cat(sprintf("Spearman: %.4f\n", rank_corr))
cat(sprintf("Total Actual (log): %.2f\n", sum(test_loss$Cost_claims_year_log)))
cat(sprintf("Total Predicted (log): %.2f\n", sum(preds)))
cat(sprintf("Ratio: %.3f\n", sum(preds) / sum(test_loss$Cost_claims_year_log)))
cat(sprintf("Tweedie p: %.4f, phi: %.4f\n", model_cpglm_final$p, model_cpglm_final$phi))

# save predictions
tweedie_results <- data.frame(
  unique_row_id = test_loss_unique_ids,
  ID = test_loss_ids,
  actual_cost_log = test_loss$Cost_claims_year_log,
  pred_tweedie_log = preds
)

write.csv(tweedie_results, "tweedie_predictions.csv", row.names = FALSE)

# ---- SEVERITY MODELLING ----

# suggestion for using the split
train_severity <- train_global %>%
  filter(N_claims_year > 0) %>%
  mutate(
    Cost_claims_year = expm1(Cost_claims_year_log),
    Claim_Severity = Cost_claims_year / N_claims_year
  )

test_severity <- test_global %>%
  filter(N_claims_year > 0) %>%
  mutate(
    Cost_claims_year = expm1(Cost_claims_year_log),
    Claim_Severity = Cost_claims_year / N_claims_year
  )

# prep data
forbidden_cols_severity <- c(
  "Cost_claims_year", "Cost_claims_year_log", "Lapse", "Date_lapse",
  "Payment", "Premium", "Has_Claimed_In_History", "Contract_Duration",
  "exposure_years_actual", "claim_rate_annual", "exposure_years"
)

train_data <- train_severity %>%
  dplyr::select(-all_of(forbidden_cols_severity)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

test_data <- test_severity %>%
  dplyr::select(-all_of(forbidden_cols_severity)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

test_severity_ids <- test_data$ID # this should be used to assign ID at the end, which can be used for mapping
test_severity_unique_ids <- test_data$unique_row_id  # ← ADD THIS
# Remove ID and unique_row_id from modeling data
train_data <- train_data %>% dplyr::select(-ID, -unique_row_id)
test_data_modeling <- test_data %>% dplyr::select(-ID, -unique_row_id)



#linear regression


m_lm <- lm(log(Claim_Severity+ 1 )~  Seniority + Policies_in_force +
             Type_risk + Area + Second_driver +
             Year_matriculation + Value_vehicle +
             Type_fuel + Length  +
             + Driving_Experience + 
             + Product_Holding_Ratio ,
           
           data = train_data)
summary(m_lm)
vif_values <- vif(m_lm)
vif_values
#random forest

m_rf <- ranger(
  log(Claim_Severity + 1) ~  Seniority + Policies_in_force +
    Year_matriculation + Value_vehicle + N_doors +
    Length + missing_Length +
    Driver_Age + Driving_Experience + 
    Power_to_Weight_Ratio + Product_Holding_Ratio +
    Weight_log + Cylinder_capacity_log + Power_log,
  data = train_data,
  num.trees = 500,
  importance = "impurity"
)
importance(m_rf)

#xgboost

train_label_raw <- train_data$Claim_Severity
test_label_raw  <- test_data_modeling$Claim_Severity

train_prep <- dplyr::select(train_data, -dplyr::any_of(c("Claim_Severity","Claim_Severity_log","N_claims_year","Max_products","Area","Distribution_channel","Vehicle_Age")))
test_prep  <- dplyr::select(test_data_modeling,  -dplyr::any_of(c("Claim_Severity","N_claims_year","Claim_Severity_log","Max_products","Area","Distribution_channel","Vehicle_Age")))

combined_features <- rbind(train_prep, test_prep)
combined_matrix <- model.matrix(~ . -1, data = combined_features)

train_matrix <- combined_matrix[1:nrow(train_prep), ]
test_matrix  <- combined_matrix[(nrow(train_prep)+1):nrow(combined_matrix), ]

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label_raw, weight = train_data$N_claims_year)
dtest  <- xgb.DMatrix(data = test_matrix)

set.seed(123)

param_grid <- data.frame(
  eta = runif(20, 0.01, 0.05),
  max_depth = sample(3:6, 20, replace = TRUE),
  min_child_weight = sample(c(1, 5, 10), 20, replace = TRUE),
  subsample = runif(20, 0.7, 1.0),
  colsample_bytree = runif(20, 0.7, 1.0),
  tweedie_variance_power = runif(20, 1.1, 1.6),
  gamma = sample(c(0, 1, 3), 20, replace = TRUE),
  lambda = runif(20, 1, 8)
)



best_score <- Inf
best_param <- NULL
best_iter <- NULL

for (i in 1:nrow(param_grid)) {
  
  params <- list(
    objective = "reg:tweedie",
    tweedie_variance_power = param_grid$tweedie_variance_power[i],
    eta               = param_grid$eta[i],
    max_depth         = param_grid$max_depth[i],
    min_child_weight  = param_grid$min_child_weight[i],
    subsample         = param_grid$subsample[i],
    colsample_bytree  = param_grid$colsample_bytree[i],
    gamma             = param_grid$gamma[i],
    lambda            = param_grid$lambda[i],
    eval_metric       = "rmse",
    tree_method       = "hist",       
    grow_policy       = "lossguide"   
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 600,                  
    nfold = 3,                        
    early_stopping_rounds = 30,       
    verbose = 0
  )
  
  rmse_score <- cv$evaluation_log$test_rmse_mean[cv$best_iteration]
  
  if (rmse_score < best_score) {
    best_score <- rmse_score
    best_param <- params
    best_iter  <- cv$best_iteration
  }
}

best_param
best_score
best_iter


m_xgb <- xgb.train(
  params = best_param,
  data = dtrain,
  nrounds = best_iter,
  verbose = 1
)

#gamlss

numeric_vars <- c("Seniority", "Driving_Experience", "Policies_in_force",
                  "Year_matriculation", "Value_vehicle", "Length", "Product_Holding_Ratio")

train_scaled <- train_data
train_scaled[numeric_vars] <- scale(train_data[numeric_vars])


m_gamlss_stable <- gamlss(
  log(Claim_Severity + 1)  ~ 
    pb(Seniority, df = 2,inter = 10) +
    pb(Driving_Experience,df = 2, inter =10) +
    Policies_in_force +
    Type_risk +
    Area +
    Second_driver +
    Year_matriculation +
    Value_vehicle +
    Type_fuel +
    Product_Holding_Ratio,
  
  sigma.formula = ~ 
    pb(Seniority, df = 1,inter = 5) +pb(Driving_Experience,df = 2, inter =10),
  
  
  family = IG,   
  data = train_scaled,
  trace = TRUE,
  
)

train_means <- attr(scale(train_data[numeric_vars]), "scaled:center")
train_sds   <- attr(scale(train_data[numeric_vars]), "scaled:scale")
test_scaled <- test_data_modeling
test_scaled[numeric_vars] <- sweep(test_scaled[numeric_vars], 2, train_means, "-")
test_scaled[numeric_vars] <- sweep(test_scaled[numeric_vars], 2, train_sds, "/")





#accuracy metrics

y_true <- test_data_modeling$Claim_Severity

pred_lm_log <- predict(m_lm, newdata = test_data_modeling)
pred_lm <- exp(pred_lm_log) - 1

pred_rf_log <- predict(m_rf, data = test_data_modeling)$predictions
pred_rf <- exp(pred_rf_log) - 1


pred_xgb <- predict(m_xgb, dtest)

pred_gamlss_log <- predict(m_gamlss_stable, newdata = test_scaled, type = "response")
pred_gamlss <- exp(pred_gamlss_log) - 1


compute_metrics <- function(y_true, y_pred) {
  y_pred <- pmax(y_pred, 0) 
  
  mae  <- mean(abs(y_pred - y_true))
  rmse <- sqrt(mean((y_pred - y_true)^2))
  
  mape <- mean(abs((y_pred - y_true) / (y_true + 1e-6))) * 100
  
  return(c(MAE = mae, RMSE = rmse, MAPE = mape))
}

results <- data.frame(
  Model = c("Linear", "RandomForest", "XGBoost", "GAMLSS"),
  rbind(
    compute_metrics(y_true, pred_lm),
    compute_metrics(y_true, pred_rf),
    compute_metrics(y_true, pred_xgb),
    compute_metrics(y_true, pred_gamlss)
  )
)

results <- results %>% mutate(across(where(is.numeric), round, 2))
print(results)

#visualization

# Combine actual and predicted values into a long format
pred_df <- data.frame(
  Actual = y_true,
  Linear = pred_lm,
  RandomForest = pred_rf,
  XGBoost = pred_xgb,
  GAMLSS = pred_gamlss
) %>%
  pivot_longer(
    cols = -Actual,
    names_to = "Model",
    values_to = "Predicted"
  )

# Scatter plot Actual vs Predicted
ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "steelblue") +           # semi-transparent points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # y=x reference
  facet_wrap(~ Model, scales = "free") +   
  theme_minimal() +
  labs(
    title = "Actual vs Predicted Claim Severity",
    x = "Actual Claim Severity",
    y = "Predicted Claim Severity"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#Scaled plots to observe visualization without outliers

ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "steelblue") + 
  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  
  facet_wrap(~ Model, scales = "free") + 
  
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  
  theme_minimal() +
  labs(
    title = "Actual vs Predicted Claim Severity (Log-Log Scale)",
    subtitle = "Diagonals indicate correlation; Horizontal lines indicate failure to predict variance",
    x = "Actual Claim Severity (Log Scale)",
    y = "Predicted Claim Severity (Log Scale)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# store results: 
if (FALSE) { # change to TRUE or remove when updated with needed values
severity_results <- data.frame(
  unique_row_id = test_severity_unique_ids,  # ← ADD THIS
  ID = test_severity_ids,
  
  actual_severity = y_true,  
  actual_n_claims = test_severity$N_claims_year,
  
  pred_lm_severity = pred_lm,
  pred_rf_severity = pred_rf,
  pred_xgb_severity = pred_xgb,
  pred_gamlss_severity = pred_gamlss
)

write.csv(severity_results, "severity_predictions.csv", row.names = FALSE)
}

frequency_results <- data.frame(
  # identifier for merging
  unique_row_id = test_unique_id, # since ID can be duplicated
  ID = test_id,
  
  # actual values
  actual_claims = test_freq_temp$N_claims_year,
  actual_exposure = test_exposure,
  actual_rate = test_rate_annual,
  
  # predictions (annual rates)
  pred_rf_rate = pred_rf_initial,
  pred_poisson_rate = pred_poisson,
  pred_zip_rate = pred_zip,
  
  # predictions (expected counts for actual exposure)
  pred_rf_count = pred_rf_initial * test_exposure,
  pred_poisson_count = pred_poisson * test_exposure,
  pred_zip_count = pred_zip * test_exposure
)
#Two step XGBoost model for severity

THRESHOLD <- 5000
train_data$Is_Large <- ifelse(train_data$Claim_Severity >= THRESHOLD, 1, 0)
test_data_modeling$Is_Large <- ifelse(test_data_modeling$Claim_Severity >= THRESHOLD, 1, 0)

train_prep <- dplyr::select(train_data, -dplyr::any_of(c("Claim_Severity","Is_Large","Claim_Severity_log","N_claims_year","Max_products","Area","Distribution_channel","Vehicle_Age")))
test_prep  <- dplyr::select(test_data_modeling,  -dplyr::any_of(c("Claim_Severity","Is_Large","N_claims_year","Claim_Severity_log","Max_products","Area","Distribution_channel","Vehicle_Age")))

combined_features <- rbind(train_prep, test_prep)
combined_matrix <- model.matrix(~ . -1, data = combined_features)

train_matrix <- combined_matrix[1:nrow(train_prep), ]
test_matrix  <- combined_matrix[(nrow(train_prep)+1):nrow(combined_matrix), ]

idx_small <- which(train_data$Claim_Severity < THRESHOLD)
idx_large <- which(train_data$Claim_Severity >= THRESHOLD)

dtrain_class <- xgb.DMatrix(data = train_matrix, label = train_data$Is_Large)

dtrain_small <- xgb.DMatrix(
  data = train_matrix[idx_small, ], 
  label = train_data$Claim_Severity[idx_small],
  weight = train_data$N_claims_year[idx_small]
)

dtrain_large <- xgb.DMatrix(
  data = train_matrix[idx_large, ], 
  label = train_data$Claim_Severity[idx_large],
  weight = train_data$N_claims_year[idx_large]
)

dtest <- xgb.DMatrix(data = test_matrix)

params_class <- list(
  eval_metric = "auc",
  eta = 0.05,
  max_depth = 4
)

m_class <- xgb.train(params = params_class, data = dtrain_class, nrounds = 200)

params_small <- list(
  objective = "reg:gamma", 
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 4
)

m_small <- xgb.train(params = params_small, data = dtrain_small, nrounds = 300)

params_large <- list(
  objective = "reg:tweedie", 
  tweedie_variance_power = 1.5,
  eval_metric = "rmse",
  eta = 0.03, 
  max_depth = 3
)

m_large <- xgb.train(params = params_large, data = dtrain_large, nrounds = 300)

cat("Calculating Final Mixture Predictions...\n")
prob_large <- predict(m_class, dtest)
prob_small <- 1 - prob_large
pred_small_raw <- predict(m_small, dtest)
pred_large_raw <- predict(m_large, dtest)
final_preds <- (prob_small * pred_small_raw) + (prob_large * pred_large_raw)
actuals <- test_data_modeling$Claim_Severity
rmse_val <- sqrt(mean((actuals - final_preds)^2))
mae_val <- mean(abs(actuals - final_preds))
cat(sprintf("RMSE: $%.2f\n", rmse_val))
cat(sprintf("MAE:  $%.2f\n", mae_val))
cat(sprintf("\nTotal Actual:    $%.2f\n", sum(actuals)))
cat(sprintf("Total Predicted: $%.2f\n", sum(final_preds)))



#loss comparison actual vs predicted
# Merge frequency and severity results
combined_results <- merge(
  frequency_results,
  severity_results,
  by = c("unique_row_id", "ID"),
  all = FALSE
)

# Apply rules for predicted pure premium
combined_results$pred_rf_count_adj <- ifelse(
  combined_results$pred_rf_count > 0,
  combined_results$pred_rf_count,
  0
)

# Predicted total claim cost (pure premium)
combined_results$pred_total_cost <- combined_results$pred_rf_count_adj *
  combined_results$pred_gamlss_severity

# Actual total claim cost
combined_results$actual_total_cost <- ifelse(
  combined_results$actual_n_claims > 0,
  combined_results$actual_severity * combined_results$actual_n_claims,
  0
)

#scatter plot

ggplot(combined_results, aes(x = actual_total_cost, y = pred_total_cost)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Actual vs Predicted Total Losses",
    x = "Actual Total Cost",
    y = "Predicted Total Cost"
  ) +
  theme_minimal()

#calibration plot
calibration_df <- combined_results %>%
  mutate(pred_bin = ntile(pred_total_cost, 10)) %>%
  group_by(pred_bin) %>%
  summarise(
    mean_pred = mean(pred_total_cost),
    mean_actual = mean(actual_total_cost)
  )

ggplot(calibration_df, aes(x = mean_pred, y = mean_actual)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Calibration Plot (Deciles)",
    x = "Average Predicted Loss",
    y = "Average Actual Loss"
  ) +
  theme_minimal()

#distribution of actual vs predicted losses

combined_long <- reshape2::melt(
  combined_results[, c("actual_total_cost", "pred_total_cost")],
  variable.name = "type",
  value.name = "loss"
)

ggplot(combined_long, aes(x = loss, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Actual vs Predicted Losses",
    x = "Loss Amount",
    y = "Count"
  ) +
  theme_minimal()

#metrics



actual <- combined_results$actual_total_cost
predicted <- combined_results$pred_total_cost

total_actual <- sum(actual)
total_pred <- sum(predicted)

portfolio_error <- total_pred - total_actual
portfolio_pct_error <- portfolio_error / total_actual * 100

cat("Portfolio-level metrics:\n")
cat("Total Actual Loss:", total_actual, "\n")
cat("Total Predicted Loss:", total_pred, "\n")
cat("Absolute Error:", portfolio_error, "\n")
cat("Percentage Error:", round(portfolio_pct_error, 2), "%\n\n")

decile_lift <- combined_results %>%
  mutate(decile = ntile(pred_total_cost, 10)) %>%
  group_by(decile) %>%
  summarise(
    actual_total = sum(actual_total_cost),
    predicted_total = sum(pred_total_cost)
  ) %>%
  mutate(lift = predicted_total / actual_total)

cat("Decile lift table:\n")
print(decile_lift)

ggplot(decile_lift, aes(x = factor(decile), y = lift)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Predicted Decile", y = "Predicted / Actual Total Loss", 
       title = "Decile Lift: Predicted vs Actual Total Loss") +
  theme_minimal()

df <- combined_results %>% arrange(desc(pred_total_cost))
df$cum_actual <- cumsum(df$actual_total_cost) / sum(df$actual_total_cost)
df$cum_pred <- cumsum(df$pred_total_cost) / sum(df$pred_total_cost)
df$cum_policy <- seq_along(df$pred_total_cost) / nrow(df)

ggplot(df) +
  geom_line(aes(x = cum_policy, y = cum_actual), color = "blue", size = 1) +
  geom_line(aes(x = cum_policy, y = cum_pred), color = "red", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(
    x = "Cumulative % of Policies",
    y = "Cumulative % of Total Claims",
    title = "Lorenz Curve: Actual vs Predicted Claims"
  ) +
  theme_minimal()


msle_val <- mean((log1p(predicted) - log1p(actual))^2)

mae_val <- mean(abs(actual - predicted))
rmse_val <- sqrt(mean((actual - predicted)^2))

cat("\nAlternative metrics:\n")
cat("MSLE:", round(msle_val, 4), "\n")
cat("MAE:", round(mae_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")











