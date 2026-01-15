# Predictive-risk-modelling-in-motor-vehicle-insurance
# 🚗 Advanced Risk Modeling for Motor Vehicle Insurance

## 📌 Project Overview
This project investigates **advanced predictive risk modeling** techniques for motor vehicle insurance by comparing **two-stage frequency–severity frameworks** against a **single-stage Tweedie Generalized Linear Model (GLM)**.

Using a real-world **Spanish motor insurance dataset**, the study evaluates how different statistical and machine learning approaches estimate:
- Claim frequency  
- Claim severity  
- Aggregate expected financial loss  

The goal is to identify which modeling strategy best handles the **extreme skewness and rare catastrophic losses** characteristic of insurance data.

---

## 📊 Dataset Description
- **Policies:** 105,555 annual motor insurance policies  
- **Features:** 30 variables covering:
  - Policyholder demographics
  - Vehicle characteristics
  - Historical claims information
- **Target Variables:**
  - Claim count (frequency)
  - Claim cost (severity)
  - Total annual loss per policy

---

## 🧪 Data Characteristics & Feature Engineering

### Key Data Challenges
- **Severe Zero-Inflation:**  
  - 81% of policies have zero claims
- **Exposure Bias Risk:**  
  - Treating lapsed policies as fully exposed would cause a **3.8× underestimation of claim frequency**
- **Missing Data:**  
  - Vehicle *Length* missing in ~10% of records (mostly low-spec vehicles)

### Preprocessing & Transformations
- **Exposure Adjustment:** Corrected policy exposure to avoid frequency distortion
- **Missing Value Imputation:**  
  - Applied **missRanger** for vehicle length
- **Variance Stabilization:**  
  - Log transformations applied to:
    - Claim cost
    - Vehicle weight
    - Cylinder capacity
    - Engine power

---

## ⚙️ Modeling Frameworks Compared

### 1️⃣ Two-Stage Frequency–Severity Framework
- **Stage 1:** Predict number of claims per policy  
- **Stage 2:** Predict average cost per claim (conditional on at least one claim)  
- **Total Loss:** Frequency × Severity  

### 2️⃣ Single-Stage Tweedie Model
- Directly models **total annual loss**
- Assumes a **Compound Poisson–Gamma distribution**
- Simultaneously captures:
  - Claim occurrence
  - Claim magnitude

---

## 📈 Model Performance Results

## 1️⃣ Claim Frequency Modeling

| Model | MAE | RMSE | R² (OOB) / AIC |
|------|-----|------|---------------|
| Random Forest | **0.34** | **1.06** | **50.35%** |
| Poisson GLM | 0.41 | 1.20 | 88,451 |
| Zero-Inflated Poisson | 0.44 | 1.20 | 89,604 |

### Key Insights
- **Random Forest achieved the highest predictive accuracy**, improving MAE by **18%** over Poisson GLM
- Performed well for **99.8% of the portfolio**
- Systematically **underestimated extreme claim frequencies (>10)**, representing **0.16%** of policies

---

## 2️⃣ Claim Severity Modeling

Evaluated only on policies with ≥1 claim (**N = 4,900**).

| Model | MAE (€) | RMSE (€) | MAPE |
|-----|---------|----------|------|
| GAMLSS | **410.61** | **1791.96** | **146.89%** |
| Linear Regression | 410.68 | 1792.29 | 148.03% |
| Random Forest | 418.75 | 1791.66 | 169.70% |
| XGBoost (Tweedie) | 457.00 | 1761.39 | 323.85% |

### Key Insights
- **GAMLSS performed best**, explicitly modeling the **Inverse Gaussian distribution**
- All models struggled with **high-severity claims (> €5,000)** due to sparse tail data
- Machine learning models showed **higher variance and instability** in the upper tail

---

## 3️⃣ Single-Stage Tweedie Model Results

### Model Diagnostics
- **Tweedie Power Parameter (p):** 1.0454  
  - Confirms suitability of **Compound Poisson–Gamma** (1 < p < 2)
- **Spearman Rank Correlation (ρ):** 0.5545  
  - Indicates strong risk ranking capability

### Aggregate Portfolio Performance
- **Prediction / Actual Loss Ratio:** **0.975**
- Only **2.5% underestimation** at portfolio level

---

## 🧮 Final Comparative Evaluation

| Approach | Prediction / Actual Ratio | Interpretation |
|-------|--------------------------|---------------|
| **Tweedie (Single-Stage)** | **0.975** | Near-perfect portfolio calibration |
| Two-Stage (RF × GAMLSS) | 0.317 | Severe underestimation of rare losses |

---

## 📌 Key Findings & Takeaways

### 🏆 Best Model for Pricing
- **Single-stage Tweedie GLM** is the clear winner for **portfolio-level pricing**
- Robustly handles:
  - Zero inflation
  - Heavy-tailed severity
  - Rare catastrophic losses

### 🔍 Interpretability vs Accuracy Tradeoff
- **Two-stage models** offer clearer interpretation of:
  - Frequency drivers
  - Severity drivers
- However, they:
  - Accumulate error across stages
  - Severely underestimate extreme losses

### 💡 Technical Contributions
- Demonstrates the **critical importance of exposure adjustment**
- Shows why **tail-aware distributions** are essential in insurance modeling
- Highlights the limits of ML models when data is **sparse in the extreme tail**

---

## 🚀 Practical Implications
- **For Actuaries:** Tweedie models provide safer, more stable pricing
- **For Insurers:** Prevents capital underestimation from rare catastrophic events
- **For Data Scientists:** Shows where ML excels—and where classical statistics dominate

---

## 🛠️ Technologies & Tools Used
- **R Programming Language**
- **Data Manipulation & Wrangling**
  - `dplyr`, `tidyr`, `data.table` 
- **Data Visualization**
  - `ggplot2`, `gridExtra`, `patchwork`
- **Machine Learning & Statistical Modeling**
  - Random Forest (`randomForest`, `ranger`)
  - GAMLSS (Generalized Additive Models for Location, Scale, and Shape)
  - Tweedie Generalized Linear Models
  - XGBoost (Tweedie objective)
- **Missing Data Imputation**
  - `missRanger`
- **Actuarial & Statistical Techniques**
  - Statistical distribution modeling
  - Exposure-adjusted risk modeling



---

## 📍 Conclusion
While two-stage frequency–severity models deliver interpretability and strong performance for standard claims, they fail to capture the financial impact of rare, high-cost events. The **single-stage Tweedie GLM** emerges as the most reliable and financially sound approach for insurance portfolio risk modeling.



