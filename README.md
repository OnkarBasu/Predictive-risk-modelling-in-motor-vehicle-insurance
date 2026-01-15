# Predictive-risk-modelling-in-motor-vehicle-insurance

# 🚢 Vessel ETA Prediction Using AIS Data

## 📌 Project Overview
Predicting accurate vessel arrival times is challenging due to complex maritime movement patterns and varying environmental and operational factors. This project uses **Automatic Identification System (AIS) data** and machine learning techniques to predict **Estimated Time of Arrival (ETA)** with high accuracy.

The pipeline combines **data preprocessing, advanced feature engineering, exploratory data analysis (EDA)**, and a **Random Forest Regressor** to capture vessel behavior, spatial movement, and temporal trends.

---

## 🎯 Objectives
- Predict vessel ETA using historical AIS data  
- Identify key factors influencing vessel arrival times  
- Improve maritime logistics planning and operational efficiency  

---

## 🧠 Solution Architecture
1. AIS data cleaning and preprocessing  
2. Feature engineering (temporal, spatial, behavioral)  
3. Exploratory Data Analysis (EDA)  
4. Model training using Random Forest Regressor  
5. Model evaluation and insight generation  

---

## 📊 Dataset Description

### AIS Dataset Overview
The dataset contains real-world AIS signals with the following attributes:

- Timestamp  
- Vessel Identifier (MMSI)  
- Latitude & Longitude  
- Speed Over Ground (SOG)  
- Course Over Ground (COG)  
- Rate of Turn (ROT)  
- Navigational Status  
- Heading  
- Vessel Dimensions (Length, Width, Draught)  
- Destination  
- Estimated Time of Arrival (ETA)  

---

## 🧹 Data Preprocessing

### Handling Missing Values
- Filled missing numerical values (ROT, COG, Heading, Draught, Width, Length) using **mean or median**
- Replaced missing vessel names with `"Unknown"`
- Imputed missing ETA values using the **most frequent value**
- Dropped records missing **critical navigational data** to ensure model reliability

---

## ⚙️ Feature Engineering

### Time-Based Features
- Hour of day  
- Day of week  
- Month  

### Behavioral Features
- Acceleration  
- Change in heading  
- Turning status  
- Stationary status  

### Spatial Features
- Distance traveled (Haversine formula)  
- Bearing between consecutive AIS positions  
- Previous latitude and longitude  

### Vessel Characteristics
- Width-to-length ratio  
- Draught-to-length ratio  

### Anomaly Indicators
- Abnormal speed detection (±3 standard deviations)  
- Suspicious stops (stationary vessels marked as *"Under way using engine"*)  

### Geographic Segmentation
- KMeans clustering on latitude and longitude to define maritime zones  

---

## 📈 Exploratory Data Analysis (EDA)

### Correlation Analysis
#### Strong Positive Correlations
- Heading & COG (**0.81**)  
- Width & Length (**0.79**)  
- Draught & Length (**0.75**)  
- Width & Draught (**0.58**)  

#### Strong Negative Correlations
- Draught-to-Length Ratio & Length (**-0.73**)  
- Draught-to-Length Ratio & Width (**-0.58**)  

#### Weak or No Correlation
- Speed Over Ground (SOG)  
- Acceleration  
- Change in heading  

---

## 📊 Distribution Analysis

### Speed Over Ground (SOG)
- Bimodal distribution:
  - 0 knots → stationary vessels
  - 12–15 knots → cruising speed
- Few vessels operate between 2–10 knots
- Minor high-speed outliers (>17 knots)

### Course Over Ground (COG)
- Peaks around **0–50°** and **200–250°**
- Indicates dominant navigation routes

### Rate of Turn (ROT)
- Highly skewed toward **0**
- Few extreme values indicating sharp maneuvers

---

## ⏱️ Temporal Analysis

### Ship Activity by Hour
- Peak activity: **00:00 – 04:00**
- Lowest activity: **18:00 – 21:00**

### Ship Activity by Day of Week
- Highest activity: **Midweek (Wednesday)**
- Lowest activity: **Weekends**

### Ship Activity by Month
- Peak activity observed in **March**
- Decline after peak season

---

## 🔥 Activity Heatmap Insights
- Highest traffic on **Wednesdays**
- Peaks during **2–4 AM** and **12–2 PM**
- Reduced activity on **Mondays and Sundays**
- Late-night navigation helps avoid congestion

---

## 🚀 Average Ship Speed Analysis
- Fastest speeds during:
  - Early morning (4–6 AM)
  - Midday (12–4 PM)
- Speed drops between **6–8 PM**
- Speeds recover after **9 PM**

---

## 🚨 Anomaly Detection

### Identified Anomalies
- Suspicious stops: ~48,000 cases  
- Abnormal turns: Rare  
- Abnormal speeds: Minimal  

### Recommendations
- Investigate suspicious stops for security or congestion issues
- Monitor abnormal turns in high-risk maritime zones

---

## 📉 Historical Trends & Forecast

### Historical Traffic Trends
- Stable traffic (~87,000 vessels) from Jan–early Mar 2024
- Temporary spike (~92,500 vessels) in mid-March
- Return to normal levels in April

### 30-Day Traffic Forecast
- Predicted stabilization around **88,000 vessels**
- No major future surges expected

---

## 🤖 Modeling & Results

### Model Used
- Random Forest Regressor

### Performance Metrics
- **87% ETA prediction accuracy**

### Impact
- Improved ETA reliability
- Identified key arrival-time drivers
- Enhanced maritime logistics planning

---

## 🛠️ Technologies Used
- Python  
- Pandas, NumPy  
- Scikit-learn  
- Matplotlib, Seaborn  
- Geospatial analysis (Haversine formula)

---

## 📌 Key Takeaways
- Maritime traffic follows strong temporal and spatial patterns
- Vessel size and navigation behavior significantly affect ETA
- Feature-rich AIS data enables accurate ETA prediction
- Insights support smarter port and shipping operations
