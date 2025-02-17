# Temperature and Humidity Analysis for Agricultural Production

This project conducts a comprehensive analysis of temperature and humidity data sourced from the open data portal of the Argentine National Meteorological Service (SMN) [https://datosabiertos.aeroterra.com/](https://datosabiertos.aeroterra.com/). Given that moderate temperatures and humidity levels are conducive to agricultural production, particularly for crops such as soybeans, corn, and wheat, we aim to explore whether this relationship is reflected in cultivated regions and farmland prices across different areas of Argentina.

It is the final assignment for the subject of Spatial Analysis from the MSc in Data Science. The authors are Paula Luvini and Florencia Ludueña.

## Repository Structure

```
├── Datos/                     # Raw and processed data
├── estimaciones/              # Statistical estimation scripts
├── Script/                    # Data processing and analysis scripts
├── .gitignore                 # Files and directories to be ignored by Git
├── README.md                  # This file
├── estadisticaespacial.Rproj   # R project file
```

## Methodology

1. **Data Collection**: Temperature and humidity data are retrieved from the SMN open data portal.
2. **Exploratory Data Analysis (EDA)**: Descriptive statistics and visualizations to understand the relationships between climate variables and agricultural land distribution.
3. **Spatial Analysis**: Examining geographic patterns in climate conditions and their correlation with agricultural land prices.
4. **Statistical Modeling**: Implementing estimation techniques to quantify the impact of climate variables on cultivated regions and farmland value.

## Requirements

This project primarily utilizes R for data analysis. Ensure you have the necessary packages installed before running the scripts.

## Results

- Identifies correlations between temperature, humidity, and agricultural land values.
- Provides spatial insights into how climatic conditions influence cultivation patterns.
- Offers a statistical foundation for understanding regional agricultural productivity.
