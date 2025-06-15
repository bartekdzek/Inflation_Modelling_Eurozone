# Inflation_Modelling_Eurozone - Econometric Model for Explaining Inflation Variability in the Eurozone

## Table of Contents
1.  [Project Description](#project-description)
2.  [Project Goal](#project-goal)
3.  [Data Used](#data-used)
4.  [Methodology](#methodology)
5.  [Model Structure](#model-structure)
6.  [Model Verification](#model-verification)
7.  [Results and Conclusions](#results-and-conclusions)
8.  [Running the Project](#running-the-project)
9.  [Authors](#authors)

## Project Description

This project is an assignment from the **Econometrics** course, aiming to create an econometric model based on time series data, explaining the quarterly variability of inflation in the Eurozone between 2005 and 2024. The project involves a comprehensive analysis of macroeconomic variables, data preparation, variable selection for the model, its construction, as well as detailed statistical and econometric verification. Additionally, an ex-post forecast was performed to assess the predictive capabilities of the constructed model.

## Project Goal

The main goal of the project was to build a stable and reliable econometric model that:
* Effectively explains the quarterly variability of the inflation rate in the Eurozone.
* Utilizes key macroeconomic variables influencing inflation.
* Meets the basic econometric assumptions regarding residuals and model parameters.
* Allows for inflation forecasting.

## Data Used

For the model construction, indicators from the TradingView platform were used, based on data provided by institutions such as Eurostat, the European Central Bank (ECB), ZEW, and TVC. The explanatory variables in the model are:
* **stopa_inflacji** (inflation_rate) – year-on-year inflation rate, expressed as a percentage change in the Consumer Price Index (CPI).
* **ceny_importowe** (import_prices) – index of prices of goods imported into the Eurozone.
* **ceny_producentow** (producer_prices) – year-on-year change in the Producer Price Index (PPI).
* **wzrost_pkb** (gdp_growth) – year-on-year change in Gross Domestic Product (GDP).
* **wzrost_kredytow** (credit_growth) – year-on-year growth rate of credits granted to the private sector.
* **stopa_bezrobocia** (unemployment_rate) – the share of the active population that is unemployed.
* **indeks_zew** (zew_index) – an economic sentiment index developed by the ZEW institute, based on surveys of financial analysts and market experts.
* **moc_produkcyjna** (production_capacity) – an indicator of industrial capacity utilization, expressed as a percentage of total production potential.
* **ropa_brent** (brent_oil_price) – price per barrel of Brent crude oil, determined based on EUR/USD exchange rates.

For the variables `wzrost_pkb` (gdp_growth), `wzrost_kredytow` (credit_growth), and `moc_produkcyjna` (production_capacity), a two-quarter lag was applied, while for other explanatory variables, a one-quarter lag was used. This approach allows for a better capture of cause-and-effect relationships in the analyzed context and its impact on the correctness of the built model.

## Methodology

The project is based on the methodology of building econometric models for time series, covering the following stages:
1.  **Variable Analysis:** Preliminary statistical and graphical analysis of variables, stationarity testing.
2.  **Data Preparation:** Data cleaning, transformations (if necessary), setting the analysis period.
3.  **Variable Selection and Model Construction:** Selection of explanatory variables based on economic rationale and statistical analysis (e.g., correlation matrix, significance tests). The Ordinary Least Squares (OLS) method was applied.
4.  **Model Verification:** Detailed testing of the assumptions of the classical linear regression model.
5.  **Model Presentation:** Presentation of the final model form and interpretation of parameters.
6.  **Ex-post Forecast:** Evaluation of the model's forecasting capabilities based on historical data.

## Model Structure

The final econometric model is a linear regression model that explains the quarterly inflation rate in the Eurozone depending on selected macroeconomic variables and their lags. The specific analytical form of the model and the selected variables are described in detail in the report (`Modelowanie_inflacji.pdf`).

## Model Verification

The model underwent a series of tests verifying its statistical and econometric correctness, including:
* **Residual Normality Test:** Checked the conformity of residuals with a normal distribution (e.g., Shapiro-Wilk test, residual histogram).
* **Autocorrelation Testing:** Verified the absence of residual autocorrelation (e.g., Durbin-Watson test, autocorrelation function).
* **Heteroskedasticity Testing:** Checked the homogeneity of residual variance (e.g., Breusch-Pagan test, visual analysis of residual plot).
* **Multicollinearity Testing:** Assessed the degree of multicollinearity among explanatory variables (e.g., VIF). The project indicated slight multicollinearity, deemed acceptable.
* **Parameter Stability Testing:** Checked whether model parameters are stable over time.
* **Analytical Form Stability Testing:** Verified whether the linear form of the model is appropriate.
* **Catalysis Effect Analysis:** Assessed the influence of explanatory variables on inflation variability.
* **Coincidence Analysis:** Confirmed that the model is coincident, meaning it accurately reflects general inflation trends.

## Results and Conclusions

The model accurately reflects general inflation trends – upward and downward phases, as well as turning points. Using the model, the dynamic inflation growth in 2021–2022 was correctly predicted. However, the ex-post forecast showed that in 2023–2024, the model consistently underestimated actual inflation levels. Despite moderate accuracy, especially during periods of prolonged impact of the war in Ukraine on commodity prices and supply chain disruptions, the model effectively identifies the main mechanisms shaping inflation and can serve as a basis for further analysis.

In summary:
* Model residuals show conformity with a normal distribution, absence of autocorrelation, and significant heteroskedasticity.
* There is slight multicollinearity among explanatory variables, which was considered acceptable.
* Model parameters are stable, and its analytical form (strictly linear) is correct.
* A coincident model was obtained, and the intensity of the catalysis effect was considered acceptable, especially in the context of the macroeconomic data used and the nature of the dependent variable.

## Running the Project

To run and reproduce the analysis, you will need an R environment.
1.  **Download the `Modelowanie_inflacji.Rmd` file.**

2.  **Install required R packages:**
    Open the `Modelowanie_inflacji.Rmd` file and check the list of packages in the ````{r setup, include=FALSE}`` section. These packages can be installed by running in the R console:
    ```R
    install.packages(c("readxl", "dplyr", "psych", "kableExtra", "ggplot2", "ggcorrplot", "tseries", "mbstats", "nortest", "lmtest", "forecast", "car", "showtext"))
    ```
3.  **Compile the R Markdown document:**
    Open the `Modelowanie_inflacji.Rmd` file in RStudio and use the "Knit" option (e.g., "Knit to PDF") to generate the report in PDF format. Make sure you have a LaTeX distribution installed (e.g., TinyTeX or MiKTeX/MacTeX), as the R Markdown document uses `xelatex` for PDF compilation.

## Authors

* Sebastian Szklarski
* Bartosz Tasak
