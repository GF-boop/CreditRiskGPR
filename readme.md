# Country-Specific Macroeconomic Risk Modeling

This project provides a robust and automated framework for modeling a country-specific corporate risk factor (the "Z-factor") using lagged macroeconomic variables. The script identifies the best predictive model for each European country from a set of potential covariates, leveraging parallel processing for efficiency and generating detailed diagnostic reports for each successful model.

## Key Features

-   **Automated Z-Factor Estimation**: Calculates a systemic risk factor (Z-factor) from corporate default risk time series data for each country.
-   **Dynamic Lag Generation**: Automatically creates lagged versions of all macroeconomic variables to capture time-delayed effects.
-   **Robust Model Selection**:
    -   Tests thousands of variable combinations to find the optimal model.
    -   Filters models based on the statistical significance of all predictors (p-value threshold).
    -   Selects the final model based on its **out-of-sample predictive power (minimum RMSE)**, ensuring the model generalizes well to new data.
-   **Parallel Processing**: Uses R's `parallel` package to significantly speed up computations by modeling multiple countries simultaneously.
-   **Automated Report Generation**: For each country with a valid model, automatically generates a summary text file and a diagnostic plot.
-   **Flexible & Maintainable**: Handles inconsistent country naming conventions through a simple mapping file and is designed to be easily adaptable to new data or countries.

## Directory Structure

The repository is organized as follows:

```
.
├── data/
│   ├── data_risk_EBA.csv
│   └── data_macro_statio.csv
├── R/
│   └── main_analysis.R
├── outputs/
│   ├── Austria/
│   │   ├── model_summary.txt
│   │   └── Z_factor_fit_diagnostic.png
│   └── ... (a folder for each country)
└── README.md
```

-   **`data/`**: Should contain the input data files.
    -   `data_risk_EBA.csv`: Time series data of corporate default risk for each country.
    -   `data_macro_statio.csv`: Time series data of stationary macroeconomic variables.
-   **`R/`**: Contains the main R analysis script(s).
    -   `main_analysis.R`: The main script that executes the entire workflow.
-   **`outputs/`**: The target directory for all generated results. The script will automatically create this folder and a sub-folder for each country.

## Prerequisites

To run this project, you will need:

1.  **R**: Version 4.0 or higher is recommended.
2.  **RStudio**: Recommended for ease of use.
3.  The following R packages:
    -   `rstudioapi`
    -   `dplyr`
    -   `parallel`
    -   `lubridate`

You can install all required packages by running the following command in your R console:
```r
install.packages(c("rstudioapi", "dplyr", "parallel", "lubridate"))
```

## How to Use

1.  **Clone the Repository**:
    ```bash
    git clone <your-repository-url>
    ```
2.  **Add Data**: Place your `data_risk_EBA.csv` and `data_macro_statio.csv` files into the `data/` directory.
3.  **Open the Project**: Open the main R script (e.g., `R/main_analysis.R`) in RStudio. The script is configured to automatically set the correct working directory.
4.  **Run the Script**: Execute the entire script. You can do this by clicking the "Source" button in RStudio or by running `source("R/main_analysis.R")` in the console.
5.  **Check Progress**: The script will print its progress to the console, indicating which phase it is in and which country it is processing.
6.  **Find Results**: Once the script completes, all results will be organized by country inside the `outputs/` directory.

## Methodology Workflow

The script follows a two-phase architecture to ensure stability and prevent file-writing conflicts during parallel execution.

#### Phase 0: Data Preparation

Before modeling, the script performs several key preparation steps:
1.  **Z-Factor Calculation**: For each country, the `f_Z_estimation` function is used to derive a latent systemic risk factor from the input risk data.
2.  **Date Standardization**: All date formats are harmonized to ensure correct merging between different data sources.
3.  **Lag Generation**: A comprehensive set of lagged predictors is created automatically for all macroeconomic variables.

#### Phase 1: Parallel Modeling

The core of the analysis runs in parallel to save time:
-   The list of countries is split among the available CPU cores.
-   Each core independently executes the `compute_model_for_country` function for its assigned countries.
-   For each country, the function searches for the best model by:
    1.  Generating all possible combinations of predictor variables (up to a specified maximum).
    2.  Fitting an `lm` model for each combination on the training dataset.
    3.  Discarding any model that has statistically insignificant predictors (p-value > 0.05).
    4.  Calculating the out-of-sample Root Mean Squared Error (RMSE) on the test dataset for all remaining valid models.
    5.  **Selecting the model with the lowest OOS RMSE** as the best performer.
-   The complete result (the model object, performance metrics, and data used) is returned to the main process.

#### Phase 2: Sequential Reporting

Once all parallel computations are finished, the main process takes over again:
-   It iterates sequentially through the list of results received from the workers.
-   For each result object:
    -   If the modeling was successful, it creates the country's output directory.
    -   It writes a `model_summary.txt` file containing the model's details, coefficients, and performance metrics (AIC and OOS RMSE).
    -   It generates a `Z_factor_fit_diagnostic.png` plot showing the observed Z-factor, the in-sample fit, and the out-of-sample prediction.
    -   If the modeling failed for any reason (e.g., no significant model found), it prints a status message to the console and moves to the next country.

## Output Description

For each successfully modeled country, you will find two files in its output sub-folder:

1.  **`model_summary.txt`**: A text file containing:
    -   The selection criterion used (OOS RMSE).
    -   The final RMSE value on the test set.
    -   The AIC of the final model (for reference).
    -   The full `summary()` output of the final `lm` model object, including coefficients, standard errors, and p-values.

2.  **`Z_factor_fit_diagnostic.png`**: A plot visualizing the model's performance:
    -   **Grey Line**: The actual, observed Z-factor over time.
    -   **Green Line**: The model's fit on the training data.
    -   **Red Line**: The model's out-of-sample prediction on the test data.
    -   **Blue Dashed Line**: The separation point between the training and test sets.

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
