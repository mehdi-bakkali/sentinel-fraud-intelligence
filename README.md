# Sentinel Fraud Intelligence

Sentinel is an educational credit card fraud analysis application built in R. It provides tools to inspect an imbalanced transaction dataset, compare classification models, and prioritize transactions for human review.

> **Scope:** This application is for education and demonstration only. It is not designed or validated for real payment decisions. Use synthetic or approved anonymized data only.

## Capabilities

- Interactive dashboard with transaction activity, class balance, and a prioritized review queue
- Synthetic demo mode that requires no downloaded dataset
- CSV upload and data profiling
- Comparison of logistic regression, decision tree, class-weighted random forest, and gradient boosting models
- Chronological train, validation, and test partitions
- Average precision, precision, recall, F1, ROC AUC, and confusion matrix reporting
- Validation-based alert threshold selection
- Hypothetical transaction scoring and downloadable model metrics

## Requirements and setup

Install R and the required packages:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "DT",
  "rpart", "ranger", "gbm", "scales"
))
```

From the repository root, launch the application:

```r
shiny::runApp()
```

The application opens with generated synthetic data. To train models, upload a compatible CSV and select **Model lab → Train model suite**. Training the complete model suite may take several minutes, depending on the dataset and computer.

## Input data

The application is designed for the commonly used European cardholder fraud dataset, typically distributed as `creditcard.csv`. It contains 284,807 transactions and 492 fraud examples. The target is `Class` (`1` = fraud, `0` = legitimate), with anonymized PCA features `V1`–`V28`, plus `Time` and `Amount`.

Input requirements:

- A binary target column named `Class` or `IsFraud`
- Numeric `Time` and `Amount` columns
- At least one additional numeric feature
- Both classes represented in the training, validation, and test periods

The dataset is not included. Obtain it from the source linked by the [DataFlair article](https://data-flair.training/blogs/data-science-machine-learning-project-credit-card-fraud-detection/). Keep downloaded datasets out of version control; CSV data files are excluded by `.gitignore`.

## Modeling and evaluation

Fraud is a rare event. Accuracy alone can obscure a model that misses most fraud cases, so Sentinel reports precision, recall, F1, ROC AUC, and average precision.

1. Transactions are sorted by `Time` when available; otherwise, the input order is preserved.
2. Data is partitioned chronologically into 70% training, 15% validation, and 15% final test data.
3. Missing-value imputation and scaling parameters are learned from training data only.
4. Models are ranked by validation average precision.
5. The alert threshold is selected on validation data, targeting at least 80% precision while maximizing recall. If the target cannot be met, the threshold with the best validation F1 is used.
6. Final test metrics are reported separately and are not used to select models or thresholds.

The 80% precision target is an illustrative setting, not a recommended operational standard. A real system would require requirements based on review capacity, the costs of false alerts and missed fraud, calibration, monitoring, and domain-specific validation.

## Model comparison

| Model | Purpose |
|---|---|
| Logistic regression | Classification baseline |
| Decision tree | Transparent rule-based comparison |
| Class-weighted random forest | Nonlinear ensemble comparison |
| Gradient boosting | Sequential tree ensemble comparison |

No benchmark results are stated here because results depend on the uploaded dataset and execution environment. Run the project and use its generated metrics when presenting results.


## Limitations and responsible use

- Synthetic demo scores are artificial prioritization signals, not trained model predictions.
- The public dataset is historical and may not represent current transaction patterns or other populations.
- The anonymized PCA features cannot explain activity in terms of merchant, device, customer behavior, or location. The application does not fabricate these explanations.
- Model scores are not calibrated probabilities of real-world fraud.
- Scores should be treated as review aids, not automatic approvals or declines.
- Do not upload personal or payment-card information, and do not commit sensitive data to GitHub.

## Inspiration and attribution

The project idea was inspired by DataFlair Team's [Data Science Project — Detect Credit Card Fraud with Machine Learning in R](https://data-flair.training/blogs/data-science-machine-learning-project-credit-card-fraud-detection/). DataFlair is credited for the inspiration. Sentinel's application design, code, and additional workflow are independent work.
