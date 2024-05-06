# Quality-of-Life-Prediction

Dataset link: https://catalog.data.gov/dataset/final-report-of-the-asian-american-quality-of-life-aaqol

## Introduction
This project investigates the Quality of Life (QoL) determinants among Asian Americans, a diverse group impacted by various socioeconomic and cultural factors. The study focuses on understanding the unique challenges faced by this group, such as linguistic barriers, cultural stigmatization regarding mental health, and disparities in healthcare access. Advanced regression methods are applied to a comprehensive dataset to predict QoL, providing insights that could support targeted interventions.

## Data Collection and Preparation
Data was sourced from a detailed U.S. Census survey, capturing demographics, health, and happiness metrics among Asian Americans. It comprises 31 columns and 2,609 rows. The data preparation involved cleaning, encoding categorical variables, scaling continuous variables, and dealing with missing values to ensure it was suitable for regression analysis.

## Methodology
The study employed various regression models including Linear Regression, Support Vector Regression (SVR), Decision Trees, Random Forest, Lasso, and Ridge Regression. The models were evaluated using Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE), with Random Forest performing best in terms of MAE.

## Key Findings
- Random Forest emerged as the most effective model, demonstrating the lowest MAE, indicating its predictions are most consistent with actual QoL outcomes.
- Significant predictors identified include age, income, mental health status, familiarity with American and ethnic cultures, and satisfaction with life, among others.
- A comparative analysis of different models showed that while Lasso Regression minimized larger errors effectively, Random Forest provided the most reliable predictions across various tests.
  <img width="192" alt="Comparison Table" src="https://github.com/PavanTejaSadem/Quality-of-Life-Prediction/assets/159733657/1bbf9557-07e9-4272-9374-eeec60fb06df">

R-Shiny Application Link: https://pavanteja.shinyapps.io/Capstone/
## Conclusion
The project successfully leveraged multiple regression techniques to model and predict the Quality of Life among Asian Americans, highlighting the Random Forest model's suitability for this complex prediction task. This work not only enhances the understanding of factors influencing the well-being of Asian Americans but also showcases the application of predictive analytics in social and public health research. An R Shiny application was also developed, enabling interactive exploration of the impact of various predictors on Quality of Life.

## Practical Applications
The findings from this study can inform public health officials and policymakers in crafting more precise and effective interventions aimed at improving the Quality of Life among Asian American communities.
