# Scheme Statistical Analysis Project

## Overview

In this project, I developed a Scheme program to perform basic statistical analysis and linear regression on CSV data files. This was an opportunity to deepen my understanding of functional programming using Scheme, a language rooted in the Lambda Calculus, which emphasizes a declarative programming paradigm quite different from imperative or object-oriented styles.

## What I Built

I implemented a collection of Scheme functions that together enable reading CSV data, computing descriptive statistics, and performing simple linear regression analysis:

* **CSV Reader:** Extracts a specified column from a CSV file, supporting files both with and without header rows.
* **Mean Calculator:** Computes the average of a list of numerical values.
* **Standard Deviation Calculator:** Computes the standard deviation of a list of numerical values using the population formula.
* **Pearson Correlation Coefficient:** Calculates the correlation between two sets of numerical data.
* **Linear Regression Parameters:**

  * `regressiona`: Computes the slope (α) of the best-fit line for two data sets.
  * `regressionb`: Computes the intercept (β) of the best-fit line.
* **Regression Application:** Applies the derived linear regression model to a new list of input values, generating predicted outputs.

## Key Features

* **Functional Paradigm:** The entire program is written in Scheme, showcasing pure functional programming concepts.
* **Robust CSV Parsing:** Supports flexible handling of CSV files, enabling seamless data extraction.
* **Statistical Computations:** Implements foundational statistics essential for data analysis.
* **Model Application:** Beyond computing regression parameters, the program can apply the model to new datasets to predict outcomes.

## Example Usage

Given a CSV file `sat-gpa.csv` containing SAT scores and corresponding GPAs, my program can:

* Extract SAT and GPA columns as separate lists.
* Compute the mean and standard deviation for each.
* Calculate the linear regression line: $y = \alpha x + \beta$.
* Predict GPA values for new SAT scores based on the model.

This was tested interactively in MIT Scheme, ensuring correctness and reliability of the functions.

## Why This Matters

This project not only strengthened my functional programming skills but also provided practical experience with statistical methods and data processing — skills valuable in many software development, data science, and research roles.
