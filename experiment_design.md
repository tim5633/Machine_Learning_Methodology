# Experiment Design Template

Use this template to evaluate model or product changes with statistical rigor.

## 1) Objective

- Business goal:
- Model/product change:
- Expected impact:

## 2) Hypothesis

- Null hypothesis (H0): New method does not improve the target metric.
- Alternative hypothesis (H1): New method improves the target metric.

Example for model evaluation:
- H0: Treatment model accuracy <= control model accuracy
- H1: Treatment model accuracy > control model accuracy

## 3) Metrics

Primary metric:
- e.g., Accuracy, AUC, conversion rate

Guardrail metrics:
- e.g., latency, false positive rate, churn

## 4) Experiment Setup

- Control group:
- Treatment group:
- Unit of randomization: user / session / sample
- Allocation ratio: 50/50 (or other)
- Experiment duration:
- Stopping rule:

## 5) Statistical Plan

- Test method:
  - Continuous metric: two-sample t-test
  - Binary metric: two-proportion z-test
- Significance level (alpha): 0.05
- Confidence interval: 95%
- Minimum detectable effect (MDE):

## 6) Sample Size and Power

- Target power: 0.80
- Required sample size per group:
- Assumptions used:

## 7) Analysis Checklist

1. Validate randomization and data quality
2. Compute group-level summary statistics
3. Run statistical test and obtain p-value
4. Report effect size and confidence interval
5. Evaluate guardrail metrics before shipping

## 8) Decision Rule

Ship treatment only if:
- p-value < alpha
- confidence interval supports practical lift
- no guardrail regression

## 9) Reporting Format

- Summary:
- Key numbers:
- Recommendation:
- Risks and follow-up:
