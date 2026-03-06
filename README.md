# Machine Learning Methodology: Modeling + Experimental Design

This repository presents a machine learning workflow extended with an experimentation layer for statistically defensible decision-making.

## Project Scope

Core ML capabilities:
- Model training and hyperparameter tuning
- Model comparison across algorithms
- Predictive performance diagnostics and visualization

Experimental analytics capabilities:
- Hypothesis testing
- Confidence intervals
- Power analysis
- A/B testing simulation
- Structured experiment design templates

---

## Credit Risk Modeling and Tree-based Benchmarking

Using German credit risk data, this module builds and evaluates interpretable and ensemble models under a consistent train/test and cross-validation setup.

What is covered:
- Pruned decision tree training and interpretation
- Random forest tuning with `ntree = 1000`
- Comparative error-rate and ROC-AUC evaluation
- Feature importance interpretation for model governance

Reference visualizations:

![Decision Tree](https://user-images.githubusercontent.com/61338647/170880971-42ca57d9-6d02-4146-b560-a29e97df9c0a.png)
![Random Forest](https://user-images.githubusercontent.com/61338647/170881126-2330d30e-3180-42cc-88a2-b9439a6a974c.png)
![ROC Comparison](https://user-images.githubusercontent.com/61338647/170881241-fd3e720a-194c-4170-8a4a-028697312e55.png)

## Nonlinear Multi-class Classification with Kernel SVM

This module simulates a non-linearly separable three-class dataset and evaluates linear, polynomial, and RBF kernels to demonstrate boundary flexibility vs. generalization.

What is covered:
- Synthetic nonlinear data generation
- 5-fold CV parameter tuning for each kernel
- Holdout-set comparison of predictive quality
- Decision-boundary behavior across kernel choices

Reference visualizations:

![Three-class Simulation](https://user-images.githubusercontent.com/61338647/170881307-0b6210f9-2a01-4867-a81e-a50e24b691ae.png)
![SVM Comparison](https://user-images.githubusercontent.com/61338647/170881531-e071e91e-3a53-4c39-8ae3-35f0170783df.png)

## Robustness Evaluation via Repeated-split AUC Analysis

This module compares kNN and LDA under repeated random train/test splits to evaluate variance, stability, and reliability in out-of-sample performance.

What is covered:
- Repeated 70/30 data partitions
- CV tuning for kNN over candidate `k`
- AUC collection across repeated runs
- Distribution-based model robustness comparison

Reference visualization:

![AUC Boxplot](https://user-images.githubusercontent.com/61338647/170881606-e8ae7c1a-a05a-4d7d-8caa-1e729f532e5f.png)

## Stratified K-fold Index Engineering

This module implements a custom function that generates class-balanced training indices for K-fold workflows, useful when teams need transparent control over fold construction.

What is covered:
- User-defined fold index generation
- Class-balance control by construction
- Reproducibility via explicit seed handling
- Fold-wise training index output for downstream pipelines

---

## Experimental Evaluation Layer (New)

### A/B Testing for Model Comparison

Notebook:
- `notebooks/ab_testing_experiment.ipynb`

Experiment setup:
- Control: Logistic Regression
- Treatment: Random Forest

Statistical components implemented:
1. Two-sample t-test (hypothesis testing)
2. 95% confidence intervals
3. Power analysis and sample-size estimation
4. Conversion-rate A/B simulation with z-test
5. Monte Carlo simulation of detection performance

### New Visualizations from Experimental Analysis

Model score distribution:

![A/B Accuracy Distribution](assets/ab_accuracy_distribution.png)

Mean performance with uncertainty bands:

![A/B Mean CI](assets/ab_mean_ci.png)

Simulated conversion lift distribution:

![A/B Conversion Lift](assets/ab_conversion_lift_distribution.png)

### Conversion Simulation Utility

Script:
- `simulate_user_conversion.py`

Example:

```bash
python simulate_user_conversion.py \
  --control-rate 0.08 \
  --treatment-rate 0.10 \
  --control-n 5000 \
  --treatment-n 5000 \
  --simulations 2000
```

Outputs:
- Observed lift
- z-statistic and one-sided p-value
- 95% confidence intervals
- Empirical power estimate from simulation

### Reusable Experiment Playbook

Template:
- `experiment_design.md`

Includes:
- Objective and hypothesis framing
- Primary + guardrail metric design
- Statistical test planning
- Power/sample-size planning
- Decision gates for rollout decisions

---

## Experiment Design Framework

1. Define hypothesis
2. Define metrics
3. Run experiment
4. Evaluate statistical significance
5. Quantify confidence intervals
6. Validate power and sample-size assumptions

---

## Repository Structure

```text
Machine_Learning_Methodology/
├── Practice_1.R
├── Practice_2.R
├── Practice_3.R
├── Practice_4.R
├── churn.csv
├── assets/
│   ├── ab_accuracy_distribution.png
│   ├── ab_mean_ci.png
│   └── ab_conversion_lift_distribution.png
├── experiment_design.md
├── notebooks/
│   └── ab_testing_experiment.ipynb
├── simulate_user_conversion.py
└── README.md
```

---

## Environment

R stack (existing scripts):
- `caret`, `rattle`, `pROC`, `ggplot2`, `e1071`, `simstudy`

Python stack (experimentation layer):

```bash
pip install numpy pandas scipy matplotlib seaborn jupyter
```

---

## Professional Relevance

This repository demonstrates the ability to combine machine learning evaluation with statistical decision frameworks, including:
- experimental design and A/B testing
- inference and uncertainty quantification
- repeatable analytical templates for team adoption
- evidence-based ship/no-ship evaluation logic
