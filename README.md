# Machine Learning Methodology: Modeling + Experimental Design

This repository extends a classic ML modeling workflow into an **experimentation framework** with A/B testing and statistical inference.

## Project Positioning

Original strengths (retained):
- Model training and tuning
- Model comparison
- Performance metrics and visualization

New layer (added):
- Hypothesis testing
- Confidence intervals
- Power analysis
- A/B testing simulation
- Experiment design workflow for decision validation

---

## Existing ML Work (Retained and Reframed)

### Practice 1: Tree vs Random Forest on German Credit

- Built a pruned decision tree and tuned random forest (`ntree = 1000`, CV-based tuning).
- Compared test error and ROC-AUC.
- Interpreted variable importance and model generalization.

Visualizations preserved:
- Pruned tree and split logic
- Random forest tuning/importance plot
- Dual ROC curve comparison

![Decision Tree](https://user-images.githubusercontent.com/61338647/170880971-42ca57d9-6d02-4146-b560-a29e97df9c0a.png)
![Random Forest](https://user-images.githubusercontent.com/61338647/170881126-2330d30e-3180-42cc-88a2-b9439a6a974c.png)
![ROC Comparison](https://user-images.githubusercontent.com/61338647/170881241-fd3e720a-194c-4170-8a4a-028697312e55.png)

### Practice 2: Non-linearly Separable Multi-class SVM Simulation

- Simulated 3-class nonlinear data.
- Trained SVM with linear, polynomial, and RBF kernels.
- Showed why nonlinear kernels outperform linear SVM for this geometry.

Visualizations preserved:
- Simulated class scatter plot
- Kernel tuning/decision boundary visual comparisons

![Three-class Simulation](https://user-images.githubusercontent.com/61338647/170881307-0b6210f9-2a01-4867-a81e-a50e24b691ae.png)
![SVM Comparison](https://user-images.githubusercontent.com/61338647/170881531-e071e91e-3a53-4c39-8ae3-35f0170783df.png)

### Practice 3: Repeated-Split Evaluation (kNN vs LDA)

- Repeated random split (10 times) with CV tuning.
- Collected AUC distribution for model robustness comparison.
- Used boxplots to compare stability and central tendency.

Visualization preserved:

![AUC Boxplot](https://user-images.githubusercontent.com/61338647/170881606-e8ae7c1a-a05a-4d7d-8caa-1e729f532e5f.png)

### Practice 4: User-defined K-fold Training Index Function

- Built custom K-fold index generation with class-balance control.
- Demonstrated fold-wise training index construction and validation.

---

## New: Experimental Evaluation Layer

### 1) A/B Testing Notebook

New file:
- `notebooks/ab_testing_experiment.ipynb`

It treats model comparison as an experiment:
- **Control**: Logistic Regression
- **Treatment**: Random Forest

Included statistical components:
1. Hypothesis testing (two-sample t-test)
2. Confidence intervals (95% CI)
3. Power analysis (required sample size)
4. Conversion A/B simulation (z-test, p-value)
5. Monte Carlo simulation for empirical detection rate

### 2) Conversion Simulation Script

New file:
- `simulate_user_conversion.py`

Command-line example:

```bash
python simulate_user_conversion.py \
  --control-rate 0.08 \
  --treatment-rate 0.10 \
  --control-n 5000 \
  --treatment-n 5000 \
  --simulations 2000
```

Outputs:
- Observed conversion lift
- z-statistic and one-sided p-value
- Wilson 95% confidence intervals
- Empirical power from repeated simulation

### 3) Experiment Design Playbook

New file:
- `experiment_design.md`

Provides a reusable checklist/template for:
- objective and hypothesis definition
- metric and guardrail planning
- sample size/power planning
- statistical analysis and ship/no-ship decision criteria

---

## Experiment Design Framework

1. Define hypothesis
2. Define metrics
3. Run experiment
4. Evaluate statistical significance
5. Measure confidence intervals
6. Validate sample size and power assumptions

---

## Repository Structure

```text
Machine_Learning_Methodology/
├── Practice_1.R
├── Practice_2.R
├── Practice_3.R
├── Practice_4.R
├── churn.csv
├── experiment_design.md
├── notebooks/
│   └── ab_testing_experiment.ipynb
├── simulate_user_conversion.py
└── README.md
```

---

## Environment Setup

### R workflow (existing)
- Main packages: `caret`, `rattle`, `pROC`, `ggplot2`, `e1071`, `simstudy`

### Python experimentation layer (new)

```bash
pip install numpy pandas scipy statsmodels matplotlib seaborn jupyter
```

---

## Why This Extension Matters

This project now demonstrates both:
- **Machine Learning modeling capability**
- **Experimentation and statistical decision capability**

Core capability coverage:
- Experimental Design
- A/B Testing
- Statistical Inference
- Confidence Intervals
- Power Analysis

This is especially useful for data, analytics, and AI-enablement roles where process changes must be validated with statistically defensible evidence.

## Interview Positioning (Company-neutral)

How to describe this project in interviews:
- Built an end-to-end ML evaluation workflow that combines model performance metrics with formal statistical testing.
- Added reusable experiment templates and simulation tools so teams can independently validate workflow changes.
- Framed model comparisons as controlled experiments (control vs treatment) and reported p-values, confidence intervals, and power.
- Emphasized decision quality: ship/no-ship recommendations based on effect size, uncertainty, and guardrail metrics.
