"""Simulate control vs treatment conversion and evaluate significance with z-test."""

from __future__ import annotations

import argparse
import numpy as np
from statsmodels.stats.proportion import proportions_ztest, proportion_confint


def run_simulation(
    control_rate: float,
    treatment_rate: float,
    control_n: int,
    treatment_n: int,
    simulations: int,
    seed: int,
) -> None:
    rng = np.random.default_rng(seed)

    control_conv = rng.binomial(control_n, control_rate)
    treatment_conv = rng.binomial(treatment_n, treatment_rate)

    counts = np.array([treatment_conv, control_conv])
    nobs = np.array([treatment_n, control_n])

    z_stat, p_value = proportions_ztest(count=counts, nobs=nobs, alternative="larger")

    obs_control_rate = control_conv / control_n
    obs_treatment_rate = treatment_conv / treatment_n
    lift = obs_treatment_rate - obs_control_rate

    control_ci = proportion_confint(control_conv, control_n, alpha=0.05, method="wilson")
    treatment_ci = proportion_confint(treatment_conv, treatment_n, alpha=0.05, method="wilson")

    sim_pvals = []
    sim_lifts = []
    for _ in range(simulations):
        c = rng.binomial(control_n, control_rate)
        t = rng.binomial(treatment_n, treatment_rate)
        z, p = proportions_ztest([t, c], [treatment_n, control_n], alternative="larger")
        sim_pvals.append(p)
        sim_lifts.append((t / treatment_n) - (c / control_n))

    empirical_power = float(np.mean(np.array(sim_pvals) < 0.05))

    print("=== A/B Conversion Experiment ===")
    print(f"Control:   {control_conv}/{control_n} = {obs_control_rate:.4%}")
    print(f"Treatment: {treatment_conv}/{treatment_n} = {obs_treatment_rate:.4%}")
    print(f"Observed lift (T - C): {lift:.4%}")
    print(f"z-statistic: {z_stat:.4f}")
    print(f"p-value (one-sided, treatment > control): {p_value:.6f}")
    print(f"Control 95% CI: ({control_ci[0]:.4%}, {control_ci[1]:.4%})")
    print(f"Treatment 95% CI: ({treatment_ci[0]:.4%}, {treatment_ci[1]:.4%})")
    print(f"Empirical power from {simulations} simulations: {empirical_power:.3f}")
    print(f"Average simulated lift: {np.mean(sim_lifts):.4%}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Simulate A/B conversion with z-test")
    parser.add_argument("--control-rate", type=float, default=0.08, help="Control conversion rate")
    parser.add_argument("--treatment-rate", type=float, default=0.10, help="Treatment conversion rate")
    parser.add_argument("--control-n", type=int, default=5000, help="Control sample size")
    parser.add_argument("--treatment-n", type=int, default=5000, help="Treatment sample size")
    parser.add_argument("--simulations", type=int, default=2000, help="Number of Monte Carlo runs")
    parser.add_argument("--seed", type=int, default=42, help="Random seed")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    run_simulation(
        control_rate=args.control_rate,
        treatment_rate=args.treatment_rate,
        control_n=args.control_n,
        treatment_n=args.treatment_n,
        simulations=args.simulations,
        seed=args.seed,
    )


if __name__ == "__main__":
    main()
