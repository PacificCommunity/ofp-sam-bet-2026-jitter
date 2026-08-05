# BET 2026 Diagnostic model jitter

This public repository contains the minimum compact inputs needed to recreate
the report-ready jitter diagnostics for the BET 2026 Diagnostic model.

It includes one compact model payload, 30 jitter result payloads, and compact
regional and stock-status data under `data/diagnostic/`. The report applies
the stated convergence rule to those results: 25 fits meet the maximum gradient component threshold of
\(1.0\times10^{-4}\); four model runs did not complete and one completed fit
exceeds the threshold.

Original MFCL inputs and executables, raw report files, internal compute paths,
workflow identifiers, and credentials are deliberately excluded. No model is
refitted when this report is rendered.

## Render

Install `mfclshiny` at commit
`ada7dd0b08380cf000c93a3d127cf1bf0193bfbe`, then run:

```sh
./run-report
```

The self-contained HTML report and A4-ready PNG/PDF figures are written to
`results/`. The runner first verifies every compact payload against
`data/SHA256SUMS` and then checks the expected 30-run/25-fit inclusion set and
public-data hygiene rules.

## BET stock-status calculations

The compact endpoint payload preserves the three key management quantities
used in the WCPFC bigeye tuna assessment. For this model, which ends in 2024,
they are calculated as follows:

- `SBrecent/SBF=0`: mean spawning biomass for 2021–2024 divided by mean
  no-fishing spawning biomass for 2014–2023.
- `SBrecent/SBMSY`: mean spawning biomass for 2021–2024 divided by the MFCL
  equilibrium-yield estimate of `SBMSY`.
- `Frecent/FMSY`: `1/Fmult`, where `Fmult` is the MFCL multiplier that scales
  the 2020–2023 fishing-mortality pattern to `FMSY`.

These windows follow the model controls: the native four-year and ten-year
spawning-biomass windows, and 20 quarterly fishing-mortality periods with the
terminal four quarters omitted. The report uses the completed model outputs;
it does not approximate these endpoints by averaging the plotted annual
ratios. The validation script reconstructs the unjittered values from the
embedded native `MFCLPar` and `MFCLRep` objects and fails if either the controls,
windows or calculated endpoints differ from the stored report values.

GitHub Actions uses the same pinned `mfclshiny` commit and FLR4MFCL commit
`ff8367fcec19baff98333170c0f1bca3f9903029`, validates the compact payload,
and publishes the rendered files as a workflow artifact.
