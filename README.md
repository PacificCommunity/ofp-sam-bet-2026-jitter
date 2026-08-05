# BET 2026 Diagnostic model jitter

This public repository contains the minimum compact inputs needed to recreate
the report-ready jitter diagnostics for the BET 2026 Diagnostic model.

It includes one compact model payload, 30 jitter result payloads, and compact
regional time series. The report applies the stated convergence rule to those
results: 25 fits meet the maximum gradient component threshold of
\(1.0\times10^{-4}\); four model runs did not complete and one completed fit
exceeds the threshold.

Original MFCL inputs and executables, raw report files, internal compute paths,
workflow identifiers, and credentials are deliberately excluded. No model is
refitted when this report is rendered.

## Render

Install `mfclshiny` at commit
`67a4dbafd170405e9d54383ef07b1e7790639da9`, then run:

```sh
./run-report
```

The self-contained HTML report and A4-ready PNG/PDF figures are written to
`results/`. The runner first verifies every compact payload against
`data/SHA256SUMS` and then checks the expected 30-run/25-fit inclusion set and
public-data hygiene rules.

GitHub Actions uses the same pinned `mfclshiny` commit and FLR4MFCL commit
`ff8367fcec19baff98333170c0f1bca3f9903029`, validates the compact payload,
and publishes the rendered files as a workflow artifact.
