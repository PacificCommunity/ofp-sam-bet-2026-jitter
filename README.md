# BET 2026 Diagnostic model jitter

This public repository contains the report-ready jitter diagnostics and the
files needed to verify and reproduce the retained BET 2026 Diagnostic model
jitter fits.

Thirty jitter starts were attempted with CV = 0.1. Twenty-six native MFCL fits
completed, and the 25 fits with maximum gradient component (MGC) no greater
than \(1.0\times10^{-4}\) were retained. The exact retained seeds are:

```text
1 2 3 5 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 25 26 28 29 30
```

Only these 25 seeds have recovered final PAR files and are used by the
reproduction runner. The four incomplete runs and completed seed 6 remain in
the compact report payload only so the attempted/completed totals can be
audited.

## Included files

- `data/diagnostic/jitter/` contains the compact result payloads and the exact
  `jittered_out_<seed>.par` file for each retained seed.
- `data/diagnostic/mfcl/` contains one shared copy of native MFCL 2.2.7.9,
  `doitall.sh`, and the common BET input/configuration files. The public
  `bet.ini` itself contains the Diagnostic model value `sv(29) = 0.90`; the
  workflow keeps it fixed and audits it in every phase.
- `data/diagnostic/reproduction/` contains the fitted starting point, the
  independently recovered Phase-1 reference, and the exact 25-seed jitter
  plans.
- `data/diagnostic/native-par-validation.csv` records the native load and
  objective-parity check for every recovered PAR.

All data files are covered by `data/SHA256SUMS`. Rendering the report never
refits the model. All 25 retained final PAR files also contain `sv(29) = 0.90`
with age flag 162 fixed at zero.

## Verify the recovered PAR files

Run:

```sh
./verify-native-pars
```

The verifier checks the executable and input bundle, loads and evaluates every
retained PAR with native MFCL in an isolated temporary directory, compares its
objective with the compact result, and rewrites the deterministic validation
manifest. Native status 3 and the temporary `selblocks.dat` output are normal
for this zero-iteration validation call.

## Reproduce the 25 retained starts and fits

The runner uses the packages already installed in this immutable private
image; it never installs or updates a package at run time:

```text
ghcr.io/pacificcommunity/tuna-flow-private:v2.7@sha256:4fee4c40cb6439ff920b1dd233a84bf19d5cc0e37278c99ceff3fd79cb9c8852
mfclkit  0.0.0.9040 @ 44abaaa05692db7ae3e0ec0e52250c51714d1e50
FLR4MFCL 1.8.0       @ 5a29a9b3246bd19dcff350ded7e0e5099145da5e
```

Authenticate to GHCR and pull that exact digest once:

```sh
docker pull ghcr.io/pacificcommunity/tuna-flow-private:v2.7@sha256:4fee4c40cb6439ff920b1dd233a84bf19d5cc0e37278c99ceff3fd79cb9c8852
```

Then prepare and verify the Phase-1 baseline and all 25 jitter starting points
without running the long fits:

```sh
./run-reproduce-jitter --output /absolute/fresh/bet-jitter-prepare
```

To execute all 25 native fits as well:

```sh
./run-reproduce-jitter --run --output /absolute/fresh/bet-jitter-full
```

The full 25-fit mode is intentionally long and opt-in. Repository validation
uses the prepare-only path; the recovered final PAR files are checked
separately with a zero-iteration native MFCL evaluation.

The output path must be a new absolute directory outside this repository. The
repository is mounted read-only, network access is disabled, CV is fixed at
0.1, and both final optimisation phases use the \(1.0\times10^{-4}\)
convergence setting. The runner rejects any seed set other than the 25 listed
above and validates the generated plans, fitted parameters, objectives and
MGCs against the recovered references.

## Render the public report

With the report dependencies available, run:

```sh
./run-report
```

The self-contained HTML report and A4-ready PNG/PDF figures are written to
`results/`. The runner first verifies `data/SHA256SUMS`, the exact 30/26/25
accounting, the recovered native files and the public-data hygiene rules.
GitHub Actions renders with the same digest-pinned `tuna-flow-private:v2.7`
image.

## BET stock-status calculations

The compact endpoint payload preserves the three management quantities used
in the WCPFC bigeye tuna assessment for this model ending in 2024:

- `SBrecent/SBF=0`: mean 2021–2024 spawning biomass divided by mean 2014–2023
  no-fishing spawning biomass.
- `SBrecent/SBMSY`: mean 2021–2024 spawning biomass divided by the MFCL
  equilibrium-yield estimate of `SBMSY`.
- `Frecent/FMSY`: `1/Fmult`, using the 2020–2023 fishing-mortality pattern.

The validation script reconstructs these values from the embedded native
`MFCLPar` and `MFCLRep` objects and fails if the controls, windows or values
differ from the stored report data.
