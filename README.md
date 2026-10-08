[![Preservation checks](https://github.com/PacificCommunity/ofp-sam-bet-2026-jitter/actions/workflows/verify-preserved-results.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-jitter/actions/workflows/verify-preserved-results.yml?query=branch%3Amain)

# BET 2026 Diagnostic model jitter

<a id="included-files"></a>
<a id="verify-the-recovered-par-files"></a>
<a id="reproduce-the-25-retained-starts-and-fits"></a>
<a id="render-the-public-report"></a>
<a id="bet-stock-status-calculations"></a>

[View results](https://pacificcommunity.github.io/ofp-sam-bet-2026-jitter/jitter-report.html).

Thirty starts were attempted at CV 0.1; 26 fits completed and 25 met the
maximum-gradient threshold of 1e-4. The retained seed list, objectives and
stock-status summaries are preserved in the report.

`data/diagnostic/jitter/` holds the 25 exact final PARs. The shared MFCL
executable, fitting script and inputs are in `data/diagnostic/mfcl/`.
Hashes are recorded in `data/SHA256SUMS`.

From the repository root:

```sh
make help
make verify
make rerun CASE=1 OUT=/tmp/bet-jitter-1
```

`make rerun` retains native outputs and checks the saved objective, zero
iteration/evaluation counters, annual SB and depletion, and the three
stock-status endpoints. R and Linux x86-64 are required. Whole REP, MSY yield,
Hessian and projection identity remain unverified. `make native-check` retains
the earlier objective-only check of all 25 PARs.

For independent refits, prepare the original starts with pinned mfclkit in
the private Docker image described in the reproduction instructions:

```sh
make prepare OUT=/absolute/fresh/bet-jitter-prepare
```

This runs native Phases 0–1 to rebuild the Phase-1 baseline and prepares the
starts, then stops before the 25 Phase 2–11 fits.
Use `make refit OUT=/absolute/fresh/bet-jitter-refit` to run all 25 fits.

See [reproduction instructions](docs/reproduction.md) for the pinned runtime,
seed controls, output regeneration and stock-status definitions.
