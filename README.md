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
./run-report
./verify-native-pars
```

The first command rebuilds the report from saved results. The second checks
all retained PARs with native zero-iteration evaluations on 64-bit Linux;
it uses temporary directories and verifies the archived objectives.

For independent refits, prepare the original starts, then add `--run` to run
all 25 fits:

```sh
./run-reproduce-jitter --output /absolute/fresh/bet-jitter-prepare
```

See [reproduction instructions](docs/reproduction.md) for the pinned runtime,
seed controls, output regeneration and stock-status definitions.
