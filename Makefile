.PHONY: all

all:
	./run-report

# Reader commands; the original report target above is retained.
CASE ?= 1
OUT ?=
export CASE OUT
.PHONY: help verify native-check rerun prepare refit

help:
	@printf '%s\n' 'make verify                            Check preserved files and data hashes' 'make native-check                      Validate all 25 PARs in temporary directories' 'make rerun CASE=1 OUT=/tmp/bet-jitter-1 Retain outputs from one saved PAR' 'make prepare OUT=/tmp/bet-jitter-starts Prepare all 25 starts with pinned mfclkit' 'make refit OUT=/tmp/bet-jitter-refit    Fit and validate all 25 starts' 'Saved-PAR reruns need R and Linux x86-64. Full preparation/refits need the pinned Docker image.'

verify:
	@sha256sum --quiet -c ci/PRESERVED.sha256
	@sha256sum --quiet -c data/SHA256SUMS
	@printf '%s\n' 'Preserved files and MFCL data verified.'

native-check:
	./verify-native-pars -

rerun: verify
	Rscript reproduce/rerun-saved.R "$$CASE" "$$OUT"

prepare:
	./run-reproduce-jitter --output "$$OUT"

refit:
	./run-reproduce-jitter --run --output "$$OUT"
