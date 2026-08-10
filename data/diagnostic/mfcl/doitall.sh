#!/bin/sh
set -eu

doitall_source=$0

# The exact F2 controls are literal blocks in this file.  This inspection mode
# is consumed by the external validators and deliberately needs no MFCL binary,
# model configuration file, or selectivity CSV at runtime.
if [ "${SELECTIVITY_PRINT_CONTROLS:-0}" = 1 ]; then
  awk '
    /^# BEGIN EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS$/ { print_controls=1; next }
    /^# END EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS$/ { print_controls=0; next }
    /^# BEGIN EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS$/ { print_controls=1; next }
    /^# END EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS$/ { print_controls=0; next }
    print_controls && $1 ~ /^-[0-9]+$/ { print }
  ' "$doitall_source"
  exit 0
fi

program_path=${PROGRAM_PATH:-./mfclo64}

if [ ! -x "$program_path" ]; then
  echo "MFCL executable not found: $program_path" >&2
  echo "Place mfclo64 beside doitall.sh or set PROGRAM_PATH=/path/to/mfclo64." >&2
  exit 1
fi

# This exploration always starts from the ordinary bet.ini -makepar values.
# No jitter or archived fitted checkpoint is applied.

phase10_11_convergence=${BET_PHASE10_11_CONVERGENCE:--4}
case "$phase10_11_convergence" in
  -[0-9]|-[0-9][0-9]|[0-9]|[0-9][0-9]) ;;
  *)
    echo "BET_PHASE10_11_CONVERGENCE must be numeric, e.g. -3 for quick runs or -5 for strict runs." >&2
    exit 1
    ;;
esac
echo "PHASE 10/11 convergence criterion: $phase10_11_convergence"

regional_recruitment_penalty=${REGIONAL_RECRUITMENT_PENALTY:-0.1}
case "$regional_recruitment_penalty" in
  0.1)
    regional_recruitment_penalty_flag=0
    ;;
  0.2)
    # MFCL stores ten times this coefficient in age flag 110.
    regional_recruitment_penalty_flag=2
    ;;
  *)
    echo "REGIONAL_RECRUITMENT_PENALTY must be 0.1 or 0.2." >&2
    exit 39
    ;;
esac
echo "Regional recruitment-distribution penalty: $regional_recruitment_penalty (age flag 110=$regional_recruitment_penalty_flag)"

dm_nmax=25
dm_concentration=7
echo "DM controls: Nmax=$dm_nmax; grouped fish_pars(22) fixed at $dm_concentration; fish_pars(23) estimated"
echo "Tag overdispersion: tau fixed at 2 under the direct parameterization (parest 305=1; fish_pars(4)=0; fish flags 43/44=0)"

model_id=S0.90-F2
fixed_steepness=0.90
selectivity_model=F2
selectivity_label="Flexible: F10 and F33 weak non-decreasing"
echo "Model: $model_id"
echo "Fixed steepness: $fixed_steepness (INI sv(29); age flag 162=0)"
echo "Selectivity model: $selectivity_model - $selectivity_label (198 literal controls embedded below)"

audit_tau2_fixed()
{
  par_file=$1
  phase_label=$2
  if [ ! -s "$par_file" ]; then
    echo "$phase_label tau=2 audit: missing PAR file $par_file" >&2
    exit 42
  fi

  tau_audit=$(awk '
    /^# The parest_flags/ {
      getline
      parest111=$111
      parest305=$305
      parest306=$306
    }
    !fish_flags_done && /^# fish flags/ {
      in_fish_flags=1
      next
    }
    in_fish_flags && /^#/ {
      in_fish_flags=0
    }
    in_fish_flags && NF {
      fisheries++
      active43 += $43
      grouping44 += $44
      if (fisheries == 33) {
        in_fish_flags=0
        fish_flags_done=1
      }
      next
    }
    /^# extra fishery parameters/ {
      in_fish_pars=1
      next
    }
    in_fish_pars && /^#/ {
      next
    }
    in_fish_pars && NF {
      fish_par_row++
      if (fish_par_row == 4) {
        fish_par4_count=NF
        for (i=1; i<=NF; i++) {
          if ($i < -1e-12 || $i > 1e-12) fish_par4_nonzero++
        }
        in_fish_pars=0
      }
    }
    END {
      print parest111 + 0 "," parest305 + 0 "," parest306 + 0 "," fisheries + 0 "," active43 + 0 "," grouping44 + 0 "," fish_par4_count + 0 "," fish_par4_nonzero + 0
    }
  ' "$par_file")

  if [ "$tau_audit" != "4,1,0,33,0,0,33,0" ]; then
    echo "$phase_label tau=2 audit failed: $tau_audit" >&2
    echo "Expected parest111=4, parest305=1, parest306=0, 33 fisheries, fish flags 43/44=0, and all fish_pars(4)=0." >&2
    exit 42
  fi
  echo "$phase_label tau=2 audit: passed."
}

audit_tau2_value_fixed()
{
  par_file=$1
  phase_label=$2
  if [ ! -s "$par_file" ]; then
    echo "$phase_label tau=2 value audit: missing PAR file $par_file" >&2
    exit 42
  fi

  tau_value_audit=$(awk '
    !fish_flags_done && /^# fish flags/ {
      in_fish_flags=1
      next
    }
    in_fish_flags && /^#/ {
      in_fish_flags=0
    }
    in_fish_flags && NF {
      fisheries++
      active43 += $43
      grouping44 += $44
      if (fisheries == 33) {
        in_fish_flags=0
        fish_flags_done=1
      }
      next
    }
    /^# extra fishery parameters/ {
      in_fish_pars=1
      next
    }
    in_fish_pars && /^#/ {
      next
    }
    in_fish_pars && NF {
      fish_par_row++
      if (fish_par_row == 4) {
        fish_par4_count=NF
        for (i=1; i<=NF; i++) {
          if ($i < -1e-12 || $i > 1e-12) fish_par4_nonzero++
        }
        in_fish_pars=0
      }
    }
    END {
      print fisheries + 0 "," active43 + 0 "," grouping44 + 0 "," fish_par4_count + 0 "," fish_par4_nonzero + 0
    }
  ' "$par_file")

  if [ "$tau_value_audit" != "33,0,0,33,0" ]; then
    echo "$phase_label tau=2 value audit failed: $tau_value_audit" >&2
    echo "Expected 33 fisheries, fish flags 43/44=0, and all fish_pars(4)=0." >&2
    exit 42
  fi
  echo "$phase_label tau=2 value audit: passed (estimation switches are applied in Phase 1)."
}

audit_steepness_fixed()
{
  par_file=$1
  phase_label=$2
  steepness_audit=$(awk '
    /^# age flags/ {
      getline
      age162=$162
    }
    /^# Seasonal growth parameters/ {
      getline
      steepness=$29
    }
    END { print steepness + 0 "," age162 + 0 }
  ' "$par_file")
  observed_steepness=${steepness_audit%,*}
  observed_age162=${steepness_audit#*,}
  if [ "$observed_age162" != 0 ] ||
     ! awk -v observed="$observed_steepness" -v expected="$fixed_steepness" 'BEGIN {
       difference=observed-expected
       if (difference < 0) difference=-difference
       exit(difference <= 1e-12 ? 0 : 1)
     }'; then
    echo "$phase_label fixed-steepness audit failed: sv(29)/age_flag(162)=$steepness_audit; expected $fixed_steepness/0." >&2
    exit 41
  fi
  echo "$phase_label fixed-steepness audit: $fixed_steepness passed."
}

audit_dm_concentration_fixed()
{
  par_file=$1
  phase_label=$2
  dm22_audit=$(awk -v expected="$dm_concentration" '
    /^# extra fishery parameters/ { in_extra=1; next }
    in_extra && /^#/ { next }
    in_extra && NF {
      row++
      if (row == 22) {
        copies=NF
        for (i=1; i<=NF; i++) {
          difference=$i-expected
          if (difference < 0) difference=-difference
          if (difference > 1e-12) changed++
        }
        print copies "," changed + 0
        exit
      }
    }
  ' "$par_file")
  if [ "$dm22_audit" != "33,0" ]; then
    echo "$phase_label fixed-DM audit failed: fish_pars(22) count/non-7=$dm22_audit; expected 33,0." >&2
    exit 47
  fi
  echo "$phase_label fixed-DM concentration audit: 33 fish_pars(22)=7 passed."
}

audit_selectivity_model()
{
  par_file=$1
  phase_label=$2
  if ! awk '
    FILENAME == ARGV[1] {
      if ($0 == "# BEGIN EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS") {
        in_phase1=1
        next
      }
      if ($0 == "# END EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS") {
        in_phase1=0
        next
      }
      if ($0 == "# BEGIN EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS") {
        in_phase5=1
        next
      }
      if ($0 == "# END EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS") {
        in_phase5=0
        next
      }
      if ((in_phase1 || in_phase5) && $1 ~ /^-[0-9]+$/) {
        fishery=-$1
        flag=$2
        value=$3
        if (fishery < 1 || fishery > 33) failures++
        if (in_phase1) {
          if (flag != 16 && flag != 24 && flag != 56 &&
              flag != 57 && flag != 61) failures++
          key=fishery SUBSEP flag
          if (key in expected) failures++
          expected[key]=value
          phase1_controls++
        } else {
          if (flag != 24 || fishery in final24) failures++
          final24[fishery]=value
          phase5_controls++
        }
      }
      next
    }
    /^# fish flags/ { in_fish=1; next }
    in_fish && /^#/ { in_fish=0 }
    in_fish && NF {
      observed_fishery++
      expected16=expected[observed_fishery,16]
      expected24=final24[observed_fishery]
      expected56=expected[observed_fishery,56]
      expected57=expected[observed_fishery,57]
      expected61=expected[observed_fishery,61]
      if ($16 != expected16 || $24 != expected24 ||
          $56 != expected56 || $57 != expected57 || $61 != expected61) {
        printf "F%d observed 16/24/56/57/61=%s/%s/%s/%s/%s; expected %s/%s/%s/%s/%s\n",
          observed_fishery, $16, $24, $56, $57, $61,
          expected16, expected24, expected56, expected57, expected61 > "/dev/stderr"
        failures++
      }
      if (observed_fishery == 33) exit
    }
    END {
      if (phase1_controls != 165 || phase5_controls != 33 ||
          observed_fishery != 33) {
        printf "Expected embedded/observed control counts 165/33/33; found %d/%d/%d\n",
          phase1_controls, phase5_controls, observed_fishery > "/dev/stderr"
        failures++
      }
      for (fishery=1; fishery<=33; fishery++) {
        if (!((fishery SUBSEP 16) in expected) ||
            !((fishery SUBSEP 24) in expected) ||
            !((fishery SUBSEP 56) in expected) ||
            !((fishery SUBSEP 57) in expected) ||
            !((fishery SUBSEP 61) in expected) ||
            !(fishery in final24) ||
            expected[fishery,24] != final24[fishery]) failures++
      }
      exit(failures > 0 ? 1 : 0)
    }
  ' "$doitall_source" "$par_file"; then
    echo "$phase_label selectivity audit failed for $selectivity_model." >&2
    exit 43
  fi
  echo "$phase_label selectivity audit: $selectivity_model passed."
}

if [ -n "${SELECTIVITY_AUDIT_PAR:-}" ]; then
  audit_selectivity_model "$SELECTIVITY_AUDIT_PAR" "Standalone"
  exit 0
fi


# -----------------------------------
#  PHASE 0 - create initial par file
# -----------------------------------

# The public diagnostic INI must already contain the selected fixed
# steepness. Copy it unchanged to the run-local filename passed to MFCL.
ini_steepness=$(awk '/^# sv[(]29[)]/{getline; print $1; exit}' bet.ini)
if ! awk -v observed="$ini_steepness" -v expected="$fixed_steepness" 'BEGIN {
  difference=observed-expected
  if (difference < 0) difference=-difference
  exit(difference <= 1e-12 ? 0 : 1)
}'; then
  echo "bet.ini sv(29) is $ini_steepness; expected fixed steepness $fixed_steepness." >&2
  exit 37
fi
cp bet.ini bet.model.ini

$program_path bet.frq bet.model.ini 00.par -makepar

# Fix the direct negative-binomial parameter at fish_pars(4)=log(tau-1)=0
# for tau=2. The diagnostic model also fixes the eight grouped fish_pars(22)
# concentration intercepts at 7. Set every fishery copy explicitly in the
# makepar output before Phase 1 so neither value depends on bet.ini defaults.
awk -v concentration="$dm_concentration" '
  /^# extra fishery parameters/ { in_fish = 1; print; next }
  in_fish && /^#/ { print; next }
  in_fish && NF {
    fish_row++
    if (fish_row == 4) {
      if (NF != 33) exit 38
      for (i = 1; i <= NF; i++)
        printf "%s%s", 0, (i == NF ? "\n" : " ")
      changed_tau = 1
      next
    }
    if (fish_row == 22) {
      if (NF != 33) exit 38
      for (i = 1; i <= NF; i++)
        printf "%s%s", concentration, (i == NF ? "\n" : " ")
      changed_dm = 1
      next
    }
  }
  { print }
  END { if (changed_tau != 1 || changed_dm != 1) exit 38 }
' 00.par > 00.fixed.par
audit_tau2_value_fixed 00.fixed.par "Phase 0"
audit_steepness_fixed 00.fixed.par "Phase 0"
audit_dm_concentration_fixed 00.fixed.par "Phase 0"

if [ "${MODEL_STOP_AFTER_PHASE0:-0}" = 1 ]; then
  echo "Phase-0 validation requested; stopping after makepar and fixed-control audits."
  exit 0
fi

# -----------------------
#  PHASE 1 - initial par
# -----------------------

$program_path bet.frq 00.fixed.par 01.par -file - <<PHASE1
# Use default quasi-Newton minimizer
  1 351 0
  1 192 0
# Allow all growth parameters to be fixed during control phase
  1 32 7
# Richards growth settings
  1 226 0
  1 227 0
# Catch conditioned flags
# General activation
  1 373 1  # activate CC with Baranov equation
  1 393 0  # estimate kludged_equilib_coffs and implicit_fm_level_regression_pars
  2 92 2   # specify catch-conditioned option with Baranov equation
# Catch equation bounds
  2 116 70   # value for Zmax_fish in catch equations
  2 189 80   # fraction of Zmax_fish above which penalty is calculated
  1 382 300  # weight for Zmax_fish penalty - set to 300 to avoid triggering Zmax_flag=1
# Deactivate any catch errors flags
  -999 1 0
  -999 4 0
  -999 10 0
  -999 15 0
  -999 13 0
# Survey fisheries defined
# fish flag 92 = round(region sigma * 100), fish flag 94 = allow unequal sigma,
# fish flag 66 = 1: use normalized time-varying relative-variance multipliers from the frequency data.
# 2026 index-fishery sigma settings.
  -29 94 1 -29 92 35 -29 66 1  # Residual-based CPUE R1 maximum-likelihood observation-error estimate=0.354; fixed executed error scale=0.35 (flag 92=35)
  -30 94 1 -30 92 24 -30 66 1  # Residual-based CPUE R2 maximum-likelihood observation-error estimate=0.237; fixed executed error scale=0.24 (flag 92=24)
  -31 94 1 -31 92 21 -31 66 1  # Residual-based CPUE R3 maximum-likelihood observation-error estimate=0.212; fixed executed error scale=0.21 (flag 92=21)
  -32 94 1 -32 92 24 -32 66 1  # Residual-based CPUE R4 maximum-likelihood observation-error estimate=0.239; fixed executed error scale=0.24 (flag 92=24)
  -33 94 1 -33 92 23 -33 66 1  # Residual-based CPUE R5 maximum-likelihood observation-error estimate=0.225; fixed executed error scale=0.23 (flag 92=23)
# Grouping flags for survey CPUE
   -1 99 1
   -2 99 2
   -3 99 3
   -4 99 4
   -5 99 5
   -6 99 6
   -7 99 7
   -8 99 8
   -9 99 9
  -10 99 10
  -11 99 11
  -12 99 12
  -13 99 13
  -14 99 14
  -15 99 15
  -16 99 16
  -17 99 17
  -18 99 18
  -19 99 19
  -20 99 20
  -21 99 21
  -22 99 22
  -23 99 23
  -24 99 24
  -25 99 25
  -26 99 26
  -27 99 27
  -28 99 28
  -29 99 29  # Index R1; shared initial stationary-catchability/likelihood group
  -30 99 29  # Index R2; shared initial stationary-catchability/likelihood group
  -31 99 29  # Index R3; shared initial stationary-catchability/likelihood group
  -32 99 29  # Index R4; shared initial stationary-catchability/likelihood group
  -33 99 29  # Index R5; shared initial stationary-catchability/likelihood group
# Recruitment and initial population settings
  1 149 100        # recruitment deviation penalty
  1 400 6          # final six recruitment deviates set to zero
  2 162 0          # keep sv(29) steepness fixed at the model-input value
# Fixed terminal recruitments are arithmetic mean of remaining period (not default geometric mean)
  1 398 1
  2 177 1          # use old totpop scaling method
  2 110 $regional_recruitment_penalty_flag  # regional recruitment-distribution penalty coefficient
  2 32 1           # and estimate totpop parameter
  2 93 4           # set no. of recruitments per year to 4
  2 57 4           # set no. of recruitments per year to 4
  2 94 1 2 128 100  # initial Z = 1.0*M, i.e. initial F = 0
# Likelihood component settings
  1 111 4     # set likelihood function for tags to negative binomial
  1 305 1     # direct parameterization: tau = 1 + exp(fish_pars(4))
  1 306 0     # default bounds; inactive because fish flags 43/44 are fixed at zero
  1 141 11  # length-frequency likelihood: Dirichlet-multinomial without random effects
  1 139 3     # set likelihood function for WF data to normal
  -999 49 20  # divide LF sample sizes by 20
  -999 50 20  # divide WF sample sizes by 20
# Additional LF/WF sample-size reductions retained from the inherited setup.
# Index fisheries 29-33 are included; extraction labels use the five-region fishery map.
   -1 49 40   -1 50 40
   -2 49 40   -2 50 40
   -4 49 40   -4 50 40
   -6 49 40   -6 50 40
   -7 49 40   -7 50 40
   -8 49 40   -8 50 40
  -10 49 40  -10 50 40
  -29 49 40  -29 50 40
  -30 49 40  -30 50 40
  -31 49 40  -31 50 40
  -32 49 40  -32 50 40
  -33 49 40  -33 50 40
# Tag dynamics settings
  1 33 99    # maximum tag reporting rate for all fisheries is 0.99
  2 96 30    # pool tags after 30 quarters at liberty
# Mixing periods are read from bet.ini tag flags for this step.
  2 198 1    # activate release group reporting rates
  -999 43 0  # keep fish_pars(4) fixed; 1 would estimate tau
  -999 44 0  # no grouping is needed for a fixed common value
# Grouping of fisheries for tag return data, mapped from BET_PHrev_FNL.xlsx.
# New labels with region 4 in the workbook are treated as region 5 here.
   -1 32 1   # LL.WEST.1, old1
   -2 32 2   # LL.EAST.1, old2
   -3 32 3   # LL.US.1, old3
   -4 32 4   # LL.ALL.2, old7
   -5 32 5   # LL.OS.2, old6
   -6 32 6   # LL.ARCH.3, old8
   -7 32 7   # LL.WEST.3, old4
   -8 32 8   # LL.EAST.3, old9
   -9 32 9   # LL.OS.3, old5
  -10 32 10  # LL.ALL.5, old11 + old12 + old29
  -11 32 11  # LL.AU.5, old10 + old27
  -12 32 12  # PS.JP.1, old19
  -13 32 13  # PL.JP.1, old20
  -14 32 14  # HL.ID.2, part of old18
  -15 32 14  # HL.PH.2, part of old18
  -16 32 15  # PL.ALL.2, old28
  -17 32 14  # PS.ID.2, split old24
  -18 32 14  # PS.PH.2, split old24
  -19 32 16  # PS.ASS.2, old30
  -20 32 16  # PS.UNA.2, old31
  -21 32 14  # DOM.ID.2, old23
  -22 32 14  # DOM.PH.2, old17
  -23 32 17  # DOM.VN.2, old32
  -24 32 18  # PL.ALL.WEST.3, old21 + old22
  -25 32 19  # PS.ASS.WEST.3, old13 + old25
  -26 32 20  # PS.ASS.EAST.3, old15
  -27 32 19  # PS.UNA.WEST.3, old14 + old26
  -28 32 20  # PS.UNA.EAST.3, old16
  -29 32 21  # Index R1
  -30 32 21  # Index R2
  -31 32 21  # Index R3
  -32 32 21  # Index R4
  -33 32 21  # Index R5
# Selectivity settings
  -999 3 37  # all selectivities equal for age class 37 and older
  -999 26 2  # evaluate age-based selectivity against scaled mean length-at-age
  -999 57 3  # cubic-spline selectivity
  -999 61 5  # five cubic-spline coefficients by default
  -10 16 1  # F10 LL.ALL.5: penalize decreases in selectivity with age
  -10 56 10000  # weak F10 non-decreasing penalty (1% of the MFCL default)
# Grouping of fisheries with common selectivity, mapped from BET_PHrev_FNL.xlsx.
# Staged run 1 uses 29 contiguous groups: F1-F28 use groups 1-28; F29-F33 initially share group 29.
  -1 24 1  # F1 staged-run-1 selectivity group
  -2 24 2  # F2 staged-run-1 selectivity group
  -3 24 3  # F3 staged-run-1 selectivity group
  -4 24 4  # F4 staged-run-1 selectivity group
  -5 24 5  # F5 staged-run-1 selectivity group
  -6 24 6  # F6 staged-run-1 selectivity group
  -7 24 7  # F7 staged-run-1 selectivity group
  -8 24 8  # F8 staged-run-1 selectivity group
  -9 24 9  # F9 staged-run-1 selectivity group
  -10 24 10  # F10 staged-run-1 selectivity group
  -11 24 11  # F11 staged-run-1 selectivity group
  -12 24 12  # F12 staged-run-1 selectivity group
  -13 24 13  # F13 staged-run-1 selectivity group
  -14 24 14  # F14 staged-run-1 selectivity group
  -15 24 15  # F15 staged-run-1 selectivity group
  -16 24 16  # F16 staged-run-1 selectivity group
  -17 24 17  # F17 staged-run-1 selectivity group
  -18 24 18  # F18 staged-run-1 selectivity group
  -19 24 19  # F19 staged-run-1 selectivity group
  -20 24 20  # F20 staged-run-1 selectivity group
  -21 24 21  # F21 staged-run-1 selectivity group
  -22 24 22  # F22 staged-run-1 selectivity group
  -23 24 23  # F23 staged-run-1 selectivity group
  -24 24 24  # F24 staged-run-1 selectivity group
  -25 24 25  # F25 staged-run-1 selectivity group
  -26 24 26  # F26 staged-run-1 selectivity group
  -27 24 27  # F27 staged-run-1 selectivity group
  -28 24 28  # F28 staged-run-1 selectivity group
  -29 24 29  # F29 staged-run-1 selectivity group
  -30 24 29  # F30 staged-run-1 selectivity group
  -31 24 29  # F31 staged-run-1 selectivity group
  -32 24 29  # F32 staged-run-1 selectivity group
  -33 24 29  # F33 staged-run-1 selectivity group
# Non-decreasing selectivity for the old6-derived longline fishery.
# Selected old-derived longline fisheries set to zero for first two age classes.
  -2 75 2  # F2 youngest age classes fixed at zero selectivity
  -4 75 2  # F4 youngest age classes fixed at zero selectivity
  -5 75 2  # F5 youngest age classes fixed at zero selectivity
  -7 75 2  # F7 youngest age classes fixed at zero selectivity
  -8 75 2  # F8 youngest age classes fixed at zero selectivity
  -9 75 2  # F9 youngest age classes fixed at zero selectivity
  -10 75 2  # F10 youngest age classes fixed at zero selectivity
# Old18 split into HL.ID.2 and HL.PH.2.
# Final exploration applies the youngest-five-age constraint to both split fisheries.
  -14 75 5  # F14 HL.ID.2 youngest age classes fixed at zero selectivity
  -15 75 5  # F15 youngest age classes fixed at zero selectivity
# Age-based spline constraints mapped from old fishery recipes.
  -19 16 0 -19 3 25  # F19 selected revised fishery-specific specification: selectivity-form penalty off
  -25 16 0 -25 3 25  # F25 selected revised fishery-specific specification: selectivity-form penalty off
  -26 16 0 -26 3 25  # F26 selected revised fishery-specific specification: selectivity-form penalty off
  -27 16 0 -27 3 30  # F27 selected revised fishery-specific specification: selectivity-form penalty off
  -17 16 0 -17 3 25  # F17 selected revised fishery-specific specification: selectivity-form penalty off
  -18 16 0 -18 3 25  # F18 selected revised fishery-specific specification: selectivity-form penalty off
  -12 16 0 -12 3 25  # F12 selected revised fishery-specific specification: selectivity-form penalty off
  -13 16 0 -13 3 30  # F13 selected revised fishery-specific specification: selectivity-form penalty off
# Upper-age selectivity constraints mapped from old fishery recipes.
  -22 16 0 -22 3 7  # F22 selected revised fishery-specific specification: selectivity-form penalty off
  -24 16 0 -24 3 25  # F24 selected revised fishery-specific specification: selectivity-form penalty off
  -21 16 0 -21 3 10  # F21 selected revised fishery-specific specification: selectivity-form penalty off
  -16 16 0 -16 3 25  # F16 selected revised fishery-specific specification: selectivity-form penalty off
  -23 16 0 -23 3 6  # F23 selected revised fishery-specific specification: selectivity-form penalty off
# Turn on weighted spline for calculating maturity at age
  2 188 2
# Set Lorenzen M
  2 109 3  # select Lorenzen curve
  1 121 0    # estimate no natural-mortality age_pars(5) coefficients; fix Lorenzen intercept and length slope at incoming .par values
# Filter out comps with input samples less than 50
  1 311 1  # enable tail-compressed observed and predicted length-frequency arrays
  1 301 1   # set tail compression for WF data
  1 313 0  # not read by the DM likelihood; reset to avoid percentage-tail preprocessing, while flag 320 controls DM support
  1 303 0   # proportions in compressed tails for WF data
  1 312 50  # set minimum obs sample size for LF data
  1 302 50  # set minimum obs sample size for WF data
# MFCL 2.2.2.0 growth variance fix
  1 34 0    # set to 1 34 1 for backwards compatibility
  -15 16 0  # F15 selected revised fishery-specific specification: selectivity-form penalty off
  -15 3 25  # F15 terminal spline age and start age for the older-age dome penalty
  -25 61 7  # F25 seven estimated cubic-spline nodes
  -25 75 0  # F25 no youngest age classes forced to near-zero selectivity
  -26 61 7  # F26 seven estimated cubic-spline nodes
  -26 75 0  # F26 no youngest age classes forced to near-zero selectivity
  -1 75 2  # F1 youngest age classes fixed at zero selectivity
  -3 75 2  # F3 youngest age classes fixed at zero selectivity
  -6 75 2  # F6 youngest age classes fixed at zero selectivity
  -11 75 2  # F11 youngest age classes fixed at zero selectivity
  -12 75 2  # F12 youngest age classes fixed at zero selectivity
  -13 75 1  # F13 youngest age classes fixed at zero selectivity
  -29 75 2  # Index R1 youngest age classes fixed at zero selectivity
  -30 75 2  # Index R2 youngest age classes fixed at zero selectivity
  -31 75 2  # Index R3 youngest age classes fixed at zero selectivity
  -32 75 2  # Index R4 youngest age classes fixed at zero selectivity
  -33 75 2  # Index R5 youngest age classes fixed at zero selectivity
  1 320 5  # use tail-compressed DM when the first-to-last-positive observed span contains at least five bins
  1 342 25  # diagnostic-model DM effective-sample-size upper bound
  -1 68 1  # G8PSSET DM group for F1
  -2 68 1  # G8PSSET DM group for F2
  -3 68 1  # G8PSSET DM group for F3
  -4 68 1  # G8PSSET DM group for F4
  -5 68 2  # G8PSSET DM group for F5
  -6 68 1  # G8PSSET DM group for F6
  -7 68 1  # G8PSSET DM group for F7
  -8 68 1  # G8PSSET DM group for F8
  -9 68 2  # G8PSSET DM group for F9
  -10 68 1  # G8PSSET DM group for F10
  -11 68 1  # G8PSSET DM group for F11
  -12 68 3  # G8PSSET DM group for F12
  -13 68 7  # G8PSSET DM group for F13
  -14 68 6  # G8PSSET DM group for F14
  -15 68 6  # G8PSSET DM group for F15
  -16 68 7  # G8PSSET DM group for F16
  -17 68 3  # G8PSSET DM group for F17
  -18 68 3  # G8PSSET DM group for F18
  -19 68 4  # G8PSSET DM group for F19
  -20 68 5  # G8PSSET DM group for F20
  -21 68 7  # G8PSSET DM group for F21
  -22 68 7  # G8PSSET DM group for F22
  -23 68 7  # G8PSSET DM group for F23
  -24 68 7  # G8PSSET DM group for F24
  -25 68 4  # G8PSSET DM group for F25
  -26 68 4  # G8PSSET DM group for F26
  -27 68 5  # G8PSSET DM group for F27
  -28 68 5  # G8PSSET DM group for F28
  -29 68 8  # G8PSSET DM group for F29
  -30 68 8  # G8PSSET DM group for F30
  -31 68 8  # G8PSSET DM group for F31
  -32 68 8  # G8PSSET DM group for F32
  -33 68 8  # G8PSSET DM group for F33
  -999 69 0  # fix grouped fish_pars(22) concentration intercepts at 7
  -999 89 0  # stage relative sample-size exponent fixed at zero
# Model-specific selectivity controls are last so they override the common
# Diagnostic defaults without altering any non-selectivity setting.
# BEGIN EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS
  -1 16 0  # LL.WEST.1 flag 16 from explicit input
  -1 24 1  # LL.WEST.1 selectivity-sharing group
  -1 56 0  # LL.WEST.1 selectivity-penalty weight
  -1 57 3  # LL.WEST.1 selectivity form
  -1 61 5  # LL.WEST.1 spline-node count
  -2 16 0  # LL.EAST.1 flag 16 from explicit input
  -2 24 2  # LL.EAST.1 selectivity-sharing group
  -2 56 0  # LL.EAST.1 selectivity-penalty weight
  -2 57 3  # LL.EAST.1 selectivity form
  -2 61 5  # LL.EAST.1 spline-node count
  -3 16 0  # LL.US.1 flag 16 from explicit input
  -3 24 3  # LL.US.1 selectivity-sharing group
  -3 56 0  # LL.US.1 selectivity-penalty weight
  -3 57 3  # LL.US.1 selectivity form
  -3 61 5  # LL.US.1 spline-node count
  -4 16 0  # LL.ALL.2 flag 16 from explicit input
  -4 24 4  # LL.ALL.2 selectivity-sharing group
  -4 56 0  # LL.ALL.2 selectivity-penalty weight
  -4 57 3  # LL.ALL.2 selectivity form
  -4 61 5  # LL.ALL.2 spline-node count
  -5 16 0  # LL.OS.2 flag 16 from explicit input
  -5 24 5  # LL.OS.2 selectivity-sharing group
  -5 56 0  # LL.OS.2 selectivity-penalty weight
  -5 57 3  # LL.OS.2 selectivity form
  -5 61 5  # LL.OS.2 spline-node count
  -6 16 0  # LL.ARCH.3 flag 16 from explicit input
  -6 24 6  # LL.ARCH.3 selectivity-sharing group
  -6 56 0  # LL.ARCH.3 selectivity-penalty weight
  -6 57 3  # LL.ARCH.3 selectivity form
  -6 61 5  # LL.ARCH.3 spline-node count
  -7 16 0  # LL.WEST.3 flag 16 from explicit input
  -7 24 7  # LL.WEST.3 selectivity-sharing group
  -7 56 0  # LL.WEST.3 selectivity-penalty weight
  -7 57 3  # LL.WEST.3 selectivity form
  -7 61 5  # LL.WEST.3 spline-node count
  -8 16 0  # LL.EAST.3 flag 16 from explicit input
  -8 24 8  # LL.EAST.3 selectivity-sharing group
  -8 56 0  # LL.EAST.3 selectivity-penalty weight
  -8 57 3  # LL.EAST.3 selectivity form
  -8 61 5  # LL.EAST.3 spline-node count
  -9 16 0  # LL.OS.3 flag 16 from explicit input
  -9 24 9  # LL.OS.3 selectivity-sharing group
  -9 56 0  # LL.OS.3 selectivity-penalty weight
  -9 57 3  # LL.OS.3 selectivity form
  -9 61 5  # LL.OS.3 spline-node count
  -10 16 1  # LL.ALL.5 flag 16 from explicit input
  -10 24 10  # LL.ALL.5 selectivity-sharing group
  -10 56 10000  # LL.ALL.5 selectivity-penalty weight
  -10 57 3  # LL.ALL.5 selectivity form
  -10 61 5  # LL.ALL.5 spline-node count
  -11 16 0  # LL.AU.5 flag 16 from explicit input
  -11 24 11  # LL.AU.5 selectivity-sharing group
  -11 56 0  # LL.AU.5 selectivity-penalty weight
  -11 57 3  # LL.AU.5 selectivity form
  -11 61 5  # LL.AU.5 spline-node count
  -12 16 0  # PS.JP.1 flag 16 from explicit input
  -12 24 12  # PS.JP.1 selectivity-sharing group
  -12 56 0  # PS.JP.1 selectivity-penalty weight
  -12 57 3  # PS.JP.1 selectivity form
  -12 61 5  # PS.JP.1 spline-node count
  -13 16 0  # PL.JP.1 flag 16 from explicit input
  -13 24 13  # PL.JP.1 selectivity-sharing group
  -13 56 0  # PL.JP.1 selectivity-penalty weight
  -13 57 3  # PL.JP.1 selectivity form
  -13 61 5  # PL.JP.1 spline-node count
  -14 16 0  # HL.ID.2 flag 16 from explicit input
  -14 24 14  # HL.ID.2 selectivity-sharing group
  -14 56 0  # HL.ID.2 selectivity-penalty weight
  -14 57 3  # HL.ID.2 selectivity form
  -14 61 5  # HL.ID.2 spline-node count
  -15 16 0  # HL.PH.2 flag 16 from explicit input
  -15 24 15  # HL.PH.2 selectivity-sharing group
  -15 56 0  # HL.PH.2 selectivity-penalty weight
  -15 57 3  # HL.PH.2 selectivity form
  -15 61 5  # HL.PH.2 spline-node count
  -16 16 0  # PL.ALL.2 flag 16 from explicit input
  -16 24 16  # PL.ALL.2 selectivity-sharing group
  -16 56 0  # PL.ALL.2 selectivity-penalty weight
  -16 57 3  # PL.ALL.2 selectivity form
  -16 61 5  # PL.ALL.2 spline-node count
  -17 16 0  # PS.ID.2 flag 16 from explicit input
  -17 24 17  # PS.ID.2 selectivity-sharing group
  -17 56 0  # PS.ID.2 selectivity-penalty weight
  -17 57 3  # PS.ID.2 selectivity form
  -17 61 5  # PS.ID.2 spline-node count
  -18 16 0  # PS.PH.2 flag 16 from explicit input
  -18 24 18  # PS.PH.2 selectivity-sharing group
  -18 56 0  # PS.PH.2 selectivity-penalty weight
  -18 57 3  # PS.PH.2 selectivity form
  -18 61 5  # PS.PH.2 spline-node count
  -19 16 0  # PS.ASS.2 flag 16 from explicit input
  -19 24 19  # PS.ASS.2 selectivity-sharing group
  -19 56 0  # PS.ASS.2 selectivity-penalty weight
  -19 57 3  # PS.ASS.2 selectivity form
  -19 61 5  # PS.ASS.2 spline-node count
  -20 16 0  # PS.UNA.2 flag 16 from explicit input
  -20 24 20  # PS.UNA.2 selectivity-sharing group
  -20 56 0  # PS.UNA.2 selectivity-penalty weight
  -20 57 3  # PS.UNA.2 selectivity form
  -20 61 5  # PS.UNA.2 spline-node count
  -21 16 0  # DOM.ID.2 flag 16 from explicit input
  -21 24 21  # DOM.ID.2 selectivity-sharing group
  -21 56 0  # DOM.ID.2 selectivity-penalty weight
  -21 57 3  # DOM.ID.2 selectivity form
  -21 61 5  # DOM.ID.2 spline-node count
  -22 16 0  # DOM.PH.2 flag 16 from explicit input
  -22 24 22  # DOM.PH.2 selectivity-sharing group
  -22 56 0  # DOM.PH.2 selectivity-penalty weight
  -22 57 3  # DOM.PH.2 selectivity form
  -22 61 5  # DOM.PH.2 spline-node count
  -23 16 0  # DOM.VN.2 flag 16 from explicit input
  -23 24 23  # DOM.VN.2 selectivity-sharing group
  -23 56 0  # DOM.VN.2 selectivity-penalty weight
  -23 57 3  # DOM.VN.2 selectivity form
  -23 61 5  # DOM.VN.2 spline-node count
  -24 16 0  # PL.ALL.WEST.3 flag 16 from explicit input
  -24 24 24  # PL.ALL.WEST.3 selectivity-sharing group
  -24 56 0  # PL.ALL.WEST.3 selectivity-penalty weight
  -24 57 3  # PL.ALL.WEST.3 selectivity form
  -24 61 5  # PL.ALL.WEST.3 spline-node count
  -25 16 0  # PS.ASS.WEST.3 flag 16 from explicit input
  -25 24 25  # PS.ASS.WEST.3 selectivity-sharing group
  -25 56 0  # PS.ASS.WEST.3 selectivity-penalty weight
  -25 57 3  # PS.ASS.WEST.3 selectivity form
  -25 61 7  # PS.ASS.WEST.3 spline-node count
  -26 16 0  # PS.ASS.EAST.3 flag 16 from explicit input
  -26 24 26  # PS.ASS.EAST.3 selectivity-sharing group
  -26 56 0  # PS.ASS.EAST.3 selectivity-penalty weight
  -26 57 3  # PS.ASS.EAST.3 selectivity form
  -26 61 7  # PS.ASS.EAST.3 spline-node count
  -27 16 0  # PS.UNA.WEST.3 flag 16 from explicit input
  -27 24 27  # PS.UNA.WEST.3 selectivity-sharing group
  -27 56 0  # PS.UNA.WEST.3 selectivity-penalty weight
  -27 57 3  # PS.UNA.WEST.3 selectivity form
  -27 61 5  # PS.UNA.WEST.3 spline-node count
  -28 16 0  # PS.UNA.EAST.3 flag 16 from explicit input
  -28 24 28  # PS.UNA.EAST.3 selectivity-sharing group
  -28 56 0  # PS.UNA.EAST.3 selectivity-penalty weight
  -28 57 3  # PS.UNA.EAST.3 selectivity form
  -28 61 5  # PS.UNA.EAST.3 spline-node count
  -29 16 0  # Index_R1 flag 16 from explicit input
  -29 24 29  # Index_R1 selectivity-sharing group
  -29 56 0  # Index_R1 selectivity-penalty weight
  -29 57 3  # Index_R1 selectivity form
  -29 61 5  # Index_R1 spline-node count
  -30 16 0  # Index_R2 flag 16 from explicit input
  -30 24 30  # Index_R2 selectivity-sharing group
  -30 56 0  # Index_R2 selectivity-penalty weight
  -30 57 3  # Index_R2 selectivity form
  -30 61 5  # Index_R2 spline-node count
  -31 16 0  # Index_R3 flag 16 from explicit input
  -31 24 31  # Index_R3 selectivity-sharing group
  -31 56 0  # Index_R3 selectivity-penalty weight
  -31 57 3  # Index_R3 selectivity form
  -31 61 5  # Index_R3 spline-node count
  -32 16 0  # Index_R4 flag 16 from explicit input
  -32 24 32  # Index_R4 selectivity-sharing group
  -32 56 0  # Index_R4 selectivity-penalty weight
  -32 57 3  # Index_R4 selectivity form
  -32 61 5  # Index_R4 spline-node count
  -33 16 1  # Index_R5 flag 16 from explicit input
  -33 24 33  # Index_R5 selectivity-sharing group
  -33 56 10000  # Index_R5 selectivity-penalty weight
  -33 57 3  # Index_R5 selectivity form
  -33 61 5  # Index_R5 spline-node count
# END EMBEDDED F2 PHASE 1 SELECTIVITY CONTROLS
PHASE1
audit_tau2_fixed 01.par "Phase 1"
audit_steepness_fixed 01.par "Phase 1"
audit_dm_concentration_fixed 01.par "Phase 1"

# ---------
#  PHASE 2
# ---------

$program_path bet.frq 01.par 02.par -file - <<PHASE2
  1 1 100  # set max. number of function evaluations per phase to 100
  1 50 0   # set convergence criterion to 1
  2 113 0  # scaling init pop - turned off
  1 190 1  # write plot-xxx.par.rep
  -999 89 1  # estimate group-specific DM relative sample-size exponent (CEST)
PHASE2
audit_tau2_fixed 02.par "Phase 2"
audit_steepness_fixed 02.par "Phase 2"
audit_dm_concentration_fixed 02.par "Phase 2"

# ---------
#  PHASE 3
# ---------

$program_path bet.frq 02.par 03.par -file - <<PHASE3
  2 70 1   # activate time series of reg recruitment parameters
  2 71 1   # estimate temporal changes in recruitment distribution
  2 178 1  # constrain regional recruitments
  1 1 200
PHASE3
audit_tau2_fixed 03.par "Phase 3"
audit_steepness_fixed 03.par "Phase 3"
audit_dm_concentration_fixed 03.par "Phase 3"

# ---------
#  PHASE 4
# ---------

$program_path bet.frq 03.par 04.par -file - <<PHASE4
  2 68 1   # estimate movement coefficients
  2 69 1
  2 27 -1  # penalty wt 0.1 computed against prior
PHASE4
audit_tau2_fixed 04.par "Phase 4"
audit_steepness_fixed 04.par "Phase 4"
audit_dm_concentration_fixed 04.par "Phase 4"

# ---------
#  PHASE 5
# ---------

$program_path bet.frq 04.par 05.par -file - <<PHASE5
  -100000 1 1  # estimate
  -100000 2 1  # time-invariant
  -100000 3 1  # distribution
  -100000 4 1  # of
  -100000 5 1  # recruitment
# STAGED MFCL RUN 5: introduce REGW regional scaling and separate regional CPUE groups.
# These controls persist in subsequent runs through the carried parameter file.
  1 77 100  # REGW regional-scaling penalty weight
  1 78 1  # use mean regional-scaling target
  1 79 240  # start bound: 240 periods back from model end, mapping to source period 53
  1 80 220  # end bound: 220 periods back from model end, mapping to source period 72
  1 81 1  # enable the multivariate-normal regional-scaling penalty
  -29 99 29  # Index R1; separate stationary-catchability/likelihood group from staged run 5
  -30 99 30  # Index R2; separate stationary-catchability/likelihood group from staged run 5
  -31 99 31  # Index R3; separate stationary-catchability/likelihood group from staged run 5
  -32 99 32  # Index R4; separate stationary-catchability/likelihood group from staged run 5
  -33 99 33  # Index R5; separate stationary-catchability/likelihood group from staged run 5
  -29 94 0  # Index R1; separate flag-99 group now supplies its own flag-92 error scale
  -30 94 0  # Index R2; separate flag-99 group now supplies its own flag-92 error scale
  -31 94 0  # Index R3; separate flag-99 group now supplies its own flag-92 error scale
  -32 94 0  # Index R4; separate flag-99 group now supplies its own flag-92 error scale
  -33 94 0  # Index R5; separate flag-99 group now supplies its own flag-92 error scale
# STAGED MFCL RUN 5: separate the five regional-index selectivity-sharing groups.
  -29 24 29  # Index R1; separate selectivity coefficient-sharing group from staged run 5
  -30 24 30  # Index R2; separate selectivity coefficient-sharing group from staged run 5
  -31 24 31  # Index R3; separate selectivity coefficient-sharing group from staged run 5
  -32 24 32  # Index R4; separate selectivity coefficient-sharing group from staged run 5
  -33 24 33  # Index R5; separate selectivity coefficient-sharing group from staged run 5
# P-series models retain the two documented extraction-fishery sharing pairs
# and independent index groups after the staged-run-5 controls above.
# BEGIN EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS
  -1 24 1  # LL.WEST.1 final selectivity-sharing group
  -2 24 2  # LL.EAST.1 final selectivity-sharing group
  -3 24 3  # LL.US.1 final selectivity-sharing group
  -4 24 4  # LL.ALL.2 final selectivity-sharing group
  -5 24 5  # LL.OS.2 final selectivity-sharing group
  -6 24 6  # LL.ARCH.3 final selectivity-sharing group
  -7 24 7  # LL.WEST.3 final selectivity-sharing group
  -8 24 8  # LL.EAST.3 final selectivity-sharing group
  -9 24 9  # LL.OS.3 final selectivity-sharing group
  -10 24 10  # LL.ALL.5 final selectivity-sharing group
  -11 24 11  # LL.AU.5 final selectivity-sharing group
  -12 24 12  # PS.JP.1 final selectivity-sharing group
  -13 24 13  # PL.JP.1 final selectivity-sharing group
  -14 24 14  # HL.ID.2 final selectivity-sharing group
  -15 24 15  # HL.PH.2 final selectivity-sharing group
  -16 24 16  # PL.ALL.2 final selectivity-sharing group
  -17 24 17  # PS.ID.2 final selectivity-sharing group
  -18 24 18  # PS.PH.2 final selectivity-sharing group
  -19 24 19  # PS.ASS.2 final selectivity-sharing group
  -20 24 20  # PS.UNA.2 final selectivity-sharing group
  -21 24 21  # DOM.ID.2 final selectivity-sharing group
  -22 24 22  # DOM.PH.2 final selectivity-sharing group
  -23 24 23  # DOM.VN.2 final selectivity-sharing group
  -24 24 24  # PL.ALL.WEST.3 final selectivity-sharing group
  -25 24 25  # PS.ASS.WEST.3 final selectivity-sharing group
  -26 24 26  # PS.ASS.EAST.3 final selectivity-sharing group
  -27 24 27  # PS.UNA.WEST.3 final selectivity-sharing group
  -28 24 28  # PS.UNA.EAST.3 final selectivity-sharing group
  -29 24 29  # Index_R1 final selectivity-sharing group
  -30 24 30  # Index_R2 final selectivity-sharing group
  -31 24 31  # Index_R3 final selectivity-sharing group
  -32 24 32  # Index_R4 final selectivity-sharing group
  -33 24 33  # Index_R5 final selectivity-sharing group
# END EMBEDDED F2 PHASE 5 SELECTIVITY CONTROLS
PHASE5
audit_tau2_fixed 05.par "Phase 5"
audit_steepness_fixed 05.par "Phase 5"
audit_dm_concentration_fixed 05.par "Phase 5"
audit_selectivity_model 05.par "Phase 5"

# ---------
#  PHASE 6
# ---------

$program_path bet.frq 05.par 06.par -file - <<PHASE6
  1 240 1  # fit to age-length data
  1 14 1   # estimate von Bertalanffy K
  1 12 1   # estimate mean length of age 1
  1 13 1   # estimate length of age n
  1 1 300  # function evaluations
PHASE6
audit_tau2_fixed 06.par "Phase 6"
audit_steepness_fixed 06.par "Phase 6"
audit_dm_concentration_fixed 06.par "Phase 6"

# ---------
#  PHASE 7
# ---------

$program_path bet.frq 06.par 07.par -file - <<PHASE7
  1 15 1   # estimate overall SD of length-at-age
  1 16 1   # estimate length dependent SD
  1 173 0  # activate independent mean lengths for first 0 age classes
  1 182 0  # penalty weight
  1 184 0  # estimate parameters
  1 1 500  # function evaluations
PHASE7
audit_tau2_fixed 07.par "Phase 7"
audit_steepness_fixed 07.par "Phase 7"
audit_dm_concentration_fixed 07.par "Phase 7"

# ---------
#  PHASE 8
# ---------

$program_path bet.frq 07.par 08.par -file - <<PHASE8
  2 145 1    # use SRR parameters - low penalty for deviation
  2 146 1    # estimate SRR parameters
  2 182 1    # make SRR annual rather than quarterly
  2 161 1    # lognormal bias correction
  2 163 0    # use steepness parameterization of B&H SRR
  1 149 0    # penalty for recruitment devs
  2 147 1    # time period between spawning and recruitment
  2 148 20   # period for MSY calc - last 20 quarters
  2 155 4    # but not including last year
  2 199 212  # start period for SRR estimation/yield is start 1965?
  2 200 6    # end period for SRR estimation is mid 2017
  -999 55 1  # do impact analysis
  2 171 1    # include SRR-based equilibrium recruitment to compute unfished biomass
  1 186 1    # write fishmort and plotq0.rep
  1 187 1    # write temporary_tag_report
  1 188 1    # write ests.rep
  1 189 1    # write .fit files
  1 1 500    # function evaluations
  1 50 -2    # convergence criteria
  2 116 100  # increase F bound for NR to 1.0
PHASE8
audit_tau2_fixed 08.par "Phase 8"
audit_steepness_fixed 08.par "Phase 8"
audit_dm_concentration_fixed 08.par "Phase 8"

# ---------
#  PHASE 9
# ---------

$program_path bet.frq 08.par 09.par -file - <<PHASE9
  2 145 -1   # use SRR parameters - low penalty for deviation
  1 1 500    # function evaluations
  1 50 -2    # convergence criteria
  2 116 300  # increase F bound for NR to 3.0
PHASE9
audit_tau2_fixed 09.par "Phase 9"
audit_steepness_fixed 09.par "Phase 9"
audit_dm_concentration_fixed 09.par "Phase 9"

# ------------------------------------------------------------------
#  TAG-TAU TREATMENT - direct negative binomial, tau fixed at 2
# ------------------------------------------------------------------

# Parest flags 111/305 remain 4/1, fish flags 43/44 remain zero and
# fish_pars(4)=log(2-1)=0 remains fixed. All other Diagnostic settings carry.
$program_path bet.frq 09.par 10.par -file - <<PHASE10_TAU2_FIXED
  1 1 10000
  1 50 $phase10_11_convergence
  1 121 0
PHASE10_TAU2_FIXED
audit_tau2_fixed 10.par "Phase 10"
audit_steepness_fixed 10.par "Phase 10"
audit_dm_concentration_fixed 10.par "Phase 10"

$program_path bet.frq 10.par 11.par -file - <<PHASE11_TAU2_FIXED
  1 1 5000
  1 50 $phase10_11_convergence
  1 121 0
  1 246 1
PHASE11_TAU2_FIXED
audit_tau2_fixed 11.par "Phase 11"
audit_steepness_fixed 11.par "Phase 11"
audit_dm_concentration_fixed 11.par "Phase 11"
audit_selectivity_model 11.par "Phase 11"

final_par=11.par
parest_111=$(awk '/^# The parest_flags/{getline; print $111; exit}' "$final_par")
parest_121=$(awk '/^# The parest_flags/{getline; print $121; exit}' "$final_par")
parest_141=$(awk '/^# The parest_flags/{getline; print $141; exit}' "$final_par")
parest_305=$(awk '/^# The parest_flags/{getline; print $305; exit}' "$final_par")
parest_306=$(awk '/^# The parest_flags/{getline; print $306; exit}' "$final_par")
parest_320=$(awk '/^# The parest_flags/{getline; print $320; exit}' "$final_par")
parest_342=$(awk '/^# The parest_flags/{getline; print $342; exit}' "$final_par")
estimated_tau_count=$(awk '$2 ~ /^fish_pars[(]4[)]/ {n++} END {print n+0}' indepvar.rpt)
dm22_active=$(awk '$2 ~ /^fish_pars[(]22[)]/ {n++} END {print n+0}' indepvar.rpt)
dm23_active=$(awk '$2 ~ /^fish_pars[(]23[)]/ {n++} END {print n+0}' indepvar.rpt)
estimated_steepness_count=$(awk '$2 == "sv(29)" {n++} END {print n+0}' indepvar.rpt)
tau_fish_flag_summary=$(awk '
  /^# fish flags/ {in_fish=1; next}
  in_fish && /^#/ {exit}
  in_fish && NF {
    fishery++
    active43 += $43
    grouping44 += $44
    if (fishery == 33) {
      print active43 + 0 "," grouping44 + 0
      exit
    }
  }
' "$final_par")
active_tau_fisheries=${tau_fish_flag_summary%,*}
grouped_tau_fisheries=${tau_fish_flag_summary#*,}
fish_par4_summary=$(awk '
  /^# extra fishery parameters/ {in_fish=1; next}
  in_fish && /^#/ {next}
  in_fish && NF {
    row++
    if (row == 4) {
      for (i=1; i<=NF; i++) {
        if ($i < -1e-12 || $i > 1e-12) nonzero++
      }
      print NF "," nonzero + 0
      exit
    }
  }
' "$final_par")
final_m=$(awk '
  /^# age_pars/ || /^# age-class related parameters [(]age_pars[)]/ {
    in_age=1
    next
  }
  in_age && /^#/ {next}
  in_age && NF {
    row++
    if (row == 5) {
      print $1
      exit
    }
  }
' "$final_par")
final_steepness=$(awk '/^# Seasonal growth parameters/{getline; print $29; exit}' "$final_par")
final_age162=$(awk '/^# age flags/{getline; print $162; exit}' "$final_par")

dm_control_summary=$(awk '
  /^# fish flags/ { in_fish=1; next }
  in_fish && /^#/ { exit }
  in_fish && NF {
    n++; groups[$68]=1; flag69 += $69; flag89 += $89
    if (n == 33) exit
  }
  END {
    for (group in groups) group_count++
    print n "," group_count "," flag69 "," flag89
  }
' "$final_par")

if [ "$parest_111" != 4 ] || [ "$parest_305" != 1 ] ||
   [ "$parest_306" != 0 ] || [ "$parest_121" != 0 ] ||
   [ "$parest_141" != 11 ] || [ "$parest_320" != 5 ] ||
   [ "$parest_342" != "$dm_nmax" ] ||
   [ "$dm22_active" != 0 ] || [ "$dm23_active" != 8 ] ||
   [ "$dm_control_summary" != "33,8,0,33" ] ||
   [ "$estimated_steepness_count" != 0 ] || [ "$final_age162" != 0 ] ||
   [ "$estimated_tau_count" != 0 ] || [ "$active_tau_fisheries" != 0 ] ||
   [ "$grouped_tau_fisheries" != 0 ] || [ "$fish_par4_summary" != "33,0" ]; then
  echo "Final fit did not retain the required DM and fixed tau=2 controls." >&2
  exit 44
fi
if ! awk -v observed="$final_steepness" -v expected="$fixed_steepness" 'BEGIN {
  difference=observed-expected
  if (difference < 0) difference=-difference
  exit(difference <= 1e-12 ? 0 : 1)
}'; then
  echo "Final fit changed fixed steepness: observed $final_steepness; expected $fixed_steepness." >&2
  exit 46
fi
if ! awk -v observed="$final_m" 'BEGIN {
  expected = -2.54930339768360
  difference = observed - expected
  if (difference < 0) difference = -difference
  exit(difference <= 1e-12 ? 0 : 1)
}'; then
  echo "Fixed Lorenzen natural mortality changed: $final_m" >&2
  exit 45
fi

printf '%s\n' \
  'mode,tag_likelihood,tau,fish_pars4,parest111,parest305,parest306,estimated_tau_count,active_tau_fisheries,grouped_tau_fisheries,parest121,dm_nmax,dm_concentration,dm22_active,dm23_active,parest141,parest320,parest342,final_m,status' \
  "tau-fixed,direct-negative-binomial,2,0,$parest_111,$parest_305,$parest_306,$estimated_tau_count,$active_tau_fisheries,$grouped_tau_fisheries,$parest_121,$dm_nmax,$dm_concentration,$dm22_active,$dm23_active,$parest_141,$parest_320,$parest_342,$final_m,passed" \
  > tag-tau-audit.csv

printf '%s\n' \
  'model_id,steepness,selectivity_model,label,selectivity_groups,four_node_flags,five_node_flags,seven_node_flags,logistic_flags,weak_penalty_flags,status' \
  > selectivity-audit.csv
awk -v model_id="$model_id" -v steepness="$fixed_steepness" \
    -v model="$selectivity_model" -v label="$selectivity_label" '
  /^# fish flags/ { in_fish=1; next }
  in_fish && /^#/ { in_fish=0 }
  in_fish && NF {
    fishery++
    groups[$24]=1
    if ($61 == 4) four++
    if ($61 == 5) five++
    if ($61 == 7) seven++
    if ($57 == 1) logistic++
    if ($16 == 1 && $56 == 10000) weak++
    if (fishery == 33) exit
  }
  END {
    for (group in groups) group_count++
    printf "\"%s\",%s,\"%s\",\"%s\",%d,%d,%d,%d,%d,%d,passed\n",
      model_id, steepness, model, label, group_count, four + 0, five + 0, seven + 0,
      logistic + 0, weak + 0
  }
' "$final_par" >> selectivity-audit.csv

printf '%s\n' \
  'model_id,fixed_steepness,age_flag_162,estimated_steepness_count,selectivity_model,tau,initialization,status' \
  "$model_id,$fixed_steepness,$final_age162,$estimated_steepness_count,$selectivity_model,2,ordinary-makepar-no-seed,passed" \
  > model-input-audit.csv
exit 0
