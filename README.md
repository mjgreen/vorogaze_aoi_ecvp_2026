# VoroGaze AOI – ECVP 2026

This repository is a frozen, runnable snapshot of the VoroGaze Research
Workbench prepared for ECVP 2026. It is an R package with one public function:

```r
VoroGaze::run_vorogaze()
```

The app runs locally on `127.0.0.1` by default. Files you upload remain in a
private temporary directory for that Shiny session and are deleted when the
session ends. Do not use identifiable or sensitive participant data.

## What this repository contains

- the R functions needed for fixation-report import and column mapping;
- screen and image geometry checks;
- the interactive facial AOI workbench;
- annotated-fixation and researcher-configured participant-summary exports;
- two CSS files and the isolated face-image validation worker;
- one deterministic, synthetic 20-row fixation example under CC0; and
- one Face Research Lab London Set image under CC BY 4.0.

It does not contain the private development repository, its Git history or
commit identifiers, deployment configuration, Docker files, `renv`, GitHub
workflows, internal tests, developer tools, or real participant data.

## Install and run

Install R 4.2.2 or newer, then run:

```r
install.packages("pak")
pak::pkg_install("VoroGaze=github::mjgreen/vorogaze_aoi_ecvp_2026@v0.1.0")
VoroGaze::run_vorogaze()
```

The first installation downloads the package dependencies declared in
`DESCRIPTION`. The `magick` package may also require ImageMagick system
libraries; see <https://docs.ropensci.org/magick/>.

Use `Ctrl+C` in the R console to stop the app. To select another unused port:

```r
VoroGaze::run_vorogaze(port = 4848L)
```

## Scope and licensing

VoroGaze is research software, not a substitute for a documented analysis
plan. Inspect the mapping, coordinate-system, AOI, and aggregation choices
before using an export.

The package code is MIT licensed. The included synthetic CSV is dedicated to
the public domain under CC0 1.0. The included face image remains under CC BY
4.0; see `THIRD_PARTY_NOTICES.md` for its required attribution.
