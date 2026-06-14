# Replication Package for "Assessing the Heterogeneous Impact of Economy-Wide Shocks: A Causal Machine Learning Approach Applied to Colombian Firms"

This repository contains the code and processed analysis data for:

> Federico Nutarelli, Víctor Ortiz-Giménez, Massimo Riccaboni, Francesco Serti,
> and Marco Dueñas, "Assessing the Heterogeneous Impact of Economy-Wide Shocks:
> A Causal Machine Learning Approach Applied to Colombian Firms."

## Project overview

The paper develops a causal machine learning approach for studying heterogeneous
effects when a shock is economy-wide and an unaffected contemporaneous control
group is unavailable. The application examines the effect of the COVID-19 crisis
on Colombian firms' probability of remaining active in export markets.

The empirical strategy compares:

- a **Shock Unaware Machine (SUM)**, which predicts a no-shock counterfactual
  using pre-shock data; and
- a **Shock Aware Machine (SAM)**, which predicts outcomes using information
  from the shock period.

The paper then uses causal machine learning tools to study how the estimated
effects vary across firms. The analysis finds a substantial average decline in
export-market survival during the initial COVID-19 shock, together with
meaningful heterogeneity across firms.

## Detailed replication instructions

The complete, result-by-result instructions are in
**[REPLICATION_GUIDE.pdf](REPLICATION_GUIDE.pdf)**. The guide documents:

- the exact order in which scripts should be run;
- the inputs and outputs for every table and figure;
- software and computational requirements;
- data construction and availability;
- path configuration options; and
- expected runtime and sources of numerical variation.

Please read the guide before running the code. Several results depend on
intermediate files produced by earlier scripts, and some procedures are
computationally intensive.

## Software

The replication package was tested with:

- **R 4.5.3**;
- **RStudio 2025.09.2+418** on macOS;
- **Stata/SE 19.5** on Windows; and
- **StataNow/SE 19.5** on macOS.

The code was tested on Windows 11 Pro and macOS. Other environments may work,
but package versions, paths, and numerical results can differ slightly.

## Quick start

1. Clone or download the repository without changing its directory structure.
2. Install the required R packages and Stata version described in the detailed
   replication guide.
3. For the main analyses, begin with the included files:
   `data/data_out/final_data_18.RData`,
   `data/data_out/final_data_19.RData`, and
   `data/data_out/final_data_20.RData`.
4. Follow the execution order in
   [REPLICATION_GUIDE.pdf](REPLICATION_GUIDE.pdf), because many figures and
   tables require outputs created by earlier steps.

Most scripts determine paths relative to their own location. Where supported,
the detailed guide lists `OBES_*` environment variables that can override input
or output directories.

## Repository structure

The existing directory structure is part of the replication design and should
be preserved.

| Path | Contents |
| --- | --- |
| `data/` | Data-construction scripts, included inputs, and final analysis datasets |
| `Fig_1/`, `Fig_2/`, `Fig_3/`, `Fig_5/`, `Fig_6/`, `Fig_8/` | Code and supporting files for the corresponding figures |
| `Fig_4_and_7/` | SUM-SAM and Y-SUM sorted-effect workflows for Figures 4 and 7 |
| `Tab_3/` to `Tab_8/` | Code and supporting files for Tables 3-8 |
| `section_4_point_5/` | Monthly causal ML analyses for Section 4.5 and related appendix results |
| `REPLICATION_GUIDE.pdf` | Full data and code documentation |

Tables 1 and 2 in the manuscript are explanatory and are not generated from
data.

## Data availability

The repository includes the processed datasets needed to begin the main
replication exercises. It does not redistribute the original transaction-level
Colombian customs microdata.

The raw export and import data are publicly available from the Colombian
National Administrative Department of Statistics (DANE):

- [Exports microdata](https://microdatos.dane.gov.co/index.php/catalog/472/)
- [Imports microdata](https://microdatos.dane.gov.co/index.php/catalog/473/)

Users who download the raw data are responsible for complying with DANE's terms
and conditions. Instructions for rebuilding the processed datasets are provided
in the detailed replication guide.

## Reproducibility notes

- Random seeds are set where applicable, but software or package differences may
  produce small numerical differences.
- Full runs of the repeated-split and bootstrap procedures can take several
  hours.
- Large intermediate bootstrap and prediction files are generated locally and
  are not all included in the repository.

## Contact

Questions about the replication package may be sent to:

- Federico Nutarelli: federico.nutarelli@imtlucca.it
- Francesco Serti: francesco.serti@imtlucca.it
- Massimo Riccaboni: massimo.riccaboni@imtlucca.it
- Víctor Ortiz-Giménez: victor.ortiz.gimenez@bbva.com
- Marco Dueñas: maduenase@gmail.com

When using this repository or the joint p-value procedure documented in
`Tab_4/joint_p_values_github.pdf`, please cite the accompanying paper.
