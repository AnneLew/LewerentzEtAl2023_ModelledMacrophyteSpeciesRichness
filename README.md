
# Potential change in the future spatial distribution of submerged macrophyte species and species richness: the role of today's lake type and strength of compounded environmental change

**Research Compendium**

<!-- badges: start -->
<!-- badges: end -->

*This repository is a R package which includes all data (model output from MGM), analysis files and results to reproduce the following publications given in the Journal reference.*


## Journal references

*Release 0.9* corresponds to a *Preprint*:

- add citation


## How to reproduce the results


## Data source

Source of raw data is: 

- Observed data: 
  - *Makrophyten_WRRL_05-17_nurMakrophytes.csv*:  Bayerisches Landesamt für Umwelt, www.lfu.bayern.de (Published under *Licence CC BY 4.0*)
  - *Morphology*: Bayerisches Landesamt für Umwelt, www.lfu.bayern.de (Published under *Licence CC BY 4.0*)
  - *Indexmakrophyten_Gruppenzuordnung*: Melzer, A. (1999). Aquatic macrophytes as tools for lake management. In D. M. Harper, B. Brierley, A. J. D. Ferguson, & G. Phillips (Eds.), The Ecological Bases for Lake and Reservoir Management (pp. 181–190). Springer Netherlands.
- MGM output: 
  - Output of [Macrophytes Growth Model](https://github.com/AnneLew/MGM). The input files and the code to create virtual species is stored in the folder *MGMinput*





## Structure of research compendium

-   `data-raw/`: Raw datasets of model output (*MGMoutput*) and observed data and R code to generate data in preparation files `data/`
-   `data/`: Cleaned data used for the analysis
-   `analysis/`: R code in .Rmd files to reproduce tables, figures and analysis of main file and Supplementary material






## Installation

You can install the development version of LewerentzEtAl2022 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AnneLew/LewerentzEtAl2022")
```


