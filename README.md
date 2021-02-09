# washingtonCountiesCovid19

ECOL 8530 Group Project

Brandi Celia
Cody Dailey
Lambodhar Damodaran
Bodie Weedop
Rachel Xu


This README.md will describe the directory structure and content of this repo and explicitly detail the protocol / workflow / pipeline for analyses and research products. 


## Directory
- data
  + rawData
    + CEID County Data
      + county_popsize.rds
    + COVID-19 Case Data
      + WA_COVID19_Cases_Hospitalizations_Deaths.xlsx
    + Genetic Data
      + gisaid_hcov-19_2020_12_11_19.tsv
      + sequences.csv
    + Population Data
      + co-est2019-annres-53.xlsx
      + export.csv
    + Spatial
      + county10
        + county10.shp
- notebook
  + metadataAnalysis.html
  + metadataAnalysis.Rmd
- output
  + raxml
    + RAxML_bestTree.testrun
    + RAxML_bestTree.testrun.rooted
    + RAxML_result.testrun.RUN.1.rooted.tree
  + Rplot.png
  + Rplot01.png
  
  
  

## Protocol

Research question: *Do SARS-CoV-2 transmission dynamics exhibit metapopulation structuring in Washington, USA?*

Hypothesis: The geography of Washington, specifically the Cascade range, effectively divides the state into two regions of distinct, yet interconnected, SARS-CoV-2 transmission. 
  
Approaches: 
1. Empirical data analysis of COVID-19 case report time series to explore similarity among epidemic curves
2. Infectious disease modeling to explore potential metapopulation dynamics
3. Discrete-trait phylogeographic analysis to explore intermixing of SARS-CoV-2 strains



### Data

#### Sources


#### Management and Cleaning


### Analysis

#### Empirical Data Analysis
- Cross-correlations to identify lead / lag times
- Identify potential subdivisions of state
  + 1 (whole state) < N metapopulations < 39 counties
  + Thiessen polygons, network distance, ...
  

#### Infectious Disease Modeling

- SEIR? Deterministic? ID Model with metapops (maybe fit using Metropolis random walk?)
    + Define processes and compartments
      + Source initial parameter values and distributions?
      + Proposal distributions for MCMC
        + Uniform / noninformative
    + Objective function? 
      + SSE? For calibration
      + AIC for comparison of diff number metapops
      
      
#### Discrete-trait Phylogeography

- BEAST 1.10.4  - Bayesian Stochastic Search Variable Selection (BSSVS) 

