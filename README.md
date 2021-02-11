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




#from google doc



Data
The Washington State Public Health Department has reliably updated data for SARS-CoV-2 incidence each week on Monday. Weekly incidence data is highly resolved at the spatial scale by including incidence for each county in the state.
Incidence data - NYT, JHU, Washington State Public Health Department (https://www.doh.wa.gov/Emergencies/COVID19/DataDashboard#tables)

	Currently, Washington state is one state leading the US in SARS-CoV-2 sequencing (n = 4590 from GenBank; n = 7070 from GISAID). Many SARS-CoV-2 sequences from WA have details about which county the sample was collected from.
Sequence data - GISAID/Genbank (w/ associated metadata)

Spatial / demographic - https://www.ofm.wa.gov/washington-data-research/population-demographics/gis-data/census-geographic-files

https://datacommons.org/place/geoId/53

	Mobility data: https://www.wsdot.wa.gov/mapsdata/tools/trafficplanningtrends.htm

Data and Analysis Management

Methods


Using sequence data

BEAST 1.10.4  - Bayesian Stochastic Search Variable Selection (BSSVS) 

A discrete trait diffusion model will be estimated to characterize the viral diffusion patterns across Washington state. Bayesian phylogenetic reconstruction will be performed using BEAST 1.10.4 which utilizes a markov chain monte carlo method to average across a posterior sampling of phylogenetic trees. This posterior tree set will be used to estimate the discrete trait transitions between character states across  the phylogenetic tree. The character states used in this diffusion model will be both the county of isolation and the custom region, as determined by the incidence data. The Bayesian Stochastic Search Variable Selection (BSSVS) will then be used as a method to infer the most supported rates between character states across phylogenies in the markov chain. These diffusion rates will be mapped to show diffusion across geographic space and can be related to geographic features, in particular the elevation and the cascade mountain range. 


Using incidence data:
SEIR? Deterministic? ID Model with metapops (maybe fit using Metropolis random walk?)
Define processes and compartments
Source initial parameter values and distributions?
Proposal distributions for MCMC
Uniform / noninformative
Objective function? 
SSE? For calibration
AIC for comparison of diff number metapops




Methods


Using sequence data

BEAST 1.10.4  - Bayesian Stochastic Search Variable Selection (BSSVS) 

A discrete trait diffusion model will be estimated to characterize the viral diffusion patterns across Washington state. Bayesian phylogenetic reconstruction will be performed using BEAST 1.10.4 which utilizes a markov chain monte carlo method to average across a posterior sampling of phylogenetic trees. This posterior tree set will be used to estimate the discrete trait transitions between character states across  the phylogenetic tree. The character states used in this diffusion model will be both the county of isolation and the custom region, as determined by the incidence data. The Bayesian Stochastic Search Variable Selection (BSSVS) will then be used as a method to infer the most supported rates between character states across phylogenies in the markov chain. These diffusion rates will be mapped to show diffusion across geographic space and can be related to geographic features, in particular the elevation and the cascade mountain range. 


Using incidence data:
SEIR? Deterministic? ID Model with metapops (maybe fit using Metropolis random walk?)
Define processes and compartments
Source initial parameter values and distributions?
Proposal distributions for MCMC
Uniform / noninformative
Objective function? 
SSE? For calibration
AIC for comparison of diff number metapops
