[![DOI](https://zenodo.org/badge/265119113.svg)](https://zenodo.org/badge/latestdoi/265119113)

# Yarlagadda et al. (2022) Trade integration and climate mitigation in LAC


## Abstract
The Latin America and the Caribbean (LAC) region plays a key dual role in meeting global agricultural demands and maintaining carbon sinks due to its abundant land and water resources. The future evolution of agricultural production and trade presents an important yet uncertain opportunity for the region, but could also pose economic or environmental challenges. In this study we use the Global Change Analysis Model (GCAM) to evaluate the impacts of two global-scale drivers: agricultural market integration and land-based climate mitigation policy. We evaluate their individual and combined impact on agricultural production and trade across LAC’s economies, as well as the resulting multi-sectoral impacts on producers, consumers, and integrated land-water-climate systems across water-economy regions sub-regions. Increased global market integration results in increased agricultural production and trade revenues for many LAC economies due to their comparative advantages that enable increased market share. Climate mitigation measures on CO2 and non-CO2 greenhouse gases increase revenues due to increased agricultural prices from land competition. The combined outcomes from both drivers are complex, and sometimes non-linear, highlighting the importance of understanding the interactions between multiple drivers. Despite increased agricultural production and trade opportunities, there are significant trade-offs that require careful multi-sectoral planning, such as potential loss of livestock production when pursuing land-based climate mitigation strategies, increased consumer agricultural expenditures, and extreme changes in land-use or water withdrawals, resulting in deforestation or water scarcity pressures and the resulting environmental impacts. There is considerable heterogeneity in economic and environmental outcomes across regions and agricultural commodities, illustrating the value of considering outcomes at finer scales.

## Journal reference
Yarlagadda, B., T. Wild, X. Zhao, L. Clarke, R. Cui, A. Birnbaum, and J. Lamontagne (under review). . Earth's Future.

## Code reference

Human, I.M. (2021, April 14). Project/repo:v0.1.0 (Version v0.1.0). Zenodo. http://doi.org/some-doi-number/zenodo.7777777

## Data reference

### Input data
Reference for each minted data source for your input data.  For example:

Human, I.M. (2021). My input dataset name [Data set]. DataHub. https://doi.org/some-doi-number

### Output data
Reference for each minted data source for your output data.  For example:

Human, I.M. (2021). My output dataset name [Data set]. DataHub. https://doi.org/some-doi-number

## Contributing modeling software
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| model 1 | version | link to code repository | link to DOI dataset |
| model 2 | version | link to code repository | link to DOI dataset |
| component 1 | version | link to code repository | link to DOI dataset |

## Reproduce my experiment
Fill in detailed info here or link to other documentation that is a thorough walkthrough of how to use what is in this repository to reproduce your experiment.


1. Install the software components required to conduct the experiement from [Contributing modeling software](#contributing-modeling-software)
2. Download and install the supporting input data required to conduct the experiement from [Input data](#input-data)
3. Run the following scripts in the `workflow` directory to re-create this experiment:

| Script Name | Description | How to Run |
| --- | --- | --- |
| `step_one.py` | Script to run the first part of my experiment | `python3 step_one.py -f /path/to/inputdata/file_one.csv` |
| `step_two.py` | Script to run the last part of my experiment | `python3 step_two.py -o /path/to/my/outputdir` |

4. Download and unzip the output data from my experiment [Output data](#output-data)
5. Run the following scripts in the `workflow` directory to compare my outputs to those from the publication

| Script Name | Description | How to Run |
| --- | --- | --- |
| `compare.py` | Script to compare my outputs to the original | `python3 compare.py --orig /path/to/original/data.csv --new /path/to/new/data.csv` |

## Reproduce my figures
Use the scripts found in the `figures` directory to reproduce the figures used in this publication.

| Script Name | Description | How to Run |
| --- | --- | --- |
| `generate_figures.py` | Script to generate my figures | `python3 generate_figures.py -i /path/to/inputs -o /path/to/outuptdir` |