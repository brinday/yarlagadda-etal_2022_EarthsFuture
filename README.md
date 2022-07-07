
# Yarlagadda et al. (2022): Trade and Climate Mitigation Interactions in Latin America and the Caribbean


## Abstract
The Latin America and the Caribbean (LAC) region plays key roles in both meeting global agricultural demands and maintaining carbon sinks due to its abundant land and water resources. In this study we use the Global Change Analysis Model (GCAM) to evaluate the opportunities and challenges posed by two global-scale drivers: agricultural market integration (i.e. reduction of trade barriers) and land-based climate mitigation policy. We evaluate their potential individual and combined impacts on agricultural production and trade revenues across LAC’s economies through mid-century, as well as the resulting impacts on agricultural consumers and integrated land-water-climate systems across the region’s diverse sub-regions. Increased global market integration results in increased agricultural production and trade revenues for many LAC economies driven by their evolving comparative advantages. Climate mitigation measures on CO2 and non-CO2 greenhouse gases increase revenues due to increased agricultural prices from land competition and emissions abatement. The combined outcomes from both drivers are complex, and sometimes non-linear, highlighting the importance of understanding the interactions between multiple drivers. Despite increased agricultural production and trade opportunities, our results show that there are significant trade-offs that require careful multi-sectoral planning, such as emissions reduction challenges, potential loss of livestock production when pursuing land-based climate mitigation strategies, increased consumer expenditures, and changes in land-use or water withdrawals, resulting in deforestation or water scarcity pressures. There is considerable heterogeneity in economic and environmental outcomes across regions and agricultural commodities, illustrating the value of considering outcomes at finer scales.

## Journal reference
Yarlagadda, B., T. Wild, X. Zhao, L. Clarke, R. Cui, A. Birnbaum, and J. Lamontagne (under review). Earth's Future.

## Code and input data
Yarlagadda, B. (2022, July 7). GCAM-LAC-5.4_yarlagadda_etal_2022_trade_mitigation. Zenodo. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6807291.svg)](https://doi.org/10.5281/zenodo.6807291)

## Output data
Yarlagadda, B. (2022, July 7). yarlagadda_etal_2022_trade_mitigation Output Data. Zenodo. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6807540.svg)](https://doi.org/10.5281/zenodo.6807540)


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
