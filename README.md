Project title
================
by Joshua A. Harkness and Autumn L. Pauly

#### Introduction 
Pollutants, encompassing metals and
organic compounds from agricultural, industrial, and household sources,
have accumulated in bottom sediments in the Gulf of Maine. The U.S.
federal government, through its agencies (USEPA, USGS, and NOAA) and
laws (16 USC § 1447-1447f (1990); 33 USC § 1330 (1987)) has recognized
the need for assessment of the distribution of contaminants in marine
sediments. Our group is interested in assessing the concentration and
distribution of potentially harmful contaminants in the marine
environment in order for others to better understand and predict their
impact.

For this project, we used data from the Gulf of Maine Contaminated
Sediments Database published by the U.S. Geological Survey in 2002. This
dataset is the result of a collaborative effort of several surveys from
the U.S. Geological Survey Coastal and Marine Geology Program, Bigelow
Laboratory for Ocean Sciences, Woods Hole Oceanographic Institution, the
University of New Hampshire, and the University of Massachusetts.
Methodology included samples that were collected from the sea floor
using a “grab” or “core” method, which were then analyzed for
measurements of major and trace elements, organic contaminant compounds,
metals, and sediment textural data.

#### Missing Data and Skewness

While working with this dataset, we noticed that there was a number of
missing values for

#### Summary and Findings of Contaminates and Concentrations

The broad question that we looked at was about which contaminants are in
the Gulf of Maine and what this means for the environment. Of all the organics present in the Gulf of Maine, the most prevalent PCB by average was PCB 153 (10.59 ng/g), followed by PCB 138 (9.43 ng/g), PCB 101 (7.76 ng/g), and PCB 118 (7.12 ng/g). The most prevalent pesticides on average were DDT (0.17 ng/g), Aldrin (0.24 ng/g), Mirex (0.26 ng/g), and Lindane (1.61 ng/g). An analysis of these contaminants revealed that the highest average concentration and distribution was located in Boston Harbor, MA.

The spatial distribution of PAHs reflected that of PCBs and pesticides. Of all the PAHs present in the Gulf of Maine, the most prevalent PAH by average was Fluoranthene (962.25 ng/g), followed by Pyrene (934.50 ng/g), Chrysene (887.80 ng/g), and Phenanthrene (546.57 ng/g). Boston Harbor had the highest mean concentration of PAHs, where the highest concentrations were of Chrysene (2,234.89 ng/g), Pyrene (2,197.45 ng/g), Fluoranthene (2,016.27 ng/g), and Phenanthrene (1,307.48 ng/g). This was drastically higher than the average concentrations that were found in the Penobscot to Mount Desert Island region, where the highest concentrations were of Fluoranthene (723.32 ng/g) and Pyrene (1845.59 ng/g). 

## Navigating the Project Folder

There are a series of subfolders that are contained within this project,
including RMD, MD, and .shapefiles.

For the data dictionary, navigate to the `Readme.md` file in the `data`
folder. This contains what each dataset contains, as well as what each
variable represents.

To begin analyzing the data, navigate to the `data_tidying.rmd` in the
`analysis` folder. This contains the code that is needed to clean and
pivot the USGS datasets into readable formats. These cleaned files are
the base files for the next .Rmd files.

For PCB analysis, navigate to the `PCBs.rmd` in the `analysis` folder. This .Rmd file analyzes the distributions and concentrations of Polycyclic aromatic hydrocarbon (PAH) contaminants. 

For PAH analysis, navigate to the `PAHs.rmd` in the `analysis` folder.
This .Rmd file analyzes the distributions and concentrations of two organic contaminants,
Polychlorinated biphenyls (PCB) and pesticide. 

## Presentation

Our presentation can be found
[here](https://docs.google.com/presentation/d/1LWPEdxH3TlTmRcBp79jHunibIEL1xaWUlSAA-Bt3cE8/edit?usp=sharing).

## Acknowledgements

We are incredibly grateful for the agencies and organizations in the
Gulf of Maine area who provided this data in published reports, theses,
unpublished data, such as the U.S. Environmental Protection Agency, U.S.
Army Corps of Engineers, National Status and Trends Benthic Surveillance
program, NOAA, and U.S. Geological Survey. This includes the individuals
who participated in the collection, interpretation, and compilation of
these datasets, including but not limited to M. Buchholtz ten Brink, F.
Manheim, E. Mecray, M. Hastings, J. Currence, J. Farrington, B. Tripp,
S. Jones, P. Larsen, G. Wallace, and L. Ward. We give our thanks to the
Gulf of Maine Regional Marine Research Program for funding their
project. Finally, we are eternally grateful for Dr. Laurie Baker, who
provided us the instruction, guidance and advice necessary for us to
enhance our understanding of R and data reliability, as well as the
importance of team collaboration.

## Data

#### Data Summary

Our data came from the Gulf of Maine Contaminated Sediments Database
published by the U.S. Geological Survey in their Open-File Report
02-403. The full citation list for sources of data that was entered into
the Contaminated Sediments Database for the Gulf of Maine can be found
here: <https://pubs.usgs.gov/of/2002/of02-403/HTMLdocs/entdocs.html>.
The Database of Contaminated Sediments for the Gulf of Maine is a
carefully compiled collection of edited data on contaminated sediments
and associated properties, sourced from diverse outlets. The data was
reviewed for quality, standardized by format, and integrated into a
regional database. The resulting database includes original data from
various sources and includes documentation regarding data quality. Its
purpose is to assess the environmental status of coastal sediments,
understand contaminant transport paths, and determine the fate of
pollutants in the region.

#### Data Citation

Buchholtz ten Brink, M.R., Manheim, F.T., Mecray, E.L. , Hastings, M.E.,
and Currence, J.M., along with Farrington, J.W., Fredette, T.J., Jones,
S.H., Liebman, M.L., Larsen, P.F., Smith Leo, W., Tripp, B.W., Wallace,
Jr., G.T., and Ward, L.G., 2002, Contaminated sediments database for the
Gulf of Maine, U.S. Geological Survey Open-file Report No. 02-403,
Online at <https://pubs.usgs.gov/of/2002/of02-403/>. doi:
10.3133/ofr02403. Accessed 10/12/2023.

## References

##### These references have been cited in `PAHs.Rmd` and `PCBs.Rmd`.

CDC. (2019, May 24). Polycyclic Aromatic Hydrocarbons (PAHs) Factsheet | National Biomonitoring Program | CDC. Www.cdc.gov. https://www.cdc.gov/biomonitoring/PAHs_FactSheet.html

Clark, R,B., 1999. Marine pollution. Oxford University press, Fourth
edition, pp 161.

Honda, M., & Suzuki, N. (2020). Toxicities of Polycyclic Aromatic
Hydrocarbons for Aquatic Animals. International journal of environmental
research and public health, 17(4), 1363.
<https://doi.org/10.3390/ijerph17041363>

IARC. 2012. IARC Monographs on the Evaluation of Carcinogenic Risks to
Humans. International Agency for Research on Cancer; Lyon, France.

IARC (2010). Some non-heterocyclic polycyclic aromatic hydrocarbons and
some related exposures. IARC Monogr Eval Carcinog Risks Hum. 92:1–853.
Available from: <http://publications.iarc.fr/110> <PMID:21141735>

James R.C., Busch H., Tamburro C.H., Roberts S.M., Schell J.D., Harbison
R.D. Polychlorinated Biphenyl Exposure and Human Disease. J. Occup.
Environ. Med. 1993;35:136–148. doi: 10.1097/00043764-199302000-00014.

Kennish, M. J. (1996): Practical Handbook of Estuarine and Marine
Pollution, CRC Press: 524.

Mumtaz, M. M., George, J. D., Gold, K. W., Cibulas, W., & DeRosa, C. T.
(1996). ATSDR evaluation of health effects of chemicals. IV. Polycyclic
aromatic hydrocarbons (PAHs): understanding a complex problem.
Toxicology and industrial health, 12(6), 742–971.
<https://doi.org/10.1177/074823379601200601>
