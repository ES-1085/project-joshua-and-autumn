GOM Contaminated Sediments analysis: PCBs
================
Joshua Harkness and Autumn Pauly
2023-10-28

### Introduction

This document is an analysis of distributions and concentrations of PCB
and Organic contaminants that were reported in a publication by the U.S.
Geological Survey on contaminated sediments in the Gulf of Maine.

### Loading packages

These are the packages that will be used for analysis.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE

``` r
library(leaflet)
library(RColorBrewer)
#install.packages("dplyr")
library(dplyr)
```

### Loading PCB and Organics Datasets

This loads the datasets from .csv files and reads them into the
environment.

``` r
PCBs <- read.csv(paste0("/cloud/project/data/datasets_csv/PCBs_loc.csv"), header = T)
Organics <- read.csv(paste0("/cloud/project/data/datasets_csv/Organics_loc.csv"), header = T)
```

### Glimpsing the PCB and Organics Datasets

These data sets are quite large and contain a lot of information.
Pivoting will need to be done to restructure this into a format that
will be easier to interpret.

``` r
glimpse(PCBs)
```

    ## Rows: 7,848
    ## Columns: 39
    ## $ UNIQUE_ID  <chr> "US00001", "US00002", "US00003", "US00004", "US00005", "US0…
    ## $ LATITUDE   <dbl> 42.35972, 42.36028, 42.38500, 42.38500, 42.38500, 42.38500,…
    ## $ LONGITUDE  <dbl> -71.02861, -71.02778, -71.04611, -71.04611, -71.04611, -71.…
    ## $ SOUNDING_M <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ STATE_NAME <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA",…
    ## $ QUAD_NAME  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ GEN_LOC_NM <chr> "Boston Inner Harbor", "Boston Inner Harbor", "Boston Inner…
    ## $ SPECFC_LOC <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BI…
    ## $ AREA_CODE  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ SAMP_DATE1 <chr> NA, NA, "5/1/1981", "5/1/1981", "5/1/1981", "5/1/1981", "5/…
    ## $ TO_SMP_DT2 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DPTH_N_COR <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0"…
    ## $ DPTH_CODE  <chr> "Unknown", "Unknown", "Depth", "Depth", "Unknown", "Unknown…
    ## $ COR_GRB_CD <chr> "Grab", "Grab", "Core", "Core", "Grab", "Grab", "Grab", "Gr…
    ## $ site       <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BI…
    ## $ PCB_52_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB101_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB118_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB128_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB138_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB153_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB180_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB206_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB209_NGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DDT_4_4_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DDT_2_4_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DDE_4_4_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DDD_4_4_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ ENDRIN_C   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ ENDR_ALD_C <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ ALDRIN_C   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DIELDRN_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ CLRDNE_T_C <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ MIREX_C    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ METHOXYCLC <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BHC_A_C    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BHC_B_C    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BHC_D_C    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ LINDANE_C  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

``` r
glimpse(Organics)
```

    ## Rows: 7,849
    ## Columns: 35
    ## $ UNIQUE_ID  <chr> "US00001", "US00002", "US00003", "US00004", "US00005", "US0…
    ## $ LATITUDE   <dbl> 42.35972, 42.36028, 42.38500, 42.38500, 42.38500, 42.38500,…
    ## $ LONGITUDE  <dbl> -71.02861, -71.02778, -71.04611, -71.04611, -71.04611, -71.…
    ## $ SOUNDING_M <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ STATE_NAME <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA",…
    ## $ QUAD_NAME  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ GEN_LOC_NM <chr> "Boston Inner Harbor", "Boston Inner Harbor", "Boston Inner…
    ## $ SPECFC_LOC <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BI…
    ## $ AREA_CODE  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ SAMP_DATE1 <chr> NA, NA, "5/1/1981", "5/1/1981", "5/1/1981", "5/1/1981", "5/…
    ## $ TO_SMP_DT2 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DPTH_N_COR <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0"…
    ## $ DPTH_CODE  <chr> "Unknown", "Unknown", "Depth", "Depth", "Unknown", "Unknown…
    ## $ COR_GRB_CD <chr> "Grab", "Grab", "Core", "Core", "Grab", "Grab", "Grab", "Gr…
    ## $ site       <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BI…
    ## $ REPNO_ORG  <int> 1, 1, 1, NA, NA, 1, 1, NA, 1, NA, 1, 1, 1, 1, 1, 1, 1, NA, …
    ## $ TOTREP_ORG <int> 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 1, 1,…
    ## $ TVS_EP_PCT <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 10.00, 9.50, 9.…
    ## $ O_G_PCT    <dbl> 0.05, 0.06, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ O_G_UGG    <dbl> NA, NA, NA, NA, 10882.00, 3115.00, 880.00, 542.30, 31.44, N…
    ## $ PHCTOT_PCT <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PHCTOT_UGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PCB_T_UGG  <dbl> NA, NA, NA, NA, 0.500, 0.500, 0.500, 0.500, 0.500, NA, NA, …
    ## $ DDT_T_NGG  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 76.50, 33.00, 1…
    ## $ DDE_T_NGG  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DDD_T_NGG  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PEST_UG_G  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PAHTOT_PCT <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ PAHTOT_UGG <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ LIPIDS_NGG <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ CLOST_SP_G <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ MBT_C      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ DBT_C      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ TBT_C      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ TTBT_C     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

## Data Transformation

### Pivot Organics longer

JH – the organics dataset has already been joined with the stations
dataset (loaded “Organics_loc.csv” above), so the data now needs to be
pivoted into R format.

``` r
Organics_long <- Organics %>%
 pivot_longer(cols = `REPNO_ORG`:`TTBT_C`, 
               names_to = "organic_detected", 
               values_to = "amount_detected")

Organics_long = select(Organics_long, c(UNIQUE_ID, LATITUDE, LONGITUDE, SOUNDING_M, STATE_NAME, QUAD_NAME, GEN_LOC_NM, SPECFC_LOC, AREA_CODE, SAMP_DATE1, TO_SMP_DT2, DPTH_N_COR, DPTH_CODE, COR_GRB_CD, organic_detected, amount_detected))

glimpse(Organics_long)
```

    ## Rows: 156,980
    ## Columns: 16
    ## $ UNIQUE_ID        <chr> "US00001", "US00001", "US00001", "US00001", "US00001"…
    ## $ LATITUDE         <dbl> 42.35972, 42.35972, 42.35972, 42.35972, 42.35972, 42.…
    ## $ LONGITUDE        <dbl> -71.02861, -71.02861, -71.02861, -71.02861, -71.02861…
    ## $ SOUNDING_M       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ STATE_NAME       <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA",…
    ## $ QUAD_NAME        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ GEN_LOC_NM       <chr> "Boston Inner Harbor", "Boston Inner Harbor", "Boston…
    ## $ SPECFC_LOC       <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH…
    ## $ AREA_CODE        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ SAMP_DATE1       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ TO_SMP_DT2       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ DPTH_N_COR       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ DPTH_CODE        <chr> "Unknown", "Unknown", "Unknown", "Unknown", "Unknown"…
    ## $ COR_GRB_CD       <chr> "Grab", "Grab", "Grab", "Grab", "Grab", "Grab", "Grab…
    ## $ organic_detected <chr> "REPNO_ORG", "TOTREP_ORG", "TVS_EP_PCT", "O_G_PCT", "…
    ## $ amount_detected  <dbl> 1.00, 1.00, NA, 0.05, NA, NA, NA, NA, NA, NA, NA, NA,…

### AP - Placing this here for the time being

``` r
Organics_long_no_na_no_zero <- Organics_long %>%
  drop_na(amount_detected) %>%
  filter(amount_detected != "0")
```

### Pivoting PCBs into a Longer Format

This function creates two columns to properly contain the values of PCBs
measured during these surveys - the `pcb` column contains the type of
PCB that was measured and the `amount_detected` column contains the
values for each measurement.

``` r
PCBs_long <- PCBs %>%
  pivot_longer(cols = `PCB_52_NGG`:`LINDANE_C`, 
               names_to = "pcb", 
               values_to = "amount_detected")

glimpse(PCBs_long)
```

    ## Rows: 188,352
    ## Columns: 17
    ## $ UNIQUE_ID       <chr> "US00001", "US00001", "US00001", "US00001", "US00001",…
    ## $ LATITUDE        <dbl> 42.35972, 42.35972, 42.35972, 42.35972, 42.35972, 42.3…
    ## $ LONGITUDE       <dbl> -71.02861, -71.02861, -71.02861, -71.02861, -71.02861,…
    ## $ SOUNDING_M      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ STATE_NAME      <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", …
    ## $ QUAD_NAME       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ GEN_LOC_NM      <chr> "Boston Inner Harbor", "Boston Inner Harbor", "Boston …
    ## $ SPECFC_LOC      <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH"…
    ## $ AREA_CODE       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ SAMP_DATE1      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ TO_SMP_DT2      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ DPTH_N_COR      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ DPTH_CODE       <chr> "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",…
    ## $ COR_GRB_CD      <chr> "Grab", "Grab", "Grab", "Grab", "Grab", "Grab", "Grab"…
    ## $ site            <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH"…
    ## $ pcb             <chr> "PCB_52_NGG", "PCB101_NGG", "PCB118_NGG", "PCB128_NGG"…
    ## $ amount_detected <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

### Creating Pesticides Data Set

This is creating a data set that only contains the pesticides that were
measured.

``` r
pesticides <- PCBs_long %>%
  mutate(pcb = fct_recode(pcb,
                          "DDT_C" = "DDT_2_4_C",
                          "DDT_C" = "DDT_4_4_C",
                          "BHC_C" = "BHC_A_C",
                          "BHC_C" = "BHC_B_C",
                          "BHC_C" = "BHC_D_C")) %>%
  filter(pcb %in% c("DDT_C", "DDE_4_4_C", "DDD_4_4_C", "ENDRIN_C", "ENDR_ALD_C", "ALDRIN_C", "DIELDRN_C", "CLRDNE_T_C", "MIREX_C", "METHOXYCLC", "BHC_C", "LINDANE_C"))

glimpse(pesticides)
```

    ## Rows: 117,720
    ## Columns: 17
    ## $ UNIQUE_ID       <chr> "US00001", "US00001", "US00001", "US00001", "US00001",…
    ## $ LATITUDE        <dbl> 42.35972, 42.35972, 42.35972, 42.35972, 42.35972, 42.3…
    ## $ LONGITUDE       <dbl> -71.02861, -71.02861, -71.02861, -71.02861, -71.02861,…
    ## $ SOUNDING_M      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ STATE_NAME      <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", …
    ## $ QUAD_NAME       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ GEN_LOC_NM      <chr> "Boston Inner Harbor", "Boston Inner Harbor", "Boston …
    ## $ SPECFC_LOC      <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH"…
    ## $ AREA_CODE       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ SAMP_DATE1      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ TO_SMP_DT2      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ DPTH_N_COR      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ DPTH_CODE       <chr> "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",…
    ## $ COR_GRB_CD      <chr> "Grab", "Grab", "Grab", "Grab", "Grab", "Grab", "Grab"…
    ## $ site            <chr> "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH", "BIH"…
    ## $ pcb             <fct> DDT_C, DDT_C, DDE_4_4_C, DDD_4_4_C, ENDRIN_C, ENDR_ALD…
    ## $ amount_detected <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

## Data Visualizations

### Descriptive Visualizations

First, the number of observations collected at each location will be
visualized. This bar plot uses number of observations on its y axis,
thus reflecting sampling intensity by general location. The greatest
number of observations are in the Gulf of Maine below the 50m isobath
(partly this is a product of area extent as well as sampling intensity);
Massachusetts Bays, Boston Harbor sites, and the MA/NH/ME coast are all
generally heavily sampled.

``` r
Organics %>%
  ggplot(aes(x = fct_infreq(GEN_LOC_NM), fill = fct_infreq(GEN_LOC_NM))) +
  geom_bar(stat = "count", color = "black") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(title = "Number of Observations by General Location",
       subtitle = "for PCB Contaminents",
       x = "General Location",
       y = "Count (n Observations)")
```

![](PCBs_files/figure-gfm/n-obs-gen-loc-1.png)<!-- -->

AP - What is this graph showing us? Do we need it?

``` r
ggplot(Organics, aes(x = PCB_T_UGG)) +
geom_histogram(fill = "skyblue", color = "black")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 6243 rows containing non-finite values (`stat_bin()`).

![](PCBs_files/figure-gfm/hist-pcb-total-1.png)<!-- -->

### PCB Site to Site Comparisons

#### General PCB Summary Statistics

The table below is visualizing the mean, standard deviation, count, and
principal square root values of PCB concentrations for the general
locations.

Boston Inner Harbor has the highest mean PCB concentration (average of
44.21 ng/g per observation), followed by Cape Ann to Cape Elizabeth
(average of 26.90 ng/g per observation) and Southeast Boston Harbor
(average of 18.35 ng/g per observation). Boston Inner Harbor (average
standard deviation of 284.81) and Cape Ann to Cape Elizabeth (average
standard deviation of 227.34) have the highest standard deviations,
suggesting that, though they are the locations with the highest average
PCB concentrations, the counts per observation vary greatly. The ranges
of both locations is 3,000 ng/g to 0 ng/g, which is a wide range of
values.

``` r
Summary_Organics <- Organics %>%
  group_by(GEN_LOC_NM) %>%
  drop_na(PCB_T_UGG) %>%
  reframe(mean_PCB_T = mean(PCB_T_UGG),
    sd_PCB_T = sd(PCB_T_UGG), 
    range_PCB_T = range(PCB_T_UGG),
    n_PCB_T = n(),
    SE_PCB_T = sd(PCB_T_UGG) / sqrt(n()))

Summary_Organics
```

    ## # A tibble: 24 × 6
    ##    GEN_LOC_NM                 mean_PCB_T sd_PCB_T range_PCB_T n_PCB_T SE_PCB_T
    ##    <chr>                           <dbl>    <dbl>       <dbl>   <int>    <dbl>
    ##  1 Boston Inner Harbor           44.2    285.          0          119 26.1    
    ##  2 Boston Inner Harbor           44.2    285.       3000          119 26.1    
    ##  3 Cape Ann to Cape Elizabeth    26.9    227.          0          190 16.5    
    ##  4 Cape Ann to Cape Elizabeth    26.9    227.       3000          190 16.5    
    ##  5 Cape Code Bay                  0.0113   0.0137      0           34  0.00235
    ##  6 Cape Code Bay                  0.0113   0.0137      0.0377      34  0.00235
    ##  7 Cape Elizabeth to Rockland     4.29    25.3         0          187  1.85   
    ##  8 Cape Elizabeth to Rockland     4.29    25.3       200          187  1.85   
    ##  9 Central Boston Harbor          0.416    0.274       0.072       41  0.0427 
    ## 10 Central Boston Harbor          0.416    0.274       0.84        41  0.0427 
    ## # ℹ 14 more rows

The graph below is plotting the mean PCB concentration (ng/g) found at
each general location with error bards accounting for one standard
error. As was reflected upon above, Boston Inner Harbor, Cape Ann to
Cape Elizabeth, and Southeast Boston Harbor have the highest
concentrations of PCBs.

``` r
Summary_Organics %>%
  ggplot(aes(x = fct_rev(fct_reorder(GEN_LOC_NM, mean_PCB_T)), y = sapply(mean_PCB_T, FUN=function(x) ifelse(x==0.000000e0, -1,x) ), fill = GEN_LOC_NM)) +
  geom_col(col = "black") +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  coord_flip() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Mean PCB Concentrations in Gulf of Maine Sediments",
       x = "General location",
       y = "Mean total PCB concentration ug/g",
       caption = "Error bars = 1 standard error")
```

![](PCBs_files/figure-gfm/pcb-gen-loc-1.png)<!-- -->

#### PCB Summary Statistics of Boston Harbor

This graph shows the PCB concentrations in Boston Harbor, comparing all
locations.

``` r
Summary_Organics %>%
  filter(GEN_LOC_NM %in% c("Boston Inner Harbor", "Central Boston Harbor", "Southeast Boston Harbor", "Northwest Boston Harbor")) %>%
  ggplot(aes(x = fct_rev(fct_reorder(GEN_LOC_NM, mean_PCB_T)), y = mean_PCB_T, fill = GEN_LOC_NM)) +
  geom_col(col = "black") +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_brewer(type = "qual", palette = 4, direction = 1, aesthetics = "fill") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "PCB Concentrations in Boston Harbor Sediments",
       x = "General location",
       y = "Mean total PCB concentration ug/g",
       caption = "Error bars = 1 standard error")
```

![](PCBs_files/figure-gfm/pcb-boston-harbor1-1.png)<!-- -->

This graph shows the PCB concentrations in Boston Harbor, comparing
Central Boston Harbor and Northwest Boston Harbor.

``` r
Summary_Organics %>%
  filter(GEN_LOC_NM %in% c("Central Boston Harbor", "Northwest Boston Harbor")) %>%
  ggplot(aes(x = fct_reorder(GEN_LOC_NM, mean_PCB_T), y = mean_PCB_T, fill = GEN_LOC_NM)) +
  geom_col(col = "black") +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_brewer(type = "qual", palette = 4, direction = 1, aesthetics = "fill") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "PCB Concentrations in Boston Harbor Sediments",
       subtitle = "Central and Northwest Boston Harbor",
       x = "General location",
       y = "Mean total PCB concentration ug/g",
       caption = "Error bars = 1 standard error")
```

![](PCBs_files/figure-gfm/pcb-boston-harbor2-1.png)<!-- -->

\##Visualizing Specific PCB Concentrations \### PCB Summary Statistics
of Locations With Highest Concentrations This graph identifies the
locations with the highest concentrations of PCBs.

``` r
Summary_Organics %>%
  filter(GEN_LOC_NM %in% c("Boston Inner Harbor", "Cape Ann to Cape Elizabeth", "Southeast Boston Harbor", "Cape Elizabeth to Rockland", "Rockland to north")) %>%
  ggplot(aes(x = fct_rev(fct_reorder(GEN_LOC_NM, mean_PCB_T)), y = mean_PCB_T, fill = GEN_LOC_NM)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_brewer(type = "qual", palette = 4, direction = 1, aesthetics = "fill") +
  theme_minimal() +
  scale_fill_viridis_d()+
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Locations with the highest PCB Concentrations",
       x = "General location",
       y = "Mean total PCB concentration ug/g",
       caption = "Error bars = 1 standard error")
```

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

![](PCBs_files/figure-gfm/pcb-highest-concentrations-1.png)<!-- -->

#### Boston Inner Harbor Concentrations

This plot visualizes the specific organics present in Boston Inner
Harbor.

As shown in the plot, Oil and Grease in sediments (173,938.40 moles/g)
has the highest concentration in the harbor, followed by Petroleum
Hydrocarbons (134,266.0 percent dry weight), and then Clostridium
perfringens (99,230.00 spores/g).

``` r
Organics_long_no_na_no_zero %>%
  group_by(GEN_LOC_NM, organic_detected) %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  filter(GEN_LOC_NM == "Boston Inner Harbor") %>%
  arrange(desc(amount_detected))
```

    ## `summarise()` has grouped output by 'GEN_LOC_NM'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 17 × 3
    ## # Groups:   GEN_LOC_NM [1]
    ##    GEN_LOC_NM          organic_detected amount_detected
    ##    <chr>               <chr>                      <dbl>
    ##  1 Boston Inner Harbor O_G_UGG                173938.  
    ##  2 Boston Inner Harbor PHCTOT_UGG             134266   
    ##  3 Boston Inner Harbor CLOST_SP_G              99230   
    ##  4 Boston Inner Harbor PCB_T_UGG                5261.  
    ##  5 Boston Inner Harbor PAHTOT_UGG               1599.  
    ##  6 Boston Inner Harbor TBT_C                    1340.  
    ##  7 Boston Inner Harbor DDT_T_NGG                 902.  
    ##  8 Boston Inner Harbor TVS_EP_PCT                560.  
    ##  9 Boston Inner Harbor DBT_C                     284.  
    ## 10 Boston Inner Harbor REPNO_ORG                  97   
    ## 11 Boston Inner Harbor TOTREP_ORG                 93   
    ## 12 Boston Inner Harbor MBT_C                      79.4 
    ## 13 Boston Inner Harbor PAHTOT_PCT                 41.4 
    ## 14 Boston Inner Harbor O_G_PCT                    21.6 
    ## 15 Boston Inner Harbor TTBT_C                      5.08
    ## 16 Boston Inner Harbor LIPIDS_NGG                  3   
    ## 17 Boston Inner Harbor PHCTOT_PCT                  0.88

``` r
Organics_long_no_na_no_zero %>%
  filter(GEN_LOC_NM %in% c("Boston Inner Harbor")) %>%
  ggplot(aes(x = organic_detected, y = amount_detected, fill = organic_detected))+
  geom_col()+
  theme_minimal()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Organic Concentrations in Boston Inner Harbor Sediments",
       x = "Organic Detected",
       y = "Amount Detected (ng/g)")
```

![](PCBs_files/figure-gfm/type_of_organic-in-boston-inner-harbor-1.png)<!-- -->

#### Cape Ann to Cape Elizabeth Concentrations

This plot visualizes the specific organics present in Cape Ann to Cape
Elizabeth.

As shown in the plot, Clostridium perfringens (724,143.53 spores/g) is
PCB with the highest concentration.

``` r
Organics_long_no_na_no_zero %>%
  group_by(GEN_LOC_NM, organic_detected) %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  filter(GEN_LOC_NM == "Cape Ann to Cape Elizabeth") %>%
  arrange(desc(amount_detected))
```

    ## `summarise()` has grouped output by 'GEN_LOC_NM'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 9 × 3
    ## # Groups:   GEN_LOC_NM [1]
    ##   GEN_LOC_NM                 organic_detected amount_detected
    ##   <chr>                      <chr>                      <dbl>
    ## 1 Cape Ann to Cape Elizabeth CLOST_SP_G               724144.
    ## 2 Cape Ann to Cape Elizabeth PCB_T_UGG                  5112.
    ## 3 Cape Ann to Cape Elizabeth PAHTOT_UGG                 1011.
    ## 4 Cape Ann to Cape Elizabeth DDT_T_NGG                   480.
    ## 5 Cape Ann to Cape Elizabeth O_G_UGG                     383 
    ## 6 Cape Ann to Cape Elizabeth TOTREP_ORG                   98 
    ## 7 Cape Ann to Cape Elizabeth REPNO_ORG                    91 
    ## 8 Cape Ann to Cape Elizabeth PEST_UG_G                    84 
    ## 9 Cape Ann to Cape Elizabeth TVS_EP_PCT                   14

``` r
Organics_long_no_na_no_zero %>%
  filter(GEN_LOC_NM %in% c("Cape Ann to Cape Elizabeth")) %>%
  ggplot(aes(x = organic_detected, y = amount_detected, fill = organic_detected))+
  geom_col()+
  theme_minimal()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Organic Concentrations in Cape Ann to Cape Elizabeth Sediments",
       x = "Organic Detected",
       y = "Amount Detected (ng/g)")
```

![](PCBs_files/figure-gfm/type_of_organic-in-cape-ann-to-elizabeth-1.png)<!-- -->

#### Southeast Boston Harbor Concentrations

This plot visualizes the specific organics present in Southeast Boston
Harbor.

As shown in the plot, Clostridium perfringens (89,223 spores/g) has the
highest concentration in the harbor, followed by Oil and Grease in
sediments (74,285 moles/g), and Petroleum Hydrocarbons (in units of
percent dry weight).

``` r
Organics_long_no_na_no_zero %>%
  group_by(GEN_LOC_NM, organic_detected) %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  filter(GEN_LOC_NM == "Southeast Boston Harbor") %>%
  arrange(desc(amount_detected))
```

    ## `summarise()` has grouped output by 'GEN_LOC_NM'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 17 × 3
    ## # Groups:   GEN_LOC_NM [1]
    ##    GEN_LOC_NM              organic_detected amount_detected
    ##    <chr>                   <chr>                      <dbl>
    ##  1 Southeast Boston Harbor CLOST_SP_G             89223    
    ##  2 Southeast Boston Harbor O_G_UGG                74285    
    ##  3 Southeast Boston Harbor PHCTOT_UGG             12025    
    ##  4 Southeast Boston Harbor PCB_T_UGG                606.   
    ##  5 Southeast Boston Harbor TVS_EP_PCT               425.   
    ##  6 Southeast Boston Harbor PAHTOT_UGG                96.6  
    ##  7 Southeast Boston Harbor TBT_C                     56.7  
    ##  8 Southeast Boston Harbor REPNO_ORG                 40    
    ##  9 Southeast Boston Harbor TOTREP_ORG                35    
    ## 10 Southeast Boston Harbor DDT_T_NGG                 34.4  
    ## 11 Southeast Boston Harbor DBT_C                     17    
    ## 12 Southeast Boston Harbor PAHTOT_PCT                16.6  
    ## 13 Southeast Boston Harbor O_G_PCT                   11.2  
    ## 14 Southeast Boston Harbor MBT_C                      6    
    ## 15 Southeast Boston Harbor TTBT_C                     0.6  
    ## 16 Southeast Boston Harbor DDE_T_NGG                  0.057
    ## 17 Southeast Boston Harbor DDD_T_NGG                  0.047

``` r
Organics_long_no_na_no_zero %>%
  filter(GEN_LOC_NM %in% c("Southeast Boston Harbor")) %>%
  ggplot(aes(x = organic_detected, y = amount_detected, fill = organic_detected))+
  geom_col()+
  theme_minimal()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Organic Concentrations in Southeast Boston Harbor Sediments",
       x = "Organic Detected",
       y = "Amount Detected (ng/g)")
```

![](PCBs_files/figure-gfm/type_of_organic-in-boston-southeast-harbor-1.png)<!-- -->

#### Cape Elizabeth to Rockland Concentrations

This plot visualizes the specific organics present from Cape Elizabeth
to Rockland. As shown in the plot, DDD compounds (9,901.23 ng/g) is PCB
with the highest concentration followed by PCBs (802.64 ng/g).

``` r
Organics_long_no_na_no_zero %>%
  group_by(GEN_LOC_NM, organic_detected) %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  filter(GEN_LOC_NM == "Cape Elizabeth to Rockland") %>%
  arrange(desc(amount_detected))
```

    ## `summarise()` has grouped output by 'GEN_LOC_NM'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 2 × 3
    ## # Groups:   GEN_LOC_NM [1]
    ##   GEN_LOC_NM                 organic_detected amount_detected
    ##   <chr>                      <chr>                      <dbl>
    ## 1 Cape Elizabeth to Rockland DDT_T_NGG                  9901.
    ## 2 Cape Elizabeth to Rockland PCB_T_UGG                   803.

``` r
Organics_long_no_na_no_zero %>%
  filter(GEN_LOC_NM %in% c("Cape Elizabeth to Rockland")) %>%
  ggplot(aes(x = organic_detected, y = amount_detected, fill = organic_detected))+
  geom_col()+
  theme_minimal()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Organic Concentrations in Cape Elizabeth to Rockland Sediments",
       x = "Organic Detected",
       y = "Amount Detected (ng/g)")
```

![](PCBs_files/figure-gfm/type_of_organic-from-cape-elizabeth-to-rockland-1.png)<!-- -->

#### Rockland to North Concentrations

This plot visualizes the specific organics present from Rockland, ME to
any area northward. As shown in the plot, DDD compounds (1,303.10 ng/g)
is PCB with the highest concentration followed by PCBs (354.77 ng/g).

``` r
Organics_long_no_na_no_zero %>%
  group_by(GEN_LOC_NM, organic_detected) %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  filter(GEN_LOC_NM == "Rockland to north") %>%
  arrange(desc(amount_detected))
```

    ## `summarise()` has grouped output by 'GEN_LOC_NM'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 2 × 3
    ## # Groups:   GEN_LOC_NM [1]
    ##   GEN_LOC_NM        organic_detected amount_detected
    ##   <chr>             <chr>                      <dbl>
    ## 1 Rockland to north DDT_T_NGG                  1303.
    ## 2 Rockland to north PCB_T_UGG                   355.

``` r
Organics_long_no_na_no_zero %>%
  filter(GEN_LOC_NM %in% c("Rockland to north")) %>%
  ggplot(aes(x = organic_detected, y = amount_detected, fill = organic_detected))+
  geom_col()+
  theme_minimal()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Organic Concentrations in Rockland to North Sediments",
       x = "Organic Detected",
       y = "Amount Detected (ng/g)")
```

![](PCBs_files/figure-gfm/type_of_organic-in-rockalnd-to-N-1.png)<!-- -->

``` r
Sum_all_pcb_rock_n <- PCBs_long %>%
  group_by(pcb, site) %>%
  filter(GEN_LOC_NM == "Rockland to north") %>%
  summarise(amount_detected = sum(amount_detected, na.rm = TRUE)) %>%
  drop_na(amount_detected) %>%
  summarise(mean = mean(amount_detected),
    sd = sd(amount_detected),
    n = n(),
    SE = sd(amount_detected) / sqrt(n()))
```

    ## `summarise()` has grouped output by 'pcb'. You can override using the `.groups`
    ## argument.

``` r
Sum_all_pcb_rock_n
```

    ## # A tibble: 24 × 5
    ##    pcb          mean     sd     n     SE
    ##    <chr>       <dbl>  <dbl> <int>  <dbl>
    ##  1 ALDRIN_C   0.244   1.43     60 0.185 
    ##  2 BHC_A_C    0       0        60 0     
    ##  3 BHC_B_C    0       0        60 0     
    ##  4 BHC_D_C    1.67   12.9      60 1.67  
    ##  5 CLRDNE_T_C 0       0        60 0     
    ##  6 DDD_4_4_C  0.173   1.34     60 0.173 
    ##  7 DDE_4_4_C  0.163   0.763    60 0.0985
    ##  8 DDT_2_4_C  0.0392  0.259    60 0.0335
    ##  9 DDT_4_4_C  0.138   1.00     60 0.130 
    ## 10 DIELDRN_C  0.0728  0.414    60 0.0534
    ## # ℹ 14 more rows

``` r
Sum_all_pcb_rock_n %>%
  ggplot(aes(x = fct_rev(fct_reorder(pcb, mean)), y = mean, fill = pcb)) +
  geom_col(color = "black") +
   geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.2) +
  coord_flip() +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = 5, direction = 1, aesthetics = "fill") +
  theme(legend.position = "none") +
  labs(title = "PCB and Pesticide Concentrations in Sediments",
       subtitle = "Rockland, ME, and points north",
       x = "PCB/Pesticide Detected",
       y = "Amount Detected (ng/g)")
```

    ## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Pastel2 is 8
    ## Returning the palette you asked for with that many colors

![](PCBs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Specific Locations

#### Rivers

As stated in our project proposal, we were interested in visualizing
contaminants in Maine Rivers as well as harbors and bays.

``` r
Sum_Org_site <- Organics %>%
  group_by(site) %>%
  drop_na(PCB_T_UGG) %>%
  summarise(mean_PCB_T = mean(PCB_T_UGG),
    sd_PCB_T = sd(PCB_T_UGG),
    n_PCB_T = n(),
    SE_PCB_T = sd(PCB_T_UGG) / sqrt(n()))
```

Below we are identifying specific Maine rivers to use in our analysis.

``` r
Organics %>%
  filter(STATE_NAME == "ME") %>%
  filter(PCB_T_UGG != "NA") %>%
  distinct(site)
```

    ##                                                       site
    ## 1                            Southern Harbor (North Haven)
    ## 2                                               Fore River
    ## 3                                              Royal River
    ## 4                                        Stonington Harbor
    ## 5                                          Kennebunk River
    ## 6                                             Lermond Cove
    ## 7                                               Bar Harbor
    ## 8                                          Rockport Harbor
    ## 9                                           Castine Harbor
    ## 10                     Portsmouth Naval Shipyard (Kittery)
    ## 11                                              Mack Point
    ## 12                                             York Harbor
    ## 13                                           Camden Harbor
    ## 14                                            Wells Harbor
    ## 15                            Portland Back Cove & Channel
    ## 16                                          Kennebec River
    ## 17                                   Sears Island Terminal
    ## 18                                             Union River
    ## 19                                         Penobscot River
    ## 20                          Jonesport Harbor - Sawyer Cove
    ## 21                                  Isle Au Haut Thorofare
    ## 22                     Beals Harbor (Barneys Cove - Beals)
    ## 23                                             Bass Harbor
    ## 24                                          Tenants Harbor
    ## 25 Back Channel Near N. shore of entrance to Barters Creek
    ## 26             West of Jamaica Is. in Back Channel of PNSY
    ## 27          Back Channel E side of back gate entr. to PNSY
    ## 28                                              York River
    ## 29                                               Casco Bay
    ## 30                                           Penobscot Bay
    ## 31                                         Portland Harbor

Below we are visualizing the mean PCBs present in Maine rivers.

``` r
Sum_Org_site %>%
  filter(site %in% c("York River", "Kennebunk River", "Fore River", "Royal River", "Kennebec River", "Penobscot River", "Saco River")) %>%
  ggplot(aes(x = fct_rev(fct_reorder(site, mean_PCB_T)), y = sapply(mean_PCB_T, FUN=function(x) ifelse(x==0.000000e0, -0.2,x) ), fill = fct_rev(fct_reorder(site, mean_PCB_T)))) +
  geom_bar(stat="identity", col = "black") +
  scale_x_discrete(drop=FALSE) +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_manual(values = c("Kennebec River" = "skyblue",
                               "Penobscot River" = "skyblue",
                               "Royal River" = "green4",
                               "York River" = "darkgoldenrod4",
                               "Kennebunk River" = "firebrick",
                               "Fore River" = "peachpuff")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "PCB Concentrations in Maine Rivers",
       subtitle = "Union River excluded",
       caption = "Error bars = 1 standard error",
       x = "Location",
       y = "Mean total PCB concentration ug/g")
```

![](PCBs_files/figure-gfm/bar-pcb-me-rivers-without-union-1.png)<!-- -->
Sites with only one observation or mean = 0 do not have error bars.

``` r
Sum_Org_site %>%
  filter(site %in% c("York River", "Kennebunk River", "Fore River", "Royal River", "Kennebec River", "Penobscot River", "Saco River", "Union River")) %>%
  ggplot(aes(x = fct_rev(fct_reorder(site, mean_PCB_T)), y = sapply(mean_PCB_T, FUN=function(x) ifelse(x==0.000000e0, -2,x)), fill = site)) +
  geom_col(col = "black") +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_manual(values = c("Kennebec River" = "skyblue",
                               "Penobscot River" = "skyblue",
                               "Royal River" = "gray50",
                               "York River" = "gray50",
                               "Kennebunk River" = "gray50",
                               "Fore River" = "peachpuff",
                               "Union River" = "mediumorchid")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "PCB Concentrations in Maine Rivers",
       subtitle = "With Union River, Ellsworth",
       caption = "Error bars = 1 standard error",
       x = "Location",
       y = "Mean total PCB concentration ug/g")
```

![](PCBs_files/figure-gfm/bar-pcb-me-rivers-with-union-1.png)<!-- -->

There are no errorbars for Union River as n = 1; single sample from this
site so only a mean value.

``` r
Organics %>%
  filter(STATE_NAME == "MA") %>%
  filter(PCB_T_UGG != "NA")# %>%
```

    ##       UNIQUE_ID LATITUDE LONGITUDE SOUNDING_M STATE_NAME    QUAD_NAME
    ## 1       US00005 42.38500 -71.04611         NA         MA         <NA>
    ## 2       US00006 42.38500 -71.04611         NA         MA         <NA>
    ## 3       US00007 42.38500 -71.04611         NA         MA         <NA>
    ## 4       US00008 42.38500 -71.04611         NA         MA         <NA>
    ## 5       US00009 42.38500 -71.04611         NA         MA         <NA>
    ## 6       US00013 42.34111 -71.01389         NA         MA         <NA>
    ## 7       US00014 42.34167 -71.02444         NA         MA         <NA>
    ## 8       US00021 42.42833 -70.56667         NA         MA         <NA>
    ## 9       US00022 42.42833 -70.56667         NA         MA         <NA>
    ## 10      US00023 42.42833 -70.56667         NA         MA         <NA>
    ## 11      US00024 42.42833 -70.56667         NA         MA         <NA>
    ## 12      US00025 42.42833 -70.56667         NA         MA         <NA>
    ## 13      US00026 42.42833 -70.56667         NA         MA         <NA>
    ## 14      US00027 42.42833 -70.56667         NA         MA         <NA>
    ## 15      US00028 42.42833 -70.56667         NA         MA         <NA>
    ## 16      US00029 42.42833 -70.56667         NA         MA         <NA>
    ## 17      US00030 42.42833 -70.56667         NA         MA         <NA>
    ## 18      US00031 42.42833 -70.56667         NA         MA         <NA>
    ## 19      US00032 42.42833 -70.56667         NA         MA         <NA>
    ## 20      US00050 42.34250 -71.03194         NA         MA       Boston
    ## 21      US00051 42.34250 -71.02681         NA         MA       Boston
    ## 22      US00052 42.34250 -71.02056         NA         MA       Boston
    ## 23      US00053 42.34306 -71.03056         NA         MA       Boston
    ## 24      US00054 42.34472 -71.02056         NA         MA       Boston
    ## 25      US00055 42.34583 -70.99833         NA         MA       Boston
    ## 26      US00056 42.34736 -70.98972         NA         MA       Boston
    ## 27      US00057 42.34906 -70.98014         NA         MA       Boston
    ## 28      US00058 42.33361 -70.97333         NA         MA       Boston
    ## 29      US00059 42.33458 -70.96708         NA         MA       Boston
    ## 30      US00060 42.35861 -71.03736         NA         MA       Boston
    ## 31      US00061 42.35875 -71.03653         NA         MA       Boston
    ## 32      US00062 42.30000 -71.03667         NA         MA         BOST
    ## 33      US00063 42.35083 -70.98333         NA         MA         BOST
    ## 34      US00064 42.35083 -70.98333         NA         MA         BOST
    ## 35      US00083 42.54033 -70.87633         NA         MA         SALE
    ## 36      US00084 42.54033 -70.87633         NA         MA         SALE
    ## 37      US00085 42.54033 -70.87633         NA         MA         SALE
    ## 38      US00089 42.25000 -70.87167         NA         MA         WEYM
    ## 39      US00090 42.25000 -70.87167         NA         MA         WEYM
    ## 40      US00091 42.25000 -70.87167         NA         MA         WEYM
    ## 41      US00098 42.38667 -71.05900         NA         MA         BOST
    ## 42      US00099 42.38667 -71.05900         NA         MA         BOST
    ## 43      US00100 42.38333 -71.05617         NA         MA         BOST
    ## 44      US00101 42.38333 -71.05617         NA         MA         BOST
    ## 45      US00102 42.60833 -70.65500         NA         MA         GLOU
    ## 46      US00103 42.60833 -70.65500         NA         MA         GLOU
    ## 47      US00104 42.60833 -70.65500         NA         MA         GLOU
    ## 48      US00105 42.60833 -70.65500         NA         MA         GLOU
    ## 49      US00106 42.60833 -70.65500         NA         MA         GLOU
    ## 50      US00107 42.60833 -70.65500         NA         MA         GLOU
    ## 51      US00108 42.60833 -70.65500         NA         MA         GLOU
    ## 52      US00109 42.60833 -70.65500         NA         MA         GLOU
    ## 53      US00110 42.60833 -70.65500         NA         MA         GLOU
    ## 54      US00111 42.60833 -70.65500         NA         MA         GLOU
    ## 55      US00112 42.36667 -70.98333         NA         MA         BOST
    ## 56      US00113 42.36667 -70.98333         NA         MA         BOST
    ## 57      US00114 42.36667 -70.98333         NA         MA         BOST
    ## 58      US00115 42.36667 -70.98333         NA         MA         BOST
    ## 59      US00116 42.36667 -70.98333         NA         MA         BOST
    ## 60      US00117 42.36667 -70.98333         NA         MA         BOST
    ## 61      US00118 42.36667 -70.98333         NA         MA         BOST
    ## 62      US00119 42.36667 -70.98333         NA         MA         BOST
    ## 63      US00120 42.36667 -70.98333         NA         MA         BOST
    ## 64      US00121 42.36667 -70.98333         NA         MA         BOST
    ## 65      US00122 42.36667 -70.98333         NA         MA         BOST
    ## 66      US00123 42.36667 -70.98333         NA         MA         BOST
    ## 67      US00124 42.36667 -70.98333         NA         MA         BOST
    ## 68      US00125 42.36667 -70.98333         NA         MA         BOST
    ## 69      US00126 42.36667 -70.98333         NA         MA         BOST
    ## 70      US00127 42.36667 -70.98333         NA         MA         BOST
    ## 71      US00128 42.36667 -70.98333         NA         MA         BOST
    ## 72      US00129 42.36667 -70.98333         NA         MA         BOST
    ## 73      US00130 42.36667 -70.98333         NA         MA         BOST
    ## 74      US00131 42.36667 -70.98333         NA         MA         BOST
    ## 75      US00132 42.36667 -70.98333         NA         MA         BOST
    ## 76      US00133 42.36667 -70.98333         NA         MA         BOST
    ## 77      US00134 42.36667 -70.98333         NA         MA         BOST
    ## 78      US00135 42.36667 -70.98333         NA         MA         BOST
    ## 79      US00136 42.36667 -70.98333         NA         MA         BOST
    ## 80      US00138 42.38333 -71.05000         NA         MA         BOST
    ## 81      US00143       NA        NA         NA         MA         MANC
    ## 82      US00144       NA        NA         NA         MA         MANC
    ## 83      US00145       NA        NA         NA         MA         MANC
    ## 84      US00146       NA        NA         NA         MA         MANC
    ## 85      US00147 42.20000 -70.71667         NA         MA         SCIT
    ## 86      US00148 42.23333 -70.60000         NA         MA         WEYM
    ## 87      US00149 42.23333 -70.60000         NA         MA         WEYM
    ## 88      US00150 42.23333 -70.60000         NA         MA         WEYM
    ## 89      US00151 42.23333 -70.60000         NA         MA         WEYM
    ## 90      US00152 42.25833 -70.89500         NA         MA         HING
    ## 91      US00154 42.51667 -70.88333         NA         MA         SALE
    ## 92      US00155 42.33367 -71.03450         NA         MA         BOST
    ## 93      US00158 42.38667 -71.09000         NA         MA         BOST
    ## 94      US00159 42.38333 -71.06667         NA         MA         BOST
    ## 95      US00160 42.38333 -71.06667         NA         MA         BOST
    ## 96      US00173 42.19800 -70.72500         NA         MA         SCIT
    ## 97      US00174 42.19800 -70.72500         NA         MA         SCIT
    ## 98      US00175 42.19800 -70.72500         NA         MA         SCIT
    ## 99      US00176 42.19300 -70.72633         NA         MA         SCIT
    ## 100     US00177 42.58333 -70.66667         NA         MA         GLOU
    ## 101     US00179 42.33367 -71.03450         NA         MA         BOST
    ## 102     US00180 42.33367 -71.03450         NA         MA         BOST
    ## 103     US00181 42.33367 -71.03450         NA         MA         BOST
    ## 104     US00182 42.33367 -71.03450         NA         MA         BOST
    ## 105     US00183 42.03833 -70.67000         NA         MA         DUXB
    ## 106     US00189 42.30000 -71.03333         NA         MA         BOST
    ## 107     US00191 42.30000 -71.03333         NA         MA         BOST
    ## 108     US00194 42.30000 -71.03333         NA         MA         BOST
    ## 109     US00196 42.30000 -71.03333         NA         MA         BOST
    ## 110     US00198 42.30000 -71.03333         NA         MA         BOST
    ## 111     US00200 42.30000 -71.03333         NA         MA         BOST
    ## 112     US00201 42.30000 -71.03333         NA         MA         <NA>
    ## 113     US00202 42.54167 -70.88333         NA         MA         SALE
    ## 114     US00206 42.30000 -71.03333         NA         MA         BOST
    ## 115     US00207 42.30000 -71.03333         NA         MA         BOST
    ## 116     US00208 42.30000 -71.03333         NA         MA         BOST
    ## 117     US00209 42.30000 -71.03333         NA         MA         BOST
    ## 118     US00217 42.39000 -71.01870         NA         MA       Boston
    ## 119     US00221 42.39000 -71.01870         NA         MA       Boston
    ## 120     US00237 42.30278 -71.03056         NA         MA         BOST
    ## 121     US00241 42.35000 -71.05000         NA         MA         BOST
    ## 122     US00242 42.35000 -71.05000         NA         MA         BOST
    ## 123     US00243 42.35000 -71.05000         NA         MA         BOST
    ## 124     US00246 42.41000 -71.01528         NA         MA       Boston
    ## 125     US00247 42.40833 -71.01389         NA         MA       Boston
    ## 126     US00248 42.40597 -71.01253         NA         MA       Boston
    ## 127     US00249 42.40569 -71.01111         NA         MA       Boston
    ## 128     US00250 42.40556 -71.00861         NA         MA       Boston
    ## 129     US00251 42.40528 -71.00722         NA         MA       Boston
    ## 130     US00252 42.40361 -71.00361         NA         MA       Boston
    ## 131     US00253 42.40833 -71.01389         NA         MA       Boston
    ## 132     US00254 42.41000 -71.01528         NA         MA       Boston
    ## 133     US00255 42.40833 -71.01389         NA         MA       Boston
    ## 134     US00256 42.40833 -71.01389         NA         MA       Boston
    ## 135     US00257 42.40833 -71.01389         NA         MA       Boston
    ## 136     US00258 42.40833 -71.01389         NA         MA       Boston
    ## 137     US00259 42.41000 -71.01528         NA         MA       Boston
    ## 138     US00260 42.41000 -71.01528         NA         MA       Boston
    ## 139     US00261 42.40556 -71.00861         NA         MA       Boston
    ## 140     US00262 42.40556 -71.00861         NA         MA       Boston
    ## 141     US00263 42.40556 -71.00861         NA         MA       Boston
    ## 142     US00264 42.40361 -71.00361         NA         MA       Boston
    ## 143     US00265 42.40569 -71.01111         NA         MA       Boston
    ## 144     US00266 42.40361 -71.00361         NA         MA       Boston
    ## 145     US00267 42.40528 -71.00722         NA         MA       Boston
    ## 146     US00268 42.40361 -71.00361         NA         MA       Boston
    ## 147     US00269 42.40569 -71.01111         NA         MA       Boston
    ## 148     US00270 42.40569 -71.01111         NA         MA       Boston
    ## 149     US00271 42.40528 -71.00722         NA         MA       Boston
    ## 150     US00272 42.40528 -71.00722         NA         MA       Boston
    ## 151     US00273 42.40361 -71.00361         NA         MA       Boston
    ## 152     US00274 42.40528 -71.00722         NA         MA       Boston
    ## 153     US00275 42.40556 -71.00861         NA         MA       Boston
    ## 154     US00276 42.40597 -71.01253         NA         MA       Boston
    ## 155     US00277 42.40597 -71.01253         NA         MA       Boston
    ## 156     US00278 42.40597 -71.01253         NA         MA       Boston
    ## 157     US00279 42.41000 -71.01528         NA         MA       Boston
    ## 158     US00280 42.40569 -71.01111         NA         MA       Boston
    ## 159     US00281 42.40528 -71.00722         NA         MA       Boston
    ## 160     US00282 42.40528 -71.00722         NA         MA       Boston
    ## 161     US00285 42.30910 -71.04320         NA         MA Boston South
    ## 162     US00304 42.38333 -71.06667         NA         MA         BOST
    ## 163     US00330 42.36086 -71.05042         NA         MA       Boston
    ## 164     US00333 42.36150 -71.05025         NA         MA       Boston
    ## 165     US00380 42.14850 -70.69880         NA         MA         <NA>
    ## 166     US00381 42.14850 -70.69880         NA         MA         <NA>
    ## 167     US00420 42.36333 -70.97639         NA         MA         <NA>
    ## 168     US00421 42.35194 -70.97333         NA         MA         <NA>
    ## 169     US00422 42.36500 -70.97639         NA         MA         <NA>
    ## 170     US00432 42.53889 -70.88764         NA         MA         <NA>
    ## 171     US00433 42.53847 -70.88778         NA         MA         <NA>
    ## 172     US00434 42.53806 -70.88931         NA         MA         <NA>
    ## 173     US00435 42.54014 -70.88736         NA         MA         <NA>
    ## 174     US00436 42.53125 -70.89375         NA         MA         <NA>
    ## 175     US00437 42.52917 -70.89556         NA         MA         <NA>
    ## 176     US00438 42.52625 -70.89583         NA         MA         <NA>
    ## 177     US00439 42.52528 -70.89817         NA         MA         <NA>
    ## 178     US00440 42.52514 -70.89903         NA         MA         <NA>
    ## 179     US00441 42.52442 -70.90042         NA         MA         <NA>
    ## 180     US00442 42.52292 -70.91486         NA         MA         <NA>
    ## 181     US00477 42.38575 -71.06179         NA         MA       Boston
    ## 182     US00483 42.38773 -71.02103         NA         MA       Boston
    ## 183     US00487 42.38437 -71.02372   11.58278         MA       Boston
    ## 184     US00498 42.34275 -71.02614         NA         MA       Boston
    ## 185     US00506 42.38469 -71.04270         NA         MA       Boston
    ## 186     US00524 42.38797 -71.05974         NA         MA       Boston
    ## 187     US00525 42.38586 -71.05833         NA         MA       Boston
    ## 188     US00526 42.39778 -71.01542         NA         MA       Boston
    ## 189     US00527 42.38561 -71.02306         NA         MA       Boston
    ## 190     US00528 42.34222 -71.02542         NA         MA       Boston
    ## 191     US00529 42.34445 -71.01995         NA         MA       Boston
    ## 192     US00530       NA        NA         NA         MA       Boston
    ## 193     US00542       NA        NA         NA         MA         <NA>
    ## 194     US00543       NA        NA         NA         MA         <NA>
    ## 195     US00544       NA        NA         NA         MA         <NA>
    ## 196     US00545       NA        NA         NA         MA         <NA>
    ## 197     US00630 42.32944 -71.00806         NA         MA         <NA>
    ## 198     US00631 42.32944 -71.00806         NA         MA         <NA>
    ## 199     US00632 42.32944 -71.00806         NA         MA         <NA>
    ## 200     US00633 42.32944 -71.00806         NA         MA         <NA>
    ## 201     US00634 42.32944 -71.00806         NA         MA         <NA>
    ## 202     US00635 42.32944 -71.00806         NA         MA         <NA>
    ## 203     US00636 42.34472 -70.98056         NA         MA         <NA>
    ## 204     US00637 42.34472 -70.98056         NA         MA         <NA>
    ## 205     US00638 42.34472 -70.98056         NA         MA         <NA>
    ## 206     US00639 42.34472 -70.98056         NA         MA         <NA>
    ## 207     US00640 42.34472 -70.98056         NA         MA         <NA>
    ## 208     US00641 42.34472 -70.98056         NA         MA         <NA>
    ## 209     US00643 42.29222 -70.98639         NA         MA         <NA>
    ## 210     US00644 42.29222 -70.98639         NA         MA         <NA>
    ## 211     US00645 42.29222 -70.98639         NA         MA         <NA>
    ## 212     US00646 42.29222 -70.98639         NA         MA         <NA>
    ## 213     US00647 42.29222 -70.98639         NA         MA         <NA>
    ## 214     US00648 42.29222 -70.98639         NA         MA         <NA>
    ## 215     US00649 42.29444 -70.94722         NA         MA         <NA>
    ## 216     US00650 42.29444 -70.94722         NA         MA         <NA>
    ## 217     US00651 42.29444 -70.94722         NA         MA         <NA>
    ## 218     US00652 42.29444 -70.94722         NA         MA         <NA>
    ## 219     US00653 42.29444 -70.94722         NA         MA         <NA>
    ## 220     US00654 42.29444 -70.94722         NA         MA         <NA>
    ## 221     US00655 42.29111 -70.90556         NA         MA         <NA>
    ## 222     US00656 42.29111 -70.90556         NA         MA         <NA>
    ## 223     US00657 42.29111 -70.90556         NA         MA         <NA>
    ## 224     US00658 42.29111 -70.90556         NA         MA         <NA>
    ## 225     US00659 42.29111 -70.90556         NA         MA         <NA>
    ## 226     US00660 42.29111 -70.90556         NA         MA         <NA>
    ## 227     US00661 42.36167 -70.85667         NA         MA         <NA>
    ## 228     US00662 42.36167 -70.85667         NA         MA         <NA>
    ## 229     US00663 42.36167 -70.85667         NA         MA         <NA>
    ## 230     US00664 42.36167 -70.85667         NA         MA         <NA>
    ## 231     US00666 42.39500 -70.82000         NA         MA         <NA>
    ## 232     US00667 42.39500 -70.82000         NA         MA         <NA>
    ## 233     US00668 42.39500 -70.82000         NA         MA         <NA>
    ## 234     US00669 42.39500 -70.82000         NA         MA         <NA>
    ## 235     US00670 42.39500 -70.82000         NA         MA         <NA>
    ## 236     US00671 42.39500 -70.82000         NA         MA         <NA>
    ## 237     US00686 42.43000 -70.55500         NA         MA         <NA>
    ## 238     US00687 42.43000 -70.55500         NA         MA         <NA>
    ## 239     US00688 42.43000 -70.55500         NA         MA         <NA>
    ## 240     US00689 42.43000 -70.55500         NA         MA         <NA>
    ## 241     US00690 42.43000 -70.55500         NA         MA         <NA>
    ## 242     US00691 42.43000 -70.55500         NA         MA         <NA>
    ## 243     US00704 42.31000 -70.73667         NA         MA         <NA>
    ## 244     US00705 42.31000 -70.73667         NA         MA         <NA>
    ## 245     US00706 42.31000 -70.73667         NA         MA         <NA>
    ## 246     US00707 42.31000 -70.73667         NA         MA         <NA>
    ## 247     US00708 42.31000 -70.73667         NA         MA         <NA>
    ## 248     US00709 42.31000 -70.73667         NA         MA         <NA>
    ## 249     US00710 42.33500 -70.58333         NA         MA         <NA>
    ## 250     US00711 42.33500 -70.58333         NA         MA         <NA>
    ## 251     US00712 42.33500 -70.58333         NA         MA         <NA>
    ## 252     US00713 42.33500 -70.58333         NA         MA         <NA>
    ## 253     US00714 42.33500 -70.58333         NA         MA         <NA>
    ## 254     US00715 42.33500 -70.58333         NA         MA         <NA>
    ## 255     US00716 42.33500 -70.58333         NA         MA         <NA>
    ## 256     US00717 42.33500 -70.58333         NA         MA         <NA>
    ## 257     US00718 42.33167 -70.38000         NA         MA         <NA>
    ## 258     US00719 42.33167 -70.38000         NA         MA         <NA>
    ## 259     US00720 42.33167 -70.38000         NA         MA         <NA>
    ## 260     US00721 42.33167 -70.38000         NA         MA         <NA>
    ## 261     US00722 42.33167 -70.38000         NA         MA         <NA>
    ## 262     US00723 42.33167 -70.38000         NA         MA         <NA>
    ## 263     US00725 42.22833 -70.52500         NA         MA         <NA>
    ## 264     US00726 42.22833 -70.52500         NA         MA         <NA>
    ## 265     US00727 42.22833 -70.52500         NA         MA         <NA>
    ## 266     US00728 42.22833 -70.52500         NA         MA         <NA>
    ## 267     US00729 42.22833 -70.52500         NA         MA         <NA>
    ## 268     US00730 42.22833 -70.52500         NA         MA         <NA>
    ## 269     US00731 42.19667 -70.40333         NA         MA         <NA>
    ## 270     US00732 42.19667 -70.40333         NA         MA         <NA>
    ## 271     US00733 42.19667 -70.40333         NA         MA         <NA>
    ## 272     US00734 42.19667 -70.40333         NA         MA         <NA>
    ## 273     US00735 42.19667 -70.40333         NA         MA         <NA>
    ## 274     US00736 42.19667 -70.40333         NA         MA         <NA>
    ## 275     US00738 42.09000 -70.44167         NA         MA         <NA>
    ## 276     US00739 42.09000 -70.44167         NA         MA         <NA>
    ## 277     US00740 42.09000 -70.44167         NA         MA         <NA>
    ## 278     US00741 42.09000 -70.44167         NA         MA         <NA>
    ## 279     US00742 42.09000 -70.44167         NA         MA         <NA>
    ## 280     US00743 42.09000 -70.44167         NA         MA         <NA>
    ## 281     US00744 41.90833 -70.22333         NA         MA         <NA>
    ## 282     US00745 41.90833 -70.22333         NA         MA         <NA>
    ## 283     US00746 41.90833 -70.22333         NA         MA         <NA>
    ## 284     US00747 41.90833 -70.22333         NA         MA         <NA>
    ## 285     US00748 41.90833 -70.22333         NA         MA         <NA>
    ## 286     US00749 41.90833 -70.22333         NA         MA         <NA>
    ## 287     US00750 41.97333 -70.39833         NA         MA         <NA>
    ## 288     US00751 41.97333 -70.39833         NA         MA         <NA>
    ## 289     US00752 41.97333 -70.39833         NA         MA         <NA>
    ## 290     US00753 41.97333 -70.39833         NA         MA         <NA>
    ## 291     US00754 41.97333 -70.39833         NA         MA         <NA>
    ## 292     US00755 41.97333 -70.39833         NA         MA         <NA>
    ## 293     US01272       NA        NA         NA         MA         <NA>
    ## 294     US01273       NA        NA         NA         MA         <NA>
    ## 295     US01317 42.30139 -71.03194         NA         MA         <NA>
    ## 296     US01318 42.30139 -71.03194         NA         MA         <NA>
    ## 297     US01319 42.30139 -71.03194         NA         MA         <NA>
    ## 298     US01320 42.30139 -71.03194         NA         MA         <NA>
    ## 299     US01321       NA        NA         NA         MA         <NA>
    ## 300     US01325 42.38975 -71.05500         NA         MA         <NA>
    ## 301     US01326 42.38975 -71.05500         NA         MA         <NA>
    ## 302     US01327 42.38975 -71.05500         NA         MA         <NA>
    ## 303     US01328 42.51667 -70.88333         NA         MA         SALE
    ## 304     US01329 42.27941 -70.95303         NA         MA         HULL
    ## 305     US01330 42.27941 -70.95303         NA         MA         HULL
    ## 306     US01331 42.27941 -70.95303         NA         MA         HULL
    ## 307     US01332 42.27941 -70.95303         NA         MA         HULL
    ## 308     US01333 42.27941 -70.95303         NA         MA         HULL
    ## 309     US01334 42.27941 -70.95303         NA         MA         HULL
    ## 310     US01335 42.27941 -70.95303         NA         MA         HULL
    ## 311     US01336 42.27941 -70.95303         NA         MA         HULL
    ## 312     US01337 42.35000 -70.96667         NA         MA         HULL
    ## 313     US01338 42.35000 -70.96667         NA         MA         HULL
    ## 314     US01339 42.35000 -70.96667         NA         MA         HULL
    ## 315     US01340 42.35000 -70.96667         NA         MA         HULL
    ## 316     US01341 42.35000 -70.96667         NA         MA         HULL
    ## 317     US01342 42.35000 -70.96667         NA         MA         HULL
    ## 318     US01343 42.35000 -70.96667         NA         MA         HULL
    ## 319     US01344 42.35000 -70.96667         NA         MA         HULL
    ## 320     US01345 42.35000 -70.96667         NA         MA         HULL
    ## 321     US01346 42.35000 -70.96667         NA         MA         HULL
    ## 322     US01347 42.35000 -70.96667         NA         MA         HULL
    ## 323     US01348 42.35000 -70.96667         NA         MA         HULL
    ## 324     US01349 42.35000 -70.96667         NA         MA         HULL
    ## 325     US01350 42.23960 -70.78559         NA         MA         <NA>
    ## 326     US01354 42.23936 -70.79080         NA         MA         <NA>
    ## 327     US01357 42.24037 -70.79320         NA         MA         <NA>
    ## 328     US01361 42.24089 -70.78910         NA         MA         <NA>
    ## 329     US01364 42.24177 -70.78810         NA         MA         <NA>
    ## 330     US01367 42.24401 -70.78705         NA         MA         <NA>
    ## 331     US01371       NA        NA         NA         MA         <NA>
    ## 332     US01372       NA        NA         NA         MA         <NA>
    ## 333     US01373       NA        NA         NA         MA         <NA>
    ## 334     US01374       NA        NA         NA         MA         <NA>
    ## 335   US01441-1 42.29333 -70.95472         NA         MA         <NA>
    ## 336   US01441-2 42.29333 -70.95472         NA         MA         <NA>
    ## 337   US01441-3 42.29333 -70.95472         NA         MA         <NA>
    ## 338   US01441-4 42.29333 -70.95472         NA         MA         <NA>
    ## 339   US01441-5 42.29333 -70.95472         NA         MA         <NA>
    ## 340   US01441-6 42.29333 -70.95472         NA         MA         <NA>
    ## 341   US01441-7 42.29333 -70.95472         NA         MA         <NA>
    ## 342   US01442-1 42.29361 -70.95111         NA         MA         <NA>
    ## 343   US01442-2 42.29361 -70.95111         NA         MA         <NA>
    ## 344   US01442-3 42.29361 -70.95111         NA         MA         <NA>
    ## 345   US01442-4 42.29361 -70.95111         NA         MA         <NA>
    ## 346   US01442-5 42.29361 -70.95111         NA         MA         <NA>
    ## 347   US01442-6 42.29361 -70.95111         NA         MA         <NA>
    ## 348   US01442-7 42.29361 -70.95111         NA         MA         <NA>
    ## 349   US01444-1 42.30278 -70.94361         NA         MA         <NA>
    ## 350   US01444-2 42.30278 -70.94361         NA         MA         <NA>
    ## 351   US01444-3 42.30278 -70.94361         NA         MA         <NA>
    ## 352   US01444-4 42.30278 -70.94361         NA         MA         <NA>
    ## 353   US01444-5 42.30278 -70.94361         NA         MA         <NA>
    ## 354   US01444-6 42.30278 -70.94361         NA         MA         <NA>
    ## 355   US01444-7 42.30278 -70.94361         NA         MA         <NA>
    ## 356  US01445-01 42.32083 -71.01278         NA         MA         <NA>
    ## 357  US01445-02 42.32083 -71.01278         NA         MA         <NA>
    ## 358  US01445-03 42.32083 -71.01278         NA         MA         <NA>
    ## 359  US01445-04 42.32083 -71.01278         NA         MA         <NA>
    ## 360  US01445-05 42.32083 -71.01278         NA         MA         <NA>
    ## 361  US01445-06 42.32083 -71.01278         NA         MA         <NA>
    ## 362  US01445-07 42.32083 -71.01278         NA         MA         <NA>
    ## 363  US01445-08 42.32083 -71.01278         NA         MA         <NA>
    ## 364  US01445-09 42.32083 -71.01278         NA         MA         <NA>
    ## 365  US01445-10 42.32083 -71.01278         NA         MA         <NA>
    ## 366  US01445-11 42.32083 -71.01278         NA         MA         <NA>
    ## 367  US01445-12 42.32083 -71.01278         NA         MA         <NA>
    ## 368  US01445-13 42.32083 -71.01278         NA         MA         <NA>
    ## 369  US01445-14 42.32083 -71.01278         NA         MA         <NA>
    ## 370   US01447-1 42.32639 -70.89833         NA         MA         <NA>
    ## 371   US01447-2 42.32639 -70.89833         NA         MA         <NA>
    ## 372   US01447-3 42.32639 -70.89833         NA         MA         <NA>
    ## 373   US01447-4 42.32639 -70.89833         NA         MA         <NA>
    ## 374   US01447-5 42.32639 -70.89833         NA         MA         <NA>
    ## 375   US01447-6 42.32639 -70.89833         NA         MA         <NA>
    ## 376   US01447-7 42.32639 -70.89833         NA         MA         <NA>
    ## 377   US01448-1 42.33861 -70.95722         NA         MA         <NA>
    ## 378   US01448-2 42.33861 -70.95722         NA         MA         <NA>
    ## 379   US01448-3 42.33861 -70.95722         NA         MA         <NA>
    ## 380   US01448-4 42.33861 -70.95722         NA         MA         <NA>
    ## 381   US01448-5 42.33861 -70.95722         NA         MA         <NA>
    ## 382   US01448-6 42.33861 -70.95722         NA         MA         <NA>
    ## 383   US01448-7 42.33861 -70.95722         NA         MA         <NA>
    ## 384   US01450-1 42.33889 -70.95972         NA         MA         <NA>
    ## 385   US01450-2 42.33889 -70.95972         NA         MA         <NA>
    ## 386   US01450-3 42.33889 -70.95972         NA         MA         <NA>
    ## 387   US01450-4 42.33889 -70.95972         NA         MA         <NA>
    ## 388   US01450-5 42.33889 -70.95972         NA         MA         <NA>
    ## 389   US01450-6 42.33889 -70.95972         NA         MA         <NA>
    ## 390   US01450-7 42.33889 -70.95972         NA         MA         <NA>
    ## 391   US01453-1 42.34333 -71.00806         NA         MA         <NA>
    ## 392   US01453-2 42.34333 -71.00806         NA         MA         <NA>
    ## 393   US01455-1 42.34361 -70.97361         NA         MA         <NA>
    ## 394   US01455-2 42.34361 -70.97361         NA         MA         <NA>
    ## 395   US01455-3 42.34361 -70.97361         NA         MA         <NA>
    ## 396   US01455-4 42.34361 -70.97361         NA         MA         <NA>
    ## 397   US01455-5 42.34361 -70.97361         NA         MA         <NA>
    ## 398   US01455-6 42.34361 -70.97361         NA         MA         <NA>
    ## 399   US01455-7 42.34361 -70.97361         NA         MA         <NA>
    ## 400   US01457-1 42.34361 -70.94472         NA         MA         <NA>
    ## 401   US01457-2 42.34361 -70.94472         NA         MA         <NA>
    ## 402   US01457-3 42.34361 -70.94472         NA         MA         <NA>
    ## 403   US01457-4 42.34361 -70.94472         NA         MA         <NA>
    ## 404   US01457-5 42.34361 -70.94472         NA         MA         <NA>
    ## 405   US01457-6 42.34361 -70.94472         NA         MA         <NA>
    ## 406   US01457-7 42.34361 -70.94472         NA         MA         <NA>
    ## 407   US01460-1 42.34833 -70.94639         NA         MA         <NA>
    ## 408   US01460-2 42.34833 -70.94639         NA         MA         <NA>
    ## 409 US01461.5-1 42.35389 -70.94639         NA         MA         <NA>
    ## 410 US01461.5-2 42.35389 -70.94639         NA         MA         <NA>
    ## 411 US01461.5-3 42.35389 -70.94639         NA         MA         <NA>
    ## 412 US01461.5-4 42.35389 -70.94639         NA         MA         <NA>
    ## 413 US01461.5-5 42.35389 -70.94639         NA         MA         <NA>
    ## 414   US01462-1 42.39111 -70.82639         NA         MA         <NA>
    ## 415   US01462-2 42.39111 -70.82639         NA         MA         <NA>
    ## 416   US01462-3 42.39111 -70.82639         NA         MA         <NA>
    ## 417   US01462-4 42.39111 -70.82639         NA         MA         <NA>
    ## 418   US01462-5 42.39111 -70.82639         NA         MA         <NA>
    ## 419   US01462-6 42.39111 -70.82639         NA         MA         <NA>
    ## 420   US01462-7 42.39111 -70.82639         NA         MA         <NA>
    ## 421 US01463.5-1 42.34333 -71.00806         NA         MA         <NA>
    ## 422 US01463.5-2 42.34333 -71.00806         NA         MA         <NA>
    ## 423 US01463.5-3 42.34333 -71.00806         NA         MA         <NA>
    ## 424 US01463.5-4 42.34333 -71.00806         NA         MA         <NA>
    ## 425 US01463.5-5 42.34333 -71.00806         NA         MA         <NA>
    ## 426   US01559-1 42.34167 -70.80833         NA         MA         <NA>
    ## 427   US01559-2 42.34167 -70.80833         NA         MA         <NA>
    ## 428   US01560-1 42.34167 -70.80833         NA         MA         <NA>
    ## 429   US01560-2 42.34167 -70.80833         NA         MA         <NA>
    ## 430   US01561-1 42.37333 -70.89833         NA         MA         <NA>
    ## 431   US01561-2 42.37333 -70.89833         NA         MA         <NA>
    ## 432     US01574 42.48333 -70.74167         NA         MA         <NA>
    ## 433     US01575 42.48333 -70.74167         NA         MA         <NA>
    ## 434     US01577 42.50000 -70.63333         NA         MA         <NA>
    ## 435     US01579 42.50000 -70.63333         NA         MA         <NA>
    ## 436     US01580 42.50000 -70.63333         NA         MA         <NA>
    ## 437     US01581 42.50000 -70.58333         NA         MA         <NA>
    ## 438     US01582 42.50000 -70.58333         NA         MA         <NA>
    ## 439     US01585 42.43333 -70.82167         NA         MA         <NA>
    ## 440     US01586 42.43333 -70.82167         NA         MA         <NA>
    ## 441     US01589 42.45833 -70.63333         NA         MA         <NA>
    ## 442     US01592 42.45833 -70.63333         NA         MA         <NA>
    ## 443     US01593 42.45833 -70.63333         NA         MA         <NA>
    ## 444     US01595 42.41667 -70.63667         NA         MA         <NA>
    ## 445     US01596 42.41667 -70.63667         NA         MA         <NA>
    ## 446     US01597 42.41667 -70.63667         NA         MA         <NA>
    ## 447     US01600 42.45333 -70.58333         NA         MA         <NA>
    ## 448     US01601 42.45333 -70.58333         NA         MA         <NA>
    ## 449     US01602 42.45333 -70.58333         NA         MA         <NA>
    ## 450     US01603 42.45333 -70.58333         NA         MA         <NA>
    ## 451     US01604 42.45333 -70.58333         NA         MA         <NA>
    ## 452     US01605 42.45333 -70.58333         NA         MA         <NA>
    ## 453     US01606 42.38333 -70.60000         NA         MA         <NA>
    ## 454     US01607 42.38333 -70.60000         NA         MA         <NA>
    ## 455     US01608 42.38333 -70.60000         NA         MA         <NA>
    ## 456     US01609 42.38333 -70.60000         NA         MA         <NA>
    ## 457     US01612 42.36667 -70.53333         NA         MA         <NA>
    ## 458     US01613 42.36667 -70.53333         NA         MA         <NA>
    ## 459     US01614 42.36667 -70.53333         NA         MA         <NA>
    ## 460     US01615 42.36667 -70.53333         NA         MA         <NA>
    ## 461     US01616 42.36667 -70.53333         NA         MA         <NA>
    ## 462     US01617 42.36667 -70.53333         NA         MA         <NA>
    ## 463     US01618 42.41000 -70.50333         NA         MA         <NA>
    ## 464     US01619 42.41000 -70.50333         NA         MA         <NA>
    ## 465     US01620 42.41000 -70.50333         NA         MA         <NA>
    ## 466     US01621 42.41000 -70.50333         NA         MA         <NA>
    ## 467     US01622 42.41000 -70.50333         NA         MA         <NA>
    ## 468     US01623 42.37833 -70.43667         NA         MA         <NA>
    ## 469     US01624 42.37833 -70.43667         NA         MA         <NA>
    ## 470     US01626 42.37833 -70.43667         NA         MA         <NA>
    ## 471     US01627 42.37833 -70.43667         NA         MA         <NA>
    ## 472     US01628 42.37833 -70.43667         NA         MA         <NA>
    ## 473     US01629 42.32667 -70.48667         NA         MA         <NA>
    ## 474     US01630 42.32667 -70.48667         NA         MA         <NA>
    ## 475     US01631 42.32667 -70.48667         NA         MA         <NA>
    ## 476     US01632 42.32667 -70.48667         NA         MA         <NA>
    ## 477     US01633 42.33333 -70.42167         NA         MA         <NA>
    ## 478     US01634 42.33333 -70.42167         NA         MA         <NA>
    ## 479     US01635 42.33333 -70.42167         NA         MA         <NA>
    ## 480     US01636 42.33333 -70.42167         NA         MA         <NA>
    ## 481     US01637 42.33333 -70.42167         NA         MA         <NA>
    ## 482     US01638 42.33333 -70.42167         NA         MA         <NA>
    ## 483     US01639 42.33500 -70.35000         NA         MA         <NA>
    ## 484     US01640 42.33500 -70.35000         NA         MA         <NA>
    ## 485     US01641 42.33500 -70.35000         NA         MA         <NA>
    ## 486     US01642 42.33500 -70.35000         NA         MA         <NA>
    ## 487     US01643 42.33500 -70.35000         NA         MA         <NA>
    ## 488     US01644 42.33500 -70.35000         NA         MA         <NA>
    ## 489     US01645 42.30000 -70.50000         NA         MA         <NA>
    ## 490     US01646 42.30000 -70.50000         NA         MA         <NA>
    ## 491     US01647 42.30000 -70.50000         NA         MA         <NA>
    ## 492     US01649 42.30000 -70.50000         NA         MA         <NA>
    ## 493     US01650 42.30000 -70.50000         NA         MA         <NA>
    ## 494     US01651 42.30000 -70.50000         NA         MA         <NA>
    ## 495     US01652 42.28333 -70.41667         NA         MA         <NA>
    ## 496     US01653 42.28333 -70.41667         NA         MA         <NA>
    ## 497     US01654 42.28333 -70.41667         NA         MA         <NA>
    ## 498     US01655 42.28333 -70.41667         NA         MA         <NA>
    ## 499     US01656 42.28333 -70.41667         NA         MA         <NA>
    ## 500     US01657 42.28333 -70.41667         NA         MA         <NA>
    ## 501     US01658 42.27500 -70.35833         NA         MA         <NA>
    ## 502     US01659 42.27500 -70.35833         NA         MA         <NA>
    ## 503     US01660 42.27500 -70.35833         NA         MA         <NA>
    ## 504     US01661 42.27500 -70.35833         NA         MA         <NA>
    ## 505     US01662 42.27500 -70.35833         NA         MA         <NA>
    ## 506     US01663 42.27500 -70.35833         NA         MA         <NA>
    ## 507     US01664 42.25000 -70.47000         NA         MA         <NA>
    ## 508     US01667 42.25000 -70.47000         NA         MA         <NA>
    ## 509     US01668 42.25000 -70.47000         NA         MA         <NA>
    ## 510     US01669 42.25000 -70.47000         NA         MA         <NA>
    ## 511     US01670 42.21667 -70.41667         NA         MA         <NA>
    ## 512     US01671 42.21667 -70.41667         NA         MA         <NA>
    ## 513     US01672 42.21667 -70.41667         NA         MA         <NA>
    ## 514     US01673 42.21667 -70.41667         NA         MA         <NA>
    ## 515     US01674 42.21667 -70.41667         NA         MA         <NA>
    ## 516     US01675 42.16667 -70.50000         NA         MA         <NA>
    ## 517     US01676 42.16667 -70.50000         NA         MA         <NA>
    ## 518     US01678 42.16667 -70.50000         NA         MA         <NA>
    ## 519     US01679 42.16667 -70.41667         NA         MA         <NA>
    ## 520     US01680 42.16667 -70.41667         NA         MA         <NA>
    ## 521     US01681 42.11667 -70.43333         NA         MA         <NA>
    ## 522     US01685 42.11667 -70.43333         NA         MA         <NA>
    ## 523     US01689 42.10000 -70.36667         NA         MA         <NA>
    ## 524     US01690 42.10000 -70.36667         NA         MA         <NA>
    ## 525     US01691 42.10000 -70.36667         NA         MA         <NA>
    ## 526     US01694 42.05000 -70.43333         NA         MA         <NA>
    ## 527     US01695 42.05000 -70.43333         NA         MA         <NA>
    ## 528     US01696 42.05000 -70.43333         NA         MA         <NA>
    ## 529     US01697 42.05000 -70.43333         NA         MA         <NA>
    ## 530     US01698 42.05000 -70.43333         NA         MA         <NA>
    ## 531     US01699 42.05000 -70.43333         NA         MA         <NA>
    ## 532     US01700 42.05000 -70.30000         NA         MA         <NA>
    ## 533     US01701 42.05000 -70.30000         NA         MA         <NA>
    ## 534     US01702 42.05000 -70.30000         NA         MA         <NA>
    ## 535     US01704 42.05000 -70.30000         NA         MA         <NA>
    ## 536     US01705 42.05000 -70.30000         NA         MA         <NA>
    ## 537     US01706 42.05000 -70.30000         NA         MA         <NA>
    ## 538     US01707 42.00000 -70.36667         NA         MA         <NA>
    ## 539     US01708 42.00000 -70.36667         NA         MA         <NA>
    ## 540     US01709 42.00000 -70.36667         NA         MA         <NA>
    ## 541     US01713 42.00000 -70.28333         NA         MA         <NA>
    ## 542     US01714 42.00000 -70.28333         NA         MA         <NA>
    ## 543     US01715 42.00000 -70.28333         NA         MA         <NA>
    ## 544     US01716 42.00000 -70.28333         NA         MA         <NA>
    ## 545     US01717 42.00000 -70.28333         NA         MA         <NA>
    ## 546     US01718 42.00000 -70.28333         NA         MA         <NA>
    ## 547     US01719 42.51500 -70.81833         NA         MA         <NA>
    ## 548     US01720 42.51500 -70.81833         NA         MA         <NA>
    ## 549     US01721 42.54167 -70.79500         NA         MA         <NA>
    ## 550     US01722 42.54167 -70.79500         NA         MA         <NA>
    ## 551     US01723 42.54167 -70.79500         NA         MA         <NA>
    ## 552     US01724 42.54167 -70.79500         NA         MA         <NA>
    ## 553     US01725 42.54167 -70.79500         NA         MA         <NA>
    ## 554     US01728 42.40333 -70.91500         NA         MA         <NA>
    ## 555     US01729 42.40333 -70.91500         NA         MA         <NA>
    ## 556     US01730 42.41667 -70.58333         NA         MA         <NA>
    ## 557     US01731 42.41667 -70.58333         NA         MA         <NA>
    ## 558     US01732 42.41667 -70.58333         NA         MA         <NA>
    ## 559     US01733 42.41667 -70.58333         NA         MA         <NA>
    ## 560     US01734 42.41667 -70.58333         NA         MA         <NA>
    ## 561     US01735 42.41667 -70.58333         NA         MA         <NA>
    ## 562     US01737 42.41667 -70.58333         NA         MA         <NA>
    ## 563     US01738 42.41667 -70.58333         NA         MA         <NA>
    ## 564     US01740 42.41667 -70.58333         NA         MA         <NA>
    ## 565     US01741 42.41667 -70.58333         NA         MA         <NA>
    ## 566     US01742 42.41667 -70.58333         NA         MA         <NA>
    ## 567     US01743 42.41667 -70.58333         NA         MA         <NA>
    ## 568     US01744 42.41667 -70.58333         NA         MA         <NA>
    ## 569     US01745 42.41667 -70.58333         NA         MA         <NA>
    ## 570     US01746 42.41667 -70.58333         NA         MA         <NA>
    ## 571     US01747 42.41667 -70.58333         NA         MA         <NA>
    ## 572     US01874 42.35556 -71.03667         NA         MA         <NA>
    ## 573     US01878 42.35583 -71.04083         NA         MA         <NA>
    ## 574     US01881 42.35694 -71.03250         NA         MA         <NA>
    ## 575     US01885 42.35750 -71.04583         NA         MA         <NA>
    ## 576     US01890 42.35833 -71.04556         NA         MA         <NA>
    ## 577     US01893 42.36083 -71.04556         NA         MA         <NA>
    ## 578     US01897 42.36083 -71.02944         NA         MA         <NA>
    ## 579     US01901 42.36278 -71.04389         NA         MA         <NA>
    ## 580     US01905 42.32513 -70.99197         NA         MA         <NA>
    ## 581     US01906 42.32513 -70.99197         NA         MA         <NA>
    ## 582     US01907 42.32357 -70.98911         NA         MA         <NA>
    ## 583     US01908 42.32253 -70.99086         NA         MA         <NA>
    ## 584     US01909 42.32101 -70.98896         NA         MA         <NA>
    ## 585     US01910 42.31805 -70.98630         NA         MA         <NA>
    ## 586     US01911 42.31956 -70.98250         NA         MA         <NA>
    ## 587     US01912 42.32173 -70.97976         NA         MA         <NA>
    ## 588     US01913 42.32363 -70.97703         NA         MA         <NA>
    ## 589     US01914 42.32363 -70.97703         NA         MA         <NA>
    ## 590     US01915 42.32446 -70.97989         NA         MA         <NA>
    ## 591     US01916 42.32462 -70.98308         NA         MA         <NA>
    ## 592     US01917 42.32462 -70.98308         NA         MA         <NA>
    ## 593     US01918 42.32462 -70.98308         NA         MA         <NA>
    ## 594     US01919 42.32655 -70.97553         NA         MA         <NA>
    ## 595     US01920 42.32705 -70.98124         NA         MA         <NA>
    ## 596     US01921 42.32873 -70.98075         NA         MA         <NA>
    ## 597     US01922 42.32933 -70.98699         NA         MA         <NA>
    ## 598     US01923 42.32851 -70.99082         NA         MA         <NA>
    ## 599     US02027 42.33528 -71.01194         NA         MA         <NA>
    ## 600     US02028 42.33667 -71.01056         NA         MA         <NA>
    ## 601     US02059 42.35031 -71.03109         NA         MA         <NA>
    ## 602     US02060 42.35031 -71.03109         NA         MA         <NA>
    ## 603     US02061 42.35170 -71.02986         NA         MA         <NA>
    ## 604     US02062 42.35362 -71.02862         NA         MA         <NA>
    ## 605     US02063 42.35581 -71.02798         NA         MA         <NA>
    ## 606     US02064 42.35688 -71.02697         NA         MA         <NA>
    ## 607     US02065 42.35663 -71.02608         NA         MA         <NA>
    ## 608     US02066       NA        NA         NA         MA         <NA>
    ## 609     US02067       NA        NA         NA         MA         <NA>
    ## 610     US02068       NA        NA         NA         MA         <NA>
    ## 611     US02069       NA        NA         NA         MA         <NA>
    ## 612     US02070 42.80000 -70.91667         NA         MA         <NA>
    ## 613     US02146 42.29583 -70.96528         NA         MA         <NA>
    ## 614     US02147 42.30417 -71.03889         NA         MA         <NA>
    ## 615     US02150 42.30833 -70.97639         NA         MA         <NA>
    ## 616   US02151-1 42.31528 -71.02222         NA         MA         <NA>
    ## 617   US02156-1 42.32778 -70.90278         NA         MA         <NA>
    ## 618     US02157 42.33472 -70.95972         NA         MA         <NA>
    ## 619     US02158 42.34167 -70.94444         NA         MA         <NA>
    ## 620     US02159 42.34306 -71.01389         NA         MA         <NA>
    ## 621     US02161 42.34583 -70.98056         NA         MA         <NA>
    ## 622     US02166 42.35278 -71.02778         NA         MA         <NA>
    ## 623     US02167 42.36111 -71.04306         NA         MA         <NA>
    ## 624     US02168 42.37778 -71.04583         NA         MA         <NA>
    ## 625     US02169 42.39097 -71.05299         NA         MA         <NA>
    ## 626     US02170 42.35058 -71.05280         NA         MA         <NA>
    ## 627     US02171 42.35970 -71.04369         NA         MA         <NA>
    ## 628     US02172 42.35694 -71.02778         NA         MA         <NA>
    ## 629     US02173 42.35722 -71.02806         NA         MA         <NA>
    ## 630     US02174 42.35750 -71.02861         NA         MA         <NA>
    ## 631     US02175 42.35833 -71.02917         NA         MA         <NA>
    ## 632     US02176 42.35889 -71.02889         NA         MA         <NA>
    ## 633     US02177 42.35917 -71.02861         NA         MA         <NA>
    ## 634     US02178       NA        NA         NA         MA         <NA>
    ## 635     US02179       NA        NA         NA         MA         <NA>
    ## 636     US02180       NA        NA         NA         MA         <NA>
    ## 637     US02181       NA        NA         NA         MA         <NA>
    ## 638     US02182 42.43167 -70.58000         NA         MA         <NA>
    ## 639     US02183 42.43167 -70.57167         NA         MA         <NA>
    ## 640     US02184 42.42833 -70.57500         NA         MA         <NA>
    ## 641     US02185 42.42167 -70.58333         NA         MA         <NA>
    ## 642     US02186 42.41967 -70.57667         NA         MA         <NA>
    ## 643     US02187 42.42167 -70.56667         NA         MA         <NA>
    ## 644     US02188 42.41500 -70.59000         NA         MA         <NA>
    ## 645     US02189 42.41167 -70.55333         NA         MA         <NA>
    ## 646     US02190 42.40417 -70.60167         NA         MA         <NA>
    ## 647     US02191 42.39500 -70.57667         NA         MA         <NA>
    ## 648     US02192 42.40250 -70.54333         NA         MA         <NA>
    ## 649     US02193 42.39333 -70.61167         NA         MA         <NA>
    ## 650     US02194 42.37833 -70.57667         NA         MA         <NA>
    ## 651     US02195 42.39000 -70.52583         NA         MA         <NA>
    ## 652     US02197 42.34750 -71.02389         NA         MA         <NA>
    ## 653     US02198 42.34778 -71.02500         NA         MA         <NA>
    ## 654     US02199 42.34806 -71.02333         NA         MA         <NA>
    ## 655     US02200 42.35833 -71.02500         NA         MA         BOST
    ## 656     US02201 42.35833 -71.02500         NA         MA         BOST
    ## 657     US02202 42.35833 -71.02500         NA         MA         BOST
    ## 658     US02203 42.35833 -71.02500         NA         MA         BOST
    ## 659     US02204 42.41143 -70.54690         NA         MA         <NA>
    ## 660     US02224 42.41650 -70.56515         NA         MA         <NA>
    ## 661     US02244 42.41368 -70.55602         NA         MA         <NA>
    ## 662     US02264 42.37900 -70.56700         NA         MA         <NA>
    ## 663     US02267 42.41167 -70.57612         NA         MA         <NA>
    ## 664     US02270 42.42112 -70.58833         NA         MA         <NA>
    ## 665     US02273 42.42495 -70.59860         NA         MA         <NA>
    ## 666     US02276 42.42045 -70.57123         NA         MA         <NA>
    ## 667     US02279 42.33333 -70.46667         NA         MA         <NA>
    ## 668     US02432 42.38556 -71.04028         NA         MA         <NA>
    ## 669     US02475 42.37833 -70.50500         NA         MA         <NA>
    ## 670     US02476 42.35550 -70.45000         NA         MA         <NA>
    ## 671   US02477.1 42.38868 -70.89508   22.00000         MA         <NA>
    ## 672     US02478 42.38802 -70.89540   22.00000         MA         <NA>
    ## 673     US02479 42.38867 -70.89480   22.00000         MA         <NA>
    ## 674     US02480 42.38217 -70.89330   20.00000         MA         <NA>
    ## 675     US02481 42.38148 -70.89355   20.00000         MA         <NA>
    ## 676     US02482 42.38175 -70.89370   20.00000         MA         <NA>
    ## 677     US02484 42.37765 -70.89125   21.00000         MA         <NA>
    ## 678     US02485 42.37778 -70.89113   21.00000         MA         <NA>
    ## 679   US02486.1 42.38887 -70.86958   24.00000         MA         <NA>
    ## 680     US02487 42.38862 -70.87000   24.00000         MA         <NA>
    ## 681     US02488 42.38868 -70.87017   24.00000         MA         <NA>
    ## 682     US02489 42.38798 -70.86782   26.00000         MA         <NA>
    ## 683     US02490 42.38802 -70.86808   26.00000         MA         <NA>
    ## 684     US02491 42.38803 -70.86812   26.00000         MA         <NA>
    ## 685     US02492 42.38230 -70.86520   25.00000         MA         <NA>
    ## 686     US02493 42.38223 -70.86518   25.00000         MA         <NA>
    ## 687     US02494 42.38215 -70.86542   25.00000         MA         <NA>
    ## 688     US02495 42.39757 -70.85685   24.00000         MA         <NA>
    ## 689     US02496 42.39860 -70.83973   24.00000         MA         <NA>
    ## 690     US02497 42.39847 -70.83992   24.00000         MA         <NA>
    ## 691     US02498 42.39007 -70.83740   26.00000         MA         <NA>
    ## 692     US02499 42.39052 -70.83677   26.00000         MA         <NA>
    ## 693     US02500 42.39022 -70.83735   26.00000         MA         <NA>
    ## 694     US02501 42.38625 -70.83457   29.00000         MA         <NA>
    ## 695   US02502.1 42.38625 -70.83457   29.00000         MA         <NA>
    ## 696     US02503 42.38612 -70.83463   29.00000         MA         <NA>
    ## 697     US02504 42.40150 -70.81485   30.00000         MA         <NA>
    ## 698   US02505.1 42.40173 -70.81518   30.00000         MA         <NA>
    ## 699   US02506.1 42.40153 -70.81488   30.00000         MA         <NA>
    ## 700     US02507 42.39523 -70.81628   34.00000         MA         <NA>
    ## 701     US02508 42.39537 -70.81650   34.00000         MA         <NA>
    ## 702     US02509 42.39548 -70.81610   34.00000         MA         <NA>
    ## 703     US02510 42.39028 -70.80782   26.00000         MA         <NA>
    ## 704     US02511 42.39080 -70.80797   26.00000         MA         <NA>
    ## 705     US02512 42.39053 -70.80802   26.00000         MA         <NA>
    ## 706     US02513 42.33707 -70.83737   24.00000         MA         <NA>
    ## 707     US02514 42.33715 -70.83730   24.00000         MA         <NA>
    ## 708   US02515.1 42.33740 -70.83645   24.00000         MA         <NA>
    ## 709     US02516 42.33030 -70.83720   25.00000         MA         <NA>
    ## 710     US02517 42.33048 -70.83722   25.00000         MA         <NA>
    ## 711     US02518 42.33042 -70.83693   25.00000         MA         <NA>
    ## 712     US02519 42.34223 -70.83673   23.00000         MA         <NA>
    ## 713     US02520 42.34215 -70.83685   23.00000         MA         <NA>
    ## 714   US02521.1 42.34202 -70.83703   23.00000         MA         <NA>
    ## 715     US02523 42.38842 -70.89493   22.00000         MA         <NA>
    ## 716     US02524 42.38863 -70.89465   22.00000         MA         <NA>
    ## 717   US02525.1 42.38200 -70.89342   20.00000         MA         <NA>
    ## 718     US02526 42.38152 -70.89377   20.00000         MA         <NA>
    ## 719     US02527 42.38213 -70.89325   20.00000         MA         <NA>
    ## 720     US02528 42.37753 -70.89123   21.00000         MA         <NA>
    ## 721     US02529 42.37752 -70.89168   21.00000         MA         <NA>
    ## 722     US02530 42.37738 -70.89152   21.00000         MA         <NA>
    ## 723     US02531 42.38875 -70.86973   24.00000         MA         <NA>
    ## 724     US02532 42.38860 -70.86990   24.00000         MA         <NA>
    ## 725     US02533 42.38865 -70.86975   24.00000         MA         <NA>
    ## 726     US02534 42.38833 -70.86780   26.00000         MA         <NA>
    ## 727     US02535 42.38822 -70.86770   26.00000         MA         <NA>
    ## 728   US02536.1 42.38793 -70.86775   26.00000         MA         <NA>
    ## 729   US02537.1 42.38220 -70.86512   25.00000         MA         <NA>
    ## 730     US02538 42.38240 -70.86518   25.00000         MA         <NA>
    ## 731     US02539 42.38250 -70.86538   25.00000         MA         <NA>
    ## 732     US02540 42.39835 -70.85602   24.00000         MA         <NA>
    ## 733     US02541 42.39868 -70.83958   24.00000         MA         <NA>
    ## 734     US02542 42.39815 -70.83962   24.00000         MA         <NA>
    ## 735     US02543 42.39023 -70.83647   26.00000         MA         <NA>
    ## 736     US02544 42.38988 -70.83630   26.00000         MA         <NA>
    ## 737   US02545.1 42.38968 -70.83688   26.00000         MA         <NA>
    ## 738     US02549 42.40123 -70.81435   30.00000         MA         <NA>
    ## 739     US02550 42.40142 -70.81452   30.00000         MA         <NA>
    ## 740     US02551 42.40133 -70.81467   30.00000         MA         <NA>
    ## 741     US02552 42.39567 -70.81565   34.00000         MA         <NA>
    ## 742     US02553 42.39550 -70.81578   34.00000         MA         <NA>
    ## 743     US02554 42.39523 -70.81613   34.00000         MA         <NA>
    ## 744     US02555 42.38993 -70.80753   26.00000         MA         <NA>
    ## 745   US02556.1 42.39062 -70.80763   26.00000         MA         <NA>
    ## 746     US02557 42.39053 -70.80737   26.00000         MA         <NA>
    ## 747     US02558 42.33660 -70.83743   24.00000         MA         <NA>
    ## 748     US02559 42.33698 -70.83715   24.00000         MA         <NA>
    ## 749     US02560 42.33638 -70.83762   24.00000         MA         <NA>
    ## 750     US02561 42.33027 -70.83690   25.00000         MA         <NA>
    ## 751     US02562 42.33048 -70.83682   25.00000         MA         <NA>
    ## 752   US02563.1 42.33010 -70.83683   25.00000         MA         <NA>
    ## 753     US02564 42.34218 -70.83713   23.00000         MA         <NA>
    ## 754     US02565 42.34193 -70.83717   23.00000         MA         <NA>
    ## 755     US02566 42.34198 -70.83692   23.00000         MA         <NA>
    ## 756     US02567 42.38727 -70.89603   22.00000         MA         <NA>
    ## 757     US02568 42.38077 -70.89442   20.00000         MA         <NA>
    ## 758     US02569 42.38745 -70.89565   22.00000         MA         <NA>
    ## 759     US02570 42.38077 -70.89417   20.00000         MA         <NA>
    ## 760   US02571.3 42.38713 -70.89567   22.00000         MA         <NA>
    ## 761     US02572 42.38077 -70.89417   20.00000         MA         <NA>
    ## 762     US02573 42.37628 -70.89202   21.00000         MA         <NA>
    ## 763     US02574 42.37612 -70.89215   21.00000         MA         <NA>
    ## 764     US02575 42.37627 -70.89227   21.00000         MA         <NA>
    ## 765     US02576 42.38612 -70.86960   24.00000         MA         <NA>
    ## 766     US02577 42.38805 -70.87082   24.00000         MA         <NA>
    ## 767     US02578 42.38745 -70.87062   24.00000         MA         <NA>
    ## 768     US02579 42.38675 -70.86905   26.00000         MA         <NA>
    ## 769     US02580 42.38692 -70.86890   26.00000         MA         <NA>
    ## 770     US02581 42.38703 -70.86952   26.00000         MA         <NA>
    ## 771     US02582 42.38122 -70.86660   25.00000         MA         <NA>
    ## 772   US02583.1 42.38105 -70.86698   25.00000         MA         <NA>
    ## 773   US02584.1 42.38108 -70.86598   25.00000         MA         <NA>
    ## 774     US02585 42.39725 -70.84067   24.00000         MA         <NA>
    ## 775   US02586.1 42.39725 -70.84092   24.00000         MA         <NA>
    ## 776     US02587 42.39740 -70.84078   24.00000         MA         <NA>
    ## 777     US02588 42.38920 -70.83728   26.00000         MA         <NA>
    ## 778     US02589 42.38905 -70.83717   26.00000         MA         <NA>
    ## 779     US02590 42.38947 -70.83802   26.00000         MA         <NA>
    ## 780     US02591 42.38468 -70.83640   29.00000         MA         <NA>
    ## 781     US02592 42.38500 -70.83587   29.00000         MA         <NA>
    ## 782     US02593 42.38467 -70.83665   29.00000         MA         <NA>
    ## 783     US02594 42.40005 -70.81588   30.00000         MA         <NA>
    ## 784     US02595 42.40050 -70.81597   30.00000         MA         <NA>
    ## 785   US02596.1 42.40033 -70.81637   30.00000         MA         <NA>
    ## 786     US02597 42.39442 -70.81748   34.00000         MA         <NA>
    ## 787     US02598 42.39440 -70.81773   34.00000         MA         <NA>
    ## 788     US02599 42.39440 -70.81798   34.00000         MA         <NA>
    ## 789   US02600.1 42.38957 -70.80903   26.00000         MA         <NA>
    ## 790     US02601 42.38942 -70.80892   26.00000         MA         <NA>
    ## 791     US02602 42.38942 -70.80867   26.00000         MA         <NA>
    ## 792     US02603 42.33647 -70.83800   24.00000         MA         <NA>
    ## 793     US02604 42.33665 -70.83737   24.00000         MA         <NA>
    ## 794     US02605 42.33633 -70.83763   24.00000         MA         <NA>
    ## 795     US02606 42.33010 -70.83730   25.00000         MA         <NA>
    ## 796     US02607 42.32978 -70.83757   25.00000         MA         <NA>
    ## 797     US02608 42.32963 -70.83747   25.00000         MA         <NA>
    ## 798     US02609 42.34183 -70.83717   23.00000         MA         <NA>
    ## 799     US02610 42.34135 -70.83758   23.00000         MA         <NA>
    ## 800     US02611 42.34197 -70.83753   23.00000         MA         <NA>
    ## 801     US04059 42.34083 -70.90750   14.02080         MA         <NA>
    ## 802     US04062 42.29867 -70.90350    3.96240         MA         <NA>
    ## 803     US04065 42.27300 -70.89367    6.40080         MA         <NA>
    ## 804     US04066 42.27967 -70.90533    9.75360         MA         <NA>
    ## 805     US04069 42.26467 -70.94083    4.87680         MA         <NA>
    ## 806     US04071 42.29900 -70.97433    5.48640         MA         <NA>
    ## 807     US04073 42.29333 -70.99167    3.96240         MA         <NA>
    ## 808     US04074 42.30900 -70.94667    7.01040         MA         <NA>
    ## 809     US04075 42.30850 -70.97433    4.11480         MA         <NA>
    ## 810     US04076 42.30567 -70.97817    4.26720         MA         <NA>
    ## 811     US04078 42.32183 -71.01350    6.70560         MA         <NA>
    ## 812     US04079 42.30667 -71.03533    7.62000         MA         <NA>
    ## 813     US04080 42.32583 -70.97633    5.18160         MA         <NA>
    ## 814     US04081 42.32217 -70.99250    5.79120         MA         <NA>
    ## 815     US04083 42.34267 -71.00533    5.79120         MA         <NA>
    ## 816     US04089 42.39200 -71.01650   10.66800         MA         <NA>
    ## 817     US04091 42.39600 -71.01300   10.05840         MA         <NA>
    ## 818     US04092 42.38567 -71.04017   12.49680         MA         <NA>
    ## 819     US04093 42.38567 -71.03517   13.10640         MA         <NA>
    ## 820     US04094 42.38550 -71.02467   10.97280         MA         <NA>
    ## 821     US04095 42.38417 -71.05033   10.66800         MA         <NA>
    ## 822     US04096 42.38667 -71.05500   11.58240         MA         <NA>
    ## 823     US04097 42.38500 -71.05350   12.80160         MA         <NA>
    ## 824     US04098 42.37367 -71.04533   13.71600         MA         <NA>
    ## 825     US04099 42.37267 -71.04850   13.71600         MA         <NA>
    ## 826     US04100 42.37250 -71.05133    8.83920         MA         <NA>
    ## 827     US04103 42.36317 -71.04317   15.24000         MA         <NA>
    ## 828     US04104 42.35017 -71.01567   11.58240         MA         <NA>
    ## 829     US04107 42.34250 -71.03000   10.97280         MA         <NA>
    ## 830     US04110 42.36650 -71.04900    7.31520         MA         <NA>
    ## 831     US06717 42.42833 -70.56667         NA         MA         <NA>
    ## 832     US06718 42.42833 -70.56667         NA         MA         <NA>
    ## 833     US06719 42.42833 -70.56667         NA         MA         <NA>
    ## 834     US06720 42.42833 -70.56667         NA         MA         <NA>
    ## 835     US06721 42.42833 -70.56667         NA         MA         <NA>
    ## 836     US06722 42.42833 -70.56667         NA         MA         <NA>
    ## 837     US06723 42.42833 -70.56667         NA         MA         <NA>
    ## 838     US06724 42.42833 -70.56667         NA         MA         <NA>
    ## 839     US06725 42.42833 -70.56667         NA         MA         <NA>
    ## 840     US06726 42.42833 -70.56667         NA         MA         <NA>
    ## 841     US06820 42.34100 -71.06200         NA         MA         <NA>
    ## 842     US06821 42.34100 -71.06200         NA         MA         <NA>
    ## 843     US06822 42.34100 -71.06200         NA         MA         <NA>
    ## 844     US06823 42.31877 -70.98336         NA         MA         <NA>
    ## 845     US06824 42.31877 -70.98336         NA         MA         <NA>
    ## 846     US06825 42.31877 -70.98336         NA         MA         <NA>
    ## 847     US06826 42.31788 -70.98138         NA         MA         <NA>
    ## 848     US06827 42.31788 -70.98138         NA         MA         <NA>
    ## 849     US06828 42.31788 -70.98138         NA         MA         <NA>
    ## 850     US06829 42.31708 -70.97947         NA         MA         <NA>
    ## 851     US06830 42.31708 -70.97947         NA         MA         <NA>
    ## 852     US06831 42.31708 -70.97947         NA         MA         <NA>
    ## 853     US06832 42.31902 -70.97744         NA         MA         <NA>
    ## 854     US06833 42.31902 -70.97744         NA         MA         <NA>
    ## 855     US06834 42.31902 -70.97744         NA         MA         <NA>
    ## 856     US06835 42.31506 -70.97456         NA         MA         <NA>
    ## 857     US06836 42.31506 -70.97456         NA         MA         <NA>
    ## 858     US06837 42.31506 -70.97456         NA         MA         <NA>
    ## 859     US06848 42.25400 -70.98000         NA         MA         <NA>
    ## 860     US06849 42.25400 -70.98000         NA         MA         <NA>
    ## 861     US06850 42.25400 -70.98000         NA         MA         <NA>
    ## 862     US06851 42.25400 -70.98000         NA         MA         <NA>
    ## 863     US06852 42.25400 -70.98000         NA         MA         <NA>
    ## 864     US06853 42.25400 -70.98000         NA         MA         <NA>
    ## 865     US06854 42.25400 -70.98000         NA         MA         <NA>
    ## 866     US06856 42.25400 -70.98000         NA         MA         <NA>
    ## 867     US06857 42.25400 -70.98000         NA         MA         <NA>
    ## 868     US06858 42.25400 -70.98000         NA         MA         <NA>
    ## 869     US06859 42.25400 -70.98000         NA         MA         <NA>
    ## 870     US06860 42.25400 -70.98000         NA         MA         <NA>
    ## 871     US06861 42.25400 -70.98000         NA         MA         <NA>
    ## 872     US06863 42.25400 -70.98000         NA         MA         <NA>
    ## 873     US06864 42.25400 -70.98000         NA         MA         <NA>
    ## 874     US06865 42.25400 -70.98000         NA         MA         <NA>
    ## 875     US06866 42.25400 -70.98000         NA         MA         <NA>
    ## 876     US06867 42.25400 -70.98000         NA         MA         <NA>
    ## 877     US06868 42.25400 -70.98000         NA         MA         <NA>
    ## 878     US06869 42.55000 -70.87500         NA         MA         SALE
    ## 879     US06870 42.55000 -70.87500         NA         MA         SALE
    ## 880     US06871       NA        NA         NA         MA         SALE
    ## 881     US07000       NA        NA         NA         MA         BOSS
    ## 882     US07001       NA        NA         NA         MA         BOSS
    ## 883     US07636 42.37764 -70.99889         NA         MA         <NA>
    ## 884     US07637 42.35383 -70.98986         NA         MA         <NA>
    ## 885     US07638 42.37856 -70.99750         NA         MA         <NA>
    ## 886     US07639 42.35333 -70.98958         NA         MA         <NA>
    ## 887     US07640 42.36083 -70.98694         NA         MA         <NA>
    ## 888     US07641 42.35986 -70.98681         NA         MA         <NA>
    ## 889     US07642 42.37723 -71.00000         NA         MA         <NA>
    ## 890     US07643 42.37723 -71.00000         NA         MA         <NA>
    ## 891     US07644 42.37723 -71.00000         NA         MA         <NA>
    ## 892     US07645 42.37723 -71.00000         NA         MA         <NA>
    ## 893     US07646 42.37723 -71.00000         NA         MA         <NA>
    ## 894     US07647 42.37723 -71.00000         NA         MA         <NA>
    ## 895     US07648 42.36047 -70.98833         NA         MA         <NA>
    ## 896     US07649 42.36047 -70.98833         NA         MA         <NA>
    ## 897     US07650 42.36047 -70.98833         NA         MA         <NA>
    ## 898     US07651 42.36047 -70.98833         NA         MA         <NA>
    ## 899     US07653 42.36047 -70.98833         NA         MA         <NA>
    ## 900     US07654 42.35452 -70.99195         NA         MA         <NA>
    ## 901     US07655 42.35452 -70.99195         NA         MA         <NA>
    ## 902     US07656 42.35452 -70.99195         NA         MA         <NA>
    ## 903     US07657 42.35452 -70.99195         NA         MA         <NA>
    ## 904     US07658 42.35452 -70.99195         NA         MA         <NA>
    ## 905     US07659 42.35452 -70.99195         NA         MA         <NA>
    ## 906     US07660 42.35452 -70.99195         NA         MA         <NA>
    ## 907     US07661 42.35452 -70.99195         NA         MA         <NA>
    ## 908     US07662 42.35452 -70.99195         NA         MA         <NA>
    ## 909     US07694       NA        NA         NA         MA         <NA>
    ## 910     US07695       NA        NA         NA         MA         <NA>
    ## 911     US07696       NA        NA         NA         MA         <NA>
    ## 912     US07697       NA        NA         NA         MA         <NA>
    ## 913     US07698       NA        NA         NA         MA         <NA>
    ## 914     US07699       NA        NA         NA         MA         <NA>
    ## 915     US07700       NA        NA         NA         MA         <NA>
    ## 916     US07701       NA        NA         NA         MA         <NA>
    ## 917     US07702       NA        NA         NA         MA         <NA>
    ## 918     US07703       NA        NA         NA         MA         <NA>
    ## 919     US07704       NA        NA         NA         MA         <NA>
    ## 920     US07705       NA        NA         NA         MA         <NA>
    ## 921     US07706       NA        NA         NA         MA         <NA>
    ## 922     US07707       NA        NA         NA         MA         <NA>
    ## 923     US07708       NA        NA         NA         MA         <NA>
    ## 924     US07709       NA        NA         NA         MA         <NA>
    ## 925     US07710       NA        NA         NA         MA         <NA>
    ## 926     US07711       NA        NA         NA         MA         <NA>
    ## 927     US07712       NA        NA         NA         MA         <NA>
    ## 928     US07713       NA        NA         NA         MA         <NA>
    ##                  GEN_LOC_NM
    ## 1       Boston Inner Harbor
    ## 2       Boston Inner Harbor
    ## 3       Boston Inner Harbor
    ## 4       Boston Inner Harbor
    ## 5       Boston Inner Harbor
    ## 6       Boston Inner Harbor
    ## 7       Boston Inner Harbor
    ## 8        Massachusetts Bays
    ## 9        Massachusetts Bays
    ## 10       Massachusetts Bays
    ## 11       Massachusetts Bays
    ## 12       Massachusetts Bays
    ## 13       Massachusetts Bays
    ## 14       Massachusetts Bays
    ## 15       Massachusetts Bays
    ## 16       Massachusetts Bays
    ## 17       Massachusetts Bays
    ## 18       Massachusetts Bays
    ## 19       Massachusetts Bays
    ## 20      Boston Inner Harbor
    ## 21      Boston Inner Harbor
    ## 22      Boston Inner Harbor
    ## 23      Boston Inner Harbor
    ## 24      Boston Inner Harbor
    ## 25  Northwest Boston Harbor
    ## 26  Northwest Boston Harbor
    ## 27  Northwest Boston Harbor
    ## 28  Northwest Boston Harbor
    ## 29  Northwest Boston Harbor
    ## 30      Boston Inner Harbor
    ## 31      Boston Inner Harbor
    ## 32  Northwest Boston Harbor
    ## 33  Northwest Boston Harbor
    ## 34  Northwest Boston Harbor
    ## 35       Massachusetts Bays
    ## 36       Massachusetts Bays
    ## 37       Massachusetts Bays
    ## 38  Southeast Boston Harbor
    ## 39  Southeast Boston Harbor
    ## 40  Southeast Boston Harbor
    ## 41      Boston Inner Harbor
    ## 42      Boston Inner Harbor
    ## 43      Boston Inner Harbor
    ## 44      Boston Inner Harbor
    ## 45       Massachusetts Bays
    ## 46       Massachusetts Bays
    ## 47       Massachusetts Bays
    ## 48       Massachusetts Bays
    ## 49       Massachusetts Bays
    ## 50       Massachusetts Bays
    ## 51       Massachusetts Bays
    ## 52       Massachusetts Bays
    ## 53       Massachusetts Bays
    ## 54       Massachusetts Bays
    ## 55  Northwest Boston Harbor
    ## 56  Northwest Boston Harbor
    ## 57  Northwest Boston Harbor
    ## 58  Northwest Boston Harbor
    ## 59  Northwest Boston Harbor
    ## 60  Northwest Boston Harbor
    ## 61  Northwest Boston Harbor
    ## 62  Northwest Boston Harbor
    ## 63  Northwest Boston Harbor
    ## 64  Northwest Boston Harbor
    ## 65  Northwest Boston Harbor
    ## 66  Northwest Boston Harbor
    ## 67  Northwest Boston Harbor
    ## 68  Northwest Boston Harbor
    ## 69  Northwest Boston Harbor
    ## 70  Northwest Boston Harbor
    ## 71  Northwest Boston Harbor
    ## 72  Northwest Boston Harbor
    ## 73  Northwest Boston Harbor
    ## 74  Northwest Boston Harbor
    ## 75  Northwest Boston Harbor
    ## 76  Northwest Boston Harbor
    ## 77  Northwest Boston Harbor
    ## 78  Northwest Boston Harbor
    ## 79  Northwest Boston Harbor
    ## 80      Boston Inner Harbor
    ## 81       Massachusetts Bays
    ## 82       Massachusetts Bays
    ## 83       Massachusetts Bays
    ## 84       Massachusetts Bays
    ## 85       Massachusetts Bays
    ## 86       Massachusetts Bays
    ## 87       Massachusetts Bays
    ## 88       Massachusetts Bays
    ## 89       Massachusetts Bays
    ## 90  Southeast Boston Harbor
    ## 91       Massachusetts Bays
    ## 92  Northwest Boston Harbor
    ## 93      Boston Inner Harbor
    ## 94      Boston Inner Harbor
    ## 95      Boston Inner Harbor
    ## 96       Massachusetts Bays
    ## 97       Massachusetts Bays
    ## 98       Massachusetts Bays
    ## 99       Massachusetts Bays
    ## 100      Massachusetts Bays
    ## 101 Northwest Boston Harbor
    ## 102 Northwest Boston Harbor
    ## 103 Northwest Boston Harbor
    ## 104 Northwest Boston Harbor
    ## 105           Cape Code Bay
    ## 106 Northwest Boston Harbor
    ## 107 Northwest Boston Harbor
    ## 108 Northwest Boston Harbor
    ## 109 Northwest Boston Harbor
    ## 110 Northwest Boston Harbor
    ## 111 Northwest Boston Harbor
    ## 112 Northwest Boston Harbor
    ## 113      Massachusetts Bays
    ## 114 Northwest Boston Harbor
    ## 115 Northwest Boston Harbor
    ## 116 Northwest Boston Harbor
    ## 117 Northwest Boston Harbor
    ## 118     Boston Inner Harbor
    ## 119     Boston Inner Harbor
    ## 120 Northwest Boston Harbor
    ## 121     Boston Inner Harbor
    ## 122     Boston Inner Harbor
    ## 123     Boston Inner Harbor
    ## 124           Inland/Rivers
    ## 125           Inland/Rivers
    ## 126           Inland/Rivers
    ## 127           Inland/Rivers
    ## 128           Inland/Rivers
    ## 129           Inland/Rivers
    ## 130           Inland/Rivers
    ## 131           Inland/Rivers
    ## 132           Inland/Rivers
    ## 133           Inland/Rivers
    ## 134           Inland/Rivers
    ## 135           Inland/Rivers
    ## 136           Inland/Rivers
    ## 137           Inland/Rivers
    ## 138           Inland/Rivers
    ## 139           Inland/Rivers
    ## 140           Inland/Rivers
    ## 141           Inland/Rivers
    ## 142           Inland/Rivers
    ## 143           Inland/Rivers
    ## 144           Inland/Rivers
    ## 145           Inland/Rivers
    ## 146           Inland/Rivers
    ## 147           Inland/Rivers
    ## 148           Inland/Rivers
    ## 149           Inland/Rivers
    ## 150           Inland/Rivers
    ## 151           Inland/Rivers
    ## 152           Inland/Rivers
    ## 153           Inland/Rivers
    ## 154           Inland/Rivers
    ## 155           Inland/Rivers
    ## 156           Inland/Rivers
    ## 157           Inland/Rivers
    ## 158           Inland/Rivers
    ## 159           Inland/Rivers
    ## 160           Inland/Rivers
    ## 161 Northwest Boston Harbor
    ## 162     Boston Inner Harbor
    ## 163     Boston Inner Harbor
    ## 164     Boston Inner Harbor
    ## 165           Inland/Rivers
    ## 166           Inland/Rivers
    ## 167 Northwest Boston Harbor
    ## 168 Northwest Boston Harbor
    ## 169 Northwest Boston Harbor
    ## 170           Inland/Rivers
    ## 171           Inland/Rivers
    ## 172           Inland/Rivers
    ## 173           Inland/Rivers
    ## 174           Inland/Rivers
    ## 175           Inland/Rivers
    ## 176           Inland/Rivers
    ## 177           Inland/Rivers
    ## 178           Inland/Rivers
    ## 179           Inland/Rivers
    ## 180           Inland/Rivers
    ## 181     Boston Inner Harbor
    ## 182     Boston Inner Harbor
    ## 183     Boston Inner Harbor
    ## 184     Boston Inner Harbor
    ## 185     Boston Inner Harbor
    ## 186     Boston Inner Harbor
    ## 187     Boston Inner Harbor
    ## 188     Boston Inner Harbor
    ## 189     Boston Inner Harbor
    ## 190     Boston Inner Harbor
    ## 191     Boston Inner Harbor
    ## 192     Boston Inner Harbor
    ## 193     Boston Inner Harbor
    ## 194     Boston Inner Harbor
    ## 195     Boston Inner Harbor
    ## 196     Boston Inner Harbor
    ## 197 Northwest Boston Harbor
    ## 198 Northwest Boston Harbor
    ## 199 Northwest Boston Harbor
    ## 200 Northwest Boston Harbor
    ## 201 Northwest Boston Harbor
    ## 202 Northwest Boston Harbor
    ## 203 Northwest Boston Harbor
    ## 204 Northwest Boston Harbor
    ## 205 Northwest Boston Harbor
    ## 206 Northwest Boston Harbor
    ## 207 Northwest Boston Harbor
    ## 208 Northwest Boston Harbor
    ## 209   Central Boston Harbor
    ## 210   Central Boston Harbor
    ## 211   Central Boston Harbor
    ## 212   Central Boston Harbor
    ## 213   Central Boston Harbor
    ## 214   Central Boston Harbor
    ## 215   Central Boston Harbor
    ## 216   Central Boston Harbor
    ## 217   Central Boston Harbor
    ## 218   Central Boston Harbor
    ## 219   Central Boston Harbor
    ## 220   Central Boston Harbor
    ## 221 Southeast Boston Harbor
    ## 222 Southeast Boston Harbor
    ## 223 Southeast Boston Harbor
    ## 224 Southeast Boston Harbor
    ## 225 Southeast Boston Harbor
    ## 226 Southeast Boston Harbor
    ## 227      Massachusetts Bays
    ## 228      Massachusetts Bays
    ## 229      Massachusetts Bays
    ## 230      Massachusetts Bays
    ## 231      Massachusetts Bays
    ## 232      Massachusetts Bays
    ## 233      Massachusetts Bays
    ## 234      Massachusetts Bays
    ## 235      Massachusetts Bays
    ## 236      Massachusetts Bays
    ## 237      Massachusetts Bays
    ## 238      Massachusetts Bays
    ## 239      Massachusetts Bays
    ## 240      Massachusetts Bays
    ## 241      Massachusetts Bays
    ## 242      Massachusetts Bays
    ## 243      Massachusetts Bays
    ## 244      Massachusetts Bays
    ## 245      Massachusetts Bays
    ## 246      Massachusetts Bays
    ## 247      Massachusetts Bays
    ## 248      Massachusetts Bays
    ## 249      Massachusetts Bays
    ## 250      Massachusetts Bays
    ## 251      Massachusetts Bays
    ## 252      Massachusetts Bays
    ## 253      Massachusetts Bays
    ## 254      Massachusetts Bays
    ## 255      Massachusetts Bays
    ## 256      Massachusetts Bays
    ## 257      Massachusetts Bays
    ## 258      Massachusetts Bays
    ## 259      Massachusetts Bays
    ## 260      Massachusetts Bays
    ## 261      Massachusetts Bays
    ## 262      Massachusetts Bays
    ## 263      Massachusetts Bays
    ## 264      Massachusetts Bays
    ## 265      Massachusetts Bays
    ## 266      Massachusetts Bays
    ## 267      Massachusetts Bays
    ## 268      Massachusetts Bays
    ## 269      Massachusetts Bays
    ## 270      Massachusetts Bays
    ## 271      Massachusetts Bays
    ## 272      Massachusetts Bays
    ## 273      Massachusetts Bays
    ## 274      Massachusetts Bays
    ## 275      Massachusetts Bays
    ## 276      Massachusetts Bays
    ## 277      Massachusetts Bays
    ## 278      Massachusetts Bays
    ## 279      Massachusetts Bays
    ## 280      Massachusetts Bays
    ## 281           Cape Code Bay
    ## 282           Cape Code Bay
    ## 283           Cape Code Bay
    ## 284           Cape Code Bay
    ## 285           Cape Code Bay
    ## 286           Cape Code Bay
    ## 287           Cape Code Bay
    ## 288           Cape Code Bay
    ## 289           Cape Code Bay
    ## 290           Cape Code Bay
    ## 291           Cape Code Bay
    ## 292           Cape Code Bay
    ## 293   Central Boston Harbor
    ## 294      Massachusetts Bays
    ## 295 Northwest Boston Harbor
    ## 296 Northwest Boston Harbor
    ## 297 Northwest Boston Harbor
    ## 298 Northwest Boston Harbor
    ## 299     Boston Inner Harbor
    ## 300     Boston Inner Harbor
    ## 301     Boston Inner Harbor
    ## 302     Boston Inner Harbor
    ## 303      Massachusetts Bays
    ## 304 Northwest Boston Harbor
    ## 305 Northwest Boston Harbor
    ## 306 Northwest Boston Harbor
    ## 307 Northwest Boston Harbor
    ## 308 Northwest Boston Harbor
    ## 309 Northwest Boston Harbor
    ## 310 Northwest Boston Harbor
    ## 311 Northwest Boston Harbor
    ## 312 Northwest Boston Harbor
    ## 313 Northwest Boston Harbor
    ## 314 Northwest Boston Harbor
    ## 315 Northwest Boston Harbor
    ## 316 Northwest Boston Harbor
    ## 317 Northwest Boston Harbor
    ## 318 Northwest Boston Harbor
    ## 319 Northwest Boston Harbor
    ## 320 Northwest Boston Harbor
    ## 321 Northwest Boston Harbor
    ## 322 Northwest Boston Harbor
    ## 323 Northwest Boston Harbor
    ## 324 Northwest Boston Harbor
    ## 325      Massachusetts Bays
    ## 326      Massachusetts Bays
    ## 327      Massachusetts Bays
    ## 328      Massachusetts Bays
    ## 329      Massachusetts Bays
    ## 330      Massachusetts Bays
    ## 331      Massachusetts Bays
    ## 332      Massachusetts Bays
    ## 333      Massachusetts Bays
    ## 334      Massachusetts Bays
    ## 335   Central Boston Harbor
    ## 336   Central Boston Harbor
    ## 337   Central Boston Harbor
    ## 338   Central Boston Harbor
    ## 339   Central Boston Harbor
    ## 340   Central Boston Harbor
    ## 341   Central Boston Harbor
    ## 342   Central Boston Harbor
    ## 343   Central Boston Harbor
    ## 344   Central Boston Harbor
    ## 345   Central Boston Harbor
    ## 346   Central Boston Harbor
    ## 347   Central Boston Harbor
    ## 348   Central Boston Harbor
    ## 349   Central Boston Harbor
    ## 350   Central Boston Harbor
    ## 351   Central Boston Harbor
    ## 352   Central Boston Harbor
    ## 353   Central Boston Harbor
    ## 354   Central Boston Harbor
    ## 355   Central Boston Harbor
    ## 356 Northwest Boston Harbor
    ## 357 Northwest Boston Harbor
    ## 358 Northwest Boston Harbor
    ## 359 Northwest Boston Harbor
    ## 360 Northwest Boston Harbor
    ## 361 Northwest Boston Harbor
    ## 362 Northwest Boston Harbor
    ## 363 Northwest Boston Harbor
    ## 364 Northwest Boston Harbor
    ## 365 Northwest Boston Harbor
    ## 366 Northwest Boston Harbor
    ## 367 Northwest Boston Harbor
    ## 368 Northwest Boston Harbor
    ## 369 Northwest Boston Harbor
    ## 370       Harbor Approaches
    ## 371       Harbor Approaches
    ## 372       Harbor Approaches
    ## 373       Harbor Approaches
    ## 374       Harbor Approaches
    ## 375       Harbor Approaches
    ## 376       Harbor Approaches
    ## 377 Northwest Boston Harbor
    ## 378 Northwest Boston Harbor
    ## 379 Northwest Boston Harbor
    ## 380 Northwest Boston Harbor
    ## 381 Northwest Boston Harbor
    ## 382 Northwest Boston Harbor
    ## 383 Northwest Boston Harbor
    ## 384 Northwest Boston Harbor
    ## 385 Northwest Boston Harbor
    ## 386 Northwest Boston Harbor
    ## 387 Northwest Boston Harbor
    ## 388 Northwest Boston Harbor
    ## 389 Northwest Boston Harbor
    ## 390 Northwest Boston Harbor
    ## 391     Boston Inner Harbor
    ## 392     Boston Inner Harbor
    ## 393 Northwest Boston Harbor
    ## 394 Northwest Boston Harbor
    ## 395 Northwest Boston Harbor
    ## 396 Northwest Boston Harbor
    ## 397 Northwest Boston Harbor
    ## 398 Northwest Boston Harbor
    ## 399 Northwest Boston Harbor
    ## 400       Harbor Approaches
    ## 401       Harbor Approaches
    ## 402       Harbor Approaches
    ## 403       Harbor Approaches
    ## 404       Harbor Approaches
    ## 405       Harbor Approaches
    ## 406       Harbor Approaches
    ## 407       Harbor Approaches
    ## 408       Harbor Approaches
    ## 409       Harbor Approaches
    ## 410       Harbor Approaches
    ## 411       Harbor Approaches
    ## 412       Harbor Approaches
    ## 413       Harbor Approaches
    ## 414      Massachusetts Bays
    ## 415      Massachusetts Bays
    ## 416      Massachusetts Bays
    ## 417      Massachusetts Bays
    ## 418      Massachusetts Bays
    ## 419      Massachusetts Bays
    ## 420      Massachusetts Bays
    ## 421     Boston Inner Harbor
    ## 422     Boston Inner Harbor
    ## 423     Boston Inner Harbor
    ## 424     Boston Inner Harbor
    ## 425     Boston Inner Harbor
    ## 426      Massachusetts Bays
    ## 427      Massachusetts Bays
    ## 428      Massachusetts Bays
    ## 429      Massachusetts Bays
    ## 430      Massachusetts Bays
    ## 431      Massachusetts Bays
    ## 432      Massachusetts Bays
    ## 433      Massachusetts Bays
    ## 434      Massachusetts Bays
    ## 435      Massachusetts Bays
    ## 436      Massachusetts Bays
    ## 437      Massachusetts Bays
    ## 438      Massachusetts Bays
    ## 439      Massachusetts Bays
    ## 440      Massachusetts Bays
    ## 441      Massachusetts Bays
    ## 442      Massachusetts Bays
    ## 443      Massachusetts Bays
    ## 444      Massachusetts Bays
    ## 445      Massachusetts Bays
    ## 446      Massachusetts Bays
    ## 447      Massachusetts Bays
    ## 448      Massachusetts Bays
    ## 449      Massachusetts Bays
    ## 450      Massachusetts Bays
    ## 451      Massachusetts Bays
    ## 452      Massachusetts Bays
    ## 453      Massachusetts Bays
    ## 454      Massachusetts Bays
    ## 455      Massachusetts Bays
    ## 456      Massachusetts Bays
    ## 457      Massachusetts Bays
    ## 458      Massachusetts Bays
    ## 459      Massachusetts Bays
    ## 460      Massachusetts Bays
    ## 461      Massachusetts Bays
    ## 462      Massachusetts Bays
    ## 463      Massachusetts Bays
    ## 464      Massachusetts Bays
    ## 465      Massachusetts Bays
    ## 466      Massachusetts Bays
    ## 467      Massachusetts Bays
    ## 468      Massachusetts Bays
    ## 469      Massachusetts Bays
    ## 470      Massachusetts Bays
    ## 471      Massachusetts Bays
    ## 472      Massachusetts Bays
    ## 473      Massachusetts Bays
    ## 474      Massachusetts Bays
    ## 475      Massachusetts Bays
    ## 476      Massachusetts Bays
    ## 477      Massachusetts Bays
    ## 478      Massachusetts Bays
    ## 479      Massachusetts Bays
    ## 480      Massachusetts Bays
    ## 481      Massachusetts Bays
    ## 482      Massachusetts Bays
    ## 483      Massachusetts Bays
    ## 484      Massachusetts Bays
    ## 485      Massachusetts Bays
    ## 486      Massachusetts Bays
    ## 487      Massachusetts Bays
    ## 488      Massachusetts Bays
    ## 489      Massachusetts Bays
    ## 490      Massachusetts Bays
    ## 491      Massachusetts Bays
    ## 492      Massachusetts Bays
    ## 493      Massachusetts Bays
    ## 494      Massachusetts Bays
    ## 495      Massachusetts Bays
    ## 496      Massachusetts Bays
    ## 497      Massachusetts Bays
    ## 498      Massachusetts Bays
    ## 499      Massachusetts Bays
    ## 500      Massachusetts Bays
    ## 501      Massachusetts Bays
    ## 502      Massachusetts Bays
    ## 503      Massachusetts Bays
    ## 504      Massachusetts Bays
    ## 505      Massachusetts Bays
    ## 506      Massachusetts Bays
    ## 507      Massachusetts Bays
    ## 508      Massachusetts Bays
    ## 509      Massachusetts Bays
    ## 510      Massachusetts Bays
    ## 511      Massachusetts Bays
    ## 512      Massachusetts Bays
    ## 513      Massachusetts Bays
    ## 514      Massachusetts Bays
    ## 515      Massachusetts Bays
    ## 516      Massachusetts Bays
    ## 517      Massachusetts Bays
    ## 518      Massachusetts Bays
    ## 519      Massachusetts Bays
    ## 520      Massachusetts Bays
    ## 521      Massachusetts Bays
    ## 522      Massachusetts Bays
    ## 523      Massachusetts Bays
    ## 524      Massachusetts Bays
    ## 525      Massachusetts Bays
    ## 526           Cape Code Bay
    ## 527           Cape Code Bay
    ## 528           Cape Code Bay
    ## 529           Cape Code Bay
    ## 530           Cape Code Bay
    ## 531           Cape Code Bay
    ## 532           Cape Code Bay
    ## 533           Cape Code Bay
    ## 534           Cape Code Bay
    ## 535           Cape Code Bay
    ## 536           Cape Code Bay
    ## 537           Cape Code Bay
    ## 538           Cape Code Bay
    ## 539           Cape Code Bay
    ## 540           Cape Code Bay
    ## 541           Cape Code Bay
    ## 542           Cape Code Bay
    ## 543           Cape Code Bay
    ## 544           Cape Code Bay
    ## 545           Cape Code Bay
    ## 546           Cape Code Bay
    ## 547      Massachusetts Bays
    ## 548      Massachusetts Bays
    ## 549      Massachusetts Bays
    ## 550      Massachusetts Bays
    ## 551      Massachusetts Bays
    ## 552      Massachusetts Bays
    ## 553      Massachusetts Bays
    ## 554      Massachusetts Bays
    ## 555      Massachusetts Bays
    ## 556      Massachusetts Bays
    ## 557      Massachusetts Bays
    ## 558      Massachusetts Bays
    ## 559      Massachusetts Bays
    ## 560      Massachusetts Bays
    ## 561      Massachusetts Bays
    ## 562      Massachusetts Bays
    ## 563      Massachusetts Bays
    ## 564      Massachusetts Bays
    ## 565      Massachusetts Bays
    ## 566      Massachusetts Bays
    ## 567      Massachusetts Bays
    ## 568      Massachusetts Bays
    ## 569      Massachusetts Bays
    ## 570      Massachusetts Bays
    ## 571      Massachusetts Bays
    ## 572     Boston Inner Harbor
    ## 573     Boston Inner Harbor
    ## 574     Boston Inner Harbor
    ## 575     Boston Inner Harbor
    ## 576     Boston Inner Harbor
    ## 577     Boston Inner Harbor
    ## 578     Boston Inner Harbor
    ## 579     Boston Inner Harbor
    ## 580 Northwest Boston Harbor
    ## 581 Northwest Boston Harbor
    ## 582 Northwest Boston Harbor
    ## 583 Northwest Boston Harbor
    ## 584 Northwest Boston Harbor
    ## 585 Northwest Boston Harbor
    ## 586 Northwest Boston Harbor
    ## 587 Northwest Boston Harbor
    ## 588 Northwest Boston Harbor
    ## 589 Northwest Boston Harbor
    ## 590 Northwest Boston Harbor
    ## 591 Northwest Boston Harbor
    ## 592 Northwest Boston Harbor
    ## 593 Northwest Boston Harbor
    ## 594 Northwest Boston Harbor
    ## 595 Northwest Boston Harbor
    ## 596 Northwest Boston Harbor
    ## 597 Northwest Boston Harbor
    ## 598 Northwest Boston Harbor
    ## 599 Northwest Boston Harbor
    ## 600 Northwest Boston Harbor
    ## 601     Boston Inner Harbor
    ## 602     Boston Inner Harbor
    ## 603     Boston Inner Harbor
    ## 604     Boston Inner Harbor
    ## 605     Boston Inner Harbor
    ## 606     Boston Inner Harbor
    ## 607     Boston Inner Harbor
    ## 608     Boston Inner Harbor
    ## 609     Boston Inner Harbor
    ## 610     Boston Inner Harbor
    ## 611     Boston Inner Harbor
    ## 612           Inland/Rivers
    ## 613   Central Boston Harbor
    ## 614 Northwest Boston Harbor
    ## 615   Central Boston Harbor
    ## 616 Northwest Boston Harbor
    ## 617       Harbor Approaches
    ## 618 Northwest Boston Harbor
    ## 619       Harbor Approaches
    ## 620     Boston Inner Harbor
    ## 621 Northwest Boston Harbor
    ## 622     Boston Inner Harbor
    ## 623     Boston Inner Harbor
    ## 624     Boston Inner Harbor
    ## 625     Boston Inner Harbor
    ## 626     Boston Inner Harbor
    ## 627     Boston Inner Harbor
    ## 628     Boston Inner Harbor
    ## 629     Boston Inner Harbor
    ## 630     Boston Inner Harbor
    ## 631     Boston Inner Harbor
    ## 632     Boston Inner Harbor
    ## 633     Boston Inner Harbor
    ## 634     Boston Inner Harbor
    ## 635     Boston Inner Harbor
    ## 636     Boston Inner Harbor
    ## 637     Boston Inner Harbor
    ## 638      Massachusetts Bays
    ## 639      Massachusetts Bays
    ## 640      Massachusetts Bays
    ## 641      Massachusetts Bays
    ## 642      Massachusetts Bays
    ## 643      Massachusetts Bays
    ## 644      Massachusetts Bays
    ## 645      Massachusetts Bays
    ## 646      Massachusetts Bays
    ## 647      Massachusetts Bays
    ## 648      Massachusetts Bays
    ## 649      Massachusetts Bays
    ## 650      Massachusetts Bays
    ## 651      Massachusetts Bays
    ## 652     Boston Inner Harbor
    ## 653     Boston Inner Harbor
    ## 654     Boston Inner Harbor
    ## 655     Boston Inner Harbor
    ## 656     Boston Inner Harbor
    ## 657     Boston Inner Harbor
    ## 658     Boston Inner Harbor
    ## 659      Massachusetts Bays
    ## 660      Massachusetts Bays
    ## 661      Massachusetts Bays
    ## 662      Massachusetts Bays
    ## 663      Massachusetts Bays
    ## 664      Massachusetts Bays
    ## 665      Massachusetts Bays
    ## 666      Massachusetts Bays
    ## 667      Massachusetts Bays
    ## 668     Boston Inner Harbor
    ## 669      Massachusetts Bays
    ## 670      Massachusetts Bays
    ## 671      Massachusetts Bays
    ## 672      Massachusetts Bays
    ## 673      Massachusetts Bays
    ## 674      Massachusetts Bays
    ## 675      Massachusetts Bays
    ## 676      Massachusetts Bays
    ## 677      Massachusetts Bays
    ## 678      Massachusetts Bays
    ## 679      Massachusetts Bays
    ## 680      Massachusetts Bays
    ## 681      Massachusetts Bays
    ## 682      Massachusetts Bays
    ## 683      Massachusetts Bays
    ## 684      Massachusetts Bays
    ## 685      Massachusetts Bays
    ## 686      Massachusetts Bays
    ## 687      Massachusetts Bays
    ## 688      Massachusetts Bays
    ## 689      Massachusetts Bays
    ## 690      Massachusetts Bays
    ## 691      Massachusetts Bays
    ## 692      Massachusetts Bays
    ## 693      Massachusetts Bays
    ## 694      Massachusetts Bays
    ## 695      Massachusetts Bays
    ## 696      Massachusetts Bays
    ## 697      Massachusetts Bays
    ## 698      Massachusetts Bays
    ## 699      Massachusetts Bays
    ## 700      Massachusetts Bays
    ## 701      Massachusetts Bays
    ## 702      Massachusetts Bays
    ## 703      Massachusetts Bays
    ## 704      Massachusetts Bays
    ## 705      Massachusetts Bays
    ## 706      Massachusetts Bays
    ## 707      Massachusetts Bays
    ## 708      Massachusetts Bays
    ## 709      Massachusetts Bays
    ## 710      Massachusetts Bays
    ## 711      Massachusetts Bays
    ## 712      Massachusetts Bays
    ## 713      Massachusetts Bays
    ## 714      Massachusetts Bays
    ## 715      Massachusetts Bays
    ## 716      Massachusetts Bays
    ## 717      Massachusetts Bays
    ## 718      Massachusetts Bays
    ## 719      Massachusetts Bays
    ## 720      Massachusetts Bays
    ## 721      Massachusetts Bays
    ## 722      Massachusetts Bays
    ## 723      Massachusetts Bays
    ## 724      Massachusetts Bays
    ## 725      Massachusetts Bays
    ## 726      Massachusetts Bays
    ## 727      Massachusetts Bays
    ## 728      Massachusetts Bays
    ## 729      Massachusetts Bays
    ## 730      Massachusetts Bays
    ## 731      Massachusetts Bays
    ## 732      Massachusetts Bays
    ## 733      Massachusetts Bays
    ## 734      Massachusetts Bays
    ## 735      Massachusetts Bays
    ## 736      Massachusetts Bays
    ## 737      Massachusetts Bays
    ## 738      Massachusetts Bays
    ## 739      Massachusetts Bays
    ## 740      Massachusetts Bays
    ## 741      Massachusetts Bays
    ## 742      Massachusetts Bays
    ## 743      Massachusetts Bays
    ## 744      Massachusetts Bays
    ## 745      Massachusetts Bays
    ## 746      Massachusetts Bays
    ## 747      Massachusetts Bays
    ## 748      Massachusetts Bays
    ## 749      Massachusetts Bays
    ## 750      Massachusetts Bays
    ## 751      Massachusetts Bays
    ## 752      Massachusetts Bays
    ## 753      Massachusetts Bays
    ## 754      Massachusetts Bays
    ## 755      Massachusetts Bays
    ## 756      Massachusetts Bays
    ## 757      Massachusetts Bays
    ## 758      Massachusetts Bays
    ## 759      Massachusetts Bays
    ## 760      Massachusetts Bays
    ## 761      Massachusetts Bays
    ## 762      Massachusetts Bays
    ## 763      Massachusetts Bays
    ## 764      Massachusetts Bays
    ## 765      Massachusetts Bays
    ## 766      Massachusetts Bays
    ## 767      Massachusetts Bays
    ## 768      Massachusetts Bays
    ## 769      Massachusetts Bays
    ## 770      Massachusetts Bays
    ## 771      Massachusetts Bays
    ## 772      Massachusetts Bays
    ## 773      Massachusetts Bays
    ## 774      Massachusetts Bays
    ## 775      Massachusetts Bays
    ## 776      Massachusetts Bays
    ## 777      Massachusetts Bays
    ## 778      Massachusetts Bays
    ## 779      Massachusetts Bays
    ## 780      Massachusetts Bays
    ## 781      Massachusetts Bays
    ## 782      Massachusetts Bays
    ## 783      Massachusetts Bays
    ## 784      Massachusetts Bays
    ## 785      Massachusetts Bays
    ## 786      Massachusetts Bays
    ## 787      Massachusetts Bays
    ## 788      Massachusetts Bays
    ## 789      Massachusetts Bays
    ## 790      Massachusetts Bays
    ## 791      Massachusetts Bays
    ## 792      Massachusetts Bays
    ## 793      Massachusetts Bays
    ## 794      Massachusetts Bays
    ## 795      Massachusetts Bays
    ## 796      Massachusetts Bays
    ## 797      Massachusetts Bays
    ## 798      Massachusetts Bays
    ## 799      Massachusetts Bays
    ## 800      Massachusetts Bays
    ## 801       Harbor Approaches
    ## 802 Southeast Boston Harbor
    ## 803 Southeast Boston Harbor
    ## 804 Southeast Boston Harbor
    ## 805 Southeast Boston Harbor
    ## 806   Central Boston Harbor
    ## 807   Central Boston Harbor
    ## 808   Central Boston Harbor
    ## 809   Central Boston Harbor
    ## 810   Central Boston Harbor
    ## 811 Northwest Boston Harbor
    ## 812 Northwest Boston Harbor
    ## 813 Northwest Boston Harbor
    ## 814 Northwest Boston Harbor
    ## 815 Northwest Boston Harbor
    ## 816     Boston Inner Harbor
    ## 817     Boston Inner Harbor
    ## 818     Boston Inner Harbor
    ## 819     Boston Inner Harbor
    ## 820     Boston Inner Harbor
    ## 821     Boston Inner Harbor
    ## 822     Boston Inner Harbor
    ## 823     Boston Inner Harbor
    ## 824     Boston Inner Harbor
    ## 825     Boston Inner Harbor
    ## 826     Boston Inner Harbor
    ## 827     Boston Inner Harbor
    ## 828     Boston Inner Harbor
    ## 829     Boston Inner Harbor
    ## 830     Boston Inner Harbor
    ## 831      Massachusetts Bays
    ## 832      Massachusetts Bays
    ## 833      Massachusetts Bays
    ## 834      Massachusetts Bays
    ## 835      Massachusetts Bays
    ## 836      Massachusetts Bays
    ## 837      Massachusetts Bays
    ## 838      Massachusetts Bays
    ## 839      Massachusetts Bays
    ## 840      Massachusetts Bays
    ## 841     Boston Inner Harbor
    ## 842     Boston Inner Harbor
    ## 843     Boston Inner Harbor
    ## 844 Northwest Boston Harbor
    ## 845 Northwest Boston Harbor
    ## 846 Northwest Boston Harbor
    ## 847 Northwest Boston Harbor
    ## 848 Northwest Boston Harbor
    ## 849 Northwest Boston Harbor
    ## 850 Northwest Boston Harbor
    ## 851 Northwest Boston Harbor
    ## 852 Northwest Boston Harbor
    ## 853 Northwest Boston Harbor
    ## 854 Northwest Boston Harbor
    ## 855 Northwest Boston Harbor
    ## 856 Northwest Boston Harbor
    ## 857 Northwest Boston Harbor
    ## 858 Northwest Boston Harbor
    ## 859 Southeast Boston Harbor
    ## 860 Southeast Boston Harbor
    ## 861 Southeast Boston Harbor
    ## 862 Southeast Boston Harbor
    ## 863 Southeast Boston Harbor
    ## 864 Southeast Boston Harbor
    ## 865 Southeast Boston Harbor
    ## 866 Southeast Boston Harbor
    ## 867 Southeast Boston Harbor
    ## 868 Southeast Boston Harbor
    ## 869 Southeast Boston Harbor
    ## 870 Southeast Boston Harbor
    ## 871 Southeast Boston Harbor
    ## 872 Southeast Boston Harbor
    ## 873 Southeast Boston Harbor
    ## 874 Southeast Boston Harbor
    ## 875 Southeast Boston Harbor
    ## 876 Southeast Boston Harbor
    ## 877 Southeast Boston Harbor
    ## 878      Massachusetts Bays
    ## 879      Massachusetts Bays
    ## 880      Massachusetts Bays
    ## 881 Northwest Boston Harbor
    ## 882 Northwest Boston Harbor
    ## 883 Northwest Boston Harbor
    ## 884 Northwest Boston Harbor
    ## 885 Northwest Boston Harbor
    ## 886 Northwest Boston Harbor
    ## 887 Northwest Boston Harbor
    ## 888 Northwest Boston Harbor
    ## 889 Northwest Boston Harbor
    ## 890 Northwest Boston Harbor
    ## 891 Northwest Boston Harbor
    ## 892 Northwest Boston Harbor
    ## 893 Northwest Boston Harbor
    ## 894 Northwest Boston Harbor
    ## 895 Northwest Boston Harbor
    ## 896 Northwest Boston Harbor
    ## 897 Northwest Boston Harbor
    ## 898 Northwest Boston Harbor
    ## 899 Northwest Boston Harbor
    ## 900 Northwest Boston Harbor
    ## 901 Northwest Boston Harbor
    ## 902 Northwest Boston Harbor
    ## 903 Northwest Boston Harbor
    ## 904 Northwest Boston Harbor
    ## 905 Northwest Boston Harbor
    ## 906 Northwest Boston Harbor
    ## 907 Northwest Boston Harbor
    ## 908 Northwest Boston Harbor
    ## 909 Northwest Boston Harbor
    ## 910 Northwest Boston Harbor
    ## 911 Northwest Boston Harbor
    ## 912 Northwest Boston Harbor
    ## 913 Northwest Boston Harbor
    ## 914 Northwest Boston Harbor
    ## 915 Northwest Boston Harbor
    ## 916 Northwest Boston Harbor
    ## 917 Northwest Boston Harbor
    ## 918 Northwest Boston Harbor
    ## 919 Northwest Boston Harbor
    ## 920 Northwest Boston Harbor
    ## 921 Northwest Boston Harbor
    ## 922 Northwest Boston Harbor
    ## 923 Northwest Boston Harbor
    ## 924 Northwest Boston Harbor
    ## 925 Northwest Boston Harbor
    ## 926 Northwest Boston Harbor
    ## 927 Northwest Boston Harbor
    ## 928 Northwest Boston Harbor
    ##                                                                                    SPECFC_LOC
    ## 1                                                                                         BIH
    ## 2                                                                                         BIH
    ## 3                                                                                         BIH
    ## 4                                                                                         BIH
    ## 5                                                                                         BIH
    ## 6                                                                                         BIH
    ## 7                                                                                         BIH
    ## 8                                                                                        MBDS
    ## 9                                                                                        MBDS
    ## 10                                                                                       MBDS
    ## 11                                                                                       MBDS
    ## 12                                                                                       MBDS
    ## 13                                                                                       MBDS
    ## 14                                                                                       MBDS
    ## 15                                                                                       MBDS
    ## 16                                                                                       MBDS
    ## 17                                                                                       MBDS
    ## 18                                                                                       MBDS
    ## 19                                                                                       MBDS
    ## 20  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 21  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 22  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 23  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 24  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 25  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 26  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 27  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 28  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 29  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 30  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 31  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 32                                                                             DORCHESTER BAY
    ## 33                                                                        CRYSTAL COVE MARINA
    ## 34                                                                        CRYSTAL COVE MARINA
    ## 35                                                                                 BASS RIVER
    ## 36                                                                                 BASS RIVER
    ## 37                                                                                 BASS RIVER
    ## 38                                                                              HINGAM HARBOR
    ## 39                                                                             HINGHAM HARBOR
    ## 40                                                                             HINGHAM HARBOR
    ## 41                                                                               MYSTIC RIVER
    ## 42                                                                               MYSTIC RIVER
    ## 43                                                                               MYSTIC RIVER
    ## 44                                                                               MYSTIC RIVER
    ## 45                                                                                 SMITH COVE
    ## 46                                                                                 SMITH COVE
    ## 47                                                                                 SMITH COVE
    ## 48                                                                                 SMITH COVE
    ## 49                                                                                 SMITH COVE
    ## 50                                                                                 SMITH COVE
    ## 51                                                                                 SMITH COVE
    ## 52                                                                                 SMITH COVE
    ## 53                                                                                 SMITH COVE
    ## 54                                                                                 SMITH COVE
    ## 55                                                                            WINTHROP HARBOR
    ## 56                                                                            WINTHROP HARBOR
    ## 57                                                                            WINTHROP HARBOR
    ## 58                                                                            WINTHROP HARBOR
    ## 59                                                                            WINTHROP HARBOR
    ## 60                                                                            WINTHROP HARBOR
    ## 61                                                                            WINTHROP HARBOR
    ## 62                                                                            WINTHROP HARBOR
    ## 63                                                                             WINTROP HARBOR
    ## 64                                                                            WINTHROP HARBOR
    ## 65                                                                            WINTHROP HARBOR
    ## 66                                                                            WINTHROP HARBOR
    ## 67                                                                            WINTHROP HARBOR
    ## 68                                                                            WINTHROP HARBOR
    ## 69                                                                            WINTHROP HARBOR
    ## 70                                                                            WINTHROP HARBOR
    ## 71                                                                            WINTHROP HARBOR
    ## 72                                                                            WINTHROP HARBOR
    ## 73                                                                            WINTHROP HARBOR
    ## 74                                                                            WINTHROP HARBOR
    ## 75                                                                            WINTHROP HARBOR
    ## 76                                                                            WINTHROP HARBOR
    ## 77                                                                            WINTHROP HARBOR
    ## 78                                                                            WINTHROP HARBOR
    ## 79                                                                            WINTHROP HARBOR
    ## 80                                                                      Little Mystic Channel
    ## 81                                                                          MANCHESTER HARBOR
    ## 82                                                                          MANCHESTER HARBOR
    ## 83                                                                          MANCHESTER HARBOR
    ## 84                                                                          MANCHESTER HARBOR
    ## 85                                                                            SCITUATE HARBOR
    ## 86                                                                                 FORE RIVER
    ## 87                                                                                 FORE RIVER
    ## 88                                                                                 FORE RIVER
    ## 89                                                                                 FORE RIVER
    ## 90                                                                             HINGHAM HARBOR
    ## 91                                                                               SALEM HARBOR
    ## 92                                                                       BOSTON HARBOR MARINA
    ## 93                                                                               MYSTIC RIVER
    ## 94                                                                               MYSTIC RIVER
    ## 95                                                                               MYSTIC RIVER
    ## 96                                                                            SCITUATE HARBOR
    ## 97                                                                            SCITUATE HARBOR
    ## 98                                                                            SCITUATE HARBOR
    ## 99                                                                            SCITUATE HARBOR
    ## 100                                                                         GLOUCESTER HARBOR
    ## 101                                                                             BOSTON HARBOR
    ## 102                                                                             BOSTON HARBOR
    ## 103                                                                             BOSTON HARBOR
    ## 104                                                                             BOSTON HARBOR
    ## 105                                                                               DUXBURY BAY
    ## 106                                                                            DORCHESTER BAY
    ## 107                                                                            DORCHESTER BAY
    ## 108                                                                            DORCHESTER BAY
    ## 109                                                                            DORCHESTER BAY
    ## 110                                                                            DORCHESTER BAY
    ## 111                                                                            DORCHESTER BAY
    ## 112                                                                            DORCHESTER BAY
    ## 113                                                                            BEVERLY HARBOR
    ## 114                                                                   PORT NORFOLK YACHT CLUB
    ## 115                                                                   PORT NORFOLK YACHT CLUB
    ## 116                                                                   PORT NORFOLK YACHT CLUB
    ## 117                                                                   PORT NORFOLK YACHT CLUB
    ## 118                                             Chelsea River, Golf Oil Fuel Off-Loading Pier
    ## 119                                             Chelsea River, Gulf Oil Fuel Off-Loading Pier
    ## 120                                                                            Dorchester Bay
    ## 121                                                                   Rowes and Fosters Wharf
    ## 122                                                                   Rowes and Fosters Wharf
    ## 123                                                                   Rowes and Fosters Wharf
    ## 124                                                                                Mill Creek
    ## 125                                                                                Mill Creek
    ## 126                                                                                Mill Creek
    ## 127                                                                                Mill Creek
    ## 128                                                                                Mill Creek
    ## 129                                                                                Mill Creek
    ## 130                                                                                Mill Creek
    ## 131                                                                                Mill Creek
    ## 132                                                                                Mill Creek
    ## 133                                                                                Mill Creek
    ## 134                                                                                Mill Creek
    ## 135                                                                                Mill Creek
    ## 136                                                                                Mill Creek
    ## 137                                                                                Mill Creek
    ## 138                                                                                Mill Creek
    ## 139                                                                                Mill Creek
    ## 140                                                                                Mill Creek
    ## 141                                                                                Mill Creek
    ## 142                                                                                Mill Creek
    ## 143                                                                                Mill Creek
    ## 144                                                                                Mill Creek
    ## 145                                                                                Mill Creek
    ## 146                                                                                Mill Creek
    ## 147                                                                                Mill Creek
    ## 148                                                                                Mill Creek
    ## 149                                                                                Mill Creek
    ## 150                                                                                Mill Creek
    ## 151                                                                                Mill Creek
    ## 152                                                                                Mill Creek
    ## 153                                                                                Mill Creek
    ## 154                                                                                Mill Creek
    ## 155                                                                                Mill Creek
    ## 156                                                                                Mill Creek
    ## 157                                                                                Mill Creek
    ## 158                                                                                Mill Creek
    ## 159                                                                                Mill Creek
    ## 160                                                                                Mill Creek
    ## 161                                                           Victory Road Park Inlet Channel
    ## 162                                                                              MYSTIC RIVER
    ## 163                                                                Seaward of Waterfront Park
    ## 164                                                                                Long Wharf
    ## 165                                                                               South River
    ## 166                                                                               South River
    ## 167                                                                           Winthrop Harbor
    ## 168                                                                           Winthrop Harbor
    ## 169                                                         Winthrop Harbor, entrance channel
    ## 170                                                                     North & Danvers River
    ## 171                                                                     North & Danvers River
    ## 172                                                                     North & Danvers River
    ## 173                                                                     North & Danvers River
    ## 174                                                                     North & Danvers River
    ## 175                                                                     North & Danvers River
    ## 176                                                                     North & Danvers River
    ## 177                                                                     North & Danvers River
    ## 178                                                                     North & Danvers River
    ## 179                                                                     North & Danvers River
    ## 180                                                                     North & Danvers River
    ## 181                                                                          Mystic River "B"
    ## 182                                                                           Chelsea River C
    ## 183                                                                           Chelsea River F
    ## 184                                                                         Reserve Channel B
    ## 185                                                                      Inner Confluence "B"
    ## 186                                                                           Mystic "A"-7830
    ## 187                                                                           Mystic "B"-7831
    ## 188                                                                          Chelsea "A"-7828
    ## 189                                                                          Chelsea "E"-7829
    ## 190                                                                          Reserve "B"-7826
    ## 191                                                                          Reserve "D"-7827
    ## 192                                                              FADS-Reference location-7832
    ## 193                                                                              Gulf Oil Co.
    ## 194                                                                              Gibb Oil ???
    ## 195                                                                      Gibb Oil North Berth
    ## 196                                                                      Gibb Oil South Berth
    ## 197                                                                      ESE of Castle Island
    ## 198                                                                      ESE of Castle Island
    ## 199                                                                      ESE of Castle Island
    ## 200                                                                      ESE of Castle Island
    ## 201                                                                      ESE of Castle Island
    ## 202                                                                      ESE of Castle Island
    ## 203                                                         btwn Deer I. & Governors I. Flats
    ## 204                                                         btwn Deer I. & Governors I. Flats
    ## 205                                                         btwn Deer I. & Governors I. Flats
    ## 206                                                         btwn Deer I. & Governors I. Flats
    ## 207                                                         btwn Deer I. & Governors I. Flats
    ## 208                                                         btwn Deer I. & Governors I. Flats
    ## 209                                                                                Quincy Bay
    ## 210                                                                                Quincy Bay
    ## 211                                                                                Quincy Bay
    ## 212                                                                                Quincy Bay
    ## 213                                                                                Quincy Bay
    ## 214                                                                                Quincy Bay
    ## 215                                                           Nantasket Roads W of Perry Cove
    ## 216                                                           Nantasket Roads W of Perry Cove
    ## 217                                                           Nantasket Roads W of Perry Cove
    ## 218                                                           Nantasket Roads W of Perry Cove
    ## 219                                                           Nantasket Roads W of Perry Cove
    ## 220                                                           Nantasket Roads W of Perry Cove
    ## 221                                                                                  Hull Bay
    ## 222                                                                                  Hull Bay
    ## 223                                                                                  Hull Bay
    ## 224                                                                                  Hull Bay
    ## 225                                                                                  Hull Bay
    ## 226                                                                                  Hull Bay
    ## 227                                                               SE of The Graves, Mass. Bay
    ## 228                                                               SE of The Graves, Mass. Bay
    ## 229                                                               SE of The Graves, Mass. Bay
    ## 230                                                               SE of The Graves, Mass. Bay
    ## 231                                                                         Massachusetts Bay
    ## 232                                                                         Massachusetts Bay
    ## 233                                                                         Massachusetts Bay
    ## 234                                                                         Massachusetts Bay
    ## 235                                                                         Massachusetts Bay
    ## 236                                                                         Massachusetts Bay
    ## 237                                                                         Massachusetts Bay
    ## 238                                                                         Massachusetts Bay
    ## 239                                                                         Massachusetts Bay
    ## 240                                                                         Massachusetts Bay
    ## 241                                                                         Massachusetts Bay
    ## 242                                                                         Massachusetts Bay
    ## 243                                                                         Massachusetts Bay
    ## 244                                                                         Massachusetts Bay
    ## 245                                                                         Massachusetts Bay
    ## 246                                                                         Massachusetts Bay
    ## 247                                                                         Massachusetts Bay
    ## 248                                                                         Massachusetts Bay
    ## 249                                                                         Massachusetts Bay
    ## 250                                                                         Massachusetts Bay
    ## 251                                                                         Massachusetts Bay
    ## 252                                                                         Massachusetts Bay
    ## 253                                                                         Massachusetts Bay
    ## 254                                                                         Massachusetts Bay
    ## 255                                                                         Massachusetts Bay
    ## 256                                                                         Massachusetts Bay
    ## 257                                                                         Massachusetts Bay
    ## 258                                                                         Massachusetts Bay
    ## 259                                                                         Massachusetts Bay
    ## 260                                                                         Massachusetts Bay
    ## 261                                                                         Massachusetts Bay
    ## 262                                                                         Massachusetts Bay
    ## 263                                                                         Massachusetts Bay
    ## 264                                                                         Massachusetts Bay
    ## 265                                                                         Massachusetts Bay
    ## 266                                                                         Massachusetts Bay
    ## 267                                                                         Massachusetts Bay
    ## 268                                                                         Massachusetts Bay
    ## 269                                                                         Massachusetts Bay
    ## 270                                                                         Massachusetts Bay
    ## 271                                                                         Massachusetts Bay
    ## 272                                                                         Massachusetts Bay
    ## 273                                                                         Massachusetts Bay
    ## 274                                                                         Massachusetts Bay
    ## 275                                                                         Massachusetts Bay
    ## 276                                                                         Massachusetts Bay
    ## 277                                                                         Massachusetts Bay
    ## 278                                                                         Massachusetts Bay
    ## 279                                                                         Massachusetts Bay
    ## 280                                                                         Massachusetts Bay
    ## 281                                                                              Cape Cod Bay
    ## 282                                                                              Cape Cod Bay
    ## 283                                                                              Cape Cod Bay
    ## 284                                                                              Cape Cod Bay
    ## 285                                                                              Cape Cod Bay
    ## 286                                                                              Cape Cod Bay
    ## 287                                                                              Cape Cod Bay
    ## 288                                                                              Cape Cod Bay
    ## 289                                                                              Cape Cod Bay
    ## 290                                                                              Cape Cod Bay
    ## 291                                                                              Cape Cod Bay
    ## 292                                                                              Cape Cod Bay
    ## 293                                                            HbrView Marina,Town Rvr Quincy
    ## 294                                                                                 Foul Area
    ## 295                                                           Marina Bay, Squantum Pt, Quincy
    ## 296                                                           Marina Bay, Squantum Pt, Quincy
    ## 297                                                           Marina Bay, Squantum Pt, Quincy
    ## 298                                                           Marina Bay, Squantum Pt, Quincy
    ## 299                                                                     Little Mystic Channel
    ## 300                                                                          ISLAND END RIVER
    ## 301                                                                          ISLAND END RIVER
    ## 302                                                                          ISLAND END RIVER
    ## 303                                                                              SALEM HARBOR
    ## 304                                                                                NUT ISLAND
    ## 305                                                                                NUT ISLAND
    ## 306                                                                                NUT ISLAND
    ## 307                                                                                NUT ISLAND
    ## 308                                                                                NUT ISLAND
    ## 309                                                                                NUT ISLAND
    ## 310                                                                                NUT ISLAND
    ## 311                                                                                NUT ISLAND
    ## 312                                                                               DEER ISLAND
    ## 313                                                                               DEER ISLAND
    ## 314                                                                               DEER ISLAND
    ## 315                                                                               DEER ISLAND
    ## 316                                                                               DEER ISLAND
    ## 317                                                                               DEER ISLAND
    ## 318                                                                               DEER ISLAND
    ## 319                                                                               DEER ISLAND
    ## 320                                                                               DEER ISLAND
    ## 321                                                                               DEER ISLAND
    ## 322                                                                               DEER ISLAND
    ## 323                                                                               DEER ISLAND
    ## 324                                                                               DEER ISLAND
    ## 325                                                                           Cohasset Harbor
    ## 326                                                                           Cohasset Harbor
    ## 327                                                                           Cohasset Harbor
    ## 328                                                                           Cohasset Harbor
    ## 329                                                                           Cohasset Harbor
    ## 330                                                                           Cohasset Harbor
    ## 331                                                                           Scituate Harbor
    ## 332                                                                           Scituate Harbor
    ## 333                                                                           Scituate Harbor
    ## 334                                                                           Scituate Harbor
    ## 335                                                                                       NAR
    ## 336                                                                                       NAR
    ## 337                                                                                       NAR
    ## 338                                                                                       NAR
    ## 339                                                                                       NAR
    ## 340                                                                                       NAR
    ## 341                                                                                       NAR
    ## 342                                                                                       NAR
    ## 343                                                                                       NAR
    ## 344                                                                                       NAR
    ## 345                                                                                       NAR
    ## 346                                                                                       NAR
    ## 347                                                                                       NAR
    ## 348                                                                                       NAR
    ## 349                                                                                       NAR
    ## 350                                                                                       NAR
    ## 351                                                                                       NAR
    ## 352                                                                                       NAR
    ## 353                                                                                       NAR
    ## 354                                                                                       NAR
    ## 355                                                                                       NAR
    ## 356                                                                                       DOB
    ## 357                                                                                       DOB
    ## 358                                                                                       DOB
    ## 359                                                                                       DOB
    ## 360                                                                                       DOB
    ## 361                                                                                       DOB
    ## 362                                                                                       DOB
    ## 363                                                                                       DOB
    ## 364                                                                                       DOB
    ## 365                                                                                       DOB
    ## 366                                                                                       DOB
    ## 367                                                                                       DOB
    ## 368                                                                                       DOB
    ## 369                                                                                       DOB
    ## 370                                                                                       BOI
    ## 371                                                                                       BOI
    ## 372                                                                                       BOI
    ## 373                                                                                       BOI
    ## 374                                                                                       BOI
    ## 375                                                                                       BOI
    ## 376                                                                                       BOI
    ## 377                                                                                       PRR
    ## 378                                                                                       PRR
    ## 379                                                                                       PRR
    ## 380                                                                                       PRR
    ## 381                                                                                       PRR
    ## 382                                                                                       PRR
    ## 383                                                                                       PRR
    ## 384                                                                                       PRR
    ## 385                                                                                       PRR
    ## 386                                                                                       PRR
    ## 387                                                                                       PRR
    ## 388                                                                                       PRR
    ## 389                                                                                       PRR
    ## 390                                                                                       PRR
    ## 391                                                                                       BIH
    ## 392                                                                                       BIH
    ## 393                                                                                       LDF
    ## 394                                                                                       LDF
    ## 395                                                                                       LDF
    ## 396                                                                                       LDF
    ## 397                                                                                       LDF
    ## 398                                                                                       LDF
    ## 399                                                                                       LDF
    ## 400                                                                                       PRR
    ## 401                                                                                       PRR
    ## 402                                                                                       PRR
    ## 403                                                                                       PRR
    ## 404                                                                                       PRR
    ## 405                                                                                       PRR
    ## 406                                                                                       PRR
    ## 407                                                                                       BRS
    ## 408                                                                                       BRS
    ## 409                                                                                       BRS
    ## 410                                                                                       BRS
    ## 411                                                                                       BRS
    ## 412                                                                                       BRS
    ## 413                                                                                       BRS
    ## 414                                                                                       MAB
    ## 415                                                                                       MAB
    ## 416                                                                                       MAB
    ## 417                                                                                       MAB
    ## 418                                                                                       MAB
    ## 419                                                                                       MAB
    ## 420                                                                                       MAB
    ## 421                                                                                       BIH
    ## 422                                                                                       BIH
    ## 423                                                                                       BIH
    ## 424                                                                                       BIH
    ## 425                                                                                       BIH
    ## 426                                                                                      <NA>
    ## 427                                                                                      <NA>
    ## 428                                                                                      <NA>
    ## 429                                                                                      <NA>
    ## 430                                                                               Broad Sound
    ## 431                                                                               Broad Sound
    ## 432                                                                                      <NA>
    ## 433                                                                                      <NA>
    ## 434                                                                                      <NA>
    ## 435                                                                                      <NA>
    ## 436                                                                                      <NA>
    ## 437                                                                                      <NA>
    ## 438                                                                                      <NA>
    ## 439                                                                                      <NA>
    ## 440                                                                                      <NA>
    ## 441                                                                                      <NA>
    ## 442                                                                                      <NA>
    ## 443                                                                                      <NA>
    ## 444                                                                                      <NA>
    ## 445                                                                                      <NA>
    ## 446                                                                                      <NA>
    ## 447                                                                                      <NA>
    ## 448                                                                                      <NA>
    ## 449                                                                                      <NA>
    ## 450                                                                                      <NA>
    ## 451                                                                                      <NA>
    ## 452                                                                                      <NA>
    ## 453                                                                                      <NA>
    ## 454                                                                                      <NA>
    ## 455                                                                                      <NA>
    ## 456                                                                                      <NA>
    ## 457                                                                                      <NA>
    ## 458                                                                                      <NA>
    ## 459                                                                                      <NA>
    ## 460                                                                                      <NA>
    ## 461                                                                                      <NA>
    ## 462                                                                                      <NA>
    ## 463                                                                                      <NA>
    ## 464                                                                                      <NA>
    ## 465                                                                                      <NA>
    ## 466                                                                                      <NA>
    ## 467                                                                                      <NA>
    ## 468                                                                                      <NA>
    ## 469                                                                                      <NA>
    ## 470                                                                                      <NA>
    ## 471                                                                                      <NA>
    ## 472                                                                                      <NA>
    ## 473                                                                                      <NA>
    ## 474                                                                                      <NA>
    ## 475                                                                                      <NA>
    ## 476                                                                                      <NA>
    ## 477                                                                                      <NA>
    ## 478                                                                                      <NA>
    ## 479                                                                                      <NA>
    ## 480                                                                                      <NA>
    ## 481                                                                                      <NA>
    ## 482                                                                                      <NA>
    ## 483                                                                                      <NA>
    ## 484                                                                                      <NA>
    ## 485                                                                                      <NA>
    ## 486                                                                                      <NA>
    ## 487                                                                                      <NA>
    ## 488                                                                                      <NA>
    ## 489                                                                                      <NA>
    ## 490                                                                                      <NA>
    ## 491                                                                                      <NA>
    ## 492                                                                                      <NA>
    ## 493                                                                                      <NA>
    ## 494                                                                                      <NA>
    ## 495                                                                                      <NA>
    ## 496                                                                                      <NA>
    ## 497                                                                                      <NA>
    ## 498                                                                                      <NA>
    ## 499                                                                                      <NA>
    ## 500                                                                                      <NA>
    ## 501                                                                                      <NA>
    ## 502                                                                                      <NA>
    ## 503                                                                                      <NA>
    ## 504                                                                                      <NA>
    ## 505                                                                                      <NA>
    ## 506                                                                                      <NA>
    ## 507                                                                                      <NA>
    ## 508                                                                                      <NA>
    ## 509                                                                                      <NA>
    ## 510                                                                                      <NA>
    ## 511                                                                                      <NA>
    ## 512                                                                                      <NA>
    ## 513                                                                                      <NA>
    ## 514                                                                                      <NA>
    ## 515                                                                                      <NA>
    ## 516                                                                                      <NA>
    ## 517                                                                                      <NA>
    ## 518                                                                                      <NA>
    ## 519                                                                                      <NA>
    ## 520                                                                                      <NA>
    ## 521                                                                                      <NA>
    ## 522                                                                                      <NA>
    ## 523                                                                                      <NA>
    ## 524                                                                                      <NA>
    ## 525                                                                                      <NA>
    ## 526                                                                                      <NA>
    ## 527                                                                                      <NA>
    ## 528                                                                                      <NA>
    ## 529                                                                                      <NA>
    ## 530                                                                                      <NA>
    ## 531                                                                                      <NA>
    ## 532                                                                                      <NA>
    ## 533                                                                                      <NA>
    ## 534                                                                                      <NA>
    ## 535                                                                                      <NA>
    ## 536                                                                                      <NA>
    ## 537                                                                                      <NA>
    ## 538                                                                                      <NA>
    ## 539                                                                                      <NA>
    ## 540                                                                                      <NA>
    ## 541                                                                                      <NA>
    ## 542                                                                                      <NA>
    ## 543                                                                                      <NA>
    ## 544                                                                                      <NA>
    ## 545                                                                                      <NA>
    ## 546                                                                                      <NA>
    ## 547                                                                               Salem Sound
    ## 548                                                                               Salem Sound
    ## 549                                                                               Salem Sound
    ## 550                                                                               Salem Sound
    ## 551                                                                               Salem Sound
    ## 552                                                                               Salem Sound
    ## 553                                                                               Salem Sound
    ## 554                                                                               Broad Sound
    ## 555                                                                               Broad Sound
    ## 556                                                                           Foul area north
    ## 557                                                                           Foul area north
    ## 558                                                                           Foul area north
    ## 559                                                                          Foul area center
    ## 560                                                                          Foul area center
    ## 561                                                                          Foul area center
    ## 562                                                                           Foul area south
    ## 563                                                                           Foul area south
    ## 564                                                                            Foul area east
    ## 565                                                                            Foul area east
    ## 566                                                                            Foul area west
    ## 567                                                                            Foul area west
    ## 568                                                                            Foul area west
    ## 569                                                                        south of Foul area
    ## 570                                                                        south of Foul area
    ## 571                                                                        south of Foul area
    ## 572                                                                                       BIH
    ## 573                                                                                       BIH
    ## 574                                                                                       BIH
    ## 575                                                                                       BIH
    ## 576                                                                                       BIH
    ## 577                                                                                       BIH
    ## 578                                                                                       BIH
    ## 579                                                                                       BIH
    ## 580                                                                              Spec. Island
    ## 581                                                                              Spec. Island
    ## 582                                                                              Spec. Island
    ## 583                                                                              Spec. Island
    ## 584                                                                              Spec. Island
    ## 585                                                                              Spec. Island
    ## 586                                                                              Spec. Island
    ## 587                                                                              Spec. Island
    ## 588                                                                              Spec. Island
    ## 589                                                                              Spec. Island
    ## 590                                                                              Spec. Island
    ## 591                                                                              Spec. Island
    ## 592                                                                              Spec. Island
    ## 593                                                                              Spec. Island
    ## 594                                                                              Spec. Island
    ## 595                                                                              Spec. Island
    ## 596                                                                              Spec. Island
    ## 597                                                                              Spec. Island
    ## 598                                                                              Spec. Island
    ## 599                                                                                       DOB
    ## 600                                                                                       DOB
    ## 601                                                                          Third Hbr Tunnel
    ## 602                                                                          Third Hbr Tunnel
    ## 603                                                                          Third Hbr Tunnel
    ## 604                                                                          Third Hbr Tunnel
    ## 605                                                                          Third Hbr Tunnel
    ## 606                                                                          Third Hbr Tunnel
    ## 607                                                                          Third Hbr Tunnel
    ## 608                                                                          Third Hbr Tunnel
    ## 609                                                                          Third Hbr Tunnel
    ## 610                                                                          Third Hbr Tunnel
    ## 611                                                                          Third Hbr Tunnel
    ## 612                                                                           MERRIMACK RIVER
    ## 613                                                                                       QUB
    ## 614                                                                                       DOB
    ## 615                                                                                       QUB
    ## 616                                                                                       DOB
    ## 617                                                                                       BOI
    ## 618                                                                                       PRR
    ## 619                                                                                       PRR
    ## 620                                                                                       BIH
    ## 621                                                                                       LDF
    ## 622                                                                                       BIH
    ## 623                                                                                       BIH
    ## 624                                                                                       BIH
    ## 625                                                                          Island End River
    ## 626                                                                        Fort Point Channel
    ## 627                                                                                       BIH
    ## 628                                                                                       BIH
    ## 629                                                                                       BIH
    ## 630                                                                                       BIH
    ## 631                                                                                       BIH
    ## 632                                                                                       BIH
    ## 633                                                                                       BIH
    ## 634                                                            1-U.S. GypsumCo.200TerminalSt.
    ## 635                                                            2-U.S. GypsumCo.200TerminalSt.
    ## 636                                                            3-U.S. GypsumCo.200TerminalSt.
    ## 637                                                            4-U.S. GypsumCo.200TerminalSt.
    ## 638                                                                                      FADS
    ## 639                                                                                      FADS
    ## 640                                                                                      FADS
    ## 641                                                                                      FADS
    ## 642                                                                                      FADS
    ## 643                                                                                      FADS
    ## 644                                                                                      FADS
    ## 645                                                                                      FADS
    ## 646                                                                                      FADS
    ## 647                                                                                      FADS
    ## 648                                                                                      FADS
    ## 649                                                                                      FADS
    ## 650                                                                                      FADS
    ## 651                                                                                      FADS
    ## 652                                                                                       BIH
    ## 653                                                                                       BIH
    ## 654                                                                                       BIH
    ## 655                                                                         JEFFRIES POINT YC
    ## 656                                                                         JEFFRIES POINT YC
    ## 657                                                                         JEFFRIES POINT YC
    ## 658                                                                         JEFFRIES POINT YC
    ## 659                                                                                      MBDS
    ## 660                                                                                      MBDS
    ## 661                                                                                      MBDS
    ## 662                                                                                      MBDS
    ## 663                                                                                      MBDS
    ## 664                                                                                      MBDS
    ## 665                                                                                      MBDS
    ## 666                                                                                      MBDS
    ## 667                                                                                      MBDS
    ## 668                                                                                       BIH
    ## 669                                                                      MBDS Reference sites
    ## 670                                                                      MBDS Reference sites
    ## 671                                                                       STFP outfall siting
    ## 672                                                                       STFP outfall siting
    ## 673                                                                       STFP outfall siting
    ## 674                                                                       STFP outfall siting
    ## 675                                                                       STFP outfall siting
    ## 676                                                                       STFP outfall siting
    ## 677                                                                       STFP outfall siting
    ## 678                                                                       STFP outfall siting
    ## 679                                                                       STFP outfall siting
    ## 680                                                                       STFP outfall siting
    ## 681                                                                       STFP outfall siting
    ## 682                                                                       STFP outfall siting
    ## 683                                                                       STFP outfall siting
    ## 684                                                                       STFP outfall siting
    ## 685                                                                       STFP outfall siting
    ## 686                                                                       STFP outfall siting
    ## 687                                                                       STFP outfall siting
    ## 688                                                                       STFP outfall siting
    ## 689                                                                       STFP outfall siting
    ## 690                                                                       STFP outfall siting
    ## 691                                                                       STFP outfall siting
    ## 692                                                                       STFP outfall siting
    ## 693                                                                       STFP outfall siting
    ## 694                                                                       STFP outfall siting
    ## 695                                                                       STFP outfall siting
    ## 696                                                                       STFP outfall siting
    ## 697                                                                       STFP outfall siting
    ## 698                                                                       STFP outfall siting
    ## 699                                                                       STFP outfall siting
    ## 700                                                                       STFP outfall siting
    ## 701                                                                       STFP outfall siting
    ## 702                                                                       STFP outfall siting
    ## 703                                                                       STFP outfall siting
    ## 704                                                                       STFP outfall siting
    ## 705                                                                       STFP outfall siting
    ## 706                                                                       STFP outfall siting
    ## 707                                                                       STFP outfall siting
    ## 708                                                                       STFP outfall siting
    ## 709                                                                       STFP outfall siting
    ## 710                                                                       STFP outfall siting
    ## 711                                                                       STFP outfall siting
    ## 712                                                                       STFP outfall siting
    ## 713                                                                       STFP outfall siting
    ## 714                                                                       STFP outfall siting
    ## 715                                                                       STFP outfall siting
    ## 716                                                                       STFP outfall siting
    ## 717                                                                       STFP outfall siting
    ## 718                                                                       STFP outfall siting
    ## 719                                                                       STFP outfall siting
    ## 720                                                                       STFP outfall siting
    ## 721                                                                       STFP outfall siting
    ## 722                                                                       STFP outfall siting
    ## 723                                                                       STFP outfall siting
    ## 724                                                                       STFP outfall siting
    ## 725                                                                       STFP outfall siting
    ## 726                                                                       STFP outfall siting
    ## 727                                                                       STFP outfall siting
    ## 728                                                                       STFP outfall siting
    ## 729                                                                       STFP outfall siting
    ## 730                                                                       STFP outfall siting
    ## 731                                                                       STFP outfall siting
    ## 732                                                                       STFP outfall siting
    ## 733                                                                       STFP outfall siting
    ## 734                                                                       STFP outfall siting
    ## 735                                                                       STFP outfall siting
    ## 736                                                                       STFP outfall siting
    ## 737                                                                       STFP outfall siting
    ## 738                                                                       STFP outfall siting
    ## 739                                                                       STFP outfall siting
    ## 740                                                                       STFP outfall siting
    ## 741                                                                       STFP outfall siting
    ## 742                                                                       STFP outfall siting
    ## 743                                                                       STFP outfall siting
    ## 744                                                                       STFP outfall siting
    ## 745                                                                       STFP outfall siting
    ## 746                                                                       STFP outfall siting
    ## 747                                                                       STFP outfall siting
    ## 748                                                                       STFP outfall siting
    ## 749                                                                       STFP outfall siting
    ## 750                                                                       STFP outfall siting
    ## 751                                                                       STFP outfall siting
    ## 752                                                                       STFP outfall siting
    ## 753                                                                       STFP outfall siting
    ## 754                                                                       STFP outfall siting
    ## 755                                                                       STFP outfall siting
    ## 756                                                                       STFP outfall siting
    ## 757                                                                       STFP outfall siting
    ## 758                                                                       STFP outfall siting
    ## 759                                                                       STFP outfall siting
    ## 760                                                                       STFP outfall siting
    ## 761                                                                       STFP outfall siting
    ## 762                                                                       STFP outfall siting
    ## 763                                                                       STFP outfall siting
    ## 764                                                                       STFP outfall siting
    ## 765                                                                       STFP outfall siting
    ## 766                                                                       STFP outfall siting
    ## 767                                                                       STFP outfall siting
    ## 768                                                                       STFP outfall siting
    ## 769                                                                       STFP outfall siting
    ## 770                                                                       STFP outfall siting
    ## 771                                                                       STFP outfall siting
    ## 772                                                                       STFP outfall siting
    ## 773                                                                       STFP outfall siting
    ## 774                                                                       STFP outfall siting
    ## 775                                                                       STFP outfall siting
    ## 776                                                                       STFP outfall siting
    ## 777                                                                       STFP outfall siting
    ## 778                                                                       STFP outfall siting
    ## 779                                                                       STFP outfall siting
    ## 780                                                                       STFP outfall siting
    ## 781                                                                       STFP outfall siting
    ## 782                                                                       STFP outfall siting
    ## 783                                                                       STFP outfall siting
    ## 784                                                                       STFP outfall siting
    ## 785                                                                       STFP outfall siting
    ## 786                                                                       STFP outfall siting
    ## 787                                                                       STFP outfall siting
    ## 788                                                                       STFP outfall siting
    ## 789                                                                       STFP outfall siting
    ## 790                                                                       STFP outfall siting
    ## 791                                                                       STFP outfall siting
    ## 792                                                                       STFP outfall siting
    ## 793                                                                       STFP outfall siting
    ## 794                                                                       STFP outfall siting
    ## 795                                                                       STFP outfall siting
    ## 796                                                                       STFP outfall siting
    ## 797                                                                       STFP outfall siting
    ## 798                                                                       STFP outfall siting
    ## 799                                                                       STFP outfall siting
    ## 800                                                                       STFP outfall siting
    ## 801                                                                         Massachusetts Bay
    ## 802                                                                                  Hull Bay
    ## 803                                                                               Hingham Bay
    ## 804                                                                               Hingham Bay
    ## 805                                                                       Weymouth Fore River
    ## 806                                                                                Quincy Bay
    ## 807                                                                                Quincy Bay
    ## 808                                                                           Nantasket Roads
    ## 809                                                                           Nantasket Roads
    ## 810                                                                           Nantasket Roads
    ## 811                                                                            Dorchester Bay
    ## 812                                                                            Dorchester Bay
    ## 813                                                                             Sculpin Ledge
    ## 814                                                                             Sculpin Ledge
    ## 815                                                                          Northwest Harbor
    ## 816                                                                       Upper Chelsea River
    ## 817                                                                       Upper Chelsea River
    ## 818                                                                       Lower Chelsea River
    ## 819                                                                       Lower Chelsea River
    ## 820                                                                       Lower Chelsea River
    ## 821                                                                              Mystic River
    ## 822                                                                              Mystic River
    ## 823                                                                              Mystic River
    ## 824                                                                        Charleston Channel
    ## 825                                                                        Charleston Channel
    ## 826                                                                        Charleston Channel
    ## 827                                                                            Boston Channel
    ## 828                                                                             Channel Mouth
    ## 829                                                                          Reserved Channel
    ## 830                                                                            Boston Wharves
    ## 831                                                                                      FADS
    ## 832                                                                                      FADS
    ## 833                                                                                      FADS
    ## 834                                                                                      FADS
    ## 835                                                                                      FADS
    ## 836                                                                                      FADS
    ## 837                                                                                      FADS
    ## 838                                                                                      FADS
    ## 839                                                                                      FADS
    ## 840                                                                                      FADS
    ## 841                                                  South Bay area of the Fort Point Channel
    ## 842                                                  South Bay area of the Fort Point Channel
    ## 843                                                  South Bay area of the Fort Point Channel
    ## 844                                                  between Spectacle Island and Long Island
    ## 845                                                  between Spectacle Island and Long Island
    ## 846                                                  between Spectacle Island and Long Island
    ## 847                                                  between Spectacle Island and Long Island
    ## 848                                                  between Spectacle Island and Long Island
    ## 849                                                  between Spectacle Island and Long Island
    ## 850                                                  between Spectacle Island and Long Island
    ## 851                                                  between Spectacle Island and Long Island
    ## 852                                                  between Spectacle Island and Long Island
    ## 853                                                  between Spectacle Island and Long Island
    ## 854                                                  between Spectacle Island and Long Island
    ## 855                                                  between Spectacle Island and Long Island
    ## 856                                                  between Spectacle Island and Long Island
    ## 857                                                  between Spectacle Island and Long Island
    ## 858                                                  between Spectacle Island and Long Island
    ## 859                                                                                Town Brook
    ## 860                                                                                Town Brook
    ## 861                                                                                Town Brook
    ## 862                                                                                Town Brook
    ## 863                                                                                Town Brook
    ## 864                                                                                Town Brook
    ## 865                                                                                Town Brook
    ## 866                                                                                Town Brook
    ## 867                                                                                Town Brook
    ## 868                                                                                Town Brook
    ## 869                                                                                Town Brook
    ## 870                                                                                Town Brook
    ## 871                                                                                Town Brook
    ## 872                                                                                Town Brook
    ## 873                                                                                Town Brook
    ## 874                                                                                Town Brook
    ## 875                                                                                Town Brook
    ## 876                                                                                Town Brook
    ## 877                                                                                Town Brook
    ## 878                                                                            Beverly Harbor
    ## 879                                                                            Beverly Harbor
    ## 880                                                                            Beverly Harbor
    ## 881                                                         WINTHROP HARBOR, BELLE ISLE INLET
    ## 882                                                         WINTHROP HARBOR, BELLE ISLE INLET
    ## 883                                                                    LOGAN AIRPORT E.BOSTON
    ## 884                                                                    LOGAN AIRPORT E.BOSTON
    ## 885                                                                    LOGAN AIRPORT E.BOSTON
    ## 886                                                                    LOGAN AIRPORT E.BOSTON
    ## 887                                                                    LOGAN AIRPORT E.BOSTON
    ## 888                                                                    LOGAN AIRPORT E.BOSTON
    ## 889                                                              Logan Airport Runway End 22L
    ## 890                                                              Logan Airport Runway End 22L
    ## 891                                                              Logan Airport Runway End 22L
    ## 892                                                              Logan Airport Runway End 22L
    ## 893                                                              Logan Airport Runway End 22L
    ## 894                                                              Logan Airport Runway End 22L
    ## 895                                                               Logan Airport Runway End 27
    ## 896                                                               Logan Airport Runway End 27
    ## 897                                                               Logan Airport Runway End 27
    ## 898                                                               Logan Airport Runway End 27
    ## 899                                                               Logan Airport Runway End 27
    ## 900                                                              Logan Airport Runway End 33L
    ## 901                                                              Logan Airport Runway End 33L
    ## 902                                                              Logan Airport Runway End 33L
    ## 903                                                              Logan Airport Runway End 33L
    ## 904                                                              Logan Airport Runway End 33L
    ## 905                                                              Logan Airport Runway End 33L
    ## 906                                                              Logan Airport Runway End 33L
    ## 907                                                              Logan Airport Runway End 33L
    ## 908                                                              Logan Airport Runway End 33L
    ## 909                                                                           Winthrop Harbor
    ## 910                                                                           Winthrop Harbor
    ## 911                                                                           Winthrop Harbor
    ## 912                                                                  Winthrop Basin Anchorage
    ## 913                                                                  Winthrop Basin Anchorage
    ## 914                                                                Wnthrop Basin Spur Channel
    ## 915                                                                  Winthrop Basin Anchorage
    ## 916                                                    Entrance Channel Opposite Snake Island
    ## 917                                                        Entrance Channel at Basin Entrance
    ## 918                                                                    Cottage Park Anchorage
    ## 919                                                                      Cottage Park Channel
    ## 920                                                                    Cottage Park Anchorage
    ## 921                                                                    Snake Island Anchorage
    ## 922                                                                      Cottage Park Channel
    ## 923                                                                    Snake Island Anchorage
    ## 924                                                                      Cottage Park Channel
    ## 925                                                                    Crystal Cove Anchorage
    ## 926                                                                    Crystal Cove Anchorage
    ## 927                                                        Entrance Channel off Winthrop Y.C.
    ## 928                                                          Entrance Channel at Crystal Cove
    ##     AREA_CODE SAMP_DATE1           TO_SMP_DT2 DPTH_N_COR
    ## 1           1   5/1/1981                 <NA>       <NA>
    ## 2           1   5/1/1981                 <NA>       <NA>
    ## 3           1   5/1/1981                 <NA>       <NA>
    ## 4           1   5/1/1981                 <NA>       <NA>
    ## 5           1   5/1/1981                 <NA>       <NA>
    ## 6           1       <NA>                 <NA>       <NA>
    ## 7           1       <NA>                 <NA>       <NA>
    ## 8           5       <NA>                 <NA>       <NA>
    ## 9           5       <NA>                 <NA>       <NA>
    ## 10          5       <NA>                 <NA>       <NA>
    ## 11          5       <NA>                 <NA>       <NA>
    ## 12          5       <NA>                 <NA>       <NA>
    ## 13          5       <NA>                 <NA>       <NA>
    ## 14          5       <NA>                 <NA>       <NA>
    ## 15          5       <NA>                 <NA>       <NA>
    ## 16          5       <NA>                 <NA>       <NA>
    ## 17          5       <NA>                 <NA>       <NA>
    ## 18          5       <NA>                 <NA>       <NA>
    ## 19          5       <NA>                 <NA>       <NA>
    ## 20          1       <NA>                 <NA>       <NA>
    ## 21          1       <NA>                 <NA>       <NA>
    ## 22          1       <NA>                 <NA>       <NA>
    ## 23          1       <NA>                 <NA>       <NA>
    ## 24          1       <NA>                 <NA>       <NA>
    ## 25          2       <NA>                 <NA>       <NA>
    ## 26          2       <NA>                 <NA>       <NA>
    ## 27          2       <NA>                 <NA>       <NA>
    ## 28          2       <NA>                 <NA>       <NA>
    ## 29          2       <NA>                 <NA>       <NA>
    ## 30          1       <NA>                 <NA>       <NA>
    ## 31          1       <NA>                 <NA>       <NA>
    ## 32          2 10/26/1983                 <NA>       <NA>
    ## 33          2  4/17/1985                 <NA>       <NA>
    ## 34          2  4/17/1985                 <NA>       <NA>
    ## 35          5 10/21/1986                 <NA>       <NA>
    ## 36          5 10/21/1986                 <NA>       <NA>
    ## 37          5 10/21/1986                 <NA>       <NA>
    ## 38          4  4/19/1985                 <NA>       <NA>
    ## 39          4  4/19/1985                 <NA>       <NA>
    ## 40          4  4/19/1985                 <NA>       <NA>
    ## 41          1  3/10/1986                 <NA>       <NA>
    ## 42          1  3/10/1986                 <NA>       <NA>
    ## 43          1 10/10/1985                 <NA>       <NA>
    ## 44          1 10/10/1985                 <NA>       <NA>
    ## 45          5   1/1/1985                 <NA>       <NA>
    ## 46          5   1/1/1985                 <NA>       <NA>
    ## 47          5   1/1/1985                 <NA>       <NA>
    ## 48          5   1/1/1985                 <NA>       <NA>
    ## 49          5   1/1/1985                 <NA>       <NA>
    ## 50          5   1/1/1985                 <NA>       <NA>
    ## 51          5   1/1/1985                 <NA>       <NA>
    ## 52          5   1/1/1985                 <NA>       <NA>
    ## 53          5   1/1/1985                 <NA>       <NA>
    ## 54          5   1/1/1985                 <NA>       <NA>
    ## 55          2   7/0/1985                 <NA>       <NA>
    ## 56          2   7/0/1985                 <NA>       <NA>
    ## 57          2   7/0/1985                 <NA>       <NA>
    ## 58          2   7/0/1985                 <NA>       <NA>
    ## 59          2   7/0/1985                 <NA>       <NA>
    ## 60          2   7/0/1985                 <NA>       <NA>
    ## 61          2   7/0/1985                 <NA>       <NA>
    ## 62          2   7/0/1985                 <NA>       <NA>
    ## 63          2   7/0/1985                 <NA>       <NA>
    ## 64          2   7/0/1985                 <NA>       <NA>
    ## 65          2   7/0/1985                 <NA>       <NA>
    ## 66          2   7/0/1985                 <NA>       <NA>
    ## 67          2   7/0/1985                 <NA>       <NA>
    ## 68          2   7/0/1985                 <NA>       <NA>
    ## 69          2   7/0/1985                 <NA>       <NA>
    ## 70          2   7/0/1985                 <NA>       <NA>
    ## 71          2   7/0/1985                 <NA>       <NA>
    ## 72          2   7/0/1985                 <NA>       <NA>
    ## 73          2   7/0/1985                 <NA>       <NA>
    ## 74          2   7/0/1985                 <NA>       <NA>
    ## 75          2   7/0/1985                 <NA>       <NA>
    ## 76          2   7/0/1985                 <NA>       <NA>
    ## 77          2   7/0/1985                 <NA>       <NA>
    ## 78          2   7/0/1985                 <NA>       <NA>
    ## 79          2   7/0/1985                 <NA>       <NA>
    ## 80          1   6/1/1987                 <NA>       <NA>
    ## 81          5  4/16/1986                 <NA>       <NA>
    ## 82          5  4/16/1986                 <NA>       <NA>
    ## 83          5   4/6/1986                 <NA>       <NA>
    ## 84          5   4/6/1986                 <NA>       <NA>
    ## 85          5  6/10/1986                 <NA>       <NA>
    ## 86          5   6/3/1986                 <NA>       <NA>
    ## 87          5   6/3/1987                 <NA>       <NA>
    ## 88          5   6/3/1986                 <NA>       <NA>
    ## 89          5   6/3/1986                 <NA>       <NA>
    ## 90          4  8/29/1986                 <NA>       <NA>
    ## 91          5   3/1/1985                 <NA>       <NA>
    ## 92          2   8/9/1985                 <NA>       <NA>
    ## 93          1   3/1/1987                 <NA>       <NA>
    ## 94          1  1/15/1987                 <NA>       <NA>
    ## 95          1  1/15/1987                 <NA>       <NA>
    ## 96          5       <NA>                 <NA>       <NA>
    ## 97          5       <NA>                 <NA>       <NA>
    ## 98          5       <NA>                 <NA>       <NA>
    ## 99          5  3/17/1985                 <NA>       <NA>
    ## 100         5 11/28/1986                 <NA>       <NA>
    ## 101         2  8/22/1985                 <NA>       <NA>
    ## 102         2  8/25/1985                 <NA>       <NA>
    ## 103         2  8/25/1985                 <NA>       <NA>
    ## 104         2  8/25/1985                 <NA>       <NA>
    ## 105         7   1/7/1987                 <NA>       <NA>
    ## 106         2   1/1/1986                 <NA>       <NA>
    ## 107         2   1/1/1986                 <NA>       <NA>
    ## 108         2   1/1/1986                 <NA>       <NA>
    ## 109         2   1/1/1986                 <NA>       <NA>
    ## 110         2   1/1/1986                 <NA>       <NA>
    ## 111         2   1/1/1986                 <NA>       <NA>
    ## 112         2   1/1/1986                 <NA>       <NA>
    ## 113         5  1/26/1988                 <NA>       <NA>
    ## 114         2   7/7/1986                 <NA>       <NA>
    ## 115         2   7/7/1986                 <NA>       <NA>
    ## 116         2   7/7/1986                 <NA>       <NA>
    ## 117         2   7/7/1986                 <NA>       <NA>
    ## 118         1 10/13/1987                 <NA>       <NA>
    ## 119         1 10/13/1987                 <NA>       <NA>
    ## 120         2  9/19/1987 1987-09-22T00:00:00Z       <NA>
    ## 121         1   3/2/1984                 <NA>       <NA>
    ## 122         1  3/12/1984                 <NA>       <NA>
    ## 123         1  3/12/1984                 <NA>       <NA>
    ## 124        10       <NA>                 <NA>       <NA>
    ## 125        10       <NA>                 <NA>       <NA>
    ## 126        10       <NA>                 <NA>       <NA>
    ## 127        10       <NA>                 <NA>       <NA>
    ## 128        10       <NA>                 <NA>       <NA>
    ## 129        10       <NA>                 <NA>       <NA>
    ## 130        10       <NA>                 <NA>       <NA>
    ## 131        10       <NA>                 <NA>       <NA>
    ## 132        10       <NA>                 <NA>       <NA>
    ## 133        10       <NA>                 <NA>       <NA>
    ## 134        10       <NA>                 <NA>       <NA>
    ## 135        10       <NA>                 <NA>       <NA>
    ## 136        10       <NA>                 <NA>       <NA>
    ## 137        10       <NA>                 <NA>       <NA>
    ## 138        10       <NA>                 <NA>       <NA>
    ## 139        10       <NA>                 <NA>       <NA>
    ## 140        10       <NA>                 <NA>       <NA>
    ## 141        10       <NA>                 <NA>       <NA>
    ## 142        10       <NA>                 <NA>       <NA>
    ## 143        10       <NA>                 <NA>       <NA>
    ## 144        10       <NA>                 <NA>       <NA>
    ## 145        10       <NA>                 <NA>       <NA>
    ## 146        10       <NA>                 <NA>       <NA>
    ## 147        10       <NA>                 <NA>       <NA>
    ## 148        10       <NA>                 <NA>       <NA>
    ## 149        10       <NA>                 <NA>       <NA>
    ## 150        10       <NA>                 <NA>       <NA>
    ## 151        10       <NA>                 <NA>       <NA>
    ## 152        10       <NA>                 <NA>       <NA>
    ## 153        10       <NA>                 <NA>       <NA>
    ## 154        10       <NA>                 <NA>       <NA>
    ## 155        10       <NA>                 <NA>       <NA>
    ## 156        10       <NA>                 <NA>       <NA>
    ## 157        10       <NA>                 <NA>       <NA>
    ## 158        10       <NA>                 <NA>       <NA>
    ## 159        10       <NA>                 <NA>       <NA>
    ## 160        10       <NA>                 <NA>       <NA>
    ## 161         2       <NA>                 <NA>       <NA>
    ## 162         1  7/29/1987                 <NA>       <NA>
    ## 163         1       <NA>                 <NA>       <NA>
    ## 164         1       <NA>                 <NA>       <NA>
    ## 165        10  8/21/1989                 <NA>       <NA>
    ## 166        10  8/21/1989                 <NA>       <NA>
    ## 167         2       <NA>                 <NA>       <NA>
    ## 168         2       <NA>                 <NA>       <NA>
    ## 169         2       <NA>                 <NA>    Surface
    ## 170        10   8/1/1988                 <NA>       <NA>
    ## 171        10   8/1/1988                 <NA>       <NA>
    ## 172        10   8/1/1988                 <NA>       <NA>
    ## 173        10   8/1/1988                 <NA>       <NA>
    ## 174        10   8/1/1988                 <NA>       <NA>
    ## 175        10   8/1/1988                 <NA>       <NA>
    ## 176        10   8/1/1988                 <NA>       <NA>
    ## 177        10   8/1/1988                 <NA>       <NA>
    ## 178        10   8/1/1988                 <NA>       <NA>
    ## 179        10   8/1/1988                 <NA>       <NA>
    ## 180        10   8/1/1988                 <NA>       <NA>
    ## 181         1       <NA>                 <NA>       <NA>
    ## 182         1       <NA>                 <NA>    surface
    ## 183         1       <NA>                 <NA>    surface
    ## 184         1       <NA>                 <NA>       <NA>
    ## 185         1       <NA>                 <NA>    surface
    ## 186         1       <NA>                 <NA>       <NA>
    ## 187         1       <NA>                 <NA>       <NA>
    ## 188         1       <NA>                 <NA>       <NA>
    ## 189         1       <NA>                 <NA>       <NA>
    ## 190         1       <NA>                 <NA>       <NA>
    ## 191         1       <NA>                 <NA>       <NA>
    ## 192         1       <NA>                 <NA>       <NA>
    ## 193         1       <NA>                 <NA>       <NA>
    ## 194         1       <NA>                 <NA>       <NA>
    ## 195         1       <NA>                 <NA>       <NA>
    ## 196         1       <NA>                 <NA>       <NA>
    ## 197         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 198         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 199         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 200         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 201         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 202         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 203         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 204         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 205         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 206         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 207         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 208         2  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 209         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 210         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 211         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 212         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 213         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 214         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 215         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 216         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 217         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 218         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 219         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 220         3  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 221         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 222         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 223         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 224         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 225         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 226         4  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 227         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 228         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 229         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 230         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 231         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 232         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 233         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 234         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 235         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 236         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 237         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 238         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 239         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 240         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 241         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 242         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 243         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 244         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 245         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 246         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 247         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 248         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 249         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 250         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 251         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 252         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 253         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 254         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 255         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 256         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 257         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 258         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 259         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 260         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 261         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 262         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 263         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 264         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 265         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 266         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 267         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 268         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 269         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 270         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 271         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 272         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 273         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 274         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 275         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 276         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 277         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 278         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 279         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 280         5  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 281         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 282         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 283         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 284         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 285         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 286         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 287         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 288         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 289         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 290         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 291         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 292         7  6/27/1983 1983-07-06T00:00:00Z       <NA>
    ## 293         3 11/10/1988                 <NA>       <NA>
    ## 294         5 11/10/1988                 <NA>       <NA>
    ## 295         2  8/24/1987                 <NA>       <NA>
    ## 296         2  8/24/1987                 <NA>       <NA>
    ## 297         2  8/24/1987                 <NA>       <NA>
    ## 298         2  8/24/1987                 <NA>       <NA>
    ## 299         1   5/6/1987                 <NA>       <NA>
    ## 300         1 10/14/1987                 <NA>       <NA>
    ## 301         1 10/14/1987                 <NA>       <NA>
    ## 302         1 10/14/1987                 <NA>       <NA>
    ## 303         5  2/15/1987                 <NA>       <NA>
    ## 304         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 305         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 306         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 307         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 308         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 309         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 310         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 311         4  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 312         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 313         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 314         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 315         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 316         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 317         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 318         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 319         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 320         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 321         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 322         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 323         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 324         2  3/25/1986 1986-03-28T00:00:00Z       <NA>
    ## 325         5 10/31/1986                 <NA>       <NA>
    ## 326         5 10/31/1986                 <NA>       <NA>
    ## 327         5 10/31/1986                 <NA>       <NA>
    ## 328         5 10/31/1986                 <NA>       <NA>
    ## 329         5 10/31/1986                 <NA>       <NA>
    ## 330         5 10/31/1986                 <NA>       <NA>
    ## 331         5  11/5/1986                 <NA>       <NA>
    ## 332         5  11/5/1986                 <NA>       <NA>
    ## 333         5  11/5/1986                 <NA>       <NA>
    ## 334         5  11/5/1986                 <NA>       <NA>
    ## 335         3  6/16/1982                 <NA>       <NA>
    ## 336         3  6/16/1982                 <NA>       <NA>
    ## 337         3  6/16/1982                 <NA>       <NA>
    ## 338         3  6/16/1982                 <NA>       <NA>
    ## 339         3  6/16/1982                 <NA>       <NA>
    ## 340         3  6/16/1982                 <NA>       <NA>
    ## 341         3  6/16/1982                 <NA>       <NA>
    ## 342         3  6/16/1982                 <NA>       <NA>
    ## 343         3  6/16/1982                 <NA>       <NA>
    ## 344         3  6/16/1982                 <NA>       <NA>
    ## 345         3  6/16/1982                 <NA>       <NA>
    ## 346         3  6/16/1982                 <NA>       <NA>
    ## 347         3  6/16/1982                 <NA>       <NA>
    ## 348         3  6/16/1982                 <NA>       <NA>
    ## 349         3  6/17/1982                 <NA>       <NA>
    ## 350         3  6/17/1982                 <NA>       <NA>
    ## 351         3  6/17/1982                 <NA>       <NA>
    ## 352         3  6/17/1982                 <NA>       <NA>
    ## 353         3  6/17/1982                 <NA>       <NA>
    ## 354         3  6/17/1982                 <NA>       <NA>
    ## 355         3  6/17/1982                 <NA>       <NA>
    ## 356         2  6/18/1982                 <NA>       <NA>
    ## 357         2  6/18/1982                 <NA>       <NA>
    ## 358         2  6/18/1982                 <NA>       <NA>
    ## 359         2  6/18/1982                 <NA>       <NA>
    ## 360         2  6/18/1982                 <NA>       <NA>
    ## 361         2  6/18/1982                 <NA>       <NA>
    ## 362         2  6/18/1982                 <NA>       <NA>
    ## 363         2  6/18/1982                 <NA>       <NA>
    ## 364         2  6/18/1982                 <NA>       <NA>
    ## 365         2  6/18/1982                 <NA>       <NA>
    ## 366         2  6/18/1982                 <NA>       <NA>
    ## 367         2  6/18/1982                 <NA>       <NA>
    ## 368         2  6/18/1982                 <NA>       <NA>
    ## 369         2  6/18/1982                 <NA>       <NA>
    ## 370         6  6/17/1982                 <NA>       <NA>
    ## 371         6  6/17/1982                 <NA>       <NA>
    ## 372         6  6/17/1982                 <NA>       <NA>
    ## 373         6  6/17/1982                 <NA>       <NA>
    ## 374         6  6/17/1982                 <NA>       <NA>
    ## 375         6  6/17/1982                 <NA>       <NA>
    ## 376         6  6/17/1982                 <NA>       <NA>
    ## 377         2  6/23/1982                 <NA>       <NA>
    ## 378         2  6/23/1982                 <NA>       <NA>
    ## 379         2  6/23/1983                 <NA>       <NA>
    ## 380         2  6/23/1983                 <NA>       <NA>
    ## 381         2  6/23/1983                 <NA>       <NA>
    ## 382         2  6/23/1983                 <NA>       <NA>
    ## 383         2  6/23/1983                 <NA>       <NA>
    ## 384         2  6/24/1982                 <NA>       <NA>
    ## 385         2  6/24/1982                 <NA>       <NA>
    ## 386         2  6/24/1982                 <NA>       <NA>
    ## 387         2  6/24/1982                 <NA>       <NA>
    ## 388         2  6/24/1982                 <NA>       <NA>
    ## 389         2  6/24/1982                 <NA>       <NA>
    ## 390         2  6/24/1982                 <NA>       <NA>
    ## 391         1  6/18/1982                 <NA>       <NA>
    ## 392         1  6/18/1982                 <NA>       <NA>
    ## 393         2  6/23/1982                 <NA>       <NA>
    ## 394         2  6/23/1982                 <NA>       <NA>
    ## 395         2  6/23/1982                 <NA>       <NA>
    ## 396         2  6/23/1982                 <NA>       <NA>
    ## 397         2  6/23/1982                 <NA>       <NA>
    ## 398         2  6/23/1982                 <NA>       <NA>
    ## 399         2  6/23/1982                 <NA>       <NA>
    ## 400         6  6/21/1982                 <NA>       <NA>
    ## 401         6  6/21/1982                 <NA>       <NA>
    ## 402         6  6/21/1982                 <NA>       <NA>
    ## 403         6  6/21/1982                 <NA>       <NA>
    ## 404         6  6/21/1982                 <NA>       <NA>
    ## 405         6  6/21/1982                 <NA>       <NA>
    ## 406         6  6/21/1982                 <NA>       <NA>
    ## 407         6  6/22/1982                 <NA>       <NA>
    ## 408         6  6/22/1982                 <NA>       <NA>
    ## 409         6  6/22/1982                 <NA>       <NA>
    ## 410         6  6/22/1982                 <NA>       <NA>
    ## 411         6  6/22/1982                 <NA>       <NA>
    ## 412         6  6/22/1982                 <NA>       <NA>
    ## 413         6  6/22/1982                 <NA>       <NA>
    ## 414         5  6/22/1982                 <NA>       <NA>
    ## 415         5  6/22/1982                 <NA>       <NA>
    ## 416         5  6/22/1982                 <NA>       <NA>
    ## 417         5  6/22/1982                 <NA>       <NA>
    ## 418         5  6/22/1982                 <NA>       <NA>
    ## 419         5  6/22/1982                 <NA>       <NA>
    ## 420         5  6/22/1982                 <NA>       <NA>
    ## 421         1  6/22/1982                 <NA>       <NA>
    ## 422         1  6/22/1982                 <NA>       <NA>
    ## 423         1  6/22/1982                 <NA>       <NA>
    ## 424         1  6/22/1982                 <NA>       <NA>
    ## 425         1  6/22/1982                 <NA>       <NA>
    ## 426         5  7/30/1975                 <NA>          0
    ## 427         5  7/30/1975                 <NA>          0
    ## 428         5  7/30/1975                 <NA>         30
    ## 429         5  7/30/1975                 <NA>         30
    ## 430         5   7/9/1976                 <NA>          0
    ## 431         5   7/9/1976                 <NA>          0
    ## 432         5  7/23/1975                 <NA>          0
    ## 433         5  7/23/1975                 <NA>          0
    ## 434         5  7/23/1975                 <NA>          0
    ## 435         5  7/23/1975                 <NA>          0
    ## 436         5  7/23/1975                 <NA>         30
    ## 437         5  7/16/1975                 <NA>          0
    ## 438         5  7/16/1975                 <NA>          0
    ## 439         5  7/23/1975                 <NA>          0
    ## 440         5  7/23/1975                 <NA>         30
    ## 441         5  7/23/1975                 <NA>          0
    ## 442         5  7/23/1975                 <NA>          0
    ## 443         5  7/23/1975                 <NA>         30
    ## 444         5  6/23/1976                 <NA>          0
    ## 445         5  6/23/1976                 <NA>         30
    ## 446         5  6/23/1976                 <NA>          0
    ## 447         5  7/30/1975                 <NA>          0
    ## 448         5  7/30/1975                 <NA>         30
    ## 449         5  7/30/1975                 <NA>         60
    ## 450         5  7/30/1975                 <NA>          0
    ## 451         5  7/30/1975                 <NA>         30
    ## 452         5  7/30/1975                 <NA>         60
    ## 453         5  7/30/1975                 <NA>          0
    ## 454         5  7/30/1975                 <NA>         30
    ## 455         5  7/30/1975                 <NA>         60
    ## 456         5  7/30/1975                 <NA>          0
    ## 457         5  7/30/1975                 <NA>          0
    ## 458         5  7/30/1975                 <NA>         30
    ## 459         5  7/30/1975                 <NA>         60
    ## 460         5  7/30/1975                 <NA>          0
    ## 461         5  7/30/1975                 <NA>         30
    ## 462         5  7/30/1975                 <NA>         60
    ## 463         5  7/30/1975                 <NA>          0
    ## 464         5  7/30/1975                 <NA>         30
    ## 465         5  7/30/1975                 <NA>          0
    ## 466         5  7/30/1975                 <NA>         30
    ## 467         5  7/30/1975                 <NA>         60
    ## 468         5  7/30/1975                 <NA>          0
    ## 469         5  7/30/1975                 <NA>         30
    ## 470         5  7/30/1975                 <NA>          0
    ## 471         5  7/30/1975                 <NA>         30
    ## 472         5  7/30/1975                 <NA>         60
    ## 473         5  7/30/1975                 <NA>          0
    ## 474         5  7/30/1975                 <NA>         30
    ## 475         5  7/30/1975                 <NA>          0
    ## 476         5  7/30/1975                 <NA>         30
    ## 477         5  6/23/1976                 <NA>          0
    ## 478         5  6/23/1976                 <NA>         30
    ## 479         5  6/23/1976                 <NA>         60
    ## 480         5  6/23/1976                 <NA>          0
    ## 481         5  6/23/1976                 <NA>         30
    ## 482         5  6/23/1976                 <NA>         60
    ## 483         5  6/23/1976                 <NA>          0
    ## 484         5  6/23/1976                 <NA>         30
    ## 485         5  6/23/1976                 <NA>         60
    ## 486         5  6/23/1976                 <NA>          0
    ## 487         5  6/23/1976                 <NA>         30
    ## 488         5  6/23/1976                 <NA>         60
    ## 489         5  8/14/1975                 <NA>          0
    ## 490         5  8/14/1975                 <NA>         30
    ## 491         5  8/14/1975                 <NA>         60
    ## 492         5  8/14/1975                 <NA>          0
    ## 493         5  8/14/1975                 <NA>         30
    ## 494         5  8/14/1975                 <NA>         60
    ## 495         5  8/14/1975                 <NA>          0
    ## 496         5  8/14/1975                 <NA>         30
    ## 497         5  8/14/1975                 <NA>         60
    ## 498         5  8/14/1975                 <NA>          0
    ## 499         5  8/14/1975                 <NA>         30
    ## 500         5  8/14/1975                 <NA>         60
    ## 501         5  8/14/1975                 <NA>          0
    ## 502         5  8/14/1975                 <NA>         30
    ## 503         5  8/14/1975                 <NA>         60
    ## 504         5  8/14/1975                 <NA>          0
    ## 505         5  8/14/1975                 <NA>         30
    ## 506         5  8/14/1975                 <NA>         60
    ## 507         5  8/13/1975                 <NA>          0
    ## 508         5  8/13/1975                 <NA>          0
    ## 509         5  8/13/1975                 <NA>         30
    ## 510         5  8/13/1975                 <NA>         60
    ## 511         5  8/14/1975                 <NA>          0
    ## 512         5  8/14/1975                 <NA>         30
    ## 513         5  8/14/1975                 <NA>         60
    ## 514         5  8/14/1975                 <NA>          0
    ## 515         5  8/14/1975                 <NA>         30
    ## 516         5  8/13/1975                 <NA>          0
    ## 517         5  8/13/1975                 <NA>         30
    ## 518         5  8/13/1975                 <NA>         30
    ## 519         5  8/14/1975                 <NA>          0
    ## 520         5  8/14/1975                 <NA>          0
    ## 521         5  8/13/1975                 <NA>          0
    ## 522         5  8/13/1975                 <NA>          0
    ## 523         5  8/14/1975                 <NA>         30
    ## 524         5  8/14/1975                 <NA>         60
    ## 525         5  8/14/1975                 <NA>          0
    ## 526         7  8/13/1975                 <NA>          0
    ## 527         7  8/13/1975                 <NA>         30
    ## 528         7  8/13/1975                 <NA>         60
    ## 529         7  8/13/1975                 <NA>          0
    ## 530         7  8/13/1975                 <NA>         30
    ## 531         7  8/13/1975                 <NA>         60
    ## 532         7  8/14/1975                 <NA>          0
    ## 533         7  8/14/1975                 <NA>         30
    ## 534         7  8/14/1975                 <NA>         60
    ## 535         7  8/14/1975                 <NA>          0
    ## 536         7  8/14/1975                 <NA>         30
    ## 537         7  8/14/1975                 <NA>         60
    ## 538         7  8/13/1975                 <NA>          0
    ## 539         7  8/13/1975                 <NA>         30
    ## 540         7  8/13/1975                 <NA>         60
    ## 541         7  8/13/1975                 <NA>          0
    ## 542         7  8/13/1975                 <NA>         30
    ## 543         7  8/13/1975                 <NA>         60
    ## 544         7  8/13/1975                 <NA>          0
    ## 545         7  8/13/1975                 <NA>         30
    ## 546         7  8/13/1975                 <NA>         60
    ## 547         5   7/9/1976                 <NA>          0
    ## 548         5   7/9/1976                 <NA>          0
    ## 549         5   7/9/1976                 <NA>          0
    ## 550         5   7/9/1976                 <NA>         30
    ## 551         5   7/9/1976                 <NA>          0
    ## 552         5   7/9/1976                 <NA>         30
    ## 553         5   7/9/1976                 <NA>         50
    ## 554         5   7/9/1976                 <NA>          0
    ## 555         5   7/9/1976                 <NA>          0
    ## 556         5       <NA>                 <NA>          0
    ## 557         5       <NA>                 <NA>       <NA>
    ## 558         5       <NA>                 <NA>       <NA>
    ## 559         5       <NA>                 <NA>          0
    ## 560         5       <NA>                 <NA>       <NA>
    ## 561         5       <NA>                 <NA>       <NA>
    ## 562         5       <NA>                 <NA>       <NA>
    ## 563         5       <NA>                 <NA>       <NA>
    ## 564         5       <NA>                 <NA>       <NA>
    ## 565         5       <NA>                 <NA>       <NA>
    ## 566         5       <NA>                 <NA>          0
    ## 567         5       <NA>                 <NA>       <NA>
    ## 568         5       <NA>                 <NA>       <NA>
    ## 569         5       <NA>                 <NA>          0
    ## 570         5       <NA>                 <NA>       <NA>
    ## 571         5       <NA>                 <NA>       <NA>
    ## 572         1  4/23/1982                 <NA>       <NA>
    ## 573         1  4/26/1982                 <NA>       <NA>
    ## 574         1  4/20/1982                 <NA>       <NA>
    ## 575         1  4/23/1982                 <NA>       <NA>
    ## 576         1  4/14/1982                 <NA>       <NA>
    ## 577         1  4/28/1982                 <NA>       <NA>
    ## 578         1  4/28/1982                 <NA>       <NA>
    ## 579         1  4/19/1982                 <NA>       <NA>
    ## 580         2 11/21/1988                 <NA>       <NA>
    ## 581         2 11/21/1988                 <NA>       <NA>
    ## 582         2 11/21/1988                 <NA>       <NA>
    ## 583         2 11/21/1988                 <NA>       <NA>
    ## 584         2 11/21/1988                 <NA>       <NA>
    ## 585         2 11/21/1988                 <NA>       <NA>
    ## 586         2 11/21/1988                 <NA>       <NA>
    ## 587         2 11/21/1988                 <NA>       <NA>
    ## 588         2 11/21/1988                 <NA>       <NA>
    ## 589         2 11/21/1988                 <NA>       <NA>
    ## 590         2 11/21/1988                 <NA>       <NA>
    ## 591         2 11/21/1988                 <NA>       <NA>
    ## 592         2 11/21/1988                 <NA>       <NA>
    ## 593         2 11/21/1988                 <NA>       <NA>
    ## 594         2 11/21/1988                 <NA>       <NA>
    ## 595         2 11/21/1988                 <NA>       <NA>
    ## 596         2 11/21/1988                 <NA>       <NA>
    ## 597         2 11/21/1988                 <NA>       <NA>
    ## 598         2 11/21/1988                 <NA>       <NA>
    ## 599         2  4/16/1981                 <NA>        300
    ## 600         2  4/16/1981                 <NA>        250
    ## 601         1       <NA>                 <NA>       <NA>
    ## 602         1       <NA>                 <NA>       <NA>
    ## 603         1       <NA>                 <NA>       <NA>
    ## 604         1       <NA>                 <NA>       <NA>
    ## 605         1       <NA>                 <NA>       <NA>
    ## 606         1       <NA>                 <NA>       <NA>
    ## 607         1       <NA>                 <NA>       <NA>
    ## 608         1       <NA>                 <NA>       <NA>
    ## 609         1       <NA>                 <NA>       <NA>
    ## 610         1       <NA>                 <NA>          0
    ## 611         1       <NA>                 <NA>          0
    ## 612        10  5/18/1987                 <NA>       <NA>
    ## 613         3     6/1979                 <NA>       <NA>
    ## 614         2     6/1979                 <NA>       <NA>
    ## 615         3     6/1979                 <NA>       <NA>
    ## 616         2     6/1979                 <NA>       <NA>
    ## 617         6     6/1979                 <NA>       <NA>
    ## 618         2     6/1979                 <NA>       <NA>
    ## 619         6     6/1979                 <NA>       <NA>
    ## 620         1     6/1979                 <NA>       <NA>
    ## 621         2     6/1979                 <NA>       <NA>
    ## 622         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 623         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 624         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 625         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 626         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 627         1  7/23/1980 1980-07-25T00:00:00Z       <NA>
    ## 628         1  4/28/1981                 <NA>       <NA>
    ## 629         1  4/28/1981                 <NA>       <NA>
    ## 630         1  4/28/1981                 <NA>       <NA>
    ## 631         1  4/28/1981                 <NA>       <NA>
    ## 632         1  4/28/1981                 <NA>       <NA>
    ## 633         1  4/28/1981                 <NA>       <NA>
    ## 634         1  9/19/1985                 <NA>       <NA>
    ## 635         1  9/19/1985                 <NA>       <NA>
    ## 636         1  9/19/1985                 <NA>       <NA>
    ## 637         1  9/19/1985                 <NA>       <NA>
    ## 638         5 10/23/1987                 <NA>       <NA>
    ## 639         5 10/23/1987                 <NA>       <NA>
    ## 640         5 10/23/1987                 <NA>       <NA>
    ## 641         5 10/23/1987                 <NA>       <NA>
    ## 642         5 10/23/1987                 <NA>       <NA>
    ## 643         5 10/23/1987                 <NA>       <NA>
    ## 644         5 10/23/1987                 <NA>       <NA>
    ## 645         5 10/23/1987                 <NA>       <NA>
    ## 646         5 10/23/1987                 <NA>       <NA>
    ## 647         5 10/23/1987                 <NA>       <NA>
    ## 648         5 10/23/1987                 <NA>       <NA>
    ## 649         5 10/23/1987                 <NA>       <NA>
    ## 650         5 10/23/1987                 <NA>       <NA>
    ## 651         5 10/23/1987                 <NA>       <NA>
    ## 652         1  3/16/1982                 <NA>       <NA>
    ## 653         1  3/16/1982                 <NA>       <NA>
    ## 654         1  3/16/1982                 <NA>       <NA>
    ## 655         1  6/20/1984                 <NA>       <NA>
    ## 656         1  6/20/1984                 <NA>       <NA>
    ## 657         1  6/20/1984                 <NA>       <NA>
    ## 658         1  6/20/1984                 <NA>       <NA>
    ## 659         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 660         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 661         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 662         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 663         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 664         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 665         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 666         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 667         5 10/19/1987 1987-10-29T00:00:00Z      0-2cm
    ## 668         1       <NA>                 <NA>       <NA>
    ## 669         5  12/7/1988                 <NA>       <NA>
    ## 670         5  12/7/1988                 <NA>       <NA>
    ## 671         5       <NA>                 <NA>       <NA>
    ## 672         5       <NA>                 <NA>       <NA>
    ## 673         5       <NA>                 <NA>       <NA>
    ## 674         5       <NA>                 <NA>       <NA>
    ## 675         5       <NA>                 <NA>       <NA>
    ## 676         5       <NA>                 <NA>       <NA>
    ## 677         5       <NA>                 <NA>       <NA>
    ## 678         5       <NA>                 <NA>       <NA>
    ## 679         5       <NA>                 <NA>       <NA>
    ## 680         5       <NA>                 <NA>       <NA>
    ## 681         5       <NA>                 <NA>       <NA>
    ## 682         5       <NA>                 <NA>       <NA>
    ## 683         5       <NA>                 <NA>       <NA>
    ## 684         5       <NA>                 <NA>       <NA>
    ## 685         5       <NA>                 <NA>       <NA>
    ## 686         5       <NA>                 <NA>       <NA>
    ## 687         5       <NA>                 <NA>       <NA>
    ## 688         5       <NA>                 <NA>       <NA>
    ## 689         5       <NA>                 <NA>       <NA>
    ## 690         5       <NA>                 <NA>       <NA>
    ## 691         5       <NA>                 <NA>       <NA>
    ## 692         5       <NA>                 <NA>       <NA>
    ## 693         5       <NA>                 <NA>       <NA>
    ## 694         5       <NA>                 <NA>       <NA>
    ## 695         5       <NA>                 <NA>       <NA>
    ## 696         5       <NA>                 <NA>       <NA>
    ## 697         5       <NA>                 <NA>       <NA>
    ## 698         5       <NA>                 <NA>       <NA>
    ## 699         5       <NA>                 <NA>       <NA>
    ## 700         5       <NA>                 <NA>       <NA>
    ## 701         5       <NA>                 <NA>       <NA>
    ## 702         5       <NA>                 <NA>       <NA>
    ## 703         5       <NA>                 <NA>       <NA>
    ## 704         5       <NA>                 <NA>       <NA>
    ## 705         5       <NA>                 <NA>       <NA>
    ## 706         5       <NA>                 <NA>       <NA>
    ## 707         5       <NA>                 <NA>       <NA>
    ## 708         5       <NA>                 <NA>       <NA>
    ## 709         5       <NA>                 <NA>       <NA>
    ## 710         5       <NA>                 <NA>       <NA>
    ## 711         5       <NA>                 <NA>       <NA>
    ## 712         5       <NA>                 <NA>       <NA>
    ## 713         5       <NA>                 <NA>       <NA>
    ## 714         5       <NA>                 <NA>       <NA>
    ## 715         5       <NA>                 <NA>       <NA>
    ## 716         5       <NA>                 <NA>       <NA>
    ## 717         5       <NA>                 <NA>       <NA>
    ## 718         5       <NA>                 <NA>       <NA>
    ## 719         5       <NA>                 <NA>       <NA>
    ## 720         5       <NA>                 <NA>       <NA>
    ## 721         5       <NA>                 <NA>       <NA>
    ## 722         5       <NA>                 <NA>       <NA>
    ## 723         5       <NA>                 <NA>       <NA>
    ## 724         5       <NA>                 <NA>       <NA>
    ## 725         5       <NA>                 <NA>       <NA>
    ## 726         5       <NA>                 <NA>       <NA>
    ## 727         5       <NA>                 <NA>       <NA>
    ## 728         5       <NA>                 <NA>       <NA>
    ## 729         5       <NA>                 <NA>       <NA>
    ## 730         5       <NA>                 <NA>       <NA>
    ## 731         5       <NA>                 <NA>       <NA>
    ## 732         5       <NA>                 <NA>       <NA>
    ## 733         5       <NA>                 <NA>       <NA>
    ## 734         5       <NA>                 <NA>       <NA>
    ## 735         5       <NA>                 <NA>       <NA>
    ## 736         5       <NA>                 <NA>       <NA>
    ## 737         5       <NA>                 <NA>       <NA>
    ## 738         5       <NA>                 <NA>       <NA>
    ## 739         5       <NA>                 <NA>       <NA>
    ## 740         5       <NA>                 <NA>       <NA>
    ## 741         5       <NA>                 <NA>       <NA>
    ## 742         5       <NA>                 <NA>       <NA>
    ## 743         5       <NA>                 <NA>       <NA>
    ## 744         5       <NA>                 <NA>       <NA>
    ## 745         5       <NA>                 <NA>       <NA>
    ## 746         5       <NA>                 <NA>       <NA>
    ## 747         5       <NA>                 <NA>       <NA>
    ## 748         5       <NA>                 <NA>       <NA>
    ## 749         5       <NA>                 <NA>       <NA>
    ## 750         5       <NA>                 <NA>       <NA>
    ## 751         5       <NA>                 <NA>       <NA>
    ## 752         5       <NA>                 <NA>       <NA>
    ## 753         5       <NA>                 <NA>       <NA>
    ## 754         5       <NA>                 <NA>       <NA>
    ## 755         5       <NA>                 <NA>       <NA>
    ## 756         5       <NA>                 <NA>       <NA>
    ## 757         5       <NA>                 <NA>       <NA>
    ## 758         5       <NA>                 <NA>       <NA>
    ## 759         5       <NA>                 <NA>       <NA>
    ## 760         5       <NA>                 <NA>       <NA>
    ## 761         5       <NA>                 <NA>       <NA>
    ## 762         5       <NA>                 <NA>       <NA>
    ## 763         5       <NA>                 <NA>       <NA>
    ## 764         5       <NA>                 <NA>       <NA>
    ## 765         5       <NA>                 <NA>       <NA>
    ## 766         5       <NA>                 <NA>       <NA>
    ## 767         5       <NA>                 <NA>       <NA>
    ## 768         5       <NA>                 <NA>       <NA>
    ## 769         5       <NA>                 <NA>       <NA>
    ## 770         5       <NA>                 <NA>       <NA>
    ## 771         5       <NA>                 <NA>       <NA>
    ## 772         5       <NA>                 <NA>       <NA>
    ## 773         5       <NA>                 <NA>       <NA>
    ## 774         5       <NA>                 <NA>       <NA>
    ## 775         5       <NA>                 <NA>       <NA>
    ## 776         5       <NA>                 <NA>       <NA>
    ## 777         5       <NA>                 <NA>       <NA>
    ## 778         5       <NA>                 <NA>       <NA>
    ## 779         5       <NA>                 <NA>       <NA>
    ## 780         5       <NA>                 <NA>       <NA>
    ## 781         5       <NA>                 <NA>       <NA>
    ## 782         5       <NA>                 <NA>       <NA>
    ## 783         5       <NA>                 <NA>       <NA>
    ## 784         5       <NA>                 <NA>       <NA>
    ## 785         5       <NA>                 <NA>       <NA>
    ## 786         5       <NA>                 <NA>       <NA>
    ## 787         5       <NA>                 <NA>       <NA>
    ## 788         5       <NA>                 <NA>       <NA>
    ## 789         5       <NA>                 <NA>       <NA>
    ## 790         5       <NA>                 <NA>       <NA>
    ## 791         5       <NA>                 <NA>       <NA>
    ## 792         5       <NA>                 <NA>       <NA>
    ## 793         5       <NA>                 <NA>       <NA>
    ## 794         5       <NA>                 <NA>       <NA>
    ## 795         5       <NA>                 <NA>       <NA>
    ## 796         5       <NA>                 <NA>       <NA>
    ## 797         5       <NA>                 <NA>       <NA>
    ## 798         5       <NA>                 <NA>       <NA>
    ## 799         5       <NA>                 <NA>       <NA>
    ## 800         5       <NA>                 <NA>       <NA>
    ## 801         6    6/29/93                 <NA> 0 to 2-3cm
    ## 802         4    7/14/93                 <NA> 0 to 2-3cm
    ## 803         4    6/29/93                 <NA> 0 to 2-3cm
    ## 804         4    6/29/93                 <NA> 0 to 2-3cm
    ## 805         4    7/14/93                 <NA> 0 to 2-3cm
    ## 806         3    7/12/93                 <NA> 0 to 2-3cm
    ## 807         3    7/12/93                 <NA> 0 to 2-3cm
    ## 808         3    6/29/93                 <NA> 0 to 2-3cm
    ## 809         3    6/29/93                 <NA> 0 to 2-3cm
    ## 810         3    6/29/93                 <NA> 0 to 2-3cm
    ## 811         2    6/30/93                 <NA> 0 to 2-3cm
    ## 812         2    6/30/93                 <NA> 0 to 2-3cm
    ## 813         2    7/12/93                 <NA> 0 to 2-3cm
    ## 814         2    7/12/93                 <NA> 0 to 2-3cm
    ## 815         2    7/14/93                 <NA> 0 to 2-3cm
    ## 816         1    6/28/93                 <NA> 0 to 2-3cm
    ## 817         1    6/28/93                 <NA> 0 to 2-3cm
    ## 818         1    7/13/93                 <NA> 0 to 2-3cm
    ## 819         1    7/13/93                 <NA> 0 to 2-3cm
    ## 820         1    7/13/93                 <NA> 0 to 2-3cm
    ## 821         1    7/13/93                 <NA> 0 to 2-3cm
    ## 822         1    7/13/93                 <NA> 0 to 2-3cm
    ## 823         1    7/13/93                 <NA> 0 to 2-3cm
    ## 824         1    7/15/93                 <NA> 0 to 2-3cm
    ## 825         1    7/15/93                 <NA> 0 to 2-3cm
    ## 826         1    7/15/93                 <NA> 0 to 2-3cm
    ## 827         1    6/28/93                 <NA> 0 to 2-3cm
    ## 828         1    7/15/93                 <NA> 0 to 2-3cm
    ## 829         1    6/28/93                 <NA> 0 to 2-3cm
    ## 830         1    6/30/93                 <NA> 0 to 2-3cm
    ## 831         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 832         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 833         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 834         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 835         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 836         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 837         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 838         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 839         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 840         5  1/22/1987 1987-01-29T00:00:00Z       <NA>
    ## 841         1  8/10/1994                 <NA>       <NA>
    ## 842         1  8/10/1994                 <NA>       <NA>
    ## 843         1  8/10/1994                 <NA>       <NA>
    ## 844         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 845         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 846         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 847         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 848         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 849         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 850         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 851         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 852         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 853         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 854         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 855         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 856         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 857         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 858         2   5/3/1994 1994-05-04T00:00:00Z       <NA>
    ## 859         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 860         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 861         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 862         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 863         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 864         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 865         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 866         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 867         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 868         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 869         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 870         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 871         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 872         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 873         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 874         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 875         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 876         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 877         4     5/1989 1989-06-01T00:00:00Z       <NA>
    ## 878         5       <NA>                 <NA>     12 ft.
    ## 879         5       <NA>                 <NA>     12 ft.
    ## 880         5       <NA>                 <NA>     12 ft.
    ## 881         2   8/2/1989                 <NA>       <NA>
    ## 882         2   8/2/1989                 <NA>       <NA>
    ## 883         2       <NA>                 <NA>          3
    ## 884         2       <NA>                 <NA>          3
    ## 885         2       <NA>                 <NA>          3
    ## 886         2       <NA>                 <NA>          3
    ## 887         2       <NA>                 <NA>          3
    ## 888         2       <NA>                 <NA>          3
    ## 889         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 890         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 891         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 892         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 893         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 894         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 895         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 896         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 897         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 898         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 899         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 900         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 901         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 902         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 903         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 904         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 905         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 906         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 907         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 908         2     8/1992 1992-10-01T00:00:00Z       <NA>
    ## 909         2       <NA>                 <NA>       <NA>
    ## 910         2       <NA>                 <NA>       <NA>
    ## 911         2       <NA>                 <NA>       <NA>
    ## 912         2    10/1989                 <NA>       <NA>
    ## 913         2    10/1989                 <NA>       <NA>
    ## 914         2    10/1989                 <NA>       <NA>
    ## 915         2    10/1989                 <NA>       <NA>
    ## 916         2    10/1989                 <NA>       <NA>
    ## 917         2    10/1989                 <NA>       <NA>
    ## 918         2    10/1989                 <NA>       <NA>
    ## 919         2    10/1989                 <NA>       <NA>
    ## 920         2    10/1989                 <NA>       <NA>
    ## 921         2    10/1989                 <NA>       <NA>
    ## 922         2    10/1989                 <NA>       <NA>
    ## 923         2    10/1989                 <NA>       <NA>
    ## 924         2    10/1989                 <NA>       <NA>
    ## 925         2    10/1989                 <NA>       <NA>
    ## 926         2    10/1989                 <NA>       <NA>
    ## 927         2    10/1989                 <NA>       <NA>
    ## 928         2    10/1989                 <NA>       <NA>
    ##                           DPTH_CODE COR_GRB_CD
    ## 1                           Unknown       Grab
    ## 2                           Unknown       Grab
    ## 3                           Unknown       Grab
    ## 4                           Unknown       Grab
    ## 5                           Unknown       Grab
    ## 6                           Unknown       Grab
    ## 7                           Unknown       Grab
    ## 8                           Surface       Grab
    ## 9                           Surface       Grab
    ## 10                          Surface       Grab
    ## 11                          Surface       Grab
    ## 12                          Surface       Grab
    ## 13                          Surface       Grab
    ## 14                          Surface       Grab
    ## 15                          Surface       Grab
    ## 16                          Surface       Grab
    ## 17                          Surface       Grab
    ## 18                          Surface       Grab
    ## 19                          Surface       Grab
    ## 20                          Unknown       Grab
    ## 21                            Depth       Grab
    ## 22                            Depth       Grab
    ## 23                            Depth       Grab
    ## 24                            Depth       Grab
    ## 25                            Depth       Grab
    ## 26                            Depth       Grab
    ## 27                            Depth       Grab
    ## 28                            Depth       Grab
    ## 29                            Depth       Grab
    ## 30                            Depth       Grab
    ## 31                            Depth       Grab
    ## 32                          Unknown       Grab
    ## 33                          Unknown       Grab
    ## 34                          Unknown       Grab
    ## 35                          Unknown       Grab
    ## 36                          Unknown       Grab
    ## 37                          Unknown       Grab
    ## 38                          Unknown       Grab
    ## 39                          Unknown       Grab
    ## 40                          Unknown       Grab
    ## 41                          Unknown       Grab
    ## 42                          Unknown       Grab
    ## 43                          Unknown       Grab
    ## 44                          Unknown       Grab
    ## 45                            Depth       Core
    ## 46                          Surface       Core
    ## 47                            Depth       Core
    ## 48                          Surface       Core
    ## 49                            Depth       Core
    ## 50                          Surface       Core
    ## 51                            Depth       Core
    ## 52                          Surface       Core
    ## 53                          Surface       Core
    ## 54                            Depth       Core
    ## 55                            Depth       Core
    ## 56                          Surface       Core
    ## 57                            Depth       Core
    ## 58                          Surface       Core
    ## 59                            Depth       Core
    ## 60                          Surface       Core
    ## 61                            Depth       Core
    ## 62                          Surface       Core
    ## 63                            Depth       Core
    ## 64                          Surface       Core
    ## 65                            Depth       Core
    ## 66                          Surface       Core
    ## 67                          Surface       Core
    ## 68                            Depth       Core
    ## 69                          Surface       Core
    ## 70                            Depth       Core
    ## 71                          Surface       Core
    ## 72                            Depth       Core
    ## 73                          Surface       Core
    ## 74                            Depth       Core
    ## 75                          Surface       Core
    ## 76                            Depth       Core
    ## 77                          Surface       Core
    ## 78                            Depth       Core
    ## 79                          Surface       Core
    ## 80                          Unknown       Grab
    ## 81                          Unknown       Grab
    ## 82                          Unknown       Grab
    ## 83                          Unknown       Grab
    ## 84                          Unknown       Grab
    ## 85                          Unknown       Grab
    ## 86                          Unknown       Grab
    ## 87                          Unknown       Grab
    ## 88                          Unknown       Grab
    ## 89                          Unknown       Grab
    ## 90                          Unknown       Grab
    ## 91                          Unknown       Grab
    ## 92                          Unknown       Grab
    ## 93                          Unknown       Grab
    ## 94                          Unknown       Grab
    ## 95                          Unknown       Grab
    ## 96                          Unknown       Grab
    ## 97                          Unknown       Grab
    ## 98                          Unknown       Grab
    ## 99                          Unknown       Grab
    ## 100                         Unknown       Grab
    ## 101                         Unknown       Grab
    ## 102                         Unknown       Grab
    ## 103                         Unknown       Grab
    ## 104                         Unknown       Grab
    ## 105                         Unknown       Grab
    ## 106                         Surface       Core
    ## 107                         Surface       Core
    ## 108                         Surface       Core
    ## 109                         Surface       Core
    ## 110                         Surface       Core
    ## 111                         Surface       Core
    ## 112                         Surface       Core
    ## 113                         Unknown       Grab
    ## 114                         Unknown   Core (?)
    ## 115                         Unknown   Core (?)
    ## 116                         Unknown   Core (?)
    ## 117                         Unknown   Core (?)
    ## 118                         Unknown       Grab
    ## 119                         Unknown       Grab
    ## 120                         Unknown       Grab
    ## 121                         Surface       Core
    ## 122                           Depth       Core
    ## 123                           Depth       Core
    ## 124                         Unknown       Core
    ## 125                         Unknown       Core
    ## 126                         Unknown       Core
    ## 127                         Unknown       Core
    ## 128                         Unknown       Core
    ## 129                         Unknown       Core
    ## 130                         Unknown       Core
    ## 131                           Depth       Core
    ## 132                           Depth       Core
    ## 133                           Depth       Core
    ## 134                           Depth       Core
    ## 135                           Depth       Core
    ## 136                           Depth       Core
    ## 137                           Depth       Core
    ## 138                           Depth       Core
    ## 139                           Depth       Core
    ## 140                           Depth       Core
    ## 141                           Depth       Core
    ## 142                           Depth       Core
    ## 143                           Depth       Core
    ## 144                           Depth       Core
    ## 145                           Depth       Core
    ## 146                           Depth       Core
    ## 147                           Depth       Core
    ## 148                           Depth       Core
    ## 149                           Depth       Core
    ## 150                           Depth       Core
    ## 151                           Depth       Core
    ## 152                           Depth       Core
    ## 153                           Depth       Core
    ## 154                           Depth       Core
    ## 155                           Depth       Core
    ## 156                           Depth       Core
    ## 157                         Unknown       Core
    ## 158                         Unknown       Core
    ## 159                         Unknown       Core
    ## 160                         Unknown       Core
    ## 161                         Unknown       Grab
    ## 162                         Unknown       Grab
    ## 163                           Depth       Grab
    ## 164                           Depth       Grab
    ## 165                         Unknown       Grab
    ## 166                         Unknown       Grab
    ## 167                         Unknown       Grab
    ## 168                         Unknown       Grab
    ## 169                         Surface       Grab
    ## 170 Unknown (COE, probably surface)       Grab
    ## 171 Unknown (COE, probably surface)       Grab
    ## 172 Unknown (COE, probably surface)       Grab
    ## 173 Unknown (COE, probably surface)       Grab
    ## 174 Unknown (COE, probably surface)       Grab
    ## 175 Unknown (COE, probably surface)       Grab
    ## 176 Unknown (COE, probably surface)       Grab
    ## 177 Unknown (COE, probably surface)       Grab
    ## 178 Unknown (COE, probably surface)       Grab
    ## 179 Unknown (COE, probably surface)       Grab
    ## 180 Unknown (COE, probably surface)       Grab
    ## 181                         Surface       Core
    ## 182                         Surface       Grab
    ## 183                         Surface       Grab
    ## 184                           Depth       Core
    ## 185                         Surface       Grab
    ## 186                         Surface       Grab
    ## 187                         Surface       Grab
    ## 188                         Surface       Grab
    ## 189                         Surface       Grab
    ## 190                         Surface       Grab
    ## 191                         Surface       Grab
    ## 192                         Surface       Grab
    ## 193                         Unknown       Grab
    ## 194                         Unknown       Grab
    ## 195                         Unknown       Grab
    ## 196                         Unknown       Grab
    ## 197                         Surface       Grab
    ## 198                         Surface       Grab
    ## 199                         Surface       Grab
    ## 200                         Surface       Grab
    ## 201                         Surface       Grab
    ## 202                         Surface       Grab
    ## 203                         Surface       Grab
    ## 204                         Surface       Grab
    ## 205                         Surface       Grab
    ## 206                         Surface       Grab
    ## 207                         Surface       Grab
    ## 208                         Surface       Grab
    ## 209                         Surface       Grab
    ## 210                         Surface       Grab
    ## 211                         Surface       Grab
    ## 212                         Surface       Grab
    ## 213                         Surface       Grab
    ## 214                         Surface       Grab
    ## 215                         Surface       Grab
    ## 216                         Surface       Grab
    ## 217                         Surface       Grab
    ## 218                         Surface       Grab
    ## 219                         Surface       Grab
    ## 220                         Surface       Grab
    ## 221                         Surface       Grab
    ## 222                         Surface       Grab
    ## 223                         Surface       Grab
    ## 224                         Surface       Grab
    ## 225                         Surface       Grab
    ## 226                         Surface       Grab
    ## 227                         Surface       Grab
    ## 228                         Surface       Grab
    ## 229                         Surface       Grab
    ## 230                         Surface       Grab
    ## 231                         Surface       Grab
    ## 232                         Surface       Grab
    ## 233                         Surface       Grab
    ## 234                         Surface       Grab
    ## 235                         Surface       Grab
    ## 236                         Surface       Grab
    ## 237                         Surface       Grab
    ## 238                         Surface       Grab
    ## 239                         Surface       Grab
    ## 240                         Surface       Grab
    ## 241                         Surface       Grab
    ## 242                         Surface       Grab
    ## 243                         Surface       Grab
    ## 244                         Surface       Grab
    ## 245                         Surface       Grab
    ## 246                         Surface       Grab
    ## 247                         Surface       Grab
    ## 248                         Surface       Grab
    ## 249                         Surface       Grab
    ## 250                         Surface       Grab
    ## 251                         Surface       Grab
    ## 252                         Surface       Grab
    ## 253                         Surface       Grab
    ## 254                         Surface       Grab
    ## 255                         Surface       Grab
    ## 256                         Surface       Grab
    ## 257                         Surface       Grab
    ## 258                         Surface       Grab
    ## 259                         Surface       Grab
    ## 260                         Surface       Grab
    ## 261                         Surface       Grab
    ## 262                         Surface       Grab
    ## 263                         Surface       Grab
    ## 264                         Surface       Grab
    ## 265                         Surface       Grab
    ## 266                         Surface       Grab
    ## 267                         Surface       Grab
    ## 268                         Surface       Grab
    ## 269                         Surface       Grab
    ## 270                         Surface       Grab
    ## 271                         Surface       Grab
    ## 272                         Surface       Grab
    ## 273                         Surface       Grab
    ## 274                         Surface       Grab
    ## 275                         Surface       Grab
    ## 276                         Surface       Grab
    ## 277                         Surface       Grab
    ## 278                         Surface       Grab
    ## 279                         Surface       Grab
    ## 280                         Surface       Grab
    ## 281                         Surface       Grab
    ## 282                         Surface       Grab
    ## 283                         Surface       Grab
    ## 284                         Surface       Grab
    ## 285                         Surface       Grab
    ## 286                         Surface       Grab
    ## 287                         Surface       Grab
    ## 288                         Surface       Grab
    ## 289                         Surface       Grab
    ## 290                         Surface       Grab
    ## 291                         Surface       Grab
    ## 292                         Surface       Grab
    ## 293                         Unknown       Grab
    ## 294                         Unknown       Grab
    ## 295                         Unknown       Grab
    ## 296                         Unknown       Grab
    ## 297                         Unknown       Grab
    ## 298                         Unknown       Grab
    ## 299                         Unknown       Grab
    ## 300                           Depth       Grab
    ## 301                           Depth       Grab
    ## 302                           Depth       Grab
    ## 303                         Unknown       Grab
    ## 304                         Unknown       Grab
    ## 305                         Unknown       Grab
    ## 306                         Unknown       Grab
    ## 307                         Unknown       Grab
    ## 308                         Unknown       Grab
    ## 309                         Unknown       Grab
    ## 310                         Unknown       Grab
    ## 311                         Unknown       Grab
    ## 312                         Unknown       Grab
    ## 313                         Unknown       Grab
    ## 314                         Unknown       Grab
    ## 315                         Unknown       Grab
    ## 316                         Unknown       Grab
    ## 317                         Unknown       Grab
    ## 318                         Unknown       Grab
    ## 319                         Unknown       Grab
    ## 320                         Unknown       Grab
    ## 321                         Unknown       Grab
    ## 322                         Unknown       Grab
    ## 323                         Unknown       Grab
    ## 324                         Unknown       Grab
    ## 325                         Unknown       Grab
    ## 326                         Unknown       Grab
    ## 327                         Unknown       Grab
    ## 328                         Unknown       Grab
    ## 329                         Unknown       Grab
    ## 330                         Unknown       Grab
    ## 331                           Depth       Grab
    ## 332                           Depth       Grab
    ## 333                           Depth       Grab
    ## 334                           Depth       Grab
    ## 335                         Surface       Grab
    ## 336                         Surface       Grab
    ## 337                         Surface       Grab
    ## 338                         Surface       Grab
    ## 339                         Surface       Grab
    ## 340                         Surface       Grab
    ## 341                         Surface       Grab
    ## 342                         Surface       Grab
    ## 343                         Surface       Grab
    ## 344                         Surface       Grab
    ## 345                         Surface       Grab
    ## 346                         Surface       Grab
    ## 347                         Surface       Grab
    ## 348                         Surface       Grab
    ## 349                         Surface       Grab
    ## 350                         Surface       Grab
    ## 351                         Surface       Grab
    ## 352                         Surface       Grab
    ## 353                         Surface       Grab
    ## 354                         Surface       Grab
    ## 355                         Surface       Grab
    ## 356                         Surface       Grab
    ## 357                         Surface       Grab
    ## 358                         Surface       Grab
    ## 359                         Surface       Grab
    ## 360                         Surface       Grab
    ## 361                         Surface       Grab
    ## 362                         Surface       Grab
    ## 363                         Surface       Grab
    ## 364                         Surface       Grab
    ## 365                         Surface       Grab
    ## 366                         Surface       Grab
    ## 367                         Surface       Grab
    ## 368                         Surface       Grab
    ## 369                         Surface       Grab
    ## 370                         Surface       Grab
    ## 371                         Surface       Grab
    ## 372                         Surface       Grab
    ## 373                         Surface       Grab
    ## 374                         Surface       Grab
    ## 375                         Surface       Grab
    ## 376                         Surface       Grab
    ## 377                         Surface       Grab
    ## 378                         Surface       Grab
    ## 379                         Surface       Grab
    ## 380                         Surface       Grab
    ## 381                         Surface       Grab
    ## 382                         Surface       Grab
    ## 383                         Surface       Grab
    ## 384                         Surface       Grab
    ## 385                         Surface       Grab
    ## 386                         Surface       Grab
    ## 387                         Surface       Grab
    ## 388                         Surface       Grab
    ## 389                         Surface       Grab
    ## 390                         Surface       Grab
    ## 391                         Surface       Grab
    ## 392                         Surface       Grab
    ## 393                         Surface       Grab
    ## 394                         Surface       Grab
    ## 395                         Surface       Grab
    ## 396                         Surface       Grab
    ## 397                         Surface       Grab
    ## 398                         Surface       Grab
    ## 399                         Surface       Grab
    ## 400                         Surface       Grab
    ## 401                         Surface       Grab
    ## 402                         Surface       Grab
    ## 403                         Surface       Grab
    ## 404                         Surface       Grab
    ## 405                         Surface       Grab
    ## 406                         Surface       Grab
    ## 407                         Surface       Grab
    ## 408                         Surface       Grab
    ## 409                         Surface       Grab
    ## 410                         Surface       Grab
    ## 411                         Surface       Grab
    ## 412                         Surface       Grab
    ## 413                         Surface       Grab
    ## 414                         Surface       Grab
    ## 415                         Surface       Grab
    ## 416                         Surface       Grab
    ## 417                         Surface       Grab
    ## 418                         Surface       Grab
    ## 419                         Surface       Grab
    ## 420                         Surface       Grab
    ## 421                         Surface       Grab
    ## 422                         Surface       Grab
    ## 423                         Surface       Grab
    ## 424                         Surface       Grab
    ## 425                         Surface       Grab
    ## 426                         Surface       Core
    ## 427                         Surface       Core
    ## 428                           Depth       Core
    ## 429                           Depth       Core
    ## 430                         Surface       Grab
    ## 431                         Surface       Grab
    ## 432                         Surface       Core
    ## 433                         Surface       Core
    ## 434                         Surface       Core
    ## 435                         Surface       Core
    ## 436                           Depth       Core
    ## 437                         Surface       Grab
    ## 438                         Surface       Grab
    ## 439                         Surface       Core
    ## 440                           Depth       Core
    ## 441                         Surface       Core
    ## 442                         Surface       Core
    ## 443                           Depth       Core
    ## 444                         Surface       Core
    ## 445                           Depth       Core
    ## 446                         Surface       Core
    ## 447                         Surface       Core
    ## 448                           Depth       Core
    ## 449                           Depth       Core
    ## 450                         Surface       Core
    ## 451                           Depth       Core
    ## 452                           Depth       Core
    ## 453                         Surface       Core
    ## 454                           Depth       Core
    ## 455                           Depth       Core
    ## 456                         Surface       Core
    ## 457                         Surface       Core
    ## 458                           Depth       Core
    ## 459                           Depth       Core
    ## 460                         Surface       Core
    ## 461                           Depth       Core
    ## 462                           Depth       Core
    ## 463                         Surface       Core
    ## 464                           Depth       Core
    ## 465                         Surface       Core
    ## 466                           Depth       Core
    ## 467                           Depth       Core
    ## 468                         Surface       Core
    ## 469                           Depth       Core
    ## 470                         Surface       Core
    ## 471                           Depth       Core
    ## 472                           Depth       Core
    ## 473                         Surface       Core
    ## 474                           Depth       Core
    ## 475                         Surface       Core
    ## 476                           Depth       Core
    ## 477                         Surface       Core
    ## 478                           Depth       Core
    ## 479                           Depth       Core
    ## 480                         Surface       Core
    ## 481                           Depth       Core
    ## 482                           Depth       Core
    ## 483                         Surface       Core
    ## 484                           Depth       Core
    ## 485                           Depth       Core
    ## 486                         Surface       Core
    ## 487                           Depth       Core
    ## 488                           Depth       Core
    ## 489                         Surface       Core
    ## 490                           Depth       Core
    ## 491                           Depth       Core
    ## 492                         Surface       Core
    ## 493                           Depth       Core
    ## 494                           Depth       Core
    ## 495                         Surface       Core
    ## 496                           Depth       Core
    ## 497                           Depth       Core
    ## 498                         Surface       Core
    ## 499                           Depth       Core
    ## 500                           Depth       Core
    ## 501                         Surface       Core
    ## 502                           Depth       Core
    ## 503                           Depth       Core
    ## 504                         Surface       Core
    ## 505                           Depth       Core
    ## 506                           Depth       Core
    ## 507                         Surface       Core
    ## 508                         Surface       Core
    ## 509                           Depth       Core
    ## 510                           Depth       Core
    ## 511                         Surface       Core
    ## 512                           Depth       Core
    ## 513                           Depth       Core
    ## 514                         Surface       Core
    ## 515                           Depth       Core
    ## 516                         Surface       Core
    ## 517                           Depth       Core
    ## 518                           Depth       Core
    ## 519                         Surface       Core
    ## 520                         Surface       Core
    ## 521                         Surface       Core
    ## 522                         Surface       Core
    ## 523                           Depth       Core
    ## 524                           Depth       Core
    ## 525                         Surface       Core
    ## 526                         Surface       Core
    ## 527                           Depth       Core
    ## 528                           Depth       Core
    ## 529                         Surface       Core
    ## 530                           Depth       Core
    ## 531                           Depth       Core
    ## 532                         Surface       Core
    ## 533                           Depth       Core
    ## 534                           Depth       Core
    ## 535                         Surface       Core
    ## 536                           Depth       Core
    ## 537                           Depth       Core
    ## 538                         Surface       Core
    ## 539                           Depth       Core
    ## 540                           Depth       Core
    ## 541                         Surface       Core
    ## 542                           Depth       Core
    ## 543                           Depth       Core
    ## 544                         Surface       Core
    ## 545                           Depth       Core
    ## 546                           Depth       Core
    ## 547                         Surface       Grab
    ## 548                         Surface       Grab
    ## 549                         Surface       Core
    ## 550                           Depth       Core
    ## 551                         Surface       Core
    ## 552                           Depth       Core
    ## 553                           Depth       Core
    ## 554                         Surface       Grab
    ## 555                         Surface       Grab
    ## 556                         Surface       Grab
    ## 557                         Surface       Grab
    ## 558                           Depth       Grab
    ## 559                         Surface       Grab
    ## 560                         Surface       Grab
    ## 561                           Depth       Grab
    ## 562                         Surface       Grab
    ## 563                           Depth       Grab
    ## 564                         Surface       Grab
    ## 565                           Depth       Grab
    ## 566                         Surface       Grab
    ## 567                         Surface       Grab
    ## 568                           Depth       Grab
    ## 569                         Surface       Grab
    ## 570                         Surface       Grab
    ## 571                           Depth       Grab
    ## 572                         Surface       Core
    ## 573                         Surface       Core
    ## 574                         Surface       Core
    ## 575                         Surface       Core
    ## 576                         Surface       Core
    ## 577                         Surface       Core
    ## 578                         Surface       Core
    ## 579                         Surface       Core
    ## 580                         Surface       Core
    ## 581                         Surface       Core
    ## 582                         Surface       Grab
    ## 583                         Surface       Grab
    ## 584                         Surface       Grab
    ## 585                         Surface       Grab
    ## 586                         Surface       Grab
    ## 587                         Surface       Grab
    ## 588                         Surface       Core
    ## 589                         Surface       Core
    ## 590                         Surface       Grab
    ## 591                         Surface       Core
    ## 592                         Surface       Core
    ## 593                         Surface       Core
    ## 594                         Surface       Grab
    ## 595                         Surface       Grab
    ## 596                         Surface       Grab
    ## 597                         Surface       Grab
    ## 598                         Surface       Grab
    ## 599                           Depth       Core
    ## 600                           Depth       Grab
    ## 601                         Unknown       Grab
    ## 602                         Unknown       Grab
    ## 603                         Unknown       Grab
    ## 604                         Unknown       Grab
    ## 605                         Unknown       Grab
    ## 606                         Unknown       Grab
    ## 607                         Unknown       Grab
    ## 608                           Depth       Grab
    ## 609                           Depth       Grab
    ## 610                   Surface (COE)       Grab
    ## 611                   Surface (COE)       Grab
    ## 612                         Unknown       Grab
    ## 613                         Unknown       Grab
    ## 614                         Unknown       Grab
    ## 615                         Unknown       Grab
    ## 616                         Unknown       Grab
    ## 617                         Unknown       Grab
    ## 618                         Unknown       Grab
    ## 619                         Unknown       Grab
    ## 620                         Unknown       Grab
    ## 621                         Unknown       Grab
    ## 622                         Unknown       Grab
    ## 623                         Unknown       Grab
    ## 624                         Unknown       Grab
    ## 625                         Unknown       Grab
    ## 626                         Unknown       Grab
    ## 627                         Unknown       Grab
    ## 628                         Unknown       Grab
    ## 629                         Unknown       Grab
    ## 630                         Unknown       Grab
    ## 631                         Unknown       Grab
    ## 632                         Unknown       Grab
    ## 633                         Unknown       Grab
    ## 634                         Unknown       Grab
    ## 635                         Unknown       Grab
    ## 636                         Unknown       Grab
    ## 637                         Unknown       Grab
    ## 638                         Surface       Grab
    ## 639                         Surface       Grab
    ## 640                         Surface       Grab
    ## 641                         Surface       Grab
    ## 642                         Surface       Grab
    ## 643                         Surface       Grab
    ## 644                         Surface       Grab
    ## 645                         Surface       Grab
    ## 646                         Surface       Grab
    ## 647                         Surface       Grab
    ## 648                         Surface       Grab
    ## 649                         Surface       Grab
    ## 650                         Surface       Grab
    ## 651                         Surface       Grab
    ## 652                         Unknown       Grab
    ## 653                         Unknown       Grab
    ## 654                         Unknown       Grab
    ## 655                         Unknown    Unknown
    ## 656                         Unknown    Unknown
    ## 657                         Unknown    Unknown
    ## 658                         Unknown    Unknown
    ## 659                         Surface       Grab
    ## 660                         Surface       Grab
    ## 661                         Surface       Grab
    ## 662                         Surface       Grab
    ## 663                         Surface       Grab
    ## 664                         Surface       Grab
    ## 665                         Surface       Grab
    ## 666                         Surface       Grab
    ## 667                         Surface       Grab
    ## 668                           Depth       Core
    ## 669                           Depth       Grab
    ## 670                           Depth       Grab
    ## 671                         Surface       Grab
    ## 672                         Surface       Grab
    ## 673                         Surface       Grab
    ## 674                         Surface       Grab
    ## 675                         Surface       Grab
    ## 676                         Surface       Grab
    ## 677                         Surface       Grab
    ## 678                         Surface       Grab
    ## 679                         Surface       Grab
    ## 680                         Surface       Grab
    ## 681                         Surface       Grab
    ## 682                         Surface       Grab
    ## 683                         Surface       Grab
    ## 684                         Surface       Grab
    ## 685                         Surface       Grab
    ## 686                         Surface       Grab
    ## 687                         Surface       Grab
    ## 688                         Surface       Grab
    ## 689                         Surface       Grab
    ## 690                         Surface       Grab
    ## 691                         Surface       Grab
    ## 692                         Surface       Grab
    ## 693                         Surface       Grab
    ## 694                         Surface       Grab
    ## 695                         Surface       Grab
    ## 696                         Surface       Grab
    ## 697                         Surface       Grab
    ## 698                         Surface       Grab
    ## 699                         Surface       Grab
    ## 700                         Surface       Grab
    ## 701                         Surface       Grab
    ## 702                         Surface       Grab
    ## 703                         Surface       Grab
    ## 704                         Surface       Grab
    ## 705                         Surface       Grab
    ## 706                         Surface       Grab
    ## 707                         Surface       Grab
    ## 708                         Surface       Grab
    ## 709                         Surface       Grab
    ## 710                         Surface       Grab
    ## 711                         Surface       Grab
    ## 712                         Surface       Grab
    ## 713                         Surface       Grab
    ## 714                         Surface       Grab
    ## 715                         Surface       Grab
    ## 716                         Surface       Grab
    ## 717                         Surface       Grab
    ## 718                         Surface       Grab
    ## 719                         Surface       Grab
    ## 720                         Surface       Grab
    ## 721                         Surface       Grab
    ## 722                         Surface       Grab
    ## 723                         Surface       Grab
    ## 724                         Surface       Grab
    ## 725                         Surface       Grab
    ## 726                         Surface       Grab
    ## 727                         Surface       Grab
    ## 728                         Surface       Grab
    ## 729                         Surface       Grab
    ## 730                         Surface       Grab
    ## 731                         Surface       Grab
    ## 732                         Surface       Grab
    ## 733                         Surface       Grab
    ## 734                         Surface       Grab
    ## 735                         Surface       Grab
    ## 736                         Surface       Grab
    ## 737                         Surface       Grab
    ## 738                         Surface       Grab
    ## 739                         Surface       Grab
    ## 740                         Surface       Grab
    ## 741                         Surface       Grab
    ## 742                         Surface       Grab
    ## 743                         Surface       Grab
    ## 744                         Surface       Grab
    ## 745                         Surface       Grab
    ## 746                         Surface       Grab
    ## 747                         Surface       Grab
    ## 748                         Surface       Grab
    ## 749                         Surface       Grab
    ## 750                         Surface       Grab
    ## 751                         Surface       Grab
    ## 752                         Surface       Grab
    ## 753                         Surface       Grab
    ## 754                         Surface       Grab
    ## 755                         Surface       Grab
    ## 756                         Surface       Grab
    ## 757                         Surface       Grab
    ## 758                         Surface       Grab
    ## 759                         Surface       Grab
    ## 760                         Surface       Grab
    ## 761                         Surface       Grab
    ## 762                         Surface       Grab
    ## 763                         Surface       Grab
    ## 764                         Surface       Grab
    ## 765                         Surface       Grab
    ## 766                         Surface       Grab
    ## 767                         Surface       Grab
    ## 768                         Surface       Grab
    ## 769                         Surface       Grab
    ## 770                         Surface       Grab
    ## 771                         Surface       Grab
    ## 772                         Surface       Grab
    ## 773                         Surface       Grab
    ## 774                         Surface       Grab
    ## 775                         Surface       Grab
    ## 776                         Surface       Grab
    ## 777                         Surface       Grab
    ## 778                         Surface       Grab
    ## 779                         Surface       Grab
    ## 780                         Surface       Grab
    ## 781                         Surface       Grab
    ## 782                         Surface       Grab
    ## 783                         Surface       Grab
    ## 784                         Surface       Grab
    ## 785                         Surface       Grab
    ## 786                         Surface       Grab
    ## 787                         Surface       Grab
    ## 788                         Surface       Grab
    ## 789                         Surface       Grab
    ## 790                         Surface       Grab
    ## 791                         Surface       Grab
    ## 792                         Surface       Grab
    ## 793                         Surface       Grab
    ## 794                         Surface       Grab
    ## 795                         Surface       Grab
    ## 796                         Surface       Grab
    ## 797                         Surface       Grab
    ## 798                         Surface       Grab
    ## 799                         Surface       Grab
    ## 800                         Surface       Grab
    ## 801                         Surface       Grab
    ## 802                         Surface       Grab
    ## 803                         Surface       Grab
    ## 804                         Surface       Grab
    ## 805                         Surface       Grab
    ## 806                         Surface       Grab
    ## 807                         Surface       Grab
    ## 808                         Surface       Grab
    ## 809                         Surface       Grab
    ## 810                         Surface       Grab
    ## 811                         Surface       Grab
    ## 812                         Surface       Grab
    ## 813                         Surface       Grab
    ## 814                         Surface       Grab
    ## 815                         Surface       Grab
    ## 816                         Surface       Grab
    ## 817                         Surface       Grab
    ## 818                         Surface       Grab
    ## 819                         Surface       Grab
    ## 820                         Surface       Grab
    ## 821                         Surface       Grab
    ## 822                         Surface       Grab
    ## 823                         Surface       Grab
    ## 824                         Surface       Grab
    ## 825                         Surface       Grab
    ## 826                         Surface       Grab
    ## 827                         Surface       Grab
    ## 828                         Surface       Grab
    ## 829                         Surface       Grab
    ## 830                         Surface       Grab
    ## 831                         Surface       Grab
    ## 832                         Surface       Grab
    ## 833                         Surface       Grab
    ## 834                         Surface       Grab
    ## 835                         Surface       Grab
    ## 836                         Surface       Grab
    ## 837                         Surface       Grab
    ## 838                         Surface       Grab
    ## 839                         Surface       Grab
    ## 840                         Surface       Grab
    ## 841                           Depth       Core
    ## 842                           depth       Core
    ## 843                           depth       Core
    ## 844                           depth       Core
    ## 845                           depth       Core
    ## 846                           depth       Core
    ## 847                           depth       Core
    ## 848                           depth       Core
    ## 849                           depth       Core
    ## 850                           depth       Core
    ## 851                           depth       Core
    ## 852                           depth       Core
    ## 853                           depth       Core
    ## 854                           depth       Core
    ## 855                           depth       Core
    ## 856                           depth       Core
    ## 857                           depth       Core
    ## 858                           depth       Core
    ## 859                         Unknown       Core
    ## 860                         Unknown       Core
    ## 861                         Unknown       Core
    ## 862                         Unknown       Core
    ## 863                         Unknown       Core
    ## 864                         Unknown       Core
    ## 865                         Unknown       Core
    ## 866                         Unknown       Core
    ## 867                         Unknown       Core
    ## 868                         Unknown       Core
    ## 869                         Unknown       Core
    ## 870                         Unknown       Core
    ## 871                         Unknown       Core
    ## 872                         Unknown       Core
    ## 873                         Unknown       Core
    ## 874                         Unknown       Core
    ## 875                         Unknown       Core
    ## 876                         Unknown       Core
    ## 877                         Unknown       Core
    ## 878                           Depth       Grab
    ## 879                           Depth       Grab
    ## 880                           Depth       Grab
    ## 881                           Depth       Core
    ## 882                           Depth       Core
    ## 883                         Surface       Grab
    ## 884                         Surface       Grab
    ## 885                         Surface       Grab
    ## 886                         Surface       Grab
    ## 887                         Surface       Grab
    ## 888                         Surface       Grab
    ## 889                           depth       Grab
    ## 890                           depth       Grab
    ## 891                           depth       Grab
    ## 892                           depth       Grab
    ## 893                           depth       Grab
    ## 894                           depth       Grab
    ## 895                           depth       Grab
    ## 896                           depth       Grab
    ## 897                           depth       Grab
    ## 898                           depth       Grab
    ## 899                           depth       Grab
    ## 900                           depth       Grab
    ## 901                           depth       Grab
    ## 902                           depth       Grab
    ## 903                           depth       Grab
    ## 904                           depth       Grab
    ## 905                           depth       Grab
    ## 906                           depth       Grab
    ## 907                           depth       Grab
    ## 908                           depth       Grab
    ## 909                         Unknown       Grab
    ## 910                         Unknown       Grab
    ## 911                         Unknown       Grab
    ## 912                           Depth       Grab
    ## 913                           Depth       Grab
    ## 914                           Depth       Grab
    ## 915                         Surface       Grab
    ## 916                         Surface       Grab
    ## 917                         Surface       Grab
    ## 918                         Surface       Grab
    ## 919                         Surface       Grab
    ## 920                           Depth       Grab
    ## 921                         Surface       Grab
    ## 922                         Surface       Grab
    ## 923                         Surface       Grab
    ## 924                         Surface       Grab
    ## 925                         Surface       Grab
    ## 926                         Surface       Grab
    ## 927                         Surface       Grab
    ## 928                         Surface       Grab
    ##                                                                                          site
    ## 1                                                                                         BIH
    ## 2                                                                                         BIH
    ## 3                                                                                         BIH
    ## 4                                                                                         BIH
    ## 5                                                                                         BIH
    ## 6                                                                                         BIH
    ## 7                                                                                         BIH
    ## 8                                                                                        MBDS
    ## 9                                                                                        MBDS
    ## 10                                                                                       MBDS
    ## 11                                                                                       MBDS
    ## 12                                                                                       MBDS
    ## 13                                                                                       MBDS
    ## 14                                                                                       MBDS
    ## 15                                                                                       MBDS
    ## 16                                                                                       MBDS
    ## 17                                                                                       MBDS
    ## 18                                                                                       MBDS
    ## 19                                                                                       MBDS
    ## 20  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 21  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 22  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 23  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 24  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 25  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 26  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 27  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 28  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 29  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 30  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 31  Reserve Channel, Main Shipping Channel, Boston Harbor to the Western shore of Deer Island
    ## 32                                                                             DORCHESTER BAY
    ## 33                                                                        CRYSTAL COVE MARINA
    ## 34                                                                        CRYSTAL COVE MARINA
    ## 35                                                                                 BASS RIVER
    ## 36                                                                                 BASS RIVER
    ## 37                                                                                 BASS RIVER
    ## 38                                                                              HINGAM HARBOR
    ## 39                                                                             HINGHAM HARBOR
    ## 40                                                                             HINGHAM HARBOR
    ## 41                                                                               Mystic River
    ## 42                                                                               Mystic River
    ## 43                                                                               Mystic River
    ## 44                                                                               Mystic River
    ## 45                                                                                 SMITH COVE
    ## 46                                                                                 SMITH COVE
    ## 47                                                                                 SMITH COVE
    ## 48                                                                                 SMITH COVE
    ## 49                                                                                 SMITH COVE
    ## 50                                                                                 SMITH COVE
    ## 51                                                                                 SMITH COVE
    ## 52                                                                                 SMITH COVE
    ## 53                                                                                 SMITH COVE
    ## 54                                                                                 SMITH COVE
    ## 55                                                                            WINTHROP HARBOR
    ## 56                                                                            WINTHROP HARBOR
    ## 57                                                                            WINTHROP HARBOR
    ## 58                                                                            WINTHROP HARBOR
    ## 59                                                                            WINTHROP HARBOR
    ## 60                                                                            WINTHROP HARBOR
    ## 61                                                                            WINTHROP HARBOR
    ## 62                                                                            WINTHROP HARBOR
    ## 63                                                                             WINTROP HARBOR
    ## 64                                                                            WINTHROP HARBOR
    ## 65                                                                            WINTHROP HARBOR
    ## 66                                                                            WINTHROP HARBOR
    ## 67                                                                            WINTHROP HARBOR
    ## 68                                                                            WINTHROP HARBOR
    ## 69                                                                            WINTHROP HARBOR
    ## 70                                                                            WINTHROP HARBOR
    ## 71                                                                            WINTHROP HARBOR
    ## 72                                                                            WINTHROP HARBOR
    ## 73                                                                            WINTHROP HARBOR
    ## 74                                                                            WINTHROP HARBOR
    ## 75                                                                            WINTHROP HARBOR
    ## 76                                                                            WINTHROP HARBOR
    ## 77                                                                            WINTHROP HARBOR
    ## 78                                                                            WINTHROP HARBOR
    ## 79                                                                            WINTHROP HARBOR
    ## 80                                                                      Little Mystic Channel
    ## 81                                                                          MANCHESTER HARBOR
    ## 82                                                                          MANCHESTER HARBOR
    ## 83                                                                          MANCHESTER HARBOR
    ## 84                                                                          MANCHESTER HARBOR
    ## 85                                                                            SCITUATE HARBOR
    ## 86                                                                        Weymouth Fore River
    ## 87                                                                        Weymouth Fore River
    ## 88                                                                        Weymouth Fore River
    ## 89                                                                        Weymouth Fore River
    ## 90                                                                             HINGHAM HARBOR
    ## 91                                                                               SALEM HARBOR
    ## 92                                                                       BOSTON HARBOR MARINA
    ## 93                                                                               Mystic River
    ## 94                                                                               Mystic River
    ## 95                                                                               Mystic River
    ## 96                                                                            SCITUATE HARBOR
    ## 97                                                                            SCITUATE HARBOR
    ## 98                                                                            SCITUATE HARBOR
    ## 99                                                                            SCITUATE HARBOR
    ## 100                                                                         GLOUCESTER HARBOR
    ## 101                                                                             BOSTON HARBOR
    ## 102                                                                             BOSTON HARBOR
    ## 103                                                                             BOSTON HARBOR
    ## 104                                                                             BOSTON HARBOR
    ## 105                                                                               DUXBURY BAY
    ## 106                                                                            DORCHESTER BAY
    ## 107                                                                            DORCHESTER BAY
    ## 108                                                                            DORCHESTER BAY
    ## 109                                                                            DORCHESTER BAY
    ## 110                                                                            DORCHESTER BAY
    ## 111                                                                            DORCHESTER BAY
    ## 112                                                                            DORCHESTER BAY
    ## 113                                                                            BEVERLY HARBOR
    ## 114                                                                   PORT NORFOLK YACHT CLUB
    ## 115                                                                   PORT NORFOLK YACHT CLUB
    ## 116                                                                   PORT NORFOLK YACHT CLUB
    ## 117                                                                   PORT NORFOLK YACHT CLUB
    ## 118                                                                             Chelsea River
    ## 119                                                                             Chelsea River
    ## 120                                                                            Dorchester Bay
    ## 121                                                                   Rowes and Fosters Wharf
    ## 122                                                                   Rowes and Fosters Wharf
    ## 123                                                                   Rowes and Fosters Wharf
    ## 124                                                                                Mill Creek
    ## 125                                                                                Mill Creek
    ## 126                                                                                Mill Creek
    ## 127                                                                                Mill Creek
    ## 128                                                                                Mill Creek
    ## 129                                                                                Mill Creek
    ## 130                                                                                Mill Creek
    ## 131                                                                                Mill Creek
    ## 132                                                                                Mill Creek
    ## 133                                                                                Mill Creek
    ## 134                                                                                Mill Creek
    ## 135                                                                                Mill Creek
    ## 136                                                                                Mill Creek
    ## 137                                                                                Mill Creek
    ## 138                                                                                Mill Creek
    ## 139                                                                                Mill Creek
    ## 140                                                                                Mill Creek
    ## 141                                                                                Mill Creek
    ## 142                                                                                Mill Creek
    ## 143                                                                                Mill Creek
    ## 144                                                                                Mill Creek
    ## 145                                                                                Mill Creek
    ## 146                                                                                Mill Creek
    ## 147                                                                                Mill Creek
    ## 148                                                                                Mill Creek
    ## 149                                                                                Mill Creek
    ## 150                                                                                Mill Creek
    ## 151                                                                                Mill Creek
    ## 152                                                                                Mill Creek
    ## 153                                                                                Mill Creek
    ## 154                                                                                Mill Creek
    ## 155                                                                                Mill Creek
    ## 156                                                                                Mill Creek
    ## 157                                                                                Mill Creek
    ## 158                                                                                Mill Creek
    ## 159                                                                                Mill Creek
    ## 160                                                                                Mill Creek
    ## 161                                                           Victory Road Park Inlet Channel
    ## 162                                                                              Mystic River
    ## 163                                                                Seaward of Waterfront Park
    ## 164                                                                                Long Wharf
    ## 165                                                                               South River
    ## 166                                                                               South River
    ## 167                                                                           Winthrop Harbor
    ## 168                                                                           Winthrop Harbor
    ## 169                                                         Winthrop Harbor, entrance channel
    ## 170                                                                     North & Danvers River
    ## 171                                                                     North & Danvers River
    ## 172                                                                     North & Danvers River
    ## 173                                                                     North & Danvers River
    ## 174                                                                     North & Danvers River
    ## 175                                                                     North & Danvers River
    ## 176                                                                     North & Danvers River
    ## 177                                                                     North & Danvers River
    ## 178                                                                     North & Danvers River
    ## 179                                                                     North & Danvers River
    ## 180                                                                     North & Danvers River
    ## 181                                                                              Mystic River
    ## 182                                                                             Chelsea River
    ## 183                                                                             Chelsea River
    ## 184                                                                         Reserve Channel B
    ## 185                                                                      Inner Confluence "B"
    ## 186                                                                              Mystic River
    ## 187                                                                              Mystic River
    ## 188                                                                             Chelsea River
    ## 189                                                                             Chelsea River
    ## 190                                                                          Reserve "B"-7826
    ## 191                                                                          Reserve "D"-7827
    ## 192                                                              FADS-Reference location-7832
    ## 193                                                                              Gulf Oil Co.
    ## 194                                                                              Gibb Oil ???
    ## 195                                                                      Gibb Oil North Berth
    ## 196                                                                      Gibb Oil South Berth
    ## 197                                                                      ESE of Castle Island
    ## 198                                                                      ESE of Castle Island
    ## 199                                                                      ESE of Castle Island
    ## 200                                                                      ESE of Castle Island
    ## 201                                                                      ESE of Castle Island
    ## 202                                                                      ESE of Castle Island
    ## 203                                                         btwn Deer I. & Governors I. Flats
    ## 204                                                         btwn Deer I. & Governors I. Flats
    ## 205                                                         btwn Deer I. & Governors I. Flats
    ## 206                                                         btwn Deer I. & Governors I. Flats
    ## 207                                                         btwn Deer I. & Governors I. Flats
    ## 208                                                         btwn Deer I. & Governors I. Flats
    ## 209                                                                                Quincy Bay
    ## 210                                                                                Quincy Bay
    ## 211                                                                                Quincy Bay
    ## 212                                                                                Quincy Bay
    ## 213                                                                                Quincy Bay
    ## 214                                                                                Quincy Bay
    ## 215                                                           Nantasket Roads W of Perry Cove
    ## 216                                                           Nantasket Roads W of Perry Cove
    ## 217                                                           Nantasket Roads W of Perry Cove
    ## 218                                                           Nantasket Roads W of Perry Cove
    ## 219                                                           Nantasket Roads W of Perry Cove
    ## 220                                                           Nantasket Roads W of Perry Cove
    ## 221                                                                                  Hull Bay
    ## 222                                                                                  Hull Bay
    ## 223                                                                                  Hull Bay
    ## 224                                                                                  Hull Bay
    ## 225                                                                                  Hull Bay
    ## 226                                                                                  Hull Bay
    ## 227                                                               SE of The Graves, Mass. Bay
    ## 228                                                               SE of The Graves, Mass. Bay
    ## 229                                                               SE of The Graves, Mass. Bay
    ## 230                                                               SE of The Graves, Mass. Bay
    ## 231                                                                         Massachusetts Bay
    ## 232                                                                         Massachusetts Bay
    ## 233                                                                         Massachusetts Bay
    ## 234                                                                         Massachusetts Bay
    ## 235                                                                         Massachusetts Bay
    ## 236                                                                         Massachusetts Bay
    ## 237                                                                         Massachusetts Bay
    ## 238                                                                         Massachusetts Bay
    ## 239                                                                         Massachusetts Bay
    ## 240                                                                         Massachusetts Bay
    ## 241                                                                         Massachusetts Bay
    ## 242                                                                         Massachusetts Bay
    ## 243                                                                         Massachusetts Bay
    ## 244                                                                         Massachusetts Bay
    ## 245                                                                         Massachusetts Bay
    ## 246                                                                         Massachusetts Bay
    ## 247                                                                         Massachusetts Bay
    ## 248                                                                         Massachusetts Bay
    ## 249                                                                         Massachusetts Bay
    ## 250                                                                         Massachusetts Bay
    ## 251                                                                         Massachusetts Bay
    ## 252                                                                         Massachusetts Bay
    ## 253                                                                         Massachusetts Bay
    ## 254                                                                         Massachusetts Bay
    ## 255                                                                         Massachusetts Bay
    ## 256                                                                         Massachusetts Bay
    ## 257                                                                         Massachusetts Bay
    ## 258                                                                         Massachusetts Bay
    ## 259                                                                         Massachusetts Bay
    ## 260                                                                         Massachusetts Bay
    ## 261                                                                         Massachusetts Bay
    ## 262                                                                         Massachusetts Bay
    ## 263                                                                         Massachusetts Bay
    ## 264                                                                         Massachusetts Bay
    ## 265                                                                         Massachusetts Bay
    ## 266                                                                         Massachusetts Bay
    ## 267                                                                         Massachusetts Bay
    ## 268                                                                         Massachusetts Bay
    ## 269                                                                         Massachusetts Bay
    ## 270                                                                         Massachusetts Bay
    ## 271                                                                         Massachusetts Bay
    ## 272                                                                         Massachusetts Bay
    ## 273                                                                         Massachusetts Bay
    ## 274                                                                         Massachusetts Bay
    ## 275                                                                         Massachusetts Bay
    ## 276                                                                         Massachusetts Bay
    ## 277                                                                         Massachusetts Bay
    ## 278                                                                         Massachusetts Bay
    ## 279                                                                         Massachusetts Bay
    ## 280                                                                         Massachusetts Bay
    ## 281                                                                              Cape Cod Bay
    ## 282                                                                              Cape Cod Bay
    ## 283                                                                              Cape Cod Bay
    ## 284                                                                              Cape Cod Bay
    ## 285                                                                              Cape Cod Bay
    ## 286                                                                              Cape Cod Bay
    ## 287                                                                              Cape Cod Bay
    ## 288                                                                              Cape Cod Bay
    ## 289                                                                              Cape Cod Bay
    ## 290                                                                              Cape Cod Bay
    ## 291                                                                              Cape Cod Bay
    ## 292                                                                              Cape Cod Bay
    ## 293                                                            HbrView Marina,Town Rvr Quincy
    ## 294                                                                                 Foul Area
    ## 295                                                           Marina Bay, Squantum Pt, Quincy
    ## 296                                                           Marina Bay, Squantum Pt, Quincy
    ## 297                                                           Marina Bay, Squantum Pt, Quincy
    ## 298                                                           Marina Bay, Squantum Pt, Quincy
    ## 299                                                                     Little Mystic Channel
    ## 300                                                                          Island End River
    ## 301                                                                          Island End River
    ## 302                                                                          Island End River
    ## 303                                                                              SALEM HARBOR
    ## 304                                                                                NUT ISLAND
    ## 305                                                                                NUT ISLAND
    ## 306                                                                                NUT ISLAND
    ## 307                                                                                NUT ISLAND
    ## 308                                                                                NUT ISLAND
    ## 309                                                                                NUT ISLAND
    ## 310                                                                                NUT ISLAND
    ## 311                                                                                NUT ISLAND
    ## 312                                                                               DEER ISLAND
    ## 313                                                                               DEER ISLAND
    ## 314                                                                               DEER ISLAND
    ## 315                                                                               DEER ISLAND
    ## 316                                                                               DEER ISLAND
    ## 317                                                                               DEER ISLAND
    ## 318                                                                               DEER ISLAND
    ## 319                                                                               DEER ISLAND
    ## 320                                                                               DEER ISLAND
    ## 321                                                                               DEER ISLAND
    ## 322                                                                               DEER ISLAND
    ## 323                                                                               DEER ISLAND
    ## 324                                                                               DEER ISLAND
    ## 325                                                                           Cohasset Harbor
    ## 326                                                                           Cohasset Harbor
    ## 327                                                                           Cohasset Harbor
    ## 328                                                                           Cohasset Harbor
    ## 329                                                                           Cohasset Harbor
    ## 330                                                                           Cohasset Harbor
    ## 331                                                                           Scituate Harbor
    ## 332                                                                           Scituate Harbor
    ## 333                                                                           Scituate Harbor
    ## 334                                                                           Scituate Harbor
    ## 335                                                                                       NAR
    ## 336                                                                                       NAR
    ## 337                                                                                       NAR
    ## 338                                                                                       NAR
    ## 339                                                                                       NAR
    ## 340                                                                                       NAR
    ## 341                                                                                       NAR
    ## 342                                                                                       NAR
    ## 343                                                                                       NAR
    ## 344                                                                                       NAR
    ## 345                                                                                       NAR
    ## 346                                                                                       NAR
    ## 347                                                                                       NAR
    ## 348                                                                                       NAR
    ## 349                                                                                       NAR
    ## 350                                                                                       NAR
    ## 351                                                                                       NAR
    ## 352                                                                                       NAR
    ## 353                                                                                       NAR
    ## 354                                                                                       NAR
    ## 355                                                                                       NAR
    ## 356                                                                                       DOB
    ## 357                                                                                       DOB
    ## 358                                                                                       DOB
    ## 359                                                                                       DOB
    ## 360                                                                                       DOB
    ## 361                                                                                       DOB
    ## 362                                                                                       DOB
    ## 363                                                                                       DOB
    ## 364                                                                                       DOB
    ## 365                                                                                       DOB
    ## 366                                                                                       DOB
    ## 367                                                                                       DOB
    ## 368                                                                                       DOB
    ## 369                                                                                       DOB
    ## 370                                                                                       BOI
    ## 371                                                                                       BOI
    ## 372                                                                                       BOI
    ## 373                                                                                       BOI
    ## 374                                                                                       BOI
    ## 375                                                                                       BOI
    ## 376                                                                                       BOI
    ## 377                                                                                       PRR
    ## 378                                                                                       PRR
    ## 379                                                                                       PRR
    ## 380                                                                                       PRR
    ## 381                                                                                       PRR
    ## 382                                                                                       PRR
    ## 383                                                                                       PRR
    ## 384                                                                                       PRR
    ## 385                                                                                       PRR
    ## 386                                                                                       PRR
    ## 387                                                                                       PRR
    ## 388                                                                                       PRR
    ## 389                                                                                       PRR
    ## 390                                                                                       PRR
    ## 391                                                                                       BIH
    ## 392                                                                                       BIH
    ## 393                                                                                       LDF
    ## 394                                                                                       LDF
    ## 395                                                                                       LDF
    ## 396                                                                                       LDF
    ## 397                                                                                       LDF
    ## 398                                                                                       LDF
    ## 399                                                                                       LDF
    ## 400                                                                                       PRR
    ## 401                                                                                       PRR
    ## 402                                                                                       PRR
    ## 403                                                                                       PRR
    ## 404                                                                                       PRR
    ## 405                                                                                       PRR
    ## 406                                                                                       PRR
    ## 407                                                                                       BRS
    ## 408                                                                                       BRS
    ## 409                                                                                       BRS
    ## 410                                                                                       BRS
    ## 411                                                                                       BRS
    ## 412                                                                                       BRS
    ## 413                                                                                       BRS
    ## 414                                                                                       MAB
    ## 415                                                                                       MAB
    ## 416                                                                                       MAB
    ## 417                                                                                       MAB
    ## 418                                                                                       MAB
    ## 419                                                                                       MAB
    ## 420                                                                                       MAB
    ## 421                                                                                       BIH
    ## 422                                                                                       BIH
    ## 423                                                                                       BIH
    ## 424                                                                                       BIH
    ## 425                                                                                       BIH
    ## 426                                                                                      <NA>
    ## 427                                                                                      <NA>
    ## 428                                                                                      <NA>
    ## 429                                                                                      <NA>
    ## 430                                                                               Broad Sound
    ## 431                                                                               Broad Sound
    ## 432                                                                                      <NA>
    ## 433                                                                                      <NA>
    ## 434                                                                                      <NA>
    ## 435                                                                                      <NA>
    ## 436                                                                                      <NA>
    ## 437                                                                                      <NA>
    ## 438                                                                                      <NA>
    ## 439                                                                                      <NA>
    ## 440                                                                                      <NA>
    ## 441                                                                                      <NA>
    ## 442                                                                                      <NA>
    ## 443                                                                                      <NA>
    ## 444                                                                                      <NA>
    ## 445                                                                                      <NA>
    ## 446                                                                                      <NA>
    ## 447                                                                                      <NA>
    ## 448                                                                                      <NA>
    ## 449                                                                                      <NA>
    ## 450                                                                                      <NA>
    ## 451                                                                                      <NA>
    ## 452                                                                                      <NA>
    ## 453                                                                                      <NA>
    ## 454                                                                                      <NA>
    ## 455                                                                                      <NA>
    ## 456                                                                                      <NA>
    ## 457                                                                                      <NA>
    ## 458                                                                                      <NA>
    ## 459                                                                                      <NA>
    ## 460                                                                                      <NA>
    ## 461                                                                                      <NA>
    ## 462                                                                                      <NA>
    ## 463                                                                                      <NA>
    ## 464                                                                                      <NA>
    ## 465                                                                                      <NA>
    ## 466                                                                                      <NA>
    ## 467                                                                                      <NA>
    ## 468                                                                                      <NA>
    ## 469                                                                                      <NA>
    ## 470                                                                                      <NA>
    ## 471                                                                                      <NA>
    ## 472                                                                                      <NA>
    ## 473                                                                                      <NA>
    ## 474                                                                                      <NA>
    ## 475                                                                                      <NA>
    ## 476                                                                                      <NA>
    ## 477                                                                                      <NA>
    ## 478                                                                                      <NA>
    ## 479                                                                                      <NA>
    ## 480                                                                                      <NA>
    ## 481                                                                                      <NA>
    ## 482                                                                                      <NA>
    ## 483                                                                                      <NA>
    ## 484                                                                                      <NA>
    ## 485                                                                                      <NA>
    ## 486                                                                                      <NA>
    ## 487                                                                                      <NA>
    ## 488                                                                                      <NA>
    ## 489                                                                                      <NA>
    ## 490                                                                                      <NA>
    ## 491                                                                                      <NA>
    ## 492                                                                                      <NA>
    ## 493                                                                                      <NA>
    ## 494                                                                                      <NA>
    ## 495                                                                                      <NA>
    ## 496                                                                                      <NA>
    ## 497                                                                                      <NA>
    ## 498                                                                                      <NA>
    ## 499                                                                                      <NA>
    ## 500                                                                                      <NA>
    ## 501                                                                                      <NA>
    ## 502                                                                                      <NA>
    ## 503                                                                                      <NA>
    ## 504                                                                                      <NA>
    ## 505                                                                                      <NA>
    ## 506                                                                                      <NA>
    ## 507                                                                                      <NA>
    ## 508                                                                                      <NA>
    ## 509                                                                                      <NA>
    ## 510                                                                                      <NA>
    ## 511                                                                                      <NA>
    ## 512                                                                                      <NA>
    ## 513                                                                                      <NA>
    ## 514                                                                                      <NA>
    ## 515                                                                                      <NA>
    ## 516                                                                                      <NA>
    ## 517                                                                                      <NA>
    ## 518                                                                                      <NA>
    ## 519                                                                                      <NA>
    ## 520                                                                                      <NA>
    ## 521                                                                                      <NA>
    ## 522                                                                                      <NA>
    ## 523                                                                                      <NA>
    ## 524                                                                                      <NA>
    ## 525                                                                                      <NA>
    ## 526                                                                                      <NA>
    ## 527                                                                                      <NA>
    ## 528                                                                                      <NA>
    ## 529                                                                                      <NA>
    ## 530                                                                                      <NA>
    ## 531                                                                                      <NA>
    ## 532                                                                                      <NA>
    ## 533                                                                                      <NA>
    ## 534                                                                                      <NA>
    ## 535                                                                                      <NA>
    ## 536                                                                                      <NA>
    ## 537                                                                                      <NA>
    ## 538                                                                                      <NA>
    ## 539                                                                                      <NA>
    ## 540                                                                                      <NA>
    ## 541                                                                                      <NA>
    ## 542                                                                                      <NA>
    ## 543                                                                                      <NA>
    ## 544                                                                                      <NA>
    ## 545                                                                                      <NA>
    ## 546                                                                                      <NA>
    ## 547                                                                               Salem Sound
    ## 548                                                                               Salem Sound
    ## 549                                                                               Salem Sound
    ## 550                                                                               Salem Sound
    ## 551                                                                               Salem Sound
    ## 552                                                                               Salem Sound
    ## 553                                                                               Salem Sound
    ## 554                                                                               Broad Sound
    ## 555                                                                               Broad Sound
    ## 556                                                                           Foul area north
    ## 557                                                                           Foul area north
    ## 558                                                                           Foul area north
    ## 559                                                                          Foul area center
    ## 560                                                                          Foul area center
    ## 561                                                                          Foul area center
    ## 562                                                                           Foul area south
    ## 563                                                                           Foul area south
    ## 564                                                                            Foul area east
    ## 565                                                                            Foul area east
    ## 566                                                                            Foul area west
    ## 567                                                                            Foul area west
    ## 568                                                                            Foul area west
    ## 569                                                                        south of Foul area
    ## 570                                                                        south of Foul area
    ## 571                                                                        south of Foul area
    ## 572                                                                                       BIH
    ## 573                                                                                       BIH
    ## 574                                                                                       BIH
    ## 575                                                                                       BIH
    ## 576                                                                                       BIH
    ## 577                                                                                       BIH
    ## 578                                                                                       BIH
    ## 579                                                                                       BIH
    ## 580                                                                              Spec. Island
    ## 581                                                                              Spec. Island
    ## 582                                                                              Spec. Island
    ## 583                                                                              Spec. Island
    ## 584                                                                              Spec. Island
    ## 585                                                                              Spec. Island
    ## 586                                                                              Spec. Island
    ## 587                                                                              Spec. Island
    ## 588                                                                              Spec. Island
    ## 589                                                                              Spec. Island
    ## 590                                                                              Spec. Island
    ## 591                                                                              Spec. Island
    ## 592                                                                              Spec. Island
    ## 593                                                                              Spec. Island
    ## 594                                                                              Spec. Island
    ## 595                                                                              Spec. Island
    ## 596                                                                              Spec. Island
    ## 597                                                                              Spec. Island
    ## 598                                                                              Spec. Island
    ## 599                                                                                       DOB
    ## 600                                                                                       DOB
    ## 601                                                                          Third Hbr Tunnel
    ## 602                                                                          Third Hbr Tunnel
    ## 603                                                                          Third Hbr Tunnel
    ## 604                                                                          Third Hbr Tunnel
    ## 605                                                                          Third Hbr Tunnel
    ## 606                                                                          Third Hbr Tunnel
    ## 607                                                                          Third Hbr Tunnel
    ## 608                                                                          Third Hbr Tunnel
    ## 609                                                                          Third Hbr Tunnel
    ## 610                                                                          Third Hbr Tunnel
    ## 611                                                                          Third Hbr Tunnel
    ## 612                                                                           MERRIMACK RIVER
    ## 613                                                                                       QUB
    ## 614                                                                                       DOB
    ## 615                                                                                       QUB
    ## 616                                                                                       DOB
    ## 617                                                                                       BOI
    ## 618                                                                                       PRR
    ## 619                                                                                       PRR
    ## 620                                                                                       BIH
    ## 621                                                                                       LDF
    ## 622                                                                                       BIH
    ## 623                                                                                       BIH
    ## 624                                                                                       BIH
    ## 625                                                                          Island End River
    ## 626                                                                        Fort Point Channel
    ## 627                                                                                       BIH
    ## 628                                                                                       BIH
    ## 629                                                                                       BIH
    ## 630                                                                                       BIH
    ## 631                                                                                       BIH
    ## 632                                                                                       BIH
    ## 633                                                                                       BIH
    ## 634                                                            1-U.S. GypsumCo.200TerminalSt.
    ## 635                                                            2-U.S. GypsumCo.200TerminalSt.
    ## 636                                                            3-U.S. GypsumCo.200TerminalSt.
    ## 637                                                            4-U.S. GypsumCo.200TerminalSt.
    ## 638                                                                                      FADS
    ## 639                                                                                      FADS
    ## 640                                                                                      FADS
    ## 641                                                                                      FADS
    ## 642                                                                                      FADS
    ## 643                                                                                      FADS
    ## 644                                                                                      FADS
    ## 645                                                                                      FADS
    ## 646                                                                                      FADS
    ## 647                                                                                      FADS
    ## 648                                                                                      FADS
    ## 649                                                                                      FADS
    ## 650                                                                                      FADS
    ## 651                                                                                      FADS
    ## 652                                                                                       BIH
    ## 653                                                                                       BIH
    ## 654                                                                                       BIH
    ## 655                                                                         JEFFRIES POINT YC
    ## 656                                                                         JEFFRIES POINT YC
    ## 657                                                                         JEFFRIES POINT YC
    ## 658                                                                         JEFFRIES POINT YC
    ## 659                                                                                      MBDS
    ## 660                                                                                      MBDS
    ## 661                                                                                      MBDS
    ## 662                                                                                      MBDS
    ## 663                                                                                      MBDS
    ## 664                                                                                      MBDS
    ## 665                                                                                      MBDS
    ## 666                                                                                      MBDS
    ## 667                                                                                      MBDS
    ## 668                                                                                       BIH
    ## 669                                                                      MBDS Reference sites
    ## 670                                                                      MBDS Reference sites
    ## 671                                                                       STFP outfall siting
    ## 672                                                                       STFP outfall siting
    ## 673                                                                       STFP outfall siting
    ## 674                                                                       STFP outfall siting
    ## 675                                                                       STFP outfall siting
    ## 676                                                                       STFP outfall siting
    ## 677                                                                       STFP outfall siting
    ## 678                                                                       STFP outfall siting
    ## 679                                                                       STFP outfall siting
    ## 680                                                                       STFP outfall siting
    ## 681                                                                       STFP outfall siting
    ## 682                                                                       STFP outfall siting
    ## 683                                                                       STFP outfall siting
    ## 684                                                                       STFP outfall siting
    ## 685                                                                       STFP outfall siting
    ## 686                                                                       STFP outfall siting
    ## 687                                                                       STFP outfall siting
    ## 688                                                                       STFP outfall siting
    ## 689                                                                       STFP outfall siting
    ## 690                                                                       STFP outfall siting
    ## 691                                                                       STFP outfall siting
    ## 692                                                                       STFP outfall siting
    ## 693                                                                       STFP outfall siting
    ## 694                                                                       STFP outfall siting
    ## 695                                                                       STFP outfall siting
    ## 696                                                                       STFP outfall siting
    ## 697                                                                       STFP outfall siting
    ## 698                                                                       STFP outfall siting
    ## 699                                                                       STFP outfall siting
    ## 700                                                                       STFP outfall siting
    ## 701                                                                       STFP outfall siting
    ## 702                                                                       STFP outfall siting
    ## 703                                                                       STFP outfall siting
    ## 704                                                                       STFP outfall siting
    ## 705                                                                       STFP outfall siting
    ## 706                                                                       STFP outfall siting
    ## 707                                                                       STFP outfall siting
    ## 708                                                                       STFP outfall siting
    ## 709                                                                       STFP outfall siting
    ## 710                                                                       STFP outfall siting
    ## 711                                                                       STFP outfall siting
    ## 712                                                                       STFP outfall siting
    ## 713                                                                       STFP outfall siting
    ## 714                                                                       STFP outfall siting
    ## 715                                                                       STFP outfall siting
    ## 716                                                                       STFP outfall siting
    ## 717                                                                       STFP outfall siting
    ## 718                                                                       STFP outfall siting
    ## 719                                                                       STFP outfall siting
    ## 720                                                                       STFP outfall siting
    ## 721                                                                       STFP outfall siting
    ## 722                                                                       STFP outfall siting
    ## 723                                                                       STFP outfall siting
    ## 724                                                                       STFP outfall siting
    ## 725                                                                       STFP outfall siting
    ## 726                                                                       STFP outfall siting
    ## 727                                                                       STFP outfall siting
    ## 728                                                                       STFP outfall siting
    ## 729                                                                       STFP outfall siting
    ## 730                                                                       STFP outfall siting
    ## 731                                                                       STFP outfall siting
    ## 732                                                                       STFP outfall siting
    ## 733                                                                       STFP outfall siting
    ## 734                                                                       STFP outfall siting
    ## 735                                                                       STFP outfall siting
    ## 736                                                                       STFP outfall siting
    ## 737                                                                       STFP outfall siting
    ## 738                                                                       STFP outfall siting
    ## 739                                                                       STFP outfall siting
    ## 740                                                                       STFP outfall siting
    ## 741                                                                       STFP outfall siting
    ## 742                                                                       STFP outfall siting
    ## 743                                                                       STFP outfall siting
    ## 744                                                                       STFP outfall siting
    ## 745                                                                       STFP outfall siting
    ## 746                                                                       STFP outfall siting
    ## 747                                                                       STFP outfall siting
    ## 748                                                                       STFP outfall siting
    ## 749                                                                       STFP outfall siting
    ## 750                                                                       STFP outfall siting
    ## 751                                                                       STFP outfall siting
    ## 752                                                                       STFP outfall siting
    ## 753                                                                       STFP outfall siting
    ## 754                                                                       STFP outfall siting
    ## 755                                                                       STFP outfall siting
    ## 756                                                                       STFP outfall siting
    ## 757                                                                       STFP outfall siting
    ## 758                                                                       STFP outfall siting
    ## 759                                                                       STFP outfall siting
    ## 760                                                                       STFP outfall siting
    ## 761                                                                       STFP outfall siting
    ## 762                                                                       STFP outfall siting
    ## 763                                                                       STFP outfall siting
    ## 764                                                                       STFP outfall siting
    ## 765                                                                       STFP outfall siting
    ## 766                                                                       STFP outfall siting
    ## 767                                                                       STFP outfall siting
    ## 768                                                                       STFP outfall siting
    ## 769                                                                       STFP outfall siting
    ## 770                                                                       STFP outfall siting
    ## 771                                                                       STFP outfall siting
    ## 772                                                                       STFP outfall siting
    ## 773                                                                       STFP outfall siting
    ## 774                                                                       STFP outfall siting
    ## 775                                                                       STFP outfall siting
    ## 776                                                                       STFP outfall siting
    ## 777                                                                       STFP outfall siting
    ## 778                                                                       STFP outfall siting
    ## 779                                                                       STFP outfall siting
    ## 780                                                                       STFP outfall siting
    ## 781                                                                       STFP outfall siting
    ## 782                                                                       STFP outfall siting
    ## 783                                                                       STFP outfall siting
    ## 784                                                                       STFP outfall siting
    ## 785                                                                       STFP outfall siting
    ## 786                                                                       STFP outfall siting
    ## 787                                                                       STFP outfall siting
    ## 788                                                                       STFP outfall siting
    ## 789                                                                       STFP outfall siting
    ## 790                                                                       STFP outfall siting
    ## 791                                                                       STFP outfall siting
    ## 792                                                                       STFP outfall siting
    ## 793                                                                       STFP outfall siting
    ## 794                                                                       STFP outfall siting
    ## 795                                                                       STFP outfall siting
    ## 796                                                                       STFP outfall siting
    ## 797                                                                       STFP outfall siting
    ## 798                                                                       STFP outfall siting
    ## 799                                                                       STFP outfall siting
    ## 800                                                                       STFP outfall siting
    ## 801                                                                         Massachusetts Bay
    ## 802                                                                                  Hull Bay
    ## 803                                                                               Hingham Bay
    ## 804                                                                               Hingham Bay
    ## 805                                                                       Weymouth Fore River
    ## 806                                                                                Quincy Bay
    ## 807                                                                                Quincy Bay
    ## 808                                                                           Nantasket Roads
    ## 809                                                                           Nantasket Roads
    ## 810                                                                           Nantasket Roads
    ## 811                                                                            Dorchester Bay
    ## 812                                                                            Dorchester Bay
    ## 813                                                                             Sculpin Ledge
    ## 814                                                                             Sculpin Ledge
    ## 815                                                                          Northwest Harbor
    ## 816                                                                             Chelsea River
    ## 817                                                                             Chelsea River
    ## 818                                                                             Chelsea River
    ## 819                                                                             Chelsea River
    ## 820                                                                             Chelsea River
    ## 821                                                                              Mystic River
    ## 822                                                                              Mystic River
    ## 823                                                                              Mystic River
    ## 824                                                                        Charleston Channel
    ## 825                                                                        Charleston Channel
    ## 826                                                                        Charleston Channel
    ## 827                                                                            Boston Channel
    ## 828                                                                             Channel Mouth
    ## 829                                                                          Reserved Channel
    ## 830                                                                            Boston Wharves
    ## 831                                                                                      FADS
    ## 832                                                                                      FADS
    ## 833                                                                                      FADS
    ## 834                                                                                      FADS
    ## 835                                                                                      FADS
    ## 836                                                                                      FADS
    ## 837                                                                                      FADS
    ## 838                                                                                      FADS
    ## 839                                                                                      FADS
    ## 840                                                                                      FADS
    ## 841                                                  South Bay area of the Fort Point Channel
    ## 842                                                  South Bay area of the Fort Point Channel
    ## 843                                                  South Bay area of the Fort Point Channel
    ## 844                                                  between Spectacle Island and Long Island
    ## 845                                                  between Spectacle Island and Long Island
    ## 846                                                  between Spectacle Island and Long Island
    ## 847                                                  between Spectacle Island and Long Island
    ## 848                                                  between Spectacle Island and Long Island
    ## 849                                                  between Spectacle Island and Long Island
    ## 850                                                  between Spectacle Island and Long Island
    ## 851                                                  between Spectacle Island and Long Island
    ## 852                                                  between Spectacle Island and Long Island
    ## 853                                                  between Spectacle Island and Long Island
    ## 854                                                  between Spectacle Island and Long Island
    ## 855                                                  between Spectacle Island and Long Island
    ## 856                                                  between Spectacle Island and Long Island
    ## 857                                                  between Spectacle Island and Long Island
    ## 858                                                  between Spectacle Island and Long Island
    ## 859                                                                                Town Brook
    ## 860                                                                                Town Brook
    ## 861                                                                                Town Brook
    ## 862                                                                                Town Brook
    ## 863                                                                                Town Brook
    ## 864                                                                                Town Brook
    ## 865                                                                                Town Brook
    ## 866                                                                                Town Brook
    ## 867                                                                                Town Brook
    ## 868                                                                                Town Brook
    ## 869                                                                                Town Brook
    ## 870                                                                                Town Brook
    ## 871                                                                                Town Brook
    ## 872                                                                                Town Brook
    ## 873                                                                                Town Brook
    ## 874                                                                                Town Brook
    ## 875                                                                                Town Brook
    ## 876                                                                                Town Brook
    ## 877                                                                                Town Brook
    ## 878                                                                            Beverly Harbor
    ## 879                                                                            Beverly Harbor
    ## 880                                                                            Beverly Harbor
    ## 881                                                         WINTHROP HARBOR, BELLE ISLE INLET
    ## 882                                                         WINTHROP HARBOR, BELLE ISLE INLET
    ## 883                                                                    LOGAN AIRPORT E.BOSTON
    ## 884                                                                    LOGAN AIRPORT E.BOSTON
    ## 885                                                                    LOGAN AIRPORT E.BOSTON
    ## 886                                                                    LOGAN AIRPORT E.BOSTON
    ## 887                                                                    LOGAN AIRPORT E.BOSTON
    ## 888                                                                    LOGAN AIRPORT E.BOSTON
    ## 889                                                              Logan Airport Runway End 22L
    ## 890                                                              Logan Airport Runway End 22L
    ## 891                                                              Logan Airport Runway End 22L
    ## 892                                                              Logan Airport Runway End 22L
    ## 893                                                              Logan Airport Runway End 22L
    ## 894                                                              Logan Airport Runway End 22L
    ## 895                                                               Logan Airport Runway End 27
    ## 896                                                               Logan Airport Runway End 27
    ## 897                                                               Logan Airport Runway End 27
    ## 898                                                               Logan Airport Runway End 27
    ## 899                                                               Logan Airport Runway End 27
    ## 900                                                              Logan Airport Runway End 33L
    ## 901                                                              Logan Airport Runway End 33L
    ## 902                                                              Logan Airport Runway End 33L
    ## 903                                                              Logan Airport Runway End 33L
    ## 904                                                              Logan Airport Runway End 33L
    ## 905                                                              Logan Airport Runway End 33L
    ## 906                                                              Logan Airport Runway End 33L
    ## 907                                                              Logan Airport Runway End 33L
    ## 908                                                              Logan Airport Runway End 33L
    ## 909                                                                           Winthrop Harbor
    ## 910                                                                           Winthrop Harbor
    ## 911                                                                           Winthrop Harbor
    ## 912                                                                  Winthrop Basin Anchorage
    ## 913                                                                  Winthrop Basin Anchorage
    ## 914                                                                Wnthrop Basin Spur Channel
    ## 915                                                                  Winthrop Basin Anchorage
    ## 916                                                    Entrance Channel Opposite Snake Island
    ## 917                                                        Entrance Channel at Basin Entrance
    ## 918                                                                    Cottage Park Anchorage
    ## 919                                                                      Cottage Park Channel
    ## 920                                                                    Cottage Park Anchorage
    ## 921                                                                    Snake Island Anchorage
    ## 922                                                                      Cottage Park Channel
    ## 923                                                                    Snake Island Anchorage
    ## 924                                                                      Cottage Park Channel
    ## 925                                                                    Crystal Cove Anchorage
    ## 926                                                                    Crystal Cove Anchorage
    ## 927                                                        Entrance Channel off Winthrop Y.C.
    ## 928                                                          Entrance Channel at Crystal Cove
    ##     REPNO_ORG TOTREP_ORG TVS_EP_PCT   O_G_PCT   O_G_UGG PHCTOT_PCT PHCTOT_UGG
    ## 1          NA         NA         NA        NA 10882.000         NA         NA
    ## 2           1         NA         NA        NA  3115.000         NA         NA
    ## 3           1         NA         NA        NA   880.000         NA         NA
    ## 4          NA         NA         NA        NA   542.300         NA         NA
    ## 5           1         NA         NA        NA    31.440         NA         NA
    ## 6           1          1       9.50        NA  7370.000         NA         NA
    ## 7           1          1       9.70        NA  8510.000         NA         NA
    ## 8          NA         NA         NA        NA        NA         NA         NA
    ## 9          NA         NA         NA        NA        NA         NA         NA
    ## 10         NA         NA         NA        NA        NA         NA         NA
    ## 11         NA         NA         NA        NA        NA         NA         NA
    ## 12         NA         NA         NA        NA        NA         NA         NA
    ## 13         NA         NA         NA        NA        NA         NA         NA
    ## 14         NA         NA         NA        NA        NA         NA         NA
    ## 15         NA         NA         NA        NA        NA         NA         NA
    ## 16         NA         NA         NA        NA        NA         NA         NA
    ## 17         NA         NA         NA        NA        NA         NA         NA
    ## 18         NA         NA         NA        NA        NA         NA         NA
    ## 19         NA         NA         NA        NA        NA         NA         NA
    ## 20         NA         NA       8.00        NA        NA         NA         NA
    ## 21         NA         NA       5.40 0.2400000        NA         NA         NA
    ## 22         NA         NA       8.30 0.2500000        NA         NA         NA
    ## 23         NA         NA       3.30 0.5500000        NA         NA         NA
    ## 24         NA         NA       4.90 0.0900000        NA         NA         NA
    ## 25         NA         NA       2.20 0.2400000        NA         NA         NA
    ## 26         NA         NA       4.80 0.0500000        NA         NA         NA
    ## 27         NA         NA       8.00 0.2600000        NA         NA         NA
    ## 28         NA         NA       3.40 0.3000000        NA         NA         NA
    ## 29         NA         NA       4.00 0.2000000        NA         NA         NA
    ## 30         NA         NA       7.50 0.1100000        NA         NA         NA
    ## 31         NA         NA       2.70 0.0800000        NA         NA         NA
    ## 32         NA         NA         NA        NA        NA         NA         NA
    ## 33         NA         NA         NA        NA        NA         NA         NA
    ## 34         NA         NA         NA        NA        NA         NA         NA
    ## 35         NA         NA         NA        NA        NA         NA         NA
    ## 36         NA         NA         NA        NA        NA         NA         NA
    ## 37         NA         NA         NA        NA        NA         NA         NA
    ## 38         NA         NA         NA        NA        NA         NA         NA
    ## 39         NA         NA         NA        NA        NA         NA         NA
    ## 40         NA         NA         NA        NA        NA         NA         NA
    ## 41         NA         NA         NA 0.0001000        NA         NA         NA
    ## 42         NA         NA         NA        NA        NA         NA         NA
    ## 43         NA         NA         NA        NA        NA         NA         NA
    ## 44         NA         NA         NA        NA        NA         NA         NA
    ## 45         NA         NA         NA        NA        NA         NA         NA
    ## 46         NA         NA         NA        NA        NA         NA         NA
    ## 47         NA         NA         NA        NA        NA         NA         NA
    ## 48         NA         NA         NA        NA        NA         NA         NA
    ## 49         NA         NA         NA        NA        NA         NA         NA
    ## 50         NA         NA         NA        NA        NA         NA         NA
    ## 51         NA         NA         NA        NA        NA         NA         NA
    ## 52         NA         NA         NA        NA        NA         NA         NA
    ## 53         NA         NA         NA        NA        NA         NA         NA
    ## 54         NA         NA         NA        NA        NA         NA         NA
    ## 55         NA         NA       1.57        NA   190.000         NA         NA
    ## 56         NA         NA       1.35        NA     0.000         NA         NA
    ## 57         NA         NA       2.51        NA   530.000         NA         NA
    ## 58         NA         NA       5.33        NA  1470.000         NA         NA
    ## 59         NA         NA       3.74        NA   890.000         NA         NA
    ## 60         NA         NA       2.54        NA     0.000         NA         NA
    ## 61         NA         NA       3.74        NA  1830.000         NA         NA
    ## 62         NA         NA       2.54        NA     0.000         NA         NA
    ## 63         NA         NA       4.92        NA   480.000         NA         NA
    ## 64         NA         NA       4.67        NA   640.000         NA         NA
    ## 65         NA         NA       3.40        NA   860.000         NA         NA
    ## 66         NA         NA       4.87        NA   740.000         NA         NA
    ## 67         NA         NA       3.85        NA     0.000         NA         NA
    ## 68         NA         NA       6.44        NA  1730.000         NA         NA
    ## 69         NA         NA       1.84        NA     0.000         NA         NA
    ## 70         NA         NA       4.50        NA  1390.000         NA         NA
    ## 71         NA         NA       1.97        NA     0.000         NA         NA
    ## 72         NA         NA       4.51        NA     0.000         NA         NA
    ## 73         NA         NA       2.84        NA   905.000         NA         NA
    ## 74         NA         NA       4.99        NA   430.000         NA         NA
    ## 75         NA         NA       3.21        NA     0.000         NA         NA
    ## 76         NA         NA       3.92        NA   490.000         NA         NA
    ## 77         NA         NA       4.75        NA     0.000         NA         NA
    ## 78         NA         NA       4.09        NA   470.000         NA         NA
    ## 79         NA         NA       2.72        NA     0.000         NA         NA
    ## 80         NA         NA         NA        NA        NA         NA         NA
    ## 81         NA         NA         NA        NA        NA         NA         NA
    ## 82         NA         NA         NA        NA        NA         NA         NA
    ## 83         NA         NA         NA        NA        NA         NA         NA
    ## 84         NA         NA         NA        NA        NA         NA         NA
    ## 85         NA         NA         NA        NA        NA         NA         NA
    ## 86         NA         NA         NA        NA        NA         NA         NA
    ## 87         NA         NA         NA        NA        NA         NA         NA
    ## 88         NA         NA         NA        NA        NA         NA         NA
    ## 89         NA         NA         NA        NA        NA         NA         NA
    ## 90         NA         NA         NA 0.1000000        NA         NA         NA
    ## 91         NA         NA         NA 0.1500000        NA         NA         NA
    ## 92         NA         NA         NA        NA        NA         NA         NA
    ## 93         NA         NA         NA        NA        NA         NA         NA
    ## 94         NA         NA         NA        NA        NA         NA         NA
    ## 95         NA         NA         NA        NA     0.000         NA         NA
    ## 96         NA         NA         NA        NA        NA         NA         NA
    ## 97         NA         NA         NA        NA        NA         NA         NA
    ## 98         NA         NA         NA        NA        NA         NA         NA
    ## 99         NA         NA         NA        NA        NA         NA         NA
    ## 100        NA         NA         NA 0.1000000        NA         NA         NA
    ## 101        NA         NA         NA 0.0658000   658.000         NA         NA
    ## 102        NA         NA         NA 0.0520000   520.000         NA         NA
    ## 103        NA         NA         NA 0.0518000   518.000         NA         NA
    ## 104        NA         NA         NA 0.3500000  3500.000         NA         NA
    ## 105        NA         NA         NA 0.0700000        NA         NA         NA
    ## 106        NA         NA         NA 0.0154000        NA         NA         NA
    ## 107        NA         NA         NA 0.0126000        NA         NA         NA
    ## 108        NA         NA         NA 0.0848000        NA         NA         NA
    ## 109        NA         NA         NA 0.0345000        NA         NA         NA
    ## 110        NA         NA         NA 0.0660000        NA         NA         NA
    ## 111        NA         NA         NA 0.0038000        NA         NA         NA
    ## 112        NA         NA         NA 0.0382000        NA         NA         NA
    ## 113        NA         NA      10.40 0.2560000        NA         NA         NA
    ## 114        NA         NA         NA        NA        NA         NA         NA
    ## 115        NA         NA         NA        NA        NA         NA         NA
    ## 116        NA         NA         NA        NA        NA         NA         NA
    ## 117        NA         NA         NA        NA        NA         NA         NA
    ## 118        NA         NA         NA        NA    47.000         NA         NA
    ## 119        NA         NA         NA        NA  1653.000         NA         NA
    ## 120         3          1         NA 0.0700000        NA         NA         NA
    ## 121        NA         NA         NA 1.1700000        NA         NA         NA
    ## 122        NA         NA         NA 3.6200000        NA         NA         NA
    ## 123        NA         NA         NA 1.4500000        NA         NA         NA
    ## 124        NA         NA         NA        NA        NA         NA         NA
    ## 125        NA         NA         NA        NA        NA         NA         NA
    ## 126        NA         NA         NA        NA        NA         NA         NA
    ## 127        NA         NA         NA        NA        NA         NA         NA
    ## 128        NA         NA         NA        NA        NA         NA         NA
    ## 129        NA         NA         NA        NA        NA         NA         NA
    ## 130        NA         NA         NA        NA        NA         NA         NA
    ## 131        NA         NA         NA        NA        NA         NA         NA
    ## 132        NA         NA         NA        NA        NA         NA         NA
    ## 133        NA         NA         NA        NA        NA         NA         NA
    ## 134        NA         NA         NA        NA        NA         NA         NA
    ## 135        NA         NA         NA        NA        NA         NA         NA
    ## 136        NA         NA         NA        NA        NA         NA         NA
    ## 137        NA         NA         NA        NA        NA         NA         NA
    ## 138        NA         NA         NA        NA        NA         NA         NA
    ## 139        NA         NA         NA        NA        NA         NA         NA
    ## 140        NA         NA         NA        NA        NA         NA         NA
    ## 141        NA         NA         NA        NA        NA         NA         NA
    ## 142        NA         NA         NA        NA        NA         NA         NA
    ## 143        NA         NA         NA        NA        NA         NA         NA
    ## 144        NA         NA         NA        NA        NA         NA         NA
    ## 145        NA         NA         NA        NA        NA         NA         NA
    ## 146        NA         NA         NA        NA        NA         NA         NA
    ## 147        NA         NA         NA        NA        NA         NA         NA
    ## 148        NA         NA         NA        NA        NA         NA         NA
    ## 149        NA         NA         NA        NA        NA         NA         NA
    ## 150        NA         NA         NA        NA        NA         NA         NA
    ## 151        NA         NA         NA        NA        NA         NA         NA
    ## 152        NA         NA         NA        NA        NA         NA         NA
    ## 153        NA         NA         NA        NA        NA         NA         NA
    ## 154        NA         NA         NA        NA        NA         NA         NA
    ## 155        NA         NA         NA        NA        NA         NA         NA
    ## 156        NA         NA         NA        NA        NA         NA         NA
    ## 157        NA         NA         NA        NA        NA         NA         NA
    ## 158        NA         NA         NA        NA        NA         NA         NA
    ## 159        NA         NA         NA        NA        NA         NA         NA
    ## 160        NA         NA         NA        NA        NA         NA         NA
    ## 161         2          1      14.00 1.6830000 16830.000         NA         NA
    ## 162        NA         NA         NA 0.4567000  4567.500         NA         NA
    ## 163        NA         NA       6.45 0.1800000        NA         NA         NA
    ## 164         4          1      16.60 2.0800000        NA         NA         NA
    ## 165        NA         NA       1.74 0.4600000        NA         NA         NA
    ## 166        NA         NA       0.81 0.2200000        NA         NA         NA
    ## 167        NA         NA         NA 0.0570000        NA         NA         NA
    ## 168        NA         NA         NA 0.0360000        NA         NA         NA
    ## 169        NA         NA         NA 0.0560000        NA         NA         NA
    ## 170        NA         NA         NA        NA   851.000         NA       39.0
    ## 171        NA         NA         NA        NA   410.000         NA       12.6
    ## 172        NA         NA         NA        NA   436.000         NA        1.0
    ## 173        NA         NA         NA        NA  3610.000         NA       72.0
    ## 174        NA         NA         NA        NA   550.000         NA       52.0
    ## 175        NA         NA         NA        NA  5430.000         NA      116.0
    ## 176        NA         NA         NA        NA  1150.000         NA       83.0
    ## 177        NA         NA         NA        NA  5490.000         NA      539.0
    ## 178        NA         NA         NA        NA 13200.000         NA      329.0
    ## 179        NA         NA         NA        NA  2490.000         NA      272.0
    ## 180        NA         NA         NA        NA   276.000         NA      404.0
    ## 181        NA         NA      11.09        NA  5300.000         NA         NA
    ## 182        NA         NA       3.49        NA  1400.000         NA         NA
    ## 183        NA         NA       4.67        NA        NA         NA         NA
    ## 184        NA         NA       5.85        NA   710.000         NA         NA
    ## 185        NA         NA       5.56        NA  1500.000         NA         NA
    ## 186        NA         NA         NA        NA        NA         NA         NA
    ## 187        NA         NA         NA        NA        NA         NA         NA
    ## 188        NA         NA         NA        NA        NA         NA         NA
    ## 189        NA         NA         NA        NA        NA         NA         NA
    ## 190        NA         NA         NA        NA        NA         NA         NA
    ## 191        NA         NA         NA        NA        NA         NA         NA
    ## 192        NA         NA         NA        NA        NA         NA         NA
    ## 193        NA         NA       7.00 1.0700000        NA         NA         NA
    ## 194        NA         NA       3.50 0.3050000        NA         NA         NA
    ## 195        NA         NA       2.90 0.4930000        NA         NA         NA
    ## 196        NA         NA       7.00 0.7290000        NA         NA         NA
    ## 197        NA         NA         NA        NA        NA         NA         NA
    ## 198        NA         NA         NA        NA        NA         NA         NA
    ## 199        NA         NA         NA        NA        NA         NA         NA
    ## 200        NA         NA         NA        NA        NA         NA         NA
    ## 201        NA         NA         NA        NA        NA         NA         NA
    ## 202        NA         NA         NA        NA        NA         NA         NA
    ## 203        NA         NA         NA        NA        NA         NA         NA
    ## 204        NA         NA         NA        NA        NA         NA         NA
    ## 205        NA         NA         NA        NA        NA         NA         NA
    ## 206        NA         NA         NA        NA        NA         NA         NA
    ## 207        NA         NA         NA        NA        NA         NA         NA
    ## 208        NA         NA         NA        NA        NA         NA         NA
    ## 209        NA         NA         NA        NA        NA         NA         NA
    ## 210        NA         NA         NA        NA        NA         NA         NA
    ## 211        NA         NA         NA        NA        NA         NA         NA
    ## 212        NA         NA         NA        NA        NA         NA         NA
    ## 213        NA         NA         NA        NA        NA         NA         NA
    ## 214        NA         NA         NA        NA        NA         NA         NA
    ## 215        NA         NA         NA        NA        NA         NA         NA
    ## 216        NA         NA         NA        NA        NA         NA         NA
    ## 217        NA         NA         NA        NA        NA         NA         NA
    ## 218        NA         NA         NA        NA        NA         NA         NA
    ## 219        NA         NA         NA        NA        NA         NA         NA
    ## 220        NA         NA         NA        NA        NA         NA         NA
    ## 221        NA         NA         NA        NA        NA         NA         NA
    ## 222        NA         NA         NA        NA        NA         NA         NA
    ## 223        NA         NA         NA        NA        NA         NA         NA
    ## 224        NA         NA         NA        NA        NA         NA         NA
    ## 225        NA         NA         NA        NA        NA         NA         NA
    ## 226        NA         NA         NA        NA        NA         NA         NA
    ## 227        NA         NA         NA        NA        NA         NA         NA
    ## 228        NA         NA         NA        NA        NA         NA         NA
    ## 229        NA         NA         NA        NA        NA         NA         NA
    ## 230        NA         NA         NA        NA        NA         NA         NA
    ## 231        NA         NA         NA        NA        NA         NA         NA
    ## 232        NA         NA         NA        NA        NA         NA         NA
    ## 233        NA         NA         NA        NA        NA         NA         NA
    ## 234        NA         NA         NA        NA        NA         NA         NA
    ## 235        NA         NA         NA        NA        NA         NA         NA
    ## 236        NA         NA         NA        NA        NA         NA         NA
    ## 237        NA         NA         NA        NA        NA         NA         NA
    ## 238        NA         NA         NA        NA        NA         NA         NA
    ## 239        NA         NA         NA        NA        NA         NA         NA
    ## 240        NA         NA         NA        NA        NA         NA         NA
    ## 241        NA         NA         NA        NA        NA         NA         NA
    ## 242        NA         NA         NA        NA        NA         NA         NA
    ## 243        NA         NA         NA        NA        NA         NA         NA
    ## 244        NA         NA         NA        NA        NA         NA         NA
    ## 245        NA         NA         NA        NA        NA         NA         NA
    ## 246        NA         NA         NA        NA        NA         NA         NA
    ## 247        NA         NA         NA        NA        NA         NA         NA
    ## 248        NA         NA         NA        NA        NA         NA         NA
    ## 249        NA         NA         NA        NA        NA         NA         NA
    ## 250        NA         NA         NA        NA        NA         NA         NA
    ## 251        NA         NA         NA        NA        NA         NA         NA
    ## 252        NA         NA         NA        NA        NA         NA         NA
    ## 253        NA         NA         NA        NA        NA         NA         NA
    ## 254        NA         NA         NA        NA        NA         NA         NA
    ## 255        NA         NA         NA        NA        NA         NA         NA
    ## 256        NA         NA         NA        NA        NA         NA         NA
    ## 257        NA         NA         NA        NA        NA         NA         NA
    ## 258        NA         NA         NA        NA        NA         NA         NA
    ## 259        NA         NA         NA        NA        NA         NA         NA
    ## 260        NA         NA         NA        NA        NA         NA         NA
    ## 261        NA         NA         NA        NA        NA         NA         NA
    ## 262        NA         NA         NA        NA        NA         NA         NA
    ## 263        NA         NA         NA        NA        NA         NA         NA
    ## 264        NA         NA         NA        NA        NA         NA         NA
    ## 265        NA         NA         NA        NA        NA         NA         NA
    ## 266        NA         NA         NA        NA        NA         NA         NA
    ## 267        NA         NA         NA        NA        NA         NA         NA
    ## 268        NA         NA         NA        NA        NA         NA         NA
    ## 269        NA         NA         NA        NA        NA         NA         NA
    ## 270        NA         NA         NA        NA        NA         NA         NA
    ## 271        NA         NA         NA        NA        NA         NA         NA
    ## 272        NA         NA         NA        NA        NA         NA         NA
    ## 273        NA         NA         NA        NA        NA         NA         NA
    ## 274        NA         NA         NA        NA        NA         NA         NA
    ## 275        NA         NA         NA        NA        NA         NA         NA
    ## 276        NA         NA         NA        NA        NA         NA         NA
    ## 277        NA         NA         NA        NA        NA         NA         NA
    ## 278        NA         NA         NA        NA        NA         NA         NA
    ## 279        NA         NA         NA        NA        NA         NA         NA
    ## 280        NA         NA         NA        NA        NA         NA         NA
    ## 281        NA         NA         NA        NA        NA         NA         NA
    ## 282        NA         NA         NA        NA        NA         NA         NA
    ## 283        NA         NA         NA        NA        NA         NA         NA
    ## 284        NA         NA         NA        NA        NA         NA         NA
    ## 285        NA         NA         NA        NA        NA         NA         NA
    ## 286        NA         NA         NA        NA        NA         NA         NA
    ## 287        NA         NA         NA        NA        NA         NA         NA
    ## 288        NA         NA         NA        NA        NA         NA         NA
    ## 289        NA         NA         NA        NA        NA         NA         NA
    ## 290        NA         NA         NA        NA        NA         NA         NA
    ## 291        NA         NA         NA        NA        NA         NA         NA
    ## 292        NA         NA         NA        NA        NA         NA         NA
    ## 293        NA         NA       3.93 0.2800000        NA         NA         NA
    ## 294        NA         NA       2.14 0.7900000        NA         NA         NA
    ## 295        NA         NA       0.83 0.0000000        NA         NA         NA
    ## 296        NA         NA       0.54 0.0000000        NA         NA         NA
    ## 297        NA         NA       2.60 0.3200000        NA         NA         NA
    ## 298        NA         NA       6.10 2.9000000        NA         NA         NA
    ## 299        NA         NA       6.60 0.5200000        NA         NA         NA
    ## 300        NA         NA         NA        NA        NA       0.03      300.0
    ## 301        NA         NA         NA        NA        NA       0.28     2800.0
    ## 302        NA         NA         NA        NA        NA       0.57     5700.0
    ## 303        NA         NA         NA 0.0400000        NA         NA         NA
    ## 304        NA         NA         NA 0.0063100    63.100         NA         NA
    ## 305        NA         NA         NA 0.0263000   263.000         NA         NA
    ## 306        NA         NA         NA 0.0000000     0.000         NA         NA
    ## 307        NA         NA         NA 0.0042600    42.600         NA         NA
    ## 308        NA         NA         NA 0.0564000   564.000         NA         NA
    ## 309        NA         NA         NA 0.0289000   289.000         NA         NA
    ## 310        NA         NA         NA 0.0000000     0.000         NA         NA
    ## 311        NA         NA         NA 0.0034100    34.100         NA         NA
    ## 312        NA         NA         NA 0.1180000  1180.000         NA         NA
    ## 313        NA         NA         NA 0.0588000   588.000         NA         NA
    ## 314        NA         NA         NA 0.0811000   811.000         NA         NA
    ## 315        NA         NA         NA 0.0105000   105.000         NA         NA
    ## 316        NA         NA         NA 0.0262000   262.000         NA         NA
    ## 317        NA         NA         NA 0.0565000   565.000         NA         NA
    ## 318        NA         NA         NA 0.0156000   156.000         NA         NA
    ## 319        NA         NA         NA 0.0000000     0.000         NA         NA
    ## 320        NA         NA         NA 0.0600000   600.000         NA         NA
    ## 321        NA         NA         NA 0.0400000   400.000         NA         NA
    ## 322        NA         NA         NA 0.0272000   272.000         NA         NA
    ## 323        NA         NA         NA 0.0144000   144.000         NA         NA
    ## 324        NA         NA         NA 0.0541000   541.000         NA         NA
    ## 325         3          1         NA 0.0001410     1.410         NA         NA
    ## 326         2          1         NA 0.0000789     0.789         NA         NA
    ## 327         3          1         NA 0.0000563     0.563         NA         NA
    ## 328         2          1         NA 0.0002640     2.640         NA         NA
    ## 329         2          1         NA 0.0001940     1.940         NA         NA
    ## 330         3          1         NA 0.0001160     1.160         NA         NA
    ## 331        NA         NA         NA 0.0001280     1.280         NA         NA
    ## 332        NA         NA         NA 0.0001100     1.100         NA         NA
    ## 333        NA         NA         NA 0.0002340     2.340         NA         NA
    ## 334        NA         NA         NA 0.0000932     0.932         NA         NA
    ## 335         1          1         NA        NA        NA         NA         NA
    ## 336         2          7         NA        NA        NA         NA         NA
    ## 337         3          7         NA        NA        NA         NA         NA
    ## 338         4          7         NA        NA        NA         NA         NA
    ## 339         5          7         NA        NA        NA         NA         NA
    ## 340         6          7         NA        NA        NA         NA         NA
    ## 341         7          7         NA        NA        NA         NA         NA
    ## 342         1          7         NA        NA        NA         NA         NA
    ## 343         2          7         NA        NA        NA         NA         NA
    ## 344         3          7         NA        NA        NA         NA         NA
    ## 345         4          7         NA        NA        NA         NA         NA
    ## 346         5          7         NA        NA        NA         NA         NA
    ## 347         6          7         NA        NA        NA         NA         NA
    ## 348         7          7         NA        NA        NA         NA         NA
    ## 349         1          7         NA        NA        NA         NA         NA
    ## 350         2          7         NA        NA        NA         NA         NA
    ## 351         3          7         NA        NA        NA         NA         NA
    ## 352         4          7         NA        NA        NA         NA         NA
    ## 353         5          7         NA        NA        NA         NA         NA
    ## 354         6          7         NA        NA        NA         NA         NA
    ## 355         7          7         NA        NA        NA         NA         NA
    ## 356         1         14         NA        NA        NA         NA         NA
    ## 357         2         14         NA        NA        NA         NA         NA
    ## 358         3         14         NA        NA        NA         NA         NA
    ## 359         4         14         NA        NA        NA         NA         NA
    ## 360         5         14         NA        NA        NA         NA         NA
    ## 361         6         14         NA        NA        NA         NA         NA
    ## 362         7         14         NA        NA        NA         NA         NA
    ## 363         8         14         NA        NA        NA         NA         NA
    ## 364         9         14         NA        NA        NA         NA         NA
    ## 365        10         14         NA        NA        NA         NA         NA
    ## 366        11         14         NA        NA        NA         NA         NA
    ## 367        12         14         NA        NA        NA         NA         NA
    ## 368        13         14         NA        NA        NA         NA         NA
    ## 369        14         14         NA        NA        NA         NA         NA
    ## 370         1          7         NA        NA        NA         NA         NA
    ## 371         2          7         NA        NA        NA         NA         NA
    ## 372         3          7         NA        NA        NA         NA         NA
    ## 373         4          7         NA        NA        NA         NA         NA
    ## 374         5          7         NA        NA        NA         NA         NA
    ## 375         6          7         NA        NA        NA         NA         NA
    ## 376         7          7         NA        NA        NA         NA         NA
    ## 377         1          7         NA        NA        NA         NA         NA
    ## 378         2          7         NA        NA        NA         NA         NA
    ## 379         3          7         NA        NA        NA         NA         NA
    ## 380         4          7         NA        NA        NA         NA         NA
    ## 381         5          7         NA        NA        NA         NA         NA
    ## 382         6          7         NA        NA        NA         NA         NA
    ## 383         7          7         NA        NA        NA         NA         NA
    ## 384         1          7         NA        NA        NA         NA         NA
    ## 385         2          7         NA        NA        NA         NA         NA
    ## 386         3          7         NA        NA        NA         NA         NA
    ## 387         4          7         NA        NA        NA         NA         NA
    ## 388         5          7         NA        NA        NA         NA         NA
    ## 389         6          7         NA        NA        NA         NA         NA
    ## 390         7          7         NA        NA        NA         NA         NA
    ## 391         1          2         NA        NA        NA         NA         NA
    ## 392         2          2         NA        NA        NA         NA         NA
    ## 393         1          7         NA        NA        NA         NA         NA
    ## 394         2          7         NA        NA        NA         NA         NA
    ## 395         3          7         NA        NA        NA         NA         NA
    ## 396         4          7         NA        NA        NA         NA         NA
    ## 397         5          7         NA        NA        NA         NA         NA
    ## 398         6          7         NA        NA        NA         NA         NA
    ## 399         7          7         NA        NA        NA         NA         NA
    ## 400         1          7         NA        NA        NA         NA         NA
    ## 401         2          7         NA        NA        NA         NA         NA
    ## 402         3          7         NA        NA        NA         NA         NA
    ## 403         4          7         NA        NA        NA         NA         NA
    ## 404         5          7         NA        NA        NA         NA         NA
    ## 405         6          7         NA        NA        NA         NA         NA
    ## 406         7          7         NA        NA        NA         NA         NA
    ## 407         1          2         NA        NA        NA         NA         NA
    ## 408         2          2         NA        NA        NA         NA         NA
    ## 409         1          5         NA        NA        NA         NA         NA
    ## 410         2          5         NA        NA        NA         NA         NA
    ## 411         3          5         NA        NA        NA         NA         NA
    ## 412         4          5         NA        NA        NA         NA         NA
    ## 413         5          5         NA        NA        NA         NA         NA
    ## 414         1          7         NA        NA        NA         NA         NA
    ## 415         2          7         NA        NA        NA         NA         NA
    ## 416         3          7         NA        NA        NA         NA         NA
    ## 417         4          7         NA        NA        NA         NA         NA
    ## 418         5          7         NA        NA        NA         NA         NA
    ## 419         6          7         NA        NA        NA         NA         NA
    ## 420         7          7         NA        NA        NA         NA         NA
    ## 421         1          5         NA        NA        NA         NA         NA
    ## 422         2          5         NA        NA        NA         NA         NA
    ## 423         3          5         NA        NA        NA         NA         NA
    ## 424         4          5         NA        NA        NA         NA         NA
    ## 425         5          5         NA        NA        NA         NA         NA
    ## 426        NA         NA         NA        NA        NA         NA         NA
    ## 427        NA         NA         NA        NA        NA         NA         NA
    ## 428        NA         NA         NA        NA        NA         NA         NA
    ## 429        NA         NA         NA        NA        NA         NA         NA
    ## 430        NA         NA         NA        NA        NA         NA         NA
    ## 431        NA         NA         NA        NA        NA         NA         NA
    ## 432        NA         NA         NA        NA        NA         NA         NA
    ## 433        NA         NA         NA        NA        NA         NA         NA
    ## 434        NA         NA         NA        NA        NA         NA         NA
    ## 435        NA         NA         NA        NA        NA         NA         NA
    ## 436        NA         NA         NA        NA        NA         NA         NA
    ## 437        NA         NA         NA        NA        NA         NA         NA
    ## 438        NA         NA         NA        NA        NA         NA         NA
    ## 439        NA         NA         NA        NA        NA         NA         NA
    ## 440        NA         NA         NA        NA        NA         NA         NA
    ## 441        NA         NA         NA        NA        NA         NA         NA
    ## 442        NA         NA         NA        NA        NA         NA         NA
    ## 443        NA         NA         NA        NA        NA         NA         NA
    ## 444        NA         NA         NA        NA        NA         NA         NA
    ## 445        NA         NA         NA        NA        NA         NA         NA
    ## 446        NA         NA         NA        NA        NA         NA         NA
    ## 447        NA         NA         NA        NA        NA         NA         NA
    ## 448        NA         NA         NA        NA        NA         NA         NA
    ## 449        NA         NA         NA        NA        NA         NA         NA
    ## 450        NA         NA         NA        NA        NA         NA         NA
    ## 451        NA         NA         NA        NA        NA         NA         NA
    ## 452        NA         NA         NA        NA        NA         NA         NA
    ## 453        NA         NA         NA        NA        NA         NA         NA
    ## 454        NA         NA         NA        NA        NA         NA         NA
    ## 455        NA         NA         NA        NA        NA         NA         NA
    ## 456        NA         NA         NA        NA        NA         NA         NA
    ## 457        NA         NA         NA        NA        NA         NA         NA
    ## 458        NA         NA         NA        NA        NA         NA         NA
    ## 459        NA         NA         NA        NA        NA         NA         NA
    ## 460        NA         NA         NA        NA        NA         NA         NA
    ## 461        NA         NA         NA        NA        NA         NA         NA
    ## 462        NA         NA         NA        NA        NA         NA         NA
    ## 463        NA         NA         NA        NA        NA         NA         NA
    ## 464        NA         NA         NA        NA        NA         NA         NA
    ## 465        NA         NA         NA        NA        NA         NA         NA
    ## 466        NA         NA         NA        NA        NA         NA         NA
    ## 467        NA         NA         NA        NA        NA         NA         NA
    ## 468        NA         NA         NA        NA        NA         NA         NA
    ## 469        NA         NA         NA        NA        NA         NA         NA
    ## 470        NA         NA         NA        NA        NA         NA         NA
    ## 471        NA         NA         NA        NA        NA         NA         NA
    ## 472        NA         NA         NA        NA        NA         NA         NA
    ## 473        NA         NA         NA        NA        NA         NA         NA
    ## 474        NA         NA         NA        NA        NA         NA         NA
    ## 475        NA         NA         NA        NA        NA         NA         NA
    ## 476        NA         NA         NA        NA        NA         NA         NA
    ## 477        NA         NA         NA        NA        NA         NA         NA
    ## 478        NA         NA         NA        NA        NA         NA         NA
    ## 479        NA         NA         NA        NA        NA         NA         NA
    ## 480        NA         NA         NA        NA        NA         NA         NA
    ## 481        NA         NA         NA        NA        NA         NA         NA
    ## 482        NA         NA         NA        NA        NA         NA         NA
    ## 483        NA         NA         NA        NA        NA         NA         NA
    ## 484        NA         NA         NA        NA        NA         NA         NA
    ## 485        NA         NA         NA        NA        NA         NA         NA
    ## 486        NA         NA         NA        NA        NA         NA         NA
    ## 487        NA         NA         NA        NA        NA         NA         NA
    ## 488        NA         NA         NA        NA        NA         NA         NA
    ## 489        NA         NA         NA        NA        NA         NA         NA
    ## 490        NA         NA         NA        NA        NA         NA         NA
    ## 491        NA         NA         NA        NA        NA         NA         NA
    ## 492        NA         NA         NA        NA        NA         NA         NA
    ## 493        NA         NA         NA        NA        NA         NA         NA
    ## 494        NA         NA         NA        NA        NA         NA         NA
    ## 495        NA         NA         NA        NA        NA         NA         NA
    ## 496        NA         NA         NA        NA        NA         NA         NA
    ## 497        NA         NA         NA        NA        NA         NA         NA
    ## 498        NA         NA         NA        NA        NA         NA         NA
    ## 499        NA         NA         NA        NA        NA         NA         NA
    ## 500        NA         NA         NA        NA        NA         NA         NA
    ## 501        NA         NA         NA        NA        NA         NA         NA
    ## 502        NA         NA         NA        NA        NA         NA         NA
    ## 503        NA         NA         NA        NA        NA         NA         NA
    ## 504        NA         NA         NA        NA        NA         NA         NA
    ## 505        NA         NA         NA        NA        NA         NA         NA
    ## 506        NA         NA         NA        NA        NA         NA         NA
    ## 507        NA         NA         NA        NA        NA         NA         NA
    ## 508        NA         NA         NA        NA        NA         NA         NA
    ## 509        NA         NA         NA        NA        NA         NA         NA
    ## 510        NA         NA         NA        NA        NA         NA         NA
    ## 511        NA         NA         NA        NA        NA         NA         NA
    ## 512        NA         NA         NA        NA        NA         NA         NA
    ## 513        NA         NA         NA        NA        NA         NA         NA
    ## 514        NA         NA         NA        NA        NA         NA         NA
    ## 515        NA         NA         NA        NA        NA         NA         NA
    ## 516        NA         NA         NA        NA        NA         NA         NA
    ## 517        NA         NA         NA        NA        NA         NA         NA
    ## 518        NA         NA         NA        NA        NA         NA         NA
    ## 519        NA         NA         NA        NA        NA         NA         NA
    ## 520        NA         NA         NA        NA        NA         NA         NA
    ## 521        NA         NA         NA        NA        NA         NA         NA
    ## 522        NA         NA         NA        NA        NA         NA         NA
    ## 523        NA         NA         NA        NA        NA         NA         NA
    ## 524        NA         NA         NA        NA        NA         NA         NA
    ## 525        NA         NA         NA        NA        NA         NA         NA
    ## 526        NA         NA         NA        NA        NA         NA         NA
    ## 527        NA         NA         NA        NA        NA         NA         NA
    ## 528        NA         NA         NA        NA        NA         NA         NA
    ## 529        NA         NA         NA        NA        NA         NA         NA
    ## 530        NA         NA         NA        NA        NA         NA         NA
    ## 531        NA         NA         NA        NA        NA         NA         NA
    ## 532        NA         NA         NA        NA        NA         NA         NA
    ## 533        NA         NA         NA        NA        NA         NA         NA
    ## 534        NA         NA         NA        NA        NA         NA         NA
    ## 535        NA         NA         NA        NA        NA         NA         NA
    ## 536        NA         NA         NA        NA        NA         NA         NA
    ## 537        NA         NA         NA        NA        NA         NA         NA
    ## 538        NA         NA         NA        NA        NA         NA         NA
    ## 539        NA         NA         NA        NA        NA         NA         NA
    ## 540        NA         NA         NA        NA        NA         NA         NA
    ## 541        NA         NA         NA        NA        NA         NA         NA
    ## 542        NA         NA         NA        NA        NA         NA         NA
    ## 543        NA         NA         NA        NA        NA         NA         NA
    ## 544        NA         NA         NA        NA        NA         NA         NA
    ## 545        NA         NA         NA        NA        NA         NA         NA
    ## 546        NA         NA         NA        NA        NA         NA         NA
    ## 547        NA         NA         NA        NA        NA         NA         NA
    ## 548        NA         NA         NA        NA        NA         NA         NA
    ## 549        NA         NA         NA        NA        NA         NA         NA
    ## 550        NA         NA         NA        NA        NA         NA         NA
    ## 551        NA         NA         NA        NA        NA         NA         NA
    ## 552        NA         NA         NA        NA        NA         NA         NA
    ## 553        NA         NA         NA        NA        NA         NA         NA
    ## 554        NA         NA         NA        NA        NA         NA         NA
    ## 555        NA         NA         NA        NA        NA         NA         NA
    ## 556        NA         NA       7.70        NA   150.000         NA         NA
    ## 557        NA         NA       9.10        NA   280.000         NA         NA
    ## 558        NA         NA       7.30        NA   420.000         NA         NA
    ## 559        NA         NA      10.50        NA  1460.000         NA         NA
    ## 560        NA         NA       8.20        NA  1070.000         NA         NA
    ## 561        NA         NA       7.00        NA   620.000         NA         NA
    ## 562        NA         NA       7.10        NA   970.000         NA         NA
    ## 563        NA         NA       6.90        NA   300.000         NA         NA
    ## 564        NA         NA       8.00        NA  1520.000         NA         NA
    ## 565        NA         NA       4.00        NA   260.000         NA         NA
    ## 566        NA         NA       8.90        NA   840.000         NA         NA
    ## 567        NA         NA       8.50        NA   510.000         NA         NA
    ## 568        NA         NA       6.70        NA   880.000         NA         NA
    ## 569        NA         NA       8.30        NA   170.000         NA         NA
    ## 570        NA         NA       8.20        NA  1070.000         NA         NA
    ## 571        NA         NA       6.10        NA   880.000         NA         NA
    ## 572        NA         NA         NA        NA        NA         NA         NA
    ## 573        NA         NA         NA        NA        NA         NA         NA
    ## 574        NA         NA         NA        NA        NA         NA         NA
    ## 575        NA         NA         NA        NA        NA         NA         NA
    ## 576        NA         NA         NA        NA        NA         NA         NA
    ## 577        NA         NA         NA        NA        NA         NA         NA
    ## 578        NA         NA         NA        NA        NA         NA         NA
    ## 579        NA         NA         NA        NA        NA         NA         NA
    ## 580        NA         NA      10.00        NA  5700.000         NA     5600.0
    ## 581        NA         NA      10.00        NA        NA       0.00        0.0
    ## 582        NA         NA      10.00        NA   830.000         NA      600.0
    ## 583        NA         NA      10.00        NA   250.000       0.00        0.0
    ## 584        NA         NA      10.00        NA  1300.000         NA      800.0
    ## 585        NA         NA      10.00        NA  1360.000         NA      920.0
    ## 586        NA         NA      10.00        NA   860.000         NA      330.0
    ## 587        NA         NA      10.00        NA   420.000       0.00        0.0
    ## 588        NA         NA      10.00        NA   320.000         NA      200.0
    ## 589        NA         NA      10.00        NA   200.000         NA      100.0
    ## 590        NA         NA      10.00        NA   800.000       0.00        0.0
    ## 591        NA         NA      10.00        NA  1500.000         NA     1300.0
    ## 592        NA         NA      10.00        NA   110.000       0.00        0.0
    ## 593        NA         NA      10.00        NA   110.000       0.00        0.0
    ## 594        NA         NA      10.00        NA   250.000       0.00        0.0
    ## 595        NA         NA      10.00        NA     0.000       0.00        0.0
    ## 596        NA         NA      10.00        NA     0.000       0.00        0.0
    ## 597        NA         NA      10.00        NA     0.000       0.00        0.0
    ## 598        NA         NA      10.00        NA   600.000         NA      400.0
    ## 599        NA         NA         NA        NA        NA         NA         NA
    ## 600        NA         NA         NA        NA        NA         NA         NA
    ## 601        NA         NA       0.01        NA        NA         NA         NA
    ## 602        NA         NA       0.01        NA        NA         NA         NA
    ## 603        NA         NA       0.01        NA        NA         NA         NA
    ## 604        NA         NA       0.01        NA        NA         NA         NA
    ## 605        NA         NA       0.01        NA        NA         NA         NA
    ## 606        NA         NA       0.01        NA        NA         NA         NA
    ## 607        NA         NA       0.01        NA        NA         NA         NA
    ## 608        NA         NA       0.01        NA        NA         NA         NA
    ## 609        NA         NA       0.01        NA        NA         NA         NA
    ## 610        NA         NA       0.01        NA        NA         NA         NA
    ## 611        NA         NA       0.01        NA        NA         NA         NA
    ## 612        NA         NA         NA        NA        NA         NA         NA
    ## 613        NA         NA         NA        NA        NA         NA         NA
    ## 614        NA         NA         NA        NA        NA         NA         NA
    ## 615        NA         NA         NA        NA        NA         NA         NA
    ## 616        NA         NA         NA        NA        NA         NA         NA
    ## 617        NA         NA         NA        NA        NA         NA         NA
    ## 618        NA         NA         NA        NA        NA         NA         NA
    ## 619        NA         NA         NA        NA        NA         NA         NA
    ## 620        NA         NA         NA        NA        NA         NA         NA
    ## 621        NA         NA         NA        NA        NA         NA         NA
    ## 622        NA         NA         NA        NA     2.530         NA         NA
    ## 623        NA         NA         NA        NA     4.960         NA         NA
    ## 624        NA         NA         NA        NA     8.850         NA         NA
    ## 625        NA         NA         NA        NA     1.410         NA         NA
    ## 626        NA         NA         NA        NA    26.700         NA         NA
    ## 627        NA         NA         NA        NA    28.600         NA         NA
    ## 628        NA         NA         NA        NA        NA         NA         NA
    ## 629        NA         NA         NA        NA        NA         NA         NA
    ## 630        NA         NA         NA        NA        NA         NA         NA
    ## 631        NA         NA         NA        NA        NA         NA         NA
    ## 632        NA         NA         NA        NA        NA         NA         NA
    ## 633        NA         NA         NA        NA        NA         NA         NA
    ## 634        NA         NA         NA 0.6200000        NA         NA         NA
    ## 635        NA         NA         NA 1.2000000        NA         NA         NA
    ## 636        NA         NA         NA 0.2100000        NA         NA         NA
    ## 637        NA         NA         NA 0.1100000        NA         NA         NA
    ## 638        NA         NA         NA        NA        NA         NA         NA
    ## 639        NA         NA         NA        NA        NA         NA         NA
    ## 640        NA         NA         NA        NA        NA         NA         NA
    ## 641        NA         NA         NA        NA        NA         NA         NA
    ## 642        NA         NA         NA        NA        NA         NA         NA
    ## 643        NA         NA         NA        NA        NA         NA         NA
    ## 644        NA         NA         NA        NA        NA         NA         NA
    ## 645        NA         NA         NA        NA        NA         NA         NA
    ## 646        NA         NA         NA        NA        NA         NA         NA
    ## 647        NA         NA         NA        NA        NA         NA         NA
    ## 648        NA         NA         NA        NA        NA         NA         NA
    ## 649        NA         NA         NA        NA        NA         NA         NA
    ## 650        NA         NA         NA        NA        NA         NA         NA
    ## 651        NA         NA         NA        NA        NA         NA         NA
    ## 652        NA         NA         NA        NA        NA         NA         NA
    ## 653        NA         NA         NA        NA        NA         NA         NA
    ## 654        NA         NA         NA        NA        NA         NA         NA
    ## 655        NA         NA         NA 0.3500000        NA         NA         NA
    ## 656        NA         NA         NA 0.0600000        NA         NA         NA
    ## 657        NA         NA         NA 0.1500000        NA         NA         NA
    ## 658        NA         NA         NA 0.2500000        NA         NA         NA
    ## 659         1         10         NA        NA        NA         NA      300.0
    ## 660         1         10         NA        NA        NA         NA      430.0
    ## 661         1         10         NA        NA        NA         NA      330.0
    ## 662         1          3         NA        NA        NA         NA      210.0
    ## 663         1          3         NA        NA        NA         NA      360.0
    ## 664         1          3         NA        NA        NA         NA      530.0
    ## 665         1          3         NA        NA        NA         NA      460.0
    ## 666         1          3         NA        NA        NA         NA      720.0
    ## 667         1          3         NA        NA        NA         NA      260.0
    ## 668        NA         NA         NA        NA        NA         NA         NA
    ## 669        NA         NA         NA        NA        NA         NA         NA
    ## 670        NA         NA         NA        NA        NA         NA         NA
    ## 671        NA         NA         NA        NA        NA         NA         NA
    ## 672        NA         NA         NA        NA        NA         NA         NA
    ## 673        NA         NA         NA        NA        NA         NA         NA
    ## 674        NA         NA         NA        NA        NA         NA         NA
    ## 675        NA         NA         NA        NA        NA         NA         NA
    ## 676        NA         NA         NA        NA        NA         NA         NA
    ## 677        NA         NA         NA        NA        NA         NA         NA
    ## 678        NA         NA         NA        NA        NA         NA         NA
    ## 679        NA         NA         NA        NA        NA         NA         NA
    ## 680        NA         NA         NA        NA        NA         NA         NA
    ## 681        NA         NA         NA        NA        NA         NA         NA
    ## 682        NA         NA         NA        NA        NA         NA         NA
    ## 683        NA         NA         NA        NA        NA         NA         NA
    ## 684        NA         NA         NA        NA        NA         NA         NA
    ## 685        NA         NA         NA        NA        NA         NA         NA
    ## 686        NA         NA         NA        NA        NA         NA         NA
    ## 687        NA         NA         NA        NA        NA         NA         NA
    ## 688        NA         NA         NA        NA        NA         NA         NA
    ## 689        NA         NA         NA        NA        NA         NA         NA
    ## 690        NA         NA         NA        NA        NA         NA         NA
    ## 691        NA         NA         NA        NA        NA         NA         NA
    ## 692        NA         NA         NA        NA        NA         NA         NA
    ## 693        NA         NA         NA        NA        NA         NA         NA
    ## 694        NA         NA         NA        NA        NA         NA         NA
    ## 695        NA         NA         NA        NA        NA         NA         NA
    ## 696        NA         NA         NA        NA        NA         NA         NA
    ## 697        NA         NA         NA        NA        NA         NA         NA
    ## 698        NA         NA         NA        NA        NA         NA         NA
    ## 699        NA         NA         NA        NA        NA         NA         NA
    ## 700        NA         NA         NA        NA        NA         NA         NA
    ## 701        NA         NA         NA        NA        NA         NA         NA
    ## 702        NA         NA         NA        NA        NA         NA         NA
    ## 703        NA         NA         NA        NA        NA         NA         NA
    ## 704        NA         NA         NA        NA        NA         NA         NA
    ## 705        NA         NA         NA        NA        NA         NA         NA
    ## 706        NA         NA         NA        NA        NA         NA         NA
    ## 707        NA         NA         NA        NA        NA         NA         NA
    ## 708        NA         NA         NA        NA        NA         NA         NA
    ## 709        NA         NA         NA        NA        NA         NA         NA
    ## 710        NA         NA         NA        NA        NA         NA         NA
    ## 711        NA         NA         NA        NA        NA         NA         NA
    ## 712        NA         NA         NA        NA        NA         NA         NA
    ## 713        NA         NA         NA        NA        NA         NA         NA
    ## 714        NA         NA         NA        NA        NA         NA         NA
    ## 715        NA         NA         NA        NA        NA         NA         NA
    ## 716        NA         NA         NA        NA        NA         NA         NA
    ## 717        NA         NA         NA        NA        NA         NA         NA
    ## 718        NA         NA         NA        NA        NA         NA         NA
    ## 719        NA         NA         NA        NA        NA         NA         NA
    ## 720        NA         NA         NA        NA        NA         NA         NA
    ## 721        NA         NA         NA        NA        NA         NA         NA
    ## 722        NA         NA         NA        NA        NA         NA         NA
    ## 723        NA         NA         NA        NA        NA         NA         NA
    ## 724        NA         NA         NA        NA        NA         NA         NA
    ## 725        NA         NA         NA        NA        NA         NA         NA
    ## 726        NA         NA         NA        NA        NA         NA         NA
    ## 727        NA         NA         NA        NA        NA         NA         NA
    ## 728        NA         NA         NA        NA        NA         NA         NA
    ## 729        NA         NA         NA        NA        NA         NA         NA
    ## 730        NA         NA         NA        NA        NA         NA         NA
    ## 731        NA         NA         NA        NA        NA         NA         NA
    ## 732        NA         NA         NA        NA        NA         NA         NA
    ## 733        NA         NA         NA        NA        NA         NA         NA
    ## 734        NA         NA         NA        NA        NA         NA         NA
    ## 735        NA         NA         NA        NA        NA         NA         NA
    ## 736        NA         NA         NA        NA        NA         NA         NA
    ## 737        NA         NA         NA        NA        NA         NA         NA
    ## 738        NA         NA         NA        NA        NA         NA         NA
    ## 739        NA         NA         NA        NA        NA         NA         NA
    ## 740        NA         NA         NA        NA        NA         NA         NA
    ## 741        NA         NA         NA        NA        NA         NA         NA
    ## 742        NA         NA         NA        NA        NA         NA         NA
    ## 743        NA         NA         NA        NA        NA         NA         NA
    ## 744        NA         NA         NA        NA        NA         NA         NA
    ## 745        NA         NA         NA        NA        NA         NA         NA
    ## 746        NA         NA         NA        NA        NA         NA         NA
    ## 747        NA         NA         NA        NA        NA         NA         NA
    ## 748        NA         NA         NA        NA        NA         NA         NA
    ## 749        NA         NA         NA        NA        NA         NA         NA
    ## 750        NA         NA         NA        NA        NA         NA         NA
    ## 751        NA         NA         NA        NA        NA         NA         NA
    ## 752        NA         NA         NA        NA        NA         NA         NA
    ## 753        NA         NA         NA        NA        NA         NA         NA
    ## 754        NA         NA         NA        NA        NA         NA         NA
    ## 755        NA         NA         NA        NA        NA         NA         NA
    ## 756        NA         NA         NA        NA        NA         NA         NA
    ## 757        NA         NA         NA        NA        NA         NA         NA
    ## 758        NA         NA         NA        NA        NA         NA         NA
    ## 759        NA         NA         NA        NA        NA         NA         NA
    ## 760        NA         NA         NA        NA        NA         NA         NA
    ## 761        NA         NA         NA        NA        NA         NA         NA
    ## 762        NA         NA         NA        NA        NA         NA         NA
    ## 763        NA         NA         NA        NA        NA         NA         NA
    ## 764        NA         NA         NA        NA        NA         NA         NA
    ## 765        NA         NA         NA        NA        NA         NA         NA
    ## 766        NA         NA         NA        NA        NA         NA         NA
    ## 767        NA         NA         NA        NA        NA         NA         NA
    ## 768        NA         NA         NA        NA        NA         NA         NA
    ## 769        NA         NA         NA        NA        NA         NA         NA
    ## 770        NA         NA         NA        NA        NA         NA         NA
    ## 771        NA         NA         NA        NA        NA         NA         NA
    ## 772        NA         NA         NA        NA        NA         NA         NA
    ## 773        NA         NA         NA        NA        NA         NA         NA
    ## 774        NA         NA         NA        NA        NA         NA         NA
    ## 775        NA         NA         NA        NA        NA         NA         NA
    ## 776        NA         NA         NA        NA        NA         NA         NA
    ## 777        NA         NA         NA        NA        NA         NA         NA
    ## 778        NA         NA         NA        NA        NA         NA         NA
    ## 779        NA         NA         NA        NA        NA         NA         NA
    ## 780        NA         NA         NA        NA        NA         NA         NA
    ## 781        NA         NA         NA        NA        NA         NA         NA
    ## 782        NA         NA         NA        NA        NA         NA         NA
    ## 783        NA         NA         NA        NA        NA         NA         NA
    ## 784        NA         NA         NA        NA        NA         NA         NA
    ## 785        NA         NA         NA        NA        NA         NA         NA
    ## 786        NA         NA         NA        NA        NA         NA         NA
    ## 787        NA         NA         NA        NA        NA         NA         NA
    ## 788        NA         NA         NA        NA        NA         NA         NA
    ## 789        NA         NA         NA        NA        NA         NA         NA
    ## 790        NA         NA         NA        NA        NA         NA         NA
    ## 791        NA         NA         NA        NA        NA         NA         NA
    ## 792        NA         NA         NA        NA        NA         NA         NA
    ## 793        NA         NA         NA        NA        NA         NA         NA
    ## 794        NA         NA         NA        NA        NA         NA         NA
    ## 795        NA         NA         NA        NA        NA         NA         NA
    ## 796        NA         NA         NA        NA        NA         NA         NA
    ## 797        NA         NA         NA        NA        NA         NA         NA
    ## 798        NA         NA         NA        NA        NA         NA         NA
    ## 799        NA         NA         NA        NA        NA         NA         NA
    ## 800        NA         NA         NA        NA        NA         NA         NA
    ## 801        NA         NA         NA        NA        NA         NA         NA
    ## 802        NA         NA         NA        NA        NA         NA         NA
    ## 803        NA         NA         NA        NA        NA         NA         NA
    ## 804        NA         NA         NA        NA        NA         NA         NA
    ## 805        NA         NA         NA        NA        NA         NA         NA
    ## 806        NA         NA         NA        NA        NA         NA         NA
    ## 807        NA         NA         NA        NA        NA         NA         NA
    ## 808        NA         NA         NA        NA        NA         NA         NA
    ## 809        NA         NA         NA        NA        NA         NA         NA
    ## 810        NA         NA         NA        NA        NA         NA         NA
    ## 811        NA         NA         NA        NA        NA         NA         NA
    ## 812        NA         NA         NA        NA        NA         NA         NA
    ## 813        NA         NA         NA        NA        NA         NA         NA
    ## 814        NA         NA         NA        NA        NA         NA         NA
    ## 815        NA         NA         NA        NA        NA         NA         NA
    ## 816        NA         NA         NA        NA        NA         NA         NA
    ## 817        NA         NA         NA        NA        NA         NA         NA
    ## 818        NA         NA         NA        NA        NA         NA         NA
    ## 819        NA         NA         NA        NA        NA         NA         NA
    ## 820        NA         NA         NA        NA        NA         NA         NA
    ## 821        NA         NA         NA        NA        NA         NA         NA
    ## 822        NA         NA         NA        NA        NA         NA         NA
    ## 823        NA         NA         NA        NA        NA         NA         NA
    ## 824        NA         NA         NA        NA        NA         NA         NA
    ## 825        NA         NA         NA        NA        NA         NA         NA
    ## 826        NA         NA         NA        NA        NA         NA         NA
    ## 827        NA         NA         NA        NA        NA         NA         NA
    ## 828        NA         NA         NA        NA        NA         NA         NA
    ## 829        NA         NA         NA        NA        NA         NA         NA
    ## 830        NA         NA         NA        NA        NA         NA         NA
    ## 831        NA         NA         NA        NA        NA         NA     1300.0
    ## 832        NA         NA         NA        NA        NA         NA      620.0
    ## 833        NA         NA         NA        NA        NA         NA     1100.0
    ## 834        NA         NA         NA        NA        NA         NA      830.0
    ## 835        NA         NA         NA        NA        NA         NA      990.0
    ## 836        NA         NA         NA        NA        NA         NA      510.0
    ## 837        NA         NA         NA        NA        NA         NA      680.0
    ## 838        NA         NA         NA        NA        NA         NA      340.0
    ## 839        NA         NA         NA        NA        NA         NA      250.0
    ## 840        NA         NA         NA        NA        NA         NA        0.0
    ## 841        NA         NA         NA        NA        NA         NA     1652.0
    ## 842        NA         NA         NA        NA        NA         NA     2240.0
    ## 843        NA         NA         NA        NA        NA         NA     2059.0
    ## 844        NA         NA         NA        NA        NA         NA        0.0
    ## 845        NA         NA         NA        NA        NA         NA        0.0
    ## 846        NA         NA         NA        NA        NA         NA        0.0
    ## 847        NA         NA         NA        NA        NA         NA      764.0
    ## 848        NA         NA         NA        NA        NA         NA       92.5
    ## 849        NA         NA         NA        NA        NA         NA       20.2
    ## 850        NA         NA         NA        NA        NA         NA      322.0
    ## 851        NA         NA         NA        NA        NA         NA      711.0
    ## 852        NA         NA         NA        NA        NA         NA      256.0
    ## 853        NA         NA         NA        NA        NA         NA      363.0
    ## 854        NA         NA         NA        NA        NA         NA      996.0
    ## 855        NA         NA         NA        NA        NA         NA      292.0
    ## 856        NA         NA         NA        NA        NA         NA      636.0
    ## 857        NA         NA         NA        NA        NA         NA      310.0
    ## 858        NA         NA         NA        NA        NA         NA        0.0
    ## 859        NA         NA       8.30        NA  4530.000         NA         NA
    ## 860        NA         NA      10.50        NA   289.000         NA         NA
    ## 861        NA         NA         NA        NA  2760.000         NA         NA
    ## 862        NA         NA      17.70        NA  2842.000         NA         NA
    ## 863        NA         NA      16.60        NA  1831.000         NA         NA
    ## 864        NA         NA         NA        NA   684.000         NA         NA
    ## 865        NA         NA       7.40        NA  6836.000         NA         NA
    ## 866        NA         NA      15.50        NA  5070.000         NA         NA
    ## 867        NA         NA       7.60        NA   465.000         NA         NA
    ## 868        NA         NA       7.70        NA  3990.000         NA         NA
    ## 869        NA         NA       4.60        NA  3260.000         NA         NA
    ## 870        NA         NA      10.10        NA  4640.000         NA         NA
    ## 871        NA         NA      11.90        NA  4433.000         NA         NA
    ## 872        NA         NA      16.00        NA   256.000         NA         NA
    ## 873        NA         NA       6.10        NA   317.000         NA         NA
    ## 874        NA         NA       5.70        NA   221.000         NA         NA
    ## 875        NA         NA       5.70        NA   267.000         NA         NA
    ## 876        NA         NA         NA        NA        NA         NA         NA
    ## 877        NA         NA         NA        NA        NA         NA         NA
    ## 878        NA         NA       0.82        NA   300.000         NA         NA
    ## 879        NA         NA       1.60        NA   120.000         NA         NA
    ## 880        NA         NA       3.10        NA  1700.000         NA         NA
    ## 881        NA         NA         NA 0.0300000        NA         NA        0.0
    ## 882        NA         NA         NA 0.0600000        NA         NA        0.0
    ## 883        NA         NA         NA 0.6800000        NA         NA         NA
    ## 884        NA         NA         NA 0.8100000        NA         NA         NA
    ## 885        NA         NA         NA 0.9000000        NA         NA         NA
    ## 886        NA         NA         NA 0.5100000        NA         NA         NA
    ## 887        NA         NA         NA 0.6200000        NA         NA         NA
    ## 888        NA         NA         NA 0.4000000        NA         NA         NA
    ## 889        NA         NA         NA        NA        NA         NA       51.1
    ## 890        NA         NA         NA        NA        NA         NA         NA
    ## 891        NA         NA         NA        NA        NA         NA        0.0
    ## 892        NA         NA         NA        NA        NA         NA         NA
    ## 893        NA         NA         NA        NA        NA         NA        0.0
    ## 894        NA         NA         NA        NA        NA         NA       52.0
    ## 895        NA         NA         NA        NA        NA         NA      264.0
    ## 896        NA         NA         NA        NA        NA         NA       78.2
    ## 897        NA         NA         NA        NA        NA         NA      252.0
    ## 898        NA         NA         NA        NA        NA         NA      136.0
    ## 899        NA         NA         NA        NA        NA         NA      195.0
    ## 900        NA         NA         NA        NA        NA         NA      260.0
    ## 901        NA         NA         NA        NA        NA         NA         NA
    ## 902        NA         NA         NA        NA        NA         NA      176.0
    ## 903        NA         NA         NA        NA        NA         NA         NA
    ## 904        NA         NA         NA        NA        NA         NA         NA
    ## 905        NA         NA         NA        NA        NA         NA      170.0
    ## 906        NA         NA         NA        NA        NA         NA      243.0
    ## 907        NA         NA         NA        NA        NA         NA         NA
    ## 908        NA         NA         NA        NA        NA         NA         NA
    ## 909        NA         NA         NA 4.6000000        NA         NA         NA
    ## 910        NA         NA         NA 3.9000000        NA         NA         NA
    ## 911        NA         NA         NA 2.4000000        NA         NA         NA
    ## 912        NA         NA         NA        NA        NA         NA         NA
    ## 913        NA         NA         NA        NA        NA         NA         NA
    ## 914        NA         NA         NA        NA        NA         NA         NA
    ## 915        NA         NA         NA        NA        NA         NA         NA
    ## 916        NA         NA         NA        NA        NA         NA         NA
    ## 917        NA         NA         NA        NA        NA         NA         NA
    ## 918        NA         NA         NA        NA        NA         NA         NA
    ## 919        NA         NA         NA        NA        NA         NA         NA
    ## 920        NA         NA         NA        NA        NA         NA         NA
    ## 921        NA         NA         NA        NA        NA         NA         NA
    ## 922        NA         NA         NA        NA        NA         NA         NA
    ## 923        NA         NA         NA        NA        NA         NA         NA
    ## 924        NA         NA         NA        NA        NA         NA         NA
    ## 925        NA         NA         NA        NA        NA         NA         NA
    ## 926        NA         NA         NA        NA        NA         NA         NA
    ## 927        NA         NA         NA        NA        NA         NA         NA
    ## 928        NA         NA         NA        NA        NA         NA         NA
    ##        PCB_T_UGG  DDT_T_NGG DDE_T_NGG DDD_T_NGG PEST_UG_G PAHTOT_PCT PAHTOT_UGG
    ## 1   5.000000e-01         NA        NA        NA        NA         NA         NA
    ## 2   5.000000e-01         NA        NA        NA        NA         NA         NA
    ## 3   5.000000e-01         NA        NA        NA        NA         NA         NA
    ## 4   5.000000e-01         NA        NA        NA        NA         NA         NA
    ## 5   5.000000e-01         NA        NA        NA        NA         NA         NA
    ## 6   1.050000e+02  33.000000        NA        NA        NA         NA         NA
    ## 7   6.600000e+01 143.000000        NA        NA        NA         NA         NA
    ## 8   1.800000e-02   0.850000        NA        NA        NA         NA    1.31200
    ## 9   2.200000e-02   1.350000        NA        NA        NA         NA    1.63000
    ## 10  1.800000e-02   5.500000        NA        NA        NA         NA    1.35800
    ## 11  2.300000e-02   1.500000        NA        NA        NA         NA    1.54500
    ## 12  2.100000e-02  16.470000        NA        NA        NA         NA    1.56200
    ## 13  2.200000e-02   1.070000        NA        NA        NA         NA    1.62800
    ## 14  1.500000e-02   0.430000        NA        NA        NA         NA    0.93700
    ## 15  1.400000e-02   1.400000        NA        NA        NA         NA    0.92400
    ## 16  1.900000e-02   0.430000        NA        NA        NA         NA    1.08400
    ## 17  2.100000e-02   2.300000        NA        NA        NA         NA    1.30100
    ## 18  1.800000e-02   0.780000        NA        NA        NA         NA    1.12600
    ## 19  1.600000e-02   1.600000        NA        NA        NA         NA    1.18800
    ## 20  2.400000e-01         NA        NA        NA        NA       6.84         NA
    ## 21  2.800000e-02         NA        NA        NA        NA       4.30         NA
    ## 22  3.900000e-02         NA        NA        NA        NA       5.75         NA
    ## 23  2.000000e-02         NA        NA        NA        NA       1.79         NA
    ## 24  4.000000e-02         NA        NA        NA        NA       2.96         NA
    ## 25  2.000000e-02         NA        NA        NA        NA       1.89         NA
    ## 26  2.000000e-02         NA        NA        NA        NA       2.42         NA
    ## 27  2.000000e-02         NA        NA        NA        NA       4.01         NA
    ## 28  2.000000e-02         NA        NA        NA        NA       3.77         NA
    ## 29  3.200000e-02         NA        NA        NA        NA       1.98         NA
    ## 30  0.000000e+00         NA        NA        NA        NA      12.68         NA
    ## 31  0.000000e+00         NA        NA        NA        NA       7.06         NA
    ## 32  5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 33  4.000000e-01         NA        NA        NA        NA         NA         NA
    ## 34  4.200000e-01         NA        NA        NA        NA         NA         NA
    ## 35  1.090000e+00         NA        NA        NA        NA         NA         NA
    ## 36  9.000000e-01         NA        NA        NA        NA         NA         NA
    ## 37  3.930000e+00         NA        NA        NA        NA         NA         NA
    ## 38  1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 39  1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 40  1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 41  6.700000e-01         NA        NA        NA        NA         NA         NA
    ## 42  3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 43  2.800000e+00         NA        NA        NA        NA         NA         NA
    ## 44  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 45  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 46  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 47  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 48  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 49  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 50  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 51  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 52  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 53  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 54  0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 55  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 56  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 57  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 58  2.430000e+00         NA        NA        NA        NA         NA         NA
    ## 59  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 60  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 61  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 62  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 63  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 64  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 65  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 66  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 67  2.550000e-01         NA        NA        NA        NA         NA         NA
    ## 68  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 69  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 70  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 71  4.800000e+00         NA        NA        NA        NA         NA         NA
    ## 72  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 73  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 74  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 75  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 76  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 77  1.120000e+00         NA        NA        NA        NA         NA         NA
    ## 78  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 79  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 80  4.900000e-01         NA        NA        NA        NA         NA         NA
    ## 81  1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 82  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 83  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 84  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 85  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 86  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 87  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 88  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 89  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 90  6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 91  2.600000e-01         NA        NA        NA        NA         NA         NA
    ## 92  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 93  3.500000e-01         NA        NA        NA        NA         NA         NA
    ## 94  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 95  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 96  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 97  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 98  1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 99  0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 100 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 101 1.340000e-01         NA        NA        NA        NA         NA         NA
    ## 102 4.180000e-02         NA        NA        NA        NA         NA         NA
    ## 103 3.580000e-02         NA        NA        NA        NA         NA         NA
    ## 104 1.120000e-01         NA        NA        NA        NA         NA         NA
    ## 105 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 106 1.600000e-01         NA        NA        NA        NA         NA         NA
    ## 107 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 108 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 109 1.650000e+00         NA        NA        NA        NA         NA         NA
    ## 110 5.630000e+00         NA        NA        NA        NA         NA         NA
    ## 111 7.900000e-01         NA        NA        NA        NA         NA         NA
    ## 112 9.300000e-01         NA        NA        NA        NA         NA         NA
    ## 113 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 114 1.900000e+00         NA        NA        NA        NA         NA         NA
    ## 115 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 116 2.300000e+00         NA        NA        NA        NA         NA         NA
    ## 117 3.200000e-01         NA        NA        NA        NA         NA         NA
    ## 118 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 119 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 120 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 121 0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 122 0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 123 0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 124 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 125 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 126 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 127 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 128 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 129 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 130 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 131 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 132 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 133 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 134 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 135 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 136 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 137 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 138 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 139 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 140 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 141 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 142 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 143 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 144 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 145 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 146 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 147 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 148 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 149 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 150 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 151 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 152 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 153 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 154 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 155 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 156 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 157 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 158 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 159 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 160 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 161 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 162 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 163 5.300000e-01         NA        NA        NA        NA         NA         NA
    ## 164 3.600000e-01         NA        NA        NA        NA         NA         NA
    ## 165 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 166 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 167 2.500000e-02   7.000000     0.005     0.008        NA         NA         NA
    ## 168 1.500000e-02   0.000000     0.008     0.001        NA         NA         NA
    ## 169 5.000000e-03  96.000000        NA        NA        NA         NA         NA
    ## 170 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 171 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 172 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 173 2.700000e-01         NA        NA        NA        NA         NA         NA
    ## 174 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 175 3.700000e-01         NA        NA        NA        NA         NA         NA
    ## 176 2.600000e-01         NA        NA        NA        NA         NA         NA
    ## 177 9.600000e-01         NA        NA        NA        NA         NA         NA
    ## 178 1.300000e+00         NA        NA        NA        NA         NA         NA
    ## 179 3.300000e-01         NA        NA        NA        NA         NA         NA
    ## 180 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 181 3.000000e+03         NA        NA        NA        NA         NA         NA
    ## 182 2.400000e+02         NA        NA        NA        NA         NA         NA
    ## 183 2.400000e+02         NA        NA        NA        NA         NA         NA
    ## 184 7.700000e+02         NA        NA        NA        NA         NA         NA
    ## 185 1.700000e+02         NA        NA        NA        NA         NA         NA
    ## 186 7.900000e+01         NA        NA        NA        NA         NA         NA
    ## 187 9.820000e+01         NA        NA        NA        NA         NA         NA
    ## 188 9.060000e+01         NA        NA        NA        NA         NA         NA
    ## 189 5.620000e+01         NA        NA        NA        NA         NA         NA
    ## 190 8.160000e+01         NA        NA        NA        NA         NA         NA
    ## 191 8.890000e+01         NA        NA        NA        NA         NA         NA
    ## 192 9.930000e+01         NA        NA        NA        NA         NA         NA
    ## 193 4.800000e-01         NA        NA        NA        NA         NA         NA
    ## 194 1.700000e+00         NA        NA        NA        NA         NA         NA
    ## 195 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 196 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 197 3.820000e-02         NA        NA        NA        NA       0.60         NA
    ## 198 8.080000e-02         NA        NA        NA        NA       4.50         NA
    ## 199 1.070000e-01         NA        NA        NA        NA       4.60         NA
    ## 200 7.830000e-02         NA        NA        NA        NA       1.90         NA
    ## 201 4.750000e-02         NA        NA        NA        NA       1.60         NA
    ## 202 7.040000e-02         NA        NA        NA        NA       2.70         NA
    ## 203 1.550000e-01         NA        NA        NA        NA    1070.00         NA
    ## 204 1.770000e-01         NA        NA        NA        NA     813.00         NA
    ## 205 1.520000e-01         NA        NA        NA        NA     552.00         NA
    ## 206 1.340000e-01         NA        NA        NA        NA    1050.00         NA
    ## 207 1.350000e-01         NA        NA        NA        NA     914.00         NA
    ## 208 1.390000e-01         NA        NA        NA        NA     880.00         NA
    ## 209 4.500000e-01         NA        NA        NA        NA       8.10         NA
    ## 210 3.080000e-01         NA        NA        NA        NA       7.80         NA
    ## 211 1.460000e-01         NA        NA        NA        NA       3.50         NA
    ## 212 3.140000e-01         NA        NA        NA        NA       7.60         NA
    ## 213 4.210000e-01         NA        NA        NA        NA       5.50         NA
    ## 214 3.270000e-01         NA        NA        NA        NA       6.50         NA
    ## 215 7.300000e-02         NA        NA        NA        NA       6.20         NA
    ## 216 8.660000e-02         NA        NA        NA        NA       9.40         NA
    ## 217 1.270000e-01         NA        NA        NA        NA       5.90         NA
    ## 218 9.060000e-02         NA        NA        NA        NA       5.20         NA
    ## 219 1.230000e-01         NA        NA        NA        NA       8.20         NA
    ## 220 1.000000e-01         NA        NA        NA        NA       6.00         NA
    ## 221 5.710000e-02         NA        NA        NA        NA       2.20         NA
    ## 222 6.520000e-02         NA        NA        NA        NA       3.20         NA
    ## 223 5.550000e-02         NA        NA        NA        NA       1.80         NA
    ## 224 6.670000e-02         NA        NA        NA        NA       1.90         NA
    ## 225 5.840000e-02         NA        NA        NA        NA       2.70         NA
    ## 226 6.060000e-02         NA        NA        NA        NA       2.40         NA
    ## 227 2.230000e-02         NA        NA        NA        NA       1.00         NA
    ## 228 7.100000e-03         NA        NA        NA        NA       0.90         NA
    ## 229 1.390000e-02         NA        NA        NA        NA       0.50         NA
    ## 230 1.440000e-02         NA        NA        NA        NA       0.80         NA
    ## 231 4.290000e-02         NA        NA        NA        NA       7.80         NA
    ## 232 2.790000e-02         NA        NA        NA        NA      10.80         NA
    ## 233 7.940000e-02         NA        NA        NA        NA       7.20         NA
    ## 234 2.060000e-02         NA        NA        NA        NA      33.30         NA
    ## 235 2.580000e-02         NA        NA        NA        NA      12.20         NA
    ## 236 3.930000e-02         NA        NA        NA        NA      14.30         NA
    ## 237 6.450000e-02         NA        NA        NA        NA       2.00         NA
    ## 238 8.420000e-02         NA        NA        NA        NA       3.70         NA
    ## 239 7.540000e-02         NA        NA        NA        NA       3.20         NA
    ## 240 1.080000e-01         NA        NA        NA        NA       4.70         NA
    ## 241 8.240000e-02         NA        NA        NA        NA       3.80         NA
    ## 242 8.290000e-02         NA        NA        NA        NA       3.50         NA
    ## 243 3.100000e-03         NA        NA        NA        NA       0.30         NA
    ## 244 1.500000e-03         NA        NA        NA        NA       0.10         NA
    ## 245 3.000000e-03         NA        NA        NA        NA       0.30         NA
    ## 246 1.900000e-03         NA        NA        NA        NA       0.10         NA
    ## 247 1.900000e-03         NA        NA        NA        NA       0.10         NA
    ## 248 2.300000e-03         NA        NA        NA        NA       0.20         NA
    ## 249 2.270000e-02         NA        NA        NA        NA       1.40         NA
    ## 250 2.080000e-02         NA        NA        NA        NA       1.40         NA
    ## 251 2.770000e-02         NA        NA        NA        NA       1.70         NA
    ## 252 2.540000e-02         NA        NA        NA        NA       1.40         NA
    ## 253 3.640000e-02         NA        NA        NA        NA       1.50         NA
    ## 254 2.200000e-02         NA        NA        NA        NA       1.30         NA
    ## 255 3.070000e-02         NA        NA        NA        NA       1.40         NA
    ## 256 2.650000e-02         NA        NA        NA        NA       1.50         NA
    ## 257 4.500000e-03         NA        NA        NA        NA       1.70         NA
    ## 258 7.800000e-03         NA        NA        NA        NA       1.80         NA
    ## 259 6.800000e-03         NA        NA        NA        NA       2.10         NA
    ## 260 6.300000e-03         NA        NA        NA        NA       1.90         NA
    ## 261 9.900000e-03         NA        NA        NA        NA       1.80         NA
    ## 262 7.000000e-03         NA        NA        NA        NA       1.90         NA
    ## 263 6.300000e-03         NA        NA        NA        NA       0.40         NA
    ## 264 5.900000e-03         NA        NA        NA        NA       0.30         NA
    ## 265 7.900000e-03         NA        NA        NA        NA       0.60         NA
    ## 266 6.300000e-03         NA        NA        NA        NA       0.60         NA
    ## 267 6.800000e-03         NA        NA        NA        NA       0.50         NA
    ## 268 6.700000e-03         NA        NA        NA        NA       0.50         NA
    ## 269 1.170000e-02         NA        NA        NA        NA       1.00         NA
    ## 270 1.080000e-02         NA        NA        NA        NA       0.60         NA
    ## 271 1.030000e-02         NA        NA        NA        NA       0.70         NA
    ## 272 9.400000e-03         NA        NA        NA        NA       0.70         NA
    ## 273 9.100000e-03         NA        NA        NA        NA       0.60         NA
    ## 274 1.030000e-02         NA        NA        NA        NA       0.70         NA
    ## 275 7.500000e-03         NA        NA        NA        NA       0.70         NA
    ## 276 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 277 8.800000e-03         NA        NA        NA        NA       0.70         NA
    ## 278 7.100000e-03         NA        NA        NA        NA       0.10         NA
    ## 279 3.000000e-04         NA        NA        NA        NA       0.40         NA
    ## 280 5.200000e-03         NA        NA        NA        NA       0.60         NA
    ## 281 2.800000e-02         NA        NA        NA        NA       1.20         NA
    ## 282 3.770000e-02         NA        NA        NA        NA       1.50         NA
    ## 283 2.810000e-02         NA        NA        NA        NA       0.60         NA
    ## 284 3.090000e-02         NA        NA        NA        NA       1.20         NA
    ## 285 3.180000e-02         NA        NA        NA        NA       0.60         NA
    ## 286 3.130000e-02         NA        NA        NA        NA       1.00         NA
    ## 287 3.160000e-02         NA        NA        NA        NA       8.60         NA
    ## 288 3.070000e-02         NA        NA        NA        NA       1.40         NA
    ## 289 2.460000e-02         NA        NA        NA        NA       1.30         NA
    ## 290 2.260000e-02         NA        NA        NA        NA       1.50         NA
    ## 291 2.490000e-02         NA        NA        NA        NA       1.40         NA
    ## 292 2.690000e-02         NA        NA        NA        NA       1.40         NA
    ## 293 6.360000e-01         NA        NA        NA        NA    2190.00         NA
    ## 294 2.752000e+00         NA        NA        NA        NA         NA         NA
    ## 295 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 296 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 297 1.000000e-01         NA        NA        NA        NA         NA         NA
    ## 298 1.400000e+00         NA        NA        NA        NA         NA         NA
    ## 299 4.900000e-01         NA        NA        NA        NA         NA         NA
    ## 300 6.000000e+00         NA        NA        NA        NA         NA         NA
    ## 301 1.600000e+01         NA        NA        NA        NA         NA         NA
    ## 302 1.600000e+01         NA        NA        NA        NA         NA         NA
    ## 303 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 304 7.000000e-02         NA        NA        NA        NA         NA         NA
    ## 305 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 306 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 307 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 308 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 309 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 310 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 311 1.100000e-01         NA        NA        NA        NA         NA         NA
    ## 312 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 313 1.600000e-01         NA        NA        NA        NA         NA         NA
    ## 314 3.300000e-01         NA        NA        NA        NA         NA         NA
    ## 315 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 316 2.200000e-01         NA        NA        NA        NA         NA         NA
    ## 317 1.200000e-01         NA        NA        NA        NA         NA         NA
    ## 318 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 319 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 320 1.000000e-01         NA        NA        NA        NA         NA         NA
    ## 321 9.000000e-02         NA        NA        NA        NA         NA         NA
    ## 322 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 323 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 324 7.000000e-02         NA        NA        NA        NA         NA         NA
    ## 325 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 326 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 327 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 328 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 329 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 330 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 331 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 332 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 333 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 334 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 335 6.300000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 336 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 337 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 338 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 339 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 340 1.300000e-01         NA     0.000     0.000        NA         NA         NA
    ## 341 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 342 6.600000e-01   0.560000        NA        NA        NA         NA         NA
    ## 343 6.600000e-01   0.040000        NA        NA        NA         NA         NA
    ## 344 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 345 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 346 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 347 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 348 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 349 8.400000e-01         NA        NA        NA        NA         NA         NA
    ## 350 1.700000e-01         NA        NA        NA        NA         NA         NA
    ## 351 1.700000e-01         NA        NA        NA        NA         NA         NA
    ## 352 8.400000e-01         NA        NA        NA        NA         NA         NA
    ## 353 8.400000e-01         NA        NA        NA        NA         NA         NA
    ## 354 8.400000e-01         NA        NA        NA        NA         NA         NA
    ## 355 8.400000e-01         NA        NA        NA        NA         NA         NA
    ## 356 7.000000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 357 7.000000e-01         NA     0.000     0.000        NA         NA         NA
    ## 358 7.000000e-01         NA     0.000        NA        NA         NA         NA
    ## 359 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 360 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 361 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 362 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 363 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 364 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 365 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 366 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 367 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 368 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 369 7.700000e-01         NA        NA        NA        NA         NA         NA
    ## 370 1.300000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 371 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 372 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 373 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 374 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 375 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 376 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 377 6.600000e-01   0.130000     0.000     0.000        NA         NA         NA
    ## 378 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 379 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 380 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 381 6.600000e-01         NA        NA        NA        NA         NA         NA
    ## 382 1.100000e+00         NA        NA        NA        NA         NA         NA
    ## 383 1.100000e+00         NA        NA        NA        NA         NA         NA
    ## 384 6.400000e-01   0.150000     0.000     0.000        NA         NA         NA
    ## 385 6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 386 6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 387 6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 388 6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 389 1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 390 1.000000e+00         NA        NA        NA        NA         NA         NA
    ## 391 7.300000e-01   0.053000     0.000     0.000        NA         NA         NA
    ## 392 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 393 7.000000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 394 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 395 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 396 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 397 7.000000e-01         NA        NA        NA        NA         NA         NA
    ## 398 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 399 1.400000e-01         NA        NA        NA        NA         NA         NA
    ## 400 6.300000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 401 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 402 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 403 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 404 6.300000e-01         NA        NA        NA        NA         NA         NA
    ## 405 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 406 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 407 2.500000e-01   0.040000     0.000     0.000        NA         NA         NA
    ## 408 2.500000e-01         NA        NA        NA        NA         NA         NA
    ## 409 2.500000e-01         NA        NA        NA        NA         NA         NA
    ## 410 2.500000e-01         NA        NA        NA        NA         NA         NA
    ## 411 2.500000e-01         NA        NA        NA        NA         NA         NA
    ## 412 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 413 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 414 1.300000e-01   0.040000        NA        NA        NA         NA         NA
    ## 415 1.300000e-01         NA     0.000     0.000        NA         NA         NA
    ## 416 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 417 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 418 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 419 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 420 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 421 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 422 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 423 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 424 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 425 7.300000e-01         NA        NA        NA        NA         NA         NA
    ## 426 1.800000e-02         NA        NA        NA        NA         NA         NA
    ## 427 1.510000e-02         NA        NA        NA        NA         NA         NA
    ## 428 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 429 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 430 2.250000e-02         NA        NA        NA        NA         NA         NA
    ## 431 1.610000e-02         NA        NA        NA        NA         NA         NA
    ## 432 3.000000e-04         NA        NA        NA        NA         NA         NA
    ## 433 2.300000e-03         NA        NA        NA        NA         NA         NA
    ## 434 3.300000e-03         NA        NA        NA        NA         NA         NA
    ## 435 1.800000e-03         NA        NA        NA        NA         NA         NA
    ## 436 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 437 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 438 3.100000e-03         NA        NA        NA        NA         NA         NA
    ## 439 3.600000e-03         NA        NA        NA        NA         NA         NA
    ## 440 2.100000e-03         NA        NA        NA        NA         NA         NA
    ## 441 1.200000e-03         NA        NA        NA        NA         NA         NA
    ## 442 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 443 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 444 1.300000e-03         NA        NA        NA        NA         NA         NA
    ## 445 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 446 2.300000e-03         NA        NA        NA        NA         NA         NA
    ## 447 4.700000e-03         NA        NA        NA        NA         NA         NA
    ## 448 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 449 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 450 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 451 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 452 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 453 1.180000e-02         NA        NA        NA        NA         NA         NA
    ## 454 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 455 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 456 1.180000e-02         NA        NA        NA        NA         NA         NA
    ## 457 5.800000e-03         NA        NA        NA        NA         NA         NA
    ## 458 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 459 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 460 1.520000e-02         NA        NA        NA        NA         NA         NA
    ## 461 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 462 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 463 9.000000e-04         NA        NA        NA        NA         NA         NA
    ## 464 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 465 1.100000e-03         NA        NA        NA        NA         NA         NA
    ## 466 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 467 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 468 9.000000e-04         NA        NA        NA        NA         NA         NA
    ## 469 1.100000e-03         NA        NA        NA        NA         NA         NA
    ## 470 1.400000e-03         NA        NA        NA        NA         NA         NA
    ## 471 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 472 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 473 3.100000e-03         NA        NA        NA        NA         NA         NA
    ## 474 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 475 9.000000e-04         NA        NA        NA        NA         NA         NA
    ## 476 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 477 2.100000e-03         NA        NA        NA        NA         NA         NA
    ## 478 2.000000e-04         NA        NA        NA        NA         NA         NA
    ## 479 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 480 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 481 2.000000e-04         NA        NA        NA        NA         NA         NA
    ## 482 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 483 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 484 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 485 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 486 2.100000e-03         NA        NA        NA        NA         NA         NA
    ## 487 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 488 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 489 3.300000e-03         NA        NA        NA        NA         NA         NA
    ## 490 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 491 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 492 6.500000e-03         NA        NA        NA        NA         NA         NA
    ## 493 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 494 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 495 7.900000e-03         NA        NA        NA        NA         NA         NA
    ## 496 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 497 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 498 1.280000e-02         NA        NA        NA        NA         NA         NA
    ## 499 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 500 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 501 1.070000e-02         NA        NA        NA        NA         NA         NA
    ## 502 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 503 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 504 1.877000e-01         NA        NA        NA        NA         NA         NA
    ## 505 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 506 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 507 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 508 1.600000e-03         NA        NA        NA        NA         NA         NA
    ## 509 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 510 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 511 3.300000e-03         NA        NA        NA        NA         NA         NA
    ## 512 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 513 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 514 1.390000e-02         NA        NA        NA        NA         NA         NA
    ## 515 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 516 2.500000e-03         NA        NA        NA        NA         NA         NA
    ## 517 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 518 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 519 8.000000e-04         NA        NA        NA        NA         NA         NA
    ## 520 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 521 7.000000e-04         NA        NA        NA        NA         NA         NA
    ## 522 4.900000e-03         NA        NA        NA        NA         NA         NA
    ## 523 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 524 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 525 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 526 5.600000e-03         NA        NA        NA        NA         NA         NA
    ## 527 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 528 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 529 2.100000e-03         NA        NA        NA        NA         NA         NA
    ## 530 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 531 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 532 6.200000e-03         NA        NA        NA        NA         NA         NA
    ## 533 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 534 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 535 3.500000e-03         NA        NA        NA        NA         NA         NA
    ## 536 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 537 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 538 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 539 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 540 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 541 4.300000e-03         NA        NA        NA        NA         NA         NA
    ## 542 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 543 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 544 4.800000e-03         NA        NA        NA        NA         NA         NA
    ## 545 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 546 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 547 2.300000e-02         NA        NA        NA        NA         NA         NA
    ## 548 6.600000e-03         NA        NA        NA        NA         NA         NA
    ## 549 2.610000e-02         NA        NA        NA        NA         NA         NA
    ## 550 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 551 3.050000e-02         NA        NA        NA        NA         NA         NA
    ## 552 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 553 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 554 2.160000e-02         NA        NA        NA        NA         NA         NA
    ## 555 2.200000e-02         NA        NA        NA        NA         NA         NA
    ## 556 5.400000e-02         NA        NA        NA        NA         NA         NA
    ## 557 1.300000e-01         NA        NA        NA        NA         NA         NA
    ## 558 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 559 1.310000e-01         NA        NA        NA        NA         NA         NA
    ## 560 1.330000e-01         NA        NA        NA        NA         NA         NA
    ## 561 2.600000e-02         NA        NA        NA        NA         NA         NA
    ## 562 2.200000e-02         NA        NA        NA        NA         NA         NA
    ## 563 1.200000e-02         NA        NA        NA        NA         NA         NA
    ## 564 1.420000e-01         NA        NA        NA        NA         NA         NA
    ## 565 1.500000e-02         NA        NA        NA        NA         NA         NA
    ## 566 2.700000e-02         NA        NA        NA        NA         NA         NA
    ## 567 5.900000e-02         NA        NA        NA        NA         NA         NA
    ## 568 1.300000e-02         NA        NA        NA        NA         NA         NA
    ## 569 2.100000e-02         NA        NA        NA        NA         NA         NA
    ## 570 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 571 9.000000e-03         NA        NA        NA        NA         NA         NA
    ## 572 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 573 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 574 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 575 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 576 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 577 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 578 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 579 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 580 1.140000e+00         NA        NA        NA        NA         NA         NA
    ## 581 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 582 7.600000e-01         NA        NA        NA        NA         NA   18.75000
    ## 583 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 584 3.600000e-01         NA        NA        NA        NA         NA         NA
    ## 585 8.100000e-01         NA        NA        NA        NA         NA         NA
    ## 586 6.000000e-02         NA        NA        NA        NA         NA         NA
    ## 587 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 588 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 589 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 590 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 591 2.500000e-01         NA        NA        NA        NA         NA    3.30000
    ## 592 3.300000e-01         NA        NA        NA        NA         NA    0.00000
    ## 593 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 594 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 595 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 596 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 597 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 598 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 599 5.000000e+00         NA        NA        NA        NA         NA         NA
    ## 600 5.000000e+00         NA        NA        NA        NA         NA         NA
    ## 601 6.400000e-02   0.000000        NA        NA        NA         NA   21.55100
    ## 602 7.400000e-02   0.000000        NA        NA        NA         NA   21.67400
    ## 603 7.400000e-02   0.000000        NA        NA        NA         NA   13.92100
    ## 604 7.700000e-02   0.000000        NA        NA        NA         NA    2.39500
    ## 605 1.000000e-01   0.000000        NA        NA        NA         NA    2.91700
    ## 606 1.320000e-01   0.000000        NA        NA        NA         NA    9.55300
    ## 607 5.700000e-02   0.000000        NA        NA        NA         NA    4.28200
    ## 608 1.600000e-01         NA        NA        NA        NA         NA         NA
    ## 609 3.300000e-01         NA        NA        NA        NA         NA         NA
    ## 610 4.040000e-01         NA        NA        NA        NA         NA         NA
    ## 611 2.920000e-01         NA        NA        NA        NA         NA         NA
    ## 612 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 613 7.200000e-02   1.000000        NA        NA        NA         NA         NA
    ## 614 1.660000e-01   1.000000        NA        NA        NA         NA         NA
    ## 615 1.290000e-01   8.000000        NA        NA        NA         NA         NA
    ## 616 2.430000e-01  36.000000        NA        NA        NA         NA         NA
    ## 617 1.800000e-02   1.000000        NA        NA        NA         NA         NA
    ## 618 3.650000e-01  11.000000        NA        NA        NA         NA         NA
    ## 619 9.400000e-02  36.000000        NA        NA        NA         NA         NA
    ## 620 1.700000e-01  29.000000        NA        NA        NA         NA         NA
    ## 621 1.370000e-01   7.000000        NA        NA        NA         NA         NA
    ## 622 1.400000e-01   9.300000        NA        NA        NA         NA         NA
    ## 623 1.000000e-01   4.400000        NA        NA        NA         NA         NA
    ## 624 1.060000e+00  47.000000        NA        NA        NA         NA         NA
    ## 625 8.400000e-02   2.300000        NA        NA        NA         NA         NA
    ## 626 7.000000e-01  42.000000        NA        NA        NA         NA         NA
    ## 627 1.120000e+00 163.000000        NA        NA        NA         NA         NA
    ## 628 6.000000e-01         NA        NA        NA        NA         NA         NA
    ## 629 8.600000e-01         NA        NA        NA        NA         NA         NA
    ## 630 1.200000e+00         NA        NA        NA        NA         NA         NA
    ## 631 6.900000e-01         NA        NA        NA        NA         NA         NA
    ## 632 1.520000e+00         NA        NA        NA        NA         NA         NA
    ## 633 4.600000e-01         NA        NA        NA        NA         NA         NA
    ## 634 1.500000e+00         NA        NA        NA        NA         NA         NA
    ## 635 1.100000e+00         NA        NA        NA        NA         NA         NA
    ## 636 8.000000e-01         NA        NA        NA        NA         NA         NA
    ## 637 2.000000e-01         NA        NA        NA        NA         NA         NA
    ## 638 7.970000e-01         NA        NA        NA        NA         NA   15.30000
    ## 639 2.240000e-01         NA        NA        NA        NA         NA   12.00000
    ## 640 3.580000e-01         NA        NA        NA        NA         NA   15.50000
    ## 641 1.630000e-01         NA        NA        NA        NA         NA   13.90000
    ## 642 1.140000e-01         NA        NA        NA     FALSE         NA   22.10000
    ## 643 1.350000e-01         NA        NA        NA        NA         NA   10.70000
    ## 644 1.390000e+00         NA        NA        NA        NA         NA   23.20000
    ## 645 7.670000e-02         NA        NA        NA        NA         NA    6.91000
    ## 646 3.720000e-02         NA        NA        NA        NA         NA    5.58000
    ## 647 3.740000e-02         NA        NA        NA        NA         NA    6.57000
    ## 648 3.820000e-02         NA        NA        NA        NA         NA    4.34000
    ## 649 3.140000e-02         NA        NA        NA        NA         NA    5.07000
    ## 650 2.710000e-02         NA        NA        NA        NA         NA    5.29000
    ## 651 3.680000e-02         NA        NA        NA        NA         NA    6.76000
    ## 652 6.400000e-01         NA        NA        NA        NA         NA         NA
    ## 653 5.100000e-01         NA        NA        NA        NA         NA         NA
    ## 654 2.800000e-01         NA        NA        NA        NA         NA         NA
    ## 655 3.000000e-01         NA        NA        NA        NA         NA         NA
    ## 656 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 657 1.900000e-01         NA        NA        NA        NA         NA         NA
    ## 658 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 659 5.000000e-02         NA        NA        NA        NA         NA         NA
    ## 660 2.200000e-01         NA        NA        NA        NA         NA         NA
    ## 661 6.000000e-02         NA        NA        NA        NA         NA         NA
    ## 662 4.000000e-02         NA        NA        NA        NA         NA         NA
    ## 663 8.000000e-02         NA        NA        NA        NA         NA         NA
    ## 664 1.000000e-01         NA        NA        NA        NA         NA         NA
    ## 665 2.100000e-01         NA        NA        NA        NA         NA         NA
    ## 666 4.300000e-01         NA        NA        NA        NA         NA         NA
    ## 667 4.000000e-02         NA        NA        NA        NA         NA         NA
    ## 668 6.700000e-01         NA        NA        NA        NA         NA         NA
    ## 669 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 670 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 671 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 672 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 673 3.200000e-02         NA        NA        NA        NA         NA         NA
    ## 674 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 675 1.300000e-02         NA        NA        NA        NA         NA         NA
    ## 676 4.000000e-03         NA        NA        NA        NA         NA         NA
    ## 677 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 678 1.400000e-02         NA        NA        NA        NA         NA         NA
    ## 679 3.600000e-02         NA        NA        NA        NA         NA         NA
    ## 680 3.600000e-02         NA        NA        NA        NA         NA         NA
    ## 681 3.200000e-02         NA        NA        NA        NA         NA         NA
    ## 682 4.100000e-02         NA        NA        NA        NA         NA         NA
    ## 683 4.700000e-02         NA        NA        NA        NA         NA         NA
    ## 684 3.100000e-02         NA        NA        NA        NA         NA         NA
    ## 685 1.500000e-02         NA        NA        NA        NA         NA         NA
    ## 686 1.200000e-02         NA        NA        NA        NA         NA         NA
    ## 687 1.900000e-02         NA        NA        NA        NA         NA         NA
    ## 688 3.700000e-02         NA        NA        NA        NA         NA         NA
    ## 689 2.900000e-02         NA        NA        NA        NA         NA         NA
    ## 690 3.600000e-02         NA        NA        NA        NA         NA         NA
    ## 691 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 692 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 693 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 694 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 695 2.100000e-02         NA        NA        NA        NA         NA         NA
    ## 696 2.200000e-02         NA        NA        NA        NA         NA         NA
    ## 697 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 698 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 699 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 700 1.400000e-02         NA        NA        NA        NA         NA         NA
    ## 701 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 702 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 703 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 704 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 705 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 706 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 707 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 708 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 709 1.000000e-03         NA        NA        NA        NA         NA         NA
    ## 710 4.400000e-02         NA        NA        NA        NA         NA         NA
    ## 711 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 712 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 713 2.300000e-02         NA        NA        NA        NA         NA         NA
    ## 714 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 715 1.300000e-02         NA        NA        NA        NA         NA         NA
    ## 716 2.400000e-02         NA        NA        NA        NA         NA         NA
    ## 717 8.000000e-03         NA        NA        NA        NA         NA         NA
    ## 718 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 719 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 720 1.100000e-02         NA        NA        NA        NA         NA         NA
    ## 721 1.100000e-02         NA        NA        NA        NA         NA         NA
    ## 722 9.000000e-03         NA        NA        NA        NA         NA         NA
    ## 723 1.800000e-02         NA        NA        NA        NA         NA         NA
    ## 724 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 725 3.400000e-02         NA        NA        NA        NA         NA         NA
    ## 726 3.600000e-02         NA        NA        NA        NA         NA         NA
    ## 727 2.500000e-02         NA        NA        NA        NA         NA         NA
    ## 728 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 729 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 730 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 731 1.100000e-02         NA        NA        NA        NA         NA         NA
    ## 732 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 733 1.500000e-02         NA        NA        NA        NA         NA         NA
    ## 734 2.100000e-02         NA        NA        NA        NA         NA         NA
    ## 735 8.000000e-03         NA        NA        NA        NA         NA         NA
    ## 736 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 737 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 738 4.000000e-03         NA        NA        NA        NA         NA         NA
    ## 739 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 740 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 741 1.200000e-02         NA        NA        NA        NA         NA         NA
    ## 742 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 743 8.000000e-03         NA        NA        NA        NA         NA         NA
    ## 744 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 745 2.400000e-02         NA        NA        NA        NA         NA         NA
    ## 746 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 747 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 748 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 749 2.000000e-03         NA        NA        NA        NA         NA         NA
    ## 750 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 751 4.000000e-03         NA        NA        NA        NA         NA         NA
    ## 752 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 753 5.000000e-03         NA        NA        NA        NA         NA         NA
    ## 754 9.000000e-03         NA        NA        NA        NA         NA         NA
    ## 755 1.600000e-02         NA        NA        NA        NA         NA         NA
    ## 756 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 757 3.100000e-02         NA        NA        NA        NA         NA         NA
    ## 758 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 759 2.300000e-02         NA        NA        NA        NA         NA         NA
    ## 760 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 761 3.300000e-02         NA        NA        NA        NA         NA         NA
    ## 762 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 763 4.000000e-03         NA        NA        NA        NA         NA         NA
    ## 764 8.000000e-03         NA        NA        NA        NA         NA         NA
    ## 765 4.000000e-02         NA        NA        NA        NA         NA         NA
    ## 766 3.800000e-02         NA        NA        NA        NA         NA         NA
    ## 767 3.000000e-02         NA        NA        NA        NA         NA         NA
    ## 768 3.100000e-02         NA        NA        NA        NA         NA         NA
    ## 769 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 770 1.400000e-02         NA        NA        NA        NA         NA         NA
    ## 771 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 772 7.000000e-03         NA        NA        NA        NA         NA         NA
    ## 773 9.000000e-03         NA        NA        NA        NA         NA         NA
    ## 774 2.900000e-02         NA        NA        NA        NA         NA         NA
    ## 775 2.000000e-02         NA        NA        NA        NA         NA         NA
    ## 776 3.100000e-02         NA        NA        NA        NA         NA         NA
    ## 777 1.100000e-02         NA        NA        NA        NA         NA         NA
    ## 778 1.500000e-02         NA        NA        NA        NA         NA         NA
    ## 779 1.400000e-02         NA        NA        NA        NA         NA         NA
    ## 780 3.200000e-02         NA        NA        NA        NA         NA         NA
    ## 781 4.200000e-02         NA        NA        NA        NA         NA         NA
    ## 782 2.100000e-02         NA        NA        NA        NA         NA         NA
    ## 783 1.500000e-02         NA        NA        NA        NA         NA         NA
    ## 784 9.000000e-03         NA        NA        NA        NA         NA         NA
    ## 785 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 786 2.100000e-02         NA        NA        NA        NA         NA         NA
    ## 787 4.700000e-02         NA        NA        NA        NA         NA         NA
    ## 788 1.700000e-02         NA        NA        NA        NA         NA         NA
    ## 789 1.400000e-02         NA        NA        NA        NA         NA         NA
    ## 790 1.800000e-02         NA        NA        NA        NA         NA         NA
    ## 791 1.700000e-02         NA        NA        NA        NA         NA         NA
    ## 792 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 793 8.000000e-03         NA        NA        NA        NA         NA         NA
    ## 794 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 795 3.000000e-03         NA        NA        NA        NA         NA         NA
    ## 796 4.000000e-03         NA        NA        NA        NA         NA         NA
    ## 797 6.000000e-03         NA        NA        NA        NA         NA         NA
    ## 798 1.000000e-02         NA        NA        NA        NA         NA         NA
    ## 799 1.200000e-02         NA        NA        NA        NA         NA         NA
    ## 800 1.200000e-02         NA        NA        NA        NA         NA         NA
    ## 801 9.336899e-02   7.880641        NA        NA        NA         NA    6.21092
    ## 802 5.495571e-02   7.073304        NA        NA        NA         NA    1.71800
    ## 803 1.036671e-01   8.862175        NA        NA        NA         NA    3.47656
    ## 804 3.979226e-02   5.157832        NA        NA        NA         NA    2.90277
    ## 805 1.396994e-01  13.304775        NA        NA        NA         NA    5.78125
    ## 806 2.023465e-01  10.391232        NA        NA        NA         NA    7.95201
    ## 807 2.731970e-01  13.934569        NA        NA        NA         NA    8.46793
    ## 808 1.370842e-01  17.183135        NA        NA        NA         NA    4.83015
    ## 809 2.206964e-01  25.068820        NA        NA        NA         NA   27.41791
    ## 810 2.445237e-01  26.571098        NA        NA        NA         NA    8.44959
    ## 811 1.279174e-01   7.987967        NA        NA        NA         NA    2.23777
    ## 812 2.988552e-01  15.478185        NA        NA        NA         NA    4.71710
    ## 813 1.563419e-01   8.818167        NA        NA        NA         NA    9.70154
    ## 814 2.856147e-01  16.473663        NA        NA        NA         NA    9.97442
    ## 815 7.373708e-02   5.331315        NA        NA        NA         NA    5.03447
    ## 816 2.988395e-01  26.018973        NA        NA        NA         NA   20.77525
    ## 817 2.415681e-01  23.126761        NA        NA        NA         NA   13.99272
    ## 818 1.718975e-01  11.003652        NA        NA        NA         NA   25.62249
    ## 819 3.255409e-01  23.014837        NA        NA        NA         NA   16.00799
    ## 820 1.256435e-01  19.523081        NA        NA        NA         NA   46.44532
    ## 821 4.919805e-01  24.423375        NA        NA        NA         NA   29.54397
    ## 822 2.181242e-01  12.543636        NA        NA        NA         NA   22.61669
    ## 823 4.275052e-01  24.370349        NA        NA        NA         NA   27.06151
    ## 824 3.874238e-01  25.519965        NA        NA        NA         NA   18.98001
    ## 825 4.214625e-01  21.896717        NA        NA        NA         NA   19.90737
    ## 826 7.867322e-01  38.335429        NA        NA        NA         NA   40.00415
    ## 827 3.649005e-01  27.657106        NA        NA        NA         NA   14.13225
    ## 828 1.754052e-01  13.540276        NA        NA        NA         NA   10.20416
    ## 829 1.701479e-01  19.824549        NA        NA        NA         NA   15.68454
    ## 830 8.326123e-01  41.545837        NA        NA        NA         NA   31.74582
    ## 831 1.200000e-01   0.000000        NA        NA        NA         NA         NA
    ## 832 1.100000e-01   0.000000        NA        NA        NA         NA         NA
    ## 833 1.400000e-01   0.000000        NA        NA        NA         NA         NA
    ## 834 2.300000e-01   0.000000        NA        NA        NA         NA         NA
    ## 835 7.000000e-02   0.000000        NA        NA        NA         NA         NA
    ## 836 2.000000e-01   0.000000        NA        NA        NA         NA         NA
    ## 837 1.000000e-01   0.000000        NA        NA        NA         NA         NA
    ## 838 1.500000e-01   0.000000        NA        NA        NA         NA         NA
    ## 839 3.000000e-01   0.000000        NA        NA        NA         NA         NA
    ## 840 0.000000e+00   0.000000        NA        NA        NA         NA         NA
    ## 841 0.000000e+00         NA        NA        NA        NA         NA   32.37000
    ## 842 0.000000e+00         NA        NA        NA        NA         NA   23.40000
    ## 843 0.000000e+00         NA        NA        NA        NA         NA   21.44000
    ## 844 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 845 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 846 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 847 0.000000e+00         NA        NA        NA        NA         NA    0.45600
    ## 848 0.000000e+00         NA        NA        NA        NA         NA    8.37400
    ## 849 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 850 0.000000e+00         NA        NA        NA        NA         NA    0.47700
    ## 851 0.000000e+00         NA        NA        NA        NA         NA    1.53000
    ## 852 0.000000e+00         NA        NA        NA        NA         NA    0.39400
    ## 853 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 854 0.000000e+00         NA        NA        NA        NA         NA    1.38900
    ## 855 0.000000e+00         NA        NA        NA        NA         NA    1.05300
    ## 856 0.000000e+00         NA        NA        NA        NA         NA    1.32300
    ## 857 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 858 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 859 1.230000e+00         NA        NA        NA        NA         NA         NA
    ## 860 2.000000e-01         NA        NA        NA        NA         NA         NA
    ## 861 5.600000e-02         NA        NA        NA        NA         NA         NA
    ## 862 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 863 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 864 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 865 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 866 1.870000e+00         NA        NA        NA        NA         NA         NA
    ## 867 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 868 2.440000e+00         NA        NA        NA        NA         NA         NA
    ## 869 4.400000e-01         NA        NA        NA        NA         NA         NA
    ## 870 1.890000e+00         NA        NA        NA        NA         NA         NA
    ## 871 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 872 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 873 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 874 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 875 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 876 3.481600e+02         NA        NA        NA        NA         NA         NA
    ## 877 2.479100e+02         NA        NA        NA        NA         NA         NA
    ## 878 1.430000e-01         NA        NA        NA        NA         NA         NA
    ## 879 2.600000e-03         NA        NA        NA        NA         NA         NA
    ## 880 1.880000e-01         NA        NA        NA        NA         NA         NA
    ## 881 0.000000e+00   0.000000     0.000     0.000        NA         NA         NA
    ## 882 0.000000e+00   0.000000     0.000     0.000        NA         NA         NA
    ## 883 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 884 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 885 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 886 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 887 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 888 0.000000e+00         NA        NA        NA        NA         NA         NA
    ## 889 0.000000e+00         NA        NA        NA        NA         NA   16.29000
    ## 890 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 891 0.000000e+00         NA        NA        NA        NA         NA    3.65000
    ## 892 0.000000e+00         NA        NA        NA        NA         NA    0.46200
    ## 893 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 894 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 895 0.000000e+00         NA        NA        NA        NA         NA    7.43000
    ## 896 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 897 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 898 0.000000e+00         NA        NA        NA        NA         NA    0.34000
    ## 899 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 900 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 901 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 902 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 903 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 904 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 905 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 906 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 907 0.000000e+00         NA        NA        NA        NA         NA    5.47000
    ## 908 0.000000e+00         NA        NA        NA        NA         NA    0.00000
    ## 909 1.400000e-01   0.000000        NA        NA        NA         NA         NA
    ## 910 1.700000e-01   0.000000        NA        NA        NA         NA         NA
    ## 911 2.600000e-01   0.000000        NA        NA        NA         NA         NA
    ## 912 6.315000e-02         NA        NA        NA        NA         NA         NA
    ## 913 7.666000e-02         NA        NA        NA        NA         NA         NA
    ## 914 5.448000e-02         NA        NA        NA        NA         NA         NA
    ## 915 6.453000e-02         NA        NA        NA        NA         NA         NA
    ## 916 6.010000e-02         NA        NA        NA        NA         NA         NA
    ## 917 6.567000e-02         NA        NA        NA        NA         NA         NA
    ## 918 6.593000e-02         NA        NA        NA        NA         NA         NA
    ## 919 7.625000e-02         NA        NA        NA        NA         NA         NA
    ## 920 5.412000e-02         NA        NA        NA        NA         NA         NA
    ## 921 6.186000e-02         NA        NA        NA        NA         NA         NA
    ## 922 8.935000e-02         NA        NA        NA        NA         NA         NA
    ## 923 1.082500e-01         NA        NA        NA        NA         NA         NA
    ## 924 1.047000e-01         NA        NA        NA        NA         NA         NA
    ## 925 8.547000e-02         NA        NA        NA        NA         NA         NA
    ## 926 8.453000e-02         NA        NA        NA        NA         NA         NA
    ## 927 9.927000e-02         NA        NA        NA        NA         NA         NA
    ## 928 8.340000e-02         NA        NA        NA        NA         NA         NA
    ##     LIPIDS_NGG CLOST_SP_G   MBT_C   DBT_C     TBT_C    TTBT_C
    ## 1           NA         NA      NA      NA        NA        NA
    ## 2           NA         NA      NA      NA        NA        NA
    ## 3           NA         NA      NA      NA        NA        NA
    ## 4           NA         NA      NA      NA        NA        NA
    ## 5           NA         NA      NA      NA        NA        NA
    ## 6           NA         NA      NA      NA        NA        NA
    ## 7           NA         NA      NA      NA        NA        NA
    ## 8           NA         NA      NA      NA        NA        NA
    ## 9           NA         NA      NA      NA        NA        NA
    ## 10          NA         NA      NA      NA        NA        NA
    ## 11          NA         NA      NA      NA        NA        NA
    ## 12          NA         NA      NA      NA        NA        NA
    ## 13          NA         NA      NA      NA        NA        NA
    ## 14          NA         NA      NA      NA        NA        NA
    ## 15          NA         NA      NA      NA        NA        NA
    ## 16          NA         NA      NA      NA        NA        NA
    ## 17          NA         NA      NA      NA        NA        NA
    ## 18          NA         NA      NA      NA        NA        NA
    ## 19          NA         NA      NA      NA        NA        NA
    ## 20          NA         NA      NA      NA        NA        NA
    ## 21          NA         NA      NA      NA        NA        NA
    ## 22          NA         NA      NA      NA        NA        NA
    ## 23          NA         NA      NA      NA        NA        NA
    ## 24          NA         NA      NA      NA        NA        NA
    ## 25          NA         NA      NA      NA        NA        NA
    ## 26          NA         NA      NA      NA        NA        NA
    ## 27          NA         NA      NA      NA        NA        NA
    ## 28          NA         NA      NA      NA        NA        NA
    ## 29          NA         NA      NA      NA        NA        NA
    ## 30          NA         NA      NA      NA        NA        NA
    ## 31          NA         NA      NA      NA        NA        NA
    ## 32          NA         NA      NA      NA        NA        NA
    ## 33          NA         NA      NA      NA        NA        NA
    ## 34          NA         NA      NA      NA        NA        NA
    ## 35          NA         NA      NA      NA        NA        NA
    ## 36          NA         NA      NA      NA        NA        NA
    ## 37          NA         NA      NA      NA        NA        NA
    ## 38          NA         NA      NA      NA        NA        NA
    ## 39          NA         NA      NA      NA        NA        NA
    ## 40          NA         NA      NA      NA        NA        NA
    ## 41          NA         NA      NA      NA        NA        NA
    ## 42          NA         NA      NA      NA        NA        NA
    ## 43          NA         NA      NA      NA        NA        NA
    ## 44          NA         NA      NA      NA        NA        NA
    ## 45          NA         NA      NA      NA        NA        NA
    ## 46          NA         NA      NA      NA        NA        NA
    ## 47          NA         NA      NA      NA        NA        NA
    ## 48          NA         NA      NA      NA        NA        NA
    ## 49          NA         NA      NA      NA        NA        NA
    ## 50          NA         NA      NA      NA        NA        NA
    ## 51          NA         NA      NA      NA        NA        NA
    ## 52          NA         NA      NA      NA        NA        NA
    ## 53          NA         NA      NA      NA        NA        NA
    ## 54          NA         NA      NA      NA        NA        NA
    ## 55          NA         NA      NA      NA        NA        NA
    ## 56          NA         NA      NA      NA        NA        NA
    ## 57          NA         NA      NA      NA        NA        NA
    ## 58          NA         NA      NA      NA        NA        NA
    ## 59          NA         NA      NA      NA        NA        NA
    ## 60          NA         NA      NA      NA        NA        NA
    ## 61          NA         NA      NA      NA        NA        NA
    ## 62          NA         NA      NA      NA        NA        NA
    ## 63          NA         NA      NA      NA        NA        NA
    ## 64          NA         NA      NA      NA        NA        NA
    ## 65          NA         NA      NA      NA        NA        NA
    ## 66          NA         NA      NA      NA        NA        NA
    ## 67          NA         NA      NA      NA        NA        NA
    ## 68          NA         NA      NA      NA        NA        NA
    ## 69          NA         NA      NA      NA        NA        NA
    ## 70          NA         NA      NA      NA        NA        NA
    ## 71          NA         NA      NA      NA        NA        NA
    ## 72          NA         NA      NA      NA        NA        NA
    ## 73          NA         NA      NA      NA        NA        NA
    ## 74          NA         NA      NA      NA        NA        NA
    ## 75          NA         NA      NA      NA        NA        NA
    ## 76          NA         NA      NA      NA        NA        NA
    ## 77          NA         NA      NA      NA        NA        NA
    ## 78          NA         NA      NA      NA        NA        NA
    ## 79          NA         NA      NA      NA        NA        NA
    ## 80          NA         NA      NA      NA        NA        NA
    ## 81          NA         NA      NA      NA        NA        NA
    ## 82          NA         NA      NA      NA        NA        NA
    ## 83          NA         NA      NA      NA        NA        NA
    ## 84          NA         NA      NA      NA        NA        NA
    ## 85          NA         NA      NA      NA        NA        NA
    ## 86          NA         NA      NA      NA        NA        NA
    ## 87          NA         NA      NA      NA        NA        NA
    ## 88          NA         NA      NA      NA        NA        NA
    ## 89          NA         NA      NA      NA        NA        NA
    ## 90          NA         NA      NA      NA        NA        NA
    ## 91          NA         NA      NA      NA        NA        NA
    ## 92          NA         NA      NA      NA        NA        NA
    ## 93          NA         NA      NA      NA        NA        NA
    ## 94          NA         NA      NA      NA        NA        NA
    ## 95          NA         NA      NA      NA        NA        NA
    ## 96          NA         NA      NA      NA        NA        NA
    ## 97          NA         NA      NA      NA        NA        NA
    ## 98          NA         NA      NA      NA        NA        NA
    ## 99          NA         NA      NA      NA        NA        NA
    ## 100         NA         NA      NA      NA        NA        NA
    ## 101         NA         NA      NA      NA        NA        NA
    ## 102         NA         NA      NA      NA        NA        NA
    ## 103         NA         NA      NA      NA        NA        NA
    ## 104         NA         NA      NA      NA        NA        NA
    ## 105         NA         NA      NA      NA        NA        NA
    ## 106         NA         NA      NA      NA        NA        NA
    ## 107         NA         NA      NA      NA        NA        NA
    ## 108         NA         NA      NA      NA        NA        NA
    ## 109         NA         NA      NA      NA        NA        NA
    ## 110         NA         NA      NA      NA        NA        NA
    ## 111         NA         NA      NA      NA        NA        NA
    ## 112         NA         NA      NA      NA        NA        NA
    ## 113         NA         NA      NA      NA        NA        NA
    ## 114         NA         NA      NA      NA        NA        NA
    ## 115         NA         NA      NA      NA        NA        NA
    ## 116         NA         NA      NA      NA        NA        NA
    ## 117         NA         NA      NA      NA        NA        NA
    ## 118         NA         NA      NA      NA        NA        NA
    ## 119         NA         NA      NA      NA        NA        NA
    ## 120         NA         NA      NA      NA        NA        NA
    ## 121         NA         NA      NA      NA        NA        NA
    ## 122         NA         NA      NA      NA        NA        NA
    ## 123         NA         NA      NA      NA        NA        NA
    ## 124         NA         NA      NA      NA        NA        NA
    ## 125         NA         NA      NA      NA        NA        NA
    ## 126         NA         NA      NA      NA        NA        NA
    ## 127         NA         NA      NA      NA        NA        NA
    ## 128         NA         NA      NA      NA        NA        NA
    ## 129         NA         NA      NA      NA        NA        NA
    ## 130         NA         NA      NA      NA        NA        NA
    ## 131         NA         NA      NA      NA        NA        NA
    ## 132         NA         NA      NA      NA        NA        NA
    ## 133         NA         NA      NA      NA        NA        NA
    ## 134         NA         NA      NA      NA        NA        NA
    ## 135         NA         NA      NA      NA        NA        NA
    ## 136         NA         NA      NA      NA        NA        NA
    ## 137         NA         NA      NA      NA        NA        NA
    ## 138         NA         NA      NA      NA        NA        NA
    ## 139         NA         NA      NA      NA        NA        NA
    ## 140         NA         NA      NA      NA        NA        NA
    ## 141         NA         NA      NA      NA        NA        NA
    ## 142         NA         NA      NA      NA        NA        NA
    ## 143         NA         NA      NA      NA        NA        NA
    ## 144         NA         NA      NA      NA        NA        NA
    ## 145         NA         NA      NA      NA        NA        NA
    ## 146         NA         NA      NA      NA        NA        NA
    ## 147         NA         NA      NA      NA        NA        NA
    ## 148         NA         NA      NA      NA        NA        NA
    ## 149         NA         NA      NA      NA        NA        NA
    ## 150         NA         NA      NA      NA        NA        NA
    ## 151         NA         NA      NA      NA        NA        NA
    ## 152         NA         NA      NA      NA        NA        NA
    ## 153         NA         NA      NA      NA        NA        NA
    ## 154         NA         NA      NA      NA        NA        NA
    ## 155         NA         NA      NA      NA        NA        NA
    ## 156         NA         NA      NA      NA        NA        NA
    ## 157         NA         NA      NA      NA        NA        NA
    ## 158         NA         NA      NA      NA        NA        NA
    ## 159         NA         NA      NA      NA        NA        NA
    ## 160         NA         NA      NA      NA        NA        NA
    ## 161         NA         NA      NA      NA        NA        NA
    ## 162         NA         NA      NA      NA        NA        NA
    ## 163         NA         NA      NA      NA        NA        NA
    ## 164         NA         NA      NA      NA        NA        NA
    ## 165         NA         NA      NA      NA        NA        NA
    ## 166         NA         NA      NA      NA        NA        NA
    ## 167         NA         NA      NA      NA        NA        NA
    ## 168         NA         NA      NA      NA        NA        NA
    ## 169         NA         NA      NA      NA        NA        NA
    ## 170         NA         NA      NA      NA        NA        NA
    ## 171         NA         NA      NA      NA        NA        NA
    ## 172         NA         NA      NA      NA        NA        NA
    ## 173         NA         NA      NA      NA        NA        NA
    ## 174         NA         NA      NA      NA        NA        NA
    ## 175         NA         NA      NA      NA        NA        NA
    ## 176         NA         NA      NA      NA        NA        NA
    ## 177         NA         NA      NA      NA        NA        NA
    ## 178         NA         NA      NA      NA        NA        NA
    ## 179         NA         NA      NA      NA        NA        NA
    ## 180         NA         NA      NA      NA        NA        NA
    ## 181         NA         NA      NA      NA        NA        NA
    ## 182         NA         NA      NA      NA        NA        NA
    ## 183         NA         NA      NA      NA        NA        NA
    ## 184         NA         NA      NA      NA        NA        NA
    ## 185         NA         NA      NA      NA        NA        NA
    ## 186         NA         NA      NA      NA        NA        NA
    ## 187         NA         NA      NA      NA        NA        NA
    ## 188         NA         NA      NA      NA        NA        NA
    ## 189         NA         NA      NA      NA        NA        NA
    ## 190         NA         NA      NA      NA        NA        NA
    ## 191         NA         NA      NA      NA        NA        NA
    ## 192         NA         NA      NA      NA        NA        NA
    ## 193         NA         NA      NA      NA        NA        NA
    ## 194         NA         NA      NA      NA        NA        NA
    ## 195         NA         NA      NA      NA        NA        NA
    ## 196         NA         NA      NA      NA        NA        NA
    ## 197         NA         NA      NA      NA        NA        NA
    ## 198         NA         NA      NA      NA        NA        NA
    ## 199         NA         NA      NA      NA        NA        NA
    ## 200         NA         NA      NA      NA        NA        NA
    ## 201         NA         NA      NA      NA        NA        NA
    ## 202         NA         NA      NA      NA        NA        NA
    ## 203         NA         NA      NA      NA        NA        NA
    ## 204         NA         NA      NA      NA        NA        NA
    ## 205         NA         NA      NA      NA        NA        NA
    ## 206         NA         NA      NA      NA        NA        NA
    ## 207         NA         NA      NA      NA        NA        NA
    ## 208         NA         NA      NA      NA        NA        NA
    ## 209         NA         NA      NA      NA        NA        NA
    ## 210         NA         NA      NA      NA        NA        NA
    ## 211         NA         NA      NA      NA        NA        NA
    ## 212         NA         NA      NA      NA        NA        NA
    ## 213         NA         NA      NA      NA        NA        NA
    ## 214         NA         NA      NA      NA        NA        NA
    ## 215         NA         NA      NA      NA        NA        NA
    ## 216         NA         NA      NA      NA        NA        NA
    ## 217         NA         NA      NA      NA        NA        NA
    ## 218         NA         NA      NA      NA        NA        NA
    ## 219         NA         NA      NA      NA        NA        NA
    ## 220         NA         NA      NA      NA        NA        NA
    ## 221         NA         NA      NA      NA        NA        NA
    ## 222         NA         NA      NA      NA        NA        NA
    ## 223         NA         NA      NA      NA        NA        NA
    ## 224         NA         NA      NA      NA        NA        NA
    ## 225         NA         NA      NA      NA        NA        NA
    ## 226         NA         NA      NA      NA        NA        NA
    ## 227         NA         NA      NA      NA        NA        NA
    ## 228         NA         NA      NA      NA        NA        NA
    ## 229         NA         NA      NA      NA        NA        NA
    ## 230         NA         NA      NA      NA        NA        NA
    ## 231         NA         NA      NA      NA        NA        NA
    ## 232         NA         NA      NA      NA        NA        NA
    ## 233         NA         NA      NA      NA        NA        NA
    ## 234         NA         NA      NA      NA        NA        NA
    ## 235         NA         NA      NA      NA        NA        NA
    ## 236         NA         NA      NA      NA        NA        NA
    ## 237         NA         NA      NA      NA        NA        NA
    ## 238         NA         NA      NA      NA        NA        NA
    ## 239         NA         NA      NA      NA        NA        NA
    ## 240         NA         NA      NA      NA        NA        NA
    ## 241         NA         NA      NA      NA        NA        NA
    ## 242         NA         NA      NA      NA        NA        NA
    ## 243         NA         NA      NA      NA        NA        NA
    ## 244         NA         NA      NA      NA        NA        NA
    ## 245         NA         NA      NA      NA        NA        NA
    ## 246         NA         NA      NA      NA        NA        NA
    ## 247         NA         NA      NA      NA        NA        NA
    ## 248         NA         NA      NA      NA        NA        NA
    ## 249         NA         NA      NA      NA        NA        NA
    ## 250         NA         NA      NA      NA        NA        NA
    ## 251         NA         NA      NA      NA        NA        NA
    ## 252         NA         NA      NA      NA        NA        NA
    ## 253         NA         NA      NA      NA        NA        NA
    ## 254         NA         NA      NA      NA        NA        NA
    ## 255         NA         NA      NA      NA        NA        NA
    ## 256         NA         NA      NA      NA        NA        NA
    ## 257         NA         NA      NA      NA        NA        NA
    ## 258         NA         NA      NA      NA        NA        NA
    ## 259         NA         NA      NA      NA        NA        NA
    ## 260         NA         NA      NA      NA        NA        NA
    ## 261         NA         NA      NA      NA        NA        NA
    ## 262         NA         NA      NA      NA        NA        NA
    ## 263         NA         NA      NA      NA        NA        NA
    ## 264         NA         NA      NA      NA        NA        NA
    ## 265         NA         NA      NA      NA        NA        NA
    ## 266         NA         NA      NA      NA        NA        NA
    ## 267         NA         NA      NA      NA        NA        NA
    ## 268         NA         NA      NA      NA        NA        NA
    ## 269         NA         NA      NA      NA        NA        NA
    ## 270         NA         NA      NA      NA        NA        NA
    ## 271         NA         NA      NA      NA        NA        NA
    ## 272         NA         NA      NA      NA        NA        NA
    ## 273         NA         NA      NA      NA        NA        NA
    ## 274         NA         NA      NA      NA        NA        NA
    ## 275         NA         NA      NA      NA        NA        NA
    ## 276         NA         NA      NA      NA        NA        NA
    ## 277         NA         NA      NA      NA        NA        NA
    ## 278         NA         NA      NA      NA        NA        NA
    ## 279         NA         NA      NA      NA        NA        NA
    ## 280         NA         NA      NA      NA        NA        NA
    ## 281         NA         NA      NA      NA        NA        NA
    ## 282         NA         NA      NA      NA        NA        NA
    ## 283         NA         NA      NA      NA        NA        NA
    ## 284         NA         NA      NA      NA        NA        NA
    ## 285         NA         NA      NA      NA        NA        NA
    ## 286         NA         NA      NA      NA        NA        NA
    ## 287         NA         NA      NA      NA        NA        NA
    ## 288         NA         NA      NA      NA        NA        NA
    ## 289         NA         NA      NA      NA        NA        NA
    ## 290         NA         NA      NA      NA        NA        NA
    ## 291         NA         NA      NA      NA        NA        NA
    ## 292         NA         NA      NA      NA        NA        NA
    ## 293         NA         NA      NA      NA        NA        NA
    ## 294         NA         NA      NA      NA        NA        NA
    ## 295         NA         NA      NA      NA        NA        NA
    ## 296         NA         NA      NA      NA        NA        NA
    ## 297         NA         NA      NA      NA        NA        NA
    ## 298         NA         NA      NA      NA        NA        NA
    ## 299         NA         NA      NA      NA        NA        NA
    ## 300         NA         NA      NA      NA        NA        NA
    ## 301         NA         NA      NA      NA        NA        NA
    ## 302         NA         NA      NA      NA        NA        NA
    ## 303         NA         NA      NA      NA        NA        NA
    ## 304         NA         NA      NA      NA        NA        NA
    ## 305         NA         NA      NA      NA        NA        NA
    ## 306         NA         NA      NA      NA        NA        NA
    ## 307         NA         NA      NA      NA        NA        NA
    ## 308         NA         NA      NA      NA        NA        NA
    ## 309         NA         NA      NA      NA        NA        NA
    ## 310         NA         NA      NA      NA        NA        NA
    ## 311         NA         NA      NA      NA        NA        NA
    ## 312         NA         NA      NA      NA        NA        NA
    ## 313         NA         NA      NA      NA        NA        NA
    ## 314         NA         NA      NA      NA        NA        NA
    ## 315         NA         NA      NA      NA        NA        NA
    ## 316         NA         NA      NA      NA        NA        NA
    ## 317         NA         NA      NA      NA        NA        NA
    ## 318         NA         NA      NA      NA        NA        NA
    ## 319         NA         NA      NA      NA        NA        NA
    ## 320         NA         NA      NA      NA        NA        NA
    ## 321         NA         NA      NA      NA        NA        NA
    ## 322         NA         NA      NA      NA        NA        NA
    ## 323         NA         NA      NA      NA        NA        NA
    ## 324         NA         NA      NA      NA        NA        NA
    ## 325         NA         NA      NA      NA        NA        NA
    ## 326         NA         NA      NA      NA        NA        NA
    ## 327         NA         NA      NA      NA        NA        NA
    ## 328         NA         NA      NA      NA        NA        NA
    ## 329         NA         NA      NA      NA        NA        NA
    ## 330         NA         NA      NA      NA        NA        NA
    ## 331         NA         NA      NA      NA        NA        NA
    ## 332         NA         NA      NA      NA        NA        NA
    ## 333         NA         NA      NA      NA        NA        NA
    ## 334         NA         NA      NA      NA        NA        NA
    ## 335         NA         NA      NA      NA        NA        NA
    ## 336         NA         NA      NA      NA        NA        NA
    ## 337         NA         NA      NA      NA        NA        NA
    ## 338         NA         NA      NA      NA        NA        NA
    ## 339         NA         NA      NA      NA        NA        NA
    ## 340         NA         NA      NA      NA        NA        NA
    ## 341         NA         NA      NA      NA        NA        NA
    ## 342         NA         NA      NA      NA        NA        NA
    ## 343         NA         NA      NA      NA        NA        NA
    ## 344         NA         NA      NA      NA        NA        NA
    ## 345         NA         NA      NA      NA        NA        NA
    ## 346         NA         NA      NA      NA        NA        NA
    ## 347         NA         NA      NA      NA        NA        NA
    ## 348         NA         NA      NA      NA        NA        NA
    ## 349         NA         NA      NA      NA        NA        NA
    ## 350         NA         NA      NA      NA        NA        NA
    ## 351         NA         NA      NA      NA        NA        NA
    ## 352         NA         NA      NA      NA        NA        NA
    ## 353         NA         NA      NA      NA        NA        NA
    ## 354         NA         NA      NA      NA        NA        NA
    ## 355         NA         NA      NA      NA        NA        NA
    ## 356         NA         NA      NA      NA        NA        NA
    ## 357         NA         NA      NA      NA        NA        NA
    ## 358         NA         NA      NA      NA        NA        NA
    ## 359         NA         NA      NA      NA        NA        NA
    ## 360         NA         NA      NA      NA        NA        NA
    ## 361         NA         NA      NA      NA        NA        NA
    ## 362         NA         NA      NA      NA        NA        NA
    ## 363         NA         NA      NA      NA        NA        NA
    ## 364         NA         NA      NA      NA        NA        NA
    ## 365         NA         NA      NA      NA        NA        NA
    ## 366         NA         NA      NA      NA        NA        NA
    ## 367         NA         NA      NA      NA        NA        NA
    ## 368         NA         NA      NA      NA        NA        NA
    ## 369         NA         NA      NA      NA        NA        NA
    ## 370         NA         NA      NA      NA        NA        NA
    ## 371         NA         NA      NA      NA        NA        NA
    ## 372         NA         NA      NA      NA        NA        NA
    ## 373         NA         NA      NA      NA        NA        NA
    ## 374         NA         NA      NA      NA        NA        NA
    ## 375         NA         NA      NA      NA        NA        NA
    ## 376         NA         NA      NA      NA        NA        NA
    ## 377         NA         NA      NA      NA        NA        NA
    ## 378         NA         NA      NA      NA        NA        NA
    ## 379         NA         NA      NA      NA        NA        NA
    ## 380         NA         NA      NA      NA        NA        NA
    ## 381         NA         NA      NA      NA        NA        NA
    ## 382         NA         NA      NA      NA        NA        NA
    ## 383         NA         NA      NA      NA        NA        NA
    ## 384         NA         NA      NA      NA        NA        NA
    ## 385         NA         NA      NA      NA        NA        NA
    ## 386         NA         NA      NA      NA        NA        NA
    ## 387         NA         NA      NA      NA        NA        NA
    ## 388         NA         NA      NA      NA        NA        NA
    ## 389         NA         NA      NA      NA        NA        NA
    ## 390         NA         NA      NA      NA        NA        NA
    ## 391         NA         NA      NA      NA        NA        NA
    ## 392         NA         NA      NA      NA        NA        NA
    ## 393         NA         NA      NA      NA        NA        NA
    ## 394         NA         NA      NA      NA        NA        NA
    ## 395         NA         NA      NA      NA        NA        NA
    ## 396         NA         NA      NA      NA        NA        NA
    ## 397         NA         NA      NA      NA        NA        NA
    ## 398         NA         NA      NA      NA        NA        NA
    ## 399         NA         NA      NA      NA        NA        NA
    ## 400         NA         NA      NA      NA        NA        NA
    ## 401         NA         NA      NA      NA        NA        NA
    ## 402         NA         NA      NA      NA        NA        NA
    ## 403         NA         NA      NA      NA        NA        NA
    ## 404         NA         NA      NA      NA        NA        NA
    ## 405         NA         NA      NA      NA        NA        NA
    ## 406         NA         NA      NA      NA        NA        NA
    ## 407         NA         NA      NA      NA        NA        NA
    ## 408         NA         NA      NA      NA        NA        NA
    ## 409         NA         NA      NA      NA        NA        NA
    ## 410         NA         NA      NA      NA        NA        NA
    ## 411         NA         NA      NA      NA        NA        NA
    ## 412         NA         NA      NA      NA        NA        NA
    ## 413         NA         NA      NA      NA        NA        NA
    ## 414         NA         NA      NA      NA        NA        NA
    ## 415         NA         NA      NA      NA        NA        NA
    ## 416         NA         NA      NA      NA        NA        NA
    ## 417         NA         NA      NA      NA        NA        NA
    ## 418         NA         NA      NA      NA        NA        NA
    ## 419         NA         NA      NA      NA        NA        NA
    ## 420         NA         NA      NA      NA        NA        NA
    ## 421         NA         NA      NA      NA        NA        NA
    ## 422         NA         NA      NA      NA        NA        NA
    ## 423         NA         NA      NA      NA        NA        NA
    ## 424         NA         NA      NA      NA        NA        NA
    ## 425         NA         NA      NA      NA        NA        NA
    ## 426         NA         NA      NA      NA        NA        NA
    ## 427         NA         NA      NA      NA        NA        NA
    ## 428         NA         NA      NA      NA        NA        NA
    ## 429         NA         NA      NA      NA        NA        NA
    ## 430         NA         NA      NA      NA        NA        NA
    ## 431         NA         NA      NA      NA        NA        NA
    ## 432         NA         NA      NA      NA        NA        NA
    ## 433         NA         NA      NA      NA        NA        NA
    ## 434         NA         NA      NA      NA        NA        NA
    ## 435         NA         NA      NA      NA        NA        NA
    ## 436         NA         NA      NA      NA        NA        NA
    ## 437         NA         NA      NA      NA        NA        NA
    ## 438         NA         NA      NA      NA        NA        NA
    ## 439         NA         NA      NA      NA        NA        NA
    ## 440         NA         NA      NA      NA        NA        NA
    ## 441         NA         NA      NA      NA        NA        NA
    ## 442         NA         NA      NA      NA        NA        NA
    ## 443         NA         NA      NA      NA        NA        NA
    ## 444         NA         NA      NA      NA        NA        NA
    ## 445         NA         NA      NA      NA        NA        NA
    ## 446         NA         NA      NA      NA        NA        NA
    ## 447         NA         NA      NA      NA        NA        NA
    ## 448         NA         NA      NA      NA        NA        NA
    ## 449         NA         NA      NA      NA        NA        NA
    ## 450         NA         NA      NA      NA        NA        NA
    ## 451         NA         NA      NA      NA        NA        NA
    ## 452         NA         NA      NA      NA        NA        NA
    ## 453         NA         NA      NA      NA        NA        NA
    ## 454         NA         NA      NA      NA        NA        NA
    ## 455         NA         NA      NA      NA        NA        NA
    ## 456         NA         NA      NA      NA        NA        NA
    ## 457         NA         NA      NA      NA        NA        NA
    ## 458         NA         NA      NA      NA        NA        NA
    ## 459         NA         NA      NA      NA        NA        NA
    ## 460         NA         NA      NA      NA        NA        NA
    ## 461         NA         NA      NA      NA        NA        NA
    ## 462         NA         NA      NA      NA        NA        NA
    ## 463         NA         NA      NA      NA        NA        NA
    ## 464         NA         NA      NA      NA        NA        NA
    ## 465         NA         NA      NA      NA        NA        NA
    ## 466         NA         NA      NA      NA        NA        NA
    ## 467         NA         NA      NA      NA        NA        NA
    ## 468         NA         NA      NA      NA        NA        NA
    ## 469         NA         NA      NA      NA        NA        NA
    ## 470         NA         NA      NA      NA        NA        NA
    ## 471         NA         NA      NA      NA        NA        NA
    ## 472         NA         NA      NA      NA        NA        NA
    ## 473         NA         NA      NA      NA        NA        NA
    ## 474         NA         NA      NA      NA        NA        NA
    ## 475         NA         NA      NA      NA        NA        NA
    ## 476         NA         NA      NA      NA        NA        NA
    ## 477         NA         NA      NA      NA        NA        NA
    ## 478         NA         NA      NA      NA        NA        NA
    ## 479         NA         NA      NA      NA        NA        NA
    ## 480         NA         NA      NA      NA        NA        NA
    ## 481         NA         NA      NA      NA        NA        NA
    ## 482         NA         NA      NA      NA        NA        NA
    ## 483         NA         NA      NA      NA        NA        NA
    ## 484         NA         NA      NA      NA        NA        NA
    ## 485         NA         NA      NA      NA        NA        NA
    ## 486         NA         NA      NA      NA        NA        NA
    ## 487         NA         NA      NA      NA        NA        NA
    ## 488         NA         NA      NA      NA        NA        NA
    ## 489         NA         NA      NA      NA        NA        NA
    ## 490         NA         NA      NA      NA        NA        NA
    ## 491         NA         NA      NA      NA        NA        NA
    ## 492         NA         NA      NA      NA        NA        NA
    ## 493         NA         NA      NA      NA        NA        NA
    ## 494         NA         NA      NA      NA        NA        NA
    ## 495         NA         NA      NA      NA        NA        NA
    ## 496         NA         NA      NA      NA        NA        NA
    ## 497         NA         NA      NA      NA        NA        NA
    ## 498         NA         NA      NA      NA        NA        NA
    ## 499         NA         NA      NA      NA        NA        NA
    ## 500         NA         NA      NA      NA        NA        NA
    ## 501         NA         NA      NA      NA        NA        NA
    ## 502         NA         NA      NA      NA        NA        NA
    ## 503         NA         NA      NA      NA        NA        NA
    ## 504         NA         NA      NA      NA        NA        NA
    ## 505         NA         NA      NA      NA        NA        NA
    ## 506         NA         NA      NA      NA        NA        NA
    ## 507         NA         NA      NA      NA        NA        NA
    ## 508         NA         NA      NA      NA        NA        NA
    ## 509         NA         NA      NA      NA        NA        NA
    ## 510         NA         NA      NA      NA        NA        NA
    ## 511         NA         NA      NA      NA        NA        NA
    ## 512         NA         NA      NA      NA        NA        NA
    ## 513         NA         NA      NA      NA        NA        NA
    ## 514         NA         NA      NA      NA        NA        NA
    ## 515         NA         NA      NA      NA        NA        NA
    ## 516         NA         NA      NA      NA        NA        NA
    ## 517         NA         NA      NA      NA        NA        NA
    ## 518         NA         NA      NA      NA        NA        NA
    ## 519         NA         NA      NA      NA        NA        NA
    ## 520         NA         NA      NA      NA        NA        NA
    ## 521         NA         NA      NA      NA        NA        NA
    ## 522         NA         NA      NA      NA        NA        NA
    ## 523         NA         NA      NA      NA        NA        NA
    ## 524         NA         NA      NA      NA        NA        NA
    ## 525         NA         NA      NA      NA        NA        NA
    ## 526         NA         NA      NA      NA        NA        NA
    ## 527         NA         NA      NA      NA        NA        NA
    ## 528         NA         NA      NA      NA        NA        NA
    ## 529         NA         NA      NA      NA        NA        NA
    ## 530         NA         NA      NA      NA        NA        NA
    ## 531         NA         NA      NA      NA        NA        NA
    ## 532         NA         NA      NA      NA        NA        NA
    ## 533         NA         NA      NA      NA        NA        NA
    ## 534         NA         NA      NA      NA        NA        NA
    ## 535         NA         NA      NA      NA        NA        NA
    ## 536         NA         NA      NA      NA        NA        NA
    ## 537         NA         NA      NA      NA        NA        NA
    ## 538         NA         NA      NA      NA        NA        NA
    ## 539         NA         NA      NA      NA        NA        NA
    ## 540         NA         NA      NA      NA        NA        NA
    ## 541         NA         NA      NA      NA        NA        NA
    ## 542         NA         NA      NA      NA        NA        NA
    ## 543         NA         NA      NA      NA        NA        NA
    ## 544         NA         NA      NA      NA        NA        NA
    ## 545         NA         NA      NA      NA        NA        NA
    ## 546         NA         NA      NA      NA        NA        NA
    ## 547         NA         NA      NA      NA        NA        NA
    ## 548         NA         NA      NA      NA        NA        NA
    ## 549         NA         NA      NA      NA        NA        NA
    ## 550         NA         NA      NA      NA        NA        NA
    ## 551         NA         NA      NA      NA        NA        NA
    ## 552         NA         NA      NA      NA        NA        NA
    ## 553         NA         NA      NA      NA        NA        NA
    ## 554         NA         NA      NA      NA        NA        NA
    ## 555         NA         NA      NA      NA        NA        NA
    ## 556         NA         NA      NA      NA        NA        NA
    ## 557         NA         NA      NA      NA        NA        NA
    ## 558         NA         NA      NA      NA        NA        NA
    ## 559         NA         NA      NA      NA        NA        NA
    ## 560         NA         NA      NA      NA        NA        NA
    ## 561         NA         NA      NA      NA        NA        NA
    ## 562         NA         NA      NA      NA        NA        NA
    ## 563         NA         NA      NA      NA        NA        NA
    ## 564         NA         NA      NA      NA        NA        NA
    ## 565         NA         NA      NA      NA        NA        NA
    ## 566         NA         NA      NA      NA        NA        NA
    ## 567         NA         NA      NA      NA        NA        NA
    ## 568         NA         NA      NA      NA        NA        NA
    ## 569         NA         NA      NA      NA        NA        NA
    ## 570         NA         NA      NA      NA        NA        NA
    ## 571         NA         NA      NA      NA        NA        NA
    ## 572         NA         NA      NA      NA        NA        NA
    ## 573         NA         NA      NA      NA        NA        NA
    ## 574         NA         NA      NA      NA        NA        NA
    ## 575         NA         NA      NA      NA        NA        NA
    ## 576         NA         NA      NA      NA        NA        NA
    ## 577         NA         NA      NA      NA        NA        NA
    ## 578         NA         NA      NA      NA        NA        NA
    ## 579         NA         NA      NA      NA        NA        NA
    ## 580         NA         NA      NA      NA        NA        NA
    ## 581         NA         NA      NA      NA        NA        NA
    ## 582         NA         NA      NA      NA        NA        NA
    ## 583         NA         NA      NA      NA        NA        NA
    ## 584         NA         NA      NA      NA        NA        NA
    ## 585         NA         NA      NA      NA        NA        NA
    ## 586         NA         NA      NA      NA        NA        NA
    ## 587         NA         NA      NA      NA        NA        NA
    ## 588         NA         NA      NA      NA        NA        NA
    ## 589         NA         NA      NA      NA        NA        NA
    ## 590         NA         NA      NA      NA        NA        NA
    ## 591         NA         NA      NA      NA        NA        NA
    ## 592         NA         NA      NA      NA        NA        NA
    ## 593         NA         NA      NA      NA        NA        NA
    ## 594         NA         NA      NA      NA        NA        NA
    ## 595         NA         NA      NA      NA        NA        NA
    ## 596         NA         NA      NA      NA        NA        NA
    ## 597         NA         NA      NA      NA        NA        NA
    ## 598         NA         NA      NA      NA        NA        NA
    ## 599         NA         NA      NA      NA        NA        NA
    ## 600         NA         NA      NA      NA        NA        NA
    ## 601         NA         NA      NA      NA        NA        NA
    ## 602         NA         NA      NA      NA        NA        NA
    ## 603         NA         NA      NA      NA        NA        NA
    ## 604         NA         NA      NA      NA        NA        NA
    ## 605         NA         NA      NA      NA        NA        NA
    ## 606         NA         NA      NA      NA        NA        NA
    ## 607         NA         NA      NA      NA        NA        NA
    ## 608         NA         NA      NA      NA        NA        NA
    ## 609         NA         NA      NA      NA        NA        NA
    ## 610         NA         NA      NA      NA        NA        NA
    ## 611         NA         NA      NA      NA        NA        NA
    ## 612         NA         NA      NA      NA        NA        NA
    ## 613         NA         NA      NA      NA        NA        NA
    ## 614         NA         NA      NA      NA        NA        NA
    ## 615         NA         NA      NA      NA        NA        NA
    ## 616         NA         NA      NA      NA        NA        NA
    ## 617         NA         NA      NA      NA        NA        NA
    ## 618         NA         NA      NA      NA        NA        NA
    ## 619         NA         NA      NA      NA        NA        NA
    ## 620         NA         NA      NA      NA        NA        NA
    ## 621         NA         NA      NA      NA        NA        NA
    ## 622         NA         NA      NA      NA        NA        NA
    ## 623         NA         NA      NA      NA        NA        NA
    ## 624         NA         NA      NA      NA        NA        NA
    ## 625         NA         NA      NA      NA        NA        NA
    ## 626         NA         NA      NA      NA        NA        NA
    ## 627         NA         NA      NA      NA        NA        NA
    ## 628         NA         NA      NA      NA        NA        NA
    ## 629         NA         NA      NA      NA        NA        NA
    ## 630         NA         NA      NA      NA        NA        NA
    ## 631         NA         NA      NA      NA        NA        NA
    ## 632         NA         NA      NA      NA        NA        NA
    ## 633         NA         NA      NA      NA        NA        NA
    ## 634         NA         NA      NA      NA        NA        NA
    ## 635         NA         NA      NA      NA        NA        NA
    ## 636         NA         NA      NA      NA        NA        NA
    ## 637         NA         NA      NA      NA        NA        NA
    ## 638         NA         NA      NA      NA        NA        NA
    ## 639         NA         NA      NA      NA        NA        NA
    ## 640         NA         NA      NA      NA        NA        NA
    ## 641         NA         NA      NA      NA        NA        NA
    ## 642         NA         NA      NA      NA        NA        NA
    ## 643         NA         NA      NA      NA        NA        NA
    ## 644         NA         NA      NA      NA        NA        NA
    ## 645         NA         NA      NA      NA        NA        NA
    ## 646         NA         NA      NA      NA        NA        NA
    ## 647         NA         NA      NA      NA        NA        NA
    ## 648         NA         NA      NA      NA        NA        NA
    ## 649         NA         NA      NA      NA        NA        NA
    ## 650         NA         NA      NA      NA        NA        NA
    ## 651         NA         NA      NA      NA        NA        NA
    ## 652         NA         NA      NA      NA        NA        NA
    ## 653         NA         NA      NA      NA        NA        NA
    ## 654         NA         NA      NA      NA        NA        NA
    ## 655         NA         NA      NA      NA        NA        NA
    ## 656         NA         NA      NA      NA        NA        NA
    ## 657         NA         NA      NA      NA        NA        NA
    ## 658         NA         NA      NA      NA        NA        NA
    ## 659         NA         NA      NA      NA        NA        NA
    ## 660         NA         NA      NA      NA        NA        NA
    ## 661         NA         NA      NA      NA        NA        NA
    ## 662         NA         NA      NA      NA        NA        NA
    ## 663         NA         NA      NA      NA        NA        NA
    ## 664         NA         NA      NA      NA        NA        NA
    ## 665         NA         NA      NA      NA        NA        NA
    ## 666         NA         NA      NA      NA        NA        NA
    ## 667         NA         NA      NA      NA        NA        NA
    ## 668         NA         NA      NA      NA        NA        NA
    ## 669         NA         NA      NA      NA        NA        NA
    ## 670         NA         NA      NA      NA        NA        NA
    ## 671         NA         NA      NA      NA        NA        NA
    ## 672         NA         NA      NA      NA        NA        NA
    ## 673         NA         NA      NA      NA        NA        NA
    ## 674         NA         NA      NA      NA        NA        NA
    ## 675         NA         NA      NA      NA        NA        NA
    ## 676         NA         NA      NA      NA        NA        NA
    ## 677         NA         NA      NA      NA        NA        NA
    ## 678         NA         NA      NA      NA        NA        NA
    ## 679         NA         NA      NA      NA        NA        NA
    ## 680         NA         NA      NA      NA        NA        NA
    ## 681         NA         NA      NA      NA        NA        NA
    ## 682         NA         NA      NA      NA        NA        NA
    ## 683         NA         NA      NA      NA        NA        NA
    ## 684         NA         NA      NA      NA        NA        NA
    ## 685         NA         NA      NA      NA        NA        NA
    ## 686         NA         NA      NA      NA        NA        NA
    ## 687         NA         NA      NA      NA        NA        NA
    ## 688         NA         NA      NA      NA        NA        NA
    ## 689         NA         NA      NA      NA        NA        NA
    ## 690         NA         NA      NA      NA        NA        NA
    ## 691         NA         NA      NA      NA        NA        NA
    ## 692         NA         NA      NA      NA        NA        NA
    ## 693         NA         NA      NA      NA        NA        NA
    ## 694         NA         NA      NA      NA        NA        NA
    ## 695         NA         NA      NA      NA        NA        NA
    ## 696         NA         NA      NA      NA        NA        NA
    ## 697         NA         NA      NA      NA        NA        NA
    ## 698         NA         NA      NA      NA        NA        NA
    ## 699         NA         NA      NA      NA        NA        NA
    ## 700         NA         NA      NA      NA        NA        NA
    ## 701         NA         NA      NA      NA        NA        NA
    ## 702         NA         NA      NA      NA        NA        NA
    ## 703         NA         NA      NA      NA        NA        NA
    ## 704         NA         NA      NA      NA        NA        NA
    ## 705         NA         NA      NA      NA        NA        NA
    ## 706         NA         NA      NA      NA        NA        NA
    ## 707         NA         NA      NA      NA        NA        NA
    ## 708         NA         NA      NA      NA        NA        NA
    ## 709         NA         NA      NA      NA        NA        NA
    ## 710         NA         NA      NA      NA        NA        NA
    ## 711         NA         NA      NA      NA        NA        NA
    ## 712         NA         NA      NA      NA        NA        NA
    ## 713         NA         NA      NA      NA        NA        NA
    ## 714         NA         NA      NA      NA        NA        NA
    ## 715         NA         NA      NA      NA        NA        NA
    ## 716         NA         NA      NA      NA        NA        NA
    ## 717         NA         NA      NA      NA        NA        NA
    ## 718         NA         NA      NA      NA        NA        NA
    ## 719         NA         NA      NA      NA        NA        NA
    ## 720         NA         NA      NA      NA        NA        NA
    ## 721         NA         NA      NA      NA        NA        NA
    ## 722         NA         NA      NA      NA        NA        NA
    ## 723         NA         NA      NA      NA        NA        NA
    ## 724         NA         NA      NA      NA        NA        NA
    ## 725         NA         NA      NA      NA        NA        NA
    ## 726         NA         NA      NA      NA        NA        NA
    ## 727         NA         NA      NA      NA        NA        NA
    ## 728         NA         NA      NA      NA        NA        NA
    ## 729         NA         NA      NA      NA        NA        NA
    ## 730         NA         NA      NA      NA        NA        NA
    ## 731         NA         NA      NA      NA        NA        NA
    ## 732         NA         NA      NA      NA        NA        NA
    ## 733         NA         NA      NA      NA        NA        NA
    ## 734         NA         NA      NA      NA        NA        NA
    ## 735         NA         NA      NA      NA        NA        NA
    ## 736         NA         NA      NA      NA        NA        NA
    ## 737         NA         NA      NA      NA        NA        NA
    ## 738         NA         NA      NA      NA        NA        NA
    ## 739         NA         NA      NA      NA        NA        NA
    ## 740         NA         NA      NA      NA        NA        NA
    ## 741         NA         NA      NA      NA        NA        NA
    ## 742         NA         NA      NA      NA        NA        NA
    ## 743         NA         NA      NA      NA        NA        NA
    ## 744         NA         NA      NA      NA        NA        NA
    ## 745         NA         NA      NA      NA        NA        NA
    ## 746         NA         NA      NA      NA        NA        NA
    ## 747         NA         NA      NA      NA        NA        NA
    ## 748         NA         NA      NA      NA        NA        NA
    ## 749         NA         NA      NA      NA        NA        NA
    ## 750         NA         NA      NA      NA        NA        NA
    ## 751         NA         NA      NA      NA        NA        NA
    ## 752         NA         NA      NA      NA        NA        NA
    ## 753         NA         NA      NA      NA        NA        NA
    ## 754         NA         NA      NA      NA        NA        NA
    ## 755         NA         NA      NA      NA        NA        NA
    ## 756         NA         NA      NA      NA        NA        NA
    ## 757         NA         NA      NA      NA        NA        NA
    ## 758         NA         NA      NA      NA        NA        NA
    ## 759         NA         NA      NA      NA        NA        NA
    ## 760         NA         NA      NA      NA        NA        NA
    ## 761         NA         NA      NA      NA        NA        NA
    ## 762         NA         NA      NA      NA        NA        NA
    ## 763         NA         NA      NA      NA        NA        NA
    ## 764         NA         NA      NA      NA        NA        NA
    ## 765         NA         NA      NA      NA        NA        NA
    ## 766         NA         NA      NA      NA        NA        NA
    ## 767         NA         NA      NA      NA        NA        NA
    ## 768         NA         NA      NA      NA        NA        NA
    ## 769         NA         NA      NA      NA        NA        NA
    ## 770         NA         NA      NA      NA        NA        NA
    ## 771         NA         NA      NA      NA        NA        NA
    ## 772         NA         NA      NA      NA        NA        NA
    ## 773         NA         NA      NA      NA        NA        NA
    ## 774         NA         NA      NA      NA        NA        NA
    ## 775         NA         NA      NA      NA        NA        NA
    ## 776         NA         NA      NA      NA        NA        NA
    ## 777         NA         NA      NA      NA        NA        NA
    ## 778         NA         NA      NA      NA        NA        NA
    ## 779         NA         NA      NA      NA        NA        NA
    ## 780         NA         NA      NA      NA        NA        NA
    ## 781         NA         NA      NA      NA        NA        NA
    ## 782         NA         NA      NA      NA        NA        NA
    ## 783         NA         NA      NA      NA        NA        NA
    ## 784         NA         NA      NA      NA        NA        NA
    ## 785         NA         NA      NA      NA        NA        NA
    ## 786         NA         NA      NA      NA        NA        NA
    ## 787         NA         NA      NA      NA        NA        NA
    ## 788         NA         NA      NA      NA        NA        NA
    ## 789         NA         NA      NA      NA        NA        NA
    ## 790         NA         NA      NA      NA        NA        NA
    ## 791         NA         NA      NA      NA        NA        NA
    ## 792         NA         NA      NA      NA        NA        NA
    ## 793         NA         NA      NA      NA        NA        NA
    ## 794         NA         NA      NA      NA        NA        NA
    ## 795         NA         NA      NA      NA        NA        NA
    ## 796         NA         NA      NA      NA        NA        NA
    ## 797         NA         NA      NA      NA        NA        NA
    ## 798         NA         NA      NA      NA        NA        NA
    ## 799         NA         NA      NA      NA        NA        NA
    ## 800         NA         NA      NA      NA        NA        NA
    ## 801         NA         NA 1.10000  4.1000  13.90000 0.5000000
    ## 802         NA         NA 0.80000  3.1000   5.50000 0.1000000
    ## 803         NA         NA 1.40000  3.4000  19.10000 0.3000000
    ## 804         NA         NA 0.90000  1.7000   9.10000 0.1000000
    ## 805         NA         NA 2.90000  8.8000  23.00000 0.1000000
    ## 806         NA         NA 3.40000 11.1000  37.70000 0.1000000
    ## 807         NA         NA 3.00000 11.8000  33.10000 0.2000000
    ## 808         NA         NA 2.90000  5.9000  16.70000 0.1000000
    ## 809         NA         NA 2.40000  7.9000  26.00000 0.0000000
    ## 810         NA         NA 3.40000  7.4000  19.20000 0.1000000
    ## 811         NA         NA 0.00000  0.0000  13.10000 0.1000000
    ## 812         NA         NA 0.50000  6.7000  27.20000 0.0000000
    ## 813         NA         NA 3.50000 12.6000  27.00000 0.2000000
    ## 814         NA         NA 7.10000 23.1000  50.20000 0.3000000
    ## 815         NA         NA 3.00000 13.9000  65.00000 0.3000000
    ## 816         NA         NA 3.80000 25.0000 126.90000 0.3000000
    ## 817         NA         NA 1.70000 14.3000 122.40000 0.3000000
    ## 818         NA         NA 3.10000 13.0000  49.00000 0.3000000
    ## 819         NA         NA 1.30000  7.4000  24.50000 0.4000000
    ## 820         NA         NA 1.70000 21.8000 243.60000 0.1000000
    ## 821         NA         NA 9.00000 31.7000 128.40000 0.0000000
    ## 822         NA         NA 9.60000 22.5000  66.10000 0.0000000
    ## 823         NA         NA 3.00000  6.9000  20.40000 0.6000000
    ## 824         NA         NA 6.90000 20.3000  74.00000 0.5000000
    ## 825         NA         NA 8.10000 20.3000  68.80000 0.4000000
    ## 826         NA         NA 8.90000 32.7000  98.90000 0.3000000
    ## 827         NA         NA 6.90000 18.8000 114.20000 0.5000000
    ## 828         NA         NA 4.00000  7.4000  24.50000 0.5000000
    ## 829         NA         NA 4.20000 16.8000  86.70000 0.5000000
    ## 830         NA         NA 7.19546 24.7357  92.08104 0.3754153
    ## 831         NA         NA      NA      NA        NA        NA
    ## 832         NA         NA      NA      NA        NA        NA
    ## 833         NA         NA      NA      NA        NA        NA
    ## 834         NA         NA      NA      NA        NA        NA
    ## 835         NA         NA      NA      NA        NA        NA
    ## 836         NA         NA      NA      NA        NA        NA
    ## 837         NA         NA      NA      NA        NA        NA
    ## 838         NA         NA      NA      NA        NA        NA
    ## 839         NA         NA      NA      NA        NA        NA
    ## 840         NA         NA      NA      NA        NA        NA
    ## 841         NA         NA      NA      NA        NA        NA
    ## 842         NA         NA      NA      NA        NA        NA
    ## 843         NA         NA      NA      NA        NA        NA
    ## 844         NA         NA      NA      NA        NA        NA
    ## 845         NA         NA      NA      NA        NA        NA
    ## 846         NA         NA      NA      NA        NA        NA
    ## 847         NA         NA      NA      NA        NA        NA
    ## 848         NA         NA      NA      NA        NA        NA
    ## 849         NA         NA      NA      NA        NA        NA
    ## 850         NA         NA      NA      NA        NA        NA
    ## 851         NA         NA      NA      NA        NA        NA
    ## 852         NA         NA      NA      NA        NA        NA
    ## 853         NA         NA      NA      NA        NA        NA
    ## 854         NA         NA      NA      NA        NA        NA
    ## 855         NA         NA      NA      NA        NA        NA
    ## 856         NA         NA      NA      NA        NA        NA
    ## 857         NA         NA      NA      NA        NA        NA
    ## 858         NA         NA      NA      NA        NA        NA
    ## 859         NA         NA      NA      NA        NA        NA
    ## 860         NA         NA      NA      NA        NA        NA
    ## 861         NA         NA      NA      NA        NA        NA
    ## 862         NA         NA      NA      NA        NA        NA
    ## 863         NA         NA      NA      NA        NA        NA
    ## 864         NA         NA      NA      NA        NA        NA
    ## 865         NA         NA      NA      NA        NA        NA
    ## 866         NA         NA      NA      NA        NA        NA
    ## 867         NA         NA      NA      NA        NA        NA
    ## 868         NA         NA      NA      NA        NA        NA
    ## 869         NA         NA      NA      NA        NA        NA
    ## 870         NA         NA      NA      NA        NA        NA
    ## 871         NA         NA      NA      NA        NA        NA
    ## 872         NA         NA      NA      NA        NA        NA
    ## 873         NA         NA      NA      NA        NA        NA
    ## 874         NA         NA      NA      NA        NA        NA
    ## 875         NA         NA      NA      NA        NA        NA
    ## 876         NA         NA      NA      NA        NA        NA
    ## 877         NA         NA      NA      NA        NA        NA
    ## 878         NA         NA      NA      NA        NA        NA
    ## 879         NA         NA      NA      NA        NA        NA
    ## 880         NA         NA      NA      NA        NA        NA
    ## 881         NA         NA      NA      NA        NA        NA
    ## 882         NA         NA      NA      NA        NA        NA
    ## 883         NA         NA      NA      NA        NA        NA
    ## 884         NA         NA      NA      NA        NA        NA
    ## 885         NA         NA      NA      NA        NA        NA
    ## 886         NA         NA      NA      NA        NA        NA
    ## 887         NA         NA      NA      NA        NA        NA
    ## 888         NA         NA      NA      NA        NA        NA
    ## 889         NA         NA      NA      NA        NA        NA
    ## 890         NA         NA      NA      NA        NA        NA
    ## 891         NA         NA      NA      NA        NA        NA
    ## 892         NA         NA      NA      NA        NA        NA
    ## 893         NA         NA      NA      NA        NA        NA
    ## 894         NA         NA      NA      NA        NA        NA
    ## 895         NA         NA      NA      NA        NA        NA
    ## 896         NA         NA      NA      NA        NA        NA
    ## 897         NA         NA      NA      NA        NA        NA
    ## 898         NA         NA      NA      NA        NA        NA
    ## 899         NA         NA      NA      NA        NA        NA
    ## 900         NA         NA      NA      NA        NA        NA
    ## 901         NA         NA      NA      NA        NA        NA
    ## 902         NA         NA      NA      NA        NA        NA
    ## 903         NA         NA      NA      NA        NA        NA
    ## 904         NA         NA      NA      NA        NA        NA
    ## 905         NA         NA      NA      NA        NA        NA
    ## 906         NA         NA      NA      NA        NA        NA
    ## 907         NA         NA      NA      NA        NA        NA
    ## 908         NA         NA      NA      NA        NA        NA
    ## 909         NA         NA      NA      NA        NA        NA
    ## 910         NA         NA      NA      NA        NA        NA
    ## 911         NA         NA      NA      NA        NA        NA
    ## 912         NA         NA      NA      NA        NA        NA
    ## 913         NA         NA      NA      NA        NA        NA
    ## 914         NA         NA      NA      NA        NA        NA
    ## 915         NA         NA      NA      NA        NA        NA
    ## 916         NA         NA      NA      NA        NA        NA
    ## 917         NA         NA      NA      NA        NA        NA
    ## 918         NA         NA      NA      NA        NA        NA
    ## 919         NA         NA      NA      NA        NA        NA
    ## 920         NA         NA      NA      NA        NA        NA
    ## 921         NA         NA      NA      NA        NA        NA
    ## 922         NA         NA      NA      NA        NA        NA
    ## 923         NA         NA      NA      NA        NA        NA
    ## 924         NA         NA      NA      NA        NA        NA
    ## 925         NA         NA      NA      NA        NA        NA
    ## 926         NA         NA      NA      NA        NA        NA
    ## 927         NA         NA      NA      NA        NA        NA
    ## 928         NA         NA      NA      NA        NA        NA

``` r
#  distinct(site)
```

``` r
Sum_Org_site %>%
  filter(site %in% c("BASS RIVER", "ESSEX RIVER", "MYSTIC RIVER", "MERRIMACK RIVER", "Weymouth Fore River", "CHELSEA RIVER", "Chelsea River", "Mill Creek", "Neponset River Bridge", "South River", "Weymouth Fore & Town River", "North & Danvers River", "Island End River")) %>%
  ggplot(aes(x = fct_rev(fct_reorder(site, mean_PCB_T)), y = sapply(mean_PCB_T, FUN=function(x) ifelse(x==0.000000e0, -.7,x)), fill = site)) +
  geom_bar(stat="identity", col = "black") +
  scale_x_discrete(drop=FALSE) +
  geom_errorbar(aes(ymin = mean_PCB_T - SE_PCB_T, ymax = mean_PCB_T + SE_PCB_T), width = 0.2) +
  scale_fill_brewer(type = "qual", palette = 4, direction = 1, aesthetics = "fill") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "PCB Concentrations in Massachusetts Rivers",
       caption = "Error bars = 1 standard error",
       x = "Location",
       y = "Mean total PCB concentration ug/g")
```

![](PCBs_files/figure-gfm/bar-pcb-mass-rivers-1.png)<!-- -->

## Statistical tests

## Static map plots

### PCBs

``` r
GOM_states <- st_read("/cloud/project/extra/GOM_DD.shp")
```

    ## Reading layer `GOM_DD' from data source `/cloud/project/extra/GOM_DD.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 8 features and 6 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.72972 ymin: 40.98249 xmax: -59.69256 ymax: 48.06532
    ## Geodetic CRS:  NAD83

``` r
Bathy <- st_read("/cloud/project/extra/BATHYMGM_ARC.shp")
```

    ## Reading layer `BATHYMGM_ARC' from data source 
    ##   `/cloud/project/extra/BATHYMGM_ARC.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 5383 features and 2 fields
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: 174878.8 ymin: 577731.9 xmax: 923912.7 ymax: 1311467
    ## Projected CRS: NAD83 / Massachusetts Mainland

Bathymetry data is in projected coordinate system (NAD83), other data is
geodetic (NAD83). Wasn’t able to add this layer to map plot – need to
change projection.

``` r
st_crs(Bathy)
```

    ## Coordinate Reference System:
    ##   User input: NAD83 / Massachusetts Mainland 
    ##   wkt:
    ## PROJCRS["NAD83 / Massachusetts Mainland",
    ##     BASEGEOGCRS["NAD83",
    ##         DATUM["North American Datum 1983",
    ##             ELLIPSOID["GRS 1980",6378137,298.257222101,
    ##                 LENGTHUNIT["metre",1]]],
    ##         PRIMEM["Greenwich",0,
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         ID["EPSG",4269]],
    ##     CONVERSION["SPCS83 Massachusetts Mainland zone (meters)",
    ##         METHOD["Lambert Conic Conformal (2SP)",
    ##             ID["EPSG",9802]],
    ##         PARAMETER["Latitude of false origin",41,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8821]],
    ##         PARAMETER["Longitude of false origin",-71.5,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8822]],
    ##         PARAMETER["Latitude of 1st standard parallel",42.6833333333333,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8823]],
    ##         PARAMETER["Latitude of 2nd standard parallel",41.7166666666667,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8824]],
    ##         PARAMETER["Easting at false origin",200000,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8826]],
    ##         PARAMETER["Northing at false origin",750000,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8827]]],
    ##     CS[Cartesian,2],
    ##         AXIS["easting (X)",east,
    ##             ORDER[1],
    ##             LENGTHUNIT["metre",1]],
    ##         AXIS["northing (Y)",north,
    ##             ORDER[2],
    ##             LENGTHUNIT["metre",1]],
    ##     USAGE[
    ##         SCOPE["unknown"],
    ##         AREA["USA - Massachusetts - SPCS - mainland"],
    ##         BBOX[41.46,-73.5,42.89,-69.86]],
    ##     ID["EPSG",26986]]

``` r
Bathy <- st_transform(Bathy, "+init=epsg:4269")
```

    ## Warning in CPL_crs_from_input(x): GDAL Message 1: +init=epsg:XXXX syntax is
    ## deprecated. It might return a CRS with a non-EPSG compliant axis order.

Now the Bathy shapefile is in geodetic NAD83 format.

``` r
unique(Bathy$CONTOUR)
```

    ##  [1]    -5   -40   -15   -20   -10     0   -50   -30   -70   -60  -100   -90
    ## [13]   -80  -120  -220  -200  -160  -180  -140  -240  -300  -280  -260  -500
    ## [25]  -400 -2000 -1000 -3000 -4000

``` r
Bathy_low_res <- Bathy%>%
 filter(CONTOUR %in% c("-100","-500","-1000","-2000","-3000","-4000"))
ggplot(Bathy_low_res) +
  geom_sf(aes())
```

![](PCBs_files/figure-gfm/plot-bathy-1.png)<!-- -->

``` r
Bathy_hi_res <- Bathy%>%
 filter(CONTOUR %in% c("-40","-80","-120","160","200","240","280"))
ggplot(Bathy_hi_res) +
  geom_sf(aes())
```

![](PCBs_files/figure-gfm/plot-bathy-2.png)<!-- -->

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_low_res, color = "gray80", width = 1) +
  geom_point(data = Organics, (aes(x = LONGITUDE, y = LATITUDE, color = PCB_T_UGG, alpha = 0.5))) +
  xlim(-72,-65) +
  ylim(40,45) +
  theme_bw() +
  labs(title = "Distribution and concentration of PCBs",
       subtitle ="Gulf of Maine sediments",
       x = "Longitude",
       y = "Latitude") +
  guides(size = guide_legend(title = "PCB ug/g")) +
  guides(alpha = FALSE) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

    ## Warning in layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,
    ## : Ignoring unknown parameters: `width`

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 342 rows containing missing values (`geom_point()`).

![](PCBs_files/figure-gfm/map-pcbs-with-nas-zeros-1.png)<!-- -->

``` r
Org_no_na_no_zero <- Organics %>%
  drop_na(PCB_T_UGG) %>%
  filter(PCB_T_UGG != "0")
```

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_low_res, color = "gray80", width = 1) +
  geom_point(data = Org_no_na_no_zero, (aes(x = LONGITUDE, y = LATITUDE, size = PCB_T_UGG, alpha = 0.5))) +
  xlim(-72,-65) +
  ylim(40,45) +
  theme_bw() +
  labs(title = "Distribution and concentration of PCBs",
       subtitle ="Gulf of Maine sediments",
       x = "Longitude",
       y = "Latitude") +
  guides(size = guide_legend(title = "PCB ug/g"))+
  guides(alpha = FALSE) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

    ## Warning in layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,
    ## : Ignoring unknown parameters: `width`

    ## Warning: Removed 38 rows containing missing values (`geom_point()`).

![](PCBs_files/figure-gfm/map-pcb-gom-1.png)<!-- -->

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_hi_res, color = "gray80", width = 1) +
  geom_point(data=Org_no_na_no_zero, (aes(x = LONGITUDE, y = LATITUDE, size = PCB_T_UGG, alpha = 0.5))) +
  xlim(-69.2,-68) +
  ylim(44,44.5) +
  theme_bw() +
  labs(title = "Distribution and concentration of PCBs",
       subtitle ="MDI and Penobscot Bay sediments",
       x = "Longitude",
       y = "Latitude") +
  guides(size = guide_legend(title = "PCB ug/g")) +
  guides(alpha = FALSE) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

    ## Warning in layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,
    ## : Ignoring unknown parameters: `width`

    ## Warning: Removed 1015 rows containing missing values (`geom_point()`).

![](PCBs_files/figure-gfm/map-pcb-mdi-area-1.png)<!-- -->

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_hi_res, color = "gray80", width = 1) +
  geom_point(data=Org_no_na_no_zero, (aes(x = LONGITUDE, y = LATITUDE, size = PCB_T_UGG, alpha = 0.5))) +
  xlim(-71.2,-69.5) +
  ylim(41.8,43) +
  theme_bw() +
  labs(title = "Distribution and concentration of PCBs",
       subtitle ="Massachusetts and Cape Cod Bays sediments",
       x = "Longitude",
       y = "Latitude") +
  guides(size = guide_legend(title = "PCB ug/g"))+
  guides(alpha = FALSE) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

    ## Warning in layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,
    ## : Ignoring unknown parameters: `width`

    ## Warning: Removed 367 rows containing missing values (`geom_point()`).

![](PCBs_files/figure-gfm/map-pcb-mass-bays-1.png)<!-- -->

``` r
Organics_long_no_na_no_zero <- Organics_long %>%
  drop_na(amount_detected) %>%
  filter(amount_detected != "0")
```

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_low_res, color = "gray80", width = 1) +
  geom_point(data=Organics_long_no_na_no_zero, (aes(x = LONGITUDE, y = LATITUDE, size = amount_detected, color = organic_detected, alpha = 0.5))) +
  xlim(-72,-65) +
  ylim(40,45) +
  theme_bw() +
  labs(title = "Distribution and concentration of PCBs",
       subtitle ="MDI and Penobscot Bay sediments",
       x = "Longitude",
       y = "Latitude") +
  guides(size = guide_legend(title = "PCB ug/g")) +
  guides(alpha = FALSE) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

    ## Warning in layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,
    ## : Ignoring unknown parameters: `width`

    ## Warning: Removed 112 rows containing missing values (`geom_point()`).

![](PCBs_files/figure-gfm/map-all-organics-gom-1.png)<!-- -->

``` r
ggplot(GOM_states) +
  geom_sf(aes()) +
  geom_sf(data = Bathy_low_res, color = "gray80", width = 1) +
  geom_point(data=pesticides, (aes(x = LONGITUDE, y = LATITUDE, size = amount_detected, color = pcb, alpha = 0.5))) +
  xlim(-72,-65) +
  ylim(40,45) +
  theme_bw() +
  guides(size = guide_legend(title = "Concentration ng/g")) +
  guides(alpha = FALSE) +
  theme(legend.direction = "vertical", legend.box = "horizontal") +
  scale_color_discrete(name = "Pesticide",
                        breaks = c(
                          "DDT_C", 
                          "DDE_4_4_C", 
                          "DDD_4_4_C", 
                          "ENDRIN_C", 
                          "ENDR_ALD_C", 
                          "ALDRIN_C", 
                          "DIELDRN_C", 
                          "CLRDNE_T_C", 
                          "MIREX_C", 
                          "METHOXYCLC", 
                          "BHC_C",
                          "LINDANE_C"),
                          labels = c(
                          "DDT",
                          "4,4' DDE",
                          "4,4' DDD",
                          "Endrin",
                          "Endrin Aldehyde",
                          "Aldrin",
                          "Dieldrin",
                          "Chlordane",
                          "Mirex",
                          "Methoxychlor",
                          "Benzene Hexachloride",
                          "Lindane")) +
  labs(title = "Distribution and concentration of Pesticides",
       subtitle ="Gulf of Maine sediments",
       x = "Longitude",
       y = "Latitude") +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
```

![](PCBs_files/figure-gfm/map-all-pesticides-gom-1.png)<!-- -->

## Interactive map

``` r
Organics_long_no_na_no_zero <- Organics_long %>%
  drop_na(amount_detected) %>%
  filter(amount_detected != "0") %>%
  drop_na(SOUNDING_M) %>%
  filter(SOUNDING_M != "0")
```

``` r
#labels <- sprintf("<strong>%s</strong><br/>%s: %g ug/g</strong><br/>",
#                  Organics_long$SPECFC_LOC, Organics_long$organic_detected, #Organics_long$amount_detected) %>%
#  lapply(htmltools::HTML)
#head(labels, 1)
```

``` r
#leaflet(data = Organics_long_no_na_no_zero) %>%
#  addProviderTiles(providers$Esri.WorldTopoMap) %>%
#  setView(lng = -68.5, 
#          lat = 43.5, 
#          zoom = 6) %>%
#  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = c(~amount_detected), label = labels)
```

Note that the two above code chunks are commented out as .rmd will not
knit to github document with html functions. Uncomment to run and change
output type to `html_document` to knit.
