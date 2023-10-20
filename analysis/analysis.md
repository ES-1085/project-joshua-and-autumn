GOM Contaminated Sediments analysis
================
Joshua Harkness and Autumn Pauly,
2023-10-20

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
library(readxl)
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE

``` r
stations_full = read_excel("/cloud/project/data/STAT2002.xls", sheet = 2, skip = 3)
sediments_full = read_excel("/cloud/project/data/TXTR2002.xls", sheet = 2, skip = 3)
PCBs_full = read_excel("/cloud/project/data/PCBP2002.xls", sheet = 2, skip = 3)
PAHs_full = read_excel("/cloud/project/data/PAHS2002.xls", sheet = 2, skip = 3)
organics_full = read_excel("/cloud/project/data/GENO2002.xls", sheet = 2, skip = 3)
inorganics_full = read_excel("/cloud/project/data/INOR2002.xls", sheet = 2, skip = 3)
```

    ## New names:
    ## • `O_RAD_D_G` -> `O_RAD_D_G...233`
    ## • `O_RAD_D_G` -> `O_RAD_D_G...234`

``` r
PCBs = select(PCBs_full, c(UNIQUE_ID, PCB_52_NGG, PCB101_NGG, PCB118_NGG, PCB128_NGG, PCB138_NGG, PCB153_NGG, PCB180_NGG, PCB206_NGG, PCB209_NGG, DDT_4_4_C, DDT_2_4_C, DDE_4_4_C, DDD_4_4_C, ENDRIN_C, ENDR_ALD_C, ALDRIN_C, DIELDRN_C, CLRDNE_T_C, MIREX_C, METHOXYCLC, BHC_A_C, BHC_B_C, BHC_D_C, LINDANE_C))
```
