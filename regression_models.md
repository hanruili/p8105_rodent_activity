regression_models
================
Yujing FU
2024-11-30

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
# data import 
rat_df = read.csv("data/rat_2020_2024.csv")
food_scrap_df = 
  read.csv("data/food_scrap_drop_off.csv")
zhvi_df = read.csv("data/ny_zhvi_df_2020_2024.csv")
zori_df = read.csv("data/ny_zori_df_2020_2024.csv")

head(rat_df)
```

    ##   inspection_type job_ticket_or_work_order_id    job_id job_progress        bbl
    ## 1         Initial                    13659068 PC8078519            1 4023290019
    ## 2         Initial                    13361613 PC7824966            1 3072810182
    ## 3         Initial                    13599220 PC8056345            1 2033950047
    ## 4            BAIT                     2941952 PC8141304            5 4104480015
    ## 5         Initial                    13788881 PC8254435            1 2057930467
    ## 6      Compliance                    13383745 PC7780758            2 3034140068
    ##   boro_code block lot house_number      street_name zip_code x_coord y_coord
    ## 1         4  2329  19        48-03        59 STREET    11377       0       0
    ## 2         3  7281 182          145 SEABREEZE AVENUE    11224       0       0
    ## 3         2  3395  47          530  EAST 234 STREET    10470       0       0
    ## 4         4 10448  15       192-21   JAMAICA AVENUE    11423 1048743  199167
    ## 5         2  5793 467         3555    OXFORD AVENUE    10463 1009450  262094
    ## 6         3  3414  68            9    COVERT STREET    11207       0       0
    ##   latitude longitude  borough        inspection_date             result
    ## 1 40.73837 -73.90647   Queens 06/05/2023 03:19:22 PM             Passed
    ## 2 40.57565 -73.97020 Brooklyn 04/05/2022 03:50:52 PM             Passed
    ## 3 40.89732 -73.86322    Bronx 03/14/2023 08:40:50 AM Failed for Other R
    ## 4 40.71310 -73.76736   Queens 12/26/2023 12:16:51 PM       Bait applied
    ## 5 40.88602 -73.90886    Bronx 01/23/2024 04:10:00 PM       Rat Activity
    ## 6 40.68518 -73.91342 Brooklyn 05/09/2022 03:35:15 PM             Passed
    ##            approved_date                            location community_board
    ## 1 06/08/2023 05:06:47 PM                                                   2
    ## 2 04/08/2022 03:54:46 PM                                                  13
    ## 3 03/17/2023 02:48:23 PM                                                  12
    ## 4 12/27/2023 10:10:53 AM (40.713101329334, -73.767362471917)              12
    ## 5 01/24/2024 11:33:03 AM (40.886019879068, -73.908861936068)               8
    ## 6 05/12/2022 08:19:56 AM                                                   4
    ##   council_district census_tract     bin                      nta year month
    ## 1               26          245 4053773                 Woodside 2023     6
    ## 2               48        35602 3196597    Coney Island-Sea Gate 2022     4
    ## 3               11        45101 2019758       Wakefield-Woodlawn 2023     3
    ## 4               23          482 4222146                   Hollis 2023    12
    ## 5               11          295 2084152 Riverdale-Spuyten Duyvil 2024     1
    ## 6               37          401 3078992          Bushwick (East) 2022     5

``` r
head(food_scrap_df)
```

    ##         borough                nta_name
    ## 1      Brooklyn               Bay Ridge
    ## 2     Manhattan East Midtown-Turtle Bay
    ## 3     Manhattan          Hell's Kitchen
    ## 4     Manhattan East Midtown-Turtle Bay
    ## 5     Manhattan    Tribeca-Civic Center
    ## 6 Staten Island St. George-New Brighton
    ##                                    site_name
    ## 1             4th Avenue Presbyterian Church
    ## 2         Dag Hammarskjold Plaza Greenmarket
    ## 3 Hudson River Park's Pier 84 at W. 44th St.
    ## 4                   58th Street Library FSDO
    ## 5                        Tribeca Greenmarket
    ## 6                     St. George Greenmarket
    ##                              site_addr
    ## 1  6753 4th Avenue, Brooklyn, NY 11220
    ## 2                  E 47th St & 2nd Ave
    ## 3 Pier 84 at W. 44th St. near dog park
    ## 4                 127 East 58th Street
    ## 5             Greenwich St. & Duane St
    ## 6            St. Marks Pl and Hyatt St
    ##                                                                      hosted_by
    ## 1                                               4th Avenue Presbyterian Church
    ## 2                                                                      GrowNYC
    ## 3                                                   Staff at Hudson River Park
    ## 4                                                                      GrowNYC
    ## 5                                                                      GrowNYC
    ## 6 NYC Compost Project Hosted by Snug Harbor Cultural Center & Botanical Garden
    ##   open_month                                             day_hours
    ## 1 Year Round        Every day (Start Time: Dawn - End Time:  Dusk)
    ## 2 Year Round Wednesday (Start Time: 8:00 AM - End Time:  12:30 PM)
    ## 3 Year Round  Every day (Start Time: 7:00 AM - End Time:  7:00 PM)
    ## 4 Year Round Wednesdays (Start Time: 7:30 AM - End Time:  1:30 PM)
    ## 5 Year Round   Saturday (Start Time: 8:00 AM - End Time:  1:00 PM)
    ## 6 Year Round   Saturday (Start Time: 8:00 AM - End Time:  1:00 PM)
    ##                       notes
    ## 1 No meat, bones, or dairy.
    ## 2                      <NA>
    ## 3                      <NA>
    ## 4                      <NA>
    ## 5                      <NA>
    ## 6                      <NA>
    ##                                                                          website
    ## 1                                                                           <NA>
    ## 2                                                            grownyc.org/compost
    ## 3 https://hudsonriverpark.org/the-park/sustainability/community-compost-program/
    ## 4                                                            grownyc.org/compost
    ## 5                                                            grownyc.org/compost
    ## 6                                                                snug-harbor.org
    ##   boro_cd council_dis  ct2010 bbl bin latitude longitude police_prec object_id
    ## 1     310          47 3012600  NA  NA 40.63551 -74.02277          68     31263
    ## 2     106           4 1009000  NA  NA 40.75261 -73.96904          17     31123
    ## 3     104           3 1012901  NA  NA 40.76346 -74.00025          18     31091
    ## 4     105           4 1011203  NA  NA 40.76198 -73.96930          18     31084
    ## 5     101           1 1003900  NA  NA 40.71742 -74.01079           1     30996
    ## 6     501          49 5000300  NA  NA 40.64197 -74.07793         120     30956
    ##                 location_point app_android app_i_os assembly_district
    ## 1 POINT (-74.022767 40.635514)        <NA>     <NA>                51
    ## 2 POINT (-73.969036 40.752606)        <NA>     <NA>                74
    ## 3   POINT (-74.00025 40.76346)        <NA>     <NA>                67
    ## 4    POINT (-73.9693 40.76198)        <NA>     <NA>                73
    ## 5 POINT (-74.010793 40.717424)        <NA>     <NA>                66
    ## 6 POINT (-74.077928 40.641967)        <NA>     <NA>                61
    ##   congress_district dsny_district dsny_section dsny_zone senate_district
    ## 1                10         BKS10       BKS101       BKS              17
    ## 2                12          MN06        MN063        MN              28
    ## 3                12          MN04        MN043        MN              47
    ## 4                12          MN05        MN052        MN              28
    ## 5                10          MN01        MN013        MN              27
    ## 6                11          SI01        SI012        SI              23

``` r
head(zhvi_df)
```

    ##   RegionID SizeRank RegionName RegionType StateName State     City
    ## 1    62080        4      11368        zip        NY    NY New York
    ## 2    62093        7      11385        zip        NY    NY New York
    ## 3    62019        9      11208        zip        NY    NY New York
    ## 4    62046       16      11236        zip        NY    NY New York
    ## 5    61807       17      10467        zip        NY    NY New York
    ## 6    62085       18      11373        zip        NY    NY New York
    ##                                   Metro    CountyName X2020.1.31 X2020.2.29
    ## 1 New York-Newark-Jersey City, NY-NJ-PA Queens County   528192.6   527257.9
    ## 2 New York-Newark-Jersey City, NY-NJ-PA Queens County   766285.8   766877.9
    ## 3 New York-Newark-Jersey City, NY-NJ-PA  Kings County   584069.0   584128.8
    ## 4 New York-Newark-Jersey City, NY-NJ-PA  Kings County   556447.0   558779.1
    ## 5 New York-Newark-Jersey City, NY-NJ-PA  Bronx County   431082.0   426699.4
    ## 6 New York-Newark-Jersey City, NY-NJ-PA Queens County   675125.7   675670.1
    ##   X2020.3.31 X2020.4.30 X2020.5.31 X2020.6.30 X2020.7.31 X2020.8.31 X2020.9.30
    ## 1   524266.9   522275.8   521385.9   520385.5   516688.1   510385.4   504947.1
    ## 2   767261.6   768287.1   768536.4   767880.3   766388.6   764911.0   763249.7
    ## 3   585511.1   588232.9   591933.8   592971.8   591628.3   589038.7   587388.2
    ## 4   561201.6   563977.4   566000.7   565868.6   564319.2   562457.2   561660.1
    ## 5   417166.3   409128.2   404095.7   402165.0   402087.6   401990.7   405016.7
    ## 6   673500.0   668760.7   664276.7   662083.9   654935.9   644841.3   635070.8
    ##   X2020.10.31 X2020.11.30 X2020.12.31 X2021.1.31 X2021.2.28 X2021.3.31
    ## 1    503106.5    503648.4    506265.0   508977.3   512132.8   514154.6
    ## 2    759656.1    756066.8    753333.5   752381.3   753657.6   756193.4
    ## 3    586779.6    587397.1    588714.3   590735.8   593778.1   597376.2
    ## 4    561378.6    562520.3    564734.6   568278.6   572763.3   578465.5
    ## 5    408095.2    412207.6    416081.1   421889.1   431480.8   444899.4
    ## 6    630178.7    626333.1    623006.2   618409.3   617337.5   618486.7
    ##   X2021.4.30 X2021.5.31 X2021.6.30 X2021.7.31 X2021.8.31 X2021.9.30 X2021.10.31
    ## 1   513545.6   513274.3   512169.7   513647.7   515205.9   517516.9    520167.9
    ## 2   760293.4   764712.1   768415.4   771259.6   772388.5   773570.8    775890.5
    ## 3   602246.5   606895.0   609155.7   610491.7   610604.1   613045.0    617421.0
    ## 4   584991.4   591055.9   594904.9   596912.1   597603.3   597795.9    598878.2
    ## 5   455437.9   462904.9   461875.1   453605.1   438793.4   421945.6    411599.9
    ## 6   619560.2   620293.7   617666.3   618197.6   618781.9   619884.8    620588.8
    ##   X2021.11.30 X2021.12.31 X2022.1.31 X2022.2.28 X2022.3.31 X2022.4.30
    ## 1    521725.6    521886.5   519710.8   517534.2   516469.2   516859.7
    ## 2    779431.8    782268.2   783578.9   783441.1   785378.3   788904.9
    ## 3    623356.3    628199.3   630765.7   631970.0   633731.6   636190.4
    ## 4    601476.8    604396.6   607793.6   611521.1   616361.0   620093.7
    ## 5    403827.1    397758.7   394423.8   398191.9   407522.6   413924.8
    ## 6    619487.9    617252.4   614656.6   614966.0   617660.6   619955.0
    ##   X2022.5.31 X2022.6.30 X2022.7.31 X2022.8.31 X2022.9.30 X2022.10.31
    ## 1   516227.9   516353.1   516371.5   514825.7   511602.0    508097.2
    ## 2   796084.2   804432.2   810063.0   811312.9   807884.0    804876.2
    ## 3   640465.9   645628.6   649119.5   649810.3   647188.8    644657.8
    ## 4   622656.0   625384.0   627041.1   627201.2   623448.9    620023.4
    ## 5   414126.5   412279.0   408672.8   402399.6   392358.9    384807.9
    ## 6   619968.6   621096.5   619121.3   612323.0   601194.8    591469.8
    ##   X2022.11.30 X2022.12.31 X2023.1.31 X2023.2.28 X2023.3.31 X2023.4.30
    ## 1    506580.5    503289.1   497724.8   490815.4   484453.6   477787.1
    ## 2    802838.2    802151.0   800703.4   798950.1   797761.4   799366.6
    ## 3    642264.1    641399.5   640105.2   637786.4   636126.6   640210.0
    ## 4    616886.2    614528.1   610691.6   606678.5   604931.1   608805.4
    ## 5    380556.4    379171.4   374879.9   373540.1   375699.4   384275.2
    ## 6    584564.3    577927.4   569925.2   564723.3   561582.7   556310.6
    ##   X2023.5.31 X2023.6.30 X2023.7.31 X2023.8.31 X2023.9.30 X2023.10.31
    ## 1   470639.5   465210.9   463377.3   463371.3   462810.6    460771.9
    ## 2   801223.4   802036.8   801422.6   800283.2   798291.5    797014.0
    ## 3   648346.8   654695.9   657573.2   657020.5   656315.8    655178.3
    ## 4   615239.9   621151.8   624614.7   626492.5   627861.0    628682.6
    ## 5   391838.2   396577.9   399856.9   402236.0   403255.3    400097.0
    ## 6   545838.3   531434.2   521217.5   513782.0   509985.5    505959.8
    ##   X2023.11.30 X2023.12.31 X2024.1.31 X2024.2.29 X2024.3.31 X2024.4.30
    ## 1    457397.8    453086.2   450240.7   448627.2   451705.6   456243.1
    ## 2    795779.1    795640.7   794282.4   794342.2   797380.4   802193.9
    ## 3    654648.3    656263.8   658709.2   661959.5   666440.0   670735.0
    ## 4    629011.8    630674.1   632713.9   635540.9   638623.8   641376.0
    ## 5    394496.2    386985.9   382000.0   378034.4   377532.6   374809.3
    ## 6    501444.1    497843.2   493967.4   492163.3   491429.0   492810.0
    ##   X2024.5.31 X2024.6.30 X2024.7.31 X2024.8.31 X2024.9.30 X2024.10.31
    ## 1   460893.5   461600.4   461088.3   460451.8   461417.2    461167.7
    ## 2   805219.7   804711.7   804494.0   806317.1   810315.1    814334.5
    ## 3   673816.0   674124.7   674583.6   675525.6   677539.8    678312.6
    ## 4   642619.7   641839.5   641629.1   643184.2   646821.1    650637.9
    ## 5   372827.7   369063.0   368696.4   369597.6   371425.2    371800.4
    ## 6   495404.8   497218.9   499719.7   501979.1   503272.5    505398.0

``` r
head(rat_df)
```

    ##   inspection_type job_ticket_or_work_order_id    job_id job_progress        bbl
    ## 1         Initial                    13659068 PC8078519            1 4023290019
    ## 2         Initial                    13361613 PC7824966            1 3072810182
    ## 3         Initial                    13599220 PC8056345            1 2033950047
    ## 4            BAIT                     2941952 PC8141304            5 4104480015
    ## 5         Initial                    13788881 PC8254435            1 2057930467
    ## 6      Compliance                    13383745 PC7780758            2 3034140068
    ##   boro_code block lot house_number      street_name zip_code x_coord y_coord
    ## 1         4  2329  19        48-03        59 STREET    11377       0       0
    ## 2         3  7281 182          145 SEABREEZE AVENUE    11224       0       0
    ## 3         2  3395  47          530  EAST 234 STREET    10470       0       0
    ## 4         4 10448  15       192-21   JAMAICA AVENUE    11423 1048743  199167
    ## 5         2  5793 467         3555    OXFORD AVENUE    10463 1009450  262094
    ## 6         3  3414  68            9    COVERT STREET    11207       0       0
    ##   latitude longitude  borough        inspection_date             result
    ## 1 40.73837 -73.90647   Queens 06/05/2023 03:19:22 PM             Passed
    ## 2 40.57565 -73.97020 Brooklyn 04/05/2022 03:50:52 PM             Passed
    ## 3 40.89732 -73.86322    Bronx 03/14/2023 08:40:50 AM Failed for Other R
    ## 4 40.71310 -73.76736   Queens 12/26/2023 12:16:51 PM       Bait applied
    ## 5 40.88602 -73.90886    Bronx 01/23/2024 04:10:00 PM       Rat Activity
    ## 6 40.68518 -73.91342 Brooklyn 05/09/2022 03:35:15 PM             Passed
    ##            approved_date                            location community_board
    ## 1 06/08/2023 05:06:47 PM                                                   2
    ## 2 04/08/2022 03:54:46 PM                                                  13
    ## 3 03/17/2023 02:48:23 PM                                                  12
    ## 4 12/27/2023 10:10:53 AM (40.713101329334, -73.767362471917)              12
    ## 5 01/24/2024 11:33:03 AM (40.886019879068, -73.908861936068)               8
    ## 6 05/12/2022 08:19:56 AM                                                   4
    ##   council_district census_tract     bin                      nta year month
    ## 1               26          245 4053773                 Woodside 2023     6
    ## 2               48        35602 3196597    Coney Island-Sea Gate 2022     4
    ## 3               11        45101 2019758       Wakefield-Woodlawn 2023     3
    ## 4               23          482 4222146                   Hollis 2023    12
    ## 5               11          295 2084152 Riverdale-Spuyten Duyvil 2024     1
    ## 6               37          401 3078992          Bushwick (East) 2022     5

``` r
# clean rat data
rat_tidydf = rat_df |> 
  mutate(inspection_date = substring(inspection_date, 1, 10)) |> 
  mutate(date = as.Date(inspection_date, format = "%m/%d/%Y")) |> 
  mutate(year = as.numeric(format(date, "%Y"))) |> 
  mutate(month = as.numeric(format(date, "%m"))) |> 
  mutate(day = as.numeric(format(date, "%d"))) |> 
  select(inspection_type, result, date, year, month, day, street_name, zip_code, borough,latitude, longitude, location)
```

``` r
# clean zhvi and zori data
zhvi_tidydf =
  zhvi_df |> 
  janitor::clean_names() |> 
  mutate(region_name=as.numeric(region_name)) |> # Convert the data type to numeric
  rename(zip_code = region_name, county = county_name) |> # Rename 'region_name' to 'zip_code'
  mutate(county = gsub(" County", "", county)) |> 
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "date", 
    values_to = "house_price") |>  
  mutate(date = as.Date(gsub("^x", "", date), format = "%Y_%m_%d")) |> # Remove 'x' from date and convert to Date format
  mutate(year = as.numeric(format(date, "%Y"))) |> # Extract the year from the date
  mutate(month = as.numeric(format(date, "%m"))) |> # Extract the month from the date
  select(zip_code, county, date, year, month, house_price) |> # Select relevant columns
  drop_na() # Remove rows with missing values for data quality

# Clean and transform ZORI (rental price) dataset
zori_tidydf = 
  zori_df |> 
  janitor::clean_names() |> # Standardize column names to clearer version
  rename(zip_code = region_name, county = county_name) |> # Rename 'region_name' to 'zip_code'
  mutate(county = gsub(" County", "", county)) |> 
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "date", 
    values_to = "rental_price") |> 
  mutate(date = as.Date(gsub("^x", "", date), format = "%Y_%m_%d")) |> # Remove 'x' from date and convert to Date format
  mutate(year = as.numeric(format(date, "%Y"))) |> 
  mutate(month = as.numeric(format(date, "%m"))) |>
  select(zip_code, county, date, year, month, rental_price) |> # Select necessary columns
  drop_na() # Remove rows with missing values
```

``` r
# zip code df
url = "https://p8105.com/data/zip_codes.html"
zip_code_html = read_html(url)

zip_code_df =
  zip_code_html |> 
  html_table() |> 
  as.data.frame() |> 
  janitor::clean_names() |> 
  mutate(borough = case_when(
    county == "Bronx" ~ "Bronx",
    county == "Kings" ~ "Brooklyn",
    county == "New York" ~ "Manhattan",
    county == "Queens" ~ "Queens",
    county == "Richmond" ~ "Staten Island",
    TRUE ~ "Other"
  )) |> 
  select(zip_code, county, neighborhood, borough) |> 
  drop_na()
```

``` r
# merge the zori & zhvi with zip code based on both the `zip_code` and `county`
zori_merged = left_join(zori_tidydf, zip_code_df, by = c("zip_code", "county")) |> 
  select(-date)
zhvi_merged = left_join(zhvi_tidydf, zip_code_df, by = c("zip_code", "county")) |> 
  select(-date)
```

``` r
# merge the zillow information
zillow_merged = merge(zori_merged, zhvi_merged, by = c("zip_code", "county", "borough", "neighborhood", "year", "month")) |> 
  drop_na()
```

``` r
# define the binary variable of rodent activity
rodent_reg =
  rat_tidydf |> 
  mutate(rodent_bi = case_when(
    result == "Rat Activity" ~ 1,
    TRUE ~ 0
  )) 
head(rodent_reg)
```

    ##   inspection_type             result       date year month day      street_name
    ## 1         Initial             Passed 2023-06-05 2023     6   5        59 STREET
    ## 2         Initial             Passed 2022-04-05 2022     4   5 SEABREEZE AVENUE
    ## 3         Initial Failed for Other R 2023-03-14 2023     3  14  EAST 234 STREET
    ## 4            BAIT       Bait applied 2023-12-26 2023    12  26   JAMAICA AVENUE
    ## 5         Initial       Rat Activity 2024-01-23 2024     1  23    OXFORD AVENUE
    ## 6      Compliance             Passed 2022-05-09 2022     5   9    COVERT STREET
    ##   zip_code  borough latitude longitude                            location
    ## 1    11377   Queens 40.73837 -73.90647                                    
    ## 2    11224 Brooklyn 40.57565 -73.97020                                    
    ## 3    10470    Bronx 40.89732 -73.86322                                    
    ## 4    11423   Queens 40.71310 -73.76736 (40.713101329334, -73.767362471917)
    ## 5    10463    Bronx 40.88602 -73.90886 (40.886019879068, -73.908861936068)
    ## 6    11207 Brooklyn 40.68518 -73.91342                                    
    ##   rodent_bi
    ## 1         0
    ## 2         0
    ## 3         0
    ## 4         0
    ## 5         1
    ## 6         0

``` r
rodent_reg_merged = 
  left_join(zillow_merged, rodent_reg, by = c("zip_code", "borough", "year", "month"))
head(rodent_reg_merged)
```

    ##   zip_code   county   borough        neighborhood year month rental_price
    ## 1    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ## 2    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ## 3    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ## 4    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ## 5    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ## 6    10001 New York Manhattan Chelsea and Clinton 2020     1     4088.038
    ##   house_price inspection_type       result       date day      street_name
    ## 1     2417750         Initial Rat Activity 2020-01-24  24 WEST   26 STREET
    ## 2     2417750      Compliance       Passed 2020-01-07   7        10 AVENUE
    ## 3     2417750      Compliance Rat Activity 2020-01-07   7   WEST 25 STREET
    ## 4     2417750         Initial Rat Activity 2020-01-24  24 WEST   26 STREET
    ## 5     2417750      Compliance       Passed 2020-01-07   7         8 AVENUE
    ## 6     2417750      Compliance       Passed 2020-01-06   6 WEST   30 STREET
    ##   latitude longitude                            location rodent_bi
    ## 1 40.74942 -74.00242 (40.749422696226, -74.002421736205)         1
    ## 2 40.74844 -74.00361 (40.748437295659, -74.003612700367)         0
    ## 3 40.74862 -74.00244 (40.748618480914, -74.002436143262)         1
    ## 4 40.74924 -74.00200 (40.749244294957, -74.001995851746)         1
    ## 5 40.74940 -73.99548 (40.749395185198, -73.995481352499)         0
    ## 6 40.74991 -73.99589 (40.749911215466, -73.995885544563)         0

# Data Analysis

describe 0 and 1

## Is the rental price of houses significantly related to the rodent activity?

``` r
# Logistic Regression: Rental Price vs Rodent Activity
model1 <- glm(rodent_bi ~ rental_price, family = binomial, data = rodent_reg_merged)

# Creating Regression Model Table
model1 |>
  broom::tidy() |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 10)
```

| term         |      estimate | p.value |
|:-------------|--------------:|--------:|
| (Intercept)  | -0.7659535537 |       0 |
| rental_price |  0.0000779043 |       0 |

$$\text{log}(\frac{P(\text{rodent_bi} = 1)}{1 - P(\text{rodent_bi} = 1)}) = -0.766 + 0.0000779 \times \text{rental price}$$

Where:

- $P(\text{rodent_bi} = 1))$ is the probability of observing rodent
  activity.
- $\text{log}(\frac{P}{1-P})$ is the log-odds of rodent activity.

**Intercept ($\beta_0$):** -0.766

This represents the log-odds of rodent activity when the rental price is
0.

- Since the intercept value is negative ($\beta_0 = -0.766$), the
  log-odds of rodent activity are less than zero when the rental price
  is 0.
- Odds = $e^{\beta_0} = e^{-0.766} \approx 0.465$. This implies that,
  when the rental price is 0, the odds of rodent activity are
  approximately 0.465:1, meaning it’s less likely than not.

**Coefficient for `rental_price` ($\beta_1$):** 0.0000779

The coefficient represents the change in the log-odds of rodent activity
for a one-unit increase in the rental price.

- For every one-unit increase in the rental price, the log-odds of
  rodent activity increase by 0.0000779.
- Odds Ratio = $e^{\beta_1} = e^{0.0000779} \approx 1$. This indicates
  that a small increase in rental price slightly increases the odds of
  rodent activity.

**Statistical Significance:**

Both p-values for the intercept and coefficient are \<2e-16 $\approx$ 0,
meaning they are statistically significant at any common significance
level (e.g., $\alpha = 0.05$). This suggests that both the intercept and
the effect of rental price on rodent activity are statistically
significant.

**Summary:**

- Rental price has a statistically significant but very small positive
  relationship with rodent activity. While significant, the effect size
  is negligible, meaning rental price is not a strong predictor of
  rodent activity.
- This suggests that other factors likely play a more significant role
  in determining rodent activity in the dataset.

## Is the value of houses significantly related to the rodent activity?

``` r
# Logistic Regression: House Value vs Rodent Activity
model2 = glm(rodent_bi ~ house_price, family = binomial, data = rodent_reg_merged)

# Creating Regression Model Table
model2 |>
  broom::tidy() |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 10)
```

| term        |   estimate | p.value |
|:------------|-----------:|--------:|
| (Intercept) | -0.4510699 |       0 |
| house_price | -0.0000001 |       0 |

``` r
summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = rodent_bi ~ house_price, family = binomial, data = rodent_reg_merged)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.511e-01  1.283e-02 -35.146  < 2e-16 ***
    ## house_price -9.999e-08  1.260e-08  -7.935 2.11e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 197090  on 149878  degrees of freedom
    ## Residual deviance: 197027  on 149877  degrees of freedom
    ##   (413 observations deleted due to missingness)
    ## AIC: 197031
    ## 
    ## Number of Fisher Scoring iterations: 4

$$\text{log}(\frac{P(\text{rodent_bi} = 1)}{1 - P(\text{rodent_bi} = 1)}) = -0.451 + (-0.0000001) \times \text{house value}$$

**Intercept ($\beta_0$):** -0.451

This represents the log-odds of rodent activity when the house value is
0.

- Since `house_price` = 0 is unrealistic (as house prices cannot be
  zero), the intercept primarily serves as a baseline log-odds for the
  model.
- Odds = $e^{\beta_0} = e^{-0.451} \approx 0.637$. This means that the
  odds of rodent activity are approximately 0.637:1 when house price is
  0 (hypothetically).

**Coefficient for `rental_price` ($\beta_1$):** -0.0000001

The coefficient indicates the change in log-odds of rodent activity for
a one-unit increase in house price.

- For each one-unit increase in house price, the log-odds of rodent
  activity decrease by 0.0000001.
- Odds Ratio = $e^{\beta_1} = e^{-0.0000001} \approx 1$. This suggests
  that an increase in house price very slightly reduces the odds of
  rodent activity. However, the effect size is extremely small and
  likely negligible in practical terms.

**Statistical Significance:**

Both p-values for the intercept and coefficient $\approx$ 0, meaning
they are statistically significant at any common significance level
(e.g., $\alpha = 0.05$). This means the relationship between house price
and rodent activity is statistically significant, but the effect size is
so small that it has little practical importance.

**Summary:**

- Higher house prices are slightly associated with lower rodent
  activity, but the effect is negligible.
- While statistically significant, the effect size is so small that
  house price is unlikely to be a meaningful predictor of rodent
  activity. Other variables are likely more important in explaining the
  variation in rodent activity.

## Is borough a confounder on the relationship between rodent activity and rental price? (Also between rodent activity and house value?)

### Is borough significantly related to the rental price?

``` r
#Linear regression model: rental price and borough
model3 <- lm(rental_price ~ borough, data = rodent_reg_merged)

# Creating Regression Model Table
model3 |>
  broom::tidy() |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 10)
```

| term                 |   estimate |      p.value |
|:---------------------|-----------:|-------------:|
| (Intercept)          | 2100.02902 | 0.0000000000 |
| boroughBrooklyn      |  805.84709 | 0.0000000000 |
| boroughManhattan     | 1148.49080 | 0.0000000000 |
| boroughQueens        |  379.75174 | 0.0000000000 |
| boroughStaten Island |   45.97548 | 0.0002538912 |

``` r
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = rental_price ~ borough, data = rodent_reg_merged)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1428.6  -406.9   -82.7   346.6  4595.9 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2100.029      4.516 465.036  < 2e-16 ***
    ## boroughBrooklyn       805.847      5.115 157.548  < 2e-16 ***
    ## boroughManhattan     1148.491      5.332 215.389  < 2e-16 ***
    ## boroughQueens         379.752      6.320  60.085  < 2e-16 ***
    ## boroughStaten Island   45.975     12.567   3.658 0.000254 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 609.1 on 150287 degrees of freedom
    ## Multiple R-squared:  0.2801, Adjusted R-squared:  0.2801 
    ## F-statistic: 1.462e+04 on 4 and 150287 DF,  p-value: < 2.2e-16

$$\text{rental price} = 2100.03 + 805.85\times \text{Brooklyn} + 1148.49\times \text{Manhattan} + 379.75\times \text{Queens} + 45.98\times \text{Staten Island}$$
Where:

- $\beta_0$: Intercept (average rental price in the reference category,
  Bronx).
- $\beta_{1,2,3,4}$: Coefficients for the borough dummy variables. These
  represent the difference in average rental price between each borough
  and the Bronx.

**Intercept ($\beta_0$):** 2100.03

This represents the average rental price in the Bronx (the reference
borough).

**Coefficient for borough ($\beta_{1,2,3,4}$):**

This indicates how much the average rental price in each borough
(Brooklyn, Manhattan, Queens, Staten Island) is higher than in the
Bronx. All are positive.

**Statistical Significance:**

The overall p-value for the model (p \< 2e-16) indicates that borough is
significantly related to rental price. The p-values for each borough
relative to the reference category (Bronx) are all statistically
significant (p \< 0.001), meaning rental prices differ significantly
across boroughs.

**Summary:**

Borough is significantly related to rental price, with notable
differences in average rental prices across boroughs.

### Is borough significantly related to the rodent activity?

``` r
#Linear regression model: rodent activity and borough
model4 <- glm(rodent_bi ~ borough, data = rodent_reg_merged)

# Creating Regression Model Table
model4 |>
  broom::tidy() |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 10)
```

| term                 |    estimate | p.value |
|:---------------------|------------:|--------:|
| (Intercept)          |  0.44100985 |       0 |
| boroughBrooklyn      | -0.08470251 |       0 |
| boroughManhattan     | -0.03503736 |       0 |
| boroughQueens        | -0.16305830 |       0 |
| boroughStaten Island | -0.34428366 |       0 |

``` r
summary(model4)
```

    ## 
    ## Call:
    ## glm(formula = rodent_bi ~ borough, data = rodent_reg_merged)
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.441010   0.003548 124.287   <2e-16 ***
    ## boroughBrooklyn      -0.084703   0.004019 -21.073   <2e-16 ***
    ## boroughManhattan     -0.035037   0.004192  -8.358   <2e-16 ***
    ## boroughQueens        -0.163058   0.004973 -32.792   <2e-16 ***
    ## boroughStaten Island -0.344284   0.009887 -34.822   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.2289086)
    ## 
    ##     Null deviance: 34830  on 149878  degrees of freedom
    ## Residual deviance: 34307  on 149874  degrees of freedom
    ##   (413 observations deleted due to missingness)
    ## AIC: 204359
    ## 
    ## Number of Fisher Scoring iterations: 2

$$\text{rodent activity}=0.441+(−0.0847)\times \text{Brooklyn}+(−0.0350)\times \text{Manhattan}+(−0.1631)\text{Queens}+(−0.3443)\times \text{StatenIsland}$$
Where:

- $\beta_0$: Intercept (baseline probability of rodent activity in the
  reference borough, Bronx).
- $\beta_{1,2,3,4}$: Coefficients representing the difference in rodent
  activity between each borough and the Bronx.

**Intercept ($\beta_0$):** 0.441

This represents the baseline probability of rodent activity in the Bronx
(the reference category).

**Coefficient for borough ($\beta_{1,2,3,4}$):**

The coefficient for each borough (Brooklyn, Manhattan, Queens, Staten
Island) represents the difference in the average probability of rodent
activity between each borough and the Bronx. For example, the
probability of rodent activity in Brooklyn is 0.0847 lower than in the
Bronx.

**Statistical Significance:**

All coefficients, including the intercept, have p \< 0.001, indicating
that the differences in rodent activity probabilities across boroughs
are statistically significant.

**Summary:**

Borough is significantly related to rodent activity.
