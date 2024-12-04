data_clean
================
Meitong Zhou
2024-12-04

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
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(readr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
food_scrap_drop_off = read_csv(
  file = "Food_Scrap_Drop-Off_Locations_in_NYC_20241118.csv",
  na = c(".", "NA", "")
) |>
  clean_names()
```

    ## Rows: 591 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): Borough, NTAName, SiteName, SiteAddr, Hosted_By, Open_Month, Day_H...
    ## dbl (12): BoroCD, CouncilDis, ct2010, BBL, BIN, Latitude, Longitude, PoliceP...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
write_csv(food_scrap_drop_off, "shiny app data/data/food_scrap_drop_off.csv")
```

``` r
zori_data = read.csv("Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv") 
zori_data = janitor::clean_names(zori_data) |>
    pivot_longer(
    cols = starts_with("x20"),
    names_to = "date",
    values_to = "rental_price"
  )

url = "https://p8105.com/data/zip_codes.html"
zip_code_data = read_html(url) |>
  html_table(fill = TRUE)
zip_code_data = zip_code_data[[1]]
zip_code_data = janitor::clean_names(zip_code_data)
```

``` r
zip_code_data = zip_code_data |>
  mutate(borough = case_when(
    county == "New York" ~ "Manhattan",
    county == "Kings" ~ "Brooklyn",
    county == "Queens" ~ "Queens",
    county == "Bronx" ~ "Bronx",
    county == "Richmond" ~ "Staten Island"
  ))
```

``` r
zori_data = zori_data |>
  rename(zip_code = region_name)
head(zori_data)
```

    ## # A tibble: 6 × 11
    ##   region_id size_rank zip_code region_type state_name state city     metro      
    ##       <int>     <int>    <int> <chr>       <chr>      <chr> <chr>    <chr>      
    ## 1     62080         4    11368 zip         NY         NY    New York New York-N…
    ## 2     62080         4    11368 zip         NY         NY    New York New York-N…
    ## 3     62080         4    11368 zip         NY         NY    New York New York-N…
    ## 4     62080         4    11368 zip         NY         NY    New York New York-N…
    ## 5     62080         4    11368 zip         NY         NY    New York New York-N…
    ## 6     62080         4    11368 zip         NY         NY    New York New York-N…
    ## # ℹ 3 more variables: county_name <chr>, date <chr>, rental_price <dbl>

``` r
merged_data = merge(zori_data, zip_code_data, by = "zip_code")
head(merged_data) 
```

    ##   zip_code region_id size_rank region_type state_name state     city
    ## 1    10001     61615      4444         zip         NY    NY New York
    ## 2    10001     61615      4444         zip         NY    NY New York
    ## 3    10001     61615      4444         zip         NY    NY New York
    ## 4    10001     61615      4444         zip         NY    NY New York
    ## 5    10001     61615      4444         zip         NY    NY New York
    ## 6    10001     61615      4444         zip         NY    NY New York
    ##                                   metro     county_name        date
    ## 1 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_01_31
    ## 2 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_02_28
    ## 3 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_03_31
    ## 4 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_04_30
    ## 5 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_05_31
    ## 6 New York-Newark-Jersey City, NY-NJ-PA New York County x2015_06_30
    ##   rental_price   county state_fips county_code county_fips  file_date
    ## 1     3855.089 New York         36          61       36061 07/25/2007
    ## 2     3892.376 New York         36          61       36061 07/25/2007
    ## 3     3898.212 New York         36          61       36061 07/25/2007
    ## 4     3969.644 New York         36          61       36061 07/25/2007
    ## 5     4033.221 New York         36          61       36061 07/25/2007
    ## 6     4070.808 New York         36          61       36061 07/25/2007
    ##          neighborhood   borough
    ## 1 Chelsea and Clinton Manhattan
    ## 2 Chelsea and Clinton Manhattan
    ## 3 Chelsea and Clinton Manhattan
    ## 4 Chelsea and Clinton Manhattan
    ## 5 Chelsea and Clinton Manhattan
    ## 6 Chelsea and Clinton Manhattan

``` r
final_data = merged_data |>
  select(zip_code, borough, rental_price, neighborhood, date)
final_data = final_data |> 
  filter(!is.na(rental_price))
head(final_data)
```

    ##   zip_code   borough rental_price        neighborhood        date
    ## 1    10001 Manhattan     3855.089 Chelsea and Clinton x2015_01_31
    ## 2    10001 Manhattan     3892.376 Chelsea and Clinton x2015_02_28
    ## 3    10001 Manhattan     3898.212 Chelsea and Clinton x2015_03_31
    ## 4    10001 Manhattan     3969.644 Chelsea and Clinton x2015_04_30
    ## 5    10001 Manhattan     4033.221 Chelsea and Clinton x2015_05_31
    ## 6    10001 Manhattan     4070.808 Chelsea and Clinton x2015_06_30

``` r
write.csv(final_data, file = "shiny app data/data/final_data.csv", row.names = FALSE)
```
