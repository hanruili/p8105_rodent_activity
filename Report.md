Report
================

## Project Title

**Rodent Inspection Patterns in New York City: A Data-Driven Analysis
Using R**

## Motivation

Rodents in urban areas like New York City pose serious public health
risks, from spreading diseases and contaminating food supplies to
causing infrastructure damage. Addressing rodent activity is crucial for
improving urban sanitation and residents’ quality of life. This report
examines rodent inspection data, food scrap drop-off locations, and
socioeconomic indicators to uncover the factors influencing rodent
activity patterns.

## Related work

Rodents have long been a public health concern in New York City,
spreading diseases like leptospirosis and Hantavirus, and causing
physical harm through bites (Brown & Laco, 2015). Auerbach (2014)
estimated that NYC is home to 2 million rats, while Parsons et
al. (2021) and Bedoya-Pérez et al. (2021) highlighted the significant
societal costs, including food contamination, infrastructure damage, and
economic losses. As NYC residents ourselves, encountering rats is so
common, which drives the need to understand rodent activity and its link
to socioeconomic factors for better pest control strategies.

## Research questions ????????

What questions are you trying to answer? How did these questions evolve
over the course of the project? What new questions did you consider in
the course of your analysis? 1. Which neighborhoods in NYC report the
highest rodent activity, and how does this vary by season or year? Is
there a correlation between rodent activity and proximity to food scrap
drop-off locations? Do socioeconomic factors such as rental prices
(ZORI) and home values (ZHVI) influence rodent activity? During the
analysis, additional questions emerged:

Are neighborhoods with lower rent or home values more likely to report
higher rodent activity? How effective are NYC’s current rodent control
strategies based on inspection data?

## Data

### Data Source and cleaning method

All data for this analysis covers the period from January 2020 to August
2024, aligning with our research focus and ensuring consistency across
datasets.<br>

1.  **Rodent Inspection Data in NYC**: <br> Provides detailed records of
    rodent inspection results across New York City, is the main dataset
    of our study.<br>
    - [**Data
      Source**](https://data.cityofnewyork.us/Health/Rodent-Inspection/p937-wjvj/about_data)  
    - **Cleaning Method**
      - Classified “Rat activity” as 1 and all other results as 0.  
      - Extracted location (latitude, longitude, zip code, borough) and
        inspection dates for temporal and spatial analysis.
2.  **Food Scrap Drop-Off Locations in NYC**: <br> Details the locations
    for food scrap drop-off points across NYC neighborhoods, allowing us
    to analyze their correlation with rat activity patterns.<br>
    - [**Data
      Source**](https://data.cityofnewyork.us/Environment/Food-Scrap-Drop-Off-Locations-in-NYC/if26-z6xq/about_data)  
    - **Cleaning Method**
      - Extracted location (latitude, longitude, borough).<br>
3.  **Socioeconomic Data**:
    - **Zillow Observed Rent Index (ZORI)**: <br> Measures typical
      observed market rent across NYC, which helps in understanding how
      rent levels might relate to rat activity in various neighborhoods.
      ZORI captures rental trends in a repeat-rent index, reflecting the
      overall rental market. <br>
    - **Zillow Home Value Index (ZHVI)**: <br> Reflects typical home
      values across regions, showing property values and their changes
      in different neighborhoods.<br>
      - [**Data Source**](https://www.zillow.com/research/data/)
      - **Cleaning Method**
        - Extracted rental (ZORI) and home value (ZHVI) data for NYC by
          ZIP code.
        - Merged socioeconomic data with rodent inspection data by ZIP
          code, borough, year and month.

## Exploratory analysis ?????????

Visualizations, summaries, and exploratory statistical analyses. Justify
the steps you took, and show any major changes to your ideas.

## Statistical analysis????????

If you undertake formal statistical analyses, describe these in detail

## Discussion?????????

What were your findings? Are they what you expect? What insights into
the data can you make?

## Limitations????????
