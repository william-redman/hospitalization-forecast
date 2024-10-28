# Target Data for Ontario's Hospital Bed Occupancy (2024-25)

## Overview

The `target-data` folder contains the **CSV** data that the forecasts will be compared against. This data serves as the "gold standard" for evaluating the forecasting models. For the current Flu season, the data is stored in `target-data/season_2024_2025/hospitalization-data.csv` file.

### Table of Contents
- [Hospitalization Data](#hospitalization-data)
- [Accessing Target Data](#accessing-target-data)
- [Data Processing](#data-processing)
- [Additional Resources](#additional-resources)

## Hospitalization Data

### Source
Public Health Ontario, Ontario Repiratory Virus Tool.

Our hub's hospitalization prediction targets (`flu`, `covid` and `rsv`) are based on raw data that is stored in **Outcomes** table under column `Number`, and it is updated weekly on every Friday.

Previously collected data from earlier seasons are included in the `.auxiliary-data\target-data-archive` directory. The COVID-19 data starts from the date `2020-04-04` in `season_2019_2020` sub-directory. There are no data reports available for RSV and Influenza Bed Occupancy before the date `2022-12-03` in `season_2022_2023` sub-directory and hence these column values are not included in previous season data files. 

### Important Note
- The counts for all three of our targets — `flu`, `covid` and `rsv` — reflect hospital bed occupancy for individuals who are in the hospital with the active target disease, regardless of whether their admission was due to the target disease itself or another illness, with a subsequent positive test for the target disease.


## Accessing Target Data

**Primary Data Source:** [Public Health Ontario: Ontario Respiratory Virus Tool](https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool)

Since the tool is an embedded PowerBI dashboard, hospital bed occupancy data can be exported using the "Export Data" option located in the top-right corner of the data table menu.

### CSV Files
A set of CSV files is updated weekly with the latest observed values for [target type, e.g., incident hospitalizations]. These are available at:
- `./target-data/season_2024_2025/hospitalization-data.csv`
- `./auxiliary-data/data.csv` (raw file)

## Data Processing

### Source Field
The hospitalization target data is computed from the **Number** field in the `./auxiliary-data/data.csv` file, which captures weekly admissions with confirmed diagnoses of the target diseases. The data is filtered for `total bed occupancy cases` related to the target diseases (COVID-19, RSV and Influenza) and then pivoted across these diseases to generate the target values as separate columns.

### Location Mapping
The data report provided by the Ontario Respiratory Virus Tool had bed occupancy information for every Public Health Unit in Ontario. These [Public Health Units](https://data.ontario.ca/dataset/public-health-unit-boundaries) were then mapped to [Ontario Health Regions](https://data.ontario.ca/dataset/ontario-s-health-region-geographic-data/resource/fbf0b8f8-a77f-4532-aa20-1b285ab0587d) using the data provided by [Ontario Data Catalogue](https://data.ontario.ca/) and also the values were aggregated to Ontario Health Regions. 


## Additional Resources

Here are additional resources relevant to the data:

- [Data Dictionary](https://www.publichealthontario.ca/-/media/Documents/R/2023/respiratory-virus-tool-user-guide.pdf?sc_lang=en&rev=44a28312271e4f0f91556c5023bb78c3&hash=26AD3D217EAB4F480C5C28714E4B0CAA)
- [Technical Notes](https://www.publichealthontario.ca/-/media/Data-Files/respiratory-virus-tool-technical-notes.pdf?sc_lang=en&rev=b00f64cd5e5c48afb2155bc89899d338&hash=5ACF5F23C24434CDD512E2BCB3DDAA8E)

---
