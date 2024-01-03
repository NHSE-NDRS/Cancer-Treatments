# Chemotherapy, Radiotherapy, and Surgical Tumour Resections for Cancer Patients in England (2013-2020)

###### This code repository accompanies the National Disease Registration Service's (NDRS) treatment data, presenting population-based statistics on the patients recorded to have received chemotherapy, radiotherapy and surgical tumour resections for their tumour in England.
###### The code currently available in this repository will produce data for patients diagnosed with cancer in England between 2013-2020, as displayed in the tool. Both are updated annually to include an additional year of cancer diagnoses.
###### Statistics are calculated by linking the cancer registration data to treatment data tables for chemotherapy, tumour resections, and radiotherapy treatments.
###### To find out more about the publication and the treatment data captured, visit the [NDRS work programme page](https://digital.nhs.uk/ndrs/our-work/ncras-work-programme/treatment-data) on the NDRS website.
###### The current Standard Operating Procedure (SOP) detailing the methodology, as well as previous SOP versions and data releases, can also be found on the NDRS work programme page, or you can access the [current SOP](https://digital.nhs.uk/ndrs/our-work/ncras-work-programme/treatment-data/cas-sop-4.8) directly.
###### To view and download the data via the interactive tools, visit the [Treatment by Demographic Factors](https://nhsd-ndrs.shinyapps.io/treatment_by_demographic_factors/) and [Treatment by Cancer Alliance](https://nhsd-ndrs.shinyapps.io/treatment_by_cancer_alliance/) dashboards.

##### To clone the repository:

```shell
git clone https://github.com/ncpeters/Cancer-Treatments.git
cd Cancer-Treatments
```

For further guidance on using Git, please see our [RAP Community of Practice](https://nhsdigital.github.io/rap-community-of-practice/).

Repository owner: Pathways and Datasets Team from the National Disease Registration Service, which is part of NHS England.

Email: nhsdigital.ndrsanalysis@nhs.net

#### Producing the publication

 - **Treatment_SQL_code.sql** - This is the SQL code used to generate the tumour cohort table and create the final treatments table containing a 'flag' (count) if a patient has recieved chemotherapy, radiotherapy and/or surgery treatments for each tumour diagnosis. The code uses the opcs4resection and timeframe lookup tables so that treatments are only counted if they are relevant to the tumour diagnosis.
 - **data_preparation.RMD** - This script takes data from the treatment flags table produced by the SQL code and creates .csv files in the correct format for the R Shiny apps to use.
 - **Treatments_demography.R** and **Treatments_geography.R** - These scripts build the R Shiny apps, including the design elements for presenting the demographic and geographic breakdowns, respectively.

#### License

The codebase is released under the MIT License. This covers both the codebase and any code in the documentation.

Any HTML or Markdown documentation is [© Crown Copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms of the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

The data is signed off as non-disclosive and is released under an [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). You are free to copy, publish, distribute and transmit the information, and to adapt it and include it in your own products.

The attribution statement that must be included with any reuse of the data is:

_This work uses data that has been provided by patients and collected by the NHS as part of their care and support. The data is collated, maintained and quality assured by the National Cancer Registration and Analysis Service, which is part of NHS England (NHSE)._
