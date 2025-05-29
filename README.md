# Radiotherapy, SACT and Tumour Resections for Cancer Patients in England (2013-2022)

###### This code repository accompanies the National Disease Registration Service's (NDRS) treatment data, presenting population-based statistics on the patients recorded to have received radiotherapy, SACT (Systemic Anti-Cancer Therapy) and surgical tumour resections for their tumour in England.
###### The code currently available in this repository will produce data for patients diagnosed with cancer in England between 2013-2022, as displayed in the tool. Both are updated annually to include an additional year of cancer diagnoses.
###### Statistics are calculated by linking the cancer registration data to treatment data tables for radiotherapy, SACT and tumour resection treatments.
###### To find out more about the publication and the treatment data captured, or to view and download the current Standard Operating Procedure (SOP) detailing the methodology, visit the [cancer treatments page](https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-treatments) on the NDRS website.
###### To view and download the data via the interactive tools, visit the [Cancer Treatments 2013-2022 dashboard](https://nhsd-ndrs.shinyapps.io/cancer_treatments/).

##### To clone the repository:

```shell
git clone https://github.com/NHSE-NDRS/Cancer-Treatments.git
cd Cancer-Treatments
```

For further guidance on using Git, please see our [RAP Community of Practice](https://nhsdigital.github.io/rap-community-of-practice/).

Repository owner: Pathways and Datasets Team from the National Disease Registration Service, which is part of NHS England.

Email: ndrsenquiries@nhs.net


#### Producing the publication

##### Producing the treatment_table_1322_4p10 table

 - **R/00_setup.R** - This is the initial source script that includes the parameters used for the latest release.
 - **R/01_read_in_SQL.R** - This is the main R script that builds the treatment_table_1322_4p10 table. This reads in the opcs4resection and timeframe lookup tables (stored in /R/Lookups) to ensure treatments are only counted if they are relevant to the tumour diagnosis.
This script reads in thirteen SQL scripts (stored in /SQL) that are executed to create individual tables that are then used to build the main treatment_table_1322_4p10 table. The treatment_table_1322_4p10 table contains a 'flag' (count) if a patient has received radiotherapy (rt_flag), SACT (ct_flag) and/or a tumour resection (sg_flag) for each tumour diagnosis.

    - SQL/tr_tumour_cohort.sql
This is the first script to be pulled into R/01_read_in_SQL.R. This cleans and extracts tumour-level data for the cohort of interest.
    - SQL/TR/
A for-loop reads in all SQL scripts listed below (stored in SQL/TR/) and creates tables for each. Each script links to a treatment data set (at_treatment_england, Systemic Anti-Cancer Therapy (SACT), Radiotherapy (RTDS), Hospital Episode Statistics (HES)) and creates a binary treatment flag to identify whether each tumour was treated with radiotherapy, SACT, and/or tumour resection within a relevant timeframe (using the timeframe lookup tables (stored in /R/Lookups)).
        - SQL/TR/tr_av_ct.sql
This script links to the at_treatment_england data set and creates a avct_flag to identify SACT treatment.
        - SQL/TR/tr_av_rt.sql
This script links to the at_treatment_england data set and creates a avrt_flag to identify radiotherapy treatment.
        - SQL/TR/tr_av_sg.sql
This script links to the at_treatment_england data set and creates a avsg_flag to identify a relevant tumour resection (listed in the opcs4resection lookup).
        - SQL/TR/tr_hes_ct.sql
This script links to the HES inpatient and outpatient data sets and creates a hesct_flag to identify SACT treatment.
        - SQL/TR/tr_hes_sg.sql
This script links to the HES inpatient and outpatient data sets and creates a hessg_flag to identify a relevant tumour resection (listed in the opcs4resection lookup).
        - SQL/TR/tr_rtds.sql
This script links to the RTDS data set collected by NATCANSAT, pre-April 2016 and creates an rtds_flag to identify radiotherapy treatment for each tumour.
        - SQL/TR/tr_rtds_2.sql
This script links to the RTDS data set collected by NHSE (previously PHE), post April 2016 and creates an rtds_flag to identify radiotherapy treatment for each tumour.
        - SQL/TR/tr_sact.sql
This script links to the SACT data set pre-2018 and creates a sact_flag to identify SACT treatment for each tumour.
        - SQL/TR/tr_sact_2.sql
This script links to the SACT data set post 2018 and creates a sact_flag to identify SACT treatment for each tumour.
    - SQL/tr_av_site.sql
This script links to the at_treatment_england data set and creates a site-specific flag (e.g. liver_avtreat) to identify a relevant tumour resection based on stage-specific rules for relevant sites and OPCS-4 procedure codes.
    - SQL/tr_hes_site.sql
This script links to the HES inpatient and outpatient data sets and creates a site-specific flag (e.g. tr_hes_liver) to identify a relevant tumour resection based on stage-specific rules for relevant sites and OPCS-4 procedure codes.
    - SQL/treatment_table_13yy.sql
This script creates the final treatment_table_1322_4p10 table drawing on all the above tables to create three consolidated flags: radiotherapy (rt_flag), SACT (ct_flag) and/or a tumour resection (sg_flag) for each tumour diagnosis.


##### Producing the app output

 - **data_preparation.RMD** - This script takes data from the treatment flags table produced by the SQL code and creates CSV files in the correct format for the R Shiny apps to use (app code stored on a separate repository).
   

Note: we recomend using a code or text editor such as Visual Studio (VS) Code for editing the CSV files within this repository. Useful extensions to use for this purpose within VS Code are 'Edit csv', 'Excel Viewer', and 'Rainbow CSV'. This reduces the risk of conflicts caused by unintended additional commas separating values in Microsoft Excel.

#### License

The codebase is released under the MIT License. This covers both the codebase and any code in the documentation.

Any HTML or Markdown documentation is [© Crown Copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms of the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

The data is signed off as non-disclosive and is released under an [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). You are free to copy, publish, distribute and transmit the information, and to adapt it and include it in your own products.

The attribution statement that must be included with any reuse of the data is:

_This work uses data that has been provided by patients and collected by the NHS as part of their care and support. The data is collated, maintained and quality assured by the National Cancer Registration and Analysis Service, which is part of NHS England (NHSE)._
