# Setup and connect to CAS -----------------------------------------------------
source("R/00_setup.R")

casref <- createConnection(username = CAS_username)

# Load in timeframe_lookup -----------------------------------------------
dbSendQueryOracle(casref, "DROP TABLE timeframe_lookup PURGE")
timeframe_lookup <- fread("R/Lookups/timeframe_lookup.csv")
DBI::dbWriteTable(casref, 
                  "timeframe_lookup", 
                  timeframe_lookup)

# Load in opcs4resection_lookup ------------------------------------------
dbSendQueryOracle(casref, "DROP TABLE opcs4resection_lookup PURGE")
opcs4resection_lookup <- fread("R/Lookups/opcs4resection_lookup.csv")
DBI::dbWriteTable(casref, 
                  "opcs4resection_lookup", 
                  opcs4resection_lookup)


# Create tables: tr_tumour_cohort ----------------------------------------------
# Q: Should we replace site_icd10r4_o2_from2013 with site_code and remove 
#   site_icd10_o2_3char_pre2013?
# Still potential to replace site hardcoding with a lookup
data <- readr::read_file("SQL/tr_tumour_cohort.sql")
data <- gsub(";", "", data)
data <- gsub("yyyy", end_year, data)

dbSendQueryOracle(casref, "DROP TABLE tr_tumour_cohort PURGE")
dbSendQueryOracle(casref, data)


# Create tr tables -------------------------------------------------------------
# This will create: 
# - tr_av_sg 
# - tr_hes_sg
# - tr_av_ct
# - tr_hes_ct
# - tr_av_rt, 
# - tr_rtds
# - tr_rtds_2 
# - tr_sact 
# - tr_sact_2

filenames <- list.files("SQL/TR", pattern = "*.sql", full.names = F)

# Drop the tables if previously run
for (i in filenames){
  
  i <- gsub(".sql", "", i)
  dbSendQueryOracle(casref, paste0("DROP TABLE ", i, " PURGE"))
  
}


for (i in filenames){
  
  i <- gsub(".sql", "", i)
  
  data <- readr::read_file(paste0("SQL/TR/", i, ".sql"))
  data <- gsub(";", "", data)
  data <- gsub("snapshot_hold", snapshot, data)
  data <- gsub("yyyy", end_year, data)
  
  
  tryCatch(
    expr = {
      # Create the table
      dbSendQueryOracle(casref, data)
      
      # Index the table
      dbSendQueryOracle(casref, 
                        paste0("CREATE UNIQUE INDEX ", i, "_tumourid_uq ON ", i, 
                               " ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_table_stats('", CAS_username, 
                               "', '", i, "')}"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_index_stats ('", CAS_username,
                               "', '", i, "_tumourid_uq')}"))
      
      print(paste0("Successfully created table: ", i))
      
    },
    
    message = function(m){
      
      print(paste0("Error when creating table: ", i))
      
    }
    
  )
  
}

# Create tr_av_site tables -----------------------------------------------------
tr_av_lookups <- fread("R/Lookups/tr_site_lookups.csv")

tr_av_site <- readr::read_file("SQL/tr_av_site.sql")

sites <- tr_av_lookups[, unique(AV_TABLE_NAME)]

# This will create: 
# - tr_av_liver
# - tr_av_oesoph
# - tr_av_stomach
# - tr_av_bladder
# - tr_av_bladder_Dcodes
# - tr_av_conebiops
# - tr_av_lymph
# - tr_av_colorec
# - tr_av_coloappen

# Drop the tables if previously run
for (i in sites){
  
  # Drop the tables if previously run
  dbSendQueryOracle(casref, paste0("DROP TABLE ", i, " PURGE"))
  
}


for (i in sites){
  
  site_lookup <- tr_av_lookups[AV_TABLE_NAME == i, ]
  
  data <- gsub(";", "", tr_av_site)
  
  # colorectal appendectomies and non-malignant bladder have to be restricted using site_icd10
  if(i %in% c("tr_av_coloappen","tr_av_bladder_Dcodes")){
    
    data <- gsub("AND tc.tumour_code IN site_code_var", 
                 "AND tc.site_icd10 IN site_code_var", 
                 data)
    
  }
  
  data <- gsub("yyyy", end_year, data)
  data <- gsub("table_name_var", site_lookup[, AV_TABLE_NAME], data)
  data <- gsub("field_name_var", site_lookup[, AV_FIELD_NAME], data)
  data <- gsub("opcs4_code_var", site_lookup[, OPCS4_CODE], data)
  data <- gsub("site_code_var", site_lookup[, SITE_CODE], data)
  
  
  tryCatch(
    expr = {
      # Create the table
      dbSendQueryOracle(casref, data)
      
      # Index the table
      dbSendQueryOracle(casref, 
                        paste0("CREATE UNIQUE INDEX ", i, "_tumourid_uq ON ", i, 
                               " ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_table_stats('", CAS_username, 
                               "', '", i, "')}"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_index_stats ('", CAS_username,
                               "', '", i, "_tumourid_uq')}"))
      
      print(paste0("Successfully created table: ", i))
      
    },
    
    message = function(m){
      
      print(paste0("Error when creating table: ", i))
      
    }
  )
  
}

# Create tr_hes_site tables ----------------------------------------------------

# This will create: 
# - tr_hes_liver
# - tr_hes_oesoph
# - tr_hes_stomach
# - tr_hes_bladder
# - tr_hes_bladder_Dcodes
# - tr_hes_conebiops
# - tr_hes_lymph
# - tr_hes_colorec
# - tr_hes_coloappen

tr_hes_site <- readr::read_file("SQL/tr_hes_site.sql")

sites <- tr_av_lookups[, unique(HES_TABLE_NAME)]

# Drop the tables if previously run
for (i in sites){
  
  # Drop the tables if previously run
  dbSendQueryOracle(casref, paste0("DROP TABLE ", i, " PURGE"))
  
}

for (i in sites){
  
  site_lookup <- tr_av_lookups[HES_TABLE_NAME == i, ]
  
  data <- gsub(";", "", tr_hes_site)
  
  # colorectal appendectomies and non-malignant bladder have to be restricted using site_icd10
  if(i %in% c("tr_hes_coloappen","tr_hes_bladder_Dcodes")){
    
    data <- gsub("AND tc.tumour_code in site_code_var", 
                 "AND tc.site_icd10 IN site_code_var", 
                 data)
    
  }
  
  data <- gsub("table_name_var", site_lookup[, HES_TABLE_NAME], data)
  data <- gsub("field_name_var", site_lookup[, HES_FIELD_NAME], data)
  data <- gsub("opcs4_code_var", site_lookup[, OPCS4_CODE], data)
  data <- gsub("site_code_var", site_lookup[, SITE_CODE], data)
  
  tryCatch(
    expr = {
      # Create the table
      dbSendQueryOracle(casref, data)
      
      # Index the table
      dbSendQueryOracle(casref, 
                        paste0("CREATE UNIQUE INDEX ", i, "_tumourid_uq ON ", i, 
                               " ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_table_stats('", CAS_username, 
                               "', '", i, "')}"))
      
      dbSendQueryOracle(casref, 
                        paste0("{call dbms_stats.gather_index_stats ('", CAS_username,
                               "', '", i, "_tumourid_uq')}"))
      
      print(paste0("Successfully created table: ", i))
      
    },
    
    message = function(m){
      
      print(paste0("Error when creating table: ", i))
      
    }
  )
  
}

# Create table: treatment_table_13yy_4p9 ---------------------------------------
# Drop the table if previously run
dbSendQueryOracle(casref, paste0("DROP TABLE treatment_table_13", substr(end_year, 3, 4), "_", SOP_number))

data <- readr::read_file("SQL/treatment_table_13yy.sql")
data <- gsub(";", "", data)
data <- gsub("yyyy(?!['\"-])", end_year, data, perl = TRUE)#Do not substitute if yyyy is followed by ' or " or - i.e yyyy in a date string
data <- gsub("snapshot_hold", snapshot, data)
data <- gsub("treatment_table_13yy", 
             paste0("treatment_table_13", substr(end_year, 3, 4), "_", SOP_number), 
             data)
data <- gsub("IMD_years_var", IMD_19_years, data)
data <- gsub("charlson_table_hold", charlson_table, data)
data <- gsub("LSOAyy", paste0("LSOA", lsoa_year), data)
data <- gsub("IMD_table_hold", paste0(IMD_table), data)
data <- gsub("icb_20yy", paste0("icb_20", geography_year), data)
data <- gsub("canalliance_20yy", paste0("canalliance_20", geography_year), data)

dbSendQueryOracle(casref, data)

# Grant access -----------------------------------------------------------------
dbSendQueryOracle(casref, paste0("grant select on treatment_table_13", substr(end_year, 3, 4), "_", SOP_number, " to ", QA_analyst))
dbSendQueryOracle(casref, paste0("grant select on opcs4resection_lookup to ", QA_analyst))
dbSendQueryOracle(casref, paste0("grant select on timeframe_lookup to ", QA_analyst))
dbSendQueryOracle(casref, paste0("grant select on tr_tumour_cohort to ", QA_analyst))
