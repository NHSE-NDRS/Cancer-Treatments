--20)----------------ALL SITES - SACT LEGACY -- UP TO 30th JUNE 2017 ------------------------------------- 
-- Create a chemo flag for the tumour if: 
-- there is a record in SACT LEGACY (excluding those null or classified as 'hormones' or 'Not chemo' or 'Zoledronic acid' or 'Pamidronate' or 'Denosumab' or 'RADIUM 223' or 'LUTETIUM-177' or 'YTTRIUM-90') 
-- AND the start date of the regimen (start_date_of_regimen) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- AND the start date of the regimen is up to 30th June 2017

CREATE TABLE tr_sact AS 
    WITH TEMP1 AS (
        SELECT 
            tc.tumourid, 
            sr.start_date_of_regimen-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY sr.start_date_of_regimen, sr.merged_regimen_id, st.merged_tumour_id) AS rk,
            sr.start_date_of_regimen, 
            SUBSTR(st.organisation_code_of_provider, 1, 3) AS sact_trust_code 
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN sact_legacy.patient@casref02 sp 
            ON tc.nhsnumber = sp.nhs_number 
        INNER JOIN sact_legacy.tumour@casref02 st 
            ON sp.merged_patient_id = st.merged_patient_id 
        INNER JOIN sact_legacy.regimen@casref02 SR 
            ON st.merged_tumour_id = sr.merged_tumour_id 
            AND (NOT (benchmark_group IN ('NOT CHEMO', 'HORMONES', 'ZOLEDRONIC ACID', 
                                          'PAMIDRONATE', 'DENOSUMAB', 'RADIUM 223', 
                                          'LUTETIUM-177', 'YTTRIUM-90') 
                    OR benchmark_group IS NULL)) 
            AND sr.start_date_of_regimen-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
            AND sr.start_date_of_regimen <= TO_DATE('2017-06-30', 'YYYY-MM-DD')),
            
    TEMP2 AS (
        SELECT 
            tumourid,
            datediff,
            rk, 
            start_date_of_regimen, 
            sact_trust_code 
        FROM TEMP1
        WHERE 
            rk = 1)
    
    SELECT 
        DISTINCT 
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS sact_flag, 
        start_date_of_regimen AS sact_date, 
        sact_trust_code
    FROM TEMP2; 
    
    
    