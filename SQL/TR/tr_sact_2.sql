--21)-----------ALL SITES - SACT ENCORE -- FROM 1 JULY 2017 ----------------------------
-- Create a chemo flag for the tumour if: 
-- there is a record in SACT ENCORE (excluding those null or classified as 'hormones' or 'Not chemo' or 'Zoledronic acid' or 'Pamidronate' or 'Denosumab' or 'RADIUM 223' or 'LUTETIUM-177' or 'YTTRIUM-90') 
-- AND the start date of the regimen (start_date_of_regimen) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- AND the start date of the regimen is from 1 July 2017 onwards 

CREATE TABLE tr_sact_2 AS
    WITH TEMP1 AS (
        SELECT  /*+ USE_HASH(tc sp) USE_HASH(sp st) USE_HASH(st sr)*/ 
            tc.tumourid, 
            sr.start_date_of_regimen-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY sr.start_date_of_regimen, sr.merged_regimen_id, st.sact_tumour_id) AS rk,
            sr.start_date_of_regimen, 
            SUBSTR(st.organisation_code_of_provider, 1, 3) AS sact2_trust_code 
		FROM tr_tumour_cohort tc 
		INNER JOIN timeframe_lookup@casref02 TIM 
            ON TIM.tumouricdsite3code = tc.tumour_code 
		INNER JOIN sact.at_patient_england@snapshot_hold sp 
            ON tc.nhsnumber = sp.nhs_number 
		INNER JOIN sact.at_tumour_england@snapshot_hold st 
            ON sp.encore_patient_id = st.encore_patient_id
		INNER JOIN sact.at_regimen_england@snapshot_hold sr 
            ON st.sact_tumour_id = sr.sact_tumour_id 
            AND (NOT (benchmark_group IN ('NOT CHEMO', 'HORMONES', 'ZOLEDRONIC ACID', 
                                          'PAMIDRONATE', 'DENOSUMAB', 'RADIUM 223', 
                                          'LUTETIUM-177', 'YTTRIUM-90') 
                 OR benchmark_group IS NULL)) 
            AND sr.start_date_of_regimen-tc.diagnosisdatebest BETWEEN -31 AND TIM.chemo_time 
            AND sr.start_date_of_regimen >= TO_DATE('2017-07-01', 'YYYY-MM-DD')),
       
    TEMP2 AS (
        SELECT 
            tumourid, 
            datediff,
            rk, 
            start_date_of_regimen, 
            sact2_trust_code 
        FROM TEMP1
        WHERE 
            rk = 1) 
            
    SELECT 
        DISTINCT 
        tumourid,
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS sact2_flag,
        start_date_of_regimen AS sact2_date, 
        sact2_trust_code
    FROM TEMP2;
