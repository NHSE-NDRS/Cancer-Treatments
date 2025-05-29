------------------------------------------------------------------------------ 
-----------CREATE SURGERY FLAG TABLES - STAGE SPECIFIC RESECTIONS------------ 
------------------------------------------------------------------------------ 
--3)---------------- AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a percutaneous radiofrequency, thermal, chemical AND microwave ablation of lesion of liver (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 

CREATE TABLE table_name_var AS 
    WITH temp1 AS (
        SELECT 
            tc.tumourid, 
            avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, 
            avtreat.eventdate, 
            avtreat.trust_code AS avsg_trust_code 
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN avyyyy.at_treatment_england@casref02 avtreat 
            ON avtreat.tumourid = tc.tumourid 
            AND eventcode IN ('01a', '01b', '01z', '01c') 
            AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 
            AND tim.resect_time) 
            AND avtreat.opcs4_code IN opcs4_code_var 
            AND tc.tumour_code IN site_code_var),
            
    temp2 AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            eventdate, 
            avsg_trust_code     
          FROM temp1 
          WHERE rk = 1)
          
    SELECT 
        DISTINCT 
        tumourid,
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS field_name_var, 
        eventdate AS avsg_date, 
        avsg_trust_code
    FROM temp2;
