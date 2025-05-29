--4)---------------- HES------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is a percutaneous radiofrequency, thermal, chemical AND microwave ablation of lesion of liver (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE table_name_var AS 
    WITH HES_APC AS (
        SELECT 
            tc.tumourid, 
            ho.opdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear, hl.epikeyanon, pos) AS rk, 
            ho.opdate, 
            procode3 AS hessg_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN heslive.hes_linkage_av_apc@casref02 hl 
            ON tc.patientid = hl.patientid
        INNER JOIN heslive.hesapc@casref02 ha 
            ON ha.datayear = hl.datayear 
            AND ha.epikeyanon = hl.epikeyanon 
        INNER JOIN heslive.hesapc_opertn@casref02 ho 
            ON ho.datayear = hl.datayear 
            AND ho.epikeyanon = hl.epikeyanon 
            AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 
            AND tim.resect_time 
            AND ho.opertn IN opcs4_code_var
            AND tc.tumour_code in site_code_var),
            
    HES_OP AS (
        SELECT 
            tc.tumourid, 
            op.apptdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear, h2.attendkeyanon, pos) AS rk, 
            op.apptdate, 
            procodet AS hessg_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN heslive.hes_linkage_av_op@casref02 h2 
            ON tc.patientid = h2.patientid
        INNER JOIN heslive.hesop@casref02 op 
            ON op.datayear = h2.datayear 
            AND op.attendkeyanon = h2.attendkeyanon 
        INNER JOIN heslive.hesop_opertn@casref02 ho2 
            ON ho2.datayear = h2.datayear 
            AND ho2.attendkeyanon = h2.attendkeyanon
        WHERE 
            op.apptdate-tc.diagnosisdatebest BETWEEN -31 
            AND tim.resect_time 
            AND ho2.opertn IN opcs4_code_var
            AND tc.tumour_code in site_code_var),

    TEMP1 AS (
        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 
                ELSE 1 
            END AS field_name_var, 
            opdate AS hessg_date, 
            hessg_trust_code, 
            'HESAPC' as source
        FROM (SELECT 
                tumourid, 
                datediff, 
                rk, 
                opdate, 
                hessg_trust_code 
              FROM HES_APC 
              WHERE 
                rk = 1)

        UNION ALL

        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 
                ELSE 1 
            END AS field_name_var, 
            apptdate AS hessg_date, 
            hessg_trust_code, 
            'HESOP' as source
        FROM (SELECT 
                tumourid, 
                datediff, 
                rk, 
                apptdate, 
                hessg_trust_code 
              FROM HES_OP
              WHERE 
                rk = 1)),
              
    temp2 AS (
        select 
            tumourid, 
            field_name_var, 
            hessg_date, 
            hessg_trust_code, 
            RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
        FROM temp1)
    
    SELECT 
        DISTINCT 
        tumourid, 
        field_name_var, 
        hessg_date, 
        hessg_trust_code
    FROM temp2
    WHERE 
        rk = 1; 

