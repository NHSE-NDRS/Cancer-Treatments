--2)--------------- ALL SITES - SURGERY FROM HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the operation date (opertn) occurred in the relevant timeframe create table 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table)

CREATE TABLE tr_hes_sg AS 
    WITH HES_APC AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            opdate, 
            hessg_trust_code 
        FROM (SELECT 
                tc.tumourid, 
                ho.opdate-tc.diagnosisdatebest AS datediff, 
                RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear, hl.epikeyanon, POS) AS rk, 
                ho.opdate, 
                procode3 AS hessg_trust_code
            FROM tr_tumour_cohort tc 
            INNER JOIN timeframe_lookup@casref02  tim 
                ON tim.tumouricdsite3code = tc.tumour_code 
            INNER JOIN heslive.hes_linkage_av_apc@casref02 hl 
                ON tc.patientid = hl.patientid 
            INNER JOIN heslive.hesapc_opertn@casref02 ho 
                ON ho.datayear = hl.datayear 
                AND ho.epikeyanon = hl.epikeyanon 
                AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
            INNER JOIN heslive.hesapc@casref02 ha 
                ON ha.datayear = hl.datayear 
                AND ha.epikeyanon = hl.epikeyanon 
            INNER JOIN opcs4resection_lookup@casref02 opcs 
                ON opcs.tumouricdsite3code = tc.tumour_code 
                AND TRIM(opcs.opcsresectioncode) = ho.opertn) 
        WHERE 
            rk = 1), 
    
    HES_OP AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            apptdate, 
            hessg_trust_code 
        FROM (SELECT 
                tc.tumourid, 
                op.apptdate-tc.diagnosisdatebest AS datediff, 
                RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,POS) AS rk, 
                op.apptdate, 
                procodet AS hessg_trust_code
            FROM tr_tumour_cohort tc 
            INNER JOIN timeframe_lookup@casref02 tim 
                ON tim.tumouricdsite3code = tc.tumour_code 
            INNER JOIN heslive.hes_linkage_av_op@casref02 h2 
                ON tc.patientid = h2.patientid
            INNER JOIN heslive.hesop_opertn@casref02 ho2 
                ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
            INNER JOIN heslive.hesop@casref02 op 
                ON op.datayear = h2.datayear 
                AND op.attendkeyanon = h2.attendkeyanon 
            INNER JOIN opcs4resection_lookup@casref02 opcs 
                ON opcs.tumouricdsite3code = tc.tumour_code
            WHERE 
                op.apptdate-tc.diagnosisdatebest BETWEEN -31 
                AND tim.resect_time 
                AND TRIM(opcs.opcsresectioncode) = ho2.opertn) 
        WHERE 
            rk = 1),
            
    TEMP1 AS (
        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 ELSE 1 
            END AS hessg_flag, 
            opdate AS hessg_date, 
            hessg_trust_code, 
            'HESAPC' as source
        FROM HES_APC

        UNION ALL

        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 
                ELSE 1 
            END AS hessg_flag, 
            apptdate AS hessg_date, 
            hessg_trust_code, 
            'HESOP' as source
        FROM HES_OP),
        
    TEMP2 AS (
        SELECT 
            tumourid, 
            hessg_flag, 
            hessg_date, 
            hessg_trust_code, 
            RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
          FROM TEMP1)
    
    SELECT 
        DISTINCT 
        tumourid, 
        hessg_flag, 
        hessg_date, 
        hessg_trust_code
    FROM TEMP2
    WHERE 
        rk = 1; 