--22)--------------- ALL SITES - CHEMO FROM HES ------------------ 
-- Create a chemo flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a chemo opcs-4 code in one of the operation fields 
-- and the operation date (opertn) occurred in the relevant timeframe create table 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table)

CREATE TABLE tr_hes_ct AS 
    WITH HES_APC AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            opdate, 
            hesct_trust_code 
        FROM (SELECT 
                tc.tumourid, 
                ho.opdate-tc.diagnosisdatebest AS datediff, 
                RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear, hl.epikeyanon, POS) AS rk, 
                ho.opdate, 
                procode3 AS hesct_trust_code
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
                AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
                AND ho.opertn IN ('Y123', 'X962', 'X749', 'X748', 'X739', 'X738', 'X731', 'X729', 
                                  'X728', 'X724', 'X723', 'X722', 'X721', 'X719', 'X718', 'X715',
                                  'X714', 'X713', 'X712', 'X711', 'X709', 'X708', 'X705', 'X704',
                                  'X703', 'X702', 'X701', 'X385', 'X384', 'X374', 'X373', 'X353',
                                  'X352', 'T482', 'T133', 'A106')) 
        WHERE rk = 1),
        
    HES_OP AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            apptdate, 
            hesct_trust_code 
        FROM (SELECT 
                tc.tumourid, 
                op.apptdate-tc.diagnosisdatebest AS datediff, 
                RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear, h2.attendkeyanon, POS) AS rk, 
                op.apptdate, 
                procodet AS hesct_trust_code
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
                op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
                AND ho2.opertn IN ('Y123', 'X962', 'X749', 'X748', 'X739', 'X738', 'X731', 'X729',
                                   'X728', 'X724', 'X723', 'X722', 'X721', 'X719', 'X718', 'X715',
                                   'X714', 'X713', 'X712', 'X711', 'X709', 'X708', 'X705', 'X704',
                                   'X703', 'X702', 'X701', 'X385', 'X384', 'X374', 'X373', 'X353',
                                   'X352', 'T482', 'T133', 'A106')) 
        WHERE rk = 1),
        
    TEMP1 AS (
        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 
                ELSE 1 
            END AS hesct_flag, 
            opdate AS hesct_date, 
            hesct_trust_code, 
            'HESAPC' as source
        FROM HES_APC

        UNION ALL

        SELECT 
            DISTINCT 
            tumourid, 
            CASE 
                WHEN datediff IS NULL THEN 0 
                ELSE 1 
            END AS hesct_flag, 
            apptdate AS hesct_date, 
            hesct_trust_code, 
            'HESOP' as source
        FROM HES_OP),
   
    TEMP2 AS (
        SELECT 
            tumourid, 
            hesct_flag,
            hesct_date, 
            hesct_trust_code,
            RANK() OVER (PARTITION BY tumourid ORDER BY hesct_date, source) as rk
        FROM TEMP1)

    SELECT 
        DISTINCT 
        tumourid, 
        hesct_flag, 
        hesct_date, 
        hesct_trust_code
    FROM TEMP2
    WHERE 
        rk = 1;
        
        
        