--25)--------------- ALL SITES - RTDS POST APRIL 2016 (COLLECTED BY NCRAS; PROCESSED BY ENCORE) ----------------- 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in rtds (excluding those classed as Brachytherapy, i.e., with RTTREATMENTMODALITY='06') -- removed this restriction due to now counting radio-isotope treatments as radiotherapy rather than chemotherapy
-- AND the appointment date (APPTDATE) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- Do not flag the patient as receiving radiotherapy if the appointment date was before 1st April 2016 

CREATE TABLE tr_rtds_2 AS 
    WITH TEMP1 AS (
        SELECT /*+ USE_HASH(tc pr) */ 
            tc.tumourid, 
            TO_DATE(pr.apptdate)-tc.diagnosisdatebest AS datediff, 
            TO_DATE(pr.apptdate) AS apptdate, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY TO_DATE(pr.apptdate), pr.attendid, pr.orgcodeprovider, pr.radiotherapyepisodeid, pr.prescriptionid) AS rk, 
            pr.orgcodeprovider AS rtds2_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN rtds.at_prescriptions_england@snapshot_hold pr 
            ON pr.patientid = tc.patientid
            AND pr.orgcodeprovider <> '7A3' 
            AND TO_DATE(pr.apptdate)-tc.diagnosisdatebest BETWEEN -31 AND tim.radio_time
            AND TO_DATE(pr.apptdate) >= TO_DATE('01-APR-16', 'dd-mon-yy')),

    TEMP2 AS ( 
        SELECT 
            tumourid,
            datediff,
            rk, 
            apptdate, 
            rtds2_trust_code 
        FROM TEMP1  
        WHERE 
            rk = 1) 

    SELECT 
        DISTINCT    
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS rtds2_flag, 
        TO_DATE(apptdate) AS rtds2_date, 
        rtds2_trust_code
    FROM TEMP2; 