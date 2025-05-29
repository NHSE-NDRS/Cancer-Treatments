--24)---------------ALL SITES - RTDS PRE APRIL 2016 (COLLECTED BY NATCANSAT)----------- 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in rtds (excluding those classed as Brachytherapy, i.e., with RTTREATMENTMODALITY='06') -- removed this restriction due to now counting radio-isotope treatments as radiotherapy rather than chemotherapy
-- AND the appointment date (APPTDATE) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_rtds AS 
    WITH TEMP1 AS (
        SELECT
            tc.tumourid, 
            rl.apptdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY rl.apptdate, rl.attendid, rl.orgcodeprovider, pr.radiotherapyepisodeid, pr.prescriptionid) AS rk, 
            rl.apptdate, 
            CAST(SUBSTR(pr.orgcodeprovider, 1, 3) AS VARCHAR(3)) AS rtds_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN rtds2016.opcds_cas1712_linkage rl 
            ON tc.patientid = rl.patientid 
            AND rl.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.radio_time 
        INNER JOIN rtds2016.rtds_prescriptions pr 
            ON pr.orgcodeprovider = rl.orgcodeprovider 
            AND pr.attendid = rl.attendid 
            AND pr.apptdate = rl.apptdate), 
    
    TEMP2 AS (
        SELECT 
            tumourid,
            datediff,
            rk, 
            apptdate, 
            rtds_trust_code 
        FROM TEMP1 
        WHERE 
            rk = 1)

    SELECT 
        DISTINCT 
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS rtds_flag, 
        apptdate AS rtds_date, 
        rtds_trust_code
    FROM TEMP2;