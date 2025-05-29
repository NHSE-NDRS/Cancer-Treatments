CREATE TABLE tr_tumour_cohort AS
    -- Skin cancer have been defined in the at_tumour_skin table and so the skin 
    -- cohort needs to be selected separately to the cohort for other tumours and 
    -- joined together 
    WITH skin_cohort AS (
        -- Create cohort of non-keratinocyte skin cancers 
        SELECT 
            ats.patientid, 
            ats.tumourid, 
            ats.diagnosisdatebest, 
            ats.diagnosisyear, 
            avt.nhsnumber, 
            avt.figo, 
            avt.gender, 
            avt.ethnicity, 
            avt.morph_icd10_o2, 
            avt.fiveyearageband, 
            avt.age, 
            avt.dedup_flag, 
            CASE 
                WHEN avt.site_icd10 IS NULL THEN avt.site_coded 
                ELSE avt.site_icd10 
            END AS site_icd10, 
            CASE 
                WHEN avt.site_icd10_3char IS NULL THEN avt.site_coded_3char 
                ELSE avt.site_icd10_3char 
            END AS site_icd10_3char, 
            avt.ctry_code, 
            avt.statusofregistration,
            CASE 
                WHEN tumour_type_2 = 'Melanoma' THEN 'NON-KC_MELANOMA' 
                WHEN tumour_type_2 = 'Rare' THEN 'NON-KC_RARE'    
                WHEN tumour_type_1 = 'Extramammary paget disease' THEN 'NON-KC_EMPD'  
                WHEN tumour_type_1 = 'Melanoma in situ' THEN 'NON-KC_MELANOMA_INSITU'  
            END AS tumour_code
        FROM avyyyy.at_tumour_skin_england@casref02 ats
        LEFT JOIN avyyyy.at_tumour_england@casref02 avt 
            ON ats.tumourid = avt.tumourid
        WHERE 
            ats.diagnosisyear between 2013 and yyyy
            AND (ats.tumour_type_2 IN ('Melanoma', 'Rare') 
                OR ats.tumour_type_1 in ('Melanoma in situ', 'Extramammary paget disease'))
            AND avt.ctry_code = 'E'
            AND avt.statusofregistration = 'F'
            AND avt.dedup_flag = '1'
            AND avt.age BETWEEN 0 AND 200 
            AND avt.gender IN (1, 2)

        UNION

        -- Create cohort of keratinoctye skin cancers following the first ever 
        -- registration of BCC and first ever registration of cSCC tumours in 
        -- addition to all genital BCC tumours and all genital cSCC tumours 
        SELECT 
            ats.patientid, 
            ats.tumourid, 
            ats.diagnosisdatebest, 
            ats.diagnosisyear, 
            avt.nhsnumber, 
            avt.figo, 
            avt.gender, 
            avt.ethnicity, 
            avt.morph_icd10_o2, 
            avt.fiveyearageband, 
            avt.age, 
            avt.dedup_flag, 
            CASE 
                WHEN avt.site_icd10 IS NULL THEN avt.site_coded 
                ELSE avt.site_icd10 
            END AS site_icd10,
            CASE 
                WHEN avt.site_icd10_3char IS NULL THEN avt.site_coded_3char 
                ELSE avt.site_icd10_3char 
            END AS site_icd10_3char,
            avt.ctry_code, 
            avt.statusofregistration, 
            CASE 
                WHEN tumour_type_3 = 'BCC' THEN 'KC_BCC'
                WHEN tumour_type_3 = 'cSCC' THEN 'KC_CSCC'
            END AS tumour_code
        FROM avyyyy.at_tumour_skin_england@casref02 ats
        LEFT JOIN avyyyy.at_tumour_england@casref02 avt 
            ON ats.tumourid = avt.tumourid
        WHERE 
            ats.diagnosisyear between 2013 and yyyy
            AND (ats.tumour_type_4 IN ('Genital BCC', 'Genital cSCC')
                OR ats.tumour_type_5 IN ('First BCC', 'First cSCC'))
            AND avt.ctry_code = 'E'
            AND avt.statusofregistration = 'F'
            AND avt.dedup_flag = '1'
            AND avt.age BETWEEN 0 AND 200 
            AND avt.gender IN (1, 2)),

    -- Create tumour cohort for all other (non skin) tumours 
    non_skin AS (
        SELECT 
            AVT.tumourid, 
            AVT.patientid, 
            AVT.nhsnumber, 
            AVT.diagnosisdatebest, 
            CASE 
                WHEN avt.site_icd10 IS NULL THEN avt.site_coded 
                ELSE avt.site_icd10 
            END AS site_icd10,
            AVT.figo, 
            AVT.gender, 
            AVT.ethnicity, 
            AVT.morph_icd10_o2, 
            AVT.fiveyearageband, 
            AVT.age,
            ATSE.NDRS_MAIN,
            ATSE.NDRS_DETAILED,
            -- Create amended tumour_code variable to differentiate between 
            -- haem subsites.  
            CASE -- 'lookupnames'    -- could use NDRS_MAIN_CODE ? 
                 WHEN NDRS_DETAILED IN ('Ampulla of Vater') THEN NDRS_DETAILED
                 WHEN NDRS_MAIN IN ('Anus') THEN NDRS_MAIN
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Urothelial' AND sblad.INVASIVENESS = 'Ta/Tis' 
                                                                          AND NOT (substr(upper(avt.site_icd10),1,1) = 'C' 
                                                                          AND avt.stage_best = 'X' AND avt.t_best = '0') 
                                                                          THEN 'Malignant bladder non invasive Ta/TIS'
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Urothelial' AND sblad.INVASIVENESS = 'T1' 
                                                                          THEN 'Malignant bladder non invasive T1'
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Urothelial' AND sblad.MUSC_INV = 'Muscle-invasive' 
                                                                          THEN 'Malignant bladder invasive'
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Urothelial' AND sblad.MUSC_INV = 'Stage unknown' 
                                                                          THEN 'Malignant bladder invasion not known'
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Other' 
                                                                          THEN 'Uncertain/unknown or other bladder'
                 WHEN NDRS_DETAILED IN ('Bladder - uncertain or unknown') 
                                                                          THEN 'Uncertain/unknown or other bladder'
                 WHEN NDRS_DETAILED IN ('Bladder - malignant or in situ') AND mblad.MORPH_GROUP = 'Urothelial' AND sblad.INVASIVENESS = 'Ta/Tis' 
                                                                          AND substr(upper(avt.site_icd10),1,1) = 'C' AND avt.stage_best = 'X' 
                                                                          AND avt.t_best = '0' 
                                                                          THEN 'Malignant bladder invasion not known'
                 WHEN NDRS_DETAILED IN ('Benign endocrine') THEN 'Benign endocrine brain'
                 WHEN NDRS_DETAILED IN ('Malignant brain') THEN NDRS_DETAILED
                 WHEN NDRS_DETAILED IN ('Non-benign endocrine') THEN  'Non-benign endocrine brain'            
                 WHEN NDRS_DETAILED IN ('Non-malignant brain') THEN NDRS_DETAILED
                 WHEN NDRS_MAIN IN ('Breast') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Cervix') THEN NDRS_MAIN
                 WHEN NDRS_DETAILED IN ('Cholangiocarcinoma') THEN NDRS_DETAILED  
                 WHEN NDRS_DETAILED IN ('Colon') OR NDRS_DETAILED IN ('Rectosigmoid junction') THEN ('Colon') -- definition is different to ATSE for now - to be reviewed later in 2025, after May publication    
                 WHEN NDRS_MAIN IN ('Eye') THEN NDRS_DETAILED
                 WHEN NDRS_DETAILED IN ('Gallbladder') THEN NDRS_DETAILED
                 WHEN NDRS_MAIN IN ('Heart, mediastinum, pleura, other and ill-defined') THEN NDRS_MAIN
                 WHEN NDRS_DETAILED IN ('Hypopharynx') THEN NDRS_DETAILED -- awaiting clinical confirmation 
                 WHEN NDRS_DETAILED IN ('Larynx') THEN NDRS_DETAILED -- awaiting clinical confirmation 
                 WHEN NDRS_DETAILED IN ('Liver excluding intrahepatic cholangiocarcinoma') THEN NDRS_DETAILED    
                 WHEN NDRS_DETAILED IN ('Major salivary glands') THEN NDRS_DETAILED -- awaiting clinical confirmation 
                 WHEN NDRS_MAIN IN ('Mesothelioma') THEN NDRS_MAIN
                 WHEN NDRS_DETAILED IN ('Middle ear, and other, and ill-defined head and neck sites', 'Nasal cavity', 'Nasopharynx') THEN ('Other head and neck') -- awaiting clinical confirmation 
                 WHEN NDRS_DETAILED IN ('Non-small cell lung cancer') THEN NDRS_DETAILED
                 WHEN NDRS_DETAILED IN ('Oral cavity') THEN NDRS_DETAILED -- awaiting clinical confirmation 
                 WHEN NDRS_DETAILED IN ('Oropharynx') and site_icd10_3char = 'C09' THEN 'Oropharynx_C09' -- awaiting clinical confirmation 
                 WHEN NDRS_DETAILED IN ('Oropharynx') THEN NDRS_DETAILED -- awaiting clinical confirmation 
                 WHEN AVT.site_icd10_3char IN ('C64', 'C68') THEN 'Kidney' -- in future to be Malignant kidney, Renal pelvis and ureter, Urethra
                 WHEN AVT.site_icd10_3char IN ('C65', 'C66') THEN 'Kidney_C65_C66' -- in future to be Malignant kidney, Renal pelvis and ureter, Urethra
                 WHEN NDRS_MAIN IN ('Oesophagus') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Ovary') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Pancreas') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Prostate') THEN NDRS_MAIN
                 WHEN NDRS_DETAILED IN ('Rectum') THEN NDRS_DETAILED                      
                 WHEN NDRS_DETAILED IN ('Small cell lung cancer') THEN NDRS_DETAILED                
                 WHEN NDRS_MAIN IN ('Stomach') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Testes') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Thymus') THEN NDRS_MAIN
                 WHEN NDRS_MAIN IN ('Uterus') THEN NDRS_MAIN  -- 'Womb (Uterus)'        
                -- haem subsites
                WHEN haem.split_2 = 'Acute lymphoblastic leukaemia (ALL)' THEN haem.split_2
                WHEN haem.split_2 = 'Acute myeloid leukaemia (AML)' THEN haem.split_2
                WHEN haem.split_2 = 'Chronic myelomonocytic leukaemia (CMML)' THEN haem.split_2
                WHEN haem.split_2 = 'Hodgkin lymphoma' THEN haem.split_2
                WHEN haem.split_2 = 'Lymphoproliferative disorders and non-specific' THEN haem.split_2
                WHEN haem.split_2 = 'Mature T-cell and NK-cell neoplasms' THEN haem.split_2
                WHEN haem.split_2 = 'Plasma cell neoplasms' 
                            and haem.split_3 NOT IN ('Myeloma') 
                            THEN 'Other plasma cell neoplasms'
                WHEN haem.split_2 = 'Mature B-cell neoplasms' 
                            and haem.split_3 NOT IN ('Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)'
                            ,'Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms'
                            ,'Follicular lymphoma'
                            ,'Mantle cell lymphoma (MCL)') 
                            THEN 'Other mature B-cell neoplasms'
                WHEN haem19.split_2 = 'Mature B-cell neoplasms' 
                            and haem19.split_3 NOT IN ('Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)'
                            ,'Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms'
                            ,'Follicular lymphoma'
                            ,'Mantle cell lymphoma (MCL)') 
                            THEN 'Other mature B-cell neoplasms'
                WHEN haem.split_2 = 'Myeloproliferative neoplasms (MPN)' 
                            and haem.split_3 NOT IN ('Chronic myeloid leukaemia (CML)') 
                            THEN 'Other myeloproliferative neoplasms'
                WHEN haem.split_2 = 'Acute promyelocytic leukaemia (APL)' then haem.split_2 --combine into 'Other myeloid' for final treatment table
                WHEN haem.split_2 = 'Mastocytosis' then haem.split_2 --combine into 'Other myeloid' for final treatment table
                WHEN haem.split_2 = 'Other myeloid neoplasms' then haem.split_2 --combine into 'Other myeloid' for final treatment table
                when haem.split_2 = 'Histiocytic and dendritic cell neoplasms' then haem.split_2 --combine into 'Other haematological neoplasms' for final treatment table
                when haem19.split_2 = 'Histiocytic and dendritic cell neoplasms' then haem19.split_2 --combine into 'Other haematological neoplasms' for final treatment table
                when haem.split_2 = 'Langerhans cell histiocytosis (LCH)' then haem.split_2 --combine into 'Other haematological neoplasms' for final treatment table
                when haem.split_2 = 'Neoplasms and leukaemias of ambiguous lineage' then haem.split_2 --combine into 'Other haematological neoplasms' for final treatment table
                WHEN haem.split_3 = 'Chronic myeloid leukaemia (CML)' THEN haem.split_3                
                WHEN haem.split_3 = 'Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)' THEN haem.split_3
                WHEN haem.split_3 = 'Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms' THEN haem.split_3
                WHEN haem.split_3 = 'Follicular lymphoma' THEN haem.split_3
                WHEN haem.split_3 = 'Mantle cell lymphoma (MCL)' THEN haem.split_3
                WHEN haem.split_2 = 'Myelodysplastic syndromes (MDS)' THEN haem.split_2
                WHEN haem.split_3 = 'Myeloma' THEN haem.split_3
                WHEN atse.ndrs_main = 'Haematological neoplasms' and haem.split_2 is null THEN 'Neoplasms and leukaemias of ambiguous lineage' --combine into 'Other haematological neoplasms' for final treatment table

                ELSE AVT.site_icd10_3char
            END AS tumour_code
        FROM avyyyy.at_tumour_england@casref02 AVT 
        LEFT JOIN gdo.morph_haem haem
        -- join the lookup on morph/behaviour in ICD-O-3 Rev 2011
        -- These fields only complete for 2013 onwards
            ON haem.morph_icdo3rev2011 = AVT.morph_icdo3rev2011
            AND haem.behaviour = AVT.behaviour_icdo3rev2011
        LEFT JOIN gdo.morph_haem_2019 haem19
            ON haem19.morph_coded = AVT.morph_coded
            AND haem19.behaviour_coded = AVT.behaviour_coded
        INNER JOIN avyyyy.at_site_england@casref02 ATSE
            ON AVT.tumourid = ATSE.tumourid
        LEFT JOIN avyyyy.at_tumour_experimental_england@casref02 ATTEX 
            ON AVT.tumourid = ATTEX.tumourid  
        LEFT JOIN GDO.MORPH_BLADDER mblad
            ON AVT.morph_icdo3rev2011 = mblad.morph_icdo3rev2011
        LEFT JOIN GDO.STAGE_BLADDER sblad
            ON CASE WHEN (substr(lower(nvl(attex.stage_best_2206,'x')),1,1) IN ('?','u','x','6') or ATTEX.stage_pi_detail_2206 in ('NA','U')) 
                    THEN 'x' else substr(lower(ATTEX.stage_best_2206),1,1) end = sblad.stage_best
            AND substr(upper(AVT.site_icd10),1,1) = sblad.site_icd10_o2_1char
            AND CASE WHEN ATTEX.stage_pi_detail_2206 in ('NA','U') then 'x' else substr(lower(nvl(AVT.T_BEST,'x')),1,1) end = sblad.t_best
        -- Define cohort of interest here
        WHERE 
            avt.diagnosisyear between 2013 and yyyy
            
            -- Restrict to C and D codes except for haem (E85, L99, and null site_icd10)
            AND ((substr(avt.site_icd10_3char, 1, 1) IN ('C', 'D') 
                  AND avt.site_icd10_3char NOT IN ('C44')
                  AND ((gender = '2' and site_icd10_3char not in ('C60', 'C61', 'C62', 'C63')) 
                    OR (gender = '1' and  site_icd10_3char not in ('C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58'))) -- gender doesn't agree with tumour site 
                  )
                  
            OR atse.ndrs_main = 'Haematological neoplasms'
                -- Removing haem cases pre 2019 that are coded in ICD-O3 rev 2019 and cases pre 2016 that are coded in ICD-O3 rev 2016
                -- Temp fix until they are removed from the site table group
                AND NOT (AVT.diagnosisyear <= 2018 and haem19.morph_coded is not null and coding_system_desc = 'ICD-O-3 (2019)')
                AND NOT (AVT.diagnosisyear <= 2015 and haem19.morph_coded is not null and coding_system_desc = 'ICD-O-3 (2016)')
            )
          
            AND avt.ctry_code = 'E'
            AND avt.statusofregistration = 'F'
            AND avt.dedup_flag = '1'
            AND avt.age BETWEEN 0 AND 200 
            AND avt.gender IN (1, 2)),

    -- Remove any tumours from the all tumours cohort that also appear in the skin cohort to avoid duplication
    non_skin_cohort AS (
        SELECT 
            nsk.tumourid, 
            nsk.patientid, 
            nsk.nhsnumber, 
            nsk.diagnosisdatebest, 
            nsk.site_icd10, 
            nsk.figo, 
            nsk.gender, 
            nsk.ethnicity, 
            nsk.morph_icd10_o2, 
            nsk.fiveyearageband, 
            nsk.age, 
            nsk.tumour_code
        FROM non_skin nsk
        LEFT JOIN skin_cohort skn 
            ON nsk.tumourid = skn.tumourid
        WHERE 
            skn.tumourid IS NULL),

    -- Now union together the skin and non-skin cancer cohorts to create the full cohort 
    tumour_cohort AS (
        SELECT 
            tumourid, 
            patientid, 
            nhsnumber, 
            diagnosisdatebest, 
            site_icd10, 
            figo, 
            gender, 
            ethnicity, 
            morph_icd10_o2, 
            fiveyearageband, 
            age, 
            tumour_code
        FROM skin_cohort

        UNION

        SELECT 
            tumourid, 
            patientid, 
            nhsnumber, 
            diagnosisdatebest, 
            site_icd10, 
            figo, 
            gender, 
            ethnicity, 
            morph_icd10_o2, 
            fiveyearageband, 
            age, 
            tumour_code
        FROM non_skin_cohort)

    --  Identify patients with multiple tumours within an 18-month period with tumour_flag
    SELECT 
        tumourid, 
        patientid, 
        nhsnumber, 
        diagnosisdatebest, 
        site_icd10, 
        figo, 
        gender, 
        ethnicity, 
        morph_icd10_o2, 
        fiveyearageband, 
        age, 
        tumour_code, 
        tumour_flag
    FROM (SELECT 
            avt.tumourid, 
            avt.patientid, 
            avt.nhsnumber, 
            avt.diagnosisdatebest, 
            avt.site_icd10, 
            avt.figo, 
            avt.gender, 
            avt.ethnicity, 
            avt.morph_icd10_o2, 
            avt.fiveyearageband, 
            avt.age, 
            avt.tumour_code,
            -- This join flags any tumours diagnosed in 2013-21 that belong to a 
            -- patient who had another tumour in the 18 months before or after that diagnosis 
            -- (so that later, patient level datasets (hes, sact, rtds) are only used for patients with 1 tumour) 
            -- Tumour_flag = 1; the tumour belonged to a patient who had another tumour within 18 months 
            CASE 
                WHEN ABS(avt.diagnosisdatebest-avt2.diagnosisdatebest) < 548 THEN 1 
                ELSE 0 
            END AS tumour_flag, 
            -- In the process of joining AVT2 to AVT to identify multiple tumours, duplicate rows are generated 
            -- The difference between diagnosis date for tumours in AVT AND AVT2 ranks multiple tumours where more than one exists AND drops all but the closest tumour to the original tumour. 
            -- Where rk = 1; this is the tumour record to keep 
            RANK() OVER (
                PARTITION BY avt.tumourid 
                ORDER BY ABS(avt.diagnosisdatebest-avt2.diagnosisdatebest) ASC, avt2.tumourid) AS rk 
        FROM tumour_cohort AVT
        -- Multiple tumours join: 
        -- For tumours diagnosed from 2013-yyyy, identify any other tumour IDs that occurred between 2011-2023 
        -- A second copy of the tumour cohort (AVT2) is joined to the original tumour cohort of 2013-yy diagnoses (TC) 
        -- Records from AVT2 are only joined if the patient ID is the same but the tumour ID is different 
        LEFT JOIN avyyyy.at_tumour_england@casref02 AVT2 
            ON avt.patientid = avt2.patientid 
            AND NOT(avt.tumourid = avt2.tumourid)   
            --AND avt2.cascade_inci_flag = 1 
            AND avt2.site_icd10_3char NOT IN ('C44') 
            AND avt2.diagnosisyear BETWEEN 2011 AND (yyyy + 2)
            )
        -- Removes duplicate tumour rows that had been added to identify patients with multiple tumours
    WHERE rk = 1;

