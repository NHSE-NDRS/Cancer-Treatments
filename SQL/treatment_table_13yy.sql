------------------------------------------------------------------------------ 
----------- Create final table drawing on all previous tables------------------ 
------------------------------------------------------------------------------ 
CREATE TABLE treatment_table_13yy COMPRESS AS 
    WITH pre_treat_table AS (
    SELECT 
        -- Create radiotherapy (RT) flag for the tumour 
        -- Only use the patient level datasets (rtds, rtds2) if the patient had 
        -- no other tumours recorded in the 18 months before or after this tumour diagnosis 
        CASE 
            WHEN avrt_flag = 1 THEN 1 
            WHEN rtds_flag = 1 AND tc.tumour_flag = 0 THEN 1 
            WHEN rtds2_flag = 1 AND tc.tumour_flag = 0 THEN 1 
            ELSE 0 
        END AS rt_flag,
        -- Create chemo (CT) flag for the tumour 
        -- Only use the patient level datasets (sact, sact2) if the patient had 
        -- no other tumours recorded in the 18 months before or after this tumour diagnosis 
        CASE 
            WHEN avct_flag = 1 THEN 1 
            WHEN sact_flag = 1 AND tc.tumour_flag = 0 THEN 1 
            WHEN sact2_flag = 1 AND tc.tumour_flag = 0 THEN 1
            WHEN hesct_flag = 1 AND tc.tumour_flag = 0 THEN 1
            ELSE 0 
        END AS ct_flag, 
        -- Create resection flag for the tumour 
        -- Only use the patient level datasets (hes) if the patient had no other 
        -- tumours recorded in the 18 months before or after this tumour diagnosis 
        CASE 
            -- Firstly, incorporate non-stage specific resection flag using opcs4 resection lookup table 
            WHEN AVSG_flag = 1 THEN 1 
            WHEN hessg_flag = 1 AND tc.tumour_flag = 0 THEN 1 
            -- Secondly, incorporate stage specific rules for particular cancer sites 
            -- Cervical 
            WHEN tumour_code = 'Cervix' 
                AND (upper(SUBSTR(tc.figo, 1, 2))) IN ('1A', 'IA') 
                AND conebiops_avtreat = 1 THEN 1 
            WHEN tumour_code = 'Cervix' 
                AND (upper(SUBSTR(tc.figo, 1, 2))) IN ('1A', 'IA') 
                AND conebiops_hes = 1 AND tc.tumour_flag = 0 THEN 1 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) AND (lymph_avtreat = 1) THEN 1 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) AND (lymph_hes = 1 AND tc.tumour_flag = 0) THEN 1 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_avtreat = 1) THEN 1 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_hes = 1 AND tc.tumour_flag = 0) THEN 1 
            -- colorectal: 
            WHEN tumour_code IN ('Colon', 'Rectum')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND colorec_avtreat = 1 THEN 1 
            WHEN tumour_code IN ('Colon', 'Rectum')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND colorec_hes = 1 
                AND tc.tumour_flag = 0 THEN 1 
            -- Sub rule for appendectomies for colorectal: 
            WHEN avt.site_icd10 IN ('C181') 
                AND colorec_avtreat_appen = 1 THEN 1 
            WHEN avt.site_icd10 IN ('C181') 
                AND colorec_hes_appen = 1 
                AND tc.tumour_flag = 0 THEN 1 
            -- bladder 
            WHEN tumour_code IN  ('Malignant bladder non invasive T1')
                AND bladder_avtreat = 1 THEN 1 
            WHEN tumour_code IN  ('Malignant bladder non invasive T1')
                AND bladder_hes = 1 
                AND tc.tumour_flag = 0 THEN 1
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_avtreat_Dcodes = 1 THEN 1 
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_hes_Dcodes = 1 
                AND tc.tumour_flag=0 THEN 1
            -- liver 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND liver_avtreat = 1 THEN 1 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND liver_hes = 1 
                AND tc.tumour_flag = 0 THEN 1 
            -- oesophagus 
            WHEN tumour_code IN ('Oesophagus') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND oesoph_avtreat = 1 THEN 1 
            WHEN tumour_code IN ('Oesophagus') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND oesoph_hes = 1 
                AND tc.tumour_flag = 0 THEN 1 
            -- stomach 
            WHEN tumour_code IN ('Stomach') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND stomach_avtreat = 1 THEN 1 
            WHEN tumour_code IN ('Stomach') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND stomach_hes = 1 
                AND tc.tumour_flag = 0 THEN 1 
            ELSE 0 
        END AS sg_flag, 
        -- Create cancer site names 
        CASE 
            WHEN tumour_code IN ('Ampulla of Vater') THEN tumour_code
            WHEN tumour_code IN ('Anus') THEN tumour_code
            WHEN tumour_code IN ('Malignant bladder invasion not known') THEN ('Bladder:Unknown muscle-invasion urothelial')
            WHEN tumour_code IN ('Malignant bladder invasive') THEN ('Bladder:Muscle-invasive urothelial')
            WHEN tumour_code IN ('Malignant bladder non invasive T1') THEN ('Bladder:T1 non-muscle-invasive urothelial')
            WHEN tumour_code IN ('Malignant bladder non invasive Ta/TIS') THEN ('Bladder:Ta/Tis non-muscle-invasive urothelial')
            WHEN tumour_code IN ('Uncertain/unknown or other bladder') THEN ('Bladder:Other morphology or uncertain/unknown behaviour')
            WHEN tumour_code IN ('Benign endocrine brain') THEN 'Brain:Benign endocrine'
            WHEN tumour_code IN ('Malignant brain') THEN 'Brain:Malignant'
            WHEN tumour_code IN ('Non-benign endocrine brain') THEN 'Brain:Non-benign endocrine'         
            WHEN tumour_code IN ('Non-malignant brain') THEN  'Brain:Non-malignant brain'
            WHEN tumour_code IN ('Breast') THEN tumour_code
            WHEN tumour_code IN ('Cervix') THEN tumour_code
            WHEN tumour_code IN ('Cholangiocarcinoma') THEN tumour_code  
            WHEN tumour_code IN ('Colon') THEN tumour_code 
            WHEN tumour_code IN ('Eye') THEN tumour_code
            WHEN tumour_code IN ('Gallbladder') THEN tumour_code
            WHEN tumour_code IN ('Heart, mediastinum, pleura, other and ill-defined') THEN ('Heart, mediastinum, pleura, other and ill-defined')
            WHEN tumour_code IN ('Hypopharynx') THEN tumour_code 
            WHEN tumour_code IN ('Kidney', 'Kidney_C65_C66') THEN 'Kidney'
            WHEN tumour_code IN ('Larynx') THEN tumour_code
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma') THEN tumour_code   
            WHEN tumour_code IN ('Major salivary glands') THEN tumour_code
            WHEN tumour_code IN ('Mesothelioma') THEN tumour_code
            WHEN tumour_code IN ('Other head and neck') THEN tumour_code
             WHEN tumour_code IN ('Non-small cell lung cancer') THEN tumour_code
            WHEN tumour_code IN ('Oral cavity') THEN tumour_code 
            WHEN tumour_code IN ('Oropharynx', 'Oropharynx_C09') THEN 'Oropharynx'
            WHEN tumour_code IN ('Oesophagus') THEN tumour_code
            WHEN tumour_code IN ('Ovary') THEN tumour_code
            WHEN tumour_code IN ('Pancreas') THEN tumour_code
            WHEN tumour_code IN ('Prostate') THEN tumour_code
            WHEN tumour_code IN ('Rectum') THEN tumour_code                       
            WHEN tumour_code IN ('Small cell lung cancer') THEN tumour_code                
            WHEN tumour_code IN ('Stomach') THEN tumour_code
            WHEN tumour_code IN ('Testes') THEN tumour_code
            WHEN tumour_code IN ('Thymus') THEN tumour_code
            WHEN tumour_code IN ('Uterus') THEN tumour_code  -- 'Womb (Uterus)'

            -- skin
            WHEN tumour_code IN ('NON-KC_MELANOMA') THEN 'Skin:Non-keratinocyte, melanoma'
            WHEN tumour_code IN ('NON-KC_MELANOMA_INSITU') THEN 'Skin:Non-keratinocyte, melanoma in situ'
            WHEN tumour_code IN ('NON-KC_RARE','NON-KC_EMPD') THEN 'Skin:Non-keratinocyte, rare' 
            WHEN tumour_code IN ('KC_BCC') THEN 'Skin:Keratinocyte skin, BCC' 
            WHEN tumour_code IN ('KC_CSCC') THEN 'Skin:Keratinocyte, CSCC' 

            -- haem
            WHEN tumour_code IN ('Acute lymphoblastic leukaemia (ALL)') THEN 'Haem:Acute lymphoblastic leukaemia (ALL)'
            WHEN tumour_code IN ('Chronic myelomonocytic leukaemia (CMML)') THEN 'Haem:Chronic myelomonocytic leukaemia (CMML)'
            WHEN tumour_code IN ('Acute myeloid leukaemia (AML)') THEN 'Haem:Acute myeloid leukaemia (AML)'
            WHEN tumour_code IN ('Hodgkin lymphoma') THEN 'Haem:Hodgkin lymphoma'
            WHEN tumour_code IN ('Lymphoproliferative disorders and non-specific') THEN 'Haem:Lymphoproliferative disorders and non-specific'
            WHEN tumour_code IN ('Mature T-cell and NK-cell neoplasms') THEN 'Haem:Mature T-cell and NK-cell neoplasms'
            WHEN tumour_code IN ('Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)') THEN 'Haem:Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)'
            WHEN tumour_code IN ('Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms') THEN 'Haem:Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms'
            WHEN tumour_code IN ('Follicular lymphoma') THEN 'Haem:Follicular lymphoma'
            WHEN tumour_code IN ('Mantle cell lymphoma (MCL)') THEN 'Haem:Mantle cell lymphoma (MCL)'
            WHEN tumour_code IN ('Chronic myeloid leukaemia (CML)') THEN 'Haem:Chronic myeloid leukaemia (CML)'
            WHEN tumour_code IN ('Myeloma') THEN 'Haem:Myeloma'
            WHEN tumour_code IN ('Myelodysplastic syndromes (MDS)') THEN 'Haem:Myelodysplastic syndromes (MDS)'
            WHEN tumour_code IN ('Other plasma cell neoplasms') THEN 'Haem:Other plasma cell neoplasms'
            WHEN tumour_code IN ('Other mature B-cell neoplasms') THEN 'Haem:Other mature B-cell neoplasms'
            WHEN tumour_code IN ('Other myeloproliferative neoplasms') THEN 'Haem:Other myeloproliferative neoplasms'
            WHEN tumour_code IN ('Acute promyelocytic leukaemia (APL)', 'Mastocytosis', 'Other myeloid neoplasms') THEN 'Haem:Other myeloid'
            WHEN tumour_code IN ('Histiocytic and dendritic cell neoplasms', 'Langerhans cell histiocytosis (LCH)', 'Neoplasms and leukaemias of ambiguous lineage') THEN 'Haem:Other haematological neoplasms'
            
            WHEN SUBSTR(tumour_code, 1, 1) = 'D' THEN 'Other non-malignant'
            
            ELSE 'Other malignant' 
            
        END AS cancergroup, 
        -- Select all other variables 
        avt.tumourid,
        avt.diagnosisyear,
        avt.age,
        avt.gender as gender,
        avt.dco,
        avt.basisofdiagnosis,
        avt.fiveyearageband,
        avt.ethnicity,
        chrl.chrl_tot_27_03,
        CASE
            WHEN (diagnosisyear = 2013) then imd.imd2015_quintiles
            WHEN (diagnosisyear IN IMD_years_var) then imd.imd2019_quintiles
        END AS imd_quintile_lsoas,
        atg.icb_20yy_name,
        atg.icb_20yy_code,
        atg.canalliance_20yy_name,
        atg.canalliance_20yy_code,
        -- For checking 
        avt.morph_icd10_o2,
        tc.figo,
        avt.t_best,
        avt.stage_best,
        tc.site_icd10,
        CASE 
            WHEN avt.site_icd10_3char IS NULL THEN avt.site_coded_3char 
            ELSE avt.site_icd10_3char 
        END AS site_icd10_3char,
        tc.tumour_flag,
        -- Select dates of treatment from at_treatment_england 
        avt.diagnosisdatebest,
        avt.deathdatebest,
        avct.avct_date,
        avrt.avrt_date,
        avsg.avsg_date, 
        -- Select dates of treatment from patient-level datasets where only 1 tumour was diagnosed in 18 months before or after that tumour 
        CASE 
            WHEN tc.tumour_flag = 0 THEN sact.sact_date 
        END AS sact_date,
        CASE 
            WHEN tc.tumour_flag = 0 THEN sact2.sact2_date 
        END AS sact2_date,
        CASE 
            WHEN tc.tumour_flag = 0 THEN hesct.hesct_date 
        END AS hesct_date,
        CASE 
            WHEN tc.tumour_flag = 0 THEN rtds.rtds_date 
        END AS rtds_date,
        CASE 
            WHEN tc.tumour_flag = 0 THEN hessg.hessg_date 
        END AS hessg_date,
        CASE 
            WHEN tc.tumour_flag = 0 THEN rtds2.rtds2_date 
        END AS rtds2_date,
        -- Select date of surgery where there were additional site-specific resections flagged: 
        -- CERVICAL
        -- Take date of cone biopsy in at_treatment_england if: 
        -- The tumour received a cone biopsy and was FIGO stage 1a 
        -- Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy
        CASE 
            WHEN tumour_code = 'Cervix' 
                AND (upper(SUBSTR(tc.figo, 1, 2)) IN ('1A', 'IA')) 
                AND conebiops_avtreat = 1 THEN cbavt.avsg_date 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) 
                AND (lymph_avtreat = 1) THEN cbavt.avsg_date 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) 
                AND (lymph_hes = 1 
                AND tc.tumour_flag = 0) THEN cbavt.avsg_date 
        END AS cbavsg_date, 
        -- Take date of cone biopsy in hes if: 
        -- The tumour received a cone biopsy and was FIGO stage 1a 
        -- Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy 
        -- and only 1 tumour was diagnosed in 18 months before or after that tumour 
        CASE 
            WHEN tumour_code = 'Cervix' 
                AND (upper(SUBSTR(tc.figo, 1, 2)) IN ('1A', 'IA')) 
                AND conebiops_hes = 1 
                AND tc.tumour_flag = 0 THEN cbhes.hessg_date 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_avtreat = 1) THEN cbhes.hessg_date 
            WHEN tumour_code = 'Cervix' 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_hes = 1 AND tc.tumour_flag = 0) THEN cbhes.hessg_date 
        END AS cbhessg_date, 
        -- colorectal 
        -- As with cervical, select the date of the stage-specific resection for each tumour, according to the rules specified earlier for generating the stage-specific resection flag for that tumour site 
        CASE 
            WHEN tumour_code IN ('Colon', 'Rectum')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND colorec_avtreat = 1 THEN coloavt.avsg_date 
        END AS coloavsg_date,
        CASE 
            WHEN tumour_code IN ('Colon', 'Rectum')
            AND SUBSTR(avt.stage_best, 1, 1) = '1' 
            AND colorec_hes = 1 
            AND tc.tumour_flag = 0 THEN colohes.hessg_date 
        END AS colohessg_date,
        CASE 
            WHEN avt.site_icd10 IN ('C181') 
            AND colorec_avtreat_appen = 1 THEN coloavt_appen.avsg_date 
        END AS appenavsg_date, 
        CASE 
            WHEN avt.site_icd10 IN ('C181') 
                AND colorec_hes_appen = 1 
                AND tc.tumour_flag = 0 THEN colohes_appen.hessg_date    
        END AS appenhessg_date,
        -- bladder 
        CASE 
            WHEN tumour_code IN ('Malignant bladder non invasive T1')
                AND bladder_avtreat = 1 THEN blad1_avt.avsg_date 
        END AS bladavsg_date, 
        CASE 
            WHEN tumour_code IN ('Malignant bladder non invasive T1')
                AND bladder_hes = 1 
                AND tc.tumour_flag = 0 THEN blad1_hes.hessg_date 
        END AS bladhessg_date,
        CASE 
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_avtreat_Dcodes = 1 THEN blad2_avt.avsg_date 
        END AS blad_insitu_avsg_date, 
        CASE 
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_hes_Dcodes = 1 
                AND tc.tumour_flag = 0 THEN blad2_hes.hessg_date 
        END AS blad_insitu_hessg_date, 
        -- liver
        CASE 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND liver_avtreat = 1 THEN livavt.avsg_date 
        END AS livavsg_date, 
        CASE 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma') 
                AND SUBSTR(avt.stage_best, 1 ,1) = '1' 
                AND liver_hes = 1 
                AND tc.tumour_flag = 0 THEN livhes.hessg_date 
        END AS livhessg_date, 
        -- oesophageal
        CASE 
            WHEN tumour_code IN ('Oesophagus') 
            AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
            AND oesoph_avtreat = 1 THEN oesoavt.avsg_date 
        END AS oesoavsg_date, 
        CASE 
            WHEN tumour_code IN ('Oesophagus') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND oesoph_hes = 1 
                AND tc.tumour_flag = 0 THEN oesohes.hessg_date 
        END AS oesohessg_date, 
        -- stomach
        CASE 
            WHEN tumour_code IN ('Stomach') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND stomach_avtreat = 1 THEN stomavt.avsg_date 
        END AS stomavsg_date, 
        CASE 
            WHEN tumour_code IN ('Stomach')  
            AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
            AND stomach_hes = 1 
            AND tc.tumour_flag = 0 THEN stomhes.hessg_date 
        END AS stomhessg_date, 
        -- Select trust codes from at_treatment_england
        avsg.avsg_trust_code, 
        avct_trust_code, 
        avrt_trust_code,
        -- Select trust codes of treatment from patient-level datasets where only 1 tumour was diagnosed in 18 months before or after that tumour 
        CASE 
            WHEN tc.tumour_flag = 0 THEN hessg.hessg_trust_code 
        END AS hessg_trust_code,
        CASE 
            WHEN tc.tumour_flag = 0 THEN sact.sact_trust_code 
        END AS sact_trust_code,
        CASE 
            WHEN tc.tumour_flag = 0 THEN sact2.sact2_trust_code 
        END AS sact2_trust_code,
        CASE 
            WHEN tc.tumour_flag = 0 THEN hesct.hesct_trust_code 
        END AS hesct_trust_code,
        CASE 
            WHEN tc.tumour_flag = 0 THEN rtds.rtds_trust_code 
        END AS rtds_trust_code,
        CASE 
            WHEN tc.tumour_flag = 0 THEN rtds2.rtds2_trust_code 
        END AS rtds2_trust_code,
        -- Select trust codes of surgery where there were additional site-specific resections flagged: 
        -- CERVICAL
        -- Take trust code of cone biopsy in at_treatment_england if: 
        -- The tumour received a cone biopsy and was FIGO stage 1a 
        -- Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy
        CASE 
            WHEN tumour_code IN ('Cervix')  
                AND (upper(SUBSTR(tc.figo, 1, 2)) IN ('1A', 'IA')) 
                AND conebiops_avtreat = 1 THEN cbavt.avsg_trust_code 
            WHEN tumour_code IN ('Cervix') 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) AND (lymph_avtreat = 1) THEN cbavt.avsg_trust_code 
            WHEN tumour_code IN ('Cervix') 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_avtreat = 1) 
                AND (lymph_hes = 1 AND tc.tumour_flag = 0) THEN cbavt.avsg_trust_code 
        END AS cbavsg_trust_code, 
        -- Take date of cone biopsy in hes if: 
        -- The tumour received a cone biopsy AND was FIGO stage 1a 
        -- Or the tumour received a cone biopsy AND was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy 
        -- AND only 1 tumour was diagnosed in 18 months before or after that tumour 
        CASE 
            WHEN tumour_code IN ('Cervix') 
                AND (upper(SUBSTR(tc.figo, 1, 2)) IN ('1A', 'IA')) 
                AND conebiops_hes = 1 
                AND tc.tumour_flag = 0 THEN cbhes.hessg_trust_code 
            WHEN tumour_code IN ('Cervix') 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_avtreat = 1) THEN cbhes.hessg_trust_code 
            WHEN tumour_code IN ('Cervix') 
                AND (upper(tc.figo) IN ('1B', 'IB') or upper(SUBSTR(tc.figo, 1, 3)) IN ('1B1', 'IB1')) 
                AND (conebiops_hes = 1 AND tc.tumour_flag = 0) 
                AND (lymph_hes = 1 AND tc.tumour_flag = 0) THEN cbhes.hessg_trust_code 
        END AS cbhessg_trust_code,
        -- colorectal 
        -- As with cervical, select the date of the stage-specific resection for each tumour, according to the rules specified earlier for generating the stage-specific resection flag for that tumour site 
        CASE 
            WHEN tumour_code IN ('Colon', 'Rectum')
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND colorec_avtreat = 1 THEN coloavt.avsg_trust_code 
        END AS coloavsg_trust_code,
        CASE 
            WHEN tumour_code IN ('Colon', 'Rectum')
            AND SUBSTR(avt.stage_best, 1, 1) = '1' 
            AND colorec_hes = 1 
            AND tc.tumour_flag = 0 THEN colohes.hessg_trust_code 
        END AS colohessg_trust_code,
        CASE 
            WHEN avt.site_icd10 IN ('C181') 
            AND colorec_avtreat_appen = 1 THEN coloavt_appen.avsg_trust_code 
        END AS appenavsg_trust_code, 
        CASE 
            WHEN avt.site_icd10 IN ('C181') 
                AND colorec_hes_appen = 1 
                AND tc.tumour_flag = 0 THEN colohes_appen.hessg_trust_code 
        END AS appenhessg_trust_code,
        -- bladder
        CASE 
            WHEN tumour_code IN  ('Malignant bladder non invasive T1')
                AND bladder_avtreat = 1 THEN blad1_avt.avsg_trust_code 
        END AS bladavsg_trust_code, 
        CASE 
            WHEN tumour_code IN  ('Malignant bladder non invasive T1')
            AND bladder_hes = 1 
            AND tc.tumour_flag = 0 THEN blad1_hes.hessg_trust_code 
        END AS bladhessg_trust_code,
        CASE 
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_avtreat_Dcodes = 1 THEN blad2_avt.avsg_trust_code 
        END AS blad_insitu_avsg_trust_code, 
        CASE 
            WHEN ((avt.site_icd10 IN ('D090') 
                    AND avt.morph_icd10_o2 = '8130') 
                OR avt.site_icd10 IN ('D414'))
                AND bladder_hes_Dcodes = 1 
                AND tc.tumour_flag = 0 THEN blad2_hes.hessg_trust_code 
        END AS blad_insitu_hessg_trust_code, 
        -- liver
        CASE 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma') 
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND liver_avtreat = 1 THEN livavt.avsg_trust_code 
        END AS livavsg_trust_code, 
        CASE 
            WHEN tumour_code IN ('Liver excluding intrahepatic cholangiocarcinoma') 
                AND SUBSTR(avt.stage_best, 1, 1) = '1' 
                AND liver_hes = 1 
                AND tc.tumour_flag = 0 THEN livhes.hessg_trust_code 
        END AS livhessg_trust_code, 
        -- oesophageal
        CASE 
            WHEN tumour_code IN ('Oesophagus') 
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND oesoph_avtreat = 1 THEN oesoavt.avsg_trust_code 
        END AS oesoavsg_trust_code, 
        CASE 
            WHEN tumour_code IN ('Oesophagus') 
            AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
            AND oesoph_hes = 1 
            AND tc.tumour_flag = 0 THEN oesohes.hessg_trust_code 
        END AS oesohessg_trust_code, 
        -- stomach
        CASE 
            WHEN tumour_code IN ('Stomach')  
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND stomach_avtreat = 1 THEN stomavt.avsg_trust_code 
        END AS stomavsg_trust_code, 
        CASE 
            WHEN tumour_code IN ('Stomach')  
                AND SUBSTR(avt.stage_best, 1, 2) = '1A' 
                AND stomach_hes = 1 
                AND tc.tumour_flag = 0 THEN stomhes.hessg_trust_code 
        END AS stomhessg_trust_code  
    -- final join of tables with flags 
    -- Do not flag surgery for non-ovarian C48 tumour morphologies (these are classified as "other" tumours) 
    FROM avyyyy.at_tumour_england@casref02 AVT 
    INNER JOIN tr_tumour_cohort@casref02 tc 
        ON avt. tumourid = tc. tumourid
    LEFT JOIN tr_av_ct@casref02 avct 
        ON avt.tumourid = avct.tumourid 
    LEFT JOIN tr_sact@casref02 sact 
        ON avt.tumourid = sact.tumourid 
    LEFT JOIN tr_sact_2@casref02 sact2 
        ON avt.tumourid = sact2.tumourid
    LEFT JOIN tr_hes_ct@casref02 hesct 
        ON avt.tumourid = hesct.tumourid
    LEFT JOIN tr_av_rt@casref02 avrt 
        ON avt.tumourid = avrt.tumourid 
    LEFT JOIN tr_av_sg@casref02 avsg 
        ON avt.tumourid = avsg.tumourid 
        AND (tc.tumour_code NOT IN ('C48OTHER')) 
    LEFT JOIN tr_rtds@casref02 rtds 
        ON avt.tumourid = rtds.tumourid 
    LEFT JOIN tr_hes_sg@casref02 hessg 
        ON avt.tumourid = hessg.tumourid 
        AND (tc.tumour_code NOT IN ('C48OTHER')) 
    LEFT JOIN tr_rtds_2@casref02 rtds2 
        ON avt.tumourid = rtds2.tumourid 
    -- Add further joins for stage-specific resections:
    -- CERVICAL
    LEFT JOIN tr_av_conebiops@casref02 CBAVT 
        ON avt.tumourid = cbavt.tumourid 
    LEFT JOIN tr_hes_conebiops@casref02 CBhes 
        ON avt.tumourid = cbhes.tumourid 
    LEFT JOIN tr_av_lymph@casref02 lyavt 
        ON avt.tumourid = lyavt.tumourid 
    LEFT JOIN tr_hes_lymph@casref02 lyhes 
        ON avt.tumourid = lyhes.tumourid 
    -- Colorectal
    LEFT JOIN tr_av_colorec@casref02 coloavt 
        ON avt.tumourid = coloavt.tumourid 
    LEFT JOIN tr_hes_colorec@casref02 colohes 
        ON avt.tumourid = colohes.tumourid 
    LEFT JOIN tr_av_coloappen@casref02 coloavt_appen 
        ON avt.tumourid = coloavt_appen.tumourid 
    LEFT JOIN tr_hes_coloappen@casref02 colohes_appen 
        ON avt.tumourid = colohes_appen.tumourid 
    -- Bladder
    LEFT JOIN tr_av_bladder@casref02 blad1_avt 
        ON avt.tumourid = blad1_avt.tumourid 
    LEFT JOIN tr_hes_bladder@casref02 blad1_hes 
        ON avt.tumourid = blad1_hes.tumourid 
    LEFT JOIN tr_av_bladder_Dcodes@casref02 blad2_avt 
        ON avt.tumourid = blad2_avt.tumourid 
    LEFT JOIN tr_hes_bladder_Dcodes@casref02 blad2_hes 
        ON avt.tumourid = blad2_hes.tumourid 
    -- Liver
    LEFT JOIN tr_av_liver@casref02 livavt 
        ON avt.tumourid = livavt.tumourid 
    LEFT JOIN tr_hes_liver@casref02 livhes 
        ON avt.tumourid = livhes.tumourid 
    -- Oesophageal
    LEFT JOIN tr_av_oesoph@casref02 oesoavt 
        ON avt.tumourid = oesoavt.tumourid 
    LEFT JOIN tr_hes_oesoph@casref02 oesohes 
        ON avt.tumourid = oesohes.tumourid 
    -- Stomach
    LEFT JOIN tr_av_stomach@casref02 stomavt 
        ON avt.tumourid = stomavt.tumourid 
    LEFT JOIN tr_hes_stomach@casref02 stomhes 
        ON avt.tumourid = stomhes.tumourid 
    -- Additional demographics 
    LEFT JOIN avyyyy.at_geography_england atg 
        ON avt.tumourid = atg.tumourid
    LEFT JOIN IMD_table_hold imd 
      ON atg.LSOAyy_code = imd.LSOAyycd 
    LEFT JOIN charlson_table_hold@casref02 chrl 
        ON chrl.tumourid = avt.tumourid), 
        
    summary_fields as (
        SELECT 
            tumourid,
            -- Take first non-null date from avct, sact, sact2, hesct
            CASE 
                WHEN CT_FLAG = 1 THEN (COALESCE(avct_date, sact_date, sact2_date, hesct_date)) 
            END AS summary_ct_date,
            -- Take first non-null date from avrt, rtds, rtds2
            CASE 
                WHEN RT_FLAG = 1 THEN (COALESCE(avrt_date, rtds_date, rtds2_date)) 
            END AS summary_rt_date,
            -- Take first non-null date from Avsg, hessg, other site / stage specific stage fields
            CASE 
                WHEN SG_FLAG = 1 THEN (COALESCE(cbavsg_date, cbhessg_date, coloavsg_date, 
                                                     colohessg_date, appenavsg_date, appenhessg_date, 
                                                     bladavsg_date, bladhessg_date, blad_insitu_avsg_date, 
                                                     blad_insitu_hessg_date, livavsg_date, livhessg_date, 
                                                     oesoavsg_date, oesohessg_date, stomavsg_date, 
                                                     stomhessg_date, avsg_date, hessg_date)) 
            END AS summary_sg_date,
            -- Take name corresponding to first non-null date from avct, sact, sact2, hesct
            CASE 
                WHEN (CT_FLAG = 1 and instr(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code), '-X') > 0) 
                    THEN substr(upper(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code)), 1, 3)
                WHEN CT_FLAG = 1 THEN upper(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code)) 
            END AS summary_ct_trustcode,
            -- Take name corresponding to first non-null date from avrt, rtds, rtds2
            CASE 
                WHEN (RT_FLAG = 1 AND instr(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code), '-X') > 0) 
                    THEN substr(upper(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code)), 1, 3)
                WHEN RT_FLAG = 1 THEN upper(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code)) 
            END AS summary_rt_trustcode,
            -- Take name corresponding to first non-null date from Avsg, hessg, other site / stage specific stage fields
            CASE 
                WHEN (SG_FLAG = 1 and instr(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, 
                                                     colohessg_trust_code, appenavsg_trust_code, appenhessg_trust_code, 
                                                     bladavsg_trust_code, bladhessg_trust_code, blad_insitu_avsg_trust_code, 
                                                     blad_insitu_hessg_trust_code, livavsg_trust_code, livhessg_trust_code, 
                                                     oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, 
                                                     stomhessg_trust_code, avsg_trust_code, hessg_trust_code),'-X') > 0) 
                    THEN substr(upper(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, colohessg_trust_code, 
                                               appenavsg_trust_code, appenhessg_trust_code, bladavsg_trust_code, bladhessg_trust_code, 
                                               blad_insitu_avsg_trust_code, blad_insitu_hessg_trust_code, livavsg_trust_code, 
                                               livhessg_trust_code, oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, 
                                               stomhessg_trust_code, avsg_trust_code, hessg_trust_code)), 1, 3) 
                WHEN SG_FLAG = 1 THEN upper(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, colohessg_trust_code, 
                                                     appenavsg_trust_code, appenhessg_trust_code, bladavsg_trust_code, bladhessg_trust_code, 
                                                     blad_insitu_avsg_trust_code, blad_insitu_hessg_trust_code, livavsg_trust_code, 
                                                     livhessg_trust_code, oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, 
                                                     stomhessg_trust_code, avsg_trust_code, hessg_trust_code)) end as summary_sg_trustcode
    FROM pre_treat_table)

    SELECT 
        ptr.TUMOURID,
        ptr.RT_FLAG,
        ptr.CT_FLAG,
        ptr.SG_FLAG,
        ptr.TUMOUR_FLAG,
        ptr.CANCERGROUP,
        ptr.DIAGNOSISYEAR,
        ptr.AGE,
        ptr.FIVEYEARAGEBAND,
        ptr.GENDER,
        ptr.DIAGNOSISDATEBEST,
        ptr.BASISOFDIAGNOSIS,
        ptr.site_icd10,
        ptr.site_icd10_3char,
        ptr.MORPH_ICD10_O2,
        ptr.STAGE_BEST,
        ptr.T_BEST,
        ptr.FIGO,
        ptr.DEATHDATEBEST,
        ptr.DCO,
        ptr.ETHNICITY,
        ptr.CHRL_TOT_27_03,
        ptr.IMD_QUINTILE_LSOAS,
        ptr.icb_20yy_name,
        ptr.icb_20yy_code,
        ptr.canalliance_20yy_name,
        ptr.canalliance_20yy_code,
        ptr.AVCT_DATE,
        ptr.SACT_DATE,
        ptr.SACT2_DATE,
        ptr.HESCT_DATE,
        sf.summary_ct_date as summary_ct_date,
        ptr.AVRT_DATE,
        ptr.RTDS_DATE,
        ptr.RTDS2_DATE,
        sf.summary_rt_date as summary_rt_date,
        ptr.AVSG_DATE,
        ptr.HESSG_DATE,
        ptr.CBAVSG_DATE,
        ptr.CBHESSG_DATE,
        ptr.COLOAVSG_DATE,
        ptr.COLOHESSG_DATE,
        ptr.APPENAVSG_DATE,
        ptr.APPENHESSG_DATE,
        ptr.BLADAVSG_DATE,
        ptr.BLADHESSG_DATE,
        ptr.BLAD_INSITU_AVSG_DATE,
        ptr.BLAD_INSITU_HESSG_DATE,
        ptr.LIVAVSG_DATE,
        ptr.LIVHESSG_DATE,
        ptr.OESOAVSG_DATE,
        ptr.OESOHESSG_DATE,
        ptr.STOMAVSG_DATE,
        ptr.STOMHESSG_DATE,
        sf.summary_sg_date as summary_sg_date,
        ptr.AVCT_TRUST_CODE,
        ptr.SACT_TRUST_CODE,
        ptr.SACT2_TRUST_CODE,
        ptr.hesct_trust_code,
        sf.summary_ct_trustcode,
        atr_ct.trustname as summary_ct_trustname,
        ptr.AVRT_TRUST_CODE,
        ptr.RTDS_TRUST_CODE,
        ptr.RTDS2_TRUST_CODE,
        sf.summary_rt_trustcode,
        atr_rt.trustname as summary_rt_trustname,
        ptr.AVSG_TRUST_CODE,
        ptr.HESSG_TRUST_CODE,
        ptr.CBAVSG_TRUST_CODE,
        ptr.CBHESSG_TRUST_CODE,
        ptr.COLOAVSG_TRUST_CODE,
        ptr.COLOHESSG_TRUST_CODE,
        ptr.APPENAVSG_TRUST_CODE,
        ptr.APPENHESSG_TRUST_CODE,
        ptr.BLADAVSG_TRUST_CODE,
        ptr.BLADHESSG_TRUST_CODE,
        ptr.BLAD_INSITU_AVSG_TRUST_CODE,
        ptr.BLAD_INSITU_HESSG_TRUST_CODE,
        ptr.LIVAVSG_TRUST_CODE,
        ptr.LIVHESSG_TRUST_CODE,
        ptr.OESOAVSG_TRUST_CODE,
        ptr.OESOHESSG_TRUST_CODE,
        ptr.STOMAVSG_TRUST_CODE,
        ptr.STOMHESSG_TRUST_CODE,
        sf.summary_sg_trustcode,
        atr_sg.trustname as summary_sg_trustname
    FROM pre_treat_table ptr
    LEFT JOIN summary_fields sf 
        ON ptr.tumourid = sf.tumourid 
    -- Trust name
    LEFT JOIN analysisncr.trustsics@snapshot_hold atr_ct 
        ON sf.summary_ct_trustcode = atr_ct.code
    LEFT JOIN analysisncr.trustsics@snapshot_hold atr_rt 
        ON sf.summary_rt_trustcode = atr_rt.code
    LEFT JOIN analysisncr.trustsics@snapshot_hold atr_sg 
        ON sf.summary_sg_trustcode = atr_sg.code;