SELECT 
    saturn_spriden.spriden_pidm, 
    saturn_spriden.spriden_id, 
    saturn_spriden.spriden_first_name, 
    saturn_spriden.spriden_mi, 
    saturn_spriden.spriden_last_name, 
    saturn_spbpers.spbpers_birth_date
FROM 
    saturn_spriden 
    INNER JOIN saturn_spbpers ON saturn_spriden.spriden_pidm = saturn_spbpers.spbpers_pidm
WHERE 
    saturn_spriden.spriden_change_ind IS NULL
GROUP BY 
    saturn_spriden.spriden_pidm, 
    saturn_spriden.spriden_id, 
    saturn_spriden.spriden_first_name, 
    saturn_spriden.spriden_mi, 
    saturn_spriden.spriden_last_name, 
    saturn_spbpers.spbpers_birth_date;

-- And add column with the header SEARCH_DATE for the search date in question in mm/dd/yyyy format.