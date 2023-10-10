SELECT
    SUM(council.comprehensive_blend_ca.weight_posted) AS sum_weight_posted,
    council.comprehensive_blend_ca.fmp_area,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.agency_species_code,
    council.comprehensive_blend_ca.species_group_code,
    council.comprehensive_blend_ca.retained_or_discarded,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.agency_gear_code,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.harvest_sector
FROM
    council.comprehensive_blend_ca
WHERE
    council.comprehensive_blend_ca.fmp_area = 'BSAI'
    AND council.comprehensive_blend_ca.species_name LIKE '%skate%'
    AND council.comprehensive_blend_ca.year > 2002
GROUP BY
    council.comprehensive_blend_ca.fmp_area,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.agency_species_code,
    council.comprehensive_blend_ca.species_group_code,
    council.comprehensive_blend_ca.retained_or_discarded,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.agency_gear_code,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.harvest_sector