SELECT
    obsint.debriefed_haul.haul_date,
    obsint.debriefed_haul.gear_type,
    obsint.debriefed_haul.vessel_type,
    obsint.debriefed_haul.nmfs_area,
    obsint.debriefed_haul.year,
    obsint.debriefed_spcomp.species,
    obsint.debriefed_spcomp.species_name,
    obsint.debriefed_spcomp.extrapolated_weight
FROM
    obsint.debriefed_spcomp
    INNER JOIN obsint.debriefed_haul ON obsint.debriefed_haul.haul_join = obsint.debriefed_spcomp.haul_join
WHERE
    ( obsint.debriefed_haul.nmfs_area BETWEEN 500 AND 544
      AND obsint.debriefed_haul.year > 2002
      AND obsint.debriefed_spcomp.species BETWEEN 85 AND 98 )
    OR ( obsint.debriefed_spcomp.species BETWEEN 159 AND 168 )