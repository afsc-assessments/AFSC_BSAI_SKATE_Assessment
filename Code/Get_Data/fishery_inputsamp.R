SELECT
norpac.debriefed_length_mv.year,
norpac.debriefed_length_mv.fmp_gear,
round(
  sqrt(
    COUNT(DISTINCT norpac.debriefed_length_mv.haul_join)
  ), 1
)                                                    AS "Nsamp",
COUNT(DISTINCT norpac.debriefed_length_mv.haul_join) AS "Hauls"
FROM
norpac.debriefed_length_mv
WHERE
norpac.debriefed_length_mv.species = 88
AND norpac.debriefed_length_mv.nmfs_area BETWEEN 500 AND 543
GROUP BY
norpac.debriefed_length_mv.year,
norpac.debriefed_length_mv.fmp_gear
ORDER BY
norpac.debriefed_length_mv.year