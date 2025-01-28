CREATE MATERIALIZED VIEW albany_2024.parcels_with_turbines AS
WITH turbine_points AS (
    SELECT
        wt.gid as turbine_id,
        ST_SetSRID(ST_MakePoint(wt.xlong, wt.ylat), 4326) as geom
    FROM usgs.wind_turbines wt
    WHERE wt.t_state = 'WY'
)
SELECT
    p.gid,
    p.pk,
    p.pidn,
    p.lablepidn,
    p.accountno,
    p.name1,
    p.name2,
    p.address1,
    p.address2,
    p.city,
    p.state,
    p.zip,
    p.st_address,
    p.descript,
    p.grossacres,
    p.grosssf,
    p.totalval,
    p.landval,
    p.accttype,
    p.taxyear,
    p.taxdist,
    p.bldgs,
    p.lea,
    p.nbhd,
    p.geom,
    COUNT(t.turbine_id) as n_turbines
FROM
    albany_2024.parcels p
LEFT JOIN
    turbine_points t
ON
    ST_Intersects(
        ST_Transform(ST_SetSRID(p.geom, 32156), 4326),
        t.geom
    )
WHERE
    p.geom IS NOT NULL
GROUP BY
    p.gid,
    p.pk,
    p.pidn,
    p.lablepidn,
    p.accountno,
    p.name1,
    p.name2,
    p.address1,
    p.address2,
    p.city,
    p.state,
    p.zip,
    p.st_address,
    p.descript,
    p.grossacres,
    p.grosssf,
    p.totalval,
    p.landval,
    p.accttype,
    p.taxyear,
    p.taxdist,
    p.bldgs,
    p.lea,
    p.nbhd,
    p.geom;
