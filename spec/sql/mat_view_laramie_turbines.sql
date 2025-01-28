cREATE MATERIALIZED VIEW laramie_2022.parcels_with_turbines AS
WITH turbine_points AS (
    SELECT
        wt.gid as turbine_id,
        ST_SetSRID(ST_MakePoint(wt.xlong, wt.ylat), 4326) as geom
    FROM usgs.wind_turbines wt
    WHERE wt.t_state = 'WY'
)
SELECT
    p.gid,
    p.statepidn,
    p.mapno,
    p.areaid,
    p.localno,
    p.accountno,
    p.accttype,
    p.lea,
    p.nbhd,
    p.status,
    p.streetno,
    p.streetdir,
    p.streetname,
    p.primaryown,
    p.streetsuf,
    p.assignedto,
    p.netsf,
    p.netacres,
    p.name1,
    p.name2,
    p.address1,
    p.city,
    p.state,
    p.zipcode,
    p.legal,
    p.taxyear,
    p.share_date,
    p.totallandv,
    p.totalimpsv,
    p.totalcostv,
    p.assessedva,
    p.st_area_sh,
    p.st_length_,
    p.geom,
    COUNT(t.turbine_id) as n_turbines
FROM
    laramie_2022.parcels p
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
    p.statepidn,
    p.mapno,
    p.areaid,
    p.localno,
    p.accountno,
    p.accttype,
    p.lea,
    p.nbhd,
    p.status,
    p.streetno,
    p.streetdir,
    p.streetname,
    p.primaryown,
    p.streetsuf,
    p.assignedto,
    p.netsf,
    p.netacres,
    p.name1,
    p.name2,
    p.address1,
    p.city,
    p.state,
    p.zipcode,
    p.legal,
    p.taxyear,
    p.share_date,
    p.totallandv,
    p.totalimpsv,
    p.totalcostv,
    p.assessedva,
    p.st_area_sh,
    p.st_length_,
    p.geom;
