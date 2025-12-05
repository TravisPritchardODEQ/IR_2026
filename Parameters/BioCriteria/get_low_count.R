
get_low_count <- function(){

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


low_count <- DBI::dbGetQuery(con, "SELECT        VW_StationsAllDataAllOrgs.MLocID, VW_StationsAllDataAllOrgs.StationDes, VW_StationsAllDataAllOrgs.AU_ID, dbo.Station_AU_GNIS_Name.AU_GNIS_Name, VW_StationsAllDataAllOrgs.MonLocType, 
                         VW_StationsAllDataAllOrgs.TribalLand, VW_StationsAllDataAllOrgs.ben_use_code, VW_StationsAllDataAllOrgs.EcoRegion2, VW_StationsAllDataAllOrgs.OWRD_Basin, dbo.ResultsRawBioIndex.org_id, 
                         VW_StationsAllDataAllOrgs.GNIS_Name, dbo.ResultsRawBioIndex.Act_id, dbo.ResultsRawBioIndex.Sample_Date, dbo.ResultsRawBioIndex.Index_Name, dbo.ResultsRawBioIndex.Score, dbo.ResultsRawBioIndex.DQL, 
                         dbo.ResultsRawBioIndex.Comment, VW_StationsAllDataAllOrgs.Reachcode, VW_StationsAllDataAllOrgs.Measure
FROM            [DEQLEAD-LIMS].Stations.dbo.VW_StationsAllDataAllOrgs AS VW_StationsAllDataAllOrgs INNER JOIN
                         dbo.ResultsRawBioIndex ON VW_StationsAllDataAllOrgs.MLocID = dbo.ResultsRawBioIndex.MLocID AND VW_StationsAllDataAllOrgs.orgid = dbo.ResultsRawBioIndex.org_id LEFT OUTER JOIN
                         dbo.Station_AU_GNIS_Name ON VW_StationsAllDataAllOrgs.MLocID = dbo.Station_AU_GNIS_Name.MLocID
WHERE        (Comment = 'low count <300') AND (Sample_Date >= '1998-01-01') AND (VW_StationsAllDataAllOrgs.TribalLand = 0) AND (NOT (VW_StationsAllDataAllOrgs.AU_ID = N'99')) AND (NOT (VW_StationsAllDataAllOrgs.MLocID IN
                             (SELECT        MLocID
                               FROM            dbo.Unused_Stations)))")

return(low_count)
}
