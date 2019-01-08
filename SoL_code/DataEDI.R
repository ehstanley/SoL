##want to put sol_data and lakes files on EDI to share with publication. clean up extraneous cols a bit
#save csvs

sol <-readRDS("SoL_data/SoL_data.rds")

lakes <-readRDS("SoL_data/lakes.rds")

sol$srp=NULL
sol$srp_censorcode=NULL
sol$srp_detectionlimit=NULL
sol$year=NULL
sol$month=NULL
sol$day=NULL
sol$secchi_qual=NULL

lakes$state_zoneid=NULL

write.csv(sol, "SoL_data/Stanley_SoL_limnodata.csv")
write.csv(lakes, "SoL_data/Stanley_SoL_lakes.csv")
