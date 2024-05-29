library(data.table)
library(readr)

# Data
## primary care physicians with tract
phys_tract <- fread("Access to Care (HOI)/data/working/va_2017_2021_primary_care_phys_by_tract.csv")
phys_tract <- phys_tract[!is.na(Tract_FIPS)]
## distances between tract centroids
if (exists("tract_distances")) rm(tract_distances)
for (i in 1:5) {
  trdist <- readr::read_csv(paste0("../tract_distance_matrix/data/tract2tract_dist_time_2020/tract2tract_dist_time_", i,".zip"))
  if (!exists("tract_distances")) tract_distances <- trdist else tract_distances <- rbindlist(list(tract_distances, trdist))
}
tract_distances <- tract_distances[!is.na(tract_dest)]

# Physician count per tract 
phys_per_tract <- phys_tract[, .(count = .N), by=.(Tract_FIPS, Year)][order(Tract_FIPS, Year)]
phys_per_tract <- phys_per_tract[, .(geoid = Tract_FIPS, year = Year, phys_cnt = count)]

tract_distances[, tract_orig := as.character(tract_orig)]
tract_distances[, tract_dest := as.character(tract_dest)]

# Physicians within 30mi of tract centroid

tract_distances_lt30mi <- tract_distances[dist_meters <= 48280.3, .(geoid = tract_orig, geoid30 = tract_dest)]
self_tract <- data.table::data.table(geoid = unique(tract_distances_lt30mi$geoid), geoid30 = unique(tract_distances_lt30mi$geoid))
tract_distances_lt30mi <- rbindlist(list(tract_distances_lt30mi, self_tract))

phys_per_tract[, geoid := as.character(geoid)]

tract_distances_lt30mi_phys <- merge(tract_distances_lt30mi, phys_per_tract, by.x = "geoid30", by.y = "geoid", allow.cartesian = T)
tract_distances_lt30mi_phys <- tract_distances_lt30mi_phys[, .(geoid, geoid30, year, phys_cnt)]

phys_tract_30mi <- unique(tract_distances_lt30mi_phys[, .(geoid, year, phys_cnt)][, phys_30_cnt := sum(phys_cnt), by = c("geoid", "year")])
fwrite(phys_tract_30mi, "Access to Care (HOI)/data/distribution/va_cnt_physician_lt_30mi_from_tract.csv")



# tract no insurance 
pop_insur_tract <- fread("Access to Care (HOI)/data/working/va_tr_2017_2021_tot_pop_tot_insur.csv")


pop_insur_tract_wide <- dcast(pop_insur_tract, geoid+year ~ measure, value.var = "value")
pop_insur_tract_wide <- pop_insur_tract_wide[, .(geoid = as.character(geoid), year, tot_pop_cnt = B01001_001, no_ins_cnt = B27010_033 + B27010_050)]

phy_pop_insur_tract <- merge(pop_insur_tract_wide, phys_tract_30mi, by = c("geoid", "year"), all.x = T)
phy_pop_insur_tract <- phy_pop_insur_tract[, phys_cnt := as.double(phys_cnt)]
phy_pop_insur_tract[is.na(phys_cnt), phys_cnt := 0.0001]


phy_pop_insur_tract[, pop_phys := tot_pop_cnt/phys_cnt]
phy_pop_insur_tract[, pop_phys_z := scale(pop_phys)]
phy_pop_insur_tract[, pop_phys_z := scale(pop_phys)]
phy_pop_insur_tract[, no_ins_pct := (no_ins_cnt/tot_pop_cnt) * 100]
phy_pop_insur_tract[, no_ins_pct_z := scale(no_ins_pct)]
phy_pop_insur_tract[, sum_z := scale(pop_phys_z + no_ins_pct_z)]
phy_pop_insur_tract[, indx := sum_z * -1]







