### MB ####

#excluding Manitoba

# ALS_MB <- st_read("layers/MB/MB_LiDAR_Tracker.shp")
#
# ALS_MB %<>% select(ACQUIRED) %>%
#   mutate(ACQUIRED=as.numeric(str_sub(ACQUIRED, start = 1, end=4))) %>%
#   mutate(PPM=NA)
#
#
#
# ALS_MB_diss <-
#   ALS_MB %>% select(YEAR=ACQUIRED, PPM) %>%
#   st_transform(crs = the_crs) %>%
#   st_make_valid() %>%
#   mutate(area = st_area(.)) %>%
#   group_by(YEAR, PPM) %>%
#   summarise(n=n(), area=sum(area), geometry = st_union(geometry)) %>%
#   mutate(Province="MB") %>%
#   mutate(isAvailable=3) %>%
#   st_cast() %>%
#   select(Province, YEAR, PPM, n, area, isAvailable)
