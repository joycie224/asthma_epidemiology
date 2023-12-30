# run this script to join highway and australia boundary shapefile distances

library(rgdal)
library(RPostgreSQL)
library(rpostgis)
library(sf)
library(rgeos)
library(raster)
library(osmextract)
library(raster)
library(smoothr)
library(ggplot2)


#itemtoget <- oe_match("Australia")
#roads_aus <- oe_download(itemtoget$url)
#osmosis --read-pbf  geofabrik_australia-latest.osm.pbf --bb left=115.4495 right=116.4151 top=-31.45512 bottom=-32.80187 --write-xml perth.osm
#//roads_aus_data <- oe_read(roads_aus)
#osm2pgrouting --f perth.osm --conf map-config.xml --dbname ewan_postgis --username postgres --host postgis --password 2aJ5CbvXl63BQH2Zx1 --clean
#makes in db: public.ways_vertices_pgr public.ways etc

dsn_database = "postgres"
dsn_hostname = "localhost"
dsn_port = "5432"
dsn_uid = "postgres"

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  connec <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                     )
  print("Database Connected!")
  dbGetQuery(connec, "SELECT postgis_full_version()")
},
error=function(cond) {
  print("Unable to connect to Database.")
})
pgListGeom(connec, geog = TRUE)

# filter down to highways 
dbGetQuery(connec, "SELECT * FROM ways LIMIT 1")
dbSendQuery(connec,"CREATE INDEX g_idx_spatial ON ways USING GIST(the_geom_new)") # creates spatial index, with a bounding box  
dbSendQuery(connec,"VACUUM ANALYSE ways") 
x<-dbGetQuery(connec, "WITH highways AS (SELECT the_geom_new FROM ways WHERE maxspeed_forward > 50) 
           SELECT m.\"SA1_CODE21\" FROM wa.mesh_blocks AS m LEFT JOIN highways ON ST_Intersects(m.geometry, highways.the_geom_new)")
yy <- aggregate((as.numeric(x$SA1_CODE21)*0)+1,list(as.numeric(x$SA1_CODE21)),sum)
# class(x$MB_CODE21)

# write.csv(yy, "SA1_by_highways.csv")

# australia boundary shapefile distances
aus <- readOGR("/users/joycemo/documents/summer2023/TKI/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
dzns <- readOGR("/users/joycemo/documents/summer2023/TKI/DZN_2021_AUST_GDA2020_SHP/DZN_2021_AUST_GDA2020.shp")
dzns <- dzns[dzns$STE_NAME21=="Western Australia",]
aus <- crop(aus,extent(dzns))

lines_australia <- as(aus, "SpatialLinesDataFrame")
st_write(obj = st_as_sf(lines_australia), dsn = connec, layer= c("public","aus_lines"))
dbSendQuery(connec, "UPDATE aus_lines SET geometry=ST_Transform(geometry,3112)")
dbSendQuery(connec,"CREATE INDEX b_idx_spatial ON aus_lines USING GIST(geometry)") 
dbSendQuery(connec,"VACUUM ANALYSE aus_lines") 
dbSendQuery(connec,"ALTER TABLE aus_lines ADD COLUMN gid serial")

dbGetQuery(connec,"SELECT ST_SRID(r.geometry) FROM aus_lines AS r") 

# convert polygons to extract boundaries 
x<-dbGetQuery(connec, "SELECT ST_Length(geometry) FROM aus_lines")
# returns distances into variable x 
x<-dbGetQuery(connec, "SELECT m.\"SA1_CODE21\" , ST_Distance(aus_lines.geometry, m.geometry) FROM wa.mesh_blocks AS m CROSS JOIN aus_lines")

aus_mean <- aggregate((as.numeric(x$st_distance)),list(as.numeric(x$SA1_CODE21)),mean)
# write.csv(aus_mean, "distances_sa1_perth.csv")

aus_mean_rescaled <- (aus_mean$x - min(aus_mean$x)) / diff(range(aus_mean$x, na.rm = TRUE))
plot(sa1_perth, col = hsv(aus_mean_rescaled), border = NA)
title("Perth Mean Distance to Coastline")
