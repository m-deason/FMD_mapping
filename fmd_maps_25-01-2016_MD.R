#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### process and create maps using the fmd model output ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# setwd('C:/Users/Michael/Dropbox/fmd_model_output/')
# load('fmd_maps.RData')

library(plyr)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
# library(gpclib)
library(maptools)
library(gridExtra)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### read in a the gb grid shapefile ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

gb.10km <- readOGR(dsn = 'C:/Users/Michael/Google Drive/FMD model input/gb_10km', #  location of the folder containing the shapefiles
                  layer = 'gb_10km') #  name of the shapefiles without extensions

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### read in scotland parish shapefile ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

scot.par <- readOGR(dsn = 'C:/Users/Michael/Google Drive/FMD model input/par', #  location of the folder containing the shapefiles
                    layer = 'Agricultural_parishes') #  name of the shapefiles without extensions
scot.par <- spTransform(scot.par, CRS(proj4string(gb.10km))) #  transform CRS to match gb.10km

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 'over' the map of scotland with the gb grid ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

scot.grid <- gb.10km[!is.na(over(gb.10km, as(scot.par, "SpatialPolygons"))), ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### observed location data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# from the observed input data
# create a master list of cphs with x and y

# model.input <- read.csv('/Users/sibylle/Google Drive/FMD model imput/fmd_rewire_observed_complete.csv', header = TRUE, stringsAsFactors = FALSE)
model.input <- read.csv('C:/Users/Michael/Google Drive/FMD model input/fmd_rewire_observed_complete.csv', header = TRUE, stringsAsFactors = FALSE)
locations <- data.frame(cph = append(model.input$Best_CPH_Dep, model.input$Best_CPH_Dest),
                        x = append(model.input$Best_Easting_Dep, model.input$Best_Easting_Dest),
                        y = append(model.input$Best_Northing_Dep, model.input$Best_Northing_Dest))
locations$cph <- as.character(locations$cph) #  initially stored as factors; store as characters instead
locations <- locations[complete.cases(locations), ] #  remove those cphs without complete x and y
locations <- locations[locations$y > 500000, ] #  remove databasing errors
locations <- locations[!duplicated(locations[c('cph')]), ] #  2408 cphs have duplicate geocoords; take the first
rm(model.input)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### scotland agricultural parishes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 881 unique parishes, but 886 rows in data frame :/
# officially, 891 parishes in the records
# 10 parishes are not represented in the file

key <- data.frame(county = as.numeric(substr(locations$cph, 1, 2)), #  extract county from cph
                  parish = as.numeric(substr(locations$cph, 4, 6))) #  extract parish from cph
key <- key[key$county >= 66, ] #  only scottish locations

# remove errors from key; * is consensus
# replace with correct counties

#          county parish
# 1522      77    371*
# 2939      91    736*
# 2959      85    603*
# 5683      89    702*
# 5884      72    275*
# 20756     75    275
# 21038     70    603
# 21277     73    371
# 21622     67    702
# 23331     96    736

key$county <- ifelse(key$parish == 275 & key$county == 75, 72,
                     ifelse(key$parish == 603 & key$county == 70, 85,
                            ifelse(key$parish == 371 & key$county == 73, 77,
                                   ifelse(key$parish == 702 & key$county == 67, 89,
                                          ifelse(key$parish == 736 & key$county == 96, 91,
                                                 key$county)))))
key.unique <- unique(key)

# add counties to missing parishes; use map for visual reference
# par.over[!par.over$parish %in% key$parish, ]
# 248 = Eyemouth; use 243; county == 71
# 369 = Prestonpans; use 373; county == 77
# 430 = Pittenweem; use 416; county == 78
# 551 = Rutherglen; use 553; county == 83
# 555 = Govan; use 553; county == 83
# 559 = Kirkliston; use 845; county == 97
# 580 = Corstorphine; use 582; county == 84
# 581 = Cramond; use 582; county == 84
# 583 = Leith; use 582; county == 84
# 720 = Cathcart; use 722; county == 90

missing.key <- data.frame(parish = c(248, 369, 430, 551, 555, 559, 580, 581, 583, 720),
                          county = c(71, 77, 78, 83, 83, 97, 84, 84, 84, 90))

master.key <- rbind(key.unique, missing.key) #  combine the unique county/parish data with the missing data
master.key <- master.key[order(master.key$parish), ] #  order the parishes
rm(key, key.unique, missing.key)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### model output summary files ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# summary file contains a matrix with cphs and dates of infection
# months should be in seperate folders by month

# summary.path <- '/Users/sibylle/Documents/DB_Sibylle/SimulationResultFinal/summary/' #  location of all the summary files
summary.path <- 'C:/Users/Michael/Desktop/summary/' #  location of all the summary files
filenames <- list.files(path = summary.path,
                        pattern = "rewire") #  look for the pattern 'rewire' in the above path
names <- substr(filenames, 1, nchar(filenames) - 4) #  Create list of data frame names without the extentions
df.names <- c() #  create an empty vector to store the data.frame names

# Load all rewired files; assign reasonable names

for(i in names){

  cat(which(i == names), 'of', length(names), 'rewired networks \n')

  csv.name <- strsplit(i, '_') #  seperate the fields by '_'

  month <- format(as.Date(csv.name[[1]][10], format = '%d-%m-%Y'), '%b') #  month is as.Date = 10th field
  stand.still <- csv.name[[1]][4] #  stand still = 4th field

  nearest.market <- csv.name[[1]][7] #  nearest market = 7th field
  nearest.market <- substr(nearest.market,1,1) #  only store the first letter

  exemptions <- csv.name[[1]][9]  # exemptions = 9th field
  exemptions <- substr(exemptions, 1, 1) #  only store the first letter

  rewired.name <- paste0(month, '_', stand.still, '_', nearest.market, exemptions) #  create new dataframe names
  df.names[which(i == names)] <- rewired.name #  store df.names

  output <- read.csv(file.path(summary.path, #  read in file from csv; assign new name
                          paste0(i, ".csv")))
  output$Thread <- as.factor(output$Thread)
  test <- aggregate(output[, 2:length(output), drop = FALSE], 
                    list(group = output$Thread), mean) #  takes the mean for each row in output
  model.summary.t <- as.data.frame(t(test[, -1]), header = TRUE) #  input needs to be transposed
  
  x <- data.frame(total = rowSums(model.summary.t), #  calculate the row sums
                  cph = rownames(model.summary.t)) #  get cphs from row names
  names(x) <- c(paste0(rewired.name,'.total'), 'cph') #  rename the column names

  x$cph <- gsub('[.]', '/', x$cph) #  square brackets needed to replace full stop
  x$cph <- gsub('X', '', x$cph) #  remove 'X' from cph
  row.names(x) <- NULL #  remove row names
  assign(rewired.name, x) #  assign dataframe x a new name

  rm(model.summary.t, exemptions, month, nearest.market, stand.still, x) #  clean up

}

# load all observed files

observed.file.list <- list.files(path = summary.path, pattern = "observed") #  create a list of the observed file names; October manually capitalised in file name
network.months <- unique(substr(df.names, 1, 3)) #  substring out the names of the rewired month

for (i in network.months) {
  
  cat(i, '\n')
  model.summary <- read.csv(paste0(summary.path, observed.file.list[grep(pattern = i, observed.file.list)])) #  read in the observed
  model.summary$Thread <- as.factor(model.summary$Thread)
  test <- aggregate(model.summary[, 2:length(model.summary), drop = FALSE], 
                    list(group = model.summary$Thread), mean)
  model.summary.t <- as.data.frame(t(test[, -1]), header=TRUE) #  needs to be transposed
  
  summary.agg <- data.frame(total = rowSums(model.summary.t),
                            cph = rownames(model.summary.t))
  
  names(summary.agg) <- c(paste0(i,'.observed'), 'cph') #  rename the column names
  summary.agg$cph <- gsub('[.]', '/', summary.agg$cph) #  square brackets needed to replace full stop
  summary.agg$cph <- gsub('X', '', summary.agg$cph) #  remove 'X' from cph
  row.names(summary.agg) <- NULL # remove row names
  
  assign(paste0(i, '_observed'), summary.agg)
  
}

rm(observed.file.list, model.summary, model.summary.t, test, summary.agg, i)

# base R attempt

# model.summary$Thread <- as.factor(model.summary$Thread)
#test <- aggregate(model.summary ~ model.summary$Thread, model.summary[,2:27090], SUM)
#   code value
#tmodel.summary <- setNames(data.frame(t(model.summary[,-1])), model.summary[,1])


### Collapse over thread id; I think mean is better than sum
###
#test <- aggregate(model.summary[, 2:27089, drop=F], list(group=model.summary$Thread), sum)

# test <- aggregate(model.summary[, 2:27089, drop=F], list(group=model.summary$Thread), mean)

# model.summary.t <- as.data.frame(t(test[, -1]), header=TRUE) #  needs to be transposed
# summary.agg <- data.frame(total = rowSums(model.summary.t),
#                           cph = rownames(model.summary.t))
# summary.agg$cph <- gsub('[.]', '/', summary.agg$cph) #  square brackets needed to replace full stop
# summary.agg$cph <- gsub('X', '', summary.agg$cph) #  remove 'X' from cph
# row.names(summary.agg) <- NULL # remove row names

# merge all dataframes; 6225 cphs don't have geocoords
# Reduce function recommended by Stack Overflow, not 100% sure how it works though :/

df.names <- c(df.names, ls(pattern = 'observed'))#  add the observed data.frames to list of rewired data.frames
summary.plot <- Reduce(function(x, y) merge(x, y, by = 'cph'), append(list(locations), lapply(df.names, get)))


# calculate the number of unique holdings infected
# rewired networks still have more infected premises

# convert to spatialpointsdataframe
summary.points <- SpatialPointsDataFrame(cbind(summary.plot$x,
                                               summary.plot$y),
                                         data = summary.plot[,4:length(summary.plot)],
                                         proj4string = CRS("+init=epsg:27700")) #  projection for the observed data
summary.points <- spTransform(summary.points, CRS(proj4string(gb.10km))) #  transform CRS to match gb.10km

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### calculate the number of infected farms per polygon ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# may just be able to merge by id at fortify level

### grid calculations

grid.over <- over(as(scot.grid, "SpatialPolygons"), summary.points, fn = sum) #  sum the number of infected farms by 10km grid
grid.over$id <- rownames(grid.over) #  store row names as id for later merging

grid.length <- over(as(scot.grid, "SpatialPolygons"), summary.points[1], fn = length) #  count the number of farms in each 10km grid
names(grid.length) <- c('num_farms') #  rename variable to avoid confusion
grid.length$id <- rownames(grid.length) #  store row names as id for later merging

scot.grid@data$id <- rownames(scot.grid@data) #  create an id column for merging
scot.grid.xy <- fortify(scot.grid, region = 'id') #  convert spatial dataframe into normal dataframe for ggplot2
scot.grid.xy <- scot.grid.xy[order(scot.grid.xy$order), ] #  order the new dataframe

# combine polygons data with number of infected locations and total number of locations
scot.grid.xy.merge <- merge(scot.grid.xy, grid.over, all.x = TRUE)
scot.grid.xy.merge <- merge(scot.grid.xy.merge, grid.length, all.x = TRUE)

scot.grid.xy.merge[is.na(scot.grid.xy.merge)] <- 0 #  replace all NAs with 0s

#
for(i in names(scot.grid.xy.merge)[grepl('.total', names(scot.grid.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
  
  if (substr(i, 1, 3) == 'Jun'){
    
    scot.grid.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Jun.observed #  find the change between the network and observed
    scot.grid.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Jun.observed) / scot.grid.xy.merge$Jun.observed #  find the percentage change
    
  } else {
    
    scot.grid.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Oct.observed #  find the change between the network and observed
    scot.grid.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Oct.observed) / scot.grid.xy.merge$Oct.observed #  find the percentage change
    
  }
}

### ag parish

# par.over <- over(scot.par, summary.points, fn = sum) #  sum the number of infected farms by parish
# par.over$id <- rownames(par.over) #  store row names as id for later merging
# 
# counties.length <- over(scot.par, summary.points[c('total')], fn = length) #  count the number of infected farms by parish
# names(counties.length) <- c('num_farms') #  rename variable to avoid confusion
# counties.length$id <- rownames(counties.length) #  store row names as id for later merging
# 
# scot.par@data$id <- rownames(scot.par@data) #  create an id column for merging
# scot.par.xy <- fortify(scot.par, region='id') #  convert spatial dataframe into normal dataframe for ggplot2
# scot.par.xy <- scot.par.xy[order(scot.par.xy$order), ] #  order the new dataframe
# 
# # combine polygons data with number of infected locations and total number of locations
# scot.par.xy.merge <- merge(scot.par.xy, par.over, all.x = TRUE)
# scot.par.xy.merge <- merge(scot.par.xy.merge, counties.length, all.x = TRUE)
# 
# scot.par.xy.merge$total[is.na(scot.par.xy.merge$total)] <- 0
# 
# #
# for(i in names(scot.par.xy.merge)[grepl('.total', names(scot.par.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
# 
#   scot.par.xy.merge[[paste0('delta', substr(i, 1, nchar(i)-6))]] <- scot.par.xy.merge[[i]] - scot.par.xy.merge$total #  find the change between the network and observed
#   scot.par.xy.merge[[paste0('per.change', substr(i, 1, nchar(i)-6))]] <- (scot.par.xy.merge[[i]] - scot.par.xy.merge$total) / scot.par.xy.merge$total #  find the percentage change
# 
# }
# 
# 
# # ag county union
# county.id <- factor(master.key$county) #  factorise the list of counties
# scot.par.union <- unionSpatialPolygons(scot.par, county.id) #  combine the parishes into counties by the counties vector
# 
# test <- data.frame(getSpPPolygonsIDSlots(scot.par.union)) #  create a dataframe with ids from the counties spatial polygons
# row.names(test) <- test$getSpPPolygonsIDSlots.scot.par.union #  add the rownames to new dataframe
# scot.par.union <- SpatialPolygonsDataFrame(Sr = scot.par.union, #  create a new spatial polygons data frame; needed for fortify
#                                            data = test)
# rm(test)
# 
# # county level fortify
# scot.par.union@data$id <- rowna4mes(scot.par.union@data) #  create an id column for merging
# scot.par.union.xy <- fortify(scot.par.union, region = 'id') #  convert spatial dataframe into normal dataframe for ggplot2
# scot.par.union.xy <- scot.par.union.xy[order(scot.par.union.xy$order), ] #  order the new dataframe
# 
# par.over.union <- over(scot.par.union, summary.points, fn = sum) #  sum the number of infected farms by county
# par.over.union$id <- rownames(par.over.union) #  store row names as id for later merging
# 
# counties.length.union <- over(scot.par.union, summary.points[c('total')], fn = length) #  count the number of infected farms by parish
# names(counties.length.union) <- c('num_farms') #  rename variable to avoid confusion
# counties.length.union$id <- rownames(counties.length.union) #  store row names as id for later merging
# 
# # combine polygons data with number of infected locations and total number of locations
# scot.par.union.xy.merge <- merge(scot.par.union.xy, par.over.union)
# scot.par.union.xy.merge <- merge(scot.par.union.xy.merge, counties.length.union)
# 
# for(i in names(scot.par.union.xy.merge)[grepl('.total', names(scot.par.union.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
# 
#   scot.par.union.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$total #  find the change between the network and observed
#   scot.par.union.xy.merge[[paste0('per.change.', substr(i, 1, nchar(i) - 6))]] <- (scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$total) / scot.par.union.xy.merge$total #  find the percentage change
# 
# }


#~~~~~~~~~~~~~~~~~~~#
#### create maps ####
#~~~~~~~~~~~~~~~~~~~#

map.theme <- theme(panel.background = element_rect(fill = "lightblue3"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.border = element_blank())

ggplot() +
  # geom_polygon(data=scot.par.xy.merge, #  parishes
  # geom_polygon(data=scot.par.union.xy.merge, #  counties
  geom_polygon(data=scot.grid.xy.merge, #  grid
               aes(long,
                   lat,
                   # fill=num_farms,
                   # fill = Jun.observed,
                   # fill = Jun_13_FF.total,
                   # fill = per.changeJun_13_FF,
                   fill = deltaJun_13_FF,
                   group=group),
               colour='black') + #  draws borders for either parishes or counties
  coord_equal() + map.theme +
  scale_fill_continuous(#'Observed of farms\nper 10km grid',
                        low = 'white', high = 'red2')
# #Sibylle
# ggplot() +
#   #geom_polygon(data=scot.par.union.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    fill=per.change.Oct_13_FF)) +#,
#                    #fill=deltaOct_13_FF))+
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_13_FF\nPercent change in the\nnumber of infected farms',
#                        #limits=c(-0.5, 0.5),
#                        colours=c("#E5F5F9","#99D8C9","#2CA25F"))#,
# 
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    #fill=per.changeOct_13_FT)) +#,
#                    fill=deltaOct_13_FT)) +
#   #fill = log10(delta.Oct_13_FT + 120855)), colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_13_FT\nTotal change in the\nnumber of infected farms',
#                        #limits=c(0, 5.25),
#                        colours=rev(rainbow(2252, start = 0, end = 0.66))) #,
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    #fill=per.changeOct_13_TF)) +#,
#                    fill = deltaOct_13_TF), # max = 2178, min = -42665
#                colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_13_TF\nTotal change in the\nnumber of infected farms',
#                        trans = "log",
#                        #colours=rev(rainbow(2252, start = 0, end = 0.66)))#,
#                        colours=c("#E5F5F9","#99D8C9","#2CA25F"))
# 
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    # fill=per.changeOct_13_TT)) +#,
#                    fill=deltaOct_13_TT), # max = 3770, min = -26270
#                colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_13_TT\nTotal change in the\nnumber of infected farms',
#                        limits=c(-120854, 45525),
#                        colours=rev(rainbow(2252, start = 0, end = 0.66))) # ,
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    #fill=per.changeOct_6_FF)) +#,
#                    fill = deltaOct_6_FF), # max = 31979, min = -16698
#                colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_6_FF\nTotal change in the\nnumber of infected farms',
#                        limits=c(-120854, 45525),
#                        colours=rev(rainbow(2252, start = 0, end = 0.66)))#,
# 
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.union.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    # fill=per.changeOct_6_FT)) +#,
#                    fill=deltaOct_6_FT), # max = 22918, min = -11123
#                colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_6_FT\nTotal change in the\nnumber of infected farms',
#                        limits=c(-120854, 45525),
#                        colours=rev(rainbow(2252, start = 0, end = 0.66))) #,
# 
# ggplot() +
#   #geom_polygon(data=scot.par.xy.merge, #  parishes
#   geom_polygon(data=scot.par.xy.merge, #  counties
#                aes(long,
#                    lat,
#                    group=group,
#                    #fill=per.changeOct_6_TT)) +#,
#                    fill=deltaOct_6_TT), # max = 2106, min = -48063
#                colour='black') + #  draws borders for either parishes or counties
#   coord_equal() + map.theme +
#   scale_fill_gradientn('Oct_6_TT\nTotal change in the\nnumber of infected farms',
#                        limits=c(-120854, 45525),
#                        colours=rev(rainbow(2252, start = 0, end = .66)))#,

# density gridarrange
# png
#grid.arrange(
ggplot(scot.par.union.xy.merge) +
  geom_density(data = scot.par.union.xy.merge, aes(x = total, colour='red')) +
  geom_density(data = scot.par.union.xy.merge, aes(x = delta.Oct_13_FF, colour='blue')) +
  geom_density(data = scot.par.union.xy.merge, aes(x = delta.Oct_13_FT, colour='green')) +
  geom_density(data = scot.par.union.xy.merge, aes(x = delta.Oct_13_TF, colour='yellow')) +
  geom_density(data = scot.par.union.xy.merge, aes(x = delta.Oct_13_TT, colour='brown'))
#ncol = 2
#)

