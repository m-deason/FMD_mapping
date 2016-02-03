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

grid.crop <- gb.10km[!is.na(over(gb.10km, as(scot.par, "SpatialPolygons"))), ]
scot.shell <- gIntersection(grid.crop, scot.par) #  maybe try to just get the shell of scotland?
scot.grid <- as(gIntersection(grid.crop, scot.shell, byid = TRUE, drop_lower_td = TRUE), 'SpatialPolygonsDataFrame')
rm(scot.shell)

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

load('fmd_rewire_top5.RData') #  load in the top5 percent of epidemic size and duration from fmd_density.R


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### model output matrices ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# summary file contains a matrix with cphs and dates of infection
# months should be in seperate folders by month
# thread number needs to be calculated in order to subset the largest 5% of epidemics

# summary.path <- '/Users/sibylle/Documents/DB_Sibylle/SimulationResultFinal/summary/' #  location of all the summary files
summary.path <- 'C:/Users/Michael/Desktop/summary/' #  location of all the summary files
filenames <- list.files(path = summary.path,
                        pattern = "rewire") #  look for the pattern 'rewire' in the above path
names <- substr(filenames, 1, nchar(filenames) - 4) #  Create list of data frame names without the extentions
df.names <- c() #  create an empty vector to store the data.frame names

output.summary <- data.frame(matrix(ncol = 6, nrow = length(names)))
names(output.summary) <- names(summary(c(1,2))) #  example use of summary to get the names
# Load all rewired files; assign reasonable names

for(i in names){
  
  cat(which(i == names), 'of', length(names), 'rewired networks \n')
  
  csv.name <- strsplit(i, '_') #  seperate the fields by '_'
  
  month <- format(as.Date(csv.name[[1]][10], format = '%d-%m-%Y'), '%b') #  month is as.Date = 10th field
  stand.still <- csv.name[[1]][4] #  stand still = 4th field
  
  nearest.market <- csv.name[[1]][7] #  nearest market = 7th field
  nearest.market <- substr(nearest.market, 1, 1) #  only store the first letter
  
  exemptions <- csv.name[[1]][9]  # exemptions = 9th field
  exemptions <- substr(exemptions, 1, 1) #  only store the first letter
  
  rewired.name <- paste0(month, '_', stand.still, '_', nearest.market, exemptions) #  create new dataframe names
  df.names[which(i == names)] <- rewired.name #  store df.names
  
  output <- read.csv(file.path(summary.path, #  read in file from csv; assign new name
                               paste0(i, ".csv")))
  
#   output <- output[order(output$Thread), ] #  order the threads 
#   output$run.id <- rep.int(0:499, 21) #  assign the rep number to join with the top5 lists
#   output$concat <- paste0(output$Thread, '_', output$run.id) # concat then subset
#   
#   top5.size <- get(paste0(rewired.name, '_top5.size'))
#   top5.size$concat <- paste0(top5.size$Thread, '_', top5.size$rep)
#   output.top5.size <- output[output$concat %in% top5.size$concat, ] #  subset the output for top5 size
#   
  output$Thread <- as.factor(output$Thread)
  
  output.summary[which(names==i), ] <- summary(output$Total)
  
  test <- aggregate(output[, 2:length(output) - 1, drop = FALSE], 
                    list(group = output$Thread), mean) #  takes the mean for each row in output
  
  model.summary.t <- as.data.frame(t(test[, -1]), header = TRUE) #  input needs to be transposed
  x <- data.frame(total = rowSums(model.summary.t[2:nrow(model.summary.t), ]), #  calculate the row sums
                  cph = rownames(model.summary.t[2:nrow(model.summary.t), ])) #  get cphs from row names
  names(x) <- c(paste0(rewired.name,'.total'), 'cph') #  rename the column names
  
  x$cph <- gsub('[.]', '/', x$cph) #  square brackets needed to replace full stop
  x$cph <- gsub('X', '', x$cph) #  remove 'X' from cph
  row.names(x) <- NULL #  remove row names
  assign(rewired.name, x) #  assign dataframe x a new name
  
  rm(model.summary.t, exemptions, month, nearest.market, stand.still, x) #  clean up
  
}

row.names(output.summary) <- df.names[1:16]

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

rm(model.summary, model.summary.t, test, summary.agg, i)

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

plot.names <- c(df.names, ls(pattern = '_observed')) #  add the observed data.frames to list of rewired data.frames
summary.plot <- Reduce(function(x, y) merge(x, y, by = 'cph'), append(list(locations), lapply(plot.names, get)))


# calculate the number of unique holdings infected
# rewired networks still have more infected premises

# convert to spatialpointsdataframe
summary.points <- SpatialPointsDataFrame(cbind(summary.plot$x,
                                               summary.plot$y),
                                         data = summary.plot[,4:length(summary.plot)],
                                         proj4string = CRS("+init=epsg:27700")) #  projection for the observed data
summary.points <- spTransform(summary.points, CRS(proj4string(gb.10km))) #  transform CRS to match gb.10km
rm(gb.10km) #  free up memory

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### calculate the number of infected farms per polygon ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# process data to create maps
# may just be able to merge by id at fortify level

#~~~~~~~~~~~~~~~~~~~~~~~~~#
#### grid calculations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#

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

# scot.grid.xy.merge[is.na(scot.grid.xy.merge)] <- 0 #  replace all NAs with 0s

for(i in names(scot.grid.xy.merge)[grepl('.total', names(scot.grid.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
  
  if (substr(i, 1, 3) == 'Jun'){
    
    scot.grid.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Jun.observed #  find the change between the network and observed
    scot.grid.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Jun.observed) / scot.grid.xy.merge$Jun.observed #  find the percentage change
    
  } else {
    
    scot.grid.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Oct.observed #  find the change between the network and observed
    scot.grid.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.grid.xy.merge[[i]] - scot.grid.xy.merge$Oct.observed) / scot.grid.xy.merge$Oct.observed #  find the percentage change
    
  }
}

# #~~~~~~~~~~~~~~~~~#
# #### ag parish ####
# #~~~~~~~~~~~~~~~~~#
# 
# par.over <- over(as(scot.par, 'SpatialPolygons'), summary.points, fn = sum) #  sum the number of infected farms by parish
# par.over$id <- rownames(par.over) #  store row names as id for later merging
# 
# counties.length <- over(as(scot.par, 'SpatialPolygons'), summary.points[1], fn = length) #  count the number of infected farms by parish
# names(counties.length) <- c('num_farms') #  rename variable to avoid confusion
# counties.length$id <- rownames(counties.length) #  store row names as id for later merging
#  
# scot.par@data$id <- rownames(scot.par@data) #  create an id column for merging
# scot.par.xy <- fortify(scot.par, region = 'id') #  convert spatial dataframe into normal dataframe for ggplot2
# scot.par.xy <- scot.par.xy[order(scot.par.xy$order), ] #  order the new dataframe
# 
# # combine polygons data with number of infected locations and total number of locations
# scot.par.xy.merge <- merge(scot.par.xy, par.over, all.x = TRUE)
# scot.par.xy.merge <- merge(scot.par.xy.merge, counties.length, all.x = TRUE)
# 
# # scot.par.xy.merge[is.na(scot.par.xy.merge)] <- 0 #  replace all NAs with 0s
# 
# for(i in names(scot.par.xy.merge)[grepl('.total', names(scot.par.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
#   
#   if (substr(i, 1, 3) == 'Jun'){
#     
#     scot.par.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.par.xy.merge[[i]] - scot.par.xy.merge$Jun.observed #  find the change between the network and observed
#     scot.par.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.par.xy.merge[[i]] - scot.par.xy.merge$Jun.observed) / scot.par.xy.merge$Jun.observed #  find the percentage change
#     
#   } else {
#     
#     scot.par.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.par.xy.merge[[i]] - scot.par.xy.merge$Oct.observed #  find the change between the network and observed
#     scot.par.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.par.xy.merge[[i]] - scot.par.xy.merge$Oct.observed) / scot.par.xy.merge$Oct.observed #  find the percentage change
#     
#   }
# }
# 
# #~~~~~~~~~~~~~~~~~~~~~~~#
# #### ag county union ####
# #~~~~~~~~~~~~~~~~~~~~~~~#
# 
# county.id <- factor(master.key$county) #  factorise the list of counties
# scot.par.union <- unionSpatialPolygons(scot.par, county.id) #  combine the parishes into counties by the counties vector
# 
# test <- data.frame(getSpPPolygonsIDSlots(scot.par.union)) #  create a dataframe with ids from the counties spatial polygons
# 
# ### Warning message: use *apply and slot directly ###
# # seems to work ok
# 
# row.names(test) <- test$getSpPPolygonsIDSlots.scot.par.union #  add the rownames to new dataframe
# scot.par.union <- SpatialPolygonsDataFrame(Sr = scot.par.union, #  create a new spatial polygons data frame; needed for fortify
#                                            data = test)
# rm(test)
# 
# # county level fortify
# scot.par.union@data$id <- rownames(scot.par.union@data) #  create an id column for merging
# scot.par.union.xy <- fortify(scot.par.union, region = 'id') #  convert spatial dataframe into normal dataframe for ggplot2
# scot.par.union.xy <- scot.par.union.xy[order(scot.par.union.xy$order), ] #  order the new dataframe
# 
# par.over.union <- over(as(scot.par.union, 'SpatialPolygons'), summary.points, fn = sum) #  sum the number of infected farms by county
# par.over.union$id <- rownames(par.over.union) #  store row names as id for later merging
#  
# counties.length.union <- over(as(scot.par.union, 'SpatialPolygons'), summary.points[1], fn = length) #  count the number of infected farms by parish
# names(counties.length.union) <- c('num_farms') #  rename variable to avoid confusion
# counties.length.union$id <- rownames(counties.length.union) #  store row names as id for later merging
# 
# # combine polygons data with number of infected locations and total number of locations
# scot.par.union.xy.merge <- merge(scot.par.union.xy, par.over.union, all.x = TRUE)
# scot.par.union.xy.merge <- merge(scot.par.union.xy.merge, counties.length.union, all.x = TRUE)
# 
# # scot.par.union.xy.merge[is.na(scot.par.union.xy.merge)] <- 0 #  replace all NAs with 0s
# 
# for(i in names(scot.par.union.xy.merge)[grepl('.total', names(scot.par.union.xy.merge))]) { #  loop through the names of the columns storing the rewired network results
#   
#   if (substr(i, 1, 3) == 'Jun'){
#     
#     scot.par.union.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$Jun.observed #  find the change between the network and observed
#     scot.par.union.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$Jun.observed) / scot.par.union.xy.merge$Jun.observed #  find the percentage change
#     
#   } else {
#     
#     scot.par.union.xy.merge[[paste0('delta', substr(i, 1, nchar(i) - 6))]] <- scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$Oct.observed #  find the change between the network and observed
#     scot.par.union.xy.merge[[paste0('per.change', substr(i, 1, nchar(i) - 6))]] <- (scot.par.union.xy.merge[[i]] - scot.par.union.xy.merge$Oct.observed) / scot.par.union.xy.merge$Oct.observed #  find the percentage change
#     
#   }
# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### create choropleth maps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# generic theme to apply to all maps
map.theme <- theme(panel.background = element_rect(fill = "grey50"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.border = element_blank(),
                   legend.key.height = unit(1, "in"),
                   legend.title = element_text(face = 'bold'))

#~~~~~~~~~~~~~~~~~~~~~#
#### June maps: 13 ####
#~~~~~~~~~~~~~~~~~~~~~#

test <- lapply(scot.grid.xy.merge[,grep(pattern = 'deltaJun_13', names(scot.grid.xy.merge))], range, na.rm = TRUE) #  range of each
# test <- lapply(scot.par.xy.merge[,grep(pattern = 'deltaJun_13', names(scot.par.xy.merge))], range) #  range of each
# test <- lapply(scot.par.union.xy.merge[,grep(pattern = 'deltaJun_13', names(scot.par.union.xy.merge))], range) #  range of each
Jun.13.range <- c(ceiling(max(abs(range(unlist(test))))) * -1,
                  ceiling(max(abs(range(unlist(test))))))

png('Jun_13_plots.png', res = 300, height = 7, width = 12, units = 'in')
grid.arrange( #  for multiplot 
  ggplot() +
    geom_polygon(data=scot.grid.xy.merge, #  grid
                 aes(long, lat,
                     fill = Jun.observed,
                     group = group), 
                 colour = 'gray25') + #  draws borders for either parishes or counties
    geom_text(data=scot.grid.xy.merge,
              aes(label = 'June obs',  
                  x = min(scot.grid.xy.merge$long),
                  y = max(scot.grid.xy.merge$lat)),
              size = 16,
              vjust = "inward", hjust = "inward") +
    coord_equal() + map.theme +
    scale_fill_gradient2(#expression(paste("Infections per 10", km^{2})), 
      'Infections\n',
      low = 'blue4', midpoint = 0, high = 'red4', 
      limits = Jun.13.range,
      breaks = seq.int(round_any(Jun.13.range, 5)[1], round_any(Jun.13.range, 5)[2], 5)), #  creates flexible breaks using round_any in plyr, 
  arrangeGrob(
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, 
                   aes(long, lat, 
                       fill = deltaJun_13_FF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_FF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.13.range), 
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge,
                   aes(long, lat, 
                       fill = deltaJun_13_FT,
                       group = group),
                   colour = 'gray25') +
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_FT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.13.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, 
                   aes(long, lat, 
                       fill = deltaJun_13_TF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_TF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.13.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, 
                   aes(long, lat, 
                       fill = deltaJun_13_TT,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_TT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.13.range),
    ncol = 2), 
  ncol = 2, widths = c(1, 1))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~#
#### June maps: 6 ####
#~~~~~~~~~~~~~~~~~~~~~#

test <- lapply(scot.grid.xy.merge[,grep(pattern = 'deltaJun_6', names(scot.grid.xy.merge))], range, na.rm = TRUE) #  range of each
Jun.6.range <- c(ceiling(max(abs(range(unlist(test))))) * -1,
                 ceiling(max(abs(range(unlist(test))))))

png('Jun_6_plots.png', res = 300, height = 7, width = 12, units = 'in')
grid.arrange( #  for multiplot 
  ggplot() +
    geom_polygon(data=scot.grid.xy.merge, #  grid
                 aes(long, lat,
                     fill = Jun.observed,
                     group = group), 
                 colour = 'gray25') + #  draws borders for either parishes or counties
    geom_text(data=scot.grid.xy.merge,
              aes(label = 'June obs',  
                  x = min(scot.grid.xy.merge$long),
                  y = max(scot.grid.xy.merge$lat)),
              size = 16,
              vjust = "inward", hjust = "inward") +
    coord_equal() + map.theme +
    scale_fill_gradient2(#expression(paste("Infections per 10", km^{2})), 
      'Infections\n',
      low = 'blue4', midpoint = 0, high = 'red4', 
      limits = Jun.6.range, 
      breaks = seq.int(round_any(Jun.6.range, 5)[1], round_any(Jun.6.range, 5)[2], 5)), #  creates flexible breaks using round_any in plyr
  arrangeGrob(
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaJun_6_FF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_FF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.6.range), 
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaJun_6_FT,
                       group = group),
                   colour = 'gray25') +
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_FT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.6.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaJun_6_TF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_TF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.6.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaJun_6_TT,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_TT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Jun.6.range),
    ncol = 2), 
  ncol = 2, widths = c(1, 1))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~#
#### October maps: 13 ####
#~~~~~~~~~~~~~~~~~~~~~~~~#

test <- lapply(scot.grid.xy.merge[,grep(pattern = 'deltaOct_13', names(scot.grid.xy.merge))], range, na.rm = TRUE) #  range of each
Oct.13.range <- c(ceiling(max(abs(range(unlist(test))))) * -1,
                  ceiling(max(abs(range(unlist(test))))))

png('Oct_13_plots.png', res = 300, height = 7, width = 12, units = 'in')
grid.arrange( #  for multiplot 
  ggplot() +
    geom_polygon(data=scot.grid.xy.merge, #  grid
                 aes(long, lat,
                     fill = Oct.observed,
                     group = group), 
                 colour = 'gray25') + #  draws borders for either parishes or counties
    geom_text(data=scot.grid.xy.merge,
              aes(label = 'Oct obs',  
                  x = min(scot.grid.xy.merge$long),
                  y = max(scot.grid.xy.merge$lat)),
              size = 16,
              vjust = "inward", hjust = "inward") +
    coord_equal() + map.theme +
    scale_fill_gradient2(#expression(paste("Infections per 10", km^{2})), 
      'Infections\n',
      low = 'blue4', midpoint = 0, high = 'red4', 
      limits = Oct.13.range,
      breaks = seq.int(round_any(Oct.13.range, 5)[1], round_any(Oct.13.range, 5)[2], 5)), #  creates flexible breaks using round_any in plyr, 
  arrangeGrob(
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_13_FF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_FF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.13.range), 
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_13_FT,
                       group = group),
                   colour = 'gray25') +
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_FT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.13.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_13_TF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_TF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.13.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_13_TT,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '13_TT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.13.range),
    ncol = 2), 
  ncol = 2, widths = c(1, 1))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~#
#### October maps: 6 ####
#~~~~~~~~~~~~~~~~~~~~~~~#

test <- lapply(scot.grid.xy.merge[,grep(pattern = 'deltaOct_6', names(scot.grid.xy.merge))], range, na.rm = TRUE) #  range of each
Oct.6.range <- c(ceiling(max(abs(range(unlist(test))))) * -1,
                 ceiling(max(abs(range(unlist(test))))))

png('Oct_6_plots.png', res = 300, height = 7, width = 12, units = 'in')
grid.arrange( #  for multiplot 
  ggplot() +
    geom_polygon(data=scot.grid.xy.merge, #  grid
                 aes(long, lat,
                     fill = Oct.observed,
                     group = group), 
                 colour = 'gray25') + #  draws borders for either parishes or counties
    geom_text(data=scot.grid.xy.merge,
              aes(label = 'Oct obs',  
                  x = min(scot.grid.xy.merge$long),
                  y = max(scot.grid.xy.merge$lat)),
              size = 16,
              vjust = "inward", hjust = "inward") +
    coord_equal() + map.theme +
    scale_fill_gradient2(#expression(paste("Infections per 10", km^{2})), 
      'Infections\n',
      low = 'blue4', midpoint = 0, high = 'red4', 
      limits = Oct.6.range, 
      breaks = seq.int(-200, 200, by = 50)), #  creates flexible breaks using round_any in plyr
  arrangeGrob(
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_6_FF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_FF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.6.range), 
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_6_FT,
                       group = group),
                   colour = 'gray25') +
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_FT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.6.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_6_TF,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_TF',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE,
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.6.range),
    ggplot() +
      geom_polygon(data=scot.grid.xy.merge, #  grid
                   aes(long, lat, 
                       fill = deltaOct_6_TT,
                       group = group),
                   colour = 'gray25') + 
      geom_text(data=scot.grid.xy.merge,
                aes(label = '6_TT',  
                    x = min(scot.grid.xy.merge$long),
                    y = max(scot.grid.xy.merge$lat)),
                size = 16,
                vjust = "inward", hjust = "inward") +
      coord_equal() + map.theme +
      scale_fill_gradient2(guide=FALSE, 
                           low = 'blue4', midpoint = 0, high = 'red4', limits = Oct.6.range),
    ncol = 2), 
  ncol = 2, widths = c(1, 1))
dev.off()