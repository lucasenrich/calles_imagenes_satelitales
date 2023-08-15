library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(sf)
library(arrow)
library(geojsonio)
library(leaflet)

par(mfrow = c(6, 6))

asfalto = readOGR('./muestra_calles_brown/muestra_calles_asfalto.shp')
tierra = readOGR('./muestra_calles_brown/muestra_calles_tierra.shp')
vias = readOGR('../vias/vias.shp')#INDEC



asfalto <- spTransform(asfalto, CRSobj = proj4string(predicciones))
tierra <- spTransform(tierra, CRSobj = proj4string(predicciones))
p = 'C:/Users/HP/OneDrive/RENAPER/CALLES/almte_brown/almte_brown/rasters/'
paths = paste0(p,list.files(p))
dft = data.frame(x=as.numeric(), y=as.numeric(),
           r1 = as.numeric(),r2 = as.numeric(),
           r3 = as.numeric(),r4 = as.numeric(),
           r5 = as.numeric(), target = as.numeric()) %>% as_tibble()
for(i in 1:length(paths)){#
  print(i/length(paths))
  rs = stack(paths[i])
  tierra_cropped = crop(tierra, extent(rs))
  if(length(tierra_cropped)>0){
    r_filtered <- mask(rs, tierra_cropped)
    df1 = r_filtered %>% as.data.frame(xy = T) %>% as_tibble() %>% drop_na()
    df1 = df1 %>% distinct()
    df1['target'] = 1
    colnames(df1) = colnames(dft)
    dft = rbind(dft,df1)
    write_parquet(dft, 'tierra.parquet')
  }
}
dft = read_parquet('tierra.parquet')
dft = data.frame(x=as.numeric(), y=as.numeric(),
                 r1 = as.numeric(),r2 = as.numeric(),
                 r3 = as.numeric(),r4 = as.numeric(),
                 r5 = as.numeric(), target = as.numeric()) %>% as_tibble()
for(i in 1:length(paths)){#
  print(i/length(paths))
  rs = stack(paths[i])
  asfalto_cropped = crop(asfalto, extent(rs))
  if(length(asfalto_cropped)>0){
    r_filtered <- mask(rs, asfalto_cropped)
    df1 = r_filtered %>% as.data.frame(xy = T) %>% as_tibble() %>% drop_na()
    df1 = df1 %>% distinct()
    df1['target'] = 0
    colnames(df1) = colnames(dft)
    dft = rbind(dft,df1)
    write_parquet(dft, 'asfalto.parquet')
  }
}

asfalt_df =  read_parquet('asfalto.parquet')
tierra_df =  read_parquet('tierra.parquet')
df = rbind(asfalt_df,tierra_df)  %>% as_tibble() %>% sample_n(100000)#
library(randomForest)
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ] #%>% sample_n(100000)
test_data <- df[-train_indices, ]
train_data$target = train_data$target %>% as.factor()
# Train a random forest model
rf_model <- randomForest(target ~ ., data = train_data)


predictions <- predict(rf_model, newdata = test_data)
table(predictions, test_data$target)
# Specify the file path where you want to save the model
model_file_path <- "rfmodel.rds"
# Save the randomForest model as an RDS file
saveRDS(rf_model, file = model_file_path)
loaded_rf_model <- readRDS("rfmodel.rds")

vias = readOGR('../vias/vias.shp')#INDEC
brown = readOGR('../almte_brown/almte_brown/almirante_brown.geojson')
res = data.frame(x=as.numeric(),y=as.numeric(),tpred=as.numeric())
for(i in 1:length(paths)){#length(paths)
  print(i/length(paths))
  sat_test = stack(paths[i])
  extent_polygon <- as(extent(sat_test), "SpatialPolygons")
  vias_test <- SpatialPolygonsDataFrame(extent_polygon, data = data.frame(ID = 1))
  crs(vias_test) = crs(vias)
  cropped<-gIntersection(vias_test,vias ,byid=T)
  
  r_filtered_test <- mask(sat_test, cropped)
  df_test = r_filtered_test %>% as.data.frame(xy= T) %>% as_tibble() %>% drop_na()
  
  colnames(df_test)[3:7] = c('r1','r2','r3','r4','r5')
  
  predictions <- predict(loaded_rf_model, newdata = df_test, "prob")
  predictions = predictions %>% as_tibble()
  df_test['tpred'] = as.vector(unlist(predictions[,2]))
  df_test = df_test %>% dplyr::select(x,y,tpred)
  res = rbind(res,df_test)
  write_parquet(res, 'res.parquet')
}
cropped<-gIntersection(brown,vias ,byid=T)
cropped = st_as_sf(cropped)


res = read_parquet('res.parquet')# %>% sample_n(100000)
coordinates(res) <- ~x+y
crs(res) = crs(brown)
crs(crop_extent) = crs(brown)
res = res[crop_extent,]


res_sf <- st_as_sf(
  res,
  coords = c("Longitude", "Latitude"), # must be x, y order
  crs = st_crs(brown) # must be equivilant between objects
)
nf = st_nearest_feature(res_sf, cropped)
cropped['idx'] = 1:1381
dfg = data.frame(idx=nf, predp=res_sf$tpred)
i = sort(unique(nf),decreasing = T)[1]
dfg1 = left_join(dfg,cropped)


dfg2 = dfg1 %>% group_by(idx) %>% summarise(pred_max = max(predp,na.rm=T),
                                            pred_mean = mean(predp,na.rm=T),
                                            predp_sd = sd(predp,na.rm=T))
dfg3 = left_join(cropped,dfg2)
geojson1 <- geojson_json(dfg3)
fname = './prediccion_brown_v3.geojson'
write(geojson1, file = fname)
gc()
