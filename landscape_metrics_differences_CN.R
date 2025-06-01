# R script to test landscape metrics differences using different Cell Neighbourhood (CN)
# Copyright (C) 2025 Paolo Zatelli and Stefano Gobbi
# University of Trento, Italys
# paolo.zatelli@unitn.it

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.

library(raster)
library(tiff)
library(landscapemetrics)
library(tibble)
library(rgdal) 
library(tidyverse)

# read the list of the files (maps)
setwd("/home/paolo/misc/indici_paesaggio_2/r/ai/")
all.the.files <- list.files('fassa',full=TRUE)

names_files <- list.files('fassa',full=TRUE)
titles_maps <- list.files('fassa',full=FALSE)

#############################
# split metrics into 2 different sets to remove metrics requiring too much RAM
x<-c('lsm_c_ai','lsm_c_area_mn','lsm_c_cai_mn','lsm_c_contig_mn','lsm_c_dcore_mn','lsm_c_division','lsm_c_enn_mn','lsm_c_gyrate_mn','lsm_c_lpi','lsm_c_mesh','lsm_c_ndca', 'lsm_c_np','lsm_c_pd','lsm_c_split')

# NOT used because they overflow available RAM
# 'lsm_c_cohesion','lsm_c_frac_mn','lsm_c_pafrac','lsm_c_para_mn','lsm_c_shape_mn'

#############################

metriche_4 <- data.frame(a=double(length(x)))
metriche_4$a <- NULL                                  # new data frame for CN=4

metriche_8 <- data.frame(a=double(length(x)))
metriche_8$a <- NULL                                  # new data frame for CN=8

# evaluate the statiscs
for (name in c(1:length (names_files))){  
  
  temp<-raster(names_files[name])

  print(paste0("Map: ", names_files[name]))

# CN = 4
  metric_temp_4<-calculate_lsm(
    temp,
    what = x,
    directions = 4,
    # count_boundary = false,
    # consider_boundary = false,
    # edge_depth = 1,
     # cell_center = false,
    neighbourhood = 4,
    # ordered = true,
    # base = "log2",
    # full_name = false,
    # verbose = true,
     progress = TRUE
  )

  y_4<-filter(metric_temp_4, class=='1')
  y_4[c("value")] ->tmp
  cbind(metriche_4, tmp)->metriche_4

# CN = 8
  metric_temp_8<-calculate_lsm(
    temp,
    what = x,
    directions = 8,
    # count_boundary = false,
    # consider_boundary = false,
     # edge_depth = 1,
     # cell_center = false,
     neighbourhood = 8,
     # ordered = true,
     # base = "log2",
     # full_name = false,
     # verbose = true,
      progress = TRUE
   )
  
  y_8<-filter(metric_temp_8, class==1)
  y_8[c("value")] ->tmp_8
  cbind(metriche_8, tmp_8)->metriche_8  

}   

# evaluate metrics differences (CN=8 - CN=4)
row.names(metriche_4)<-y_4$metric
names(metriche_4)<-titles_maps

row.names(metriche_8)<-y_4$metric
names(metriche_8)<-titles_maps

new_df_metriche_4<-t(metriche_4)
new_df_metriche_8<-t(metriche_8)

diff_metriche<-new_df_metriche_8-new_df_metriche_4
diff_metriche_perc<-diff_metriche/pmax(new_df_metriche_4, new_df_metriche_8)  
diff_metriche_perc4<-diff_metriche/new_df_metriche_4

names(diff_metriche)<-metric_temp_4$metric
row.names(diff_metriche)<-titles_maps

# transform vectors in dataframe for ggplot2
grafico <- data.frame(diff_metriche)

# graphics
# CN = 4
metriche_4_ai_df <- data.frame(new_df_metriche_4)
print(paste0("############ CN=4: "))
grafico$ai=metriche_4_ai_df$ai
print(grafico)

# CN = 8
metriche_8_ai_df <- data.frame(new_df_metriche_8)
print(paste0("############ CN=8: "))
grafico$ai=metriche_8_ai_df$ai
print(grafico)

nomi_metriche=metric_temp_4$metric
print(nomi_metriche)

# polynomial approximation
library(ggplot2)
library(reshape2)
library(polynom)

colNames <- names(grafico)[2:length(nomi_metriche)]

print(colNames)

for (name in colNames){
  print(paste0("Metrica: ", name))
  
    plt <- ggplot(grafico, aes_string(x="ai", y=name))+ geom_point(aes_string(x="ai", y=name)) + geom_smooth(aes_string(x="ai", y=name))
}

polin<-array(13)

for (i in c(1:13)){  
z<-poly.calc(grafico$ai, grafico[,c(i)])
polin[i]<-as.character(z)
}

# save into an array (polin)
print(polin)

colNames <- names(grafico)[2:length(nomi_metriche)]
print(paste0("############ Colonne: ", colNames))

setwd("/home/paolo/misc/indici_paesaggio_2/r/ai/")

for (i in c(1:14)){
NomeMetrica <- names(grafico)[i]
print(paste0("############ Metrica: ", NomeMetrica))

grado=6
print(paste0("############ Grado polinomio: ", grado))


  print("grafico$ai")
  print(grafico$ai)
  print("grafico[,c(i)]")
  print(grafico[,c(i)])

  model <- lm(grafico[,c(i)] ~ poly(grafico$ai,grado))

  # statistics on stdout
  print("summary(model)")
  print(summary(model))
  print("confint(model, level=0.95)")
  print(confint(model, level=0.95))

  # print the coefficients
  model_coeff <- lm(grafico[,c(i)] ~ poly(grafico$ai,grado, raw = TRUE))
  print(coef(summary(model_coeff)))

# Get min/max values of age using the range() function
ailims = grafico$ai
    range

ai_grid = seq(from = 10, to = 90, by= 10)
sample_points = seq(from = 10, to = 90, by= 10)


# prediction model
x <- grafico$ai
y <- grafico[,c(i)]
dataTest <- data.frame(x, y)

# polynom degree
m <- lm(y ~ poly(x, grado, raw=TRUE), data=dataTest)

pred_orig=predict(m,type="response")

# add sampling points to the dataframe
nrighe=nrow(dataTest)

x <- ai_grid
y <- ai_grid
dataPredict <- data.frame(x, y)
pred_m = predict(m,newdata=dataPredict,type="response")

print("summary(m)")
print(summary(m))
print("pred_orig")
print(pred_orig)
print("pred_m")
print(pred_m)

ai = grafico[["ai"]]

fit = lm(grafico[,c(i)] ~ poly(grafico$ai, grado))
  print("summary(fit)")
  print(summary(fit))

print("Predictions")
preds = predict(fit, newdata = list(ai_grid), se = TRUE)

new_points <- data.frame('ai'=c(10.0,20.0,30.0,40.0))
preds_new = predict(fit, new_points)

preds_coeff = predict(model_coeff, newdata = list(sample_points))

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit,
                 "lower" = preds$fit-2*preds$se.fit)
print("ai_grid")
print(ai_grid)

print("preds_new")
print(preds_new)

print("preds$fit")
print(preds$fit)
print("preds_coeff")
print(preds_coeff)

# create raphs PDFs
Titolo=print(paste0("Degree-",grado," Polynomial: ", NomeMetrica))
print("Titolo")
print(Titolo)

pdf_name=paste("/home/paolo/misc/indici_paesaggio_2/r/diff_metriche/interpolazione_",NomeMetrica,".pdf",sep="")
print(pdf_name)

pdf(pdf_name,width=5.83,height=4.13,paper="special")
plt <- ggplot(grafico, aes_string(x="ai", y=NomeMetrica)) + geom_point(aes_string(x=grafico$ai, y=grafico[,c(i)])) +
  geom_line(aes(x = grafico$ai, y = preds$fit), color = "#0000FF") +
  geom_ribbon(aes(x = grafico$ai,
                  ymin = se_bands[,"lower"],
                  ymax = se_bands[,"upper"]),
              alpha = 0.3) +
  labs(title = Titolo)
print(plt)

} # loop ane

dev.off()
stop("Fine")
