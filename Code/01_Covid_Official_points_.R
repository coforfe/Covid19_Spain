#----------------------------
# Author:     Carlos Ortega
# Date:       2020-03-30
# Purpose:    COVID-19 - Spain - Analysis
# Input:      Official Daily Data by Autonoy.
# Ouput:      Different output charts. 
#----------------------------

library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggeasy)
library(ggpubr)
library(statebins)
library(tictoc)
library(zoo)

tic()
rm(list = ls())

# Get Daily file
url_data <- c('https://covid19.isciii.es/resources/serie_historica_acumulados.csv')
datRaw <- readLines(con <- file(url_data))
close(con)
datRaw <- datRaw[1:(length(datRaw) - 1)]
tail(datRaw)

# Save it conveniently
val_time     <- Sys.time()
anmedi_val   <- paste(year(val_time),month(val_time), day(val_time), sep = "")
my_title     <- paste("./data/",anmedi_val, "_serie_historica_acumulados.csv", sep = "")
write.table(
  datRaw, 
  file = my_title,
  row.names = FALSE, quote = FALSE, col.names = FALSE
)


# Read Saved file
datIn <- fread(my_title)
names(datIn)[1] <- c('CCAA')
datIn[ , fecha := dmy(Fecha)]
datIn[, Fecha := NULL]


#-------- Create New Columns --------------------------------
#Lag
my_cols <- c('Casos', 'Hospitalizados', 'UCI', 'Fallecidos')
anscols = paste("lag", my_cols, sep = "_")
datIn[, (anscols) := shift(.SD, 1, 0, "lag"), .SDcols = my_cols, by = .(CCAA)]

# Dif - Lag + Percentage
datIn[ , dif_Casos := Casos - lag_Casos, by = .(CCAA)]
datIn[ , per_Casos := round(100*dif_Casos/Casos, 2) , by = .(CCAA)]

datIn[ , dif_Hospi := Hospitalizados - lag_Hospitalizados, by = .(CCAA)]
datIn[ , per_Hospi := round(100*dif_Hospi/Hospitalizados, 2) , by = .(CCAA)]

datIn[ , dif_UCI   := UCI - lag_UCI, by = .(CCAA)]
datIn[ , per_UCI   := round(100*dif_UCI/UCI, 2) , by = .(CCAA)]

datIn[ , dif_Falle := Fallecidos - lag_Fallecidos, by = .(CCAA)]
datIn[ , per_Falle := round(100*dif_Falle/Fallecidos, 2) , by = .(CCAA)]


#----------------------
# Totales
datIn[ , tot_Casos := sum(Casos), by = .(fecha)]
datIn[ , tot_Falle := sum(Fallecidos) , by = .(fecha)]
datIn[ , tot_UCI   := sum(UCI), by = .(fecha)]
datIn[ , tot_Hospi := sum(Hospitalizados), by = .(fecha)]
datTot <- unique(datIn[ , .(tot_Casos, tot_Falle, tot_UCI, tot_Hospi), by = .(fecha)])

my_colstot <- c('tot_Casos', 'tot_Hospi', 'tot_UCI', 'tot_Falle')
anscols = paste("lag", my_colstot, sep = "")
datTot[ , (anscols) := shift(.SD, 1, 0, "lag"), .SDcols = my_colstot]

# Dif - Lag + Percentage
datTot[ , dif_Casos := tot_Casos - lagtot_Casos ]
datTot[ , per_Casos := round(100*dif_Casos/tot_Casos, 2) ]

datTot[ , dif_Hospi := tot_Hospi - lagtot_Hospi ]
datTot[ , per_Hospi := round(100*dif_Hospi/tot_Hospi, 2) ]

datTot[ , dif_UCI   := tot_UCI - lagtot_UCI]
datTot[ , per_UCI   := round(100*dif_UCI/tot_UCI, 2) ]

datTot[ , dif_Falle := tot_Falle - lagtot_Falle]
datTot[ , per_Falle := round(100*dif_Falle/tot_Falle, 2) ]






#---------------------- CHATRTS 
# ------- Lag
var_ori <- c('Casos', 'Hospitalizados', 'UCI', 'Fallecidos')
var_per <- c('per_Casos', 'per_Hospi', 'per_UCI', 'per_Falle')
var_dif <- c('dif_Casos', 'dif_Hospi', 'dif_UCI', 'dif_Falle')


ftit_ori <- function(colum) {
  tit_ori <- str_to_upper(colum)
  return(tit_ori)
}

ftit_lag <- function(colum) {
  tit_lag <- paste( str_to_upper(str_replace(colum, 'lag_', '')),
                    " - Diferencia día anterior", sep = "")
  return(tit_lag)
}

ftit_per <- function(colum) {
  tit_per <- paste( str_to_upper(str_replace(colum, 'per_', '')), 
                    " - Diferencia día anterior - Porcentaje", sep = "")
  return(tit_per)
}

ftit_tot <- function(colum) {
  tit_tot <- str_to_upper(colum)
  return(tit_tot)
}


f_gr <- function(df, colum, my_tit) {
  gg <- ggplot(df,  aes(x  = fecha,  y  = .data[[colum]]) ) + 
    #geom_col( position = 'dodge') +
    geom_line( colour = 'grey', alpha = 0.25) +
    geom_point(size = 0.5) +
    facet_wrap( ~ CCAA, scales = 'free') +
    ggtitle( my_tit, subtitle = url_data ) +
    xlab('Fecha') + ylab('# Personas') +
    theme_bw(base_size = 9) +
    theme(
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
  return(gg)
}

f_gr_one <- function(df, colum, my_tit) {
  gg <- ggplot(df,  aes(x  = fecha,  y  = .data[[colum]]) ) + 
    # geom_col( position = 'dodge') +
    geom_line( colour = 'grey', alpha = 0.25) +
    geom_point(size = 2) +
    # rolling average 7.
    geom_line(aes(y = rollmean(.data[[colum]], 5, na.pad = TRUE)), 
                  colour = 'tomato', size = 1) +
    ggtitle( paste('MADRID - ',my_tit, sep = ""), subtitle = url_data ) +
    xlab('Fecha') + ylab('Valor') +
    theme_bw()
  return(gg)
}


f_gr_tot <- function(df, colum, my_tit) {
  gg <- ggplot(df,  aes(x  = fecha,  y  = .data[[colum]]) ) + 
    # geom_col(position = 'dodge') +
    geom_line( colour = 'grey', alpha = 0.25) +
    geom_point(size = 2) +
    geom_line(aes(y = rollmean(.data[[colum]], 5, na.pad = TRUE)), 
              colour = 'tomato', size = 1) +
    ggtitle( my_tit, subtitle = "TOT:totales - DIF:Diferencia dia anterior - PER:Porcentaje diferencia dia anterior" ) +
    xlab('Fecha') + ylab('Valor') +
    theme_bw()
  return(gg)
}


#------------------ GRAFICOS -----------
#---- Totales
ncol_tot <- names(datTot)[str_detect(names(datTot), "^tot|^dif|^per")]
for (i_var in ncol_tot) {
  my_tit <- ftit_tot(i_var)
  gr_tot <- f_gr_tot(datTot, i_var, my_tit)
  print(gr_tot)
  ggsave(paste("./charts/Plot_totales_", i_var, ".png", sep = ""))
}


#---- Originales
for (i_var in var_ori) {
  my_tit <- ftit_ori(i_var)
  gr_ori <- f_gr(datIn, i_var, my_tit)
  print(gr_ori)
  ggsave(paste("./charts/Plot_Origi_", i_var, ".png", sep = ""))
  #--- Madrid
  gr_ori_mad <- f_gr_one(datIn[ CCAA == 'MD',], i_var, my_tit)
  print(gr_ori_mad)
  ggsave(paste("./charts/Plot_Origi_Madrid_", i_var, ".png", sep = ""))
}

#---- Lag
for (i_var in var_dif) {
  my_tit <- ftit_lag(i_var)
  gr_lag <- f_gr(datIn, i_var, my_tit)
  print(gr_lag)
  ggsave(paste("./charts/Plot_Lag_", i_var, ".png", sep = ""))
  #--- Madrid
  gr_lad_mad <- f_gr_one(datIn[ CCAA == 'MD',], i_var, my_tit)
  print(gr_lag_mad)
  ggsave(paste("./charts/Plot_Lag_Madrid_", i_var, ".png", sep = ""))
}

#---- Per
for (i_var in var_per) {
  my_tit <- ftit_per(i_var)
  gr_per <- f_gr(datIn, i_var, my_tit)
  print(gr_per)
  ggsave(paste("./charts/Plot_Per_", i_var, ".png", sep = ""))
  #--- Madrid
  gr_per_mad <- f_gr_one(datIn[ CCAA == 'MD',], i_var, my_tit)
  print(gr_per_mad)
  ggsave(paste("./charts/Plot_Per_Madrid_", i_var, ".png", sep = ""))
}

#------------  Casos vs. Fallecidos
datCaFa <- datIn[ , .(fecha, Casos, Fallecidos, CCAA)]
datCaFa_lg <- melt(datCaFa, id.vars = c('fecha', 'CCAA'), measure.vars = c('Casos', 'Fallecidos'))

#----- Todas CCAA
ggplot( datCaFa_lg, aes( x = fecha, y = value, group = variable, fill = variable)) +
  # geom_point( ) +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 0.5) +
  facet_wrap( ~ CCAA, scales = 'free') +
  ggtitle( 'Casos y Fallecidos por Comunidad', subtitle = url_data ) +
  xlab('Fecha') + ylab('# Personas') +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5)
  )
  # theme_bw()
ggsave(paste("./charts/Plot_CaFa_all_.png", sep = ""))

#---- Madrid
ggplot( datCaFa_lg[ CCAA == 'MD', ], aes( x = fecha, y = value, group = variable, fill = variable)) +
  # geom_col( position = 'dodge' ) +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 1) +
  ggtitle( 'Casos y Fallecidos - Madrid', subtitle = url_data ) +
  xlab('Fecha') + ylab('# Personas') +
  theme_bw()
ggsave(paste("./charts/Plot_CaFa_Madrid_.png", sep = ""))

#------------  Hospitalizados, Casos vs. Fallecidos
datHoFaCa <- datIn[ , .(fecha, Hospitalizados, Fallecidos, Casos, CCAA)]
datHoFaCa_lg <- melt(datHoFaCa, id.vars = c('fecha', 'CCAA'), measure.vars = c('Hospitalizados', 'Fallecidos', 'Casos'))

#----- Todas CCAA
ggplot( datHoFaCa_lg, aes( x = fecha, y = value, group = variable, fill = variable)) +
  # geom_point( ) +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 0.5) +
  facet_wrap( ~ CCAA, scales = 'free') +
  ggtitle( 'Hospitalizados, Fallecidos y Casos por Comunidad', subtitle = url_data ) +
  xlab('Fecha') + ylab('# Personas') +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5)
  )
  # theme_bw()
ggsave(paste("./charts/Plot_HoFaCa_all_.png", sep = ""))

#---- Madrid
ggplot( datHoFaCa_lg[ CCAA == 'MD', ], aes( x = fecha, y = value, group = variable, fill = variable)) +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 1) +
  # geom_col( position = 'dodge' ) +
  ggtitle( 'Hospitalizados, Fallecidos y Casos - Madrid', subtitle = url_data ) +
  xlab('Fecha') + ylab('# Personas') +
  theme_bw()
ggsave(paste("./charts/Plot_HoFaCa_Madrid_.png", sep = ""))

#---- Madrid Log
library(zoo)
ggplot( datHoFaCa_lg[ CCAA == 'MD', ], 
        aes( x = fecha, y = log(value),  
             group = variable, fill = variable,
             col = variable)) +
  # geom_point() +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 1) +
  geom_line(aes( y = rollmean(log(value), 7, na.pad=TRUE), 
                group = variable, fill = variable, sie = 1)) +
  ggtitle( 'Hospitalizados, Fallecidos y Casos - Madrid - (log)', subtitle = url_data ) +
  xlab('Fecha') + ylab('log(# Personas)') +
  theme_bw()
ggsave(paste("./charts/Plot_HoFa_Madrid_log_.png", sep = ""))  


#--------------------------------------
#---------- FORECAST -----------
library(forecast)
library(tsibble)
library(fpp3)
#--- MD
datHoMd <- datIn[ CCAA == 'MD', .(fecha, Hospitalizados)]
HoMd_ts <- tsibble( value = datHoMd$Hospitalizados, date = ymd(datHoMd$fecha))
#-- Evolution
HoMd_ts %>% autoplot(value) +
  ggtitle("Hopitalizados - Madrid") +
  ylab("# Personas") + xlab("Dia")
ggsave(paste("./charts/Plot_TimeSerie_Ho_Madrid_.png", sep = ""))

#-- Lag plots
HoMd_ts %>% gg_lag(value, geom = 'point') +
  ggtitle("Hopitalizados - Madrid") +
  ylab("# Personas") + xlab("Dia")
ggsave(paste("./charts/Plot_Lag_Hosp_Madrid_.png", sep = ""))

HoMd_ts %>% gg_tsdisplay(difference(log(value), 1), plot_type = 'partial')
ggsave(paste("./charts/Plot_dif_Hosp_Madrid_.png", sep = ""))

#-- Forecast - Remove inital 0s
fit <- HoMd_ts %>%
  filter( value > 0) %>%
  model(ARIMA(log(value) ~ PDQ(0,0,0)))
report(fit)
fit %>% forecast(h = 10) %>% autoplot(HoMd_ts)
ggsave(paste("./charts/Plot_Forec_Stocast_Hosp_Madrid_.png", sep = ""))

#--- Stochastic forecast
fit_stochastic <- HoMd_ts %>%
  model(ARIMA(value ~ pdq(d=1)))
report(fit_stochastic)
fit_stochastic %>% forecast(h = 10) %>% autoplot(HoMd_ts)

toc()
