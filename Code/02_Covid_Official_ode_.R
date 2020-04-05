#----------------------------
# Author:     Carlos Ortega
# Date:       2020-03-30
# Purpose:    COVID-19 - Spain - Analysis
# Input:      Official Daily Data by Autonoy.
# Ouput:      Different output charts. 
#----------------------------

suppressPackageStartupMessages({
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
  library(deSolve)
})

tic()
rm(list = ls())

# Get Daily file
url_data <- c('https://covid19.isciii.es/resources/serie_historica_acumulados.csv')
datRaw <- readLines(con <- file(url_data))
close(con)
datRaw <- datRaw[1:(length(datRaw) - 2)] # Han incluido Prevalencias...
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
my_cols <- c('Casos', 'Hospitalizados', 'UCI', 'Fallecidos', 'Recuperados')
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

datIn[ , dif_Recu  := Recuperados - lag_Recuperados, by = .(CCAA)]
datIn[ , per_Recu  := round(100*dif_Recu/Recuperados, 2) , by = .(CCAA)]

#----------------------
# Totales
datIn[ , tot_Casos := sum(Casos), by = .(fecha)]
datIn[ , tot_Falle := sum(Fallecidos) , by = .(fecha)]
datIn[ , tot_UCI   := sum(UCI), by = .(fecha)]
datIn[ , tot_Hospi := sum(Hospitalizados), by = .(fecha)]
datIn[ , tot_Recu  := sum(Recuperados), by = .(fecha)]
datTot <- unique(datIn[ , .(tot_Casos, tot_Falle, tot_UCI, tot_Hospi, tot_Recu), by = .(fecha)])

my_colstot <- c('tot_Casos', 'tot_Hospi', 'tot_UCI', 'tot_Falle', 'tot_Recu')
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

datTot[ , dif_Recu  := tot_Recu - lagtot_Recu]
datTot[ , per_Recu  := round(100*dif_Recu/tot_Recu, 2) ]




#---------------------- CHATRTS 
# ------- Lag
var_ori <- c('Casos', 'Hospitalizados', 'UCI', 'Fallecidos','Recuperados')
var_per <- c('per_Casos', 'per_Hospi', 'per_UCI', 'per_Falle', 'per_Recu')
var_dif <- c('dif_Casos', 'dif_Hospi', 'dif_UCI', 'dif_Falle', 'dif_Recu')


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
datCaFa <- datIn[ , .(fecha, Casos, Fallecidos, Recuperados, CCAA)]
datCaFa_lg <- melt(datCaFa, id.vars = c('fecha', 'CCAA'), measure.vars = c('Casos', 'Fallecidos', 'Recuperados'))

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
datHoFaCa <- datIn[ , .(fecha, Hospitalizados, Fallecidos, Casos, Recuperados, CCAA)]
datHoFaCa_lg <- melt(datHoFaCa, id.vars = c('fecha', 'CCAA'), measure.vars = c('Hospitalizados', 'Fallecidos', 'Casos', 'Recuperados'))

#----- Todas CCAA
ggplot( datHoFaCa_lg, aes( x = fecha, y = value, group = variable, fill = variable)) +
  # geom_point( ) +
  geom_line( aes (colour = variable), alpha = 0.25) +
  geom_point(aes(colour = variable), size = 0.5) +
  facet_wrap( ~ CCAA, scales = 'free') +
  ggtitle( 'Hospitalizados, Fallecidos, Recuperados y Casos por Comunidad', subtitle = url_data ) +
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
  ggtitle( 'Hospitalizados, Fallecidos, Recuperados y Casos - Madrid', subtitle = url_data ) +
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
  ggtitle( 'Hospitalizados, Fallecidos, Recuperados y Casos - Madrid - (log)', subtitle = url_data ) +
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


#--------------------------------------
#---------- ODE SOLVE -----------
# https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
# https://blog.ephorie.de/contagiousness-of-covid-19-part-i-improvements-of-mathematical-fitting-guest-post

datCasos_Md <- datIn[ CCAA == 'MD', .(fecha, Casos)]
Casos_Md <- datCasos_Md$Casos[!is.na(datCasos_Md$Casos)]

Infected <- Casos_Md
# Infected <- Casos_Md[1:20]
Day <- 1:(length(Infected))
N <- 3266126 # Madrid Population : 3266126 (Madrid)

# Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711, 9692, 11791, 14380, 17205, 20440)
# Day <- 1:(length(Infected))
# N <- 1400000000 # population of mainland china

# old <- par(mfrow = c(1, 2))
# plot(Day, Infected, type ="b")
# plot(Day, Infected, log = "y")
# abline(lm(log10(Infected) ~ Day))
# title("Confirmed Cases 2019-nCoV Madrid", outer = TRUE, line = -2)

DayInfec_df <- data.frame(
  Day = Day,
  Infected = Infected,
  logInfec = log10(Infected)
)

gr_a <- ggplot(DayInfec_df, aes( x = Day, y = Infected)) +
          geom_line( colour = 'grey', alpha = 0.25) +
          geom_point(size = 2) +
          ggtitle("Casos - Madrid") +
          ylab("# Infectados") + xlab("Dia") +
          theme_bw()

gr_b <- ggplot(DayInfec_df, aes( x = Day, y = logInfec)) +
          geom_point(size = 2) +
          geom_smooth(method = lm, se = FALSE) +
          ggtitle("Casos (log) - Madrid") +
          ylab("# Infectados") + xlab("Dia") + 
          theme_bw()
gr_a + gr_b  + 
  plot_annotation(title = 'Casos Confirmados Cov-19 - Madrid',
                  theme = theme(plot.title = element_text(size = 18)))

ggsave(paste("./charts/ODE_Cases_Madrid_.png", sep = ""))

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N - Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
# [1] "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH"..... :-(

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
# beta     gamma 
# 1.0000000 0.7127451 

t <- 1:80 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

# matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
# matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
# ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
# ## omitted from logarithmic plot
# 
# points(Day, Infected)
# legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
# par(old)

R0 <- paste("R0: ", round(setNames(Opt_par["beta"] / Opt_par["gamma"], "R0"),4), sep = "")
max_infe <- fit[fit$I == max(fit$I), "I", drop = FALSE]
val_max  <- data.frame(Day = as.numeric(rownames(max_infe)) , 
                       Value = max_infe$I )
val_lab  <- paste('Pico_Infectados:\n', as.character(round(val_max$Value),0))
dea_lab  <- paste('Muertes (2%):\n', as.character(round(val_max$Value * 0.02,0)) )

fit_dt   <- as.data.table(fit)
fit_tidy <- melt(fit_dt, id.vars = c('time') , measure.vars = c('S', 'I','R') )
fit_gd   <- merge(fit_dt, DayInfec_df[, c(1:2)], 
                  by.x = c('time'), by.y = c('Day'), 
                  all.x = TRUE)
fit_all <- melt(as.data.table(fit_gd), id.vars = c('time') , 
                 measure.vars = c('S', 'I','R', 'Infected') )
fit_all[ , Tipo := ifelse( variable != 'Infected', 'Simul', 'Real')]

gr_left <- ggplot(fit_tidy, aes( x = time, y = value, 
                                 group = variable, 
                                 color = variable) ) +
             geom_line( aes(colour = variable)) +
             ylab("# Personas") + xlab("Dia") + 
             guides(color = FALSE) +
             theme_bw()
gr_left

gr_right <- ggplot(fit_all, aes( x = time, y = value,
                                 group = variable, 
                                 color = variable, 
                                 linetype = Tipo) ) +
             geom_line( aes(colour = variable)) +
             # geom_point(aes(x = val_max$Day, y = val_max$Value)) +
             scale_y_log10() +
             ylab("# Personas (log)") + xlab("Dia") + 
             guides(color = guide_legend(title = "")) +
             scale_color_manual(labels = c('Susceptibles', 'Infectados', 'Recuperados','Casos_Actuales'),
                                values = c('red', 'green', 'blue', 'black') ) +
             theme_bw() +
             theme(legend.position = "right", legend.text=element_text(size = 6)) +
             annotate( 'text', x = val_max$Day, y = val_max$Value, 
                       size = 2.5, label = val_lab) +
             annotate( 'text', x = val_max$Day, y = 0.05*val_max$Value,
                       size = 2.5, label = dea_lab) +
             annotate( 'text', x = val_max$Day, y = 0.01*val_max$Value,
                       size = 2.5, label = R0)
  
gr_right
ggsave(paste("./charts/ODE_SIR_Proyeccion_Cases_Madrid_.png", sep = ""))

gr_left + gr_right  + 
  plot_annotation(title = 'Proyección Evolución Cov-19 - Madrid (modelo SIR)',
                  theme = theme(plot.title = element_text(size = 18)))
ggsave(paste("./charts/ODE_SIR_Proyeccion_Cases_Madrid_Two.png", sep = ""))

#------------ R0
# R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
# R0
# # R0 
# # 1.403026 
# 
# fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
# # I
# # 45 149358.2
# 
# max(fit$I) * 0.02 # max deaths with supposed 2% fatality rate
# # [1] 2987.164


#--------------------------------------------------------
#----------------- RO Evolution

datCasos_Md <- datIn[ CCAA == 'MD', .(fecha, Casos)]
Casos_Md <- datCasos_Md$Casos[!is.na(datCasos_Md$Casos)]

Infected <- Casos_Md
val_infec <- seq(10, length(Casos_Md), by = 1)

cont <- 0
infecRo <- data.frame()
for (i in val_infec) {
  
    cont <- cont + 1
    Infected <- Casos_Md[1:i]
    # print(length(Infected))
    Day <- 1:(length(Infected))
    N <- 3266126 # Madrid Population : 3266126 (Madrid)
    
    SIR <- function(time, state, parameters) {
      par <- as.list(c(state, parameters))
      with(par, {
        dS <- -beta/N * I * S
        dI <- beta/N * I * S - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
      })
    }
    
    init <- c(S = N - Infected[1], I = Infected[1], R = 0)
    RSS <- function(parameters) {
      names(parameters) <- c("beta", "gamma")
      out <- ode(y = init, times = Day, func = SIR, parms = parameters)
      fit <- out[ , 3]
      sum((Infected - fit)^2)
    }
    
    Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) 
    Opt$message
    
    Opt_par <- setNames(Opt$par, c("beta", "gamma"))
    Opt_par
    
    t <- 1:70 # time in days
    fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
    
    
    R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
    # R0
    # R0 
    # 1.403026 
    
    # fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
    # I
    # 45 149358.2
    
    max(fit$I) * 0.02 # max deaths with supposed 2% fatality rate
    # [1] 2987.164
    
    max_infe <- fit[fit$I == max(fit$I), "I", drop = FALSE]
    val_max  <- data.frame(
                           Day = i, 
                           Day_max = as.numeric(rownames(max_infe)) , 
                           Value = max_infe$I,
                           R0 = R0)
    infecRo <- rbind(infecRo, val_max)
    
    # print(infecRo)
}    

#----- R0
ggplot(infecRo, aes( x = Day, y = R0)) +
  geom_line( colour = 'grey', alpha = 0.25) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, colour = 'red') +
  ylim(0, 1.10*max(infecRo$R0)) +
  ggtitle("EVOLUCIÓN DE R0 - Madrid") +
  theme_bw()
ggsave(paste("./charts/ODE_SIR_Evolucion_R0.png", sep = ""))

#----- ValorMax Infectados Estimado
ggplot(infecRo, aes( x = Day, y = Value)) +
  geom_line( colour = 'grey', alpha = 0.25) +
  geom_point(size = 2) +
  ylim(0, 1.10*max(infecRo$Value)) +
  ggtitle("EVOLUCIÓN DE VALOR DEL PICO DE INFECTADOS (estimado) - Madrid") +
  theme_bw()
ggsave(paste("./charts/ODE_SIR_PicoInfectados_Evolucion.png", sep = ""))

toc()


