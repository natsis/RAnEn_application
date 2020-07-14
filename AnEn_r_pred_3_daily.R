#!/usr/bin/env Rscript
## /* Copyright (C) 2020 Athanasios Natsis <natsisthanasis@gmail.com> */
##

####_ Set environment _####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = c("AnEn_r_pred_3_daily.R")

## set path relative to this script
# setwd(getSrcDirectory()[1])
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(RAnEn)
library(RAnEnExtra)
library(data.table)
# source("/home/athan/FUNCTIONS/R/data.R")
# source("/home/athan/LifeAsti/Downscale/LifeAsti_Functions.R")

# install.packages("https://github.com/Weiming-Hu/AnalogsEnsemble/raw/master/RAnalogs/releases/RAnEn_latest.tar.gz", repos = NULL)
# devtools::install_github("Weiming-Hu/RAnEnExtra")

MDAY_break <- 20


# FILTERED <- FALSE
FILTERED <- TRUE

if ( FILTERED) { fifo <- "FL"}
if (!FILTERED) { fifo <- "UNFL"}

TEST <- TRUE
# TEST <- FALSE


## INPUT
out_rds  <- "./Rome_SVM_data.Rds"

## OUTPUT
if (TEST) {
    models_lsfl   <- paste0("./ROME_AnEn_predict_",fifo,"_test.Rds")
    forecast_stor <- paste0("./ROME_AnEn_forecast_data_daily_",fifo,"_test.Rds")
    observat_stor <- paste0("./ROME_AnEn_observat_data_daily_",fifo,"_test.Rds")
} else {
    models_lsfl   <- paste0("./ROME_AnEn_predict_",fifo,".Rds")
    forecast_stor <- paste0("./ROME_AnEn_forecast_data_daily_",fifo,".Rds")
    observat_stor <- paste0("./ROME_AnEn_observat_data_daily_",fifo,".Rds")
}



## load data to reshape
data    <- data.table(readRDS(out_rds))

if (FILTERED) {
    ## select data set to use using all the filters
    data <- copy( data[ TEMP_threshold == F &
                  HUMI_threshold == F &
                  HUMI_extreme   == F ])
    # data <- rm.cols.dups.DT(data)
    cat(paste("\n\n  FILTERD DATA USED  \n\n"))
}

if (!FILTERED) {
    ## select data set to use using no filtering
    data <- copy( data )
    # data <- rm.cols.dups.DT(data)
    cat(paste("\n\n  UN-FILTERD DATA USED  \n\n"))
}


## create height diff from wrf
data[, Hgt_diff := HGT - HGT_ST ]

## temp gradient correction for stations???
unique((data[, HGT - HGT_ST ]/100) * 0.68)


## use only 2 stations
if (TEST) {
    data <- data[ Station %in% unique( data$Station )[1:2] ]
}


## add flts and forecast time to data
data[, Time := as.POSIXct(as.Date(DATE)) ]
data[, FLTS := DATE - Time]

summary(as.numeric(data$FLTS)/3600)
diff(sort(unique(data$Time)))



sort(unique( data[ FLTS == 0, DATE]))
sort(unique( data$FLTS/3600 )) / 24


data_original <- copy(data)

## keep only existing variables on both and location data
matched_vars <- c(
    "Station", "DATE", "Name", "Lat_ST", "Lon_ST",
    "Temp", "RelHum", "T2", "RH2",
    "FLTS", "Time")
data <- data[, ..matched_vars ]


## create forecast file
if (!file.exists(forecast_stor)) {

    ## create vectors of meta data
    stati_vec <- sort(unique(data$Station))
    times_vec <- sort(unique(data[ FLTS == 0, DATE]))
    flts_vec  <- sort(unique(data$FLTS))

    ## get dimensions
    stati_dim <- length(stati_vec)
    times_dim <- length(times_vec)
    flts_dim  <- length(flts_vec)

    ## exclude variables
    variables <- sort(names(data))
    variables <- variables[variables != "DATE"   ]
    variables <- variables[variables != "Name"   ]
    variables <- variables[variables != "Lon_ST" ]
    variables <- variables[variables != "Lat_ST" ]
    variables <- variables[variables != "FLTS"   ]
    variables <- variables[variables != "Time"   ]
    variables <- variables[variables != "Station"]

    ## remove obs data
    variables <- variables[variables != "Temp"   ]
    variables <- variables[variables != "RelHum" ]

    vars_dim <- length(variables)

    ## create a sparce matrix and fill
    fill <- array(data = NA, dim = c(vars_dim, stati_dim, times_dim, flts_dim ))
    for (var in variables ) {
        for (stat in stati_vec) {
            for (time in times_vec) {
                for (flts in flts_vec) {

                    gg <- data[ Station == stat &
                                    Time    == time &
                                    FLTS    == flts , ..var]
                    stopifnot(length(gg)==1)

                    gg <- (unlist(gg))[1]

                    fill[ which( variables == var ),
                          which( stati_vec == stat ),
                          which( times_vec == time ),
                          which( flts_vec  == flts )  ] <- as.numeric(gg)
                    # cat(paste(gg, stat, time, lfts, var),"\n")
                }
            }
        }
    }

    ## create forecast data structure
    forecast <- generateForecastsTemplate()
    coo      <- unique(data[ , .(Station, Lat_ST, Lon_ST) ])
    ## create forecast structure
    forecast$Data           <- fill
    forecast$StationNames   <- stati_vec
    forecast$Times          <- times_vec
    forecast$FLTs           <- flts_vec
    forecast$Xs             <- coo[ match(stati_vec, coo$Station), Lat_ST ]
    forecast$Ys             <- coo[ match(stati_vec, coo$Station), Lon_ST ]
    forecast$ParameterNames <- variables

    saveRDS(object = forecast, file = forecast_stor, compress = "xz")
} else {
    cat(paste("\nLoad prepered forecast data\n"))
    forecast <- readRDS(forecast_stor)
}



##FIXME some data are empty


## create observations file
if (!file.exists(observat_stor)) {

    ## get dimensions and create the matrix
    data$Day   <- as.factor(data$Day)
    data$Epoch <- as.factor(data$Epoch)

    as.numeric(data$Day)

    stati_vec <- sort(unique(data$Station))
    times_vec <- sort(unique(data[ , DATE]))

    stati_dim <- length(stati_vec)
    times_dim <- length(times_vec)

    ## exclude vaiables
    variables <- sort(names(data))
    variables <- variables[variables != "Station"]
    variables <- variables[variables != "DATE"   ]
    variables <- variables[variables != "Name"   ]
    variables <- variables[variables != "Lon_ST" ]
    variables <- variables[variables != "Lat_ST" ]
    variables <- variables[variables != "ALBEDO" ]
    variables <- variables[variables != "EMISS"  ]
    variables <- variables[variables != "LU_INDEX" ]
    variables <- variables[variables != "YPer"   ]
    variables <- variables[variables != "Epoch"   ]
    variables <- variables[variables != "DPer"   ]
    variables <- variables[variables != "Ylin"   ]
    variables <- variables[variables != "Dlin"   ]
    variables <- variables[variables != "Split"  ]
    variables <- variables[variables != "FLTS"   ]
    variables <- variables[variables != "Time"   ]
    variables <- variables[variables != "Day"   ]


    variables <- variables[variables != "RelHum"  ]
    variables <- variables[variables != "Temp"    ]


    vars_dim <- length(variables)

    ## use the same names for obs and forecast
    data[,  T2:= NULL]
    data[, RH2:= NULL]
    names(data)[names(data) == "Temp"   ] <- "T2"
    names(data)[names(data) == "RelHum" ] <- "RH2"
    # variables[ variables    == "RelHum" ] <- "RH2"
    # variables[ variables    == "Temp"   ] <- "T2"

    ## create a sparse matrix and fill
    fill <- array(data = NA, dim = c(vars_dim, stati_dim, times_dim ))

    for (var in variables ) {
        for (stat in stati_vec) {
            for (time in times_vec) {

                gg <- data[ Station == stat &
                                Time    == time , ..var]
                stopifnot(length(gg)==1)

                gg <- (unlist(gg))[1]

                fill[ which( variables == var ),
                      which( stati_vec == stat ),
                      which( times_vec == time )  ] <- as.numeric(gg)
                # cat(paste(gg, stat, time, lfts, var),"\n")
            }
        }
    }
    dim(fill)

    observations <- generateObservationsTemplate()
    coo          <- unique(data[ , .(Station, Lat_ST, Lon_ST) ])
    ## create forecast structure
    observations$Data           <- fill
    observations$StationNames   <- stati_vec
    observations$Times          <- times_vec
    # observations$FLTs           <- flts_vec
    observations$Xs             <- coo[ match(stati_vec, coo$Station), Lat_ST ]
    observations$Ys             <- coo[ match(stati_vec, coo$Station), Lon_ST ]
    observations$ParameterNames <- variables


    saveRDS(object = observations, file = observat_stor, compress = "xz")
} else {
    cat(paste("\nLoad prepered OBS data\n"))
    observations <- readRDS(observat_stor)
}






## init anen
config <- new(Config)




## do each month separately
gather_stats <- data.table()
for ( am in sort(unique(month(forecast$Times))) ) {
    search.times <- forecast$Times[  mday(forecast$Times) <= MDAY_break & month(forecast$Times) == am ]
    test.times   <- forecast$Times[  (!mday(forecast$Times) <= MDAY_break) & month(forecast$Times) == am ]

    config$num_analogs    <- sqrt(length(search.times))



    config$observation_id <- which(observations$ParameterNames == "RH2")
    config$weights        <- (forecast$ParameterNames == "RH2") * 1


    AnEn <- generateAnalogs(forecast, observations, test.times, search.times, config )


    print(AnEn)

    anen <- AnEn$analogs


    obs <- alignObservations(observations$Data, observations$Times,
                             test.times, forecast$FLTs,
                             silent = T, show.progress = F)

    obs <- obs[config$observation_id, , , ]



    # Generate verification metrics
    ret.MAE  <- verifyMAE(anen, obs)
    ret.RMSE <- verifyRMSE(anen, obs)
    ret.Bias <- verifyBias(anen, obs)
    ret.RH   <- verifyRankHist(anen, obs)

    gather_stats <- rbind(gather_stats,
                          cbind( Month = am,
                                 MAE   = ret.MAE[1],
                                 RMSE  = ret.RMSE[1],
                                 Bias  = ret.Bias[1],
                                 RH    = ret.RH[1]    ), use.names=F
    )


    # Let's make some figures
    par(mfrow = c(4, 1), mar = c(3, 4.5, 1, 1))
    plot(forecast$FLTs/3600, ret.MAE$flt, type = 'b', pch = 1, cex = 0.5,
         xlab = '', ylab = 'MAE')
    plot(forecast$FLTs/3600, ret.RMSE$flt, type = 'b', pch = 1, cex = 0.5,
         xlab = '', ylab = 'RMSE')
    plot(forecast$FLTs/3600, ret.Bias$flt, type = 'b', pch = 1, cex = 0.5,
         xlab = 'Lead Times (h)', ylab = 'Bias')
    barplot(ret.RH$rank, ylab = 'Rank Frequency')

}



print(forecast)
print(observations)



## year test
config <- new(Config)

## do each month separate

length(sort(forecast$Times))

search.times <- forecast$Times[ 1:300 ]
test.times   <- forecast$Times[ 300:length(forecast$Times) ]

config$num_analogs    <- sqrt(length(search.times))


config$observation_id <- which(observations$ParameterNames == "RH2")
config$weights        <- (forecast$ParameterNames == "RH2") * 1



AnEn <- generateAnalogs(forecast, observations, test.times, search.times, config )


print(AnEn)

anen <- AnEn$analogs



##FIXME geting only one value per day not 24 per day. !!
anen[,,1,1]


anen[1,1,,]

ast <- anen[1,,,]


obs <- alignObservations(observations$Data, observations$Times,
                         test.times, forecast$FLTs,
                         silent = T, show.progress = F)

obs <- obs[config$observation_id, , , ]


# Generate verification metrics
ret.MAE  <- verifyMAE(anen, obs)
ret.RMSE <- verifyRMSE(anen, obs)
ret.Bias <- verifyBias(anen, obs)
ret.RH   <- verifyRankHist(anen, obs)


# Let's make some figures
par(mfrow = c(4, 1), mar = c(3, 4.5, 1, 1))
plot(forecast$FLTs/3600, ret.MAE$flt, type = 'b', pch = 1, cex = 0.5,
     xlab = '', ylab = 'MAE')
plot(forecast$FLTs/3600, ret.RMSE$flt, type = 'b', pch = 1, cex = 0.5,
     xlab = '', ylab = 'RMSE')
plot(forecast$FLTs/3600, ret.Bias$flt, type = 'b', pch = 1, cex = 0.5,
     xlab = 'Lead Times (h)', ylab = 'Bias')
barplot(ret.RH$rank, ylab = 'Rank Frequency')





####_ END _####
tac = Sys.time(); difftime(tac,tic,units="mins")
cat(paste("\n  --  ",  Script.Name, " DONE  --  \n\n"))
cat(sprintf("%s H:%s U:%s S:%s T:%f\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))

