# Run Hector v 2.5 
# Okay I am not sure what the heck is going on here! why
# # HECTOR_V25_DIR <- "/Users/dorh012/projects/Hector-Versions/v25/hector"
# # setwd(HECTOR_V25_DIR)
# devtools::load_all(HECTOR_V25_DIR)
#remotes::install_github("jgcri/hector@62381e7", force = TRUE)

library(assertthat)
library(dplyr)
library(hector)
library(ggplot2)

assert_that(packageVersion(pkg = "hector") == "2.5.0")


var <- c(VEG_C(), GLOBAL_TEMP(), RF_TOTAL(), "atm_land_flux", ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), SOIL_C(), NPP())

PRINT_PLOTS <- TRUE
# 1. Historical ----------------------------------------------------------------------
#ini <- system.file("input/hector_rcp45.ini", package = "hector") 
ini <- here::here("inputsv25", "input", "hector_rcp45.ini")
hc <- newcore(inifile = ini, name = "historical")
run(hc)

fetchvars(hc, dates = 1750:2020, vars = var) -> 
  hist_out

if(PRINT_PLOTS){
  hist_out %>%
    ggplot() +
    geom_line(aes(year, value, color = variable)) +
    facet_wrap("variable", scales = "free")
}
shutdown(hc)

# 2. Pi Control ----------------------------------------------------------------------
ini <- here::here("inputsv25", "input", "picontrol_concentration.ini")
hc <- newcore(inifile = ini, name = "picontrol_concentration")

run(hc, runtodate = 2100)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O())) -> 
  picontrol_out

if(PRINT_PLOTS){
  picontrol_out %>%
    ggplot() +
    geom_line(aes(year, value, color = variable)) +
    facet_wrap("variable", scales = "free")
}
shutdown(hc)


# 2. Pi Control ----------------------------------------------------------------------
ini <- here::here("inputsv25", "input", "picontrol_concentration.ini")
hc <- newcore(inifile = ini, name = "picontrol_concentration")

run(hc, runtodate = 2100)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O())) -> 
  picontrol_out

if(PRINT_PLOTS){
  picontrol_out %>%
    ggplot() +
    geom_line(aes(year, value, color = variable)) +
    facet_wrap("variable", scales = "free")
}
shutdown(hc)

## 2B. CO2 Pulse ----------------------------------------------------------------------
ini <- here::here("inputsv25", "input", "picontrol_concentration_pulse.ini")
hc <- newcore(ini, name = "picontrol CO2 pulse")

run(hc)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O()))  -> 
  co2pulse_out 

if(PRINT_PLOTS){
  co2pulse_out %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line() +
    facet_wrap("variable", scales = "free")
}
shutdown(hc)


diff <- co2pulse_out$value - picontrol_out$value

diff_df <- co2pulse_out
diff_df$value <- diff

# diff_df %>%
#   ggplot(aes(year, value, color = variable)) +
#   geom_line() +
#   facet_wrap("variable", scales = "free")

co2pulse_out <- diff_df

## 2C. CO2 Pulse No Carbon Cycle Feedbacks --------------------------------------------

ini <- here::here("inputsv25", "input", "picontrol_concentration.ini")
hc <- newcore(inifile = ini, name = "picontrol_concentration")
setvar(hc, dates = c(NA, NA), var = c(BETA(), Q10_RH()), 
       values = c(0, 1), unit = c(getunits(BETA()), getunits(Q10_RH())))

run(hc, runtodate = 2100)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O())) -> 
  picontrol_out_nocc

shutdown(hc)

ini <- here::here("inputsv25", "input", "picontrol_concentration_pulse.ini")
hc <- newcore(ini, name = "picontrol CO2 pulse no feedbacks")
setvar(hc, dates = c(NA, NA), var = c(BETA(), Q10_RH()), 
       values = c(0, 1), unit = c(getunits(BETA()), getunits(Q10_RH())))
run(hc)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O()))  -> 
  co2pulse_out_nocc
shutdown(hc)

diff <- co2pulse_out_nocc$value - picontrol_out_nocc$value
co2pulse_out_nocc$value <- diff


if(PRINT_PLOTS){
  co2pulse_out_nocc %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line() +
    facet_wrap("variable", scales = "free")
}

# 3. 1pct CO2  -----------------------------------------------------------------
## 3A. RF Total Driven -----------------------------------------------------------------

ini <- here::here("inputsv25", "input", "1pctCO2_RF.ini")
hc <- newcore(inifile = ini, name = "1pct RF driven")

run(hc, runtodate = 2100)

fetchvars(hc, dates = 1750:2100, vars = c(var, RF_CH4(), RF_N2O())) -> 
  pct1_rf_out

if(PRINT_PLOTS){
  pct1_rf_out %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line() +
    facet_wrap("variable", scales = "free")
}


# X. Save Results ----------------------------------------------------------------------
out <- rbind(hist_out, picontrol_out, co2pulse_out, co2pulse_out_nocc, pct1_rf_out)
out$v <- "2.5"

out %>% 
  mutate(variable = if_else(variable == GLOBAL_TEMP(), "global_tas", variable)) %>% 
  mutate(variable = if_else(variable == RF_TOTAL(), "RF_tot", variable)) %>% 
  mutate(variable = if_else(variable == "atm_land_flux", "NBP", variable)) -> 
  out 

write.csv(out, file = "hector25.csv", row.names = FALSE)
