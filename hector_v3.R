
HECTOR_V3_DIR <- "/Users/dorh012/projects/Hector-Versions/v3/hector"
HECTOR_DATA_DIR <- "/Users/dorh012/projects/2023/hec_data_attempt2/hectordata"
HECTOR_RCMIP_SUBMISSION <- "/Users/dorh012/projects/2023/rcmip-master/data/results/phase-1/hector"
devtools::load_all(HECTOR_V3_DIR)

library(assertthat)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
assert_that(packageVersion(pkg = "hector") == "3.0.1")


# variables to save
var <- c(VEG_C(), GLOBAL_TAS(), RF_TOTAL(), NBP(), CONCENTRATIONS_CO2(), CONCENTRATIONS_CH4(), SOIL_C(), NPP())

# 0. Import Data Needs ---------------------------------------------------------------
# Write out all of the excell sheet names 
excel_sheets(list.files(HECTOR_RCMIP_SUBMISSION, 
                        pattern = "rcmip_phase-1_hector_v3-0-0.xlsx", full.names = TRUE)) 
read_excel(file.path(HECTOR_RCMIP_SUBMISSION, "rcmip_phase-1_hector_v3-0-0.xlsx"), 
           sheet = "your_data") -> 
  wide_rcmip_data

index_to_keep <- which(names(wide_rcmip_data) %in% as.character(1850:2500))
names(wide_rcmip_data)[index_to_keep] <- paste0("XX", names(wide_rcmip_data)[index_to_keep])

wide_rcmip_data %>% 
  filter(ClimateModel == "hector|1d51f|DEFAULT") %>% 
  mutate_at(vars(starts_with("XX")), .funs = as.double) %>% 
  pivot_longer(cols = starts_with("XX")) %>% 
  mutate(year = as.integer(gsub(pattern = "XX", replacement = "", x = name))) -> 
  long_rcmip_data


# 1. Historical ----------------------------------------------------------------------
ini <- file.path(HECTOR_V3_DIR, "inst", "input", "hector_ssp245.ini")
hc <- newcore(ini, name = "historical")
run(hc)

fetchvars(hc, dates = 1750:2020, vars = var)  -> 
  hist_out 

shutdown(hc)

# 2. Pi Control ----------------------------------------------------------------------
ini <- file.path(HECTOR_DATA_DIR, "inst", "input", "picontrol_concentration.ini")
hc <- newcore(ini, name = "picontrol concentration")
run(hc)

fetchvars(hc, dates = 1750:2100, vars = var)  -> 
  piconc_out 

# piconc_out %>% 
#   ggplot(aes(year, value, color = variable)) + 
#   geom_line() +
#   facet_wrap("variable", scales = "free")

shutdown(hc)

## 2B. CO2 Pulse ----------------------------------------------------------------------
 
ini <- file.path(HECTOR_DATA_DIR, "inst", "input", "picontrol_concentration.ini")
hc <- newcore(ini, name = "picontrol CO2 pulse")

CO20 <- fetchvars(hc, dates = NA, vars = PREINDUSTRIAL_CO2())[["value"]]
setvar(hc, dates = 1850, var = CO2_CONSTRAIN(), values = CO20 * 4, unit = getunits(CO2_CONSTRAIN()))
run(hc)

fetchvars(hc, dates = 1750:2100, vars = var)  -> 
  co2pulse_out 

piconc_out %>%
  ggplot(aes(year, value, color = variable)) +
  geom_line() +
  facet_wrap("variable", scales = "free")

shutdown(hc)


co2pulse_out$value <- co2pulse_out$value - piconc_out$value

## 2C. CO2 Pulse no carbon-climate interactions ----------------------------------------------------------------------
ini <- file.path(HECTOR_DATA_DIR, "inst", "input", "picontrol_concentration.ini")
hc <- newcore(ini, name = "picontrol CO2 pulse no feedbacks")
setvar(hc, dates = c(NA, NA), var = c(BETA(), Q10_RH()), 
       values = c(0, 1), unit = c(getunits(BETA()), getunits(Q10_RH())))
run(hc)
fetchvars(hc, dates = 1750:2100, vars = var)  -> 
  contorl_nofeedback_out
shutdown(hc)


ini <- file.path(HECTOR_DATA_DIR, "inst", "input", "picontrol_concentration.ini")
hc <- newcore(ini, name = "picontrol CO2 pulse no feedbacks")

CO20 <- fetchvars(hc, dates = NA, vars = PREINDUSTRIAL_CO2())[["value"]]
setvar(hc, dates = 1850, var = CO2_CONSTRAIN(), values = CO20 * 4, unit = getunits(CO2_CONSTRAIN()))
setvar(hc, dates = c(NA, NA), var = c(BETA(), Q10_RH()), 
       values = c(0, 1), unit = c(getunits(BETA()), getunits(Q10_RH())))
run(hc)

fetchvars(hc, dates = 1750:2100, vars = var)  -> 
  co2pulse_nofeedback_out

shutdown(hc)

co2pulse_nofeedback_out$value <- co2pulse_nofeedback_out$value - contorl_nofeedback_out$value

co2pulse_nofeedback_out %>% 
  ggplot(aes(year, value)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free")

# 3. 1 pct co2 RF driven ----------------------------------------------------------------------
# How different is the climate system? Run some idealized experiments prescribe the RF. 
long_rcmip_data %>% 
  filter(Scenario == "1pctCO2-4xext") %>% 
  filter(Variable == "Radiative Forcing") %>%
  select(Date = year, RF_tot_constrain = value) -> 
  RF_1pct

write.csv(RF_1pct, here::here("inputsv25", "input", 
                              "constraints", "RF_1pct.csv"), row.names = FALSE)


ini <- file.path(HECTOR_DATA_DIR, "inst", "input", "picontrol_concentration.ini")
hc <- newcore(ini, name = "1pct RF driven")
setvar(core = hc, dates = RF_1pct$Date, var = FTOT_CONSTRAIN(), values = RF_1pct$RF_tot_constrain, unit = "W/m2")
run(hc)

fetchvars(hc, dates = 1750:2100, vars = var)  -> 
  rfpct1_out 

# rfpct1_out %>% 
#   ggplot(aes(year, value, color = variable)) + geom_line() + 
#   facet_wrap("variable", scales = "free")
shutdown(hc)


# 4. Abrupt 4xCO2 step RF forced ----------------------------------------------------------------------




# X. Save Results ----------------------------------------------------------------------
out <- rbind(hist_out, piconc_out, rfpct1_out, co2pulse_nofeedback_out, rfpct1_out, co2pulse_out)
out$v <- as.character("3.0")
write.csv(out, file = "hector3.csv", row.names = FALSE)
