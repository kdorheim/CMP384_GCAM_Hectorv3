
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
  mutate(year = as.integer(gsub(pattern = "XX", replacement = "", x = name))) %>%  
  na.omit -> 
  long_rcmip_data



long_rcmip_data %>% head()
"Radiative Forcing"
"Surface Air Temperature Change" 

long_rcmip_data %>% 
  filter(Scenario == "ssp585") %>%  
  filter(Variable == "Surface Air Temperature Change") %>% 
  select(year, value_v25 = value, variable = Variable) -> 
  ssp585_old




ini <- file.path(HECTOR_V3_DIR, "inst", "input", "hector_ssp585.ini")
hc <- newcore(ini)
run(hc)
fetchvars(hc, dates = ssp585_old$year, vars = GLOBAL_TAS()) -> 
  ssp585_new



ggplot() + 
  geom_line(data = ssp585_old, aes(year, value_v25, color = "v2.5")) + 
  geom_line(data = ssp585_new, aes(year, value, color = "v3"))


ssp585_old %>% 
  inner_join(ssp585_new, by = "year") %>% 
  mutate(dif = value - value_v25) %>% 
  tail()




