# Process global mean surface temperature from the gcam-hector outputstreams and get the cumulative CO2 emissions. 


library(dplyr)
library(ggplot2)
library(rgcam)
library(hector)
packageVersion("hector")

# Start by laoding the prjdata extract produced by rgcam
load(here::here("ci-results/core_gcam.prj"))
scn_names <- listScenarios(prjdata)


# funky results GCAM_SSP1_2p6|GCAM_SSP4_2p6|GCAM_SSP4_2p6, I think that the output stream is saving reuslts from mulitple runs.... would need 
# to write some special code to handel it ie detect the ################ Hector Core Reset ################ lines and then subset the df
list.files("ci-results", pattern = "GCAM_SSP1-gcam|GCAM-gcam|GCAM_SSP2-gcam|GCAM_SSP3-gcam|GCAM_SSP4-gcam|GCAM_SSP5-gcam", full.names = TRUE) %>%  
lapply(FUN = function(file){
  print(file)
  scn <- gsub(pattern = "exe_|-gcam-hector-outputstream.csv", replacement = "", x = basename(file))
  read.csv(file, skip = 1) %>% 
    filter(spinup == 0 & variable %in% c(GLOBAL_TAS(), LAND_TAS(), SST())) %>% 
    mutate(scenario = scn) %>%  
    select(year, scenario, value, variable)  %>% 
    group_by(year, scenario, variable) %>% 
    summarise(value = mean(value)) %>% 
    tidyr::spread(variable, value) %>% 
    ungroup -> 
    df
  
  flnd  <- 0.29
  
  df %>% 
    mutate(gmst = (land_tas * flnd + sst * (1 - flnd))) %>%  
    select(year, scenario, gmst) -> 
    out 
  
  return(out)
   
}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(year = as.integer(year)) -> 
  gcam_hector_gmst



# For the target files 
list.files("ci-results", pattern = "GCAM_SSP1_2p6|GCAM_SSP4_2p6|GCAM_SSP4_2p6", full.names = TRUE)  %>% 
  lapply(FUN = function(file){
    
    print(file)
    scn <- gsub(pattern = "exe_|-gcam-hector-outputstream.csv", replacement = "", x = basename(file))
    lines <- readLines(file)
    index <- max(which(lines == "################ Hector Core Reset ################"))
    read.csv(file, skip = index, col.names = c("year", "scenario", "spinup", "componet", "variable", "value", "unit")) %>% 
      filter(spinup == 0 & variable %in% c(GLOBAL_TAS(), LAND_TAS(), SST())) %>% 
      mutate(scenario = scn) %>%  
      select(year, scenario, value, variable)  %>% 
      group_by(year, scenario, variable) %>% 
      summarise(value = mean(value)) %>% 
      tidyr::spread(variable, value) %>% 
      ungroup -> 
      df
    
    flnd  <- 0.29
    
    df %>% 
      mutate(gmst = (land_tas * flnd + sst * (1 - flnd))) %>%  
      select(year, scenario, gmst) -> 
      out 
    
    return(out)
    
    
  }) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(year = as.integer(year)) -> 
  gcam_hector_gmst_target
  


gcam_hector_gmst <- rbind(gcam_hector_gmst, gcam_hector_gmst_target)


# the results are in MTC
ffi_results <- getQuery(prjdata, query = "CO2_emissions") %>% rename(ffi_emiss = value) %>%  select(-Units)
luc_reuslts <- getQuery(prjdata, query = "LUC_emissions") %>% rename(luc_emiss = value) %>%  select(-Units)

ffi_results %>% 
inner_join(luc_reuslts, by = c("year", "region", "scenario")) %>%  
  filter(scenario %in% unique(gcam_hector_gmst$scenario)) -> 
  emissions_df

  
emissions_df %>%  
  mutate(value = ffi_emiss + luc_emiss) %>% 
  group_by(scenario, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  total_annual_co2

# Find the cumulative co2 emissions. 
split(total_annual_co2, total_annual_co2$scenario) %>% 
  lapply(function(dat){
    vals <- cumsum(dat$value)
    dat$co2_MTC <- vals
    return(dat)
  }) %>% 
  do.call(what = "rbind") %>%  
  tibble::as_tibble() %>%  
  select(-value) -> 
  cumsum_co2

cumsum_co2 %>% 
  inner_join(gcam_hector_gmst) -> 
  out 

out %>% 
  ggplot(aes(co2_MTC, gmst, color = scenario)) + 
  geom_line()

write.csv(out, file = "gcam-hector_co2_gmst.csv", row.names = FALSE)


