# The comparison! 
library(dplyr)
library(ggplot2)


theme_set(theme_bw(base_size = 18))

rbind(read.csv(here::here("hector3.csv")), 
      read.csv(here::here("hector25.csv"))) %>% 
  mutate(v = as.character(v)) -> 
  hector_data

# The figures used in the CMP! 

hector_data %>% 
  filter(year %in% 1840:1945) %>% 
  filter(variable %in% c(SOIL_C(), VEG_C())) %>% 
  mutate(v = as.character(v)) %>% 
  filter(scenario == "picontrol CO2 pulse no feedbacks") %>% 
  ggplot(aes(year, value, color = v)) + 
  geom_line(size = 1.5, alpha = 0.7) +
  facet_wrap("variable", scales = "free", ncol = 1) + 
  theme_bw() + 
  labs(year = NULL, y = "Pg C", title = "Carbon Cycle Response to a [CO2] impulse")



hector_data %>% 
 # filter(year %in% 1840:2000) %>% 
  filter(variable %in% c(SOIL_C(), VEG_C())) %>% 
  mutate(v = as.character(v)) %>% 
  filter(scenario == "historical") %>% 
  ggplot(aes(year, value, color = v)) + 
  geom_line() +
  facet_wrap("variable", scales = "free", ncol = 1)


hector_data %>% 
  filter(scenario %in% c("picontrol CO2 pulse no feedbacks")) %>% 
  filter(variable == "RF_tot") %>% 
  filter(year %in% 1845:1855) %>% 
  mutate(v = as.character(v)) %>% 
  ggplot(aes(year, value, color = v)) + 
  geom_line(size = 1) + 
  theme(legend.title  = element_blank()) + 
  labs(year = NULL, y = "W/m2", title = "RF Response to a [CO2] impulse")



hector_data %>% 
  filter(scenario %in% c("1pct RF driven")) %>% 
  filter(variable == "global_tas") %>% 
  mutate(v = as.character(v)) %>% 
  ggplot(aes(year, value, color = v)) + 
  geom_line(size = 1) + 
  theme(legend.title  = element_blank()) + 
  labs(year = NULL, y = "deg C", title = "Temp. response to RF forced run") 


hector_data %>% 
  filter(scenario %in% c("1pct RF driven")) %>% 
  filter(variable == "global_tas") %>% 
  mutate(v = as.character(v)) %>% 
  distinct() %>% 
  filter(year >1850) %>% 
  tidyr::spread(v, value) %>% 
  mutate(dif = `3` - `2.5`) %>% 
  pull(dif) %>% summary()
