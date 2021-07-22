library(tidyverse)
library(WDI)
library(vtree)

new_cache <- WDIcache()
WDIsearch(string = "Access to electricity", field = "name", short = TRUE, cache = new_cache)

wb_data <- WDI(
  country = "all",
  indicator = "EG.ELC.ACCS.ZS", 
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = new_cache,
  latest = 100, # Integer indicating the number of most recent non-NA values to get.
  language = "en"
)

wb_data_2 <- wb_data %>% 
  filter(year == 2019)

# vtree ----
wb_data_2 %>% 
  mutate(less_than = ifelse(EG.ELC.ACCS.ZS < 20, country, "More than or equal to")) %>% 
  filter(!is.na(region) & region != "Aggregates") %>% 
  vtree("region less_than", 
        showpct = FALSE,
        showcount = FALSE,
        summary="EG.ELC.ACCS.ZS \nAccess to electricity\n%mean% (%)",
        prune=list(less_than=c("More than or equal to")),
        labelvar=c(region = "Region"),
        showvarnames = FALSE,
        horiz = FALSE,
        title="**Access to electricity (% of population)** \n\n**World** \nNumber of countries"
  )
