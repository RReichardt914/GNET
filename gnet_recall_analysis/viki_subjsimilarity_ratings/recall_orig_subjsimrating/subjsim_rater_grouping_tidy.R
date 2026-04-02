# Tidyverse approach to the creation of item sets for subjective similarity rating
## Notes: - Keep an eye out for float quotients (divisions with remainders)!!!!!!
##        - Number of independent rater groups and prototypes are set manually
##        - Uses the "rcll_revised" table containing filtered data generated in previous script

library(tidyverse)

# Nr of independent rating groups
rate_gr <- 4

# Prototype and recall items tables
prot_id <- tibble( prt_id = c("prototype1.png", "prototype2.png", "prototype3.png", "prototype4.png"))

rcll_id <- rcll_revised |>
  mutate(img_id = paste(multichar_knowresp, "-", rtnr, ".png", sep = "")) |>
  select(img_id)

# Every prototype matched with every recall item
rate_pool <- cross_join(rcll_id, prot_id)

# Creation of stimulus groups
groups <- split(rate_pool, sample(rep(1:rate_gr, length = nrow(rate_pool))))

# Export grouped data to csv files
groups %>% 
  names(.) %>% 
  map(~ write_csv(groups[[.]], paste0( ., ".csv")))
