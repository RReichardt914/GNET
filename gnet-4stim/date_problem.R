library(dplyr)

# Assuming gnet4s_orig is your data frame
result <- gnet4s_delay_ST %>%
  filter(multichar_knowresp == "rscl84") %>%
  select(date_startdate)

# Print the result
print(result)

# Assuming gnet4s_orig is your data frame
result <- gnet4s_delay_ST %>%
  filter(multichar_knowresp == "rscl84") %>%
  select(jatosStudyResultId)

print(result)
