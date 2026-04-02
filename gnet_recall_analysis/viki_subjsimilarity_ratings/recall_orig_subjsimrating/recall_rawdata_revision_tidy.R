# Revision of recall data: correction of errors in data collection + data cleaning
# Notes: - plcXs = object showing (1) or not showing (0) on the picture -> updated at retrieval
##       - cell1-9: "occupied" status of the given cell -> updated at retrieval
##       - "..undefined" columns do not impact the picture seen by the participant -> no need for consideration
##       - plcxc: colour of the element -> not updated upon changing the element, only upon the re-colouring
# Conclusion: recall program source code to be fixed


library(tibble)
library(tidyverse)

# Read recall data
rcll <- read.csv("gnet4s_orig_recall.csv") # sourcefile, change as needed


# Create duplicate of original data for editing
rcll_edit <- rcll
rcll_edit <- rcll_edit %>% relocate(rtnr, .after = multichar_knowresp) # place rtnr next to multichar_knowresp for better visibility

# Count NAs in data subset
rcll_na <- rcll_edit |> filter(if_any(cell1:plc5y, is.na))

# Replace missing values in cell1-9 with 0
rcll_edit <- rcll_edit %>% 
  mutate(
    across(cell1:cell9, ~replace_na(.x, 0))
  )

        
# Revise elements

# Pictures with less than 5 occupied cells -> less than 5 placed elements? 
## rcll_edit$cells_sum -> total nr of objects placed on the picture according to cell1-9
rcll_edit <- rcll_edit |> 
  rowwise() |>
  mutate(cells_sum = sum(c_across(cell1:cell9) != 0)) |>
  relocate(cells_sum, .after = cell9)

cells_err <- rcll_edit |>
  filter( cells_sum != 5) |>
  arrange(cells_sum)

cells_err <- cells_err |> arrange(multichar_knowresp, rtnr)

# Pictures with any plcXs == 0 -> retrieved elements not placed again?
show_err <- rcll_edit %>%
  select(everything()) %>%
  filter_at(vars(matches("plc\\ds$")), any_vars(.==0))

show_err <- show_err |> arrange(multichar_knowresp, rtnr)

all.equal(show_err, cells_err) # check if the two tables are the same



# Pictures with coinciding coordinates in plcs -> has "overlapping" / re-placed elements (should be included to plcXs ==0)
##Note: solution with for loop (is there a better alternative with tidyverse?)
objt_err <- as_tibble(matrix(nrow=0, ncol=ncol(rcll_edit)), .name_repair = ~ colnames(rcll_edit) )
obj_nr <- 5

for (m in 1:nrow(rcll_edit)){
  temp <- c()
  for(n in 1:obj_nr){
    temp <- append(temp, paste(rcll_edit[m, paste("plc", n, "x", sep="")], rcll_edit[m, paste("plc", n, "y", sep="")], sep=";"))
  }
  if(length(unique(temp)) != 5){
    objt_err <- rbind(objt_err, rcll_edit[m,])
  }
}

rm(temp, m, n)

setdiff(objt_err, show_err) #check if included in show_err

## Conclusion: Pictures handed in with fewer than 5 elements on them -> to be considered as faulty data


#Revise colours
##Note: code inflexible to element nr change and .png name structure change (more flexible alternative?)
clrs_err <- rcll_edit |>
  filter( substr(plc1, 4,4) != plc1c |
          substr(plc2, 4,4) != plc2c |
          substr(plc3, 4,4) != plc3c |
          substr(plc4, 4,4) != plc4c |
          substr(plc5, 4,4) != plc5c
          )

setdiff(clrs_err, cells_err) # check for only colour errors

# Correcting element colours to feture nr indicated in the .png in plcx
rcll_revised <- rcll_edit |> 
  mutate(
    plc1c = substr(plc1, 4,4),
    plc2c = substr(plc2, 4,4),
    plc3c = substr(plc3, 4,4),
    plc4c = substr(plc4, 4,4),
    plc5c = substr(plc5, 4,4)
    ) |>
  mutate_at(c("plc1c", "plc2c", "plc3c", "plc4c", "plc5c"), as.numeric) # visualiser needs colours to be numeric

## Conclusion: desynchronised updating of colour variables -> valid data in .png strings -> correction possible, exclusion not required


total_error <- bind_rows(cells_err, show_err, objt_err, clrs_err) |> distinct()


# Removal of element error pictures from dataset
removed <- bind_rows(cells_err, show_err, objt_err) |> distinct_at(vars(multichar_knowresp, rtnr), .keep_all = TRUE)
rcll_revised <- rcll_revised %>% anti_join(removed, join_by(multichar_knowresp, rtnr))
