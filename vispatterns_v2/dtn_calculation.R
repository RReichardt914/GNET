# ----------------------------
# Step 1: Prepare data
# ----------------------------
picdat_clean <- picdat %>%
  mutate(
    base_id = str_remove(pict, "col$|sha$")
  )

# assign category labels (D1–D5)
picdat_clean <- picdat_clean %>%
  mutate(
    type = case_when(
      str_detect(pict, paste(c("p6","p7","p8","p9","p10"), collapse = "|")) ~ "D5",
      str_detect(pict, "p[1-5]d4") ~ "D4",
      str_detect(pict, "p[1-5]d3") ~ "D3",
      str_detect(pict, "p[1-5]d2") ~ "D2",
      str_detect(pict, "p[1-5]d1") ~ "D1",
      TRUE ~ NA_character_
    )
  )

# remove prototypes
stimuli <- picdat_clean %>%
  filter(!str_detect(pict, "^prototype"))

# ----------------------------
# Step 2: Create stimulus-level table
# ----------------------------
stimulus_table <- stimuli %>%
  group_by(base_id, type) %>%
  summarise(
    arr = list(across(V1:V9)[!str_detect(pict, "col|sha"), ] %>% as.numeric()),
    col = list(across(V1:V9)[str_detect(pict, "col"), ] %>% as.numeric()),
    sha = list(across(V1:V9)[str_detect(pict, "sha"), ] %>% as.numeric()),
    .groups = "drop"
  )

# ----------------------------
# Step 3: Compute within-category distances
# ----------------------------
diffinnovs <- stimulus_table %>%
  group_by(type) %>%
  group_modify(function(df, key) {
    
    n <- nrow(df)
    
    map_dfr(1:n, function(i) {
      
      # distances from stimulus i to all others in same category
      distances <- map_dbl(1:n, function(j) {
        
        if (i == j) return(NA_real_)  # skip self
        
        arr_dist <- hamming_dist(df$arr[[i]], df$arr[[j]])
        col_dist <- hamming_dist(df$col[[i]], df$col[[j]])
        sha_dist <- hamming_dist(df$sha[[i]], df$sha[[j]])
        
        arr_dist + col_dist + sha_dist
      })
      
      tibble(
        stimulus_id = df$base_id[i],
        dmean = mean(distances, na.rm = TRUE),
        dalls = sum(distances, na.rm = TRUE)
      )
    })
  }) %>%
  ungroup()

# ----------------------------
# Step 4: Reattach type column (clean output)
# ----------------------------
diffinnovs <- diffinnovs %>%
  left_join(
    stimulus_table %>% select(base_id, type),
    by = c("stimulus_id" = "base_id")
  )

diffinnovs <- diffinnovs %>%
  select(-type.y) %>%      # drop duplicate
  rename(type = type.x)    # rename remaining column

rm(stimuli, stimulus_table)
