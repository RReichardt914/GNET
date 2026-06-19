# ----------------------------
# Step 1: Prepare data
# ----------------------------
picdat_clean <- picdat %>%
  mutate(
    base_id = str_remove(pict, "col$|sha$")
  )

# ----------------------------
# Step 2: Assign type (D1–D5)
# ----------------------------
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

# ----------------------------
# Step 3: Assign phase
# ----------------------------
picdat_clean <- picdat_clean %>%
  mutate(
    phase = case_when(
      str_detect(pict, "d[1-4]_[1-2]") ~ "study",
      str_detect(pict, "d[1-4]_[3-4]") ~ "test",
      TRUE ~ NA_character_
    )
  )

# ----------------------------
# Step 4: Remove prototypes
# ----------------------------
stimuli <- picdat_clean %>%
  filter(!str_detect(pict, "^prototype"))

# ----------------------------
# Step 5: Build stimulus-level table
# ----------------------------
stimulus_table <- stimuli %>%
  group_by(base_id, type, phase) %>%
  summarise(
    arr = list(across(V1:V9)[!str_detect(pict, "col|sha"), ] %>% as.numeric()),
    col = list(across(V1:V9)[str_detect(pict, "col"), ] %>% as.numeric()),
    sha = list(across(V1:V9)[str_detect(pict, "sha"), ] %>% as.numeric()),
    .groups = "drop"
  )

# ----------------------------
# Step 6: Compute distances within type + phase
# ----------------------------
diffinnovs2 <- stimulus_table %>%
  filter(!is.na(type), !is.na(phase)) %>%
  group_by(type, phase) %>%
  group_modify(function(df, key) {
    
    n <- nrow(df)
    
    map_dfr(1:n, function(i) {
      
      distances <- map_dbl(1:n, function(j) {
        
        if (i == j) return(NA_real_)
        
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

rm(stimuli,stimulus_table)
