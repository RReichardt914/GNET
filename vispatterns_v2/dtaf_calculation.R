# ----------------------------
# Step 1: Extract prototypes (1–5 only)
# ----------------------------
prototype_ids <- 1:5

proto_arr <- map_dfr(prototype_ids, ~
                       picdat %>% filter(pict == paste0("prototype", .x))
)

proto_col <- map_dfr(prototype_ids, ~
                       picdat %>% filter(pict == paste0("prototype", .x, "col"))
)

proto_sha <- map_dfr(prototype_ids, ~
                       picdat %>% filter(pict == paste0("prototype", .x, "sha"))
)

# ----------------------------
# Step 2: Split stimuli into triplets (arr, col, sha)
# ----------------------------

picdat_clean <- picdat %>%
  mutate(base_id = str_remove(pict, "col$|sha$"))

# exclude prototypes completely
stimulus_groups <- picdat_clean %>%
  filter(!str_detect(pict, "^prototype")) %>%
  group_by(base_id) %>%
  group_split()

# ----------------------------
# Step 3: Compare each stimulus to all prototypes
# ----------------------------

difftoprots <- map_dfr(stimulus_groups, function(stim) {
  
  stimulus_id <- unique(stim$base_id)
  
  comp_arr <- stim %>% filter(!str_detect(pict, "col|sha"))
  comp_col <- stim %>% filter(str_detect(pict, "col"))
  comp_sha <- stim %>% filter(str_detect(pict, "sha"))
  
  distances <- map_dbl(1:5, function(i) {
    
    arr_dist <- hamming_dist(proto_arr[i, 2:10], comp_arr[2:10])
    col_dist <- hamming_dist(proto_col[i, 2:10], comp_col[2:10])
    sha_dist <- hamming_dist(proto_sha[i, 2:10], comp_sha[2:10])
    
    arr_dist + col_dist + sha_dist
  })
  
  tibble(
    stimulus_id = stimulus_id,
    mean_distance = mean(distances),
    sum_distance  = sum(distances)
  )
})



# ----------------------------
# Step 4: Add stimulus type labels
# ----------------------------
patts <- c("p6", "p7", "p8", "p9", "p10")

difftoprots <- difftoprots %>%
  mutate(
    type = case_when(
      str_detect(stimulus_id, paste(patts, collapse = "|")) ~ "D5",
      str_detect(stimulus_id, "p[1-5]d4") ~ "D4",
      str_detect(stimulus_id, "p[1-5]d3") ~ "D3",
      str_detect(stimulus_id, "p[1-5]d2") ~ "D2",
      str_detect(stimulus_id, "p[1-5]d1") ~ "D1",
      TRUE ~ NA_character_
    )
  )

rm(proto_arr, proto_col, proto_sha, stimulus_groups)