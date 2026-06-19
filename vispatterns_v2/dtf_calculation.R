# Main computation:
# Iterate over all prototype IDs (1–10)
results <- map_dfr(1:10, function(proto_id) {
  
  # ----------------------------
  # Step 1: Extract prototype data
  # ----------------------------
  # Each prototype has three representations:
  # - arrangement (base)
  # - color ("col")
  # - shape ("sha")
  
  proto_arr <- picdat %>%
    filter(pict == paste0("prototype", proto_id))
  
  proto_col <- picdat %>%
    filter(pict == paste0("prototype", proto_id, "col"))
  
  proto_sha <- picdat %>%
    filter(pict == paste0("prototype", proto_id, "sha"))
  
  # ----------------------------
  # Step 2: Extract comparison stimuli
  # ----------------------------
  # Select all rows belonging to the current prototype condition
  # (pattern like "p{id}d...")
  
  comp_data <- picdat %>%
    filter(str_detect(pict, paste0("p", proto_id, "d")))
  
  # Each comparison stimulus consists of 3 consecutive rows:
  # 1 = arrangement, 2 = color, 3 = shape
  # We create group indices to bundle them together
  
  comp_groups <- comp_data %>%
    mutate(group = rep(1:(n()/3), each = 3)) %>%
    group_split(group)
  
  # ----------------------------
  # Step 3: Compute distances
  # ----------------------------
  # For each comparison stimulus:
  # - extract its 3 feature types
  # - compute distances to the prototype
  # - store results in a tidy format
  
  map_dfr(comp_groups, function(group_df) {
    
    # Name of the comparison stimulus
    comp_name <- group_df$pict[1]
    
    # Extract the three feature representations
    comp_arr <- group_df[1, ]
    comp_col <- group_df[2, ]
    comp_sha <- group_df[3, ]
    
    # ----------------------------
    # Step 4: Compute feature-wise Hamming distances
    # ----------------------------
    # Compare prototype vs. comparison on each feature type
    # Columns 2:10 contain the feature vectors
    
    color_distance  <- hamming_dist(proto_col[2:10], comp_col[2:10])
    shape_distance  <- hamming_dist(proto_sha[2:10], comp_sha[2:10])
    arrangement_distance <- hamming_dist(proto_arr[2:10], comp_arr[2:10])
    
    # Total distance is the sum across all feature types
    total_distance <- color_distance + shape_distance + arrangement_distance
    
    # ----------------------------
    # Step 5: Store results
    # ----------------------------
    # Return a tidy row for this comparison
    
    tibble(
      prototype_id = proto_id,                # which prototype
      stimulus_id = comp_name,            # which comparison stimulus
      total_distance = total_distance,        # overall Hamming distance
      color_distance = color_distance,        # color differences
      shape_distance = shape_distance,        # shape differences
      arrangement_distance = arrangement_distance # arrangement differences
    )
  })
})

# algorithm generated 10 prototypes
# 5 of those are used as the most different images in the experiment
# these are compared to the 5 original prototypes to yield a difference score

difftoprotda <- map_dfr(1:5, function(proto_id) {
  
  # ----------------------------
  # Step 1: Prototype (from 1–5)
  # ----------------------------
  proto_arr <- picdat %>%
    filter(pict == paste0("prototype", proto_id))
  
  proto_col <- picdat %>%
    filter(pict == paste0("prototype", proto_id, "col"))
  
  proto_sha <- picdat %>%
    filter(pict == paste0("prototype", proto_id, "sha"))
  
  # ----------------------------
  # Step 2: Comparison set (from 6–10)
  # ----------------------------
  # Note: uses proto_id + 5 (your original logic)
  
  comp_data <- picdat %>%
    filter(str_detect(pict, paste0("p", proto_id + 5, "d")))
  
  # Group rows in triples (arr, col, sha)
  comp_groups <- comp_data %>%
    mutate(group = rep(1:(n()/3), each = 3)) %>%
    group_split(group)
  
  # ----------------------------
  # Step 3: Compute distances
  # ----------------------------
  map_dfr(comp_groups, function(group_df) {
    
    comp_name <- group_df$pict[1]
    
    comp_arr <- group_df[1, ]
    comp_col <- group_df[2, ]
    comp_sha <- group_df[3, ]
    
    # Feature-wise Hamming distances
    color_distance  <- hamming_dist(proto_col[2:10], comp_col[2:10])
    shape_distance  <- hamming_dist(proto_sha[2:10], comp_sha[2:10])
    arrangement_distance <- hamming_dist(proto_arr[2:10], comp_arr[2:10])
    
    total_distance <- color_distance + shape_distance + arrangement_distance
    
    # Output row
    tibble(
      prototype_id = proto_id,
      stimulus_id = comp_name,
      total_distance = total_distance,
      color_distance = color_distance,
      shape_distance = shape_distance,
      arrangement_distance = arrangement_distance
    )
  })
})

results <- results %>%
  rows_update(difftoprotda, by = "stimulus_id")

# categorical variable added to the data D1 - D5
patts <- c("p6", "p7", "p8", "p9", "p10")

difftoprot <- results %>%
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

rm(difftoprotda,results)


