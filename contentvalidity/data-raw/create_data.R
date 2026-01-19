# Script to create example dataset
# Run this script to regenerate the expert_ratings dataset

# Create expert ratings data
# 10 experts rating 8 items on a 4-point scale
expert_ratings <- data.frame(
  item1 = c(4, 4, 3, 4, 3, 4, 4, 3, 4, 4),  # High agreement
  item2 = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),  # Perfect agreement
  item3 = c(3, 2, 2, 3, 2, 3, 2, 3, 2, 3),  # Mixed ratings
  item4 = c(4, 3, 4, 4, 3, 4, 4, 3, 4, 3),  # Good agreement
  item5 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1),  # Poor ratings
  item6 = c(4, 4, 4, 3, 4, 4, 4, 4, 3, 4),  # High agreement
  item7 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3),  # Perfect agreement (all 3s)
  item8 = c(4, 3, 4, 2, 3, 4, 3, 4, 3, 4)   # Mixed but mostly relevant
)

# Add row names for experts
rownames(expert_ratings) <- paste0("Expert", 1:10)

# Save the dataset
usethis::use_data(expert_ratings, overwrite = TRUE)
