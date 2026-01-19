expert_ratings <- data.frame(
  item1 = c(4, 4, 3, 4, 3, 4, 4, 3, 4, 4),
  item2 = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  item3 = c(3, 2, 2, 3, 2, 3, 2, 3, 2, 3),
  item4 = c(4, 3, 4, 4, 3, 4, 4, 3, 4, 3),
  item5 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1),
  item6 = c(4, 4, 4, 3, 4, 4, 4, 4, 3, 4),
  item7 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
  item8 = c(4, 3, 4, 2, 3, 4, 3, 4, 3, 4)
)
rownames(expert_ratings) <- paste0("Expert", 1:10)
save(expert_ratings, file = "data/expert_ratings.rda")
cat("Dataset created successfully\n")
