library(dplyr)

split_df <- read.csv("/Users/gmark/Documents/GitHub/PreclinicalAD_GutMicrobiome-Inflammation/R/Data/CS_guild_metadata.csv") %>% select(c("Sample_ID", "set"))

full_df <- read.csv("/Users/gmark/Documents/GitHub/PreclinicalAD_GutMicrobiome-Inflammation/R/Data/full_guild_metadata.csv")
full_df <- merge(full_df, split_df, by="Sample_ID", all.x = TRUE)

fill_set_column <- function(df) {
  # Identify subjects with values in set column
  subject_values <- df %>%
    filter(!is.na(set)) %>%
    group_by(ID) %>%
    summarise(
      unique_values = list(unique(set)),
      n_unique = n_distinct(set),
      .groups = "drop"
    )
  
  # Check for conflicts (subjects with both train and test)
  conflicts <- subject_values %>%
    filter(n_unique > 1)
  
  if (nrow(conflicts) > 0) {
    warning("The following IDs have multiple set values:\n",
            paste(conflicts$ID, collapse = ", "),
            "\nThese subjects will be skipped.")
  }
  
  # Create mapping: train -> train_long, test -> test_long
  # Exclude subjects with conflicts
  subject_mapping <- subject_values %>%
    filter(n_unique == 1) %>%  # Only subjects with single value
    mutate(new_value = sapply(unique_values, function(x) {
      if ("train" %in% x) return("train_long")
      if ("test" %in% x) return("test_long")
      return(NA_character_)
    })) %>%
    select(ID, new_value)
  
  # Fill in NA values for set column
  df_filled <- df %>%
    left_join(subject_mapping, by = "ID") %>%
    mutate(set = ifelse(is.na(set), new_value, set)) %>%
    select(-new_value)
  
  return(df_filled)
}
# Usage:
df <- fill_set_column(full_df)

write.csv(df, "/Users/gmark/Documents/GitHub/PreclinicalAD_GutMicrobiome-Inflammation/R/Data/full_guild_metadata_train_test_split.csv", row.names = FALSE)

