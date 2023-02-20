samples <- read_xlsx(here("raw_data", "LIPS_samples_2023-02-16.xlsx")) %>%
  mutate(across(.cols = matches("c_"), .fns = as.character)) %>%
  mutate(across(.cols = matches("c_"), ~ str_replace_all(., "NA", NA_character_))) %>%
  mutate(run = factor(run),
         row = factor(row, levels = c(letters)),
         date = ymd(date)) %>%
  pivot_longer(cols = matches("c_"), names_to = "cell", values_to = "sample") %>%
  mutate(cell = str_remove_all(cell, "c_"))

results <- read_xlsx(here("raw_data", "LIPS_results_2023-02-16.xlsx")) %>%
  mutate(run = factor(run),
         row = factor(row, levels = c(letters)),
         date = ymd(date)) %>%
  pivot_longer(cols = matches("c_"), names_to = "cell", values_to = "result") %>%
  mutate(cell = str_remove_all(cell, "c_"))

LIPS_data <- samples %>%
  left_join(results, by = c("run", "row", "cell", "date")) %>%
  select(date, run, row, cell, sample, result)

write_rds(LIPS_data, here("data", "LIPS_data.rds"))
