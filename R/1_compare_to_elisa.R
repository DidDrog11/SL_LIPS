LIPS_data <- read_rds(here("data", "LIPS_data.rds"))

ELISA <- readRDS(gzcon(url("https://github.com/DidDrog11/SL_lassa_ELISA/raw/main/output/ELISA_output.rds")))

elisa_results <- ELISA$ELISA_enriched %>%
  select(rodent_uid, sample = blood_sample_id, elisa_result = result, elisa_interpretation = interpretation) %>%
  group_by(rodent_uid, sample) %>%
  mutate(elisa_interpretation = factor(elisa_interpretation, levels = c("Positive", "Negative", "Equivocal"))) %>%
  arrange(elisa_interpretation) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(rodent_uid)

LIPS_ELISA_combined <- LIPS_data %>%
  mutate(sample_id = as.numeric(str_remove_all(str_sub(sample, start = 1, end = 3), "_"))) %>%
  left_join(elisa_results %>%
              mutate(sample_id = as.numeric(sample)) %>%
              select(-sample), by = "sample_id") %>%
  mutate(sample = coalesce(as.character(sample_id), sample)) %>%
  mutate(control = case_when(is.na(sample) ~ NA,
                             str_detect(sample, "_pc|_nc|elisa") ~ TRUE,
                             TRUE ~ FALSE),
         elisa_interpretation = factor(case_when(control == TRUE & str_detect(sample, "_pc") ~ "Positive Control",
                                          control == TRUE & str_detect(sample, "_nc") ~ "Negative Control",
                                          !is.na(sample) & is.na(rodent_uid) ~ "Unknown",
                                          elisa_interpretation == "Equivocal" ~ "Unknown",
                                          TRUE ~ as.character(elisa_interpretation))),
         elisa_interpretation = factor(elisa_interpretation, levels = c("Positive Control", "Negative Control", "Positive", "Negative", "Unknown")),
         species = case_when(str_detect(sample, "c_[0-9]") ~ "Cat",
                                TRUE ~ "Rodent"),
         construct = case_when(run == 6 & row %in% c("a", "b") ~ "LFV3",
                               run == 6 & row %in% c("c", "d") ~ "Glyco no IHD",
                               run == 6 & row %in% c("e", "f") ~ "LUC with stop",
                               TRUE ~ "NP-LUC")) %>%
  group_by(sample, construct) %>%
  mutate(mean_result = mean(result),
         sd_result = sd(result)) %>%
  mutate(plot_value = case_when(control == FALSE ~ mean_result,
                                control == TRUE ~ result),
         plot_lci = case_when(control == FALSE ~ mean_result - sd_result,
                              control == TRUE ~ result),
         plot_uci = case_when(control == FALSE ~ mean_result + sd_result,
                              control == TRUE ~ result)) %>%
  drop_na(sample)

ggplot(LIPS_ELISA_combined %>%
         drop_na(sample) %>%
         filter(!run %in% c(1, 2))) +
  geom_point(aes(x = elisa_interpretation, y = plot_value, shape = run, colour = species), 
             position = position_jitter(height = 0, width = 0.2, seed = 123),
             alpha = 0.5) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(x = "ELISA results",
       y = "Luminance",
       colour = "Species",
       shape = "Run",
       title = "With ELISA comparison") +
  facet_wrap(~ construct) +
  annotation_logticks(sides = "l") +
  geom_hline(yintercept = 2.02E+03)

write_csv(LIPS_ELISA_combined, file = here("data", "lips_elisa_combined.csv"))

