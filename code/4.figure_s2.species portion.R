rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)

analysis_dir <- "E:/个人文件/CLI/校园植物文章大修"

data_dir <- file.path(analysis_dir, "./分析与出图代码")
out_data_dir <- file.path(analysis_dir, "./分析与出图代码/剔除园艺植物分析数据")
out_result_dir <- file.path(analysis_dir, "./分析与出图代码/剔除园艺植物分析结果")

checklist <- read_csv(
  file.path(data_dir, "./data-old/Campus_checklist_status.csv"),
  show_col_types = FALSE
)

horticultural_list <- read_csv(
  file.path(analysis_dir, "008.cultivated.plant-accepted_matches02_20260313dong.debug(2).csv"),
  show_col_types = FALSE
)

clean_id <- function(x) {
  sub("\\.0$", "", trimws(as.character(x)))
}

horticultural_ids <- unique(clean_id(na.omit(horticultural_list$accepted_plant_name_id)))
horticultural_names <- unique(trimws(na.omit(horticultural_list$wcvp_name)))

checklist_matched <- checklist |>
  mutate(
    accepted_id_clean = clean_id(accepted_plant_name_id),
    is_horticultural = accepted_id_clean %in% horticultural_ids |
      trimws(taxon_name) %in% horticultural_names,
    plant_group = recode(
      status,
      native = "Native species",
      alien = "Non-invasive species",
      invasive = "Invasive species"
    )
  )

# Unique-species summary: a species is counted once in each plant group.
species_by_group <- checklist_matched |>
  distinct(plant_group, taxon_name, is_horticultural) |>
  count(plant_group, is_horticultural, name = "species_n")

species_all <- checklist_matched |>
  distinct(taxon_name, is_horticultural) |>
  count(is_horticultural, name = "species_n") |>
  mutate(plant_group = "All species")

species_summary <- bind_rows(species_all, species_by_group) |>
  group_by(plant_group) |>
  mutate(
    total_species = sum(species_n),
    proportion = species_n / total_species
  ) |>
  ungroup()

summary_table <- species_summary |>
  filter(is_horticultural) |>
  transmute(
    group = plant_group,
    horticultural_species = species_n,
    total_species,
    horticultural_percentage = round(proportion * 100, 1)
  )

# Campus-level percentages use unique species within each campus.
campus_summary <- checklist_matched |>
  distinct(univ.links.uni.abbrev02, taxon_name, is_horticultural) |>
  group_by(univ.links.uni.abbrev02) |>
  summarise(
    horticultural_species = sum(is_horticultural),
    total_species = n(),
    horticultural_proportion = horticultural_species / total_species,
    .groups = "drop"
  ) |>
  arrange(horticultural_proportion) |>
  mutate(campus_rank = row_number())

overall_records <- checklist_matched |>
  summarise(
    horticultural_records = sum(is_horticultural),
    total_records = n(),
    horticultural_percentage = round(mean(is_horticultural) * 100, 1)
  )

write_csv(
  summary_table,
  file.path(out_data_dir, "校园植物园艺属性总体汇总表.csv")
)
write_csv(
  campus_summary,
  file.path(out_data_dir, "各校园园艺植物物种占比.csv")
)
write_csv(
  overall_records,
  file.path(out_data_dir, "校园植物园艺属性记录汇总表.csv")
)

group_levels <- c(
  "All species",
  "Native species",
  "Non-invasive species",
  "Invasive species"
)
text_size_pt <- 14
text_size_mm <- text_size_pt / 2.845276

fill_colors <- c(
  "Horticultural-list species" = "#2F6B8A",
  "Non-horticultural-list species" = "#D9D9D9"
)

plot_a_data <- species_summary |>
  mutate(
    plant_group = factor(plant_group, levels = rev(group_levels)),
    category = if_else(
      is_horticultural,
      "Horticultural-list species",
      "Non-horticultural-list species"
    ),
    label = if_else(
      is_horticultural,
      paste0(species_n, " (", percent(proportion, accuracy = 0.1), ")"),
      ""
    )
  )

p_a <- ggplot(plot_a_data, aes(x = proportion, y = plant_group, fill = category)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.25) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = text_size_mm,
    colour = "white"
  ) +
  scale_fill_manual(values = fill_colors, breaks = names(fill_colors)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Proportion of unique species",
    y = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 14, base_family = "sans") +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.key.width = grid::unit(3, "mm"),
    legend.spacing.x = grid::unit(2, "mm"),
    legend.spacing.y = grid::unit(0.5, "mm"),
    legend.text = element_text(size = 14),
    plot.margin = margin(5, 15, 5, 5)
  )

shared_legend <- cowplot::get_legend(p_a)
p_a <- p_a + theme(legend.position = "none")

campus_median <- median(campus_summary$horticultural_proportion)
campuses_above_half <- sum(campus_summary$horticultural_proportion > 0.5)

p_b <- ggplot(
  campus_summary,
  aes(x = campus_rank, y = horticultural_proportion)
) +
  geom_line(linewidth = 0.3, colour = "#88AFC2") +
  geom_point(size = 0.7, alpha = 0.8, colour = "#2F6B8A") +
  annotate(
    "label",
    x = 108,
    y = 0.67,
    label = paste0(
      "Median = ",
      percent(campus_median, accuracy = 0.1)
    ),
    hjust = 1,
    size = text_size_mm,
    linewidth = 0.25,
    fill = "white"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = c(1, 25, 50, 75, 100, 111),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Campuses ranked by horticultural-species proportion",
    y = "Horticultural-list species"
  ) +
  theme_classic(base_size = 14, base_family = "sans") +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.margin = margin(5, 15, 5, 8)
  )

panel_plot <- cowplot::plot_grid(
  p_a,
  NULL,
  p_b,
  labels = c("a", "", "b"),
  label_fontfamily = "sans",
  label_fontface = "bold",
  label_size = 14,
  nrow = 1,
  rel_widths = c(1.05, 0.06, 1.35)
)

combined_plot <- cowplot::plot_grid(
  shared_legend,
  panel_plot,
  ncol = 1,
  rel_heights = c(0.10, 1)
)

base_file <- file.path(out_result_dir, "校园植物园艺属性占比02")
width_in <- 250 / 25.4
height_in <- 120 / 25.4

ggsave(
  paste0(base_file, ".png"),
  combined_plot,
  width = width_in,
  height = height_in,
  dpi = 300,
  bg = "white"
)

svglite::svglite(
  paste0(base_file, ".svg"),
  width = width_in,
  height = height_in
)
print(combined_plot)
dev.off()

grDevices::cairo_pdf(
  paste0(base_file, ".pdf"),
  width = width_in,
  height = height_in,
  family = "sans"
)
print(combined_plot)
dev.off()

ragg::agg_tiff(
  paste0(base_file, ".tiff"),
  width = width_in,
  height = height_in,
  units = "in",
  res = 300,
  background = "white",
  compression = "lzw"
)
print(combined_plot)
dev.off()

message("Overall unique-species percentage: ",
        percent(
          summary_table$horticultural_percentage[
            summary_table$group == "All species"
          ] / 100,
          accuracy = 0.1
        ))
message("Campus median percentage: ", percent(campus_median, accuracy = 0.1))
message("Campuses above 50%: ", campuses_above_half, "/", nrow(campus_summary))
message("Outputs written to: ", out_result_dir)
