## Imports ----
library(tidyverse)
library(here)
library(ggsci)

## Import data ----
summary_df <-
    readRDS(here::here("data_private", "social_mission_keyword_summary_df.RDS"))

## CONSTANTS ----
t_faces <-
    rev(c(
        "bold",
        rep("plain", 3),
        "bold",
        rep("plain", 2),
        "bold",
        rep("plain", 3),
        "bold",
        rep("plain", 4),
        "bold",
        rep("plain", 3),
        "bold",
        rep("plain", 2),
        "bold",
        rep("plain", 4),
        "bold",
        rep("plain", 8)
        ))

## Figure 1. Keywords by institution, ranked ----
p1 <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
        x = p_rank,
        y = criterion_cat,
        fill = interaction(category),
        alpha = interaction(category_type, value)
    )
) +
    ggplot2::geom_tile(color = "white", size = .4) +
    ggplot2::scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous("Institution", expand = c(0, 0)) +
    ggplot2::scale_y_discrete(NULL, expand = c(0, 0)) +
    ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_rect(fill = NA, color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(face = t_faces,
                                            hjust = 1,
                                            size = 10),
        axis.title.x = ggplot2::element_text(size = 11),
        panel.grid = ggplot2::element_blank(),
    ) +
    ggsci::scale_fill_aaas()

## Figure 1a. Keywords by institution, random_id ----
p1a <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
        x = institution_id,
        y = criterion_cat,
        fill = interaction(category),
        alpha = interaction(category_type, value)
    )
) +
    ggplot2::geom_tile(color = "white", size = .4) +
    ggplot2::scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous("Institution", expand = c(0, 0)) +
    ggplot2::scale_y_discrete(NULL, expand = c(0, 0)) +
    ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_rect(fill = NA, color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(face = t_faces,
                                            hjust = 1,
                                            size = 10),
        axis.title.x = ggplot2::element_text(size = 11),
        panel.grid = ggplot2::element_blank(),
    ) +
    ggsci::scale_fill_aaas()

## Save ----
ggplot2::ggsave(
    here::here("plots", "social_mission_fig1.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 5,
    height = 5,
    scale = 1.2
)

ggplot2::ggsave(
    here::here("plots", "social_mission_fig1.jpg"),
    p1,
    dpi = 300,
    width = 5,
    height = 5,
    scale = 1.2
)

ggplot2::ggsave(
    here::here("plots", "social_mission_fig1a_random_id.pdf"),
    p1a,
    device = grDevices::cairo_pdf,
    width = 5,
    height = 5,
    scale = 1.2
)

ggplot2::ggsave(
    here::here("plots", "social_mission_fig1a_random_id.jpg"),
    p1a,
    dpi = 300,
    width = 5,
    height = 5,
    scale = 1.2
)
