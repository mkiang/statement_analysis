## Imports ----
library(tidyverse)
library(here)
library(ggsci)

## Import data ----
summary_df <-
    readRDS(here::here("data_private", "keyword_summary_df.RDS"))
analytic_df <-
    readRDS(here::here("data_private", "analytic_df.RDS"))

## CONSTANTS ----
t_faces <-
    rev(
        c(
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
        )
    )

## Figure 1 PRIVATE. Keywords by institution, named ----
priv_df <-  summary_df %>%
    dplyr::left_join(analytic_df %>%
                         dplyr::select(institution_id = random_id, institution))

labels_df <- priv_df %>%
    dplyr::select(p_rank, institution) %>%
    dplyr::distinct() %>%
    dplyr::arrange(p_rank)

priv_df <- priv_df %>%
    dplyr::mutate(
        p_name = factor(
            p_rank,
            levels = labels_df$p_rank,
            labels = labels_df$institution,
            ordered = TRUE
        )
    )

p1_private <- ggplot2::ggplot(
    priv_df,
    ggplot2::aes(
        x = p_name,
        y = criterion_cat,
        fill = interaction(category),
        alpha = interaction(category_type, value)
    )
) +
    ggplot2::geom_tile(color = "white", size = .4) +
    ggplot2::scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete("Institution",
                              expand = c(0, 0)) +
    ggplot2::scale_y_discrete(NULL, expand = c(0, 0)) +
    ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_rect(fill = NA, color = "black"),
        axis.text.x = ggplot2::element_text(
            angle = 90,
            hjust = 1,
            vjust = .5
        ),
        axis.text.y = ggplot2::element_text(face = t_faces,
                                            hjust = 1,
                                            size = 10),
        axis.title.x = ggplot2::element_text(size = 11),
        panel.grid = ggplot2::element_blank(),
    ) +
    ggsci::scale_fill_aaas()

ggplot2::ggsave(
    here::here("plots_private", "fig1_private.pdf"),
    p1_private,
    device = grDevices::cairo_pdf,
    width = 10,
    height = 8.5,
    scale = 1.1
)
