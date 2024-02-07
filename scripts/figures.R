#> -------------------------------------------
#> AUTONOMY SUPPORTIVE INSTRUCTIONAL LANGUAGE
#> -- St. Germain, McKay, Tandon, Seedu, Barbera,
#>    Carrillo, Brown, and Carter
#>
#> Figures
#>
#> Authors:
#>   Laura St. Germain
#>   Michael Carter
#>
#> Last update: May 16 2023
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------
#>
#> Pull from analysis script
source("scripts/analysis.R")

#> Colour and theme setup
colour_scheme <- c("#d08770", "#5e81ac")

theme_set(
  theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold")
    )
)

#> PRIMARY ANALYSIS - SHIFT FUNCTION - FIGURE 1 ----
#>
#> Scatterplot
learn_ps <- rogme::plot_scat2(
  data = learn_shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "",
  alpha = 1,
  shape = 21,
  color = "grey10",
  fill = "grey90"
) + coord_flip()
learn_ps

#> Plot shift function
learn_psf <- rogme::plot_sf(
  learn_sf,
  plot_theme = 2,
  symb_fill = colour_scheme,
  diffint_col = colour_scheme,
  theme2_alpha = c(1, 1)
)
learn_psf

#> Create scatterplots with colour coded differences
learn_shift_p <- rogme::plot_scat2(
  learn_shift,
  xlabel = "",
  ylabel = "Stacking time (sec)",
  alpha = 0.3,
  shape = 21,
  color = "grey10",
  fill = "grey90"
)

learn_shift_p <- rogme::plot_hd_links(
  learn_shift_p,
  learn_sf[[1]],
  q_size = 1,
  md_size = 1.5,
  link_col = colour_scheme,
  add_rect = TRUE,
  rect_alpha = 0.1,
  rect_col = "grey50",
  add_lab = TRUE,
  text_size = 5
) +
  coord_flip()
learn_shift_p

fig1 <- learn_ps +
  learn_shift_p +
  learn_psf[[1]] +
  patchwork::plot_layout(ncol = 1) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        plot.margin = margin(t = 0.25,
                             r = 0.25,
                             b = 0.25,
                             l = 0.25,
                             unit = "cm"))
fig1

#> PHYSICAL PERFORMANCE ----
#>
#> All blocks
#>
fig2_inset <- ggplotGrob(ggplot2::ggplot(adj_ret_g_tib,
                                         aes(x = group_id,
                                             y = mean_adj_mt,
                                             color = group_id,
                                             shape = group_id)) +
                           geom_pointrange(aes(ymin = ci_low,
                                               ymax = ci_upp),
                                           size = 1,
                                           linewidth = 1) +
                           scale_x_discrete(name = NULL,
                                            labels = c("1" = "Autonomy-support",
                                                       "2" = "Controlling")) +
                           scale_y_continuous(name = "Stacking time (sec) \nadjusted for pre-test",
                                              limits = c(9.6, 10.4),
                                              breaks = seq(9.4, 10.4, 0.2)) +
                           scale_color_manual(values = colour_scheme,
                                              labels = c("Autonomy-support",
                                                         "Controlling")) +
                           scale_shape_manual(values = c(16, 15),
                                              labels = c("Autonomy-support",
                                                         "Controlling")) +
                           theme(plot.background = element_rect(colour = "black"),
                                 legend.position = "none"))

fig2 <- ggplot2::ggplot(motor_8blocks_g_tib,
                        aes(x = block_id_expt,
                            y = g_mean_mt,
                            group = interaction(group_id, phase_id),
                            color = group_id,
                            shape = group_id)) +
  geom_pointrange(aes(ymin = g_ci_low,
                      ymax = g_ci_upp),
                  position = position_dodge(0.6),
                  size = 1,
                  linewidth = 1) +
  geom_line(aes(linetype = group_id,
                group = interaction(group_id, phase_id)),
            show.legend = FALSE,
            position = position_dodge(0.6),
            linewidth = 1) +
  scale_y_continuous(name = "Stacking time (sec)",
                     limits = c(8, 16),
                     breaks = seq(8, 16, 2)) +
  scale_x_discrete(name = "Block",
                   labels = c("1" = "Pre",
                              "2" = "Acq1",
                              "3" = "Acq2",
                              "4" = "Acq3",
                              "5" = "Acq4",
                              "6" = "Acq5",
                              "7" = "Acq6",
                              "8" = "Ret")) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_linetype_manual(values = c(1, 3),
                        labels = c("Autonomy-support",
                                   "Controlling")) +
  theme(legend.position = "none") +
  annotation_custom(grob = fig2_inset,
                    xmin = 5,
                    xmax = 8.5,
                    ymin = 12.5,
                    ymax = 16)
fig2

#> QUESTIONNAIRES ----
#>
#> Make figures
#> Perceived autonomy
fig3a <- ggplot2::ggplot(pa_adj_tib,
                         aes(x = time_id,
                             y = marg_mean,
                             color = group_id,
                             shape = group_id)) +
  geom_point(data = pa_p_tib,
             aes(x = time_id,
                 y = score),
             position = position_jitterdodge(0.6),
             alpha = 0.22) +
  stat_summary(fun = "mean",
               geom = "segment",
               mapping = aes(xend = ..x.., yend = ..y..),
               position = position_dodge(1.5),
               linewidth = 1) +
  scale_x_discrete(name = NULL,
                   labels = c("t1" = "After \npre-test",
                              "t2" = "After \nacquisition",
                              "t3" = "Before \nretention")) +
  scale_y_continuous(name = "Perceived autonomy",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 8),
        legend.direction = "horizontal",
        legend.position = c(0.5, 0.12))
fig3a

#> Perceived competence
fig3b <- ggplot2::ggplot(pc_adj_tib,
                         aes(x = time_id,
                             y = marg_mean,
                             color = group_id,
                             shape = group_id)) +
  geom_point(data = pc_p_tib,
             aes(x = time_id,
                 y = score),
             position = position_jitterdodge(0.6),
             alpha = 0.22) +
  stat_summary(fun = "mean",
               geom = "segment",
               mapping = aes(xend = ..x.., yend = ..y..),
               position = position_dodge(1.5),
               linewidth = 1) +
  scale_x_discrete(name = NULL,
                   labels = c("t1" = "After \npre-test",
                              "t2" = "After \nacquisition",
                              "t3" = "Before \nretention")) +
  scale_y_continuous(name = "Perceived compentence",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  theme(legend.position = "none")
fig3b

#> Intrinsic motivation
fig3c <- ggplot2::ggplot(im_adj_tib,
                          aes(x = time_id,
                              y = marg_mean,
                              color = group_id,
                              shape = group_id)) +
  geom_point(data = im_p_tib,
             aes(x = time_id,
                 y = score),
             position = position_jitterdodge(0.6),
             alpha = 0.22) +
  stat_summary(fun = "mean",
               geom = "segment",
               mapping = aes(xend = ..x.., yend = ..y..),
               position = position_dodge(1.5),
               linewidth = 1,
  ) +
  scale_x_discrete(name = "Time",
                   labels = c("t1" = "After \npre-test",
                              "t2" = "After \nacquisition",
                              "t3" = "Before \nretention")) +
  scale_y_continuous(name = "Intrinsic motivation",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  theme(legend.position = "none")
fig3c

#> Perceived autonomy and retention
fig3d <- ggplot2::ggplot(ret_qaire_tib,
                         aes(x = adj_pa,
                             y = adj_mt,
                             shape = group_id,
                             color = group_id,
                             fill = group_id)) +
  stat_poly_line() +
  stat_poly_eq(vstep = 0.1) +
  geom_point(size = 2,
             alpha = 0.6) +
  scale_x_continuous(name = "Perceived autonomy adjusted for pre-test",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_y_continuous(name = "Stacking time (s) \nadjusted for pre-test",
                     limits = c(6, 16),
                     breaks = seq(6, 16, 2)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_fill_manual(values = colour_scheme,
                    labels = c("Autonomy-support",
                               "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  theme(legend.position = "none")
fig3d

#> Perceived competence and retention
fig3e <- ggplot2::ggplot(ret_qaire_tib,
                         aes(x = adj_pc,
                             y = adj_mt,
                             shape = group_id,
                             color = group_id,
                             fill = group_id)) +
  stat_poly_line() +
  stat_poly_eq(vstep = 0.1) +
  geom_point(size = 2,
             alpha = 0.6) +
  scale_x_continuous(name = "Perceived competence adjusted for pre-test",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_y_continuous(name = "Stacking time (s) \nadjusted for pre-test",
                     limits = c(6, 16),
                     breaks = seq(6, 16, 2)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_fill_manual(values = colour_scheme,
                    labels = c("Autonomy-support",
                               "Controlling")) +
  theme(legend.position = "none")
fig3e

#> Intrinsic motivation and retention
fig3f <- ggplot2::ggplot(ret_qaire_tib,
                         aes(x = adj_im,
                             y = adj_mt,
                             shape = group_id,
                             color = group_id,
                             fill = group_id)) +
  stat_poly_line() +
  stat_poly_eq(vstep = 0.1) +
  geom_point(size = 2,
             alpha = 0.6) +
  scale_x_continuous(name = "Intrinsic motivation adjusted for pre-test",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_y_continuous(name = "Stacking time (s) \nadjusted for pre-test",
                     limits = c(6, 16),
                     breaks = seq(6, 16, 2)) +
  scale_color_manual(values = colour_scheme,
                     labels = c("Autonomy-support",
                                "Controlling")) +
  scale_fill_manual(values = colour_scheme,
                    labels = c("Autonomy-support",
                               "Controlling")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Autonomy-support",
                                "Controlling")) +
  theme(legend.position = "none")
fig3f

# > Create multipanel figure for questionnaire data
#fig3 <- (fig3a | fig3b | fig3c) / (fig3d | fig3e | fig3f)
fig3 <- (fig3a | fig3d) / (fig3b | fig3e) / (fig3c | fig3f)
fig3 +
  patchwork::plot_annotation(tag_levels = list(c('A', 'D', 'B', 'E', 'C', 'F'))) &
  theme(plot.tag = element_text(size = 16, face = "bold"))
