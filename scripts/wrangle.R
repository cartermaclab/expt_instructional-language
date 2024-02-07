#> -------------------------------------------
#> AUTONOMY SUPPORTIVE INSTRUCTIONAL LANGUAGE
#> -- St. Germain, McKay, Tandon, Seedu, Barbera,
#>    Carrillo, Brown, and Carter
#>
#> Wrangle
#>
#> Authors:
#>   Laura St. Germain
#>   Michael Carter
#>
#> Last update: September 24 2023
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Load required libraries
source("scripts/libraries.R")

#> Load data files
motor_data <- readr::read_csv("data/performance.csv")
qaire_data <- readr::read_csv("data/questionnaire.csv")
cronbach_data <- readr::read_csv("data/cronbach.csv")


#> DATA SETUP - BEHAVIOUR ----
#>
#> Create workable tibble and add columns for blocks within phases and
#> non-resetting blocks and trials
motor_tib <- motor_data %>%
  dplyr::mutate(block_id = rep(c(1, 1:6, 1), each = 5, 156), .after = 4) %>%
  dplyr::mutate(block_id_expt = rep(rep(1:8, each = 5), 156), .after = 5) %>%
  dplyr::mutate(trial_expt = rep(1:40, 156), .after = 6)

#> Make factors
motor_tib <- motor_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id),
    block_id = forcats::as_factor(block_id),
    block_id_expt = forcats::as_factor(block_id_expt),
  )
dplyr::glimpse(motor_tib)

#> Tibble for all experimental blocks at the participant (p)
#> and group (g) levels
motor_8blocks_p_tib <- motor_tib %>%
  dplyr::group_by(sub_id, group_id, phase_id, block_id, block_id_expt) %>%
  dplyr::summarize(
    n = n(),
    p_mean_mt = mean(mt, na.rm = TRUE),
    p_sd_mt = sd(mt, na.rm = TRUE),
    p_ci_low = ggplot2::mean_cl_normal(mt)$ymin,
    p_ci_upp = ggplot2::mean_cl_normal(mt)$ymax,
    .groups = "drop"
  )

motor_8blocks_g_tib <- motor_8blocks_p_tib %>%
  dplyr::group_by(group_id, phase_id, block_id, block_id_expt) %>%
  dplyr::summarize(
    n = n(),
    g_mean_mt = mean(p_mean_mt),
    g_sd_mt = sd(p_mean_mt),
    g_ci_low = ggplot2::mean_cl_normal(p_mean_mt)$ymin,
    g_ci_upp = ggplot2::mean_cl_normal(p_mean_mt)$ymax,
    .groups = "drop"
  )

#> Getting just the pre-test and retention data at the participant (p)
#> and group (g) levels
learn_p_tib <-  motor_8blocks_p_tib %>%
  dplyr::filter(phase_id != 2) %>%
  dplyr::select(-5)

learn_g_tib <- motor_8blocks_g_tib %>%
  dplyr::filter(phase_id != 2) %>%
  dplyr::select(-c(3, 4))

#> Getting 20% trimmed retention means adjustsed for pre-test at the
#> participant (p) level for primary shift function
learn_trim_p <- motor_tib %>%
  dplyr::filter(phase_id != 2) %>%
  dplyr::group_by(sub_id, group_id, phase_id, block_id) %>%
  dplyr::summarise(
    n = n(),
    p_mean_trim = mean(mt, trim = 0.2, na.rm = TRUE),
    .groups = "drop"
  )

#> Make trimmed learning tibble wide format
learn_trim_p_wide <- learn_trim_p %>%
  dplyr::select(sub_id, group_id, phase_id, p_mean_trim) %>%
  tidyr::pivot_wider(names_from = phase_id, values_from = p_mean_trim) %>%
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4)

#> Fitting linear regression to get predicted retention values
retention_trim_model <- lm(ret ~ pre, learn_trim_p_wide)

#> Create tibble with predicted trimmed retention values at the participant (p)
#> level, adding a column for sub_id and group_id
adj_ret_trim_tib <- as_tibble(retention_trim_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_mt_trim = value)

#> Getting retention means adjusted for pre-test at the participant (p) level
#> for secondary t-test
#> First, make learning tibble wide format
learn_wide_p <- learn_p_tib %>%
  dplyr::select(sub_id, group_id, phase_id, p_mean_mt) %>%
  tidyr::pivot_wider(names_from = phase_id, values_from = p_mean_mt) %>%
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4)

#> Fitting linear regression to get predicted retention values
retention_model <- lm(ret ~ pre, learn_wide_p)

#> Create tibble with predicted retention values at the participant (p) and
#> group (g) levels, adding a column for sub_id, and group_id
adj_ret_p_tib <- as_tibble(retention_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_mt = value)

#> Create factors
adj_ret_p_tib <- adj_ret_p_tib %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

adj_ret_g_tib <- adj_ret_p_tib %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarise(
    mean_adj_mt = mean(adj_mt),
    sd_adj_mt = sd(adj_mt),
    ci_low = ggplot2::mean_cl_normal(adj_mt)$ymin,
    ci_upp = ggplot2::mean_cl_normal(adj_mt)$ymax
  )

#> Create tibble with acquisition data at the participant (p) level
acq_p_tib <- motor_8blocks_p_tib %>%
  dplyr::filter(phase_id == 2)


#> DATA SETUP - PSYCHOLOGICAL ----
#>
#> Make tibble long format
qaire_tib <- qaire_data %>%
  tidyr::pivot_longer(
    cols = c(3:11),
    names_to = c("time_id", "scale_id"),
    names_sep = "_",
    values_to = "score"
  )

#> Make factors
qaire_tib <- qaire_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    time_id = forcats::as_factor(time_id),
    scale_id = forcats::as_factor(scale_id)
  )

#> Tibbles for perceived autonomy (pa), perceived competence (pc), and
#> intrinsic motivation (im) at participant (p) and group (g) levels
#>
pa_p_tib <- qaire_tib %>%
  dplyr::filter(scale_id == "pa")

pa_g_tib <- pa_p_tib %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    n = n(),
    mean_score = mean(score),
    sd_score = sd(score),
    ci_low = ggplot2::mean_cl_normal(score)$ymin,
    ci_upp = ggplot2::mean_cl_normal(score)$ymax,
    .groups = "drop"
  )

pc_p_tib <- qaire_tib %>%
  dplyr::filter(scale_id == "pc")

pc_g_tib <- pc_p_tib %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    n = n(),
    mean_score = mean(score),
    sd_score = sd(score),
    ci_low = ggplot2::mean_cl_normal(score)$ymin,
    ci_upp = ggplot2::mean_cl_normal(score)$ymax,
    .groups = "drop"
  )

im_p_tib <- qaire_tib %>%
  dplyr::filter(scale_id == "im")

im_g_tib <- im_p_tib %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    n = n(),
    mean_score = mean(score),
    sd_score = sd(score),
    ci_low = ggplot2::mean_cl_normal(score)$ymin,
    ci_upp = ggplot2::mean_cl_normal(score)$ymax,
    .groups = "drop"
  )

#> Tibbles at the participant level for perceived autonomy (pa), intrinsic
#> motivation (im), and perceived competence (pc) including a separate
#> column for covariate (t1)
#>
pa_cov_tib <- pa_p_tib %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id,
                     values_from = score) %>%
  tidyr::pivot_longer(cols = c(t2, t3),
                      names_to = "time_id",
                      values_to = "score")

pc_cov_tib <- pc_p_tib %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id,
                     values_from = score) %>%
  tidyr::pivot_longer(cols = c(t2, t3),
                      names_to = "time_id",
                      values_to = "score")

im_cov_tib <- im_p_tib %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id,
                     values_from = score) %>%
  tidyr::pivot_longer(cols = c(t2, t3),
                      names_to = "time_id",
                      values_to = "score")

#> Tribbles including pre-test and pre-test adjusted means for time 2 and time 3
#> for perceived autonomy (pa), perceived competence (pc), and intrinsic
#> motivation (im) from scipts/analysis.R
#>
#> Perceived autonomy
pa_adj_tib <- tibble::tribble(
  ~group_id, ~time_id, ~marg_mean, ~ci_low, ~ci_upp, #> header row
  #>------------------------------------------------
  1, "t1", 4.95, 4.76, 5.13,
  1, "t2", 4.96, 4.83, 5.10,
  1, "t3", 4.84, 4.71, 4.97,
  2, "t1", 4.72, 4.53, 4.91,
  2, "t2", 4.76, 4.62, 4.89,
  2, "t3", 4.70, 4.57, 4.83
)

#> Make group_id a factor
pa_adj_tib <- pa_adj_tib %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Perceived competence
pc_adj_tib <- tibble::tribble(
  ~group_id, ~time_id, ~marg_mean, ~ci_low, ~ci_upp, #> header row
  #>------------------------------------------------
  1, "t1", 4.07, 3.67, 4.28,
  1, "t2", 4.43, 4.28, 4.59,
  1, "t3", 4.32, 4.16, 4.47,
  2, "t1", 3.91, 3.69, 4.13,
  2, "t2", 4.42, 4.27, 4.57,
  2, "t3", 4.35, 4.19, 4.50
)

#> Make group_id a factor
pc_adj_tib <- pc_adj_tib %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> intrinsic motivation
im_adj_tib <- tibble::tribble(
  ~group_id, ~time_id, ~marg_mean, ~ci_low, ~ci_upp, #> header row
  #>------------------------------------------------
  1, "t1", 5.12, 4.90, 5.34,
  1, "t2", 5.31, 5.17, 5.44,
  1, "t3", 5.10, 4.96, 5.24,
  2, "t1", 5.22, 4.98, 5.45,
  2, "t2", 5.29, 5.15, 5.42,
  2, "t3", 5.18, 5.04, 5.32
)

#> Make group_id a factor
im_adj_tib <- im_adj_tib %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Getting pre-test adjusted values on the individual participant level for
#> Before retention
#>
#> Perceived autonomy
#>
#> Getting pre-test adjusted means for each participant
pa_t3 <- pa_p_tib %>%
  dplyr::filter(time_id != "t2")

#> Make wide
pa_t3_wide <- pa_t3 %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id, values_from = score)

#> Fitting linear regression to get predicted retention values
pa_t3_model <- lm(t3 ~ t1, pa_t3_wide)

#> Create a tibble adding columns for sub_id, group_id,
#> and rename column title to "adj_pa"
pa_t3_tib <- as_tibble(pa_t3_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_pa = value) %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Perceived competence
#>
#> Getting pre-test adjusted means for each participant
pc_t3 <- pc_p_tib %>%
  dplyr::filter(time_id != "t2")

#> Make wide
pc_t3_wide <- pc_t3 %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id, values_from = score)

#> Fitting linear regression to get predicted retention values
pc_t3_model <- lm(t3 ~ t1, pc_t3_wide)

#> Create a tibble adding columns for sub_id, group_id,
#> and rename column title to "adj_pc"
pc_t3_tib <- as_tibble(pc_t3_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_pc = value) %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Intrinsic motivation
#>
#> Getting pre-test adjusted means for each participant
im_t3 <- im_p_tib %>%
  dplyr::filter(time_id != "t2")

#> Make wide
im_t3_wide <- im_t3 %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id, values_from = score)

#> Fitting linear regression to get predicted retention values
im_t3_model <- lm(t3 ~ t1, im_t3_wide)

#> Create a tibble adding columns for sub_id, group_id,
#> and rename column title to "adj_im"
im_t3_tib <- as_tibble(im_t3_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_im = value)  %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Create tibble with pre-test adjusted stacking time at retention, and pre-test
#> adjusted questionnaire scores before retention
ret_qaire_tib <- adj_ret_p_tib %>%
  dplyr::left_join(pa_t3_tib) %>%
  dplyr::left_join(pc_t3_tib) %>%
  dplyr::left_join(im_t3_tib) %>%
  dplyr::mutate(sub_id = forcats::as_factor(sub_id))

#> DATA SETUP - CRONBACH'S ALPHA ----
#>
#> After pre-test
#> Create tibble extracting after pre-test (t1)
cronbach_pre <- cronbach_data %>%
  dplyr::select(1:24)

#> Getting each construct
pa_pre_tib <- dplyr::select(
  cronbach_pre, 3, 4, 7, 9, 12, 14, 17, 19, 21, 23
  )
pc_pre_tib <- dplyr::select(
  cronbach_pre, 5, 11, 16, 20, 22
  )
im_pre_tib <- dplyr::select(
  cronbach_pre, 6, 8, 10, 13, 15, 18, 24
  )

#> After acquisition
#> Create tibble extracting after pre-test (t2)
cronbach_acq <- cronbach_data %>%
  dplyr::select(1, 2, 25:46)

#> Getting each construct
pa_acq_tib <- dplyr::select(
  cronbach_acq, 3, 4, 7, 9, 12, 14, 17, 19, 21, 23
)
pc_acq_tib <- dplyr::select(
  cronbach_acq, 5, 11, 16, 20, 22
)
im_acq_tib <- dplyr::select(
  cronbach_acq, 6, 8, 10, 13, 15, 18, 24
)

#> Before retention
#> Create tibble extracting before retention (t3)
cronbach_ret <- cronbach_data %>%
  dplyr::select(1, 2, 47:68)

#> Getting each construct
pa_ret_tib <- dplyr::select(
  cronbach_ret, 3, 4, 7, 9, 12, 14, 17, 19, 21, 23
)
pc_ret_tib <- dplyr::select(
  cronbach_ret, 5, 11, 16, 20, 22
)
im_ret_tib <- dplyr::select(
  cronbach_ret, 6, 8, 10, 13, 15, 18, 24
)
