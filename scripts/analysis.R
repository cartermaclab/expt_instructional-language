#> -------------------------------------------
#> AUTONOMY SUPPORTIVE INSTRUCTIONAL LANGUAGE
#> -- St. Germain, McKay, Tandon, Seedu, Barbera,
#>    Carrillo, Brown, and Carter
#>
#> Analysis
#>
#> Authors:
#>   Laura St. Germain
#>   Michael Carter
#>
#> Last update: September 24 2023
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> #> SCRIPT SETUP ----
#>
#> Pull from wrangle script
source("scripts/wrangle.R")

#> Seed for reproducible bootstraps
set.seed(1776)

#> PRIMARY ANALYSIS - SHIFT FUNCTION ----
#>
#> Pivot wider to separate by group
learn_trim_wide <- adj_ret_trim_tib %>%
  dplyr::select(sub_id, group_id, adj_mt_trim) %>%
  pivot_wider(names_from = group_id,
              values_from = adj_mt_trim)

#> Create separate lists for each group
learn_trim_aut <- na.omit(learn_trim_wide$'1')
learn_trim_con <- na.omit(learn_trim_wide$'2')

#> create list with both groups
learn_shift <- rogme::mkt2(
  learn_trim_aut,
  learn_trim_con,
  group_labels = c("Autonomy", "Controlling")
)

#> Make factors
as.factor(learn_shift$gr)

#> Compute shift function
learn_sf <- rogme::shifthd_pbci(data = learn_shift,
                                formula = obs ~ gr,
                                q = seq(0.1, 0.9, 0.1),
                                nboot = 2000,
                                alpha = 0.05)

#> Extract p-values
shift_p_values <- as_tibble(learn_sf[[1]][[7]])
shift_adj_p_values <- as_tibble(learn_sf[[1]][[8]])
#> RESULTS:
#> The significant difference in the last decile (favouring Autonomy-supportive
#> language group) does not survive multiple comparison correction
#> So trend in the last decile and also in the first decile that favours
#> the Controlling language group

###> SECONDARY ANALYSIS - WELCH'S T-TEST ----
#> Do t-test
learn_t_test <- t.test(adj_mt ~ group_id,
                       data = adj_ret_p_tib,
                       alternative = "less")

#> Calculate effect size
learn_cohen_d <- effsize::cohen.d(adj_mt ~ group_id,
                                  data = adj_ret_p_tib,
                                  within = FALSE)
#> RESULTS:
#> Adjusted means
#> Autonomy = 9.86 vs Controlling = 10.04
#> t(124.51) = 1.00, p = .159, d = .16 [-.156, .477]


###> SECONDARY ANALYSIS - ACQUISITION ANOVA ----
#> Omnibus test
acq_lm <- afex::aov_ez(
  "sub_id", "p_mean_mt", acq_p_tib,
  between = "group_id",
  within = "block_id"
)
acq_lm
#> RESULTS:
#> Main effect of Group: F(1, 154) = 2.20, p = .140, ges = .011
#> Main effect of Block: F(4.40, 677.96) = 111.91, p < .001, ges = .137 ***
#> Group x Block interaction: F(4.40, 677.96) = 0.59, p = .681, ges < .001

#> Post-hoc for main effect of Block
acq_me_block <- emmeans::emmeans(
  acq_lm, ~block_id
)
pairs(acq_me_block, adjust = "holm")
#> RESULTS:
#> Block 1 worse than all other blocks, p's < .001
#> Block 2 worse than all other blocks, p's <= .004
#> Block 3 worse than blocks 5 and 6, p's < .001, but not block 4, p = .100
#> Block 4 worse than blocks 5 and 6, p's < .001
#> Block 5 not different than block 6, p = .540


###> SECONDARY ANALYSIS - QUESTIONNAIRE ANCOVAS ----
#> Perceived autonomy
#> Omnibus test
pa_lm <- aov_4(
  score ~ group_id + t1 + (time_id|sub_id),
  data = pa_cov_tib,
  factorize = FALSE
)
pa_lm

#> RESULTS:
#> Main effect of Group: F(1, 153) = 4.40, p = .037, ges = .020 ***
#> Main effect of Time: F(1, 153) = 0.48, p = .490, ges < .001
#> Group x Time interaction: F(1, 153) < 0.01, p = .982, ges < .001

#> Get pre-test adjusted means for plotting
pa_adj <- emmeans::emmeans(
  pa_lm, ~c(group_id, time_id)
)
pa_adj

#> Calculate Cohen's d
#> We already have the pre-test adjusted scores for individual participants
#>  before retention for plotting from wrangle.R, to get for after acquisition
#> Getting pre-test adjusted means for each participant
pa_t2 <- pa_p_tib %>%
  dplyr::filter(time_id != "t3")

#> Make wide
pa_t2_wide <- pa_t2 %>%
  dplyr::select(sub_id, group_id, time_id, score) %>%
  tidyr::pivot_wider(names_from = time_id, values_from = score)

#> Fitting linear regression to get predicted retention values
pa_t2_model <- lm(t2 ~ t1, pa_t2_wide)

#> Create a tibble adding columns for sub_id, group_id,
#> and rename column title to "adj_pa"
pa_t2_tib <- as_tibble(pa_t2_model$fitted.values) %>%
  dplyr::mutate(sub_id = c(101:178, 201:278), .before = 1) %>%
  dplyr::mutate(group_id = rep(c(1, 2), each = 78), .after = 1) %>%
  dplyr::rename(adj_pa_t2 = value) %>%
  dplyr::mutate(group_id = forcats::as_factor(group_id))

#> Combine t2 and t3 perceived autonomy tibbles, rename final column
pa_joined <- pa_t2_tib %>%
  dplyr::left_join(pa_t3_tib) %>%
  dplyr::rename(adj_pa_t3 = adj_pa)

#> Make long
pa_adj_long <- pa_joined %>%
  tidyr::pivot_longer(
    cols = c(3:4),
    names_to = c("time_id"),
    values_to = "score"
  )

#> Calculate cohen's d and 95% CI
pa_cohens_d <- effsize::cohen.d(pa_adj_long$score[1:156],
                                pa_adj_long$score[157:312])
pa_cohens_d
#> Cohen's d = 0.27 [ 0.05, 0.49]

#> Perceived competence
#> Omnibus test
pc_lm <- aov_4(
  score ~ group_id + t1 + (time_id|sub_id),
  data = pc_cov_tib,
  factorize = FALSE
)
pc_lm
#> RESULTS:
#> Main effect of Group: F(1, 153) = 0.01, p = .933, ges = < .001
#> Main effect of Time: F(1, 153) = 0.56, p = .456, ges < .001
#> Group x Time interaction: F(1, 153) = 0.38, p = .539, ges < .001

#> Get pre-test adjusted means for plotting
pc_adj_g <- emmeans::emmeans(
  pc_lm, ~c(group_id*time_id)
)
pc_adj_g

#> Intrinsic motivation
#> Omnibus test
im_lm <- aov_4(
  score ~ group_id + t1 + (time_id|sub_id),
  data = im_cov_tib,
  factorize = FALSE
)
im_lm
#> RESULTS:
#> Main effect of Group: F(1, 153) = 0.11, p = .743, ges < .001
#> Main effect of Time: F(1, 153) = 0.21, p = .605, ges < .001
#> Group x Time interaction: F(1, 153) = 1.84, p = .177, ges = .002

#> Get pre-test adjusted means for plotting
im_adj_g <- emmeans::emmeans(
  im_lm, ~c(group_id, time_id)
)
im_adj_g


#> CRONBACH'S ALPHA ----
#>
#> After pre-test
cronbach_pa_pre <- Cronbach::cronbach(pa_pre_tib)
cronbach_pc_pre <- Cronbach::cronbach(pc_pre_tib)
cronbach_im_pre <- Cronbach::cronbach(im_pre_tib)

#> After acquisition
cronbach_pa_acq <- Cronbach::cronbach(pa_acq_tib)
cronbach_pc_acq <- Cronbach::cronbach(pc_acq_tib)
cronbach_im_acq <- Cronbach::cronbach(im_acq_tib)

#> Before retention
cronbach_pa_ret <- Cronbach::cronbach(pa_ret_tib)
cronbach_pc_ret <- Cronbach::cronbach(pc_ret_tib)
cronbach_im_ret <- Cronbach::cronbach(im_ret_tib)
