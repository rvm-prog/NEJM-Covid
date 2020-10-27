# assumes working directory of  R project
library(ggplot2)
library(gganimate)
library(magick)
library(av)

library(dplyr)
library(purrr)
library(tidyr)
theme_set(theme_bw() + theme(legend.position = "bottom", text = element_text(size=18), strip.text.x = element_text(size = 14)))
colours <- c("#940C09", "#34E06E")

get_res_tbl <- function(sens, spec, eg_freq=5) {
  # percentage sick and not sick
  perc_sick <- seq(0, 100, 0.5)
  n_obs <- length(perc_sick)
  perc_notsick <- 100 - perc_sick
  # example points
  eg_pnts <- seq(0, 100, eg_freq)
  # true positives
  TP <- perc_sick * (sens / 100)
  # true negatives
  TN <- perc_notsick * (spec / 100)
  # false negatives
  FN <- perc_sick - TP
  # false positives
  FP <- perc_notsick - TN
  # get predictive values
  FPPV <- (FP / (FP + TP)) * 100
  FNPV <- (FN / (FN + TN)) * 100
  TPPV <- (TP / (FP + TP)) * 100
  TNPV <- (TN / (FN + TN)) * 100
  # combine results to tbl
  tibble(
    perc_sick = rep(perc_sick, 4),
    test_acc = rep(c("False Result", "True Result", "False Result", "True Result"), each=n_obs),
    test_corr = rep(c("Incorrect", "Correct", "Incorrect", "Correct"), each=n_obs),
    test_res = rep(c("Negative Test Result", "Positive Test Result"), each=n_obs*2),
    pv = c(FNPV, TNPV, FPPV, TPPV),
    sens = sens,
    spec = spec
  ) %>%
    mutate(is_example = perc_sick %in% eg_pnts)
}

plt_frame <- function(res_tbl, ind_points=FALSE) {
  if (ind_points) {
    ggplot(filter(res_tbl, is_example), aes(perc_sick, pv, colour=test_acc)) +
      geom_line(size = 1.5, alpha = 0.2) +
      geom_point(size=2)  +
      facet_wrap(vars(test_res)) +
      labs(x = "Pre-Test Probability of Sickness (%)", y = "Predictive Value (%)", colour = NULL) +
      scale_colour_manual(values=colours) +
      theme(text = element_text(size=16))
  } else {
    ggplot(res_tbl, aes(perc_sick, pv, colour=test_acc)) +
      geom_line(size = 1.5) +
      facet_wrap(vars(test_res)) +
      labs(x = "Pre-Test Probability of Sickness (%)", y = "Predictive Value (%)", colour = NULL) +
      scale_colour_manual(values=colours) +
      theme(text = element_text(size=16))
  }
}

plt_vals_frame <- function(res_tbl) {
  res_tbl %>%
    select(sens, spec, frame_nr) %>%
    distinct() %>%
    pivot_longer(cols = c(sens, spec), names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = if_else(Variable=="sens", "Sensitivity", "Specificity")) %>%
    mutate(Variable = factor(Variable, levels=c("Specificity", "Sensitivity"))) %>%
    ggplot(aes(Variable, Value)) +
    geom_col(colour = "black") +
    ylim(c(0, 100)) +
    coord_flip() +
    labs(x = NULL, y = NULL, caption = "@JackEdTaylor") +
    theme_minimal() +
    theme(text = element_text(size=16))
}

get_anim_tbl <- function(sens = c(90, 1), spec = c(95, 95)) {
  if (length(sens) != length(spec)) stop("Inconsistent vector lengths!")
  n_frames = length(sens)
  map_df(1:n_frames, function(fr_i) {
    get_res_tbl(sens = sens[[fr_i]], spec = spec[[fr_i]]) %>%
      mutate(frame_nr = fr_i)
  }) %>%
    mutate(frame_nr = factor(frame_nr))
}

build_anim <- function(sens_vals, spec_vals, file, nframes=300, fps=10, w=750, h=600, ind_points=FALSE) {
  if (length(sens_vals) != length(spec_vals)) stop("Inconsistent vector lengths!")
  
  cat(sprintf("Getting data\n"))
  anim_tbl <- get_anim_tbl(sens_vals, spec_vals)
  
  cat(sprintf("Animating lines\n"))
  lines_anim <- anim_tbl %>%
    plt_frame(ind_points = ind_points) +
    transition_manual(frame_nr)
  
  lines_frames <- animate(lines_anim, nframes=nframes, fps=fps, duration=nframes/fps, detail=1, width=w, height=h*4/5, device="png", type = "cairo", renderer=file_renderer(file.path("animation", "lines"), overwrite = TRUE))
  
  cat(sprintf("Animating values\n"))
  vals_anim <- anim_tbl %>%
    plt_vals_frame() +
    transition_manual(frame_nr)
  
  vals_frames <- animate(vals_anim, nframes=nframes, fps=fps, duration=nframes/fps, detail=1, width=w, height=h*1/5, device="png", type = "cairo", renderer=file_renderer(file.path("animation", "vals"), overwrite = TRUE))
  
  cat(sprintf("Generating stacked animation\n"))
  stacked_gif <- image_append(c(image_read(lines_frames[1]), image_read(vals_frames[1])), stack=TRUE)
  for(i in 2:nframes){
    frame_i <- image_append(c(image_read(lines_frames[i]), image_read(vals_frames[i])), stack=TRUE)
    stacked_gif <- c(stacked_gif, frame_i)
  }
  
  cat(sprintf("Saving gif\n"))
  image_write_gif(stacked_gif, file, delay=1/fps)
  
  stacked_gif
}

sens_vals_part1 <- c(
  sort(seq(1, 95, by=0.25), decreasing=TRUE),
  rep(1, 50),
  seq(1, 95, by=0.25),
  rep(95, 50)
)
spec_vals_part1 <- rep(95, length(sens_vals_part1))

spec_vals_part2 <- c(
  sort(seq(1, 95, by=0.25), decreasing=TRUE),
  rep(1, 50),
  seq(1, 95, by=0.25),
  rep(95, 50)
)
sens_vals_part2 <- rep(95, length(spec_vals_part2))

sens_vals <- c(sens_vals_part1, sens_vals_part2)
spec_vals <- c(spec_vals_part1, spec_vals_part2)

anim <- build_anim(sens_vals, spec_vals, file.path("animation", "animation.gif"))
anim_ind <- build_anim(sens_vals, spec_vals, file.path("animation", "animation_individual_points.gif"), ind_points=TRUE)
