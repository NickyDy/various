library(tidyverse)
library(patchwork)
## ---------------------------
res_log <- read_csv("https://raw.githubusercontent.com/kathoffman/steroids-trial-emulation/main/output/res_log.csv")
res <- read_csv("https://raw.githubusercontent.com/kathoffman/steroids-trial-emulation/main/output/res.csv")

# load in results generated from Cox PH hazards models
res <- res_log |>
  rename_with(~str_c("log.", .), estimate:conf.high) |>
  select(-p.value) |>
  full_join(res)

## plotting
## ---------------------------
# create forest plot on log scale (middle section of figure)
p_mid <-
  res |>
  ggplot(aes(y = fct_rev(model))) +
  theme_classic() +
  geom_point(aes(x=log.estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=log.conf.low, xmax=log.conf.high)) +
  labs(x="Log Hazard Ratio") +
  coord_cartesian(ylim=c(1,11), xlim=c(-1, .5))+
  geom_vline(xintercept = 0, linetype="dashed") +
  annotate("text", x = -.32, y = 11, label = "Corticosteroids protective") +
  annotate("text", x = .3, y = 11, label = "Corticosteroids harmful") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
# wrangle results into pre-plotting table form
res_plot <- res |>
  mutate(across(c(estimate, conf.low, conf.high), ~str_pad(round(.x, 2), 
                                                           width=4, pad="0", side="right")),
         estimate_lab = paste0(estimate, " (", conf.low, "-", conf.high,")"),
         color = rep(c("gray","white"),5)) |>
  mutate(p.value = case_when(p.value < .01 ~ "<0.01", 
                             TRUE ~ str_pad(as.character(round(p.value, 2)), 
                                            width=4,pad="0",side="right"))) |>
  bind_rows(data.frame(model = "Model", 
                       estimate_lab = "Hazard Ratio (95% CI)", 
                       conf.low = "", conf.high="", p.value="p-value")) |>
  mutate(model = fct_rev(fct_relevel(model, "Model")))
# left side of plot - hazard ratios
p_left <-
  res_plot  |>
  ggplot(aes(y = model)) + 
  geom_text(aes(x=0, label=model), hjust=0, fontface = "bold") +
  geom_text(aes(x=1, label=estimate_lab), hjust=0, 
            fontface = ifelse(res_plot$estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,4))
# right side of plot - pvalues
p_right <-
  res_plot  |>
  ggplot() +
  geom_text(aes(x=0, y=model, label=p.value), hjust=0, 
            fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
  theme_void()

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 3),
  area(t = 1, l = 4, b = 30, r = 9),
  area(t = 0, l = 9, b = 30, r = 11)
)
# final plot arrangement
p_left + p_mid + p_right + plot_layout(design = layout)
