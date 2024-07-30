library(tidyverse)

dat_long <- read_csv("https://raw.githubusercontent.com/kathoffman/steroids-trial-emulation/main/data/dat_trt_timeline.csv", 
                     col_types = list(id  = "c", steroids = "c", death = "c", severe = "c"))
# define colors for all geometries with a color argument
cols <- c("Severe hypoxia" = "#b24745", # red
          "Intubated" = "#483d8b", # navy
          "Not intubated" = "#74aaff", # lighter blue
          "Steroids"= "#ffd966", # gold
          "Death" = "#000000") # black 

shape_override <- c(4, NA, NA, 21, 15) # order matches `cols`:severe, intubation (yes/no), steroids, death
line_override <- c(NA, 1, 1, NA, NA) # order matches `cols`:severe, intubation (yes/no), steroids, death
stroke_override <- c(1.2, 1, 1, 1, 1.4) # order matches `cols`:severe, intubation (yes/no), steroids, death
size_override <- c(2.5, 2.5, 2.6, 2.5, 2) # order matches `cols`:severe, intubation (yes/no), steroids, death

# modify swimmer data to 1) only show events if yes 2) have an id ordered by max follow up
dat_swim <- 
  dat_long |>
  mutate(severe_this_day = case_when(severe == 1 ~ day),
         steroids_this_day = case_when(steroids == 1 ~ day),
         death_this_day = case_when(death == 1 ~ day)) |>
  group_by(id) |>
  mutate(max_day = max(day)) |>
  ungroup() |>
  mutate(id = fct_reorder(factor(id), max_day))

dat_swim |>
  ggplot() +
  geom_line(aes(x=day, y=id, col = intubation_status, group=id), linewidth=1.8) +
  geom_point(aes(x=steroids_this_day, y=id, col="Steroids"), shape=15, stroke=2) +
  geom_point(aes(x=severe_this_day, y=id, col="Severe hypoxia"), size=2, stroke=1.5, shape=21) +
  geom_point(aes(x=death_this_day, y=id, col="Death"), size=2, stroke=1.5, shape=4) +
  theme_bw() +
  scale_color_manual(values = cols, name="Patient Status") +
  guides(color = guide_legend(
    override.aes = list(
      stroke = stroke_override,
      shape = shape_override,
      linetype = line_override,
      size = size_override))) +
  labs(x="Days since hospitalization", y="Patient number", title="Treatment Timeline for N = 30 Patients") +
  scale_x_continuous(expand=c(0,0)) + # remove extra white space 
  theme(# text=element_text(family="Poppins", size=11),
    title = element_text(angle = 0, vjust=.5, size=12, face="bold"),
    axis.title.y = element_text(angle = 90, vjust=.5, size=12, face="bold"),
    axis.title.x = element_text(size=15, face="bold", vjust=-0.5, hjust=0),
    axis.text.y = element_text(size=6, hjust=1.5),
    axis.ticks.y = element_blank(),
    legend.position = c(0.9, 0.3),
    legend.title = element_text(colour="black", size=13, face=4),
    legend.text = element_text(colour="black", size=10),
    legend.background = element_rect(size=0.5, linetype="solid", colour ="gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()) 
