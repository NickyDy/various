library(tidyverse)
#library(DataExplorer)
#library(dlookr)
#library(flextable)
library(ggridges)
library(GGally)
#library(treemap)
library(glue)
library(knitr)
library(ggpubr)
library(splines)

diss <- read_csv("data/diss.csv") %>% mutate_if(is.character, as.factor)
matrix <- read_csv("csv/matrix.csv") %>% janitor::clean_names()

diss <- diss %>% mutate(com = fct_recode(com, "fagus_hepatica" = "a", "fagus_physospermum" = "b", "fagus_galium" = "c",
                                 "fagus_luzula" = "d", "fagus_festuca" = "e", "fagus_brachypodium" = "f",
                                 "pinus_fragaria" = "g", "pinus_crataegus" = "h", "quercus_cornus" = "i",
                                 "festuca_chamaespartium" = "j", "galium_thymus" = "k", "juniperus_vaccinium" = "l",
                                 "juniperus_vaccinium_2" = "m", "juniperus_sesleria" = "n"),
                veg_type = fct_recode(veg_type, "forest" = "f", "subalpine" = "s"))

glimpse(diss)
diagnose_numeric(diss) %>% flextable()
plot_intro(diss)
plot_histogram(diss)
plot_correlate(diss)
diss %>% plot_bar_category(top = 10, -plot)
diss %>% plot_bar(by = "veg_type", binary_as_factor = F)
diss %>% select(veg_type, elev, slope, s_ha, s_m2) %>% plot_boxplot(by = "veg_type")
diversity %>% map_dfr(~sum(is.na(.)))
diversity %>% select(where(is.numeric)) %>% map_dfr(~mean(.x))
diversity %>% select(where(is.numeric)) %>% map_dfr(~median(.x))
diversity %>% select(where(is.numeric)) %>% map_dfr(~.x >1500)
#-------------------------------------------------------------
diss %>% skim()

diss %>% 
  mutate(n2 = if_else(elev == 1010, 1000 * n2, n2)) %>% View()

diss %>% 
  select(veg_type, s_ha, s_m2) %>% 
	ggpairs(aes(col = veg_type)) +
	theme_bw()
  ggsave("plot.png", width = 10, height = 5)

ggplot(diss, aes(elev, slope)) +
  geom_density_2d(color = "black", alpha = 0.3) +
  stat_summary_hex(aes(z = s_ha), alpha = 0.7, bins = 10) +
  scale_fill_viridis_c() +
  labs(fill = "Видово\nбогатство/0.1 ha", x ="Надморска височина (m)", y ="Наклон (º)") +
  theme(text = element_text(size = 14))
  
diss %>%
  group_by(com) %>%
  summarise(
    med = median(invasive),
    avg = mean(invasive)) %>% 
  arrange(desc(avg))

ggplot(df, aes(com, s_ha, ymin = s_ha-sd, ymax = s_ha+sd)) +
  geom_pointrange() +
  geom_line(aes(group = 1))
diss %>% 
	ggplot() + 
	stat_summary(
		aes(x = com, y = s_ha),
		fun.min = min,
		fun.max = max,
		fun = median
	)

diss %>% 
  pivot_longer(cols = slope:s_m2, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~variable)
#---------------------------------------------
exp_top <- diss %>%
  group_by(top, exp) %>%
  summarize(Count = n())%>%
  mutate(Percent = round(Count/sum(Count)*100))

exp_top %>%
  ggplot(aes(x = exp, y = Percent, fill = top)) +
  geom_bar(stat ="identity", position ="fill") +
  geom_text(aes(label=paste0(sprintf("%1.0f", Percent),"%")),
            position=position_fill(vjust=0.5), colour="white") +
  ggtitle("Percentage of topography across expositions") +
  labs(y = "Percent", x = "Exposition")

diss %>%
  ggplot(aes(elev, invasive)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, 
              formula = y ~ ns(x, df = 10), 
              color = "midnightblue", se = T)
  labs(y = NULL)
#----------------------
sp <- ggscatter(diss, x = "elev", y = "s_ha",
                color = "lightgray")
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  gradient_fill(c("white", "steelblue"))
sp + geom_density_2d()

diss %>% mutate(matching = if_else(slope == s_ha, "match", " no match"), .after = s_ha) %>% count(matching)
#------------------------------------------------------------
diss |> 
	group_by(top) |> 
	summarize(
		m_s_a = mean(s_ha[com == "a"], na.rm = TRUE),
		m_s_k = mean(s_ha[com == "k"], na.rm = TRUE),
		n = n(),
		.groups = "drop"
	)
diss %>% count(plot)
