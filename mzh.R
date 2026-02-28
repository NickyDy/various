library(tidyverse)

zemedelska_zaetost <- read_delim("https://data.egov.bg/resource/download/6399cc06-dd73-414d-b183-c8612686cf63/csv", na = "#") %>% 
  pivot_longer(3:28) %>% select(cultura = `Основни категории`, unit = `Ед. мярка`, year = name, value)
izpolzvane_zemq <- read_delim("https://data.egov.bg/resource/download/e35e4ed1-91c5-416c-8bfc-93c6519a0d03/csv", na = "#") %>% 
  pivot_longer(3:28) %>% select(cultura = `Основни категории`, unit = `Ед. мярка`, year = name, value)

glimpse(izpolzvane_zemq)

izpolzvane_zemq %>% 
  ggplot(aes(as.numeric(year), value)) +
  geom_point() +
  geom_line(linetype = 2, linewidth = 0.3) +
  labs(x = "Година") +
  facet_wrap(vars(cultura), scales = "free_y")
