library(tidyverse)
library(tidytext)
library(DataExplorer)
library(GGally)
# library(performance)
# library(tidymodels)

diss <- read_csv("data/diss.csv") %>% mutate_if(is.character, as.factor)
#----------------------------------------------------------------------
glimpse(diss)
create_report(diss)
introduce(diss)
plot_intro(diss)
plot_missing(diss)
plot_qq(diss)
plot_str(diss)
profile_missing(diss) %>% view
split_columns(diss)

diss %>% count(top, sort = T)
colSums(is.na(diss))
any(duplicated(diss$plot))

path <- ("~/Downloads/spreadsheet")
files <- dir(path, pattern = "*.xlsx")
df <- files %>% 
  map(~ read_xlsx(file.path(path, .), sheet = 5)) %>% 
  bind_rows()

diagnose_numeric(pubs) %>% flextable()
plot_intro(credit)
plot_histogram(credit)
diss %>% select(elev:rare) %>% 
  ggcorr(label = T, method = c("pairwise.complete.obs", "spearman"))
pubs %>% plot_bar_category(top = 10)
pubs %>% plot_bar(by  = "pub")
pubs %>% plot_boxplot(by = "pub")
diss %>% map_dfr(~sum(is.na(.x)))
pubs %>% select(where(is.numeric)) %>% map_dfr(~mean(.x))
pubs %>% select(where(is.numeric)) %>% map_dfr(~median(.x))
pubs %>% select(where(is.numeric)) %>% map_dfr(~.x >1500)

diss %>% select(where(is.double)) %>% map_dbl(mean)
map_lgl(diss, is.factor)
map_chr(diss, typeof)
n_unique <- function(x) length(unique(x))
map_int(diss, n_unique)
map_dfr(diss, function(x) length(unique(x)))
map_dfr(diss, ~ length(unique(.x)))
x <- map_dfr(diss, ~ runif(10))
diss %>% modify_at(6:36, ~ sqrt(.x)) %>% view
diss %>% modify_if(is.numeric, ~ log10(.x + 0.1)) %>% view

# reorder_within
diss %>% 
  summarise(elev = mean(elev), .by = c(top, exp)) %>% 
  mutate(exp = reorder_within(exp, elev, top)) %>%
  ggplot(aes(elev, exp, fill = top)) +
  geom_col(show.legend = F) +
  facet_wrap(vars(top), scales = "free_y") +
  scale_y_reordered()

tops <- split(diss, diss$top)
walk2(tops, c("top1.csv", "top2.csv", "top3.csv"), write.csv)

l <- map(1:4, ~ sample(1:10, 15, replace = T))
reduce(l, intersect)
reduce(l, union)
accumulate(l, intersect)

x <- c(4, 3, 10)
reduce(x, `+`)

accumulate(x, `+`)

diss %>% detect(is.factor)
diss %>% detect_index(is.factor)
diss %>% keep(is.factor)
diss %>% discard(is.factor)
diss %>% map_if(is.numeric, mean, na.rm = T)
diss %>% modify_if(is.numeric, mean, na.rm = T)
map(keep(diss, is.numeric), mean, na.rm = T)

# Date
months <- seq(ymd("2023-01-01"), ymd("2023-12-31"),"months") %>% 
  as_tibble(.) %>% 
  select(date = value) %>% 
  mutate(month = month(date))

weeks <- seq(ymd("2023-01-01"), ymd("2023-12-31"),"weeks") %>% 
  as_tibble(.) %>% 
  select(date = value) %>% 
  mutate(week = paste0("week_", week(date)))

days <- seq(ymd("2023-01-01"), ymd("2023-12-31"),"days") %>% 
  as_tibble(.) %>% 
  select(date = value) %>% 
  mutate(day = day(date))

# Transpose data frame
diss <- column_to_rownames(diss, "plot")
t_diss <- data.table::transpose(diss)
colnames(t_diss) <- rownames(diss)
rownames(t_diss) <- colnames(diss)
#---------------------------------

pricedata %>% 
  pivot_longer(cols = open:close, names_to = "name_price", values_to = "value") %>% 
  filter(region >= "USA") %>% 
  #mutate(diff = close-open) %>% 
  ggplot(aes(date, value, color = name_price)) +
  geom_path() +
  scale_x_date(date_breaks = "3 years") +
  scale_color_brewer(palette = "RdYlBu") +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  labs(title = "USA")
pricedata %>% 
  pivot_longer(cols = open:close, names_to = "name_price", values_to = "value") %>% 
  filter(region >= "UK") %>% 
  #mutate(diff = close-open) %>% 
  ggplot(aes(date, value, color = name_price)) +
  geom_path() +
  scale_x_date(date_breaks = "3 years") +
  scale_color_brewer(palette = "RdYlBu") +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  labs(title = "UK")
#-----------------------------
#ONE CATEGORICAL
pubs %>% filter(time == "09:00 - 23:00") %>% 
  count(product) %>% 
  summarise(product, prop = n/sum(n)) %>% 
  mutate(product = fct_reorder(product, prop)) %>% 
  ggplot(aes(product, prop, fill = product)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = NULL, y = "Percent", x = NULL) +
  coord_flip()
p2<-diss %>% filter(veg_type == "subalpine") %>% 
  count(com) %>% 
  summarise(com, prop = n/sum(n)) %>% 
  mutate(com = fct_reorder(com, prop)) %>% 
  ggplot(aes(com, prop, fill = com)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = NULL, y = "Percent", x = NULL) +
  coord_flip()
p1 + p2

goodness_of_fit_test<-contingency_table(
  data   = emma,
  x      = athletism)
emma %>% 
  count(athletism) %>% 
  summarise(athletism, prop = n/sum(n)) %>% 
  mutate(athletism = fct_inorder(athletism)) %>% 
  ggplot(aes(x = "", y = prop, fill = athletism)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales:: percent(prop)), position = position_stack(vjust = 0.5), size = 5) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Dark2") +
  labs(subtitle = parse(text = goodness_of_fit_test$expression), y = NULL, x = NULL, fill = "Athletism:") +
  theme(text = element_text(size = 16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

#Pie chart
diss %>%
  count(exp) %>%
  mutate(perc = paste0(round(n/sum(n)*100, 1), "%")) %>%
  ggplot(aes(x = factor(1), y = n, fill = exp)) + 
  geom_bar(stat ="identity", width = 1, size = 1, color = "white", show.legend = T) +
  coord_polar(theta ="y") +
  labs(fill = "Slope\ntopography:") +
  theme(text = element_text(size = 16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5), size = 5)

#TWO CATEGORICAL
diss %>% 
  count(com, exp) %>% 
  summarise(com, exp, prop = n/sum(n)) %>% 
  mutate(com = fct_reorder(com, prop)) %>% 
  ggplot(aes(com, prop, fill = exp)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(title = NULL, y = "Percent", x = NULL)

association_test <- contingency_table(
  data   = emma,
  x      = q2,
  y      = q4,
  paired = FALSE)
emma %>% 
  count(q2, q4) %>% 
  summarise(q2, q4, prop = n/sum(n)) %>% 
  mutate(q2 = fct_relevel(q2, "20-40 min", "60-90 min", "> 2 hours"),
         q4 = fct_relevel(q4, "Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")) %>% 
  ggplot(aes(q2, prop, fill = q4)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(subtitle = parse(text = association_test$expression), y = "Percent", x = NULL)

goodness_of_fit_test<-contingency_table(
  data   = emma,
  x      = q2)
emma %>% 
  count(q2, q4) %>% 
  summarise(q2, q4, prop = n/sum(n)) %>% 
  mutate(q2 = fct_relevel(q2, "20-40 min", "60-90 min", "> 2 hours"),
         q4 = fct_relevel(q4, "Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")) %>% 
  ggplot(aes(q2, prop, fill = q2)) +
  geom_col(position = "stack", show.legend = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 16)) +
  labs(subtitle = parse(text = goodness_of_fit_test$expression), y = "Percent", x = NULL)
ggsave("plot.png", width = 10, height = 5)

#ONE CATEGORICAL ONE NUMERICAL
pubs %>% 
  mutate(time = fct_reorder(time, price)) %>% 
  ggplot(aes(time, price, fill = time)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = NULL, y = "Species number/0.1ha", x = "Exposition") +
  coord_flip()

diss %>% 
  ggplot(aes(s_ha, fill = top)) +
  geom_histogram(binwidth = 5)
ggsave("plot.png", width = 10, height = 5)

#TWO NUMERICAL
diss %>% 
  ggplot(aes(elev, s_ha)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = NULL, x = "Elevation (m)", y = "Species number/0.1ha", fill = NULL) +
  theme(text = element_text(size = 16), legend.position = "right") +
  facet_wrap(~veg_type, ncol = 2, scales = "free_x")

ggsave("plot.png", width = 10, height = 5)