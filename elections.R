library(tidyverse)
library(arrow)
library(scales)
#library(sf)
#library(geodata)
#library(readxl)

votes <- read_parquet("shiny/elections/votes.parquet")
mand <- read_parquet("shiny/elections/mand.parquet")
activity <- read_parquet("shiny/elections/election_activity.parquet")
obsh_map <- st_read("data/obsh_map.gpkg")
obl_map <- st_read("data/obl_map.gpkg")
bg_map <- gadm("BGR", level = 0, path = tempdir())

write_parquet(votes, "data/votes.parquet")
write_parquet(votes, "shiny/elections/votes.parquet")
write_csv(june_2024, "data/june_2024.csv")

votes %>% count(party) %>% view
glimpse(df)
df %>% map_dfr(~ sum(is.na(.))) %>% View()
#-----------------------------------------
colors <- c(
  "ПП" = "yellow",
  "ГЕРБ-СДС" = "blue",
  "ДПС" = "purple",
  "БСП" = "red",
  "ИТН" = "#0096FF",
  "ДБ" = "darkblue",
  "ПП-ДБ" = "darkblue",
  "ИЗПРАВИ СЕ! МУТРИ ВЪН!" = "green",
  "ВЪЗРАЖДАНЕ" = "black",
  "БЪЛГАРСКИ ВЪЗХОД" = "darkgreen",
  "НФСБ" = "black",
  "ГЕРБ" = "blue",
  "ОП (НФСБ, АТАКА и ВМРО)" = "brown",
  "ВОЛЯ" = "pink",
  "ВЕЛИЧИЕ" = "darkgreen")

space_s <- function (x, accuracy = NULL, scale = 1, prefix = "", suffix = "", 
                     big.mark = " ", decimal.mark = ".", trim = TRUE, digits, 
                     ...)
{
  if (!missing(digits)) {
    lifecycle::deprecate_stop(when = "1.0.0", what = "comma(digits)", 
                              with = "comma(accuracy)")
  }
  number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, 
         suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
         trim = trim, ...)
}
#--------------------------------------------------------
votes %>%
  #filter(code == "122900055") %>% 
  mutate(vote_date = fct_relevel(vote_date,
                                 "Юни_2024",
                                 "Април_2023",
  															 "Октомври_2022", 
  															 "Ноември_2021", 
  															 "Юли_2021", 
  															 "Април_2021", 
  															 "Март_2017")) %>%
  group_by(vote_date, party) %>%
  summarise(sum_votes = sum(votes)) %>%
  filter(sum_votes >= 50000) %>%
  mutate(party = fct_reorder(party, sum_votes)) %>% 
  ggplot(aes(sum_votes, party, fill = party)) +
  geom_col(position = "dodge", show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(labels = scales::label_wrap(50)) +
  scale_x_continuous(expand = expansion(mult = c(.05, .8))) +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = space_s(sum_votes)), 
  					position = position_dodge(width = 1), hjust = -0.05, size = 16, size.unit = "pt") +
  theme(text = element_text(size = 16), 
  			axis.text.x = element_blank(), 
  			axis.ticks.x = element_blank()) +
  labs(y = NULL, x = "Брой гласове", title = ,
       caption = "Бележка: Оцветени са само партиите и коалициите влизали в Парламента, останалите са в сиво.\nИзточник на данните: ЦИК."
       ) +
  facet_wrap(~ vote_date, nrow = 1)

votes %>%
  #filter(oblast == "Пловдив град") %>% 
  mutate(vote_date = fct_relevel(vote_date,
                                 "Юни_2024",
                                 "Април_2023",
  															 "Октомври_2022", 
  															 "Ноември_2021", 
  															 "Юли_2021", 
  															 "Април_2021", 
  															 "Март_2017")) %>%
  group_by(vote_date, party) %>%
  summarise(sum_votes = sum(votes, na.rm = T)) %>%
  mutate(prop = sum_votes / sum(sum_votes)) %>%
  mutate(party = fct_reorder(party, sum_votes)) %>%
  filter(prop >= 0.01) %>%
  ggplot(aes(prop, party, fill = party)) +
  geom_col(position = "dodge", show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(expand = expansion(mult = c(.05, .7))) +
	scale_y_discrete(labels = scales::label_wrap(50)) +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
    position = position_dodge(width = 1), hjust = -0.1, size = 16, size.unit = "pt") +
  theme(text = element_text(size = 16), 
  			axis.text.x = element_blank(), 
  			axis.ticks.x = element_blank()) +
  labs(x = NULL, y = NULL, title = NULL,
       caption = "Бележка: Оцветени са само партиите и коалициите влизали в Парламента, останалите са в сиво.\nИзточник на данните: ЦИК.") +
  facet_wrap(~ vote_date, nrow = 1)
#----------------------------------
votes %>%
  filter(vote_date %in% c("Юни_2024", "Април_2023")) %>%
  pivot_wider(names_from = vote_date, values_from = votes) %>% 
  mutate(diff = Юни_2024 - Април_2023) %>%
  filter(Април_2023 < 30 & diff > 150) %>% view
votes %>%
  filter(vote_date %in% c("Юни_2024", "Април_2023")) %>%
  pivot_wider(names_from = vote_date, values_from = votes) %>% 
  summarise(april_sum = sum(Април_2023, na.rm = T),
            june_sum = sum(Юни_2024, na.rm = T),
            diff = june_sum - april_sum, .by = c(oblast, party)) %>%
  filter(party %in% c("ИТН", "ПП-ДБ", "ГЕРБ-СДС", "ДПС", "ВЪЗРАЖДАНЕ", "БСП")) %>%
  mutate(col = diff > 0, oblast = fct_rev(oblast),
         col = as.factor(col),
         col = fct_recode(col, "Загуба на гласове" = "FALSE",
                          "Печалба на гласове" = "TRUE"),
         party = fct_relevel(party, "ГЕРБ-СДС", "ДПС", "ПП-ДБ", "ВЪЗРАЖДАНЕ", "БСП", "ИТН")) %>% 
  ggplot(aes(diff, oblast, fill = col)) +
  geom_col() +
  geom_text(aes(label = space_s(diff)), 
            position = position_dodge(width = 1), hjust = -0.05, size = 16, size.unit = "pt") +
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
  theme(text = element_text(size = 16), legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(y = NULL, x = "Брой гласове", fill = "Легенда:",
       title = "Загуба и печалба на гласове от основните партии на последните избори в сравнение с предпоследните:") +
  facet_wrap(vars(party), ncol = 6)
votes %>%
  mutate(vote_date = fct_relevel(vote_date,
                                 "Юни_2024",
                                 "Април_2023",
                                 "Октомври_2022", 
                                 "Ноември_2021", 
                                 "Юли_2021", 
                                 "Април_2021", 
                                 "Март_2017")) %>%
  group_by(vote_date, party) %>%
  summarise(sum_votes = sum(votes)) %>%
  pivot_wider(names_from = vote_date, values_from = sum_votes) %>% 
  mutate(diff = Април_2023 - Октомври_2022, party = fct_reorder(party, diff, .na_rm = T),
         col = diff > 0, col = as.factor(col),
         col = fct_recode(col, "Загуба на гласове" = "FALSE",
                          "Печалба на гласове" = "TRUE")) %>% 
  drop_na(diff) %>% 
  ggplot(aes(diff, party, fill = col)) +
  geom_col() +
  geom_text(aes(label = space_s(diff)), 
            position = position_dodge(width = 1), hjust = -0.05, size = 16, size.unit = "pt") +
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  theme(text = element_text(size = 16), legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(y = NULL, x = "Брой гласове", fill = "Легенда:")
# Maps-------------------------------------------------
db <- votes %>%
  group_by(vote_date, obshtina, party) %>%
  summarise(sum_party = sum(votes)) %>%
  group_by(vote_date, obshtina) %>%
  mutate(sum_obshtina = sum(sum_party), 
  					perc = sum_party / sum_obshtina * 100) %>%
  filter(party %in% c("ВЕЛИЧИЕ"))
map <- obsh_map %>% 
  left_join(db, by = c("obshtina_bg" = "obshtina")) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(vote_date %in% c("Юни_2024")) %>%
  mutate(party = fct_reorder(party, perc))
map %>% 
  ggplot() +
  geom_sf(aes(fill = perc), alpha = .4) +
  geom_sf_text(aes(label = perc), 
  						 check_overlap = TRUE, size = 3) +
  geom_sf_text(aes(label = obshtina_bg), 
  						 check_overlap = TRUE, size = 2, vjust = -1.5) +
  theme(text = element_text(size = 16), legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "(%)", 
       title = NULL) +
  scale_fill_gradient(low = "white", high = "darkgreen")
#+ annotation_scale(location = "br", width_hint = 0.2, text_cex = 1.1) +
# annotation_north_arrow(location = "tr", which_north = "true",
# pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
# style = north_arrow_fancy_orienteering)

db <- votes %>%
  filter(vote_date %in% c("Юни_2024")) %>%
  group_by(obshtina, party) %>%
  summarise(sum_votes = sum(votes)) %>%
  group_by(obshtina) %>%
  slice_max(sum_votes) %>%
  mutate_if(is.character, as.factor)
obsh_map %>%
  left_join(db, by = c("obshtina_bg" = "obshtina")) %>%
  mutate_if(is.numeric, round, 1) -> map
map %>%
  ggplot() +
  geom_sf(data = obl_map, linewidth = 0.9) +
  geom_sf(aes(fill = party), alpha = .4) +
  geom_sf_text(aes(label = obshtina_bg), 
  						 check_overlap = TRUE, size = 2.5) +
	#geom_point(data = sections, mapping = aes(lng, lat), color = "green") +
	coord_sf() +
  theme(text = element_text(size = 16), legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Партия:", 
       title = "Изборна карта на БГ след изборите на 9 юни, 2024 г.") +
  scale_fill_manual(values = c("БСП" = "red", "ГЕРБ-СДС" = "blue", "ДПС" = "purple", 
                               "ПП-ДБ" = "yellow", "ВЪЗРАЖДАНЕ" = "darkgreen"))
#------------------------------------------------------------------------------
diff <- votes %>%
  filter(vote_date %in% c("Юни_2024", "Април_2023")) %>%
  pivot_wider(names_from = vote_date, values_from = votes) %>% 
  filter(party == "ДПС") %>% 
  summarise(april_sum = sum(Април_2023, na.rm = T),
            june_sum = sum(Юни_2024, na.rm = T),
            prop = (june_sum - april_sum) / april_sum, 
            .by = c(obshtina, party)) %>% 
  filter(prop > 0.3) %>% 
  mutate(change = case_when(
    prop < 0 ~ "Загуба на гласове",
    prop == 0 ~ "Без промяна",
    prop > 0 ~ "Печалба на гласове"))
map <- obsh_map %>%
	left_join(diff, by = c("obshtina_bg" = "obshtina")) %>% 
  drop_na()
map %>%
  ggplot() +
  geom_sf(data = obl_map) +
	geom_sf(aes(fill = change), alpha = 0.5) +
	geom_sf_text(aes(label = scales::percent(prop, accuracy = 1)),
							 check_overlap = TRUE, size = 3) +
	geom_sf_text(aes(label = obshtina_bg), 
							 check_overlap = TRUE, size = 2.5, vjust = -1.5) +
	theme(text = element_text(size = 16), legend.position = "right",
				axis.text = element_blank(),
				axis.ticks = element_blank()) +
	labs(x = NULL, y = NULL, fill = "Легенда:",
			 title = paste0("Промяна в подкрепата за ", diff$party, " на последните избори!")) +
	scale_fill_manual(values = c("Загуба на гласове" = "red", 
	                             "Без промяна" = "white", 
	                             "Печалба на гласове" = "blue"))

votes %>% 
	filter(vote_date %in% c("Ноември_2021", "Октомври_2022"), 
				 obshtina == "Пазарджик",
				 party == "Движение за права и свободи – ДПС") %>% 
	group_by(vote_date, section) %>% 
	summarise(sum_votes = sum(votes)) %>% 
	pivot_wider(names_from = vote_date, values_from = sum_votes) %>% 
	mutate(diff = Октомври_2022 - Ноември_2021) %>% 
	mutate(section = fct_reorder(section, diff)) %>%
	ggplot(aes(diff, section, fill = diff)) +
	geom_col() +
	geom_text(aes(label = diff),
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), legend.position = "none") +
	labs(x = "Брой гласове", y = NULL, 
			 title = paste0("Промяна в подкрепата за ", 
			 							 diff$party, " (Община Ихтиман): Ноември, 2021-ва -> Октомври, 2022-ра!")) +
	scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen")

votes %>% 
	filter(vote_date %in% c("Ноември_2021", "Октомври_2022"), 
				 obshtina == "Ихтиман",
				 party == "Движение за права и свободи – ДПС") %>% 
	group_by(vote_date, section) %>% 
	summarise(sum_votes = sum(votes)) %>% 
	pivot_wider(names_from = vote_date, values_from = sum_votes) %>% 
	mutate(diff = Октомври_2022 - Ноември_2021,
				 prop = diff / Ноември_2021) %>% 
	filter(!prop == Inf) %>% 
	mutate(section = fct_reorder(section, prop)) %>% 
	ggplot(aes(prop, section, fill = prop)) +
	geom_col() +
	scale_x_continuous(labels = scales::label_percent()) +
	geom_text(aes(label = scales::percent(prop, accuracy = 1)),
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), legend.position = "none") +
	labs(x = NULL, y = NULL, 
			 title = "Промяна в подкрепата за ДПС в община Каварна: Ноември, 2021-ва -> Октомври, 2022-ра") +
	scale_fill_steps2(low = "red", mid = "white", high = "darkgreen")
#------------------
#library(statsExpressions)

votes %>% count(obshtina) %>% View()

t_test <- votes %>% 
  filter(vote_date == "Април_2023", party %in% c("ГЕРБ-СДС", "ПП-ДБ"), obshtina == "Столична") %>% 
  two_sample_test(party, votes, type = "np")

med <- votes %>% 
  filter(vote_date == "Април_2023", party %in% c("ГЕРБ-СДС", "ПП-ДБ"), obshtina == "Столична") %>% 
  group_by(party) %>% 
  summarise(med = median(votes))

votes %>% 
  filter(vote_date == "Април_2023", party %in% c("ГЕРБ-СДС", "ПП-ДБ"), obshtina == "Столична") %>% 
  ggplot(aes(party, votes, fill = party)) +
  geom_boxplot(show.legend = F, alpha = 0.7, outlier.shape = NA) +
  geom_text(data = med, aes(party, med, label = med), 
            position = position_dodge(width = 1), 
            vjust = -1, size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("blue", "darkblue")) +
  labs(subtitle = t_test$expression[[1]], x = "Партия", y = "Брой гласове",
       title = "Сравнение на изборния резултат на ГЕРБ-СДС и ПП-ДБ: В страната") +
  coord_cartesian(ylim = c(0, 200)) +
  theme(text = element_text(size = 16))

votes %>% 
  filter(vote_date == "Април_2023") %>% 
  mutate(orientation = fct_collapse(party,
                                    Про_Путин = c("БСП за БЪЛГАРИЯ", "БЪЛГАРСКА СОЦИАЛДЕМОКРАЦИЯ – ЕВРОЛЕВИЦА",
                                                  "ВЪЗРАЖДАНЕ", "ВЪН от ЕС и НАТО", "КОАЛИЦИЯ НЕУТРАЛНА БЪЛГАРИЯ (АТАКА, РУСОФИЛИ, КОМУНИСТИ)",
                                                  "ЛЕВИЦАТА!", "МИР", "Социалистическа партия Български път"),
                                    Про_Запад = c("ГЕРБ-СДС", "ГЛАС НАРОДЕН", "Движение за права и свободи – ДПС",
                                                  "ИМА ТАКЪВ НАРОД", "КОД", "НДСВ", "ПП-ДБ", "БСДД – Български Съюз за Директна Демокрация",
                                                  "БЪЛГАРСКИ ВЪЗХОД"),
                                    Други = c("НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА", "Български национален съюз – НД",
                                              "Българско Национално Обединение", "ЗАЕДНО"))) %>%
  group_by(orientation) %>% 
  summarise(sum_votes = sum(votes)) %>% 
  mutate(perc_votes = sum_votes / sum(sum_votes),
         orientation = fct_rev(orientation)) %>% 
  ggplot(aes(perc_votes, orientation, fill = orientation)) +
  geom_col(show.legend = F) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(.05, .22))) +
  scale_fill_manual(values = c("gray50", "red", "darkblue")) +
  geom_text(aes(label = paste0(round(perc_votes * 100, 2), "%", " (брой гласове: ", sum_votes, ")")), 
            position = position_dodge(width = 1), hjust = -0.05, size = 5) +
  labs(y = NULL, x = "Процент от гласовете", 
       title = "Геополитическа ориентация на Байганьовците (на база резултатите на последните избори)!") +
  annotate("text", label = 
             "ГЕРБ-СДС, 
  ГЛАС НАРОДЕН, 
  Движение за права и свободи – ДПС, 
  ИМА ТАКЪВ НАРОД, 
  КОД, 
  НДСВ, 
  ПП-ДБ, 
  БСДД – Български Съюз за Директна Демокрация, 
  БЪЛГАРСКИ ВЪЗХОД",
           x = 0.2, y = 3, size = 5, color = "white") +
  annotate("text", label = 
             "БСП за БЪЛГАРИЯ,
                        БЪЛГАРСКА СОЦИАЛДЕМОКРАЦИЯ – ЕВРОЛЕВИЦА,
              ВЪЗРАЖДАНЕ, 
              ВЪН от ЕС и НАТО, 
              КОАЛИЦИЯ НЕУТРАЛНА БЪЛГАРИЯ
              (АТАКА, РУСОФИЛИ, КОМУНИСТИ),
              ЛЕВИЦАТА!, 
              МИР, 
              Социалистическа партия Български път",
           x = 0.1, y = 2, size = 5, color = "white") +
  annotate("text", label = 
             "НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА, 
             Български национален съюз – НД,
           Българско Национално Обединение, 
           ЗАЕДНО",
           x = 0.3, y = 1, size = 5, color = "gray40") +
  theme(text = element_text(size = 16))
#--------------------------------------
risk_sec <- votes %>% 
  group_by(oblast, obshtina, section, code) %>% 
  summarise(v = var(votes)) %>% 
  ungroup() %>% 
  mutate(var = case_when(
    v <= 500 ~ "Нисък",
    v > 500 & v <= 1000 ~ "Среден",
    v > 1000 ~ "Висок"), .after = v)

risk_sec %>% 
  filter(oblast %in% c("София 24"), obshtina %in% c("Столична"), section %in% c("гр.София")) %>% 
  mutate(var = fct_relevel(var, "Висок", "Среден", "Нисък"), 
         var = factor(var, levels = c("Висок", "Среден", "Нисък"))) %>% 
  arrange(-v) %>% 
  gt() %>% 
  data_color(columns = var, method = "factor", 
             palette = c("red", "orange", "darkgreen")) %>% 
  opt_interactive(page_size_default = 100, use_filters = T)
  

risk_sec %>% filter(!str_detect(code, "^32"), section == "гр.София", var == "Висок") %>%
  ggplot(aes(v, fct_reorder(code, v), fill = oblast)) +
  geom_col() +
  scale_fill_manual(values = c("darkred", "red")) +
  labs(y = NULL, x = "Коефициент на риска (дисперсията)", fill = "Избирателен район:") +
  theme(text = element_text(size = 16))

tot_v <- votes %>% 
  summarise(tot_votes = sum(votes), .by = vote_date)

df <- bind_cols(tot_v, risk_v) %>% select(-3)

options(scipen = 100)
df %>% 
  mutate(perc_risk = round(risk_votes / tot_votes * 100, 1)) %>% 
  pivot_longer(-1) %>% 
  mutate(vote_date...1 = fct_relevel(vote_date...1, 
                                     "Април_2023",
                                     "Октомври_2022", 
                                     "Ноември_2021", 
                                     "Юли_2021", 
                                     "Април_2021", 
                                     "Март_2017"),
         name = fct_recode(name, "Общ вот" = "tot_votes", 
                           "Рисков вот" = "risk_votes", 
                           "Процент в риск" = "perc_risk")) %>%
  ggplot(aes(vote_date...1, value, fill = name)) +
  geom_col(show.legend = F) +
  scale_y_continuous(expand = expansion(mult = c(.05, .25))) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), vjust = -0.3, size = 5) +
  facet_wrap(vars(name), ncol = 1, scales = "free_y") +
  theme(text = element_text(size = 16)) +
  labs(x = NULL, y = "Брой гласове")
#----------------------------
jan_2024 <- read_csv("https://data.egov.bg/resource/download/cea990e4-6805-45dc-b5aa-529a3d0b725b/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
jan <- jan_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "януари")

feb_2024 <- read_csv("https://data.egov.bg/resource/download/99c7918d-277e-4d1c-b6b4-4886a44aa927/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
feb <- feb_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "февруари")

mar_2024 <- read_csv("https://data.egov.bg/resource/download/275d844c-9c25-44de-ad91-2883d504b82e/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
mar <- mar_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "март")

apr_2024 <- read_csv("https://data.egov.bg/resource/download/f162abaa-3558-49f7-bbfd-a0d284eb72b0/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
apr <- apr_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "април")

may_2024 <- read_csv("https://data.egov.bg/resource/download/b8a7bb3a-fce2-4a5b-9852-0d430dd43ffb/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
may <- may_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "май")

june_2024 <- read_csv("https://data.egov.bg/resource/download/5d72e5db-6b60-4530-98a4-cdb42e2c98b0/csv",
                     col_names = c("oblast", "obshtina", "sett", "Постоянен адрес", "Настоящ адрес"), skip = 1) %>% 
  mutate(id = row_number(), .before = oblast)
june <- june_2024 %>% pivot_longer(5:6, names_to = "address", values_to = "юни")

df_2024 <- left_join(jan, feb, by = c("id", "oblast", "obshtina", "sett", "address")) %>% 
  left_join(., mar, by = c("id", "oblast", "obshtina", "sett", "address")) %>% 
  left_join(., apr, by = c("id", "oblast", "obshtina", "sett", "address")) %>% 
  left_join(., may, by = c("id", "oblast", "obshtina", "sett", "address")) %>% 
  left_join(., june, by = c("id", "oblast", "obshtina", "sett", "address"))
write_parquet(df_2024, "shiny/elections/df_2024.parquet")

df_2024 %>% 
  filter(obshtina == "СТОЛИЧНА") %>% 
  mutate(diff = март - януари, 
         sett = fct_reorder(sett, diff), 
         col = diff > 0) %>% 
  filter(diff != 0) %>%
ggplot(aes(diff, sett, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = diff), 
            position = position_dodge(width = 1), hjust = -0.05, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(.01, .25))) +
  theme(text = element_text(size = 16)) +
  labs(x = "Промяна в броя жители", y = NULL) +
  facet_wrap(vars(address))

# Election activity
act <- tibble(
  voters_total = c(6588372, 6578716, 6635305, 6632375, 6602990, 6594593, 6810341, 6860588, 6859390, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, 6837737, 6859318, 6817914, 6859318, NA, NA, 6593275),
  voters = c(3334283, 2775410, 2669260, 2310903, 2601963, 2683606, 3682151, 3943004, 3540829, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 4317161, 4215145, 5139891, 5206226, NA, NA, 2268644),
  activity = c(50.61, 42.19, 40.23, 34.84, 39.41, 40.69, 54.07, 57.47, 51.62, 48.66, 51.33, 60.20, 42.51, 41.21,
               55.76, 67, 41.8, 55.1, 62.4, 75.3, 83.9, 63.14, 61.67, 75.39, 75.90, 52.29, 48.25, 34.41),
  type_election = c("Парламент", "Парламент", "Парламент и президент", "Парламент и президент", 
                    "Парламент", "Парламент", "Парламент", "Президент", "Президент", "Парламент", "Парламент",
                    "Парламент", "Президент", "Президент", "Парламент", "Парламент", "Президент", "Президент",
                    "Парламент", "Парламент", "Парламент", "Президент", "Президент", "Президент", "Президент",
                    "Президент", "Президент", "Парламент"),
  election = c("Април 2021", "Юли 2021", "Ноември 2021", "Ноември 2021", "Октомври 2022", "Април 2023", 
               "Март 2017", "Ноември 2016", "Ноември 2016", "Октомври 2014", "Май 2013", "Юли 2009",
               "Октомври 2006", "Октомври 2006", "Юни 2005", "Юни 2001", "Ноември 2001", "Ноември 2001",
               "Април 1997", "Декември 1994", "Октомври 1991", "Октомври 1996", "Октомври 1996",
               "Януари 1992", "Януари 1992", "Октомври 2011", "Октомври 2011", "Юни 2024"),
  round = c("Първи тур", "Първи тур", "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур", 
            "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур", "Първи тур", "Втори тур",
            "Първи тур", "Първи тур", "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур",
            "Първи тур", "Втори тур", "Първи тур", "Втори тур", "Първи тур", "Втори тур", "Първи тур"))

write_parquet(act, "shiny/elections/election_activity.parquet")

act %>% 
  mutate(election = fct_reorder(election, activity)) %>% 
  ggplot(aes(activity, election, fill = round)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(activity, "%")), position = position_dodge(width = 1), 
            hjust = -0.1, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(.05, .5))) +
  labs(x = "Избирателна активност", y = NULL, fill = "Тур:") +
  theme(text = element_text(size = 16)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(vars(type_election))

# Mandati
mand <- tibble(
  ns = c("ВНС", "ВНС", "ВНС", "ВНС", "ВНС", "ВНС", "ВНС", "ВНС",
         "НС 36", "НС 36", "НС 36",
         "НС 37", "НС 37", "НС 37", "НС 37", "НС 37",
         "НС 38", "НС 38", "НС 38", "НС 38", "НС 38",
         "НС 39", "НС 39", "НС 39", "НС 39",
         "НС 40", "НС 40", "НС 40", "НС 40", "НС 40", "НС 40", "НС 40",
         "НС 41", "НС 41", "НС 41", "НС 41", "НС 41", "НС 41",
         "НС 42", "НС 42", "НС 42", "НС 42",
         "НС 43", "НС 43", "НС 43", "НС 43", "НС 43", "НС 43", "НС 43", "НС 43",
         "НС 44", "НС 44", "НС 44", "НС 44", "НС 44",
         "НС 45", "НС 45", "НС 45", "НС 45", "НС 45", "НС 45",
         "НС 46", "НС 46", "НС 46", "НС 46", "НС 46", "НС 46",
         "НС 47", "НС 47", "НС 47", "НС 47", "НС 47", "НС 47", "НС 47",
         "НС 48", "НС 48", "НС 48", "НС 48", "НС 48", "НС 48", "НС 48",
         "НС 49", "НС 49", "НС 49", "НС 49", "НС 49", "НС 49",
         "НС 50", "НС 50", "НС 50", "НС 50", "НС 50", "НС 50", "НС 50"),
  year = c(1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990,
           1991, 1991, 1991,
           1995, 1995, 1995, 1995, 1995,
           1997, 1997, 1997, 1997, 1997,
           2001, 2001, 2001, 2001,
           2005, 2005, 2005, 2005, 2005, 2005, 2005,
           2009, 2009, 2009, 2009, 2009, 2009,
           2013, 2013, 2013, 2013,
           2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014,
           2017, 2017, 2017, 2017, 2017,
           2021, 2021, 2021, 2021, 2021, 2021,
           2021, 2021, 2021, 2021, 2021, 2021,
           2021, 2021, 2021, 2021, 2021, 2021, 2021,
           2022, 2022, 2022, 2022, 2022, 2022, 2022,
           2023, 2023, 2023, 2023, 2023, 2023,
           2024, 2024, 2024, 2024, 2024, 2024, 2024),
  party = c("БСП", "СДС", "ДПС", "БЗНС", "ОС", "Независими", "ОПТ", "СП",
            "СДС", "БСП", "ДПС",
            "ДЛ", "СДС", "НС", "ДПС", "БББ",
            "ОДС", "ДЛ", "ОНС", "БЕ", "БББ",
            "НДСВ", "ОДС", "КБ", "ДПС",
            "КБ", "НДСВ", "ДПС", "НО АТАКА", "ОДС", "ДСБ", "БНС",
            "ГЕРБ", "КБ", "ДПС", "АТАКА", "СК", "РЗС",
            "ГЕРБ", "КБ", "ДПС", "АТАКА",
            "ГЕРБ", "КП БСП", "ДПС", "КП РБ", "КП ПФ", "КП ББЦ", "АТАКА", "КП АБВ",
            "ГЕРБ", "КП БСП", "КП ОП", "ДПС", "ВОЛЯ",
            "ГЕРБ", "ИТН", "БСП", "ДПС", "ДБ", "ИСМВ",
            "ИТН", "ГЕРБ", "БСП", "ДБ", "ДПС", "ИБГНИ",
            "ПП", "ГЕРБ-СДС", "ДПС", "БСП", "ИТН", "ДБ", "ВЪЗРАЖДАНЕ",
            "ГЕРБ-СДС", "ПП", "ДПС", "ВЪЗРАЖДАНЕ", "БСП", "ДБ", "БВ",
            "ГЕРБ-СДС", "ПП-ДБ", "ВЪЗРАЖДАНЕ", "ДПС", "БСП", "ИТН",
            "ГЕРБ-СДС", "ПП-ДБ", "ВЪЗРАЖДАНЕ", "ДПС", "БСП", "ИТН", "ВЕЛИЧИЕ"),
  mandates = c(211, 144, 23, 16, 2, 2, 1, 1,
               110, 106, 24,
               125, 69, 18, 15, 13,
               137, 58, 19, 14, 12,
               120, 51, 48, 21,
               82, 53, 34, 21, 20, 17, 13,
               117, 40, 37, 21, 15, 10,
               97, 84, 36, 23,
               84, 39, 38, 23, 19, 15, 11, 11,
               95, 80, 27, 26, 12,
               75, 51, 43, 30, 27, 14,
               65, 63, 36, 34, 29, 13,
               67, 59, 34, 26, 25, 16, 13,
               67, 53, 36, 27, 25, 20, 12,
               69, 64, 37, 36, 23, 11,
               68, 39, 38, 47, 19, 16, 13)) %>% 
  mutate(year = paste0("(", year, ")")) %>% 
  unite("ns_year", 1:2, sep = " ")

write_parquet(mand, "shiny/elections/mand.parquet")

colors_mand <- c(
  "ПП" = "yellow",
  "НДСВ" = "yellow",
  "ГЕРБ-СДС" = "blue",
  "СДС" = "blue",
  "ОДС" = "blue",
  "ДПС" = "purple",
  "БСП" = "red",
  "КБ" = "red",
  "ИТН" = "#0096FF",
  "ДБ" = "darkblue",
  "ПП-ДБ" = "darkblue",
  "ИСМВ" = "green",
  "ВЪЗРАЖДАНЕ" = "black",
  "БВ" = "darkgreen",
  "АТАКА" = "black",
  "ГЕРБ" = "blue",
  "КП ОП" = "black",
  "ВОЛЯ" = "pink",
  "РЗС" = "pink",
  "БББ" = "pink",
  "Независими" = "gray",
  "БЗНС" = "orange",
  "ОС" = "black",
  "ДЛ" = "red",
  "НС" = "black",
  "ОПТ" = "brown",
  "СП" = "red",
  "ОНС" = "black",
  "БЕ" = "red",
  "НО АТАКА" = "black",
  "ДСБ" = "darkblue",
  "БНС" = "red",
  "СК" = "darkblue",
  "КП БСП" = "red",
  "КП РБ" = "darkblue",
  "КП ПФ" = "black",
  "КП ББЦ" = "pink",
  "КП АБВ" = "red",
  "ИБГНИ" = "green")

mand %>% 
  filter(ns_year %in% c("НС 45 (2021)", "НС 46 (2021)", "НС 47 (2021)", "НС 48 (2022)", "НС 49 (2023)")) %>% 
  mutate(col = party, 
         party = reorder_within(party, mandates, ns_year)) %>%
  ggplot(aes(mandates, party, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = mandates), position = position_dodge(width = 1),
            hjust = -0.1, size = 5) +
  scale_fill_manual(values = colors_mand) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
  labs(y = NULL, x = "Брой мандати",
       caption = "Източник на данните: Wikipedia") +
  theme(text = element_text(size = 14), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  facet_wrap(vars(ns_year), scales = "free_y")
