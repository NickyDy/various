library(tidyverse)
library(arrow)
library(scales)
library(tidytext)
#library(sf)
#library(geodata)
#library(readxl)
glimpse(oct_2024)

votes <- read_parquet("shiny/elections/votes.parquet")
mand <- read_parquet("shiny/elections/mand.parquet")
activity <- read_parquet("shiny/elections/election_activity.parquet")
obsh_map <- st_read("data/obsh_map.gpkg")
obl_map <- st_read("data/obl_map.gpkg")
bg_map <- gadm("BGR", level = 0, path = tempdir())

write_parquet(votes, "data/votes.parquet")
write_parquet(votes, "shiny/elections/votes.parquet")
write_csv(oct_2024_new, "data/oct_2024.csv")

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
  "ВЕЛИЧИЕ" = "darkgreen",
  "ДПС-НH" = "purple",
  "АПС" = "purple",
  "МЕЧ" = "maroon",
  "БСП-ОЛ" = "red")

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
  #filter(code == "13700022") %>%
  mutate(vote_date = fct_relevel(vote_date,
                                 "Октомври_2024",
                                 "Юни_2024",
                                 "Април_2023",
  															 "Октомври_2022", 
  															 "Ноември_2021", 
  															 "Юли_2021", 
  															 "Април_2021", 
  															 "Март_2017")) %>%
  group_by(vote_date, party) %>%
  summarise(sum_votes = sum(votes)) %>%
  filter(sum_votes >= 2) %>%
  mutate(party = fct_reorder(party, sum_votes)) %>%
  ggplot(aes(sum_votes, party)) +
  geom_col(aes(fill = party), position = "dodge", show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(labels = scales::label_wrap(50)) +
  scale_x_continuous(expand = expansion(mult = c(.05, .9))) +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = space_s(sum_votes)), 
  					position = position_dodge(width = 1), 
  					hjust = -0.05, size = 10, size.unit = "pt") +
  theme(text = element_text(size = 12), 
  			axis.text.x = element_blank(), 
  			axis.ticks.x = element_blank()) +
  labs(y = NULL, x = "Брой гласове", title = NULL,
       caption = "Бележка: Оцветени са само партиите и коалициите влизали/щи в Парламента, останалите са в сиво.
       Източник на данните: ЦИК.") +
  facet_wrap(~ vote_date, nrow = 1)

votes %>%
  #filter(oblast == "Пловдив град") %>% 
  mutate(vote_date = fct_relevel(vote_date,
                                 "Октомври_2024",
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
       caption = "Бележка: Оцветени са само партиите и коалициите влизали/щи в Парламента, останалите са в сиво.
       Източник на данните: ЦИК.") +
  facet_wrap(~ vote_date, nrow = 1)

votes %>% 
  filter(vote_date %in% c("Октомври_2024", "Юни_2024")) %>% 
  pivot_wider(names_from = vote_date, values_from = votes) %>%
  mutate(abs_diff = abs(Октомври_2024 - Юни_2024)) %>%
  summarise(volatility_index = sum(abs_diff, na.rm = T) / 2, 
            .by = c(oblast, obshtina, section, code)) %>%
  filter(!oblast == "Извън страната") %>% 
  arrange(-volatility_index) %>%
  slice_head(n = 200) %>%
  mutate(code = reorder_within(code, volatility_index, oblast)) %>%
  ggplot(aes(volatility_index, code, fill = oblast)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = section), 
            position = position_dodge(width = 1), 
            hjust = - 0.02, size = 8, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.05, .9))) +
  scale_y_reordered() +
  labs(y = "Секция", x = "Индекс на волатилност") +
  theme(axis.title = element_text(hjust = 1)) +
  facet_wrap(vars(oblast), scales = "free_y", nrow = 3)
#----------------------------------
votes %>% 
  #filter(party == "ДПС-НH", vote_date == "Октомври_2024") %>%
  mutate(party = fct_recode(party, "ДПС-НH" = "ДПС")) %>% 
  filter(vote_date %in% c("Октомври_2024", "Юни_2024")) %>%
  pivot_wider(names_from = vote_date, values_from = votes) %>%
  mutate(diff = Октомври_2024 / Юни_2024) %>%
  filter(Юни_2024 > 2 & diff > 10) %>% view
  unite("section_code", c("section", "code"), sep = " - ") %>% 
  mutate(section_code = reorder_within(section_code, diff, party)) %>%
  ggplot(aes(diff, section_code, fill = party)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  scale_fill_manual(values = c("ДПС-НH" = "purple", "ГЕРБ-СДС" = "blue", "ИТН" = "#0096FF", 
                               "ПП-ДБ" = "darkblue", "ЛЕВИЦАТА!" = "red")) +
  scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = diff), position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
  labs(x = "Брой гласове", y = "Населено място - секция", 
       title = "Разлика в броя гласове на последните (октомври, 2024) в сравнение с предпоследните (юни, 2024) избори, получени от съответните партии.\nПоказани са само секции с повече от 80 гласа разлика.") +
  facet_wrap(vars(party), scales = "free_y", nrow = 1) +
  theme(text = element_text(size = 14))
  
  # votes %>%
  #   filter(vote_date %in% c("Юни_2024", "Април_2023"), party %in% c("ДПС", "ГЕРБ-СДС")) %>%
  #   pivot_wider(names_from = vote_date, values_from = votes) %>%
  #   mutate(diff = Юни_2024 - Април_2023) %>%
  #   filter(Април_2023 < 50 & diff > 60) %>%
  #   unite("section_code", c("section", "code"), sep = "_")

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
  filter(party %in% c("ДПС-НH"))
map <- obsh_map %>% 
  left_join(db, by = c("obshtina_bg" = "obshtina")) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(vote_date %in% c("Октомври_2024")) %>%
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
       title = "Вот за ДПС–НН по общини на последните избори (октомври, 2024 г.)") +
  scale_fill_gradient(low = "white", high = "purple")
#+ annotation_scale(location = "br", width_hint = 0.2, text_cex = 1.1) +
# annotation_north_arrow(location = "tr", which_north = "true",
# pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
# style = north_arrow_fancy_orienteering)
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
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, 6837737, 6859318, 6817914, 6859318, NA, NA, 6593275,
                   6601262),
  voters = c(3334283, 2775410, 2669260, 2310903, 2601963, 2683606, 3682151, 3943004, 3540829, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 4317161, 4215145, 5139891, 5206226, NA, NA, 2268644,
             2570629),
  activity = c(50.61, 42.19, 40.23, 34.84, 39.41, 40.69, 54.07, 57.47, 51.62, 48.66, 51.33, 60.20, 42.51, 41.21,
               55.76, 67, 41.8, 55.1, 62.4, 75.3, 83.9, 63.14, 61.67, 75.39, 75.90, 52.29, 48.25, 34.41,
               38.94),
  type_election = c("Парламент", "Парламент", "Парламент и президент", "Парламент и президент", 
                    "Парламент", "Парламент", "Парламент", "Президент", "Президент", "Парламент", "Парламент",
                    "Парламент", "Президент", "Президент", "Парламент", "Парламент", "Президент", "Президент",
                    "Парламент", "Парламент", "Парламент", "Президент", "Президент", "Президент", "Президент",
                    "Президент", "Президент", "Парламент", "Парламент"),
  election = c("Април 2021", "Юли 2021", "Ноември 2021", "Ноември 2021", "Октомври 2022", "Април 2023", 
               "Март 2017", "Ноември 2016", "Ноември 2016", "Октомври 2014", "Май 2013", "Юли 2009",
               "Октомври 2006", "Октомври 2006", "Юни 2005", "Юни 2001", "Ноември 2001", "Ноември 2001",
               "Април 1997", "Декември 1994", "Октомври 1991", "Октомври 1996", "Октомври 1996",
               "Януари 1992", "Януари 1992", "Октомври 2011", "Октомври 2011", "Юни 2024", "Октомври 2024"),
  round = c("Първи тур", "Първи тур", "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур", 
            "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур", "Първи тур", "Втори тур",
            "Първи тур", "Първи тур", "Първи тур", "Втори тур", "Първи тур", "Първи тур", "Първи тур",
            "Първи тур", "Втори тур", "Първи тур", "Втори тур", "Първи тур", "Втори тур", "Първи тур",
            "Първи тур")) %>% 
  mutate(election = factor(election, 
                           levels = c("Октомври 1991", 
                                      "Януари 1992",
                                      "Декември 1994",
                                      "Октомври 1996",
                                      "Април 1997",
                                      "Юни 2001",
                                      "Ноември 2001",
                                      "Юни 2005",
                                      "Октомври 2006",
                                      "Юли 2009",
                                      "Октомври 2011",
                                      "Май 2013",
                                      "Октомври 2014",
                                      "Ноември 2016",
                                      "Март 2017",
                                      "Април 2021",
                                      "Юли 2021", 
                                      "Ноември 2021",
                                      "Октомври 2022", 
                                      "Април 2023", 
                                      "Юни 2024",
                                      "Октомври 2024")))

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
         "НС 50", "НС 50", "НС 50", "НС 50", "НС 50", "НС 50", "НС 50", 
         "НС 51", "НС 51", "НС 51", "НС 51", "НС 51", "НС 51", "НС 51", "НС 51"),
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
           2024, 2024, 2024, 2024, 2024, 2024, 2024, 
           2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
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
            "ГЕРБ-СДС", "ПП-ДБ", "ВЪЗРАЖДАНЕ", "ДПС", "БСП", "ИТН", "ВЕЛИЧИЕ",
            "ГЕРБ-СДС", "ПП-ДБ", "ВЪЗРАЖДАНЕ", "ДПС-НH", "БСП", "АПС", "ИТН", "МЕЧ"),
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
               68, 39, 38, 47, 19, 16, 13,
               69, 37, 35, 30, 20, 19, 18, 12)) %>% 
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
#---------------------------------------------
library(fs)
library(readxl)

oct_2024_pref <- read_parquet("data/pref/oct_2024_pref.parquet")
june_2024_pref <- read_parquet("data/pref/june_2024_pref.parquet")
apr_2023_pref <- read_parquet("data/pref/apr_2023_pref.parquet")
oct_2022_pref <- read_parquet("data/pref/oct_2022_pref.parquet")
nov_2021_pref <- read_parquet("data/pref/nov_2021_pref.parquet")
jul_2021_pref <- read_parquet("data/pref/jul_2021_pref.parquet")
apr_2021_pref <- read_parquet("data/pref/apr_2021_pref.parquet")

apr_2021_pref %>% map_dfr(sum(is.na(.)))
apr_2021_pref %>% map_dfr(sum(. == 0))

#write_parquet(apr_2023_pref_new, "data/pref/apr_2023_pref.parquet")

unzip(zipfile = "~/Downloads/spreadsheet.zip", exdir = "~/Downloads/spreadsheet")
files <- dir_ls("~/Downloads/spreadsheet", glob = "*.xlsx")

sheet_6 <- function(x){
  read_excel(x, sheet = 6, col_types = c("text"))
}

sheet_5 <- function(x){
  read_excel(x, sheet = 5, col_types = c("text"))
}

df <- map(files, sheet_5) %>% bind_rows()

df %>% mutate(`т. 8 (т. 12) Действителни гласове` = parse_number(`т. 8 (т. 12) Действителни гласове`)) %>% 
  count(`Вид бюлетини`)

df %>%
  mutate(machine21 = parse_number(machine21)) %>% 
  select(contains("machine")) %>% 
  pivot_longer(everything()) %>% summarise(s = sum(value, na.rm = T))

glimpse(df_pref)

apr_2023_pref %>% 
  mutate(type_pref = fct_other(type_pref, keep = "Без", other_level = "Със")) %>% 
  count(ballot, wt = pref)

apr_2023_pref %>%
  summarise(m = sum(Машина), h = sum(Хартия))

df_pref <- tibble(
  vote_date = c("Април 2021", "Юли 2021", "Ноември 2021", "Октомври 2022", "Април 2023", "Юни 2024", "Октомври 2024"),
  `Общо гласували` = c(3334283, 2775410, 2669260, 2601963, 2683606, 2268849, 2570629),
  Хартия = c(2423789, NA, NA, NA, 1052101, 1272717, 1510210),
  Машина = c(776045, NA, NA, NA, 1476551, 734341, 926246),
  `С преференция` = c(1316689, 970867, 624254, 846341, 892300, 842650, 1027894),
  `С преференция (хартия)` = c(963756, NA, NA, NA, 354052, 489889, 600735),
  `С преференция (машина)` = c(353016, NA, NA, NA, 538248, 352761, 427159),
  `Без преференция` = c(1711756, 1591038, 1774460, 1486967, 1456524, 1164408, 1262491),
  `Без преференция (хартия)` = c(NA, NA, NA, NA, 604250, 782828, 804228),
  `Без преференция (машина)` = c(NA, NA, NA, NA, 852274, 381580, 458263))

df_pref %>% 
  pivot_longer(-vote_date) %>% 
  filter(name %in% c("Общо гласували", "Хартия", "Машина")) %>%
  mutate(vote_date = fct_inorder(vote_date), name = fct_inorder(name)) %>% 
  ggplot(aes(value, vote_date, fill = vote_date)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = space_s(value)), position = position_dodge(width = 1),
            hjust = -0.1, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
  theme(text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Брой гласове", y = NULL, 
       title = "Въпрос 2: Брой хора избрали да гласуват с машина и хартия?") +
  facet_wrap(vars(name))

june_2024 <- df %>% 
  pivot_longer(9:48, names_to = "type_pref", values_to = "pref") %>%
  select(code = `Номер на СИК`, obl = Област, obsh = `Община/Държава`,
       sett = `Населено място`, ballot = `Вид бюлетини`, party = `П/КП/МК`,
       type_pref, pref) %>% drop_na(pref) %>% filter(pref > 0) %>% 
  mutate(pref = as.numeric(pref))

june_2024 %>%
  filter(type_pref == "Без") %>% 
  mutate(party = str_wrap(party, 140)) %>% 
  summarise(pref = sum(pref), .by = c(ballot, party)) %>% 
  mutate(party = fct_reorder(party, pref)) %>% 
  ggplot(aes(pref, party, fill = party)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = space_s(pref)), position = position_dodge(width = 1),
            hjust = -0.1, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(.01, .6))) +
  scale_fill_manual(values = colors) +
  labs(x = "Брой гласове с преференции", y = NULL,
       title = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(vars(ballot))

june_2024_pref %>% filter(!type_pref == "Без") %>% count(ballot, wt = pref)
oct_2024_pref %>% filter(!type_pref == "Без") %>% count(ballot, wt = pref)
apr_2023_pref %>% filter(type_pref == "Без") %>% count(ballot, wt = pref)
oct_2022_pref %>% 
  mutate(type_pref = fct_other(type_pref, keep = "Без", other_level = "Със")) %>% 
  count(type_pref, wt = pref)
nov_2021_pref %>% 
  mutate(type_pref = fct_other(type_pref, keep = "Без", other_level = "Със")) %>% 
  count(type_pref, wt = pref)
apr_2021_pref %>% 
  mutate(type_pref = fct_other(type_pref, keep = "Без", other_level = "Със")) %>% 
  count(type_pref, wt = pref)

oct_2024 %>% 
  mutate(party = fct_recode(party, "ВЕЛИЧИЕ" = "ПП ВЕЛИЧИЕ", 
                            "ГЛАС НАРОДЕН" = "ПП ГЛАС НАРОДЕН",
                            "ИТН" = "ПП ИМА ТАКЪВ НАРОД", "АПС" = "АЛИАНС ЗА ПРАВА И СВОБОДИ – АПС",
                            "ДПС-НH" = "ДПС-Ново начало", "МЕЧ" = "ПП МЕЧ",
                            "НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА" = "ПП НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА",
                            "ПП-ДБ" = "КОАЛИЦИЯ ПРОДЪЛЖАВАМЕ ПРОМЯНАТА – ДЕМОКРАТИЧНА БЪЛГАРИЯ",
                            "БСП-ОЛ" = "БСП – ОБЕДИНЕНА ЛЕВИЦА")) %>% 
  count(party, ballot, wt = votes) %>% 
  mutate(col = party, party = reorder_within(party, n, ballot)) %>% 
  ggplot(aes(n, party, fill = col)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  geom_text(aes(label = space_s(n)), position = position_dodge(width = 1),
            hjust = -0.1, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(.01, .4))) +
  scale_fill_manual(values = colors) +
  labs(x = "Брой гласове", y = NULL,
       title = "Гласуване с машина или хартия на последните избори") +
  theme(text = element_text(size = 16)) +
  facet_wrap(vars(ballot), scales = "free_y")

oct_2024_new <- oct_2024 %>% 
  mutate(code = str_replace(code, "^0", "")) %>% 
  pivot_longer(-c(vote_date:section), names_to = "party", values_to = "votes") %>% 
  mutate(oblast = case_when(
    str_detect(code, "^16") ~ "Пловдив град",
    str_detect(code, "^17") ~ "Пловдив - област",
    str_detect(code, "^26") ~ "София - област",
    str_detect(code, "^23") ~ "София 23",
    str_detect(code, "^24") ~ "София 24",
    str_detect(code, "^25") ~ "София 25",
    .default = oblast)) %>% 
  mutate(obshtina = case_when(
    oblast == "Варна" & obshtina == "Бяла" ~ "Бяла (Варненско)",
    oblast == "Русе" & obshtina == "Бяла" ~ "Бяла (Русенско)",
    .default = obshtina)) %>%
  mutate(obshtina = str_replace(obshtina, "София", "Столична"),
         obshtina = str_replace(obshtina, "Добрич-град", "Добрич"),
         obshtina = fct_recode(obshtina, "Великобритания" = "Обединено кралство Великобритания и Северна Ирландия"),
         party = fct_recode(party, "ВЕЛИЧИЕ" = "ПП ВЕЛИЧИЕ", 
                            "ГЛАС НАРОДЕН" = "ПП ГЛАС НАРОДЕН",
                            "ИТН" = "ПП ИМА ТАКЪВ НАРОД", "АПС" = "АЛИАНС ЗА ПРАВА И СВОБОДИ – АПС",
                            "ДПС-НH" = "ДПС-Ново начало", "МЕЧ" = "ПП МЕЧ",
                            "НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА" = "ПП НАРОДНА ПАРТИЯ ИСТИНАТА И САМО ИСТИНАТА",
                            "ПП-ДБ" = "КОАЛИЦИЯ ПРОДЪЛЖАВАМЕ ПРОМЯНАТА – ДЕМОКРАТИЧНА БЪЛГАРИЯ",
                            "БСП-ОЛ" = "БСП – ОБЕДИНЕНА ЛЕВИЦА")) %>%
  mutate(obshtina = factor(obshtina), party = factor(party))

filipovci_sf <- c("254619071","254619072","254619132")
hristo_botev_sf <- c("244607070","244607071","244607072","244607073","244607074","244607075","244607076","244607077")
fakulteta_sf <- c("254611059","254611061","254611062","254611063","254611064","254611065","254611066","254611067",
                  "254611071","254611074")
sheker_mahala_pl <- c("162204014","162204015","162204016","162204033","162204069")
stolipinovo_pl <- c("162202044","162202045","162202046","162202047","162202048","162202049","162202050","162202051",
                    "162202052","162202053", "162202021","162202022","162202023","162202024","162202025","162202026",
                    "162202027","162202028", "162202029","162202030", "162202031","162202032","162202033","162202034",
                    "162202035","162202036","162202037", "162202038","162202039","162202040","162202041","162202042",
                    "162202043")
loznica_asen <- c("170100054","170100055","170100056","170100057","170100058","170100059","170100101","170100104")
rakovica_ber <- c("120200018","120200019","120200020")
orlova_chuka_bla <- c("10300006","10300007","10300099", "10300008", "10300009","10300010","10300100")
grobishtata_doce_del <- c("11100017","11100018","11100019")
petar_beron_qm <- c("312600041","312600042","312600043","312600044","312600045","312600046","312600047","312600048")

df <- read_delim("votes_02.04.2023.txt", delim = ";", col_names = c("no", "code", "admin", 
                                                                    "party1", "total1", "paper1", "machine1",
                                                                    "party2", "total2", "paper2", "machine2",
                                                                    "party3", "total3", "paper3", "machine3",
                                                                    "party4", "total4", "paper4", "machine4",
                                                                    "party5", "total5", "paper5", "machine5",
                                                                    "party6", "total6", "paper6", "machine6",
                                                                    "party7", "total7", "paper7", "machine7",
                                                                    "party8", "total8", "paper8", "machine8",
                                                                    "party9", "total9", "paper9", "machine9",
                                                                    "party10", "total10", "paper10", "machine10",
                                                                    "party11", "total11", "paper11", "machine11",
                                                                    "party12", "total12", "paper12", "machine12",
                                                                    "party13", "total13", "paper13", "machine13",
                                                                    "party14", "total14", "paper14", "machine14",
                                                                    "party15", "total15", "paper15", "machine15",
                                                                    "party16", "total16", "paper16", "machine16",
                                                                    "party17", "total17", "paper17", "machine17",
                                                                    "party18", "total18", "paper18", "machine18",
                                                                    "party19", "total19", "paper19", "machine19",
                                                                    "party20", "total20", "paper20", "machine20",
                                                                    "party21", "total21", "paper21", "machine21",
                                                                    "party22", "total22", "paper22", "machine22",
                                                                    "party23", "total23", "paper23", "machine23",
                                                                    "party24", "total24", "paper24", "machine24",
                                                                    "party25", "total25", "paper25", "machine25",
                                                                    "party26", "total26", "paper26", "machine26",
                                                                    "party27", "total27", "paper27", "machine27",
                                                                    "party28", "total28", "paper28", "machine28",
                                                                    "party29", "total29", "paper29", "machine29",
                                                                    "party30", "total30", "paper30", "machine30",
                                                                    "party31", "total31", "paper31", "machine31"))
