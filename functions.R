library(tidyverse)
library(scales)

diss <- read_csv("data/diss.csv")

find_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fct_sort <-  function(.f, .fun = sort) {
  f = forcats:::check_factor(.f)
  fct_relevel(f, .fun(levels(f)))
}

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

pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c('#feb8cd', '#ffffff'),
    domain = c(2121, 1500)
  )
  f_pos <- scales::col_numeric(
    palette = c('#ffffff', '#69cfd5'),
    domain = c(1500, 926)
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

comma <- function(x) format(x, digits = 2, big.mark = ",")

df_types <- function(df) {
	tibble(
		col_name = names(df), 
		col_type = map_chr(df, vctrs::vec_ptype_full),
		n_miss = map_int(df, \(x) sum(is.na(x)))
	)
}

diss %>% gt() %>% fmt_number(columns = 6:20, decimals = 1)

summary6 <- function(data, var) {
	data |> summarize(
		min = min({{ var }}, na.rm = TRUE),
		mean = mean({{ var }}, na.rm = TRUE),
		median = median({{ var }}, na.rm = TRUE),
		max = max({{ var }}, na.rm = TRUE),
		n = n(),
		n_miss = sum(is.na({{ var }})),
		.groups = "drop"
	)
}

count_prop <- function(df, var, sort = FALSE) {
	df |>
		count({{ var }}, sort = sort) |>
		mutate(prop = n / sum(n))
}

histogram <- function(df, var, binwidth = NULL) {
	df |> 
		ggplot(aes(x = {{ var }})) + 
		geom_histogram(binwidth = binwidth)
}

linearity_check <- function(df, x, y) {
	df |>
		ggplot(aes(x = {{ x }}, y = {{ y }})) +
		geom_point() +
		geom_smooth(method = "loess", color = "red", se = FALSE) +
		geom_smooth(method = "lm", color = "blue", se = FALSE) 
}

tree_plot <- function(var_to, pal = "Set1") {
	
	for_title <- as_label(enquo(var_to))
	for_title <- str_to_title(str_replace_all(for_title, "_", " "))
	diss %>% 
		count({{var_to}}, sort = T) %>%
		mutate(prop = str_c(round(n / sum(n) * 100, 0), "%"),
					 label = str_c({{var_to}}, " ", prop)) %>%
		treemap(
			index = "label",
			vSize = "n",
			type = "index",
			title = glue(for_title," Proportions"),
			palette = pal,
			border.col = c("black"),
			border.lwds = 1,
			fontsize.labels = 18
		)
}

gruop_treemap_plot <- function(var_1, var_2 = veg_type, var_col = "Set1"){
	
	for_title_1 <- as_label(enquo(var_1))
	for_title_1 <- str_to_title(str_replace_all(for_title_1, "_", " "))
	
	for_title_2 <- as_label(enquo(var_2))
	for_title_2 <- str_to_title(str_replace_all(for_title_2, "_", " "))
	
	
	diss %>%
		select({{var_1}}, {{var_2}}) %>% 
		mutate(var_1_ex = {{var_1}},
					 var_2_ex = {{var_2}}) %>% 
		count(var_1_ex, var_2_ex) %>% 
		arrange(desc(n)) %>%
		treemap(title = glue(for_title_2, " proportion by ", for_title_1),
						index = c("var_1_ex", "var_2_ex"),
						vSize = "n",
						type = "index",
						palette = var_col,
						fontsize.labels = c(15, 12),
						fontcolor.labels = c("black","white"),
						fontface.labels = c(2, 2),
						align.labels = list(c("center", "center"),
																c("left", "bottom")),
						overlap.labels = 0.5,
						border.col=c("white","gray"),
						border.lwds=c(6,1),
						inflate.labels = F
		)
	
}

autoplot_category <- function(dataset, x, y) {
	
	summary_stats <- dataset %>% 
		group_by({{y}}) %>% 
		summarise(
			mean = mean({{x}}, na.rm = TRUE),
			sd = sd({{x}}, na.rm = TRUE),
			n = n(),
			sem = sd/sqrt(n),
			uppr = mean + 1.96*sem,
			lwr = mean - 1.96*sem)
	
	ggplot(data = dataset, aes(x = {{x}}, y ={{y}}, fill = {{y}}))+
		geom_vline(xintercept = mean(dataset %>% pull({{x}}), na.rm = TRUE), size = 1.5, alpha = 0.2)+
		geom_density_ridges(alpha = 0.2,color = "#0000001A")+
		geom_boxplot(width = 0.5, fill = NA, outlier.colour = "darkred", position = position_nudge(y = 0.5),
								 size = 0.5, outlier.size = 1)+
		geom_errorbar(data = summary_stats, aes(x = mean, xmin = lwr, xmax = uppr ), width = 0.5, color = "darkred",
									position = position_nudge(y = 0.5), size = 0.7, alpha = 0.9)+
		geom_point(data = summary_stats, aes(x = mean), color = "darkred", shape = 18, size = 2,
							 position = position_nudge(y = 0.5))+
		geom_label(data = summary_stats, aes(x = -5, label = n), fill = NA, position = position_nudge(y = 0.5))+
		ggtitle(paste(substitute(x) ,"versus", substitute(y)))+
		labs(x = NULL, y = NULL)+
		theme(legend.position = "none")
}

expand_dates <- function(df) {
	df |> 
		mutate(
			across(where(is.Date), list(year = year, month = month, day = mday))
		)
}
