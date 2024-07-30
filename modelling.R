library(tidyverse)
library(tidymodels)

credit <- read_csv("islr2/Credit.csv")
boston <- read_csv("islr2/Boston.csv") %>% 
  select(-c(...1, rad, tax))

glimpse(df)

# Regression
mod <- lm(medv ~ ., data = boston)
tidy(mod)
glance(mod)
tibble(predict(mod), residuals(mod), hatvalues(mod))
plot(predict(mod), residuals(mod))
plot(predict(mod), rstudent(mod))
performance::check_model(mod)

# Regression
df <- read_csv("islr2/Carseats.csv") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor)

mod <- linear_reg(engine = "lm") %>% fit(sales ~ ., data = df)
tidy(mod)
glance(mod)
contrasts(df$shelve_loc)

new_data <- df %>% slice_sample(n = 1) %>% select(-sales)
predict(mod, new_data = new_data)

# Classification
df <- read_csv("islr2/Default.csv") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor)

# GLM
df <- read_csv("islr2/Bikeshare.csv") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor)

df <- read_csv("islr2/Smarket.csv") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor)

df %>% select(-9) %>% cor()

glm_fit <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume, data = df, family = binomial)
summary(glm_fit)

predict(glm_fit, type = "response")

# Tidymodels
data(trees)
trees <- as_tibble(trees)
trees

trees %>% corrr::correlate()

reg_fit <- lm(Volume ~ ., data = trees)
reg_fit

par(mfrow = c(1, 2))
plot(reg_fit, which = c(1, 2))

library(ggfortify)
autoplot(reg_fit, which = c(1, 2))

ames <- read_csv("https://raw.githubusercontent.com/koalaverse/homlr/master/data/ames.csv")
glimpse(ames)

library(AmesHousing)
library(reshape2)
ames <- ames_raw %>% janitor::clean_names()
glimpse(ames_train)

set.seed(123)
split <- initial_split(ames, prop = 0.7, strata = "sale_price")
ames_train  <- training(split)
ames_test   <- testing(split)

mod <- linear_reg(engine = "lm") %>% 
  fit(sale_price ~ garage_cars + garage_area, data = ames)

ames %>% 
  is.na() %>%
  melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + 
    geom_raster() + 
    coord_flip() +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    scale_fill_grey(name = "", labels = c("Present", "Missing")) +
    xlab("Observation") +
    theme(axis.text.y  = element_text(size = 4))









