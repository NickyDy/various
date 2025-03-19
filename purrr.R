library(tidyverse)

diss <- read_csv("data/diss.csv")
glimpse(diss)

x <- list(a = 1:10, b = 11:20, c = 21:30)
y <- list(1, 2, 3)
z <- list(4, 5, 6)

l1 <- list(x = c("a", "b"), y = c("c", "d"))
l2 <- list(x = "a", y = "z")

map_dbl(x, mean)
map_int(x, length)
map_chr(l1, paste, collapse = "")
map_lgl(x, is.integer)
map_vec(l1, paste, collapse = "")
walk(x, print)

map2_dbl(y, z, ~ .x / .y)
map2_int(y, z, `+`)
map2_chr(l1, l2, paste, collapse = ", ", sep = ":")
map2_lgl(l2, l1, `%in%`)
map2_vec(l1, l2, paste, collapse = ", ", sep = ":")
walk2(objs, paths, save)

pmap_dbl(list(y, z), ~ .x / .y)
pmap_int(list(y, z), `+`)
pmap_chr(list(l1, l2), paste, collapse = ", ", sep = ":")
pmap_lgl(list(l2, l1), `%in%`)
pmap_vec(list(l1, l2), paste, collapse = ", ", sep = ":")
pwalk(list(objs, paths), save)

modify(x, ~ . + 2)
modify(diss$elev, ~ . + 20)
modify_at(x, "b", ~ . + 2)
modify_if(x, is.numeric,~.+2)
modify_if(diss, is.numeric, ~ . + 100)
modify_depth(x, 1, ~ . + 2)

reduce(x, sum)
accumulate(x, sum)
reduce(diss$elev, sum)
accumulate(diss$elev, sum)

compact(x)
keep_at(x, "a")
keep_at(diss, c("elev", "slope"))
set_names(x, c("p", "q", "r"))
set_names(x, toupper)
set_names(diss, toupper)

keep(x, is.numeric)
keep(diss, is.numeric)
head_while(x, is.character)
head_while(diss, is.character)
detect(x, is.character)
detect(diss, is.character)
detect_index(diss, is.character)
every(diss, is.character)
some(x, is.character)
some(diss, is.character)
none(x, is.character)
none(diss, is.character)
has_element(x, "foo")
has_element(diss$top, "concave")

diss %>% pluck("elev")
assign_in(x, "b", 5)
assign_in(diss, "elev", 1000)
modify_in(x, "b", abs)
modify_in(diss, "slope", abs)

list_flatten(x)

x1 <- list(a = 1, b = 2, c = 3)
x2 <- list(
  a = data.frame(x = 1:2),
  b = data.frame(y = "a")
)

list_c(x1)
list_rbind(x2)
list_cbind(x2)

list_transpose(x)
