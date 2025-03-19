library(tidyverse)
library(pwrss)

pwrss.t.2means(mu1 = 0.5, kappa = 1,
               power = .80, alpha = 0.05, 
               alternative = "not equal")

pwrss.f.ancova(eta2 = 0.059, n.levels = 3,
               power = .80, alpha = 0.05)
