
library(tidyverse)
library(pwr)

cohen.ES(test = c("p", "t", "r", "anov", "chisq", "f2"),
         size = c("small", "medium", "large"))

cohen.ES(test = "chisq", size = "large")

ES.h(p1, p2)

ES.w1(P0, P1)

ES.w2(P)

plot(an, xlab = "Sample size", ylab = "Power")
ggsave("plot.png", width = 10, height = 5)

pwr.2p.test(h = NULL, n = NULL, sig.level = 0.05, power = NULL,
            alternative = c("two.sided","less","greater"))

pwr.2p2n.test(h = NULL, n1 = NULL, n2 = NULL, sig.level = 0.05, power = NULL,
              alternative = c("two.sided", "less","greater"))

an<-pwr.anova.test(k = 4, n = NULL, f = 0.25, sig.level = 0.05, power = 0.8)

chi<-pwr.chisq.test(w = 0.4, N = NULL, df = (3-1)*(2-1), sig.level = 0.05, power = 0.8)

pwr.f2.test(u = NULL, v = NULL, f2 = NULL, sig.level = 0.05, power = NULL)

pwr.norm.test(d = NULL, n = NULL, sig.level = 0.05, power = NULL,
              alternative = c("two.sided","less","greater"))

pwr.p.test(h = NULL, n = NULL, sig.level = 0.05, power = NULL,
           alternative = c("two.sided","less","greater"))

pwr.r.test(n = NULL, r = NULL, sig.level = 0.05, power = NULL,
           alternative = c("two.sided", "less","greater"))

t<-pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.8,
           type = c("paired"), alternative = c("two.sided"))

pwr.t2n.test(n1 = NULL, n2= NULL, d = NULL, sig.level = 0.05, power = NULL,
             alternative = c("two.sided", "less","greater"))