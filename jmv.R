library(tidyverse)
library(jmv)

diss <- read_csv("data/diss.csv", col_types = "fffff")
glimpse(diss)

# DESCRIPTIVES
otto %>% 
  group_by(participant_gender) %>% 
	descriptives(vars = c(rain),
  splitBy = NULL, freq = FALSE,
  desc = "columns", hist = FALSE, dens = FALSE, bar = FALSE,
  barCounts = FALSE, box = F, violin = FALSE, dot = FALSE,
  dotType = "jitter", boxMean = FALSE, boxLabelOutliers = TRUE,
  qq = FALSE, n = TRUE, missing = TRUE, mean = TRUE,
  median = TRUE, mode = F, sum = FALSE, sd = TRUE,
  variance = T, range = T, min = TRUE, max = TRUE,
  se = T, ci = T, ciWidth = 95, iqr = FALSE,
  skew = FALSE, kurt = FALSE, sw = T, pcEqGr = FALSE,
  pcNEqGr = 4, pc = FALSE, pcValues = "25,50,75")

# ONE SAMPLE T-TEST
ttestOneS(
  data = diss,
  vars = s_ha,
  students = TRUE,
  bf = FALSE,
  bfPrior = 0.707,
  wilcoxon = TRUE,
  testValue = 40,
  hypothesis = "dt",
  norm = TRUE,
  qq = FALSE,
  meanDiff = FALSE,
  ci = FALSE,
  ciWidth = 95,
  effectSize = TRUE,
  ciES = TRUE,
  ciWidthES = 95,
  desc = TRUE,
  plots = FALSE,
  miss = "perAnalysis",
  mann = TRUE)

# INDEPENDENT SAMPLES T-TEST
raw_df %>% 
  pivot_longer(2:3, names_to = "treatment", values_to = "value") %>% 
  mutate(treatment = factor(treatment, levels = c("Pre-EB Training", "Post-EB Training"))) %>%
  filter(question == 1) %>% 
  ttestIS(
  vars = value,
  group = treatment,
  students = TRUE,
  bf = FALSE,
  bfPrior = 0.707,
  welchs = TRUE,
  mann = TRUE,
  hypothesis = "different",
  norm = FALSE,
  qq = FALSE,
  eqv = FALSE,
  meanDiff = FALSE,
  ci = FALSE,
  ciWidth = 95,
  effectSize = TRUE,
  ciES = FALSE,
  ciWidthES = 95,
  desc = TRUE,
  plots = FALSE,
  miss = "perAnalysis")

# PAIRED SAMPLES T-TEST
raw_df %>% 
  filter(question == 1) %>% 
  ttestPS(
  pairs = list(list(i1 = "Pre-EB Training", i2 = "Post-EB Training")),
  students = TRUE,
  bf = FALSE,
  bfPrior = 0.707,
  wilcoxon = TRUE,
  hypothesis = "different",
  norm = TRUE,
  qq = FALSE,
  meanDiff = FALSE,
  ci = FALSE,
  ciWidth = 95,
  effectSize = TRUE,
  ciES = FALSE,
  ciWidthES = 95,
  desc = TRUE,
  plots = FALSE,
  miss = "perAnalysis")

# ONE-WAY ANOVA
anovaOneW(
  data = sds,
  deps = value,
  group = timepoint,
  welchs = TRUE,
  fishers = T,
  miss = "perAnalysis",
  desc = TRUE,
  descPlot = F,
  norm = TRUE,
  qq = FALSE,
  eqv = TRUE,
  phMethod = "gamesHowell",
  phMeanDif = FALSE,
  phSig = TRUE,
  phTest = FALSE,
  phFlag = FALSE)

# ANOVA
ANOVA(
  data = diss,
  dep = ph,
  factors = c(exp, top),
  effectSize = "eta",
  modelTest = TRUE,
  modelTerms = NULL,
  ss = "3",
  homo = TRUE,
  norm = TRUE,
  qq = FALSE,
  contrasts = NULL,
  postHoc = ph ~ exp + top,
  postHocCorr = list("holm"),
  postHocES = list(),
  emMeans = list(list()),
  emmPlots = TRUE,
  emmPlotData = FALSE,
  emmPlotError = "ci",
  emmTables = FALSE,
  emmWeights = TRUE,
  ciWidthEmm = 95)

# REPEATED MEASURES ANOVA
anovaRM(
  data = sds,
  rm = list(list(label = "timepoint", levels = list("timepoint_1", "timepoint_2", "timepoint_3"))),
  rmCells = list(
    list(measure = "value", cell = "timepoint_1"), list(measure = "value", cell = "timepoint_2"),
    list(measure = "value", cell = "timepoint_3")),
  bs = NULL,
  cov = NULL,
  effectSize = "eta",
  depLabel = "Dependent",
  rmTerms = list("timepoint"),
  bsTerms = NULL,
  ss = "3",
  spherTests = FALSE,
  spherCorr = list("none"),
  leveneTest = FALSE,
  contrasts = NULL,
  postHoc = NULL,
  postHocCorr = list("tukey"),
  emMeans = list(list()),
  emmPlots = TRUE,
  emmTables = FALSE,
  emmWeights = TRUE,
  ciWidthEmm = 95,
  emmPlotData = FALSE,
  emmPlotError = "ci",
  groupSumm = FALSE)

# ANCOVA
ancova(
  data = diss,
  dep = s_ha,
  factors = c(top),
  covs = c(elev),
  effectSize = "eta",
  modelTest = TRUE,
  modelTerms = NULL,
  ss = "3",
  homo = TRUE,
  norm = TRUE,
  qq = FALSE,
  contrasts = NULL,
  postHoc = ~ top,
  postHocCorr = list("holm"),
  postHocES = list(),
  emMeans = list(list()),
  emmPlots = TRUE,
  emmPlotData = FALSE,
  emmPlotError = "ci",
  emmTables = FALSE,
  emmWeights = TRUE,
  ciWidthEmm = 95)

# MANCOVA
mancova(
  data = diss,
  deps = c(s_ha, s_m2),
  factors = c(exp, top),
  covs = c(elev, slope),
  multivar = list("pillai", "wilks", "hotel", "roy"),
  boxM = TRUE,
  shapiro = TRUE,
  qqPlot = FALSE
)

# ONE-WAY ANOVA (NON-PARAMETRIC)
anovaNP(
  data = diss,
  deps = s_m2,
  group = veg_type,
  es = TRUE,
  pairs = TRUE,
  formula)

# REPEATED MEASURES ANOVA (NON-PARAMETRIC)
anovaRMNP(
  data = db,
  measures = c(before, after),
  pairs = TRUE,
  desc = TRUE,
  plots = TRUE,
  plotType = "medians")

# CORRELATION MATRIX
corrMatrix(
  data = diss,
  vars = vars(tree_cover, e, n2),
  pearson = FALSE,
  spearman = TRUE,
  kendall = TRUE,
  sig = TRUE,
  flag = TRUE,
  n = TRUE,
  ci = FALSE,
  ciWidth = 95,
  plots = TRUE,
  plotDens = TRUE,
  plotStats = TRUE,
  hypothesis = "corr"
)

# LINEAR REGRESSION
diss %>% 
	linReg(
  dep = s_m2,
  covs = vars(elev, tree_cover, slope, herb_cover),
  factors = NULL,
  blocks = list(list("elev", "tree_cover", "slope", "herb_cover")),
  refLevels = NULL,
  intercept = "refLevel",
  r = TRUE,
  r2 = TRUE,
  r2Adj = TRUE,
  aic = TRUE,
  bic = FALSE,
  rmse = TRUE,
  modelTest = TRUE,
  anova = TRUE,
  ci = FALSE,
  ciWidth = 95,
  stdEst = FALSE,
  ciStdEst = FALSE,
  ciWidthStdEst = 95,
  norm = TRUE,
  qqPlot = FALSE,
  resPlots = FALSE,
  durbin = FALSE,
  collin = T,
  cooks = FALSE,
  emMeans = list(list()),
  ciEmm = TRUE,
  ciWidthEmm = 95,
  emmPlots = TRUE,
  emmTables = FALSE,
  emmWeights = TRUE
)

# BINOMIAL LOGISTIC REGRESSION
logRegBin(
  data = stress,
  dep = stress,
  covs = vars(stability, flexibility, tasks, lack_train, lack_car_dev),
  factors = NULL,
  blocks = list(list("stability", "flexibility", "tasks", "lack_train", "lack_car_dev")),
  refLevels = list(list(var = "stress", ref = "Unstressed")),
  modelTest = TRUE,
  dev = TRUE,
  aic = TRUE,
  bic = FALSE,
  pseudoR2 = list("r2mf"),
  omni = FALSE,
  ci = FALSE,
  ciWidth = 95,
  OR = FALSE,
  ciOR = FALSE,
  ciWidthOR = 95,
  emMeans = list(list()),
  ciEmm = FALSE,
  ciWidthEmm = 95,
  emmPlots = FALSE,
  emmTables = FALSE,
  emmWeights = FALSE,
  class = TRUE,
  acc = TRUE,
  spec = TRUE,
  sens = TRUE,
  auc = TRUE,
  rocPlot = TRUE,
  cutOff = 0.5,
  cutOffPlot = FALSE,
  collin = TRUE,
  boxTidwell = FALSE,
  cooks = FALSE
)

# MULTINOMIAL LOGISTIC REGRESSION
mlr <- data.frame(
  com = factor(diss$com),
  elev = diss$elev,
  top = factor(diss$top)
)

logRegMulti(
  data = mlr,
  dep = com,
  covs = elev,
  factors = top,
  blocks = list(list("elev", "top")),
  refLevels = list(list(var = "com", ref = "A"), list(var = "top", ref = "flat")),
  modelTest = TRUE,
  dev = TRUE,
  aic = TRUE,
  bic = FALSE,
  pseudoR2 = list("r2mf"),
  omni = TRUE,
  ci = FALSE,
  ciWidth = 95,
  OR = FALSE,
  ciOR = FALSE,
  ciWidthOR = 95,
  emMeans = list(list()),
  ciEmm = TRUE,
  ciWidthEmm = 95,
  emmPlots = TRUE,
  emmTables = FALSE,
  emmWeights = TRUE
)

# ORDINAL LOGISTIC REGRESSION
set.seed(1337)
y <- factor(sample(1:3, 100, replace = TRUE))
x1 <- rnorm(100)
x2 <- rnorm(100)
df <- data.frame(y = y, x1 = x1, x2 = x2)

otto %>% 
	select(-participant_id) %>% 
	logRegOrd(
		dep = score,
		covs = NULL,
		factors = c(order_of_muscle_movements, participant_gender, avatar_gender,
								direction_of_head_movement, emotion_label),
		blocks = list(list("order_of_muscle_movements", "participant_gender", "avatar_gender",
											 "direction_of_head_movement", "emotion_label")),
		refLevels = list(
			list(var = "score", ref = "1"), 
			list(var = "order_of_muscle_movements", ref = "1"),
			list(var = "participant_gender", ref = "Fe"),
			list(var = "avatar_gender", ref = "F"),
			list(var = "direction_of_head_movement", ref = "L"),
			list(var = "emotion_label", ref = "No Emotion")),
		modelTest = TRUE,
		dev = TRUE,
		aic = TRUE,
		bic = FALSE,
		pseudoR2 = list("r2mf"),
		omni = TRUE,
		thres = FALSE,
		ci = FALSE,
		ciWidth = 95,
		OR = FALSE,
		ciOR = FALSE,
		ciWidthOR = 95
	)

# PROPORTION TEST (2 OUTCOMES)
propTest2(
  data = diss,
  vars = veg_type,
  areCounts = FALSE,
  testValue = 0.5,
  hypothesis = "notequal",
  ci = FALSE,
  ciWidth = 95,
  bf = FALSE,
  priorA = 1,
  priorB = 1,
  ciBayes = FALSE,
  ciBayesWidth = 95,
  postPlots = FALSE
)

# PROPORTION TEST (N OUTCOMES)
propTestN(
  data = diss,
  var = exp,
  counts = NULL,
  expected = FALSE,
  ratio = NULL,
  formula
)

# CONTINGENCY TABLES
contTables(
  data = diss,
  rows = exp,
  cols = com,
  counts = NULL,
  layers = NULL,
  chiSq = TRUE,
  chiSqCorr = FALSE,
  likeRat = TRUE,
  fisher = FALSE,
  contCoef = FALSE,
  phiCra = FALSE,
  logOdds = FALSE,
  odds = FALSE,
  relRisk = FALSE,
  ci = TRUE,
  ciWidth = 95,
  gamma = FALSE,
  taub = FALSE,
  obs = TRUE,
  exp = FALSE,
  pcRow = FALSE,
  pcCol = FALSE,
  pcTot = FALSE,
  formula
)

# PAIRED SAMPLES CONTINGENCY TABLES
dat <- data.frame(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  `Counts` = c(794, 150, 86, 570),
  check.names = FALSE
)

contTablesPaired(
  data = dat,
  rows = "1st survey",
  cols = "2nd survey",
  counts = `Counts`,
  chiSq = TRUE,
  chiSqCorr = TRUE,
  exact = FALSE,
  pcRow = FALSE,
  pcCol = FALSE,
  formula
)

# LOG-LINEAR REGRESSION
data("mtcars")
tab <- table("gear" = mtcars$gear, "cyl" = mtcars$cyl)
dat <- as.data.frame(tab)

logLinear(
  data = dat,
  factors = vars(gear, cyl),
  counts = Freq,
  blocks = list(list("gear", "cyl", c("gear", "cyl"))),
  refLevels = list(list(var = "gear", ref = "3"), list(var = "cyl", ref = "4")),
  modelTest = TRUE,
  dev = TRUE,
  aic = TRUE,
  bic = FALSE,
  pseudoR2 = list("r2mf"),
  omni = FALSE,
  ci = FALSE,
  ciWidth = 95,
  RR = FALSE,
  ciRR = FALSE,
  ciWidthRR = 95,
  emMeans = list(list()),
  ciEmm = TRUE,
  ciWidthEmm = 95,
  emmPlots = TRUE,
  emmTables = FALSE,
  emmWeights = TRUE
)

# RELIABILITY ANALYSIS
reliability(
  data = emma_num,
  vars = vars(q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16),
  alphaScale = TRUE,
  omegaScale = TRUE,
  meanScale = TRUE,
  sdScale = TRUE,
  corPlot = TRUE,
  alphaItems = TRUE,
  omegaItems = TRUE,
  meanItems = TRUE,
  sdItems = TRUE,
  itemRestCor = TRUE,
  revItems = NULL
)

# PRINCIPAL COMPONENT ANALYSIS
pca(
  data = diss,
  vars = vars(elev, slope, tree_cover, shrub_cover, herb_cover),
  nFactorMethod = "parallel",
  nFactors = 1,
  minEigen = 1,
  rotation = "varimax",
  hideLoadings = 0.3,
  sortLoadings = TRUE,
  screePlot = TRUE,
  eigen = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE,
  bartlett = TRUE
)

# EXPLORATORY FACTOR ANALYSIS
load("diss.RData")
john <- read_csv("john/john_new.csv")
matrix %>% select(!species) -> df

efa(
  data = john,
  vars = vars(
    d1, d2, d3,
    w1, w2, w3, w4, w5, w6, w7, w8,
    r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
    u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12,
    b1, b2, b3, b4, b5, b6, b7, b8, b9, b10
  ),
  nFactorMethod = "parallel",
  nFactors = 1,
  minEigen = 1,
  extraction = "pa",
  rotation = "oblimin",
  hideLoadings = 0.3,
  sortLoadings = TRUE,
  screePlot = TRUE,
  eigen = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  modelFit = TRUE,
  kmo = TRUE,
  bartlett = TRUE
)

# CONFIRMATORY FACTOR ANALYSIS
cfa(
  data = john,
  factors = list(
    list(label = "Demographics", vars = c("d1", "d2", "d3")),
    list(label = "Wellbeing", vars = c("w2", "w3", "w4", "w5", "w6", "w7", "w8")),
    list(label = "Resilience", vars = c("r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10")),
    list(label = "USC", vars = c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12")),
    list(label = "Bonding", vars = c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10"))
  ),
  resCov = NULL,
  miss = "listwise",
  constrain = "facVar",
  estTest = TRUE,
  ci = TRUE,
  ciWidth = 95,
  stdEst = TRUE,
  factCovEst = TRUE,
  factInterceptEst = TRUE,
  resCovEst = TRUE,
  resInterceptEst = TRUE,
  fitMeasures = list("cfi", "tli", "rmsea", "aic"),
  modelTest = TRUE,
  pathDiagram = TRUE,
  corRes = FALSE,
  hlCorRes = 0.1,
  mi = FALSE,
  hlMI = 3
)
