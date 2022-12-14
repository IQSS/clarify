#Make matched data
m <- MatchIt::matchit(treat ~ age + educ + race + married + re74,
                      data = MatchIt::lalonde, method = "full", estimand = "ATE",
                      caliper = .05)
md <- MatchIt::match.data(m, data = MatchIt::lalonde)
md$binY <- as.numeric(md$re78 > 0)

set.seed(1993)
md$countY <- rpois(nrow(md), 5)
md$propY <- runif(nrow(md))

saveRDS(md, test_path("fixtures", "mdata.rds"))
