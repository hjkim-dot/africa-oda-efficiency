# ============================================================
# helpers.R : 여러 스크립트가 함께 쓰는 공통 함수 (직접 실행할 필요 없음)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(tibble); library(censReg); library(sandwich)

MAIN <- c("wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc")
LAB  <- c(wgi_lag1 = "거버넌스", nat_resource = "천연자원",
          mobile = "모바일", ln_gdp_pc = "1인당GDP")

stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
         ifelse(p < 0.05, "*", ""))))
}

# 모형 A / B / C 의 표본과 설명변수
#  A : 거버넌스 포함, 에티오피아(ETH) 제외 (17개국)
#  B : 거버넌스 제외, 18개국 전부
#  C : 거버넌스 포함, 18개국 전부
get_sample <- function(df, model) {
  switch(model,
    A = list(data = df |> filter(iso3c != "ETH"), x = MAIN),
    B = list(data = df, x = MAIN[-1]),
    C = list(data = df, x = MAIN))
}

# Tobit 추정 (0~1 양쪽 절단) + 국가별 묶음(cluster) 표준오차
fit_tobit <- function(d, xvars, year_dummy = TRUE, cluster = TRUE) {
  d <- d |> drop_na(dea_score, all_of(xvars)) |> as.data.frame()
  rhs <- paste(c(xvars, if (year_dummy) "factor(year)"), collapse = " + ")
  f <- as.formula(paste("dea_score ~", rhs))
  m <- censReg(f, left = 0, right = 1, data = d)
  est <- summary(m)$estimate
  b <- est[, 1]; se <- est[, 2]; p <- est[, 4]
  pc <- rep(NA_real_, length(b)); sec <- rep(NA_real_, length(b))
  if (cluster) {
    vc <- tryCatch(vcovCL(m, cluster = d$iso3c), error = function(e) NULL)
    if (!is.null(vc)) { sec <- sqrt(diag(vc)); pc <- 2 * pnorm(-abs(b / sec)) }
  }
  keep <- names(b) %in% xvars
  tab <- tibble(term = names(b)[keep], coef = unname(b[keep]), se = unname(se[keep]),
                se_cluster = unname(sec[keep]), p = unname(p[keep]), p_cluster = unname(pc[keep]))
  list(table = tab, N = nrow(d), G = n_distinct(d$iso3c),
       cens = sum(d$dea_score >= 1 - 1e-9), ll = as.numeric(logLik(m)), model = m)
}
