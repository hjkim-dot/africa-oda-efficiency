# 아프리카 주요국의 ODA 효율성 분석 및 결정요인에 관한 연구 — 재현 자료

김현지 (한국외국어대학교 국제지역대학원 국제지역전략학과) 석사학위논문(2026)의 분석 코드와 처리 자료입니다.
*Replication files for "An Analysis of the Efficiency of Official Development Assistance and Its Determinants in Major African Countries" (M.A. thesis, HUFS, 2026).*

## 1. 구성

```
R/                  분석 코드 (R)
data/data.xlsx      원자료를 정리한 패널 자료 (시트: data, 18개국 × 2005~2022년)
data/processed/
  analysis_panel.csv     분석용 패널 자료 (2006~2022년, 306개 관측치)
  dea_scores.csv         국가·연도별 DEA 효율성 점수 (BCC, CCR, 규모효율성 SE=CCR/BCC)
  dea_scores_no_gdp.csv  GDP 성장률을 산출변수에서 제외한 BCC 점수 (논문 부록 7)
output/             스크립트 실행 결과 (CSV)
sessionInfo.txt     실행 환경 (R 4.6.1, Windows x86_64)
```

## 2. 실행 순서

1. `R/` 폴더의 모든 스크립트와 `data/data.xlsx`를 한 폴더에 두고, 그 폴더를 R의 작업 폴더로 지정합니다.
2. R을 새로 시작한 뒤 `run_all.R`을 실행합니다. `00_data_prep.R`이 `analysis_df.rds`를, `01_dea.R`이 `dea_results_full.csv`를 만들고, 나머지 스크립트가 결과를 `output/` 폴더에 저장합니다.
3. 이어서 `15_dea_no_gdp.R`, `16_extra_analysis.R`, `17_ccr_shift_check.R`을 차례로 실행합니다.

의존 관계: `data.xlsx` → `00_data_prep.R` → `01_dea.R` → `02`~`14` → `15`·`16`·`17`

## 3. 논문의 표와 스크립트

| 논문 | 스크립트 |
|---|---|
| 표 5, 부록 표 1~4 (DEA 효율성 점수) | `01_dea.R` |
| 표 4, 표 8 (기술통계, 상관관계) | `02_descriptive.R` |
| 표 6 (VIF), 표 7 (Tobit 주 분석) | `14_panel_tobit_level.R` (`06_vif.R` 참고) |
| 표 9 (강건성: Beta 회귀, 분수 로짓) | `08_beta_fractional.R`, `run_all.R` 끝부분(GDP 제외 Tobit) |
| 표 10, 부록 표 9 (한 나라씩 제외) | `07_leave_one_out.R` |
| 표 11·12, 부록 6 (거버넌스 세부지표, 2020년 제외, 자원 호황기, 모형 검정) | `16_extra_analysis.R` |
| 부록 4 (소지역별 Tobit) | `13_subregion_tobit.R` |
| 부록 7 (GDP 성장률 제외 DEA 재추정) | `15_dea_no_gdp.R` |
| Ⅲ장 CCR 변환 민감도 | `17_ccr_shift_check.R` |

`03_adf.R`, `04_panel_unitroot_hadri.R`, `09_tobit_diff.R`~`12_country_regressions.R`, `99_run_and_collect.R`은 분석 과정에서 검토한 탐색용 스크립트로, 논문의 최종 결과에는 사용하지 않았습니다(`run_all.R`이 함께 실행하므로 그대로 두었습니다).

## 4. 핵심 설정

- **DEA**: `Benchmarking::dea()`, 투입지향(`ORIENTATION = "in"`), BCC(`RTS = "vrs"`)·CCR(`RTS = "crs"`). 해마다 그 해의 18개국으로 프런티어를 따로 계산.
  - 투입: 1인당 순 ODA 수혜액. 산출: 1인당 GDP 성장률, 영아 생존율(1,000 − 영아사망률), 중등교육 총등록률.
  - 성장률이 0 이하인 해에는 그 해의 최솟값이 1이 되도록 모든 국가의 성장률을 평행이동. BCC 점수는 이 변환에 영향을 받지 않음(Ali & Seiford, 1990). CCR은 변환 상수에 따라 다소 달라지므로 참고 지표로 사용(`17_ccr_shift_check.R`).
- **Tobit**: `censReg::censReg(left = 0, right = 1)`. 주 모형은 풀링 Tobit + 연도 더미.
- **표준오차**: 국가별 묶음 표준오차(`sandwich::vcovCL`, G/(G−1) 보정), Tobit은 z검정.
- **강건성**: `betareg::betareg()`(1.0 처리: 0.999 대체, (y(n−1)+0.5)/n 보정), 분수 로짓 `glm(family = quasibinomial("logit"))`, 국가·연도 고정효과 OLS.
- **모형 검정**: 우도비 검정(국가 더미), 하우스만 검정(`plm`, within vs random).

## 5. 자료 출처

| 변수 | 원자료 | 코드 |
|---|---|---|
| 1인당 순 ODA 수혜액 | World Bank, World Development Indicators (WDI) | DT.ODA.ODAT.PC.ZS |
| 1인당 GDP(불변가격) → 성장률, 로그 | WDI | NY.GDP.PCAP.KD |
| 영아사망률 → 영아 생존율 | WDI | SP.DYN.IMRT.IN |
| 중등교육 총등록률 | WDI | SE.SEC.ENRR |
| 천연자원 지대/GDP | WDI | NY.GDP.TOTL.RT.ZS |
| 이동전화 가입자 수(100명당) | WDI | IT.CEL.SETS.P2 |
| 부패 통제, 정부 효과성(0~100) | World Bank, Worldwide Governance Indicators (WGI) | CC, GE |

WDI와 WGI는 세계은행이 CC BY 4.0 라이선스로 제공하는 자료입니다.

**결측치 처리**: 현재 공개된 WDI 기준으로 중등교육 등록률은 306개 국가·연도 가운데 111개가 비어 있어, 같은 나라의 앞뒤 연도 값을 이용한 선형보간 등으로 채웠습니다. 천연자원 지대/GDP의 2022년 값은 2021년 값으로 대신하였고, 이동전화 가입자 수의 결측 1개(에티오피아 2018년)는 보간하였습니다(논문 Ⅲ장 1절 참조).

## 6. 인용

김현지 (2026). 아프리카 주요국의 ODA 효율성 분석 및 결정요인에 관한 연구. 석사학위논문, 한국외국어대학교 국제지역대학원.

## 7. 라이선스

- 코드(`R/`): MIT License (`LICENSE`)
- 자료(`data/`, `output/`): CC BY 4.0 (`data/LICENSE`). 출처를 밝히면 자유롭게 공유·변형할 수 있습니다.
- 원자료인 세계은행 WDI·WGI도 CC BY 4.0으로 제공됩니다.
