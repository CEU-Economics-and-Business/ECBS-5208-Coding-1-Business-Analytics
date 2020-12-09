These codes are for analysis pattern of association between life expectancy and measures of wealth (GDP total or GDP/capita).

life_exp_getdata.R - downloads the raw data from World Development Indicators maintained by World Bank and saves it to data/raw folder.

life_exp_clean.R - loads the raw data and clean them: create a tidy table where each observation is a country.

life_exp_analysis.R - loads the clean data and executes simple linear regressions with visual inspections and quantitative analysis. It chooses model and then analyse the residuals.}
