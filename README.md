# GlobalBanksData
R code to fetch data from Bloomberg about global banks. The code requires an open Bloomberg session to run.

This code includes the following analyses, where some apply to [G-SIBs](https://www.bis.org/bcbs/gsib/) and others to a sample of 100+ large banks (some to both):

- Total stock of [loan loss reserves](https://www.bis.org/fsi/fsibriefs3.htm) by bank, as a percentage of their loans
- Average (at the jurisdiction level) build-up of loan loss reserves in recent quarters
- A [Kruskal-Wallis test](https://en.wikipedia.org/wiki/Kruskal–Wallis_one-way_analysis_of_variance) of whether banks' provisioning practices varied significantly by jurisdiction or not
- Dispersion in banks' provisioning practices, as viewed on overlaid histograms.
- Comparison of banks' quarterly build-up of loan loss reserves with their profitability as measured by operating income; this comparison is done at by each of the major accounting framework (ie, grouped by IFRS, US GAAP and other national GAAPs), since accounting rules fundamentally define how loan loss provisions occur.
- Comparison of historical and expected (by market analysts) total quarterly loan loss provisions for a balanced sample of banks.
- Comparison of US consumers' expenditures, by income bracket of ZIP, with total US consumer loans via credit cards. Note: the sources for this particular analysis are, respectively, [Prof Raj Chetty's www.tracktherecovery.org](www.tracktherecovery.org) and [Federal Reserve Board H.8 form](https://www.federalreserve.gov/releases/h8/current/default.htm) as accessed from the start [St Louis Fed's FRED system](https://fred.stlouisfed.org)
- Regressions associating a banks' loan growth with the deposit growth in each quarter.
- Loan growth by bank for the recent quarters
- Capital ratio by bank for the recent quarters, measured by the [Common Equity Tier 1 capital ratio](https://www.bis.org/basel_framework/).
