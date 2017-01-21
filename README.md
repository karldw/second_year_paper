# Cars in Alaska


## To do:
### Analysis to-do
- [x] Adjust things per-capita
- [x] Plot the daily FE within each event (to look for anticipation)
    - Nothing much there -- just pick something based on the time-to-sell literature.
- Note from the plot in overall_sale_count_ak_vs.pdf that AK sales in later years are way down.
- [ ] Estimate the whole event study (with year FE)
- [ ] See if I can correlate the year FE to the dividend amount (not causal)
- [ ] Merge in state gdp as a control
- [ ] Check how unbalanced the panel is when I do buyer-by-day
    - Consider filling out the panel by adding zeros for buyers who don't trade
- [ ] Auctions happen much more on some days than others. Consider splitting the sample for days with nothing going on.
- [ ] Make a more formal model of dealer anticipation
- [ ] Run a zero-inflated Poisson model for sale counts?
- [ ] Run synthetic controls
- [ ] Run generalized synthetic controls
- [ ] Write an explanation about why the announcements are not likely to be informative
- [ ] Get announcement dates to test anyway
- [ ] Improve data cleaning table
    - Automate numbers (avoid copy-paste)
    - Add counts of rows dropped for unintelligible date
- [ ] Run a Hsieh-style quarter-over-quarter regression for new vehicle registrations
    - Use county pop to estimate county APF dividend income
- [ ] Get answers from Harrison

### Data cleaning to-do
- [ ] For observations that have no valid state, bring in the state from other observations of the same buyer/seller/auction (as long as there's a unique other state).

## To read:
- [ ] Original synthetic controls paper
- [ ] Papers on anticipation of welfare checks
- [ ] More background on consumption smoothing
- [x] [Doudchenko and Imbens](https://www.nber.org/papers/w22791)
- [ ] That article about natural experiments in macro.

Hsieh (2003) looked at the Alaska Permanent Wealth Fund to see how consumers smooth their income and found that their consumption _was_ smooth for Permanent Fund payments, but not tax refunds.

A (much less ambitious) follow-up is to examine how used car auction prices shift when these payments occur.
Someone else (who?) found price cyclicality from income tax refunds in other parts of the US.
I'm sure Alaska is a bit different than other states, so maybe I could present both a difference-in-differences analysis and a interrupted time series.

Fun fact: you can get the [name of everyone who applied](https://pfd.alaska.gov/Division-Info/Applicant-Database) to receive a Permanent Fund check in a given year.


### Possibly related papers
#### About the Alaska Permanent Fund:
- https://link.springer.com/chapter/10.1057/9781137015020_4#page-1
- http://www.iser.uaa.alaska.edu/Publications/Economic_Impacts_AK_PFD.pdf
- https://www.jstor.org/stable/2696554?seq=1#page_scan_tab_contents
- http://www.sciencedirect.com/science/article/pii/S1094202510000359
- https://ideas.repec.org/p/alb/series/1085.html
- https://link.springer.com/chapter/10.1057/9781137015020_3#page-1
#### About consumption smoothing and the like
- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.693.16&rep=rep1&type=pdf
- http://www.mitpressjournals.org/doi/pdf/10.1162/REST_a_00184
- http://www.sciencedirect.com/science/article/pii/S0047272707000631

### Notes on the Alaska data
There's something weird with the garnishments; the numbers are the same for 2012 and 2013.

Payment is the first Thursday of October.
