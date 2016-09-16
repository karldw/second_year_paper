# Karl's second year paper

## 1. Smog pricing
Are cars that are subject to smog checks less valuable?

Identification would be off some combination of geography (since not all counties are subject to smog checks) and time (since the rules and exemptions are discontinuous and have changed over time).

Related: the [vehicle buy-back program](https://www.arb.ca.gov/msprog/avrp/avrp.htm).

## 2. Re-analyze the Muller-Mendelsohn (APEEP)  model
Aggregation matters, and a lot of the analysis in Muller and Mendelsohn's (MM) paper is done at the county-year level.
County-year is a huge improvement from what had been done before, but because health effects of pollution are convex, these averages may still understate the true health effects.
Berkeley Earth has hourly grid cell data on concentrations -- it should be possible to use those, whatever emissions data MM used and
a little atmospheric chemistry to back out emissions at a higher time frequency, then use the same marginal damages as MM (Klemm and Mason 2003).

Side note: I think the data in Klemm and Mason are about long term exposures, contrary to MM's claim that they represent short-term damages.
That's still an important number, but it seems like it tells us more about a marginal change in concentrations, not necessarily a change in emissions.
Would should be possible to use a strategy like Wolfram and Reed's airport paper to find the effects of a marginal increase in PM?
(Side side note: Wolfram and Reed don't examine PM in their paper.  Is it possible that the effects they find are due to other pollutants, not ozone?)
Of course, that will only give the short-term effect of a marginal increase in emissions, which isn't ideal.


## 3. Alaska car prices  -- dropped
**Update:** There are only sales in 32  states, and Alaska isn't one of them.


Hsieh (2003) looked at the Alaska Permanent Wealth Fund to see how consumers smooth their income and found that their consumption _was_ smooth for Permanent Fund payments, but not tax refunds.

A (much less ambitious) follow-up is to examine how used car auction prices shift when these payments occur.
Someone else (who?) found price cyclicality from income tax refunds in other parts of the US.
I'm sure Alaska is a bit different than other states, so maybe I could present both a difference-in-differences analysis and a interrupted time series.

Fun fact: you can get the [name of everyone who applied](https://pfd.alaska.gov/Division-Info/Applicant-Database) to receive a Permanent Fund check in a given year.


### Possibly related Alaska papers
- https://link.springer.com/chapter/10.1057/9781137015020_4#page-1
- http://www.iser.uaa.alaska.edu/Publications/Economic_Impacts_AK_PFD.pdf
- https://www.jstor.org/stable/2696554?seq=1#page_scan_tab_contents
- http://www.sciencedirect.com/science/article/pii/S1094202510000359
- https://ideas.repec.org/p/alb/series/1085.html
- https://link.springer.com/chapter/10.1057/9781137015020_3#page-1

### Notes on the Alaska data
There's something weird with the garnishments; the numbers are the same for 2012 and 2013.

Payment is the first Thursday of October.
