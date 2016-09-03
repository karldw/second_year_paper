# Karl's second year paper

## Alaska car prices
Hsieh (2003) looked at the Alaska Permanent Wealth Fund to see how consumers smooth their income and found that their consumption _was_ smooth for Permanent Fund payments, but not tax refunds.

A (much less ambitious) follow-up is to examine how used car auction prices shift when these payments occur.
Someone else (who?) found price cyclicality from income tax refunds in other parts of the US.
I'm sure Alaska is a bit different than other states, so maybe I could present both a difference-in-differences analysis and a interrupted time series.




## Re-analyze the Muller-Mendelsohn (APEEP)  model
Aggregation matters, and a lot of the analysis in Muller and Mendelsohn's (MM) paper is done at the county-year level.
County-year is a huge improvement from what had been done before, but because health effects of pollution are non-linear, averages may be misleading.
Berkeley Earth has hourly grid cell data on concentrations -- it should be possible to use those, whatever emissions data MM used and
a little atmospheric chemistry to back out emissions at a higher time frequency, then use the same marginal damages as MM (Klemm and Mason 2003).

Side note: I think the data in Klemm and Mason are about long term exposures, contrary to MM's claim that they represent short-term damages.
That's still an important number, but it seems like it tells us more about a marginal change in concentrations, not necessarily a change in emissions.
Would should be possible to use a strategy like Wolfram and Reed's airport paper to find the effects of a marginal increase in PM?
(Side side note: Wolfram and Reed don't examine PM in their paper.  Is it possible that the effects they find are due to other pollutants, not ozone?)
Of course, that will only give the short-term effect of a marginal increase in emissions, which isn't ideal.
