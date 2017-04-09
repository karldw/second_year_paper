# Cars in Alaska



## Implementing Doudchenko and Imbens (2016)
I'm going to apply what they call "Constrained Regression", which is like the original Abadie-Diamond-Hainmueller synthetic controls, but allowing a non-zero intercept.  Think of allowing this non-zero intercept as avoiding picking states just to match the average size of the Alaska market and instead focusing on trends.
Alternatively, this is like standard difference-in-differences, but allowing variable weights on the controls.
I'm going to, in my preferred specification, maintain the "adding up" and "non-negativity" constraints, which say that the weight on the controls must be non-negative and sum to one.
The weights are the solution to Doudchenko and Imbens' equation 5.6 (but allowing mu != 0).
In alternative specifications, I can consider dropping these two constraints. (They require more data to estimate though.)

To include covariates, they recommend "Prior to
choosing the weights and possibly the intercept, we can regress the control outcomes on the pre-treatment variables and calculate the residuals. Then we use the residuals in the approaches
discussed in the previous sections."


## Second draft to-dos:
### Michael and Thibault:
- [ ] Discuss the PIH theory.  "Under what conditions would we expect it to hold, and under what conditions would we expect it to fail?"
- [ ] Ditch or de-focus the event window figures and plot the outcome means in event time.
- [ ] Analyze subgroups of cars that seem likely to see counter-PIH effects, such as cheaper/older vehicles. (make sure you don't define "cheaper" autos endogenously.)
- [ ] Is there an error in logs vs levels? Difference seems too large.
- [ ] Incorporate the variation in rebate amount to modify the treatment "intensity" across different years

### Claire
- [ ] Tighten the intro, make it very clear what the question is.
- [ ] See if I can theoretically sign the effects on price or quality.
    - [ ] Look in the literature to see if anyone has come up with income elasticities for used cars.
- [ ] State hypotheses more clearly, particularly what we would see if dealers anticipate vs restock.
- [ ] Reorder things: together the model, then discussion of the time windows, then the estimating equations and a clear set of predictions under alternative hypotheses.
- [ ] What % of used cars does this represent? How does this compare to new registrations?
- [ ] Mention Alaskan dealers selling cars out of state.
- [ ] Make an appendix for some of the results I mention.
- [ ] Test different control states.
- [ ] Test placebo dates in spring/summer.
- [ ] Discuss the fact that sales in AK may be differentially affected by seasonal effects -- Xu (2016) factor methods would be one solution, but you worry about overfitting.
- Presenting results
    - [ ] Present some results more traditionally -- maybe just a standard DD table as a complement.
    - [ ] I can't follow the suggestion to use shape (instead of color) for sign, because that makes interpreting the size even harder. *But* I can use colors that come out better when rendered in black and white.
    - [ ] Explain the choice of 95% conf bound in more detail.
    - [ ] Consider making schematic results of what we would see under different hypotheses.
    - [ ] What's up at the bottom of the graph?

### Andy
- [ ] Fix the list of control states -- is it Montana or Oregon? (It's Montana.)
- [ ] Do better proofing of typos etc
- [ ] Make a table of descriptive statistics to show comparability with the control states
    - [ ] State demographics, including income distribution or % urban
    - [ ] Vehicle market characteristics
    - [ ] Do a simple plot of trends over time (same as Michael's suggestion)
- [ ] Do a specification with state-specific trends
- [ ] Discuss the dealership turnaround (time to sale) in more detail, including what would happen if my assumption is wrong.
    - The answer here is something along the lines of "this is why I'm considering different windows."
- [ ] Be more concrete about the dividend anticipation
- [ ] Look at PIH empirical literature.
- [ ] Section 1: Add statistics, clarify event timing.
- [ ] Section 2: Refine theory
    - [ ] Be more explicit about wealth changes over time, particularly how the shock comes in.
    - [ ] Elaborate on the differentiated products model (or drop it and explain that the section didn't add much)
    - [ ] Write FOCs.
- [ ] Section 3:
    - [ ] Move table 1 to appendix
    - [ ] Add dividend amount interacted with Alaska dummy (demean first for easier interpretation)
- [ ] Section 4:
    - [ ] Pick logs or levels as preferred spec
    - [ ] Make figure 2 (comparison between states) into a table instead, unnormalized.
    - [ ] Commit to a particular anticipation/post window and stick to it. Make section 4.2 into a robustness discussion instead of a lead-up to the many-window figures.
    - [ ] Change fixed effects notation
    - [ ] Double check which alphas I'm talking about. Point out that I interpret in the text.
    - [ ] Keep figure 3 (treatment coefficients for varying pre-treatment windows), but make explanation clearer
    - [ ] Explain why we're not particularly interested in coefficient 4, AK * post, because we're more or less capturing all interesting cases with the different windows.
    - [ ] There's nothing special about where I started (8 weeks before, in mid-August), just that it seemed like a reasonable tradeoff to having a not-too-wide window.
- [ ] Fix typos, defend relatively informal language.




## First draft must-dos:
- Rebuild all plots (run make_all_plots.sh)
- Two copies
- Signature
- Post to website
- Replace acronyms with sc version: MSRP, APF, PIH, VIN, CEX
- Search pdf for unresolved references: "??"
- Search for TODOs

## To do:
### Analysis to-do
- [x] Adjust things per-capita
- [x] Plot the daily FE within each event (to look for anticipation)
    - Nothing much there -- just pick something based on the time-to-sell literature.
- Note from the plot in overall_sale_count_ak_vs.pdf that AK sales in later years are way down.
- [x] Estimate the whole event study (with year FE)
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
- [x] Improve data cleaning table
    - Automate numbers (avoid copy-paste)
    - Add counts of rows dropped for unintelligible date
- [ ] Run a Hsieh-style quarter-over-quarter regression for new vehicle registrations
    - Use county pop to estimate county APF dividend income
- [ ] Get answers from Harrison
- [ ] Read EI feedback again.

### Data cleaning to-do
- [ ] For observations that have no valid state, bring in the state from other observations of the same buyer/seller/auction (as long as there's a unique other state).
- [ ] Use Derek's improved zip code mapping.

### Logistics to-do
- [ ] Finish SConstruct file
- [x] Update pdf link away from github (link will always be out of date)
- [ ] Make separate plots directories by outcome and for daily/weekly.

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
