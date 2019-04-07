# Imputation methods and plans (or, research to look into and keep whittling down)

Single imputation: filling in one value for each missing item
- Lets you use standard statistical things that require complete data
- Can incorporate knowledge from the data collector
- Treats the values as if they were known, which isn't great: inference will be too sharp, because we're potentially missing variability from the missing values
- In general, underestimates variance

Multiple imputation: estimating multiple values for each missing itme, then combining these estimates somehow to include better info about variability.
- Doesn't underestimate variance, under certain conditions.
- More computationally expensive
- Same problems with "confirmation bias" - predicting things we can't see based on what we already know

## Potential methods

(by name/idea, not sure what this translates to in terms of packages)
- Mean imputation: doesn't really work here, I guess the analog would be modal imputation? This is a form of single imputation and I should mention it but probably not use it seriously. Maybe include it as a comparison.
- Hot deck: pulling similar rows from the data
- Distribution-based: like hot deck but with multinomial conditional distributions (this is maybe called Chained Equations)
- Regression-based: given the demographics for each precinct and previous votes, predict the missing values (get variation/multiples on this with bootstrapping?)

## Potential R packages
<!-- Pick one / compare? -->
- MICE
- missForest
- Hmisc
- mi
- hot.deck

https://cran.r-project.org/web/views/MissingData.html
https://thomasleeper.com/Rcourse/Tutorials/mi.html

Each of these uses different assumptions and algorithms to do its deal, so an explanation of some different methods and an in-depth look into whichever subset I choose will be a good section to put in the methods.

## Routes to go down / compare

- An imputation taking absolutely nothing into account: random draw from the other available candidates, fully committing to the idea that every non-response is a complete non-preference (the Kroh method).
- An imputation taking nothing into account other than who they voted for. This shouldn't change much, because it will reflect the general proportions in place already.
- Matching within precincts if possible, and outside a precinct if impossible (maybe weight secondary precincts to choose by distance? Or just random). This will overweight the precincts with low voting rates, which should look closer to what really would happen?
- Weighting precincts by geographic distance (for this and the next, figure out some way to not just have the same precinct be 100% weighted. Maybe a metropolis-esque idea?)
- Weighting precincts by demographic similarity

## Results to look at

- Relating to the multiple imputation - considering the 'tree' structure of looking at the ballots, where you count how many people chose each combination of candidates, maybe the "parameters" that we're estimating are proportions of people at each leaf of the tree. These you can estimate across multiple imputations, and aggregate somehow to get estimates and uncertainty measures.
- Election winners / round by round eliminations, compared to the actual election
- Correlation between candidates, particularly Kim and Leno (this could maybe be looked at beter with just the original data)
- Variability in the routes above, the different methods, and within each multiple imputation
- Difference between imputing all of the undervoters (listed any candidate in the race), the rollof voters (voted, just not in this race), and weighting by the overall precinct population (to include full non-voters)? This would really look at "What if everyone voted?"

## Other thoughts

- Our case is not a traditional "missing data" case. This is mostly used for nonresponse in surveys, so make sure you discuss that the methods used aren't entirely best suited for this task. Also the issue of an undervote being a valid/rational choice of a voter, so the data isn't inherently "missing" in the same way that age is in a survey - everybody has an age, not everybody has a second preference in an election. The missing data is thus non-ignorable, because one mechanism for creating it (not having a 2nd preference) is a direct function of the variable (2nd preference). Maybe include different codings for people who only listed one choice versus people who listed the same candidate three times, or something.

- The regression of undervoting from thesis part A shows that our data is in fact not missing at random (MAR): there are variables not included in the election data (race, education, age) that can predict whether a voter will undervote. I think we have to assume MAR to use these methods, however.

- Question we know that some precincts have higher undervote rates than others (based on demographics, etc.), but do certain candidates have higher undervote rates than others?

- Recode undervotes? I'm not sure if listing the same candidate multiple times counts as an undervote right now, but it definitely should.

- When I'm doing hot deck, and there's somebody who only voted for one candidate: should I draw another observation to fill with, then fill in both 2nd and 3rd choices from the same observation? Or should I pick one match for the 2nd choice, then rematch on those two values and pick a 3rd choice? (I definitely shouldn't separately match on the 1st choice alone to pick the 3rd choice, because then you'll get weird combinations of people voting for two candidates for 2nd and third who normaly never would appear together)

- We're assuming that the nonresponse is ignorable, that is, the secondary preferences of the people who didn't list them are similar to the secondary preferences of the people who did list them. 

- This missing data is monotonic: when choice two is missing, choice three is also missing (well, in terms of counting: the algorithm sees no difference between listing A-B-_ or _-A-B). That gives us some more options in terms of methods.

- MCMC versus fully specified / conditional methods. Probably need to use the latter due to the categorical nature of the data.

- Alternative to imputation - maximum likelihood estimation? EM algorithm methods? Not sure what these do that's different. ML doesn't actually fill in the data, which is the conceptual question I'm trying to get at: if everybody *had* voted all the way, what would change in who gets elected?

- Two separate ways of doing things with your imputations: combine and then run the thing you're interested in, or run the thing on each set and then combine. I think the tree thing above is a good way of doing the former, not quite sure what a good way of doing the latter would be.

- Just a thought: this election method is pretty specific and isn't a statistical tool at all, so there's not a lot we can say about assumptions/distributions/properties at the end of the day.

- Another thought: I might need to go back and more sharply recode what is an undervote. Right now, I don't know if listing the same candidate all three times is counting in the regression as an undervote. Should it? That's clearly not voting "completely", but it's not the same idea as somebody listing only one candidate in one slot.

- Validate imputation with test/training data, to see how well different methods perform. Then you can just choose the best method(s) to use before putting into the ballot counting algorithm.

- What about if turnout rises to X% instead of 100%?

- Instead of only looking at election final results changing, we can compare that tree method to see which leaves increase the most? Or see which candidates get more "final" votes, idk.

- We're dealing with this inherent problem of trying to say things about individuals based on their group-level characteristics, which is just bad in general. It's the only way to go about it, but it's definitely not Correct by any means.

- Look into issues about data missing in the predictors versus in the response variable

## Papers to go find

- Xia and Conitzer, 2011
- Lu and Boutilier, 2011
- King, Tomz, and Wittenberg 2000
- Citrin et al., 2003
- Brunell and DiNardo, 2004
- Martinez and Gill, 2005
- Bernhagen and Marsh, 2009
- Double check Rubin, 1976. This might be the book at home.
- Lijphart, 1997
- preflib.org for more data sources?
- Paul, Mason, McCaffrey, and Fox, 2008
- Look for papers on imputing non-ordered non-binary categorical variables
- Honestly more papers on multiple imputation methods in general

- Also email John Doucette (University of Waterloo) to see what his research into imputing partial preferences in voting looks like, and maybe get more ideas for sources to hunt down. From his work:
"By July 2014, I anticipate the completion of a machine learning model tailored specifically to the problem of imputing missing preferences"

## Literature

#### King, et al., 2001 - Analyzing incomplete political science data: An alternative algorithm for multiple imputation

From Paul et al.
Let $X$ be the variable with some missing and $R_i$ be 1 when the $i$th entry is missing and 0 when it's present, let $Y$ be the other data.

1. MCAR. $P(R_i = 1)$ is independent of $X$ and $Y$.
2. MAR. $P(R_i = 1 | Y)$ is independent of $X$.
3. NI. $P(R_i = 1 | Y)$ is not independent of $X$.

- Missing completely at random (MCAR) - nothing in the data (observed or missing) can help us predict when a case will be missing or not.
\[P(data missing | observed values, missing values) = P(data missing)\]
- Missing at random (MAR) - the observed data can help us predict when a case will be missing, but after controlling for the observed data there's nothing systemic about the missing data that would indicate missingness
\[P(data missing | observed values, missing values) = P(data missing | observed values)\]
- Nonignorable (NI) - the missing data itself indicates the likelihood of its being missing. E.g., people with high incomes being unlikely to report their income, and other variables can't predict income well.
\[P(data missing | observed values, missing values) = P(data missing | observed values, missing values)\]
- When you don't have MCAR, then listwise deletion gets you biased parameter estimates.
- Posits this EMis algorithm as a better way of getting parameter estimates with missing data imputation.
- This might be implemented in the Amelia R package? Has the same name. I don't think that one works for categorical data like mine, though.

#### Bernhagen and Marsh, 2009 - Missing voters, missing data: Using multiple imputations to estimate the effects of low turnout

- Looking at the impact of turnout on partisan success: "If turnout went up, would it affect the election result?"
- This one has a really good summary of the limitations of and strategies for dealing with missing vote data.
- They also use STV data! Which is interesting, and means I can try to model my methods after their research.
- Gallagher Index of Disproportionality: maybe a similar thing to the efficiency gap?

#### Doucette

Doucette has two sort of summaries of his research, but I wasn't earlier able to actually find anything full-length.

#### Kroh, 2006 - Taking 'Don't Knows' as valid responses: A multiple complete random imputation of missing data

- Not all  "missing data" is from measurement problems, sometimes the question really doesn't apply and the data *should* be missing. These should be considered as valid responses, not missing.
- Deleting listwise data is always less efficient, but if data is not MCAR this will give biased parameter estimates. If MCAR, then the estimates are unbiased (much like taking a random subset of the data).
- The best way to impute values for a valid "I don't know" is to assign a truly random response. *From Jay: This will probably increase variability, which is good here.* These are randomly drawn from the observed data.

#### Allison, 2012 - Handling Missing Data by Maximum Likelihood

- Proposal of a maximum likelihood based model for dealing with missing data rather than a multiple imputation method.
- Not sure that this works with my data / research question.

#### Citrin, Schickler, Sides, 2003 - What if everyone voted? Simulating the impact of increased turnout in Senate elections

- Theory says that nonvoters are more typically Democratic, so increased turnout should help Democrats. This isn't spatially constant, another theory is that increased nonvoting just helps whichever party is in the minority locally because non-voters are more likely to defect. The Democratic thing is still generally true, however - talk about that as a similar reasoning for why this question is important.
  + While we may not have much outside information on undervoters, we can find the closest full voter to them using what we do know to predict their later round choices. 
- Also mentioned in this paper: just because there is a significant change due to full voter turnout doesn't necessarily mean that an election will flip. It depends on this change AND the existing margin.
  + Maybe the tight line between Leno and Breed will push the former over the top in our simulations? It's hard to predict given the weird RCV algorithm.

#### Lijphart, 1997 - Unequal participation: Democracy's unresolved dilemma

- 5 problems with low voter turnout
  + Turnout is biased against minorities and the less well-off
  + This leads to unequal political influence
  + US turnout is low, but it's low other places as well
  + Midterm and local election turnout is especially low
  + Turnout is declining everywhere
- Solutions: better registration, proportional representation, infrequent elections, weekend voting, many elections at same time
- OR we just require voting

#### Paul et al., 2008 - A cautionary case study of approaches to the treatment of missing data

- Automated model selection can lead to bias in coefficients
- Casewise deletion performed worse than expected
- Simple imputations (mean, conditional mean) did better than expected
- Imputation models are substantive and should be treated as such

- When the data is missing and missingness is uncorrelated to any of the other variables available, we get unbiased parameter estimates (this is quasi-MCAR) and don't lose standard error stuff when we drop cases.
