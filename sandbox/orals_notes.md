# Orals board notes

## Opening

From Interpolation to Imputation: Ballot Competion in a Ranked Choice Election

- RCV is an alternative voting method (draw example ballot), with an algorithm that counts people's votes in a certain way (explain way)

- There are two ballot phenomena that we're investigating

  + Overvoting: listing multiple candidates for the same ranking (usually less than half of a percent). This is a problem because it's a ballot error, which means that people tried to express their preference and were unable to do so.
  
  + Undervoting: listing fewer unique candidates than possible (usually between 20% and 40%) (in SF, you can only rank 3 candidates). This is a problem because of ballot exhaustion: when a voter did not list any of the candidates in the final round of counting, they have no say in the final round.

- Both of these are underutilizations of a ballot by a voter (and lead to people having less of a say then they are able to), and we should care about them for the same reasons we care about voter turnout more generally.

- Our test case for exploration is a special 2018 SF mayoral election.

A thesis in two parts:

1. What are the demographic qualities associated with overvoting and undervoting? If we're concerned with representation (and how RCV compares to plurality in regards to this), we're concerned with this.

2. What is the electoral impact of these phenomena? While the above issues are theoretical, we're also concerned with the tangible impact of overvoting and undervoting.

## Part 1

We have overvote/undervote data about individual voters (from the SF elections department), but we only have demographic information about block groups (from the census' American Community Survey). To start combining these, we aggregate up to overvote/undervote *rates* at the election precinct level.

This leads to an issue: we don't have demographic information about the precincts, and we don't have electoral information about the block groups. The physical precinct and census boundaries don't line up (no inherent reason to, they come from different governments), so there's not an immediate estimate of one quantity for the other zoning system (draw the toy example, quadrants vs. thirds).

To address this, we use a method called *areal interpolation* to cross this line between the zoning systems. Areal interpolation uses known parameters about one zoning system to create parameter estimates for another zoning system. In the method I used, we assume that the population (and all parameters) are uniformly distributed over the space. For example, if a block group has 50 people and 20 are under 21, then if we split the area in half we assume that each half has 25 people (10 of which are under 21). By splitting the block groups into the necessary sub-regions and combining the corresponding sub-regions into precincts (describe this), we can obtain an estimate for all the demographic parameters of interest about the precincts.

Once we have these demographic estimates, then we train some regression models (linear and logistic) to predict turnout, overvote, and undervote rate as a function of our demographic indicators. The results generally indicate that the factors affecting overvoting and undervoting are the same affecting turnout more generally: race, education, and age. 

## Part 2

To assess how much electoral impact overvoting and undervoting have, we try to create a counterfactual scenario where these never occur. Treating the unfilled votes as "missing data" (maybe talk about this assumption), we use *multiple imputation* to estimate further votes for each voter who had an underfilled ballot (for whatever reason).

Imputation is a method for estimating the true, unseen value of a data point that was unobserved during collection. Our methods include hot deck imputation, where incomplete cases are matched as closely as possible to a "donor case" that the missing value is copied from, and multinomial logistic regression, which adds demographic data to induce a metric of "distance" between categorical variables like precincts.

One of the problems with imputation is that it can underestimate variance in parameters (pretending that we observe this data when we really don't). Multiple imputation addresses this by repeating the imputation process multiple times to create a number of simulated datasets, each of which is fed into the analysis desired. Each of these imputed datasets is a simulation of our election under the counterfactual where no overvotes or undervotes occured.

While this method didn't indicate that London Breed would lose under full voting, it showed that her margin of victory would decrease by almost half. Additionally, the number of votes that were exhausted in the final round of counting decreased by almost half as well. This second result indicates that the problem of ballot exhaustion is significantly affected by "voluntary" undervoting, and isn't a product of election rules alone.

## Future research

Moving forward, we would want to apply some of the research here to more elections than the 2018 SF mayoral election studied. In Cambridge, the sparsity of data in later ranking slots would not mesh well with our imputation methods, so a stochastic process might be an interesting application. We would also want to expand our second research question to see what *full* full voter turnout under RCV looks like, not just no overvotes or undervotes. And finally, there are various ways by which the research methods used here could be made more accurate: better demographic data, more auxiliary information to interpolate, model specifications that account for the high proportion of zero-overvote precincts, etc.
