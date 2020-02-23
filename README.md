# thesis

This repository contains the work completed for my senior thesis at Reed College (Fall 2018 - Spring 2019), advised by [Dr. Heather Kitada Smalley](https://willamette.edu/cla/math/faculty/smalley/index.html) (now Williamette University).

The full thesis can be found in [`thesis_lee.pdf`](thesis_lee.pdf), the input documents to create it are in [`index`](index/), and various analysis scripts are in the [`code`](code/) folder.

A [poster](aapor_poster.pptx) of this work was presented at the [2019 American Association of Public Opinion Research Conference](https://www.aapor.org/Conference-Events/Recent-Conferences.aspx) in Toronto, ON, and won the student poster competition.

## Abstract

Ranked choice voting (RCV) is an alternative voting system where voters rank multiple candidates (instead of simply selecting their highest preference), then these multiple preferences are taken into account during the vote tabulation. One of the arguments against implementing RCV is that it is harder for voters to participate in. Two of the reasons for this are the more complicated ballot design and the extra effort that goes into forming an ordered preference of candidates. To evaluate this claim, we examine rates of ballot errors and undervoting (ranking fewer than the allowed number of candidates) in some American elections conducted with RCV. Results are somewhat inconclusive, but indicate that the variables which are significant in predicting RCV ballot completion are similar to those which are significant in predicting voting rates in general.

We further investigate the impact of overvotes and undervotes on an RCV election. By simulating vote choices for those not present in the ballot data (*imputing* the missing data), we obtain a number of simulated datasets. By re-tabulating the election results with this new data, we create a set of simulated elections where overvoting and undervoting never occur. These are used as a comparison against the true election to see what impact these undervotes and overvotes have on the electoral outcome. Methods used include hot deck imputation and multinomial logistic regression. Our results here show little effect on who actually gets elected, but indicate that the margin of victory might have been smaller without the presence of overvotes and undervotes. This result shows promise for further research on the topic, particularly using different methods and considering different elections.
