# Additionality and Asymmetric Information in Environmental Markets: Evidence from Conservation Auctions

Karl M. Aspelund and Anna Russo∗

February 3, 2026

### Abstract

Mechanisms that aim to reduce environmental degradation at low cost can be undermined when participants’ conservation actions are not marginal to the incentive — or “additional” — as the lowest-cost participants may not be the highest social value. We investigate this challenge in the Conservation Reserve Program’s auction mechanism for ecosystem services, linking bids to satellite-derived land use. Three-quarters of marginal auction winners are not additional. The heterogeneity in counterfactual land use introduces adverse selection. We develop a model of bidding and additionality to quantify welfare implications. Alternative auctions increase efficiency by using scoring rules that incorporate expected land use impacts.

\*Aspelund: Harvard University, karlaspelund@fas.harvard.edu; Russo: Harvard University, arusso@fas.harvard.edu. We are grateful to Nikhil Agarwal, Amy Finkelstein, Ben Olken, Parag Pathak, Tobias Salz, and Mike Whinston for invaluable advice and encouragement. We thank Nicholas Ryan and Pino Lopomo (discussants) for excellent feedback, David Donaldson, Daniel Hellerstein, Rich Iovanna, Bryan Pratt and Steven Wallander for generosity in discussions about data and institutions, Michael Roberts for early advice and conversations, and participants at numerous seminars for helpful comments and suggestions. Ben Bakal and Tianrui Lai provided excellent research assistance. We acknowledge support from NMFS-Sea Grant Fellowship (Aspelund) and the Martin Family Society of Fellows for Sustainability (Russo). This material is based upon work supported by the National Science Foundation Graduate Research Fellowship Program under grant number 1745302.

Land-use change contributes 13% of global greenhouse gas emissions (Friedlingstein et al., 2022) and leads to biodiversity loss, water pollution, and erosion (Dirzo et al., 2014; Vörösmarty et al., 2010; Borrelli et al., 2017). Market mechanisms for the provision of environmental services aim to combat this degradation at low cost, but many believe that existing mechanisms with voluntary participation have failed to achieve this objective (Anderson, 2012; West et al., 2020; Jones and Lewis, 2023). A leading explanation is the possibility that participants may have engaged in the incentivized action absent an incentive. The notion of "additionality," defined as the likelihood that an action is marginal to an incentive, is therefore a central challenge to the design and success of many environmental markets (Engel et al., 2008; West et al., 2023).

When does the challenge of additionality undermine the performance of mechanisms incentivizing conservation, and how can market rules be designed to achieve environmental objectives at low cost? We explore these questions by analyzing the challenge of additionality as a problem of asymmetric information. Social welfare in markets for environmental conservation depends on both a landowner's unobserved additionality and her private cost of complying with the market requirements. Market mechanisms, however, screen only on the latter and therefore may not necessarily implement an efficient allocation (Akerlof, 1970). In this paper, we use this perspective to analyze, test, and quantify this potential inefficiency and to examine remedies in alternative market designs.

We study the United States Department of Agriculture's (USDA) Conservation Reserve Program (CRP), one of the oldest and largest Payments for Ecosystem Services (PES) mechanisms in the world. The CRP incentivizes agricultural land retirement and conservation actions via procurement auctions of conservation contracts. CRP contracts pay landowners $1.6-$1.8 billion per year to take cropland out of production and to plant grass mixes, plant or maintain trees, or establish habitats for a duration of ten years. Combining administrative data and satellite-derived land use classifications, we construct a dataset that links landowners' multi-dimensional bids in the CRP scoring auction to their land use, which we use to measure additionality.

We first define additionality and analyze market efficiency in a stylized framework that builds on Einav et al. (2010). Landowners differ in two dimensions. The first is their cost of accepting a CRP contract, which includes the forgone option value of cropping over the contract period and the hassle costs of complying with program requirements. The second is their conservation behavior without the contract, which determines their additionality. The social value of contracting depends on a landowner's cost and additionality, but her choices depend only on her cost and the market incentive. A landowner's cost of accepting the contract may be positively correlated with her additionality, as landowners who are likely to conserve regardless of the CRP have lower opportunity costs of contracting, introducing adverse selection in the market. These differences between socially-optimal and landowner-optimal choices could limit the implementability of efficient allocations and undermine the performance of standard procurement mechanisms (Manelli and Vincent, 1995). These challenges may be remedied by alternative market designs that narrow this gap. Ultimately, the extent of additionality and adverse selection and together their quantitative implications for the performance and design of markets for environmental services are empirical questions.

1

We begin by examining the extent of additionality in our setting. We use the discontinuity in contracting around the winning score in the procurement auction to evaluate additionality at the margin of contract awards. We find that, as incentivized by the CRP, landowners substitute from agriculture to natural vegetation and grasslands upon contracting. However, only about one quarter of marginal winners are additional, which we calculate by comparing the regression discontinuity treatment effect to the magnitude of land contracting at the margin. In other words, three quarters of landowners at the margin of contract awards would have conserved without a CRP contract. However, the status quo auction implicitly assumes all landowners are additional in the design of its scoring rule (Claassen et al., 2014).

To test for adverse selection, we correlate heterogeneity in additionality with heterogeneity in costs reflected in landowners’ bids. We make two assumptions — perfect compliance and no spillovers, both of which we test and validate — to obtain a landowner-specific measure of additionality for all rejected bidders (82% in the most restrictive auction). We examine the correlation between additionality and bids following tests for asymmetric information in insurance markets (Chiappori and Salanie, 2000) and auctions (Hendricks and Porter, 1988). We document substantial heterogeneity in additionality and a positive correlation between additionality and bids, which persists even conditional on a rich set of observed characteristics, indicating the presence of adverse selection in the market. We also highlight that heterogeneity in additionality is predicted by landowners’ choice of conservation contract in the mechanism and by the observed characteristic of soil productivity.

These facts demonstrate that additionality and asymmetric information are relevant; to quantify welfare implications and evaluate the performance of counterfactual market designs, we develop and estimate a joint model of bidding and additionality. In the CRP auction, landowners submit multi-dimensional bids on a menu of heterogeneous contracts, which are ranked by a scoring rule. First, we infer the costs of contracts from revealed preferences in optimal bidding. Second, we model additionality with a conditional expectation function that relates land use to both observed characteristics and unobserved landowner costs. This conditional expectation function is the component of the model that captures the possibility of inefficient or adverse selection.

We estimate the model in three steps. The first two steps adapt standard procedures for the empirical analysis of auctions (Guerre et al., 2000; Hortaçsu, 2000; Hortaçsu and McAdams, 2010; Agarwal et al., 2023). First, we estimate bidder beliefs via simulation. Second, we estimate bidder costs via revealed preferences in optimal bidding. In the final step, we estimate additionality, including how it varies with unobserved landowner costs, by matching observed land use among rejected bidders and the correlation between land use and landowner characteristics and optimal bids. We use our estimates of additionality to calculate the social benefits of contracting based on valuations of environmental services from the CRP literature and the USDA’s revealed preferences across landowners and contracts implied by the scoring rule.

Using these estimates, we first examine whether the existence of a market for conservation increases social welfare. The answer to this question is theoretically ambiguous. We investigate it empirically

2

in the context of a hypothetical uniform market for the base conservation contract. We find that, despite landowners who are not additional and the presence of adverse selection, there exist social welfare gains of $14.37 per acre-year under the socially-optimal uniform price. Second, we evaluate the status quo auction mechanism. We estimate social welfare gains of $120 million per auction, relative to no market. However, while positive, the status quo auction implements only a fraction (12%) of the social welfare gains of an efficient allocation. This allocation determines contract awards based on both landowners’ costs and expected social benefits, which depend on additionality.

Implementing the efficient allocation with an incentive compatible auction may not be possible (Myerson, 1981). Because they are less additional, lower-cost landowners are not always higher social value, and our estimates imply that the allocation rule for this efficient benchmark need not be monotone in bidder cost. We instead propose and evaluate alternative auctions with scoring rules that trade off bidders’ costs against both their conservation actions’ heterogeneous social benefits and their expected additionality. This differs from the status quo scoring rule, which does not consider the latter. Alternative scoring rules adjust asymmetry across bidder observables and scores across the menu of contracts based on predictions of additionality. Our approach re-designs the auction to cost-effectively impact conservation, given that market participants are with some probability not additional.

Simple modifications to the auction’s scoring rule close the gap between the status quo and efficient allocation by 37%, increasing social welfare by over $300 million per auction. Almost all gains are due to changes that incorporate landowner additionality through modifications to the scoring rule. A large share of these gains accrue through simple changes, including a uniform adjustment to the scoring rule and its use to reduce the market’s size. Further gains accrue by using the rule to differentiate among heterogeneously additional landowners. These gains could be implemented in practice by the USDA. More broadly, we highlight that successful market design considers both the heterogeneity in private costs that determine choices and the implications of these choices for land use impacts and social welfare.

**Related Literature** Our primary contributions are to develop a framework to evaluate the implications of additionality for social welfare under current and counterfactual market designs and to provide credible estimates of the extent of additionality and evidence of adverse selection in a large-scale, mature market for conservation. Our regression discontinuity estimates of additionality contribute to a literature estimating treatment effects of payments for ecosystem services (Jack, 2013; Alix-Garcia et al., 2015; Jayachandran et al., 2017; West et al., 2020; Rosenberg and Pratt, 2024) and offset markets (Calel et al., 2025). Our framework builds on theoretical analyses of asymmetric information (van Benthem and Kerr, 2013; Mason and Plantinga, 2013; Li et al., 2022) and empirical tests for selection (Montero, 1999; Jack, 2013; Jack and Jayachandran, 2019; Chen et al., 2024) in environmental incentive programs and offset markets. Our research also relates broadly to the design of environmental incentive programs and other voluntary regulation (Einav et al., 2022; Ito et al., 2023) and complements work studying other sources of inefficiency in markets for

3

environmental conservation (Harstad, 2016; Aronoff and Rafey, 2024).

We contribute to a literature studying quality concerns in procurement auctions (Manelli and Vincent, 1995; Decarolis, 2014; Lopomo et al., 2023; Carril et al., 2024), where we provide an empirical framework to evaluate alternative auction designs in the presence of adverse selection on bidder quality (additionality in our setting). This empirical framework draws on a large literature studying selection in insurance markets (Akerlof, 1970; Chiappori and Salanie, 2000; Einav et al., 2010; Bundorf et al., 2012; Marone and Sabety, 2022). Our model and estimation strategy also use techniques from a literature advancing the empirical analysis of auctions (Guerre et al., 2000; Hortaçsu, 2000; Hortaçsu and McAdams, 2010; Agarwal et al., 2023). We draw on and extend existing work on scoring and other multi-dimensional auctions (Che, 1993; Asker and Cantillon, 2008; Lewis and Bajari, 2011; Kong et al., 2022; Allen et al., 2023; Bolotnyy and Vasserman, 2023) to incorporate discrete bidding, a non-linear scoring rule, and a correlation between additionality and bidder costs.

# I. Theoretical Framework

## A. Model

There exists a continuum of landowners, indexed by $i$, each making a decision to contract, $x_i \in \{0, 1\}$, to obtain a transfer, $p$. In Section IV., we adapt this framework to a finite number of landowners bidding for contracts in a procurement auction with a quantity constraint.

The contract involves a promise to provide an environmental service ($a_i = 1$) versus not ($a_i = 0$). In our setting, $a_i = 1$ denotes agricultural land retirement (conservation) and $a_i = 0$ denotes cropping. The action $a_i = 1$ generates social benefits from positive environmental externalities, $B > 0$.

Define $a_{i1}$ as landowner $i$’s action when $x_i = 1$ and $a_{i0}$ as her action when $x_i = 0$. We assume perfect compliance, so $a_{i1} = 1$. Because $B$ is generated whenever $i$ chooses $a_i = 1$, regardless of contract choice $x_i$, the benefit of contracting with $i$ is only the *incremental value* $B \cdot (1 - a_{i0})$. $a_{i0}$ is unobserved whenever $x_i = 1$ and is therefore non-contractible.

A landowner’s decision to contract $x_i$ is the only available instrument to affect the provision of the environmental service. In contrast to a perfect Pigouvian subsidy, the instrument $x_i$ is a binding long-term contract, compliance may involve non-zero hassle costs (see, e.g. Jack and Jayachandran (2019)), and, as is relevant in our auction setting, there may exist quantity constraints.<sup>1</sup>

**Landowner Types** Each landowner $i$ is characterized by a type $\theta_i = (c_i, a_{i0})$ distributed according to the cumulative distribution function $F(\theta)$. $c_i$ is a landowner’s cost of accepting the contract, or the minimum transfer $p$ required for a landowner to accept the contract $x_i = 1$. $c_i$ includes both a landowner’s forgone option to crop over the contract period and any hassle costs associated with complying with the program rules.<sup>2</sup> $a_{i0}$ is a landowner’s action absent the contract. Appendix A

\*<sup>1</sup>A Pigouvian subsidy on all conserved land in the United States would be technologically and administratively challenging and would cost over forty times the current budget of the CRP.

\*<sup>2</sup>Hassle costs are relevant across PES settings (Jack and Jayachandran, 2019; White and Minang, 2011). In the CRP specifically, contracts include a conservation plan developed with the Natural Resource Conservation Service,

4

defines $\theta_i = (c_i, a_{i0})$ in a micro-founded model.<sup>3</sup>

A landowner’s expectations about the payoffs to cropping determine both the magnitude of the forgone option and the likelihood of cropping when unconstrained by the contract. One would therefore expect $c_i$ and $a_{i0}$ to be correlated in the population, as landowners may have a low $c_i$ in large part because they have unprofitable land that they do not plan to crop. But, landowners face uncertainty over the stochastic payoffs to cropping that determine their forgone option over ten-year contracts, and contracting in realistic settings involves hassle costs that enter $c_i$ and may be arbitrarily correlated with $a_{i0}$. It will be useful to define the conditional expectation function:

$$ (1) \quad \tau(c) = \mathbb{E} [1 - a_{i0} \mid c = c_i]. $$

This function describes the expected additionality, or the expected impact of contracting on $a_i$, among all landowners with the same cost of accepting the contract.

**Social versus Landowner Incentives** The social surplus of contracting with landowner $i$ is:

$$ (2) \quad SS_i = B \cdot (1 - a_{i0}) - c_i. $$

This is positive when the incremental value of environmental services due to contracting is higher than a landowner’s costs of accepting that contract. Landowners choose $x_i = 1$ if $p \geq c_i$. Let

$$ (3) \quad x_i^*(p) = \mathbb{1} \{p - c_i \geq 0\} $$

be landowner $i$’s choice to contract at price $p$. Equations (2) and (3) show that landowner $i$ transacts based only on $p$ and her cost $c_i$, but social surplus depends also on $1 - a_{i0}$, or her additionality. $p$ will therefore not necessarily incentivize the highest social surplus landowners to contract.

**Efficient Prices and Allocations** The socially-optimal uniform price solves:<sup>4</sup>

$$ (4) \quad \max_p \int (B \cdot \tau(c) - c) \, x^*(p; c) \, f_C(c) \, dc, $$

where the density $f_C$ is the marginal of $F(\theta)$ on costs, $c_i$, and $x^*(p; c) = \mathbb{1} \{p - c \geq 0\}$. The solution

requirements to purchase specific seed mixes that vary in costs geographically, and mid-contract management requirements and possible audits, which may involve in-person interaction at a local USDA office. Indeed, free text survey responses among CRP participants cite a desire to reduce the amount of paperwork and record-keeping mandated by the program (Allen and Vandever, 2003).
\*<sup>3</sup>In this model, each landowner is defined by a distribution of potential payoffs to cropping over the contract period and a hassle cost type. Only the landowner’s cost of accepting the contract $c_i$, which she reports to the mechanism, and her expected action absent the contract, which is unaffected by the mechanism, are relevant for landowner choices and social welfare (for the mechanisms we consider).
\*<sup>4</sup>A uniform price and uniform $B$ are motivated by the absence of observables. This could be because they have already been conditioned on, where equation (4) defines the pricing problem in a sub-population.
5

to this problem is equivalent to one where a quantity is chosen and an allocation is implemented with a Vickrey auction.

Equation (4) shows that $f_C$ and $\tau(c)$ are sufficient statistics for social welfare and landowner choices when $p$ is the only instrument available to allocate landowners to contracts. More generally, $f_C$ and $\tau(c)$ are sufficient statistics for social welfare for any incentive compatible mechanism.<sup>5</sup> Contracting with a landowner with cost $c_i$ increases expected social surplus when:

$$ (5) \quad B \cdot \tau(c_i) - c_i \geq 0. $$

Our interest in this stylized framework is in when an allocation that maximizes expected social surplus, $B \cdot \tau(c) - c$, is implementable. We will refer to this allocation as the efficient allocation.<sup>6</sup>

## B. Graphical Analysis

We analyze the efficiency of allocations in the market graphically, plotting markets with different $F(\theta)$ in Figures 1a and 1b. Each figure plots two curves: one based on $f_C$ and one based on $\tau(c)$. The first curve is the inverse distribution function of costs $c_i$, $F_C^{-1}(q)$, or the *marginal cost curve* (MC), where the horizontal axis $q$ is the share of the population ranked by their costs of accepting the contract. The second curve is the value of contracting at each quantile of the distribution of $c_i$, $B \cdot \tau(F_C^{-1}(q))$, or the *contract value curve*.<sup>7</sup> The contract value curve lies weakly below $B$ reflecting the possibility that $a_{i0} > 0$ for some landowners.

Each panel in Figure 1 displays an upwards-sloping contract value curve ($\tau'(c) > 0$). This captures the fact that landowners’ expectations about future payoffs to cropping influence both $c_i$ and $a_{i0}$ — landowners who are likely to conserve may face a low cost of accepting a contract requiring conservation — and the existence of adverse selection in the market. Modeling adverse selection with an upwards-sloping contract value curve builds on the widely-used graphical analysis of adverse selection in insurance markets developed in Einav et al. (2010). The vertical distance between the contract value and marginal cost curves equals $B \cdot \tau(c) - c$, or the expected social surplus of contracting with all landowners with costs equal to $c$. From equation (5), it is efficient to contract only where the contract value curve lies above the marginal cost curve.

In Figure 1a, the efficient allocation can be implemented with socially-optimal incentives, satisfying $p^* = B \cdot \tau(p^*)$, at the intersection of the contract value and marginal cost curves. This implements social welfare gains in triangle CDE. Setting $p = B$, the social value of the conservation action, leads to inefficient contracting and social welfare losses (triangle EFG) where the incremental social benefits of conservation caused by the contract are less than the costs incurred among landowners.<sup>8</sup>

\*<sup>5</sup>See Lopomo et al. (2023) for more details and a proof and Einav et al. (2010) on the use of similar sufficient statistics for the analysis of adverse selection in competitive insurance markets

\*<sup>6</sup>We focus on this benchmark, following Lopomo et al. (2023), as it maximizes expected social surplus given cost reports $c_i$. This allocation achieves the maximum social welfare gain implementable by any mechanism; however, implementing this allocation will be possible only for some $F(\theta)$.

\*<sup>7</sup>We conduct the change of variables (to $q$) so that the areas between the contract value and marginal cost curves are interpretable as magnitudes of social welfare gains (or losses).

\*<sup>8</sup>Appendix A discusses a special case of a one-dimensional model in which $p^* = B$ and empirical tests that

6

In Figure 1b, the efficient allocation cannot be implemented. It is also socially-optimal not to offer a market despite the existence of landowners for whom contracting is socially desirable. In Figure 1b, the contract value curve lies below the marginal cost curve at low values of $c_i$ (low $q$). This represents landowners that have low but positive costs of accepting a contract — due to some option value of cropping and/or hassle costs — but a high likelihood of conserving without the contract. In this market, a regulator cannot implement the efficient allocation (triangle EFG), as any incentive that is attractive for landowners in triangle EFG is also attractive for landowners in CDE, where losses in CDE outweigh gains in EFG.

The difference between equations (2) and (3) causes the inefficiency in Figure 1b. The regulator can only affect allocations based on landowner costs, $c_i$, and the incentive $p$, but social surplus depends also on the impact of contracting, or $a_{i0}$. Because $B \cdot \tau(c) - c$, the vertical distance between the contract value and marginal cost curves, crosses zero from below in Figure 1b, no mechanism can implement the efficient allocation (triangle EFG) as it would require an allocation rule that is not monotonically decreasing in landowner costs (see Myerson (1981); Lopomo et al. (2023)).

The difference between Figures 1a and 1b thus has implications beyond the illustrative posted prices mechanism in this Section. In a procurement auction with a quantity constraint — our empirical setting — if $B \cdot \tau(c) - c$ is not decreasing in $c$, the efficient allocation subject to the quantity constraint may not be implementable by any mechanism. In Figure 1a, $B \cdot \tau(c) - c$ is decreasing in $c$, but in Figure 1b, it is not.

Figure 1 illustrates that social welfare and socially optimal incentives depend on $f_C$ and $\tau(c)$. The goal of our empirical analysis is therefore to estimate them. However, this stylized model was limited in its tools; our empirical analysis will include a richer set of contracts and observable characteristics to investigate social welfare gains under various market designs.

# II. Setting and Data

## A. The Conservation Reserve Program

Our empirical setting is the Conservation Reserve Program (CRP), a Payments for Ecosystem Services (PES) scheme incentivizing conservation on agricultural land administered by the United States Department of Agriculture (USDA). Established in 1985, the CRP pays landowners between $1.6 and $1.8 billion per year to retire erodible and other environmentally sensitive cropland and undertake conservation actions through long-term contracts, typically 10 years in duration.<sup>9</sup> The CRP is one of the largest and most mature PES schemes in the world, and CRP contracts are similar to those in other PES programs (see Kinzig et al. (2011) and Salzman et al. (2018) for overviews).

Unlike the uniform pricing problem in Section I., the USDA awards CRP contracts via a complex auction mechanism. Under the CRP’s General Enrollment mechanism, eligible landowners bid for

\*distinguish this special case from the cases presented in Figure 1.

\*<sup>9</sup>Most contracts are 10 years, but a small fraction of incentivized actions are associated with 15-year contracts.

7

heterogeneous contracts in a discriminatory, asymmetric, scoring auction.<sup>10</sup> Contracts are differentiated by conservation actions that “top up” the base action of land retirement. These actions include planting specific grass mixes, planting or maintaining trees, and establishing or restoring pollinator or rare habitats.

Bids are scored according to a known scoring rule that awards bidders points for the environmental sensitivity of their land, their chosen contract, and their bid rental rate, a payment per acre per year. Characteristics of the land that determine points for environmental sensitivity include erodibility, importance for habitats, and potential for water and air pollution and carbon sequestration. Bid rental rates are subject to a bid cap based on the average land rental rate in the county and soil productivity estimates. Appendix B describes the mechanism and scoring rule in more detail.

The aggregate acreage covered by CRP contracts is determined by Congress in the Farm Bill, which in turn determines the threshold score. All bidders with scores above the threshold score are awarded a contract.<sup>11</sup> Given uncertainty in both the aggregate acreage to be awarded contracts in that auction and their opposing bidders, bidders face uncertainty over the threshold score at the time of bidding. Bids are prepared with the assistance of staff at Farm Services Agency county offices, who help landowners trade-off different contracts and bid rental rates.

Auctions occur every 1-4 years. Landowners are eligible to bid if they meet erosion standards, are in a national or state conservation priority area, and had cropped at least four years in a 5-10 year window preceding the auction or are re-enrolling CRP land.<sup>12</sup> The first two requirements target environmentally sensitive land. The final requirement is designed to limit participation to additional landowners. Landowners face steep penalties, refunding all payments since enrollment plus a 25% penalty, if they exit early or fail to comply with the rules of the program.

Research quantifying the value of the CRP has documented improvements in wildlife habitats, erosion control, water quality, and carbon sequestration from cropland retirement (Feather et al., 1999; Hansen, 2007; FAPRI-MU, 2007; Allen and Vandever, 2012; Johnson et al., 2016; Hellerstein, 2017). However, these analyses are typically conducted using models that ignore counterfactual land use. Perhaps motivated by eligibility requirements designed to restrict to additional landowners, the scoring rule is constructed under the assumption that land would be cropped in the absence of the program (Claassen et al., 2014).<sup>13</sup> Because the primary motivation for the program and most environmental gains accrue from land retirement, relative to cropping, the possibility that some landowners conserve absent the CRP presents the additionality concern.

\*<sup>10</sup>In addition to the General Enrollment mechanism, there is a posted-price mechanism (Continuous Enrollment) for targeted land. Historically, approximately three quarters of CRP acreage has been contracted via the General Enrollment mechanism (Hellerstein, 2017).

\*<sup>11</sup>There is also a constraint that no more than 25% of a county’s land can be in the CRP, which almost never binds.

\*<sup>12</sup>Activities in the 1-5 years preceding bidding have no impact on CRP eligibility to eliminate any perverse incentives to crop land to in order to become eligible or maintain eligibility for the CRP.

\*<sup>13</sup>This USDA report writes: “Benefit-cost indices are used to rank applications for acceptance in all major USDA conservation programs ... Existing indices, however, implicitly assume full additionality.”

8

## B. Data

Our dataset links bids to a panel of land use to measure additionality.

Data on Bids We obtain confidential administrative data on all components of the bid, including the bid rental rate, the bid contract and acreage, and the characteristics of landowners that impact the score. Our data cover all seven auctions that occurred from 2009 to 2021. We also obtain data on all CRP contracts.

Each landowner is defined by a collection of fields, or Common Land Units, the smallest agricultural unit with a common land use. CRP contracts typically cover only a subset of a landowner’s total fields. Our data include the geolocation of all bidding landowners for all auctions as well as identifiers for the specific fields offered into the mechanism (“bid fields”) for one auction (in 2016). Auctions are nation-wide, but bidders are concentrated in the Midwest, Southern Plains, and Southeast (see Appendix Figure C.3a).

Data on Land Use We link bidders, and for the purpose of comparison, agricultural non-bidders, to a panel of land use outcomes. The primary land use outcome of interest is whether land is cropped versus retired, as this is the behavior incentivized by the CRP. We use both remote sensing and administrative datasets due to their complementary strengths.14

Our primary dataset is the Cropland Data Layer (CDL), a remote-sensing product from the National Agricultural Statistics Service (NASS). This dataset provides land cover classifications at 30m by 30m resolution (roughly a quarter acre) from 2009-2020. The binary indicator of crop versus crop — our primary outcome of interest — is rarely misclassified (Lark et al., 2021).15 However, as in other satellite-derived products, non-classical measurement error can generate biases in assessing land-use change (Alix-Garcia and Millimet, 2022; Torchiana et al., 2024).

Our second dataset is field-level administrative data on land use that agricultural landowners report to the USDA in “Form 578” for 2013-2019. These data are accurate and comprehensive for cropped land because crop insurance payouts depend on them. However, landowners are only incentivized to report Form 578 if fields are insured by crop insurance, and landowners with CRP contracts are mechanically classified as non-cropped. We also use a collection of high-resolution aerial imagery (1m) of contracted land collected under the National Agriculture Imagery Program (NAIP) from 2017-2021. We use these images to observe and confirm compliance with CRP rules.

While accurate to assess agricultural land retirement — the main incentivized activity of the CRP — these datasets are much more limited in their ability to measure different “top-up” actions that

> 14The ability to conduct a geospatial linkage between micro-data on CRP bids and land use was made possible by recent efforts within the Economic Research Service (ERS) at the USDA. Previous micro-level analyses of land use among CRP participants used the National Resources Inventory (NRI) (Roberts and Lubowski, 2007). raneous work among ERS economists spearheaded linkages to administrative land use data (“Form 578”) (Rosenberg and Pratt, 2024), which facilitated a comprehensive linkage to remotely-sensed land use outcomes.

> 15The Cropland Data Layer has cropland superclass user and producer accuracy of over 95% from 2008-2016.

<page_number>9</page_number>

differentiate the heterogeneous contracts in the mechanism (e.g. specific species). Our main estimates of additionality will focus on the measure that we can observe and the principal goal of the program: the binary outcome of retiring versus cropping land.

Appendix C provides more details about agricultural land units, the construction of our dataset, the geographic distribution of cropland and CRP bidders (see Appendix Figure C.3), and the use of aerial photographs to confirm compliance with CRP rules.

**Summary Statistics** Table 1 presents summary statistics. Columns (1)-(2) summarize all agricultural landowners in the US, including CRP-eligible and ineligible landowners. Columns (3)-(4) summarize all land among bidders in our sample and columns (5)-(6) summarize bid fields.

Panel A presents land use outcomes in the year of bidding (prior to contract awards). Approximately 22-23% of all bidders’ land is cropped (20-24% on bid fields) versus 26-29% nationwide. The majority of the remainder is in natural vegetation and grassland. Corn and soybean cultivation account for two-thirds of all cropping. The remote sensing and administrative data generally align.

Bidders have lower USDA-constructed estimates of soil productivity, are larger, and are more environmentally sensitive as measured by the scoring rule than the average agricultural landowner (Panel B). These differences, along with the differences in land use in Panel A, are likely driven in part by eligibility requirements that columns (1) and (2) are not conditioning on. The average bidder offers 84.1 acres into the CRP mechanism (33% of a bidder’s land) for a rental rate per acre per year of $83. Two-thirds of bidders bid on a contract that includes a grassland action, 21% choose a wildlife habitat action, and 12% choose a tree-related action. 70% of bidders are re-contracting after their initial contract expired.<sup>16</sup> 81% of bidders are awarded contracts across the auctions in our sample. The average auction includes 36,772 bidders.

# III. Evidence: Additionality and Asymmetric Information

## A. Regression Discontinuity Estimates of Additionality

We exploit the discontinuity at the winning score threshold to estimate the treatment effect of a CRP contract in a regression discontinuity (RD) design. Our RD specification normalizes each landowner’s score relative to her auction’s win threshold and evaluates how land use outcomes differ around this threshold. We estimate, for landowner, or bidder, $i$, in auction $g$, and year $t$:

$$ (6) \quad y_{igt} = \beta_{r(i,t)} \cdot \mathbb{1}\{S_{ig} \geq S_g\} + f_{r(i,t)}(S_{ig} - S_g) + \nu_{igt}, $$

where $S_{ig}$ is landowner $i$’s score in auction $g$, $S_g$ is the winning score threshold in auction $g$, $r(i, t) = t - t_g(i)$ normalizes the time dimension relative to the year of each auction ($t_g(i)$), and $f_{r(i,t)}(S_{ig} - S_g)$ are relative-year-specific local-linear regressions in the MSE-optimal bandwidth (Calonico et al., 2014) allowed to differ for positive and negative values of $S_{ig} - S_g$. $\beta_{r(i,t)}$ are time-

\*<sup>16</sup>Re-contracting bidders are treated identically to new bidders by the scoring rule.

10

varying RD coefficients relative to the year of the auction. We also estimate the pooled specification:

$$ (7) \qquad y_{igt} = \beta \cdot \mathbb{1}\{S_{ig} \geq S_g\} + f(S_{ig} - S_g) + \nu_{igt}. $$

Restricted to $r(i, t) > 0$, $\beta$ is an estimate of the treatment effect at the margin of contract awards pooled over post-auction years. Treatment effect estimation (in equations (6) and (7)) therefore requires post-auction observations of land use outcomes, available for bidders in five auctions that occurred between 2010 and 2016.<sup>17</sup> These auctions form the basis of our RD analysis sample, together with a panel of land use outcomes from 2009-2010 (remote-sensing data) and 2013-2019 (administrative data). We estimate equations (6) and (7) at the landowner level to accommodate the possibility of spillovers to non-bid fields, and we cluster standard errors at the bidder level.

The RD design is valid under the assumption that bidders possess information about the winning score threshold’s distribution, but not its exact ex-post realization. If bidders knew the threshold score, they would optimally bid just above it, invalidating the design. Figure 2a presents a histogram of the score distribution normalized relative to the threshold score, $S_{ig} - S_g$, the running variable of the RD, and confirms the lack of bunching at the winning score threshold. Figure 2b also shows no differential cropping at the discontinuity before the auction, plotting (binned) raw data and estimates of equation (7) for $r(i, t) \leq 0$.

Interpretation of equations (6) and (7) also requires an estimate of the magnitude of the first stage. Figure 2c plots the share of bidders with a CRP contract after the auction around the award threshold (equation (7) for $r(i, t) > 0$) and demonstrates a first stage close to one. Figure 2d plots the share of bidders’ land covered by the contract (equation (7) for $r(i, t) > 0$), equal to approximately one third among marginal winners.

**Results** Figure 3 presents data and estimates of equation (7) for $r(i, t) > 0$. As the CRP’s primary goal is to incentivize agricultural land retirement, our outcome of interest is the share of each bidder’s land that is cropped, which we measure in the remote sensing data. Figure 3a demonstrates that CRP contracts impact land use at the margin of contract awards. Landowners above the score threshold, who are awarded a contract, crop eight percentage points less land than the marginal landowners who are not awarded contracts.<sup>18</sup> This land is instead in natural vegetation and grassland (trees, shrubs, wetlands and grasses), as incentivized by the CRP (Figure 3b). The comparison between the magnitudes of land use impacts (Figure 3), relative to land covered and compensated under a CRP contract (Figure 2d) forms the basis of our additionality estimates. If contracting induced all marginal winners to change land use ($\tau = 1$), the magnitudes of the estimated treatment effects would align with the magnitudes of land covered by the contract at the margin. The fact that they do not indicate that not all marginal winners are additional.

\*<sup>17</sup>The last two auctions in our sample (in 2020 and 2021) have contract periods that begin after our panel of linked land use data concludes. Still, the variation from these auctions is useful to estimate our bidding model (Section IV.).
\*<sup>18</sup>Cropping is not zero for winning bidders, as bidders typically only contract on a subset of their land.

11

To analyze the time path of effects and illustrate our estimates of additionality, Figure 4 presents coefficient estimates of $\beta_{r(i,t)}$ from equation (6) and compares them to a 100% additional ($\tau = 1$) benchmark, estimated as the share of marginal bidders’ land in a CRP contract (the pooled post-period estimate from equation (7)). We estimate equation (6) using both the remote sensing data and the administrative data. Dashed lines represent pooled post-period estimates (equation (7)).

Figure 4 presents four facts. First, consistent with Figure 2b, coefficient estimates are zero before the auction. Because the estimates in Figure 4 are relative-year-specific RD coefficients, pre-period effects are identified in levels. Second, post-period effect sizes and time-trends are similar in both land-use datasets, confirming that results are not driven by either non-classical measurement error in the remote sensing data or misreporting in the administrative data. Third, while treatment effects grow in the first two years as land transitions, effects are approximately constant over the ten year contract period. Opportunities to rebid would attenuate treatment effects over time; we observe no evidence of decreasing magnitudes in Figure 4 and limited re-bidding at the margin even after multiple opportunities (see Appendix Figure D.4).<sup>19</sup>

Finally, Figure 4 demonstrates that over the 10-year period, the magnitude of the treatment effect of a CRP contract on land use is only approximately one quarter of the $\tau = 1$ benchmark, or that approximately one quarter of marginal winners are additional. Table 2 presents point estimates from equations (6) and (7).<sup>20</sup> The main results in Table 2 quantify additionality estimates: depending on the specification, we estimate rates of additionality at the margin of contract awards from 22-29%.

Estimates of additionality at the margin provide information about the $B \cdot \tau$ curve in Figure 1. First, it lies below $B$, as many landowners are not additional. Second, it highlights the need to estimate, rather than assume, the $\tau(c)$ function. If alternatively, costs and additionality could be summarized by a single index, in which bidders with positive costs are additional and bidders with costs equal to zero have additionality equal to zero, then at the margin additionality should be either zero or one (see Appendix A); our estimates reject both of these hypotheses.<sup>21</sup> Third, using variation in the winning score threshold across auctions, we obtain estimates of additionality at the margin of 12% to 42% that are increasing in the quantile of the auction’s marginal bidder (see Appendix Table D.2). This cross-auction heterogeneity is consistent with an upwards sloping $B \cdot \tau$ curve.

**Beyond the RD** As in the model in Section I., we argue that the estimates in Figure 4 are driven by heterogeneous land use absent the contract ($a_{i0}$) on the land bid into the mechanism. While

\*<sup>19</sup>Even five years following the initial bid, only approximately 20% of losers have rebid (and fewer have won) despite multiple opportunities (see Appendix Figure D.4, which plots the hazard rate of rebidding following a failed initial bid for all and marginal losers). This is consistent with the institutions: the General Enrollment mechanism is shrinking with total acreage contracted decreasing on average over the sample period, limiting the option value of re-bidding.

\*<sup>20</sup>Appendix Figures D.1, D.2, and D.3 present additional RD figures. Appendix Tables D.3 and D.4 assess robustness to weighting each bidder by their bid CRP acreage (Appendix Table D.3) and to restricting to bids of more than five acres (Appendix Table D.4).

\*<sup>21</sup>This interpretation is slightly complicated by bidder asymmetry, contract choices, and the pooling of auctions with different thresholds. We re-estimate the RD specification in equation (7) split by the amount a bidder would need to bid for the base contract to achieve $S_g$ and find $0 < \tau < 1$ across groups (results in Appendix Table D.1).

12

in theory both conservation without a CRP contract and cropping with a CRP contract (non-compliance) could contribute to the result in Figure 4, we find no evidence of non-compliance.<sup>22</sup> Second, we document the absence of any spillovers onto non-bid fields (see Panel C of Table 2).

These results imply that we observe a realization of each bidder’s $a_{i0}$ on bid fields for all bidders who lose the auction. With knowledge that $a_{i1} = 1$, we observe a measurement of $1 - a_{i0}$ for each losing bidder. This provides rich information about heterogeneity beyond the RD for bidders below the threshold score.

## B. Testing for Asymmetric Information

We use observations of landowners’ realized additionality ($1 - a_{i0}$) and bids to conduct a test for asymmetric information in the spirit of Chiappori and Salanie (2000) and Hendricks and Porter (1988). We estimate the following specification:

$$ (8) \quad 1 - a_{i0} = \beta \cdot \mathbf{b}_i + \pi \cdot \mathbf{z}_i + h \left( \mathbf{z}_i^s \right) + \nu_i, $$

where $1 - a_{i0}$ is measured as the share of landowner $i$’s bid fields that are cropped, observed only for rejected landowners without a contract award, $\mathbf{b}_i$ represents characteristics of $i$’s bid, $h \left( \mathbf{z}_i^s \right)$ are controls for characteristics that enter the scoring rule, and $\mathbf{z}_i$ are other landowner characteristics. We estimate multiple specifications of equation (8), but each one includes controls for the scoring rule, which impacts the strategic environment facing bidders. These include estimates of groundwater quality, surface water quality, wind and water erosion (deciles), air quality impacts, and whether a bidder is in a Wildlife Priority Zone or Air Quality Zone. We restrict to the one auction where we observe the delineations of bid fields (the 2016 auction); this is required to construct $1 - a_{i0}$. It is also the most restrictive auction, so $1 - a_{i0}$ is observed for 82% of bidders.<sup>23</sup> A positive correlation implies an upwards-sloping contract value curve (see Section I.), or adverse selection in the market.<sup>24</sup>

**Results** We first document evidence of adverse selection. Figure 5a presents a binned scatterplot of the correlation between additionality and the bid per acre-year (the bid rental rate), residualized of $h \left( \mathbf{z}_i^s \right)$. Figure 5a demonstrates substantial heterogeneity in additionality and a systematic positive correlation between higher bids — reflecting higher costs of accepting a contract — and additionality. The interpretation of Figure 5a is intuitive: bidders with low costs of accepting a CRP contract have low costs in part because of private information that they would be likely to conserve without a CRP

\*<sup>22</sup>We assess the CRP’s compliance regime by inspecting high resolution aerial photographs of over 1,000 contracted fields (see Appendix C).

\*<sup>23</sup>The low acceptance rate in 2016 resulted from an unusually small number of CRP acres available for enrollment, driven by the acreage cap set by the 2014 Farm Bill and a limited number of expiring contracts. Still, some bidders are awarded contracts, and in Section IV., we address the fact that equation (8) is estimated in a selected sample.

\*<sup>24</sup>Relative to the approach to estimating adverse selection of Einav et al. (2010), our strategy leverages the fact that we observe *both* optimal bids *and* land use among rejected bidders. To compare the two approaches, it is useful to consider a simpler second-price auction for a single contract, where the distribution of costs therefore equals the distribution of bids. Then, because we can construct a measure of $a_{i0}$ for each rejected bidder, $\tau (c)$ is simply the average $1 - a_{i0}$ at each value of the observed bid. With posted prices, as in Einav et al. (2010), estimating the distribution of costs (and $\tau (c)$) requires exogenous price variation to trace out this distribution.

13

contract. Figure 5b shows that bids remain correlated with additionality, capturing residual private information, conditional on other observables including prior land use interacted with estimates of the soil productivity of the bidders’ land.

Next, we show that choices of contracts in the mechanism are systematically correlated with additionality. Figure 5c replaces $\mathbf{b}_i$ in equation (8) with a vector of chosen contract indicators — the submitted bid on the menu of contracts — and documents substantial adverse selection (low additionality) on tree-related contracts, relative to the base category of introduced grasses. Figure 5c highlights that contract choices reveal information about additionality and that alternative menus of contracts may lead to different outcomes in the market.

Finally, we present evidence that observable characteristics are predictive of additionality. Figure 5d plots additionality by decile of landowner predicted soil productivity, conditional on $h\left(\mathbf{z}_i^s\right)$ but excluding any endogenous bid choices from the regression specification. These estimates of soil productivity are collected by the USDA and are designed to approximate the amount that a landowner would be able to earn on a given parcel of land. This characteristic is an ideal predictor of additionality in theory, and Figure 5d demonstrates that it is predictive of additionality in practice. This result highlights the potential to differentiate incentives using this characteristic, which is not currently incorporated to rank bidders in the scoring rule.

**Robustness** The results in Figure 5 restrict to the 2016 auction, which is useful because we can construct a measure of $1 - a_{i0}$ and observe it for a large share of bidders below the threshold. Cross-auction heterogeneity suggests that the patterns in 2016 generalize across auctions. First, estimates that leverage heterogeneity in the location of the RD threshold, both pooling all auctions (Appendix Table D.1) and exploiting cross-auction heterogeneity (Appendix Table D.2), replicate both the range of additionality estimates — 10-20% among lower bidders to about 40-50% among higher bidders — and the existence of adverse selection. Second, RD coefficients restricted to bids for tree-related contracts document null effects across all auctions (Appendix Figure D.6). These two exercises illustrate the same patterns as our baseline analysis using both different auctions and different empirical strategies. Finally, while our baseline approach hinges on delineating bid fields (only possible in 2016), we examine whether tract-level heterogeneity in cropping in the year of the auction follows stable patterns of heterogeneity. Tract-level cropping rates demonstrate similar, albeit not identical, heterogeneity across observables across auctions (Appendix Figure D.7).

# IV. Empirical Model of Bidding and Additionality

To quantify the welfare implications of the results in the preceding section, we develop a model of bidding and additionality to estimate (i) the distribution of landowner costs among a menu of contracts and (ii) additionality as a function of costs and observable characteristics. Together with estimates of the social benefits of CRP actions, the model facilitates analysis of social welfare under current and counterfactual market designs.

Landowners with a vector of private costs bid on discrete contracts, differentiated by heterogeneous

14

conservation actions, in response to a non-linear scoring rule. Landowners also differ in their additionality, which we model with a conditional expectation function that depends on landowners’ characteristics and costs. Our empirical strategy first uses the optimality of bidding in the auction to estimate bidders’ costs by revealed preferences and then estimates expected additionality as a function of costs and landowner characteristics by matching moments of the observed joint distribution of land use, landowner characteristics, and optimal bids.

## A. Model

There are $N$ landowners, indexed by $i$, and $J$ contracts, indexed by $j$. Each landowner is characterized by (i) her costs $(c_i, \kappa_i)$ for $\kappa_i = \{\kappa_{ij}\}$, and (ii) her land retirement conservation action $a_{i0}$ absent the CRP. Extending the model in Section I., re-define $F(\theta)$ as the cumulative distribution function of landowner types $\theta_i = ((c_i, \kappa_i), a_{i0})$ and $F_{c,\kappa}$ as the marginal on $(c_i, \kappa_i)$.

The vector $(c_i, \kappa_i)$ defines landowner $i$’s costs of accepting a contract. A landowner’s cost of contract $j$ is $c_i + \kappa_{ij}$, where $c_i$ is the base cost of accepting the contract, common across contracts, and $\kappa_{ij}$ is the top-up cost associated with contract $j$. We assume $(c_i, \kappa_i)$ are drawn independently across landowners conditional on observables $\mathbf{z}_i$.

It will again be useful to define the function:

$$ (9) \quad \tau(\mathbf{z}_i, c_i, \kappa_i) = \mathbb{E} [1 - a_{i0} \mid \mathbf{z}_i, c_i, \kappa_i], $$

or the expected difference in land retirement (conservation) with a CRP contract versus without a CRP contract given observable characteristics $\mathbf{z}_i$ and landowner costs $(c_i, \kappa_i)$.

**Auction** Landowners (bidders) submit a two-part bid $\mathbf{b}_i = (r_i, \mathbf{x}_i)$. $\mathbf{x}_i$ is a contract vector, with $x_{ij} = 1$ if the $j$-th contract is chosen and $x_{ij} = 0$ otherwise. Landowners choose a single contract so $\sum_j x_{ij} = 1$. If landowner $i$ wins the auction, $\mathbf{b}_i$ describes the terms of her CRP contract: she performs the action defined in $\mathbf{x}_i$ and receives a payment of $r_i$ dollars per acre-year. Each bid $\mathbf{b}_i$ is converted into a score according to a known scoring rule $s(\mathbf{b}_i, \mathbf{z}_i^s)$. All landowners above a winning threshold score $\underline{S}$ are awarded a contract.

Landowner $i$ forms beliefs about the probability of winning the auction with a score $S$ given uncertainty over her competitors and the acreage limit of the auction. We assume that landowner $i$ does not observe the number or characteristics of her competitors. All landowners form the same beliefs about the distribution of the threshold score $\underline{S}$. Define $G(S) = \text{Pr} \{ \underline{S} \geq S \}$.<sup>25</sup>

\*<sup>25</sup>Beliefs $G(S)$ integrate over the strategies of the other bidders, the number of competing bidders, and the aggregate acreage limit. This approach is motivated by the fact that bidding is decentralized among thousands of bidders across the U.S., and we document evidence of quantity uncertainty empirically (see Appendix Figure E.1). We assume that beliefs are consistent with the empirical distribution of scores, following standard approaches to auctions (Hortaçsu, 2000; Hortaçsu and McAdams, 2010) and other incomplete information games (Agarwal and Somaini, 2018; Agarwal et al., 2023).

15

Each landowner $i$ solves:

$$ (10) \quad \mathbf{b}_i^* = \underset{(r, \mathbf{x})}{\text{argmax}} \left\{ \underbrace{(r - c_i - \mathbf{x} \cdot \kappa_i)}_{\text{Payoff to } i \text{ conditional on bid } (r, \mathbf{x})} \times \underbrace{G\left(s\left((r, \mathbf{x}), \mathbf{z}_i^s\right)\right)}_{\text{Probability of } i \text{ winning with bid } (r, \mathbf{x})} \right\}, $$

where a landowner chooses her bid $\mathbf{b}_i = (r_i, \mathbf{x}_i)$ to maximizes her payoff conditional on winning, multiplied by the probability of winning with that bid, given her costs $(c_i, \kappa_i)$. Landowners compete on both $r$ and $\mathbf{x}$ in equation (10), but whether a landowner wins against her competitors depends only on her score. A landowner’s best response in equation (10) therefore solves both an “inner problem” and an “outer problem,” building on Asker and Cantillon (2008) and Che (1993).

The landowner’s inner problem solves for the optimal payment and contract $(r(S, \mathbf{x}_i^*(S), \mathbf{z}_i^s), \mathbf{x}_i^*(S))$ to reach a target score $S$, where $r(S, \mathbf{x}, \mathbf{z}_i^s)$ is the bid payment required to obtain score $S$ with contract $\mathbf{x}$, implicitly defined by $s((r(S, \mathbf{x}, \mathbf{z}_i^s), \mathbf{x}), \mathbf{z}_i^s) = S$. For any target score $S$, a landowner’s best response must satisfy:

$$ (11) \quad r(S, \mathbf{x}_i^*(S), \mathbf{z}_i^s) - \mathbf{x}_i^*(S) \cdot \kappa_i \geq r(S, \mathbf{x}, \mathbf{z}_i^s) - \mathbf{x} \cdot \kappa_i $$

for all $\mathbf{x}$ where $s((r(S, \mathbf{x}, \mathbf{z}_i^s), \mathbf{x}), \mathbf{z}_i^s) = S$. $r(S, \mathbf{x}, \mathbf{z}_i^s)$ defines the menu of payments awarded for each contract under the scoring rule (see Appendix Table B.3 for an example) and determines the incentives facing landowners in the inner problem. Because the scoring rule is decreasing in $r$, $r(S, \mathbf{x}, \mathbf{z}_i^s)$ is increasing in the points awarded to $\mathbf{x}$, raising the probability that $\mathbf{x}$ is a landowner’s optimal choice of contract, all else equal, for any target score $S$. Similarly, from equation (11), all else equal a landowner with a lower $\kappa_{ij}$ will be more likely to choose a contract with $x_{ij} = 1$.

Given (11), the best response $\mathbf{b}_i^* = (r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s), \mathbf{x}_i^*(S_i^*))$ in equation (10) must also satisfy

$$ (12) \quad G(S_i^*) (r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s) - c_i - \mathbf{x}_i^*(S_i^*) \cdot \kappa_i) \geq G(S) (r(S, \mathbf{x}_i^*(S), \mathbf{z}_i^s) - c_i - \mathbf{x}_i^*(S) \cdot \kappa_i) $$

for all integer scores $S$. This is the landowner’s outer problem solving for her optimal score $S_i^*$, trading off changes in the probability of winning the contract with her payoff conditional on winning. All else equal, it will be optimal for higher-cost landowners to choose lower scores.<sup>26</sup>

**Additionality** In the contract period, landowners make land use decisions. If awarded a contract, $a_{i1} = 1$. If not, landowners choose $a_{i0}$, which is not contractible. At the time of bidding, equation

<sup>26</sup>Consider a one-point upward deviation from $S_i^*$ to $S_i^* + 1$ when $\mathbf{x}_i^*(S_i^*) = \mathbf{x}_i^*(S_i^* + 1)$. Then, from equation (12), for $S_i^*$ to be optimal, it must be the case that:

$$ c_i + \mathbf{x}_i^*(S_i^*) \cdot \kappa_i \geq r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s) + (r(S_i^* + 1, \mathbf{x}_i^*(S_i^* + 1), \mathbf{z}_i^s) - r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s)) \frac{G(S_i^* + 1)}{G(S_i^* + 1) - G(S_i^*)} $$

Similarly, all else equal, lower cost landowners will choose higher scores (consider instead a one-point downward deviation from $S_i^*$ to $S_i^* - 1$).

16

(9) is the population expectation of $1 - a_{i0}$ for landowners with observable characteristics $\mathbf{z}_i$ and costs $(c_i, \kappa_i)$. We estimate the function $\tau(\mathbf{z}_i, c_i, \kappa_i)$ instead of modeling the choice of $a_{i0}$ because together with $F_{c,\kappa}$, this conditional expectation function is sufficient to calculate social welfare under counterfactuals that hold the menu of contracts fixed.

Although the mechanism is more complex, the source of inefficiency is the same as in Section I.: landowner choices and allocations depend only on the scoring rule $s(b_i, \mathbf{z}_i^s)$ and costs $(c_i, \kappa_i)$, but the efficient allocation depends also on $\tau(\mathbf{z}_i, c_i, \kappa_i)$.

## B. Identification and Estimation

We assume that beliefs $G(S)$ and the scoring rule, which defines $r(S, \mathbf{x}, \mathbf{z}_i^s)$, are known or can be estimated. Then, the inequalities in equations (11) and (12) defining a landowner's best response link the observed bids $\mathbf{b}_i^* = (r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s), \mathbf{x}_i^*(S_i^*))$ and underlying costs $(c_i, \kappa_i)$ and incorporate all restrictions on preferences given the observed bids.<sup>27</sup> These optimality conditions defining the bidders' best response is the basis for our identification argument and empirical approach, following Agarwal et al. (2023). It is also clear from these equations that without further variation in the mechanism or parametric assumptions, the vector of costs $(c_i, \kappa_i)$ for each individual bidder cannot be point identified from the observed bids: the vector of costs has dimension $J$, while the optimal score is determined only for the single optimal contract $\mathbf{x}_i^*(S_i^*)$. In principle, with sufficient exogenous variation in the scoring rule, one could "trace out" the distribution of $(c_i, \kappa_i)$ conditional on observable characteristics $\mathbf{z}_i$, $F_{c,\kappa|\mathbf{z}}$, using the best response inequalities in equations (11) and (12) (Agarwal et al., 2023). Tracing out $F_{c,\kappa|\mathbf{z}}$ without any parametric assumptions requires full support variation in the vector of payments $r(S, \mathbf{x}, \mathbf{z}_i^s)$ for each contract $\mathbf{x}$ across target scores $S$.<sup>28</sup> This is a demanding requirement.

Identification of $\tau(\mathbf{z}_i, c_i, \kappa_i)$ extends this logic. It relies on the fact that we observe the joint distribution of optimal bids and land use for rejected bidders. If we were to observe an auction where bidders submitted bids given beliefs $G(S)$, but all bidders were unexpectedly and exogenously rejected, then counterfactual land use ($a_{i0}$) would be observed for the entire population. $\tau(\mathbf{z}_i, c_i, \kappa_i)$ would be identified based on the argument above: $\mathbb{E}[1 - a_{i0} | \mathbf{z}_i, \mathbf{b}_i^*]$ is observed and the revealed preference inequalities in equations (11) and (12) link $\mathbf{b}_i^*$ to costs $(c_i, \kappa_i)$. However, even in the auction with the most rejected bidders in our sample, $a_{i0}$ is not observed for the 18% of bidders who are awarded a contract. Without this ideal "cancelled auction" experiment, variation in the mechanism to replicate it, or parametric assumptions, the entire function $\tau(\mathbf{z}_i, c_i, \kappa_i)$ cannot be learned from the rejected bidders alone. Restricting to the observed thresholds in the data, variation

\*<sup>27</sup>Intuitively, the inner problem defines inequalities for $\kappa_i$ based on the relative payments for chosen *versus* non-chosen contracts at the optimal score $S^*$ and the outer problem inequality provides bounds on $c_i + \mathbf{x}_i^*(S_i^*) \cdot \kappa_i$.

\*<sup>28</sup>Define $\mathbf{r}$ as the vector of payments to obtain a target score $S$ for each contract, $\mathbf{r} = \{r(S, \mathbf{x}^j, \mathbf{z}_i^s)\}_{j=1}^J$ (where $\mathbf{x}^j$ denotes the contract with $x_{ij} = 1$). The full argument requires support variation in $\mathbf{r}$ to trace the tails of $F_{c,\kappa|\mathbf{z}}$. For intuition, consider increasing relative payments $r(S, \mathbf{x}^j, \mathbf{z}_i^s) - r(S, \mathbf{x}^{j'}, \mathbf{z}_i^s)$ for all $S$ and all $j'$. From equation (11), as $r(S, \mathbf{x}^j, \mathbf{z}_i^s) - r(S, \mathbf{x}^{j'}, \mathbf{z}_i^s)$ increases for all $j'$, an increasing number of bidders will choose $\mathbf{x}_i^j$, ultimately replicating the familiar case of an auction for a single contract, $\mathbf{x}^j$.

17

in the scoring rule across bidders can replicate the “cancelled auction” if scores are sufficiently low for some values of $\mathbf{z}_i^s$ independent of $(c_i, \kappa_i)$ and $a_{i0}$. Or, by parameterizing $\tau (\mathbf{z}_i, c_i, \kappa_i)$, we can extrapolate based on the observations of bids *and* land use among rejected bidders.

Our approach relies on a combination of exogenous scoring rule variation—both cross-auction and within-auction cross-landowner—and parametric extrapolation to address the limits to the variation we observe in the data. Some sources of variation differentially affect certain contracts, shifting relative incentives in the inner problem in equation (11), others shift the level of the scoring rule for some bidders but not others.

The first source of scoring rule variation is a mid-mechanism policy change: after bids were initially collected in 2021, additional payments based on contracts’ carbon sequestration potential were announced and bids were re-collected under this new scoring rule. Under the final mechanism, a landowner could increase the bid to obtain the same target score $r(S, \mathbf{x}, \mathbf{z}_i^s)$ by either 3%, 5%, or 10%, relative the interim rule, depending on the chosen contract. We obtained the bids submitted in both the interim and final mechanism, providing two different functions $r(S, \mathbf{x}, \mathbf{z}_i^s)$ across the two auctions with the same bidders and contract period. In a second source of variation, before 2011, bidders in Wildlife Priority Zones (WPZs) received extra points regardless of contract $\mathbf{x}$; after, they were awarded 30 WPZ points only if they chose a subset of contracts, providing variation in $r(S, \mathbf{x}, \mathbf{z}_i^s)$ across bidders and auctions. Finally, whether or not a bidder is in an Air Quality Zone (AQZ) shifts the level of the score. Appendix Figure E.2 summarizes the scoring rule variation by plotting $r(S, \mathbf{x}, \mathbf{z}_i^s)$ for each contract $\mathbf{x}$ across the changes described above, and Appendix Figure E.3 provides more details on individual changes.<sup>29</sup>

The scoring rule variation is useful but limited in practice, both in the differential incentives across contracts and in replicating the “cancelled auction” thought experiment. We therefore parameterize $F_{c,\kappa|\mathbf{z}}$ and $\tau (\mathbf{z}_i, c_i, \kappa_i)$. We parameterize $(c_i, \kappa_i)$ as

$$ (13) \quad c_i \sim N\left(c\left(\mathbf{z}_i\right), \sigma_c^2\left(\mathbf{z}_i\right)\right) \quad \kappa_{i j}=p_j\left(\mathbf{z}_i\right)+u_j\left(\mathbf{z}_i\right)+\epsilon_{i j} \quad \epsilon_{i j} \stackrel{i i d}{\sim} N\left(0, \sigma_\kappa^2\left(\mathbf{z}_i\right)\right) $$

with $c_i$ and $\kappa_{ij}$ drawn from independent normal distributions with means and variances that depend on observable characteristics, $\mathbf{z}_i$. $\kappa_{ij}$ depends on contract features: $p_j$ defines mean costs for a vector of primary covers, relative to the base category of introduced grasses (normalized to zero), and $u_j$ is

<sup>29</sup>To contextualize the the magnitude of the variation in the scoring rule, it is helpful to compare it to the variation in the threshold score. The variation in $r(S, \mathbf{x}, \mathbf{z}_i^s)$ is of reasonable but modest magnitude: the WPZ variation yields a change in $r(S, \mathbf{x}, \mathbf{z}_i^s)$ of approximately $8 for affected contracts. The mid-mechanism policy change affects different contracts, but the magnitude of the changes (in $r(S, \mathbf{x}, \mathbf{z}_i^s)$) is similar. By comparison, the difference between $r(S, \mathbf{x}, \mathbf{z}_i^s)$ for the base contract at the average threshold score versus 2016 threshold score is over $20. See Appendix Figure E.2 for details. A final important limitation is that these sources of variation shift relative incentives across groups of contracts: even combining all of the sources of variation, $r(S, \mathbf{x}^j, \mathbf{z}_i^s) - r(S, \mathbf{x}^{j'}, \mathbf{z}_i^s)$ for some comparisons remains constant.

18

a vector of upgrade covers plus the no-upgrade option (normalized to zero).<sup>30</sup> We also parameterize

$$ (14) \quad \tau(\mathbf{z}_i, c_i, \kappa_i) = \pi \cdot \mathbf{z}_i + \beta \cdot c_i + \alpha \cdot \kappa_i. $$

This specification allows $\tau(\mathbf{z}_i, c_i, \kappa_i)$ to depend on observable characteristics, $\mathbf{z}_i$, and unobserved bidder costs $(c_i, \kappa_i)$, where we align the dimension of $\alpha$ with the primary and upgrade parameterization of $\kappa_{ij}$, i.e. we impose that $\alpha_j = \alpha_{j'}$ if $p_j = p_{j'}$ and $u_j = u_{j'}$.

**Estimation** Estimation proceeds in three steps. Appendix E provides details.

**Step 1: Simulate $G(S)$** We estimate beliefs $G(S)$ with a resampling approach following Hortaçsu (2000); Hortaçsu and McAdams (2010). First, we fit Beta distributions to the number of bidders and acreage limits across auctions. We use additional historic data on acreage limits and the numbers of bidders for all auctions from 2000 to 2021 to fit these distributions. Then, we simulate the numbers of bidders and the acreage thresholds and re-sample from the observed joint distribution of the scores and acreages of bidders within each simulated auction. $G(S)$ is the share of simulated auctions in which $S$ is a winning score.

**Step 2: Estimate Costs** Given estimates of beliefs $G(S)$ and the known scoring rule, the next step estimates $F_{c,\kappa|\mathbf{z}}$ using the parameterization in (13) and the optimality of observed bids as the solution to equation (10) via Maximum Simulated Likelihood (MSL). The likelihood of each observed optimal bid, given $F_{c,\kappa|\mathbf{z}}$, is the probability that equations (11) and (12) are satisfied at $\mathbf{b}_i^* = (r(S_i^*, \mathbf{x}_i^*(S_i^*), \mathbf{z}_i^s), \mathbf{x}_i^*(S_i^*))$, which we obtain by checking all feasible deviations for each bidder across simulation draws of $(c_i, \kappa_i)$ from a candidate $F_{c,\kappa|\mathbf{z}}$. Because the choice set of feasible bids is large, we coarsen the observed bid space used to construct each bidder's likelihood contribution in estimation.<sup>31</sup> We classify bidders into 32 categories of observable types $\mathbf{z}_i$ that parameterize $F_{c,\kappa|\mathbf{z}}$ based on interactions of quartiles of soil productivity, prior CRP status, and prior land use status. We estimate $F_{c,\kappa|\mathbf{z}}$ using all auctions in our sample, including both the interim and final mechanism in 2021, which incorporate all the scoring rule variations described above.

**Step 3: Estimate Additionality** The third step estimates the parameters in equation (14), $(\pi, \beta, \alpha)$, via the Method of Simulated Moments (MSM). This estimator searches for the parameters $(\pi, \beta, \alpha)$ that rationalize moments of the joint distribution of $1 - a_{i0}$, $\mathbf{b}_i^*$, and $\mathbf{z}_i$ observed in the data. Specifically, we simulate $(c_i^k, \kappa_i^k)$ from $F_{c,\kappa|\mathbf{z}}$ estimated in Step 2, solve for the optimal $\mathbf{b}_i^{*k}$

\*<sup>30</sup>Landowners face a discrete choice over each of the primary and upgrade covers, but primary and upgrade covers can be combined. There are 36 total possible contracts, reflecting finer categorizations beyond the five dimensions in $p_j$ (twelve total) that each can be combined with an upgrade option. See Appendix B for more details. In principle, costs may vary or be correlated across each of the 36 contracts in unmodeled ways; this parameterization attempts to parsimoniously capture key differences across contracts.

\*<sup>31</sup>We coarsen the observed choice of score into deciles of the score distribution and the choice of contract into seven categories corresponding to the seven dimensions of $p_j$ and $u_j$. We do not coarsen the choice set when solving the bidder's problem. We use importance sampling (following Ackerberg (2009)) to reduce computational burden.

19

for each simulation draw $k$, and search for the parameters $(\pi, \beta, \alpha)$ that match: (i) additionality at the winning score threshold, (ii) additionality among all rejected bidders and by observable characteristics, (iii) the covariance between additionality and chosen scores, and (iv) the additionality among all landowners choosing a given contract.

We estimate $\tau(\mathbf{z}_i, c_i, \kappa_i)$ using rejected bidders from the 2016 auction; this is the auction with the most rejected bidders in which we observe outcomes on bid fields for the 82% of bidders that are rejected (as in Figure 5). We assume that the function $\tau(\mathbf{z}_i, c_i, \kappa_i)$ is stable across years, so the auction with the most rejected bidders provides the most information about counterfactual land use.<sup>32</sup> This auction incorporates the cross-landowner variation in WPZ and AQZ status, which shifts scores; however, even with this variation we must rely on parametric extrapolation for the highest scoring bidders.<sup>33</sup> We measure additionality as the share of bid fields that are cropped over the contract period in the remote sensing data, where we weight each field and year from 2017-2020 equally. The observables $\mathbf{z}_i$ in $\tau(\mathbf{z}_i, c_i, \kappa_i)$ are the 32 observable bidder types that parameterize $F_{c,\kappa|z}$ and the components of the scoring rule excluding WPZ and AQZ status. We condition on optimal bids that are below the score threshold.

We conduct robustness checks to evaluate the sensitivity of results to parametric assumptions in Appendix E. First, we estimate alternative functional forms of $\tau(\mathbf{z}_i, c_i, \kappa_i)$. Second, we conduct an exercise to examine how additionality varies with costs without parametric assumptions. To do so, we conduct an inversion exercise in the spirit of Guerre et al. (2000) holding fixed bidders’ choice $\mathbf{x}_i$ and estimate additionality as the average cropping rates among rejected bidders across quantiles of this inverted value. We emphasize that this exercise is not directly interpretable, as it selects on bidders’ endogenous choice of contract in response to the non-linear scoring rule. Nevertheless, it is useful to assess that our conclusions are not driven by our assumed functional forms.

## C. Estimates

Table 3 summarizes mean costs across contracts and Figure 6a plots the estimated distribution of $c_i$.<sup>34</sup> A large share of landowners have low values of $c_i$, below $50 per acre, per year with a tail of bidders with higher $c_i$. Relative costs, with the base contract of introduced grasses normalized to zero, are generally intuitive. Higher soil productivity bidders have higher costs for primary covers, but lower costs for upgrade covers. Because costs are estimated using only revealed preferences in

\*<sup>32</sup>In Appendix G, we assess robustness to the assumption of stability of $\tau(\mathbf{z}_i, c_i, \kappa_i)$ and our baseline estimation approach that relies exclusively on land use data from the 2016 auction. We extend our analysis of counterfactual auctions to construct scoring rules based instead on additionality estimates that match cross-tract heterogeneity in cropping in the year of the auction (Appendix Figure D.7) and cross-auction heterogeneity in regression discontinuity coefficients (Appendix Table D.2). This approach allows us to probe the assumption of stability of $\tau(\mathbf{z}_i, c_i, \kappa_i)$ by constructing a scoring rule in a sample of (previous) auctions, comparing it to our baseline rule (Appendix Figure G.5), and assessing performance in a hold-out auction (Appendix Figure G.6).

\*<sup>33</sup>Because the variation in $r(S, \mathbf{x}, \mathbf{z}_i^s)$ across changes to the scoring rule is relatively small compared to the differences in bid payments at different score thresholds (see Appendix Figure E.2), in the most restrictive 2016 auction, we observe a measurement of $a_{i0}$ for many landowners whose optimal bid would win in other auctions across any observed variations to the scoring rule. This motivates our estimation approach.

\*<sup>34</sup>Appendix Table E.2 presents parameter estimates for select cells of $\mathbf{z}_i$ and standard errors. Appendix Table E.1 reports results estimated using beliefs based on the distribution of scores in the previous (versus the current) auction.

20

bids, we examine whether estimated costs correlate with land use. Figure 6b presents a binned scatterplot of $1 - a_{i0}$ against simulated costs $c_i$ (conditional on observables) for rejected bidders. Figure 6b documents that landowners with higher base costs are more additional, indicating the presence of adverse selection mediated by observables.

Table 4 presents select parameter estimates in $\tau(\mathbf{z}_i, c_i, \kappa_i)$ that describe how additionality varies with $(c_i, \kappa_i)$. Each column of Table 4 presents a different specification of $\tau(\mathbf{z}_i, c_i, \kappa_i)$. Columns (1) and (2) restrict to a correlation between additionality and $c_i$ and impose that $\alpha = 0$. Column (1) includes observable characteristics from the scoring rule and column (2) adds observable characteristics that parameterize $F_{c,\kappa|\mathbf{z}}$ (interactions of soil productivity and prior land use). Consistent with the positive correlation between bids and additionality in Figures 5a and 5b, the positive coefficients in columns (1) and (2) of Table 4 indicate that landowner additionality is systematically correlated with unobserved costs, conditional on observable characteristics. The coefficient estimates in Table 4 reflect the adverse selection presented in Figures 5a and 5b.

Columns (3) and (4) allow additionality to also depend on $\kappa_i$. Column (3) only allows tree-related action costs to impact additionality. The coefficient on tree-related $\kappa_{ij}$ is positive and large, while the coefficient on $c_i$ is reduced, but still positive. Column (4) allows additionality to vary with $\kappa_i$ more flexibly, and the residual correlation between costs and additionality loads onto $\kappa_i$. The largest coefficient remains on tree-related contracts.

We examine model fit in Appendix E. For costs, we compare model-implied choices to data (Appendix Figure E.6) and our estimates to some limited administrative data submitted to the USDA (Appendix Table E.3). Our fit is reasonable, and model-implied costs are similar in rank and in magnitude to the administrative data. For $\tau(\mathbf{z}_i, c_i, \kappa_i)$, we examine the fit by correlating observations of $1 - a_{i0}$ and model-implied costs among rejected bidders (Appendix Figure E.7); the linear functional form of $\tau(\mathbf{z}_i, c_i, \kappa_i)$ appears reasonable. Finally, we calculate a model-implied estimate of additionality at the RD margin of 23%, similar to our estimates in Section III..

## D. Social Benefits

To calculate social welfare, we require estimates of the social benefits of contracted actions, $B_j(\mathbf{z}_i^s)$. We index $B_j(\mathbf{z}_i^s)$ by $j$ to account for heterogeneous social benefits across contracts and allow $B_j(\mathbf{z}_i^s)$ to depend on observable characteristics in the scoring rule $\mathbf{z}_i^s$ that capture heterogeneity in the environmental sensitivity of landowners. We take average estimates of the value of CRP actions from literature that quantifies benefits from habitat restoration and reductions in erosion, water and air pollution, and greenhouse gas emissions (Johnson et al., 2016; Feather et al., 1999; Hansen, 2007). We take relative valuations across characteristics $\mathbf{z}_i^s$ and contracts $j$ from the scoring rule. This is justified by USDA reports that explicitly state that scores assume full additionality ($\tau = 1$) when scoring relative benefits (Claassen et al., 2014). Appendix F provides details.

This approach makes important assumptions that add nuance to interpretation. First, the true social benefits of each action may diverge from the scoring rule, in which case our results could instead be interpreted as representing the revealed preferences of the USDA. Second, while we cannot

21

rule out the possibility of some displaced conservation elsewhere, the CRP is targeted specifically at environmentally sensitive land; our results could be interpreted as the net social benefits of targeting conservation in these environmentally sensitive locations.<sup>35</sup>

Finally, we’ve specified additionality as one-dimensional (land-retirement), but the menu of contracts is multi-dimensional. This is due to fundamental data limitations, the substantial simplification that focusing on only this one dimension affords, and the fact that the primary incentivized activity of the CRP is land retirement. Our baseline specification follows Section I. and calculates contract value as $B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i)$.<sup>36</sup> However, in Appendix G, we present and discuss results under an alternative assumption where additionality only affects the base contract. This assumes a valuation of contracts equal to $B_0 (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i) + B^j (\mathbf{z}_i^s)$, where $B_0 (\mathbf{z}_i^s)$ is the value of the base action and $B^j (\mathbf{z}_i^s)$ is the incremental value of the top-up action.<sup>37</sup>

# V. Social Welfare and Alternative Market Designs

Estimates of the costs $(c_i, \kappa_i)$ and expected social benefits $B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i)$ of CRP contracts allow us to analyze social welfare and market design. Define the expected social surplus of contracting with landowner $i$ for contract $j$ as:

$$ (15) \quad B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i) - c_i - \kappa_{ij}. $$

Equation (15) is based on $\tau (\mathbf{z}_i, c_i, \kappa_i)$ not $1 - a_{i0}$. Because current and counterfactual mechanisms screen only on $(\mathbf{z}_i, c_i, \kappa_i)$, using equation (15) for comparisons of social welfare is without loss.

## A. Graphical Analysis

We first examine allocative efficiency and pricing in the context of our graphical framework with a uniform price and a single contract. In these analyses, we collapse heterogeneity to one dimension of cost for a single contract (as in Section I.).

Figure 7 presents the empirical analogue of Figure 1, graphing the marginal cost $(MC)$ and contract value $(B \cdot \tau)$ curves for the base contract. We simulate the minimum cost to landowners of fulfilling the base contract to construct the MC curve. Then, we calculate the average $B_0 (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i)$, where $B_0 (\mathbf{z}_i^s)$ denotes the social benefit of the base action, at each quantile of this cost distribution to obtain the contract value curve. The two facts from Section III. are reflected in Figure 7. The contract value curve lies below $B$, representing landowners who are not additional, and is upwards-sloping, representing adverse selection. Figure 7 illustrates the welfare implications of these two facts in the context of our stylized framework.

First, the contract value curve crosses the marginal cost curve from above, and social surplus — equal

\*<sup>35</sup>Appendix Figure G.1 illustrates alternative assumptions on $B_j (\mathbf{z}_i^s)$ in our graphical welfare framework.

\*<sup>36</sup>For tree actions, we can test this assumption because we can distinguish between trees and grasses in the remote sensing data. We find that over 60% of rejected bidders choosing a tree contract have the majority of their bid land in trees, and 92% have some trees, supporting this as our baseline assumption.

\*<sup>37</sup>See Appendix Figure G.7 for results under this alternative assumption.

22

to the vertical distance between the contract value and marginal cost curves — is monotonically decreasing. The empirical market for the base contract described by Figure 7 is therefore similar to Figure 1a, not Figure 1b. This socially-optimal uniform price implements social welfare gains of $14.37 per acre-year in region CDG.<sup>38</sup>

Second, Figure 7 illustrates inefficient contracting when prices are set at $B$ (the average $B_0(\mathbf{z}_i^s)$ across landowners), ignoring counterfactual land use. Pricing at $B$ leads to social welfare losses of $12.68 per acre-year in triangle GHI, almost all of the gains realized in triangle CDG.<sup>39</sup> These social welfare losses highlight the importance of quantitative analysis of the joint distribution of costs and additionality to set socially-optimal incentives. This is underscored by the fact that the presence of adverse selection implies that additionality at the margin will be higher than the average additionality of the landowners in the market, plotted for illustration in Figure 7 in gray.

**Heterogeneity** Figure 8a examines heterogeneity across the base and tree contracts motivated by the evidence of substantial adverse selection on tree contracts in Figure 5c. Figure 8a calculates the marginal cost curve as the minimum cost required to fulfill any tree-related contract and plots the contract value at each quantile of this distribution. The exercise requires substantial extrapolation, but it illustrates how alternative landowner type distributions across important classes of contracts in our setting can generate different conclusions.

In Figure 8a, the contract value curve crosses the marginal cost curve from below, leading to social welfare losses at low $q$. The social welfare gains in region DE cannot be implemented without incurring losses among lower cost landowners in CD. This is because, as in Figure 1b, the ordering of landowners by social surplus (the vertical distance between the contract value and marginal cost curves) differs from the ordering of landowners by costs (the construction of the x-axis, $q$). Because they are less additional, the lowest-cost landowners are not the highest social value.

Motivated by Figure 5d, Figures 8b and 8c replicate Figure 7 in sub-populations split by estimated soil productivity. Both contract value and marginal cost curves differ across low versus high soil productivity landowners, implying different socially-optimal prices.

## B. Alternative Auctions

The objective of payments for ecosystem services mechanisms is to *impact conservation* at lowest cost. Mechanisms that consider reports of $(c_i, \kappa_i)$ but not the effect of contracting on conservation $\tau(\mathbf{z}_i, c_i, \kappa_i)$ may not advance this goal. We simulate bidding and additionality under status quo and counterfactual auctions to investigate this possibility and the performance of alternative designs. Figure 9 and Table 5 present results. We calculate social welfare under each allocation,

\*<sup>38</sup>Appendix Figure G.2 demonstrates the robustness of the conclusions in Figure 7 and Figure 8a to estimation error by plotting marginal cost and contract value curves across bootstrap draws.

\*<sup>39</sup>The landowners that make up triangle GHI incur costs from accepting the contract that are larger than the incremental social benefits from increased conservation (the $B \cdot \tau$ curve). These costs represent both the forgone option to crop, as the contract constrains them to conserve, and hassle costs associated with contracting (discussed in Section I.). However, we do not distinguish between these two sources of costs.

23

$$ (16) \sum_{i} \sum_{j} (B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i) - c_i - \kappa_{ij}) \cdot x_{ij}. $$

Table 5 also reports additional details: USDA spending, landowner surplus, environmental benefits $\sum_i \sum_j B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i) \cdot x_{ij}$, average additionality, and the share of bidders allocated a contract.

**Status Quo** Because social welfare depends on additionality but the design of the status quo auction does not, the social value of the CRP is ambiguous. We document social welfare gains of $120 million per auction in the status quo (bar (1) of Figure 9), across the seven auctions in our sample. This is calculated by simulating optimal bidding, given our estimates of costs and beliefs.

However, social welfare under the status quo auction is only 12% of an efficient benchmark. This efficient benchmark is defined as the allocation that uses all observables $\mathbf{z}_i$ and the full vector of costs $(c_i, \kappa_i)$ to maximize social welfare (equation (16)) subject to two constraints. First, each landowner must obtain at most one contract $\sum_j x_{ij} \leq 1$. Second, no more landowners are allocated contracts, in each auction, than under the status quo. Because many landowners are not additional, the efficient allocation involves contracting with fewer landowners than the status quo and the quantity constraint does not bind for most auctions.

This efficient allocation may not be implementable in an incentive compatible auction if social surplus and allocations are not monotone in bidder costs (Myerson, 1981). This complication is relevant because of adverse selection; once the mechanism’s impact on conservation (additionality, or $\tau (\mathbf{z}_i, c_i, \kappa_i)$) is considered, the lowest-cost landowners may not be the highest social value. This issue is illustrated in principle in Figure 1b and based on our estimates in Figure 8a.

**Alternative Auctions** The status quo auction underperforms in part because it does not consider additionality in its design. Implementing the efficient allocation may be impossible, but changes to the status quo may reduce the gap. We consider an approach that treats landowners asymmetrically by the expected social benefits of a CRP contract, which depends on their additionality. This differs from alternative solutions that attempts to define eligibility requirements for participation. Contracting with a low expected additionality landowner could be justified at sufficiently low cost, and landowners who are likely to be additional may still counterfactually conserve with some probability. We allow incentives to differ across landowner observables and contracts using counterfactual scoring rules based on predictions of $B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i)$. These auctions build directly on the status quo, which uses a scoring rule based on $B_j (\mathbf{z}_i^s)$.

We define $s_j (\mathbf{z}_i)$ to score the expected social benefit of contracting with a bidder with characteristics $\mathbf{z}_i$ for contract $j$. We focus on linear rules based on (a slight simplification of) the functional form of the status quo scoring rule:

$$ (17) s_j (\mathbf{z}_i) = \omega_{\mathbf{z}} \cdot \mathbf{z}_i + \omega_j, $$

24

where $\omega_z$ parameterizes scores across observables $\mathbf{z}_i$ and $\omega_j$ parameterizes scores across contracts $j$.

Our baseline counterfactuals implement allocations with a Vickrey-Clarke-Groves (VCG) mechanism in auctions that we term "Vickrey auctions with scoring." These auctions maximize a definition of social welfare implied by the scoring rule

$$ (18) \sum_i \sum_j (s_j (\mathbf{z}_i) - c_i - \kappa_{ij}) \cdot x_{ij}, $$

where bidders are treated asymmetrically by $s_j (\mathbf{z}_i)$ not $B_j (\mathbf{z}_i^s) \cdot \tau (\mathbf{z}_i, c_i, \kappa_i)$. In Vickrey auctions with scoring, bidders truthfully report their vector of $(c_i, \kappa_i)$, then are ranked by $\max_j s_j (\mathbf{z}_i) - c_i - \kappa_{ij}$.<sup>40</sup> The highest scoring bidders subject to the auction's quantity constraint are allocated the contract $\arg \max_j s_j (\mathbf{z}_i) - c_i - \kappa_{ij}$. Importantly, $s_j (\mathbf{z}_i)$ depends *only* on observable characteristics. Moreover, $s_j (\mathbf{z}_i)$ only includes immutable characteristics to avoid introducing perverse incentives to game the scoring rule. For these reasons, the scoring rule will *not* capture all heterogeneity in $\tau (\mathbf{z}_i, c_i, \kappa_i)$.

Vickrey auctions with scoring have both advantages and disadvantages. They isolate the design of the scoring rule by implementing an allocation that maximizes equation (18), this allocation is computationally cheap to compute, and the market designer needs only to compute $s_j(\mathbf{z}_i)$ and bidders need only to report their costs. At the same time, the VCG mechanism is infrequently used and features important practical weaknesses (Ausubel and Milgrom, 2005): it reveals a substantial amount of information, it can be expensive for the market designer, and it requires that bidders understand the incentives of an unfamiliar auction format.<sup>41</sup> Therefore, while our baseline counterfactuals implement Vickrey auctions with scoring, we will also assess the performance of alternative scoring rules in mechanisms currently in use by the USDA.

**Results: Vickrey Auction with Scoring** Figure 9 plots the social welfare gains (evaluated using equation (16)) under progressively improving Vickrey auctions with scoring. The auctions in bars (3)-(6) vary $s_j (\mathbf{z}_i)$, holding the number of awarded contracts fixed at the status quo quantity for each auction. Additional details are reported in the corresponding columns of Table 5.

Bar (3) maintains the status quo scoring rule, $s_j (\mathbf{z}_i) = B_j (\mathbf{z}_i^s)$, but changes the auction mechanism to VCG. This counterfactual isolates the impact of a scoring rule that is naive to additionality (bar (2) versus bar (3)) and provides a basis for further comparisons that change only the rule $s_j (\mathbf{z}_i)$ but hold constant the VCG auction mechanism. If all landowners were additional, the scoring rule defined by $s_j (\mathbf{z}_i) = B_j (\mathbf{z}_i^s)$ would implement the efficient allocation. Instead, it implements only 14% of social welfare gains under the efficient allocation.

Bars (4)-(6) in Figure 9 adjust the scoring rule $s_j (\mathbf{z}_i)$ defined in equation (17) based on predictions

<sup>40</sup>Appendix G provides more details, including the incentive payments that implement this allocation.

<sup>41</sup>However, because every bidder and every contract is a substitute, the Vickrey auctions with scoring do not exhibit many of the deficiencies discussed in Ausubel and Milgrom (2005). Moreover, the incentive payment for bidder $i$ and contract $j$ (if she wins the auction) is simple: $s_j (\mathbf{z}_i)$ net of the scoring rule implied social surplus of the marginal bidder.

25

of additionality. First, in bar (4), we adjust the scoring of the menu of contracts, $\omega_j$, to maximize equation (16), holding $\omega_{\mathbf{z}}$ constant at the status quo rule.<sup>42</sup> This change to the scoring rule more than doubles the social welfare gains of the auction (an increase of $156 million per auction). Next, in bars (5)-(6), we adjust the bidder asymmetry terms across observables, $\omega_{\mathbf{z}}$. Bar (5) first re-weights the existing characteristics in the scoring rule, $\mathbf{z}_i^s$, based also on additionality. The final adjustment (bar (6)) adds an additional characteristic to the rule: an "additionality factor" $\hat{\tau}(\mathbf{z}_i)$, which we calculate by projecting $\tau(\mathbf{z}_i, c_i, \kappa_i)$ on immutable characteristics of landowners already collected by the USDA but not all incorporated in the status quo scoring rule (deciles of soil productivity and wind and water erosion). Then, we calculate the social-welfare maximizing score using $\mathbf{z}_i^s$ and $\hat{\tau}(\mathbf{z}_i)$ added linearly as asymmetry terms in equation (17). This change only considers immutable characteristics. Other observables, e.g. prior land use, would yield further gains but introduce incentives to game the rule. Together, these changes to bidder asymmetry terms yield a further $53 million of social welfare gains per auction.

Beyond the allocation rule, additionality also impacts the socially-optimal size of the market. Because landowners are not all additional, the status quo quantity procured is *higher* than is socially-optimal. In bar (7), we use our alternative scoring rules to reduce the size of the market, keeping the scoring rule $s_j(\mathbf{z}_i)$ of bar (6) but rejecting landowners with $\max_j s_j(\mathbf{z}_i) - c_i - \kappa_{ij} < 0$. This reduction in market size increases social welfare substantially, by $91 million per auction.<sup>43</sup>

The alternative auctions in Figure 9 outperform the status quo by adjusting the scoring rule to reflect the social benefit of contracting. This occurs via two channels. First, the status quo rule $B_j(\mathbf{z}_i^s)$ over-weights asymmetry across characteristics $\mathbf{z}_i^s$ and contracts $j$. The heterogeneous social benefits of conservation $B_j(\mathbf{z}_i^s)$ are not fully realized when conservation would have counterfactually occurred. Second, bidders may be systematically heterogeneous in their additionality, which can be exploited in the scoring rule to align the allocation implemented by the auction with the socially-optimal allocation that considers heterogeneity in both additionality and costs.

Column (8) in Table 5 explores these two mechanisms further. Holding quantity constant, we report outcomes under a scoring rule $s_j(\mathbf{z}_i) = \theta \cdot B_j(\mathbf{z}_i^s)$ for the (single) multiplier $\theta$ chosen to maximize equation (16). This examines the social welfare gains achieved with only a uniform instrument to adjust the scoring rule for additionality, in comparison to auctions that adjust $\omega_j$ and $\omega_{\mathbf{z}}$ to reflect heterogeneously additional landowners (column (6)). Adjusting the scoring rule based on heterogeneity in additionality yields social welfare gains of $24 million per auction (column (6) vs. column (8)), but a uniform adjustment for additionality achieves a large share of the gains relative to the status quo. This highlights the importance of incorporating additionality to obtain the correct marginal rate of substitution between the social benefits and costs of each CRP contract.

\*<sup>42</sup>We solve for the $\omega_j$ that maximize equation (16) given simulations of landowner $(c_i, \kappa_i)$, estimates of $\tau(\mathbf{z}_i, c_i, \kappa_i)$, calibrations of $B_j(\mathbf{z}_i^s)$, and the allocation rule, holding $\omega_{\mathbf{z}}$ fixed at the status quo scoring rule.

\*<sup>43</sup>Further differences between bar (7) and bar (2) reflect (i) $\mathbf{z}_i$ that are not incorporated into the scoring rule to avoid perverse incentives to game the rule, (ii) private landowner costs in $\tau(\mathbf{z}_i, c_i, \kappa_i)$, and (iii) the functional form of equation (17) relative to $B_j(\mathbf{z}_i^s) \cdot \tau(\mathbf{z}_i, c_i, \kappa_i)$.

26

**Implementation in Practice** The budgetary implications of alternative auctions are also relevant in practice. Holding quantity fixed, Vickrey auctions with scoring require greater USDA spending than the status quo (see column (3) *versus* column (1) of Table 5). However, the auction with the greatest social welfare gains also *reduces* USDA spending relative to the status quo (column (7) of Table 5) because reducing the size of the market, and therefore total USDA spending, increases social welfare. Still, across auctions, government spending exceeds the value of environmental services procured, $\sum_{i} \sum_{j} B_{j}\left(\mathbf{z}_{i}^{s}\right) \cdot \tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right) \cdot x_{i j}$ due to the presence of adverse selection: the marginal landowner has a higher value of $\tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right)$ than the inframarginal landowner.

The USDA may be hesitant to adopt a new auction format, particularly if it increases spending at the current program size. In column (9) of Table 5, we therefore implement our preferred scoring rule (column (6)) in the status quo auction mechanism. We hold constant bidder beliefs about quantity procured and the number of bidders and iteratively update beliefs $G(S)$ (using our resampling procedure) and bidders' best responses (given $G(S)$).<sup>44</sup> Adjusting the scoring rule more than doubles social welfare gains relative to the status quo and does not increase USDA spending. This provides another useful comparison: holding fixed the scoring rule, the switch to VCG yields gains of only $20 million per auction (columns (3) *versus* column (1) of Table 5), while holding fixed the auction format, the alternative scoring rule yields gains over five times as large (columns (9) *versus* column (1)).

Our approach also assumes that the function $\tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right)$ is stable and that the distribution of costs is known. If either of these assumptions are violated, potential efficiency gains may be limited. To address this concern, we construct an alternative limited information scoring rule that can be computed using only observables and can be evaluated in a hold-out auction to assess stability (see Appendix G for details).<sup>45</sup> In contrast to our baseline approach, this alternative scoring rule estimates heterogeneity in additionality by observables, matching cross-tract heterogeneity in cropping rates pre-auction (Appendix Figure D.7) and RD coefficients across auctions (Table D.2). This allows us to use data on auctions pre-2016 to construct the scoring rule and evaluate its performance in the 2016 auction.<sup>46</sup> We use the administrative data in this exercise, instead of the remote sensing data, to further assess the stability of $\tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right)$ to the dataset used for estimation. Column (10) of Table 5 presents results, implementing the limited information scoring rule with a Vickrey auction with scoring, evaluated in 2016 using our estimates of $F_{c, \kappa \mid \mathbf{z}}$ and $\tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right)$. We estimate substantial social welfare gains, even under this limited information rule: $217 million, compared to status quo social welfare gains of $120 million on average and $91 million in 2016 (see Appendix Figure G.6). We also obtain accurate estimates of the socially-optimal market size under the limited information rule.<sup>47</sup>

\*<sup>44</sup>Appendix G provides details and also implements a series of budget-conscious posted-prices mechanisms, as used in other USDA programs including Continuous CRP (results reported in Appendix Table G.1).

\*<sup>45</sup>A motivation for this approach is that it could be feasibly implemented by the USDA for future auctions.

\*<sup>46</sup>The 2016 auction serves as our hold-out auction because estimation of $\tau\left(\mathbf{z}_{i}, c_{i}, \kappa_{i}\right)$ uses observations of rejected bidders from the 2016 auction (with the most rejected bidders).

\*<sup>47</sup>See Appendix Figure G.6, which also includes further comparisons between the performance of the limited

27

Finally, we assume the objective in equation (16), but the USDA may alternatively place higher weight on landowner surplus versus environmental objectives. We investigate this possibility by comparing the relative performance of our highest performing auction (bar (7)) versus the status quo, varying the relative weights on landowner surplus versus environmental services procured (net of expenditures). The status quo becomes rationalized as preferred to our highest performing auction when each dollar to landowners is worth more than three times a dollar spent toward conservation (see Appendix Figure G.4 for more detail).<sup>48</sup>

## VI. Conclusion

Questions related to additionality pose a central challenge to many domains of environmental market design. This paper combines data and theory to document this challenge, quantify its implications for social welfare, and evaluate alternative market designs in the largest auction mechanism for ecosystem services in the world. Linking satellite data to auction bids, we use a regression discontinuity design to demonstrate that only one quarter of marginal auction winners are additional. The lowest bidders are also more likely to counterfactually conserve, introducing adverse selection in the market. To quantify the implications of these facts and test possible remedies, we develop and estimate a joint model of bidding and land use that incorporates adverse selection on additionality.

A common approach to additionality is to define eligibility requirements that restrict who and what can trade; we propose a more flexible alternative. Because many markets will inevitably attract landowners who are with some probability not additional, allocation mechanisms should consider this dimension of heterogeneity in their design. We show how auctions can use scoring rules to cost-effectively impact conservation, selecting participants based on both expected additionality and costs, despite the fact that not all landowners in the market are additional. While our analysis focused exclusively on the Conservation Reserve Program and the design of its auction, additionality also plays a central role in the performance and design of offset markets. In these markets, demand and intermediaries interact with supply-side adverse selection, presenting an interesting and impactful avenue for future research.

## References

**Ackerberg, Daniel A.**, “A new use of importance sampling to reduce computational burden in simulation estimation,” *QME*, December 2009, *7* (4), 343–376.

**Agarwal, Nikhil and Paulo Somaini**, “Demand Analysis Using Strategic Reports: An Application to a School Choice Mechanism,” *Econometrica*, 2018, *86* (2), 391–444.

**— , Pearl Li, and Paulo Somaini**, “Identification using Revealed Preferences in Linearly Separable Models,” October 2023.

information and baseline rules in both the 2016 auction and in all auctions. Appendix Figure G.5 compares the limited information and baseline scoring rules.

\*48Alternatively, transfers may be costly, even given a quantity constraint. Appendix Figure G.3 evaluates social welfare with a cost of funds, motivated by the need to finance expenditures with distortionary taxation.

28

Akerlof, George A., “The Market for "Lemons": Quality Uncertainty and the Market nism,” The Quarterly Journal of Economics, 1970, 84 (3), 488–500.

Alix-Garcia, Jennifer and Daniel Millimet, “Remotely Incorrect? Accounting for Nonclassical Measurement Error in Satellite Data on Deforestation,” Journal of the Association of mental and Resource Economists, December 2022.

Alix-Garcia, Jennifer M., Katharine R. E. Sims, and Patricia Ya nez Pagans, “Only One Tree from Each Seed? Environmental Effectiveness and Poverty Alleviation in Mexico’s Payments for Ecosystem Services Program,” American Economic Journal: Economic Policy, November 2015, 7 (4), 1–40.

Allen, Arthur W. and Mark W. Vandever, “A national survey of Conservation Reserve gram (CRP) participants on environmental effects, wildlife issues, and vegetation management on program lands,” Technical Report 2003-0001, U.S. Geological Survey 2003.

and , “Conservation Reserve Program (CRP) contributions to wildlife habitat, management issues, challenges and policy choices–an annotated bibliography,” USGS Numbered Series 2012- 5066, U.S. Geological Survey, Reston, VA 2012.

Allen, Jason, Robert Clark, Brent Hickman, and Eric Richert, “Resolving Failed Banks: Uncertainty, Multiple Bidding, and Auction Design,” The Review of Economic Studies, 2023.

Anderson, Kevin, “The inconvenient truth of carbon offsets,” Nature, April 2012, 484 (7392), 7–7.

Aronoff, Daniel and Will Rafey, “Conservation priorities and environmental offsets: Markets for Florida Wetlands,” 2024.

Asker, John and Estelle Cantillon, “Properties of scoring auctions,” The RAND Journal of Economics, 2008, 39 (1), 69–85.

Ausubel, Lawrence M. and Paul Milgrom, “The Lovely but Lonely Vickrey Auction,” in Peter Cramton, Yoav Shoham, and Richard Steinberg, eds., Combinatorial Auctions, The MIT Press, December 2005, p. 0.

Bolotnyy, Valentin and Shoshana Vasserman, “Scaling Auctions as Insurance: A Case Study in Infrastructure Procurement,” Econometrica, 2023, 91 (4), 1205–1259.

Borrelli, Pasquale, David A. Robinson, Larissa R. Fleischer, Emanuele Lugato, tiano Ballabio, Christine Alewell, Katrin Meusburger, Sirio Modugno, Brigitta Schütt, Vito Ferro, Vincenzo Bagarello, Kristof Van Oost, Luca Montanarella, and Panos Panagos, “An assessment of the global impact of 21st century land use change on soil erosion,” Nature Communications, December 2017, 8 (1), 2013.

Bundorf, M. Kate, Jonathan Levin, and Neale Mahoney, “Pricing and Welfare in Health Plan Choice,” American Economic Review, December 2012, 102 (7), 3214–3248.

Calel, Raphael, Jonathan Colmer, Antoine Dechezleprêtre, and Matthieu Glachant, “Do Carbon Offsets Offset Carbon?,” American Economic Journal: Applied Economics, January 2025, 17 (1), 1–40.

Calonico, Sebastian, Matias D. Cattaneo, and Rocio Titiunik, “Robust Nonparametric Confidence Intervals for Regression-Discontinuity Designs,” Econometrica, 2014, 82 (6), 2295– 2326.

Carril, Rodrigo, Andres Gonzalez-Lira, and Michael S. Walker, “Competition under complete Contracts and the Design of Procurement Policies,” November 2024.

<page_number>29</page_number>

Che, Yeon-Koo, “Design Competition Through Multidimensional Auctions,” The RAND Journal of Economics, 1993, 24 (4), 668–680.

Chen, Xiaoyi, Nicholas Ryan, and Daniel Yi Xu, “Adverse Selection in Carbon Offset Markets: Evidence from the Clean Development Mechanism in China,” August 2024.

Chiappori, PierreâAndre and Bernard Salanie, “Testing for Asymmetric Information in surance Markets,” Journal of Political Economy, 2000, 108 (1), 56–78.

Claassen, Roger, Roger Horowitz, Eric Duquette, and Kohei Ueda, “Additionality in U.S. Agricultural Conservation and Regulatory Offset Programs,” Technical Report ERR-170, U.S. Department of Agriculture, Economic Research Service July 2014.

Decarolis, Francesco, “Awarding Price, Contract Performance, and Bids Screening: Evidence from Procurement Auctions,” American Economic Journal: Applied Economics, January 2014, 6 (1), 108–132.

Dirzo, Rodolfo, Hillary S. Young, Mauro Galetti, Gerardo Ceballos, Nick J. B. Isaac, and Ben Collen, “Defaunation in the Anthropocene,” Science, July 2014, 345 (6195), 401–406.

Einav, Liran, Amy Finkelstein, and Mark R. Cullen, “Estimating Welfare in Insurance Markets Using Variation in Prices,” The Quarterly Journal of Economics, August 2010, 125 (3), 877–921.

, , Yunan Ji, and Neale Mahoney, “Voluntary Regulation: Evidence from Medicare ment Reform,” The Quarterly Journal of Economics, February 2022, 137 (1), 565–618.

Engel, Stefanie, Stefano Pagiola, and Sven Wunder, “Designing payments for environmental services in theory and practice: An overview of the issues,” Ecological Economics, May 2008, 65 (4), 663–674.

FAPRI-MU, Estimating Water Quality, Air Quality, and Soil Carbon Benefits of the Conservation Reserve Program 2007.

Feather, Peter, Daniel Hellerstein, and LeRoy T. Hansen, “Economic Valuation of mental Benefits and the Targeting of Conservation Programs: The Case of the CRP,” Agricultural Economic Reports, 1999.

Friedlingstein, Pierre et al., “Global Carbon Budget 2022,” Earth System Science Data, ber 2022, 14 (11), 4811–4900.

Guerre, Emmanuel, Isabelle Perrigne, and Quang Vuong, “Optimal Nonparametric mation of First-Price Auctions,” Econometrica, 2000, 68 (3), 525–574.

Hansen, LeRoy, “Conservation Reserve Program: Environmental Benefits Update,” Agricultural and Resource Economics Review, October 2007, 36 (2), 267–280.

Harstad, Bård, “The market for conservation and other hostages,” Journal of Economic Theory, November 2016, 166, 124–151.

Hellerstein, Daniel M., “The US Conservation Reserve Program: The evolution of an enrollment mechanism,” Land Use Policy, April 2017, 63, 601–610.

Hendricks, Kenneth and Robert Porter, “An Empirical Study of an Auction with Asymmetric Information,” American Economic Review, 1988, 78 (5), 865–83.

Hortaçsu, Ali, “Mechanism Choice and Strategic Bidding in Divisible Good Auctions: An ical Analysis Of the Turkish Treasury Auction Market,” November 2000.

<page_number>30</page_number>

**\_\_\_ and David McAdams**, “Mechanism Choice and Strategic Bidding in Divisible Good Auctions: An Empirical Analysis of the Turkish Treasury Auction Market,” *Journal of Political Economy*, October 2010, *118* (5), 833–865.

**Ito, Koichiro, Takanori Ida, and Makoto Tanaka**, “Selection on Welfare Gains: Experimental Evidence from Electricity Plan Choice,” *American Economic Review*, November 2023, *113* (11), 2937–2973.

**Jack, B. Kelsey**, “Private Information and the Allocation of Land Use Subsidies in Malawi,” *American Economic Journal: Applied Economics*, July 2013, *5* (3), 113–135.

**\_\_\_ and Seema Jayachandran**, “Self-selection into payments for ecosystem services programs,” *Proceedings of the National Academy of Sciences*, March 2019, *116* (12), 5326–5333.

**Jayachandran, Seema, Joost de Laat, Eric F. Lambin, Charlotte Y. Stanton, Robin Audy, and Nancy E. Thomas**, “Cash for carbon: A randomized trial of payments for ecosystem services to reduce deforestation,” *Science*, July 2017, *357* (6348), 267–273.

**Johnson, Kris A., Brent J. Dalzell, Marie Donahue, Jesse Gourevitch, Dennis L. Johnson, Greg S. Karlovits, Bonnie Keeler, and Jason T. Smith**, “Conservation Reserve Program (CRP) lands provide ecosystem service benefits that exceed land rental payment costs,” *Ecosystem Services*, April 2016, *18*, 175–185.

**Jones, Julia P. G. and Simon L. Lewis**, “Forest carbon offsets are failing,” *Science*, August 2023, *381* (6660), 830–831.

**Kinzig, A. P., C. Perrings, F. S. Chapin, S. Polasky, V. K. Smith, D. Tilman, and B. L. Turner**, “Paying for Ecosystem Services-Promise and Peril,” *Science*, November 2011, *334* (6056), 603–604.

**Kong, Yunmi, Isabelle Perrigne, and Quang Vuong**, “Multidimensional Auctions of Contracts: An Empirical Analysis,” *American Economic Review*, May 2022, *112* (5), 1703–1736.

**Lark, Tyler J., Ian H. Schelly, and Holly K. Gibbs**, “Accuracy, Bias, and Improvements in Mapping Crops and Cropland across the United States Using the USDA Cropland Data Layer,” *Remote Sensing*, January 2021, *13* (5), 968.

**\_\_\_, Richard M. Mueller, David M. Johnson, and Holly K. Gibbs**, “Measuring land-use and land-cover change using the U.S. department of agriculture’s cropland data layer: Cautions and recommendations,” *International Journal of Applied Earth Observation and Geoinformation*, October 2017, *62*, 224–235.

**Lewis, Gregory and Patrick Bajari**, “Procurement Contracting With Time Incentives: Theory and Evidence,” *The Quarterly Journal of Economics*, August 2011, *126* (3), 1173–1211.

**Li, Wanyi Dai, Itai Ashlagi, and Irene Lo**, “Simple and Approximately Optimal Contracts for Payment for Ecosystem Services,” *Management Science*, February 2022.

**Lopomo, Giuseppe, Nicola Persico, and Alessandro T. Villa**, “Optimal Procurement With Quality Concerns,” *American Economic Review*, 2023.

**Manelli, Alejandro M. and Daniel R. Vincent**, “Optimal Procurement Mechanisms,” *Econometrica*, 1995, *63* (3), 591–620.

**Marone, Victoria R. and Adrienne Sabety**, “When Should There Be Vertical Choice in Health Insurance Markets?,” *American Economic Review*, January 2022, *112* (1), 304–342.

31

Mason, Charles and Andrew J. Plantinga, “The additionality problem with offsets: Optimal contracts for carbon sequestration in forests,” Journal of Environmental Economics and ment, 2013, 66 (1), 1–14.

Montero, Juan-Pablo, “Voluntary Compliance with Market-Based Environmental Policy: dence from the U.S. Acid Rain Program,” Journal of Political Economy, October 1999, 107 (5), 998–1033.

Myerson, Roger B., “Optimal Auction Design,” Mathematics of Operations Research, 1981, 6 (1), 58–73.

Ribaudo, Marc O., Dana L. Hoag, Mark E. Smith, and Ralph Heimlich, “Environmental indices and the politics of the Conservation Reserve Program,” Ecological Indicators, August 2001, 1 (1), 11–20.

Roberts, Michael J. and Ruben N. Lubowski, “Enduring Impacts of Land Retirement Policies: Evidence from the Conservation Reserve Program,” Land Economics, November 2007, 83 (4), 516–538.

Rosenberg, Andrew B. and Bryan Pratt, “Land use impacts of the Conservation Reserve Program: An analysis of rejected offers,” American Journal of Agricultural Economics, May 2024, 106 (3).

Salzman, James, Genevieve Bennett, Nathaniel Carroll, Allie Goldstein, and Michael Jenkins, “The global status and trends of Payments for Ecosystem Services,” Nature ability, March 2018, 1 (3), 136–144.

Torchiana, Adrian L., Ted Rosenbaum, Paul T. Scott, and Eduardo Souza-Rodrigues, “Improving Estimates of Transitions from Satellite Data: A Hidden Markov Model Approach,” The Review of Economics and Statistics, December 2024, pp. 1–16.

van Benthem, Arthur and Suzi Kerr, “Scale and transfers in international emissions offset programs,” Journal of Public Economics, November 2013, 107, 31–46.

Vörösmarty, C. J., P. B. McIntyre, M. O. Gessner, D. Dudgeon, A. Prusevich, P. Green, S. Glidden, S. E. Bunn, C. A. Sullivan, C. Reidy Liermann, and P. M. Davies, “Global threats to human water security and river biodiversity,” Nature, September 2010, 467 (7315), 555–561.

West, Thales A. P., Jan Börner, Erin O. Sills, and Andreas Kontoleon, “Overstated carbon emission reductions from voluntary REDD+ projects in the Brazilian Amazon,” Proceedings of the National Academy of Sciences, September 2020, 117 (39), 24188–24194.

, Sven Wunder, Erin O. Sills, Jan Börner, Sami W. Rifai, Alexandra N. Neidermeier, Gabriel P. Frey, and Andreas Kontoleon, “Action needed to make carbon offsets from forest conservation work for climate change mitigation,” Science, August 2023, 381 (6660), 873–877.

White, Douglas and Peter Minang, “Estimating the Opportunity Costs of REDD+: A Training Manual,” Technical Report March 2011.

<page_number>32</page_number>

# Figures and Tables

## Figure 1: Graphical Analysis

(a) Efficient allocation can be implemented      (b) Efficient allocation cannot be implemented


<table>
  <thead>
    <tr>
        <th>Point/Region</th>
        <th>Y-axis Label</th>
        <th>X-axis Label</th>
        <th>Description</th>
    </tr>
    <tr>
        <th>B</th>
        <th>B</th>
        <th> </th>
        <th>Y-intercept/Price level</th>
    </tr>
    <tr>
        <th>F</th>
        <th>F</th>
        <th>qB</th>
        <th>Point on MC curve</th>
    </tr>
    <tr>
        <th>G</th>
        <th>G</th>
        <th>qB</th>
        <th>Point on B · τ curve</th>
    </tr>
    <tr>
        <th>E</th>
        <th>p*</th>
        <th>q*</th>
        <th>Intersection of MC and B · τ</th>
    </tr>
    <tr>
        <th>C</th>
        <th>C</th>
        <th>0</th>
        <th>Y-intercept of B · τ</th>
    </tr>
    <tr>
        <th>D</th>
        <th>D</th>
        <th>0</th>
        <th>Y-intercept of MC</th>
    </tr>
    <tr>
        <th>Social welfare losses</th>
        <th> </th>
        <th> </th>
        <th>Triangle EFG</th>
    </tr>
    <tr>
        <th>Social welfare gains</th>
        <th> </th>
        <th> </th>
        <th>Area between B · τ and MC from 0 to q*</th>
    </tr>
  </thead>
</table>
<table>
  <thead>
    <tr>
        <th>Point/Region</th>
        <th>Y-axis Label</th>
        <th>X-axis Label</th>
        <th>Description</th>
    </tr>
    <tr>
        <th>B</th>
        <th>B</th>
        <th> </th>
        <th>Y-intercept level</th>
    </tr>
    <tr>
        <th>F</th>
        <th>F</th>
        <th> </th>
        <th>Point on MC curve</th>
    </tr>
    <tr>
        <th>G</th>
        <th>G</th>
        <th> </th>
        <th>Point on B · τ curve</th>
    </tr>
    <tr>
        <th>E</th>
        <th> </th>
        <th> </th>
        <th>Intersection of MC and B · τ</th>
    </tr>
    <tr>
        <th>C</th>
        <th>C</th>
        <th>0</th>
        <th>Y-intercept of B · τ</th>
    </tr>
    <tr>
        <th>D</th>
        <th>p* / D</th>
        <th>0</th>
        <th>Origin/Y-intercept of MC</th>
    </tr>
    <tr>
        <th>Social welfare gains</th>
        <th> </th>
        <th> </th>
        <th>Area between B · τ and MC (upper section)</th>
    </tr>
    <tr>
        <th>Social welfare losses</th>
        <th> </th>
        <th> </th>
        <th>Area between MC and B · τ (lower section)</th>
    </tr>
  </thead>
</table>


Notes: Figure describes markets characterized by marginal cost ($MC = F_{C}^{-1}(q)$) and contract value ($B \cdot \tau$) curves. The horizontal axis is the share of the population ordered by costs $c_{i}$. $B$ denotes the social benefits of $a_{i1} = 1$. $B \cdot \tau$ denotes the incremental social benefits of contracting, relative to no contract, at each quantile of landowner costs. The vertical distance between the $B \cdot \tau$ and $MC$ curves represents the social surplus from contracting. Upwards-sloping $B \cdot \tau$ curves illustrate markets with adverse selection. Panel (a) documents a population distribution in which the efficient allocation (defined in equation (5)) can be implemented with the socially optimal uniform price $p^{*}$. In panel (a), social surplus is monotonically decreasing in cost $c_{i}$. In panel (b), social surplus is not monotonically decreasing in cost $c_{i}$. Panel (b) documents a population distribution in which the efficient allocation (defined in equation (5)) cannot be implemented. In panel (b), the socially optimal price is $p^{*} = 0$ (no market). Panel (a) also demonstrates the social welfare losses from mis-pricing (at $B$) (triangle $EFG$).

33

# Figure 2: Regression Discontinuity Validity and First Stage


### (a) Histogram of running variable

<table>
  <thead>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Density</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-200</td>
        <td>0</td>
    </tr>
    <tr>
        <td>-190</td>
        <td>0.0001</td>
    </tr>
    <tr>
        <td>-180</td>
        <td>0.0002</td>
    </tr>
    <tr>
        <td>-170</td>
        <td>0.0003</td>
    </tr>
    <tr>
        <td>-160</td>
        <td>0.0004</td>
    </tr>
    <tr>
        <td>-150</td>
        <td>0.0006</td>
    </tr>
    <tr>
        <td>-140</td>
        <td>0.0008</td>
    </tr>
    <tr>
        <td>-130</td>
        <td>0.0011</td>
    </tr>
    <tr>
        <td>-120</td>
        <td>0.0014</td>
    </tr>
    <tr>
        <td>-110</td>
        <td>0.0018</td>
    </tr>
    <tr>
        <td>-100</td>
        <td>0.0022</td>
    </tr>
    <tr>
        <td>-90</td>
        <td>0.0027</td>
    </tr>
    <tr>
        <td>-80</td>
        <td>0.0032</td>
    </tr>
    <tr>
        <td>-70</td>
        <td>0.0038</td>
    </tr>
    <tr>
        <td>-60</td>
        <td>0.0045</td>
    </tr>
    <tr>
        <td>-50</td>
        <td>0.0052</td>
    </tr>
    <tr>
        <td>-40</td>
        <td>0.0060</td>
    </tr>
    <tr>
        <td>-30</td>
        <td>0.0068</td>
    </tr>
    <tr>
        <td>-20</td>
        <td>0.0075</td>
    </tr>
    <tr>
        <td>-10</td>
        <td>0.0082</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.0088</td>
    </tr>
    <tr>
        <td>10</td>
        <td>0.0092</td>
    </tr>
    <tr>
        <td>20</td>
        <td>0.0095</td>
    </tr>
    <tr>
        <td>30</td>
        <td>0.0096</td>
    </tr>
    <tr>
        <td>40</td>
        <td>0.0095</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.0092</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.0088</td>
    </tr>
    <tr>
        <td>70</td>
        <td>0.0082</td>
    </tr>
    <tr>
        <td>80</td>
        <td>0.0075</td>
    </tr>
    <tr>
        <td>90</td>
        <td>0.0068</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.0060</td>
    </tr>
    <tr>
        <td>110</td>
        <td>0.0052</td>
    </tr>
    <tr>
        <td>120</td>
        <td>0.0045</td>
    </tr>
    <tr>
        <td>130</td>
        <td>0.0038</td>
    </tr>
    <tr>
        <td>140</td>
        <td>0.0032</td>
    </tr>
    <tr>
        <td>150</td>
        <td>0.0027</td>
    </tr>
    <tr>
        <td>160</td>
        <td>0.0022</td>
    </tr>
    <tr>
        <td>170</td>
        <td>0.0018</td>
    </tr>
    <tr>
        <td>180</td>
        <td>0.0014</td>
    </tr>
    <tr>
        <td>190</td>
        <td>0.0011</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.0008</td>
    </tr>
    <tr>
        <td>210</td>
        <td>0.0006</td>
    </tr>
    <tr>
        <td>220</td>
        <td>0.0004</td>
    </tr>
    <tr>
        <td>230</td>
        <td>0.0003</td>
    </tr>
    <tr>
        <td>240</td>
        <td>0.0002</td>
    </tr>
    <tr>
        <td>250</td>
        <td>0.0001</td>
    </tr>
    <tr>
        <td>260</td>
        <td>0</td>
    </tr>
  </tbody>
</table>



### (b) Placebo: pre-auction

<table>
  <thead>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Share of land cropped</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-25</td>
        <td>0.25</td>
    </tr>
    <tr>
        <td>-23</td>
        <td>0.23</td>
    </tr>
    <tr>
        <td>-21</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>-19</td>
        <td>0.27</td>
    </tr>
    <tr>
        <td>-17</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>-15</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>-13</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>-11</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>-9</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>-7</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>-5</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>-3</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.23</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>11</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>13</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>15</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>17</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>19</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>21</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>23</td>
        <td>0.20</td>
    </tr>
  </tbody>
</table>



### (c) First stage: contracting

<table>
  <thead>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Share with CRP contract</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-25</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-23</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-21</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-19</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-17</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-15</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-13</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-11</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-9</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-7</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-5</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-3</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.88</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.85</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.84</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.87</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.84</td>
    </tr>
    <tr>
        <td>11</td>
        <td>0.85</td>
    </tr>
    <tr>
        <td>13</td>
        <td>0.87</td>
    </tr>
    <tr>
        <td>15</td>
        <td>0.84</td>
    </tr>
    <tr>
        <td>17</td>
        <td>0.85</td>
    </tr>
    <tr>
        <td>19</td>
        <td>0.86</td>
    </tr>
    <tr>
        <td>21</td>
        <td>0.87</td>
    </tr>
    <tr>
        <td>23</td>
        <td>0.88</td>
    </tr>
  </tbody>
</table>



### (d) First stage: share of land contracted

<table>
  <thead>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Share of land with CRP contract</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-25</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-23</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>-21</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-19</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-17</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-15</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-13</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-11</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-9</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-7</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-5</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-3</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.36</td>
    </tr>
    <tr>
        <td>11</td>
        <td>0.39</td>
    </tr>
    <tr>
        <td>13</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>15</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>17</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>19</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>21</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>23</td>
        <td>0.38</td>
    </tr>
  </tbody>
</table>


Notes: Panel (a) presents a histogram of bidders’ scores in the auction relative to that auction’s win threshold, $S_{ig} - S_g$, pooled across auctions in the regression discontinuity sample. This is the running variable for the regression discontinuity design: bidders above zero win the auction. Panels (b)-(d) present raw data and estimates from equation (7). Panel (b) is estimated for $r(i, t) \le 0$ (pre-auction), and panels (c) and (d) are estimated for $r(i, t) > 0$ (post-auction). The outcome in panel (b) is the share of the bidder’s land that is cropped, measured in the remote sensing data. The outcome in panel (c) is an indicator for a bidder obtaining a CRP contract, and the outcome in panel (d) is the share of a bidder’s land covered by a CRP contract. Positive numbers on the x-axis correspond to winning scores, negative numbers correspond to losing scores. In panel (a), each observation is a bidder, in panels (b)-(d), each observation is a bidder-year.

34

Figure 3: The Effect of a CRP Contract on Land Use


Figure 3: The Effect of a CRP Contract on Land Use
(a) Share of land cropped	(b) Share of land in natural vegetation
<table>
  <thead>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Share of land cropped</th>
        <th>Score relative to win threshold</th>
        <th>Share of land in natural vegetation</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-20</td>
        <td>0.31</td>
        <td>-20</td>
        <td>0.56</td>
    </tr>
    <tr>
        <td>-15</td>
        <td>0.30</td>
        <td>-15</td>
        <td>0.57</td>
    </tr>
    <tr>
        <td>-10</td>
        <td>0.30</td>
        <td>-10</td>
        <td>0.57</td>
    </tr>
    <tr>
        <td>-5</td>
        <td>0.30</td>
        <td>-5</td>
        <td>0.57</td>
    </tr>
    <tr>
        <td>0 (Loss)</td>
        <td>0.30</td>
        <td>0 (Loss)</td>
        <td>0.57</td>
    </tr>
    <tr>
        <td>0 (Win)</td>
        <td>0.22</td>
        <td>0 (Win)</td>
        <td>0.66</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.22</td>
        <td>5</td>
        <td>0.66</td>
    </tr>
    <tr>
        <td>10</td>
        <td>0.22</td>
        <td>10</td>
        <td>0.66</td>
    </tr>
    <tr>
        <td>15</td>
        <td>0.22</td>
        <td>15</td>
        <td>0.66</td>
    </tr>
    <tr>
        <td>20</td>
        <td>0.21</td>
        <td>20</td>
        <td>0.67</td>
    </tr>
  </tbody>
</table>


Notes: Panels (a) and (b) present raw data and estimates from equation (7) for $r(i, t) > 0$ (post-auction). Outcomes are the share of the bidder’s land that is cropped (a) and the share of the bidder’s land that is in natural vegetation (trees, grassland, shrubs, and wetlands) (b), both measured in the remote sensing data. The running variable is the difference between each bidder’s score and the threshold score. Positive numbers on the x-axis correspond to winning scores, negative numbers correspond to losing scores. Each observation is a bidder-year. Appendix Figure D.3 provides corresponding figures with outcomes measured in the administrative data. Corresponding coefficient estimates and standard errors presented in Table 2.

Figure 4: Regression Discontinuity Estimates of Additionality


<table>
  <thead>
    <tr>
        <th>Relative year</th>
        <th>Remote sensing</th>
        <th>Admin</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-5</td>
        <td>-0.02</td>
        <td>-0.01</td>
    </tr>
    <tr>
        <td>-4</td>
        <td>-0.01</td>
        <td>-0.01</td>
    </tr>
    <tr>
        <td>-3</td>
        <td>-0.01</td>
        <td>0.00</td>
    </tr>
    <tr>
        <td>-2</td>
        <td>0.00</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>-0.01</td>
        <td>0.00</td>
    </tr>
    <tr>
        <td>0</td>
        <td>-0.01</td>
        <td>0.00</td>
    </tr>
    <tr>
        <td>1</td>
        <td>-0.04</td>
        <td>-0.05</td>
    </tr>
    <tr>
        <td>2</td>
        <td>-0.07</td>
        <td>-0.08</td>
    </tr>
    <tr>
        <td>3</td>
        <td>-0.08</td>
        <td>-0.09</td>
    </tr>
    <tr>
        <td>4</td>
        <td>-0.08</td>
        <td>-0.10</td>
    </tr>
    <tr>
        <td>5</td>
        <td>-0.09</td>
        <td>-0.10</td>
    </tr>
    <tr>
        <td>6</td>
        <td>-0.09</td>
        <td>-0.11</td>
    </tr>
    <tr>
        <td>7</td>
        <td>-0.10</td>
        <td>-0.11</td>
    </tr>
    <tr>
        <td>8</td>
        <td>-0.10</td>
        <td>-0.11</td>
    </tr>
    <tr>
        <td>9</td>
        <td>-0.10</td>
        <td>-0.12</td>
    </tr>
    <tr>
        <td>10</td>
        <td>-0.11</td>
        <td>-0.13</td>
    </tr>
  </tbody>
</table>


Notes: Figure plots coefficient estimates from equation (6). The outcome is the share of each bidder’s land that is cropped, measured with both remote sensing and administrative datasets. The x-axis is the year relative to the year of each bidder’s auction: $r(i, t) = t - t_{g(i)}$. Positive years correspond to post-auction years. Each point is a regression discontinuity coefficient. Estimates pool data across all auctions with post-period land use data (occurring between 2010 and 2016); see Table D.2 for auction-specific estimates. The relative-year coefficient estimates pool different auctions based on the coverage of the land use panel. Dashed lines indicate the pooled post-auction treatment effects (equation (7) estimated for $r(i, t) > 0$). The line labeled $\tau = 1$ represents a benchmark where all landowners are additional. This is calculated as the share of land contracting in the MSE-optimal bandwidth (Calonico et al., 2014) used to estimate the RD. Each observation is a bidder-year. Standard errors are clustered at the bidder level. Ten years is the full duration of a standard CRP contract. Corresponding coefficient estimates and standard errors presented in Table 2.

35

# Figure 5: Testing for Asymmetric Information

### (a) Additionality vs. bids


<table>
  <thead>
    <tr>
        <th>Bid ($/acre/year)</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>25</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>38</td>
        <td>0.14</td>
    </tr>
    <tr>
        <td>45</td>
        <td>0.15</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.16</td>
    </tr>
    <tr>
        <td>55</td>
        <td>0.16</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>68</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>78</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>90</td>
        <td>0.27</td>
    </tr>
    <tr>
        <td>108</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>120</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>135</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>148</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>155</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>170</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>188</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>202</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>212</td>
        <td>0.43</td>
    </tr>
    <tr>
        <td>235</td>
        <td>0.45</td>
    </tr>
  </tbody>
</table>


### (b) Additionality vs. bids | observables


<table>
  <thead>
    <tr>
        <th>Bid ($/acre/year)</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>35</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>40</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>45</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>52</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>55</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>70</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>80</td>
        <td>0.24</td>
    </tr>
    <tr>
        <td>98</td>
        <td>0.29</td>
    </tr>
    <tr>
        <td>115</td>
        <td>0.30</td>
    </tr>
    <tr>
        <td>132</td>
        <td>0.31</td>
    </tr>
    <tr>
        <td>145</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>158</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>175</td>
        <td>0.36</td>
    </tr>
    <tr>
        <td>195</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>210</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>225</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>240</td>
        <td>0.41</td>
    </tr>
  </tbody>
</table>


### (c) Additionality across contracts


<table>
  <thead>
    <tr>
        <th>Contract Type</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Native grasses</td>
        <td>-0.02</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>-0.22</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>-0.05</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>Food plot</td>
        <td>-0.04</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>0.01</td>
    </tr>
  </tbody>
</table>


### (d) Observable predictors of additionality


<table>
  <thead>
    <tr>
        <th>Decile</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>2</td>
        <td>0.09</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.13</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.15</td>
    </tr>
    <tr>
        <td>6</td>
        <td>0.17</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>8</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>10</td>
        <td>0.37</td>
    </tr>
  </tbody>
</table>


*Notes*: Figures present estimates from regression specifications in equation (8). Regressions in all panels control for landowner characteristics in the scoring rule: whether a bidder is in a wildlife priority zone, estimates of groundwater quality, estimates of surface water quality, estimates of wind and water erosion (deciles), air quality impacts, and whether or not a bidder is in an air quality zone. The outcome variable in all panels is a landowner-specific measure of additionality, calculated as the share of all fields bid into the CRP mechanism that are cropped post auction for rejected landowners. The sample is restricted to the 2016 auction, in which 82% of bidders are rejected and the delineations of bid fields are observed. Cropping on bid fields is measured in 2017-2020 in the remote sensing data (see Figure D.5 for corresponding figures using the administrative data), with each field and year equally weighted. Panel (a) is a binned scatterplot correlating the dollar bid (per acre, per year) with additionality, conditional on characteristics included in the scoring rule. Panel (b) adds controls for interaction terms of prior land use (quartiles of share of land cropped prior to bidding and re-enrolling CRP status) and deciles of estimated soil productivity. Panel (c) plots relative additionality by the chosen contract in the bid, relative to an omitted category of introduced grasses, conditional on characteristics included in the scoring rule. Panel (d) plots relative additionality by deciles of estimated soil productivity, conditional on characteristics included in the scoring rule. Standard errors clustered at the bidder level.

36

# Figure 6: Estimated Landowner Base Costs


(a) Base cost $c_i$

<table>
  <tbody>
    <tr>
        <td>Simulated ci</td>
        <td>Density</td>
    </tr>
    <tr>
        <td>-100</td>
        <td>0</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.001</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.015</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.006</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.002</td>
    </tr>
    <tr>
        <td>300</td>
        <td>0.0005</td>
    </tr>
    <tr>
        <td>400</td>
        <td>0</td>
    </tr>
  </tbody>
</table>



(b) Land use vs. revealed preference cost estimates

<table>
  <tbody>
    <tr>
        <td>Simulated ci | observables</td>
        <td>Share cropped absent contract (1 - ai0)</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.24</td>
    </tr>
    <tr>
        <td>25</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>35</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>40</td>
        <td>0.23</td>
    </tr>
    <tr>
        <td>45</td>
        <td>0.26</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.26</td>
    </tr>
    <tr>
        <td>55</td>
        <td>0.25</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.27</td>
    </tr>
    <tr>
        <td>65</td>
        <td>0.28</td>
    </tr>
    <tr>
        <td>70</td>
        <td>0.30</td>
    </tr>
    <tr>
        <td>80</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>90</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>115</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>130</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>150</td>
        <td>0.36</td>
    </tr>
    <tr>
        <td>185</td>
        <td>0.36</td>
    </tr>
  </tbody>
</table>


*Notes:* Panel (a) presents a kernel density plot of estimates of the base cost $c_i$ of a CRP contract, pooling bidders across auctions. Panel (b) correlates base costs, $c_i$, conditional on observable characteristics $\mathbf{z}_i$, with land use outcomes measuring landowner additionality in the remote sensing data. Panel (b) is restricted to the 2016 auction and the 82% of bidders who lose. $\mathbf{z}_i$ includes interactions of soil productivity, prior CRP, and prior land use. Costs are reported in dollars per acre per year.

# Figure 7: Empirical Graphical Analysis


<table>
  <tbody>
    <tr>
        <td>q</td>
        <td>MC</td>
        <td>B</td>
        <td>B · τ</td>
        <td>E[B · τ | MC ≤ p]</td>
    </tr>
    <tr>
        <td>0</td>
        <td>50</td>
        <td>240</td>
        <td>40</td>
        <td>40</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>60</td>
        <td>240</td>
        <td>50</td>
        <td>45</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>75</td>
        <td>240</td>
        <td>65</td>
        <td>55</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>100</td>
        <td>240</td>
        <td>85</td>
        <td>70</td>
    </tr>
    <tr>
        <td>0.7</td>
        <td>120</td>
        <td>240</td>
        <td>100</td>
        <td>80</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>150</td>
        <td>240</td>
        <td>120</td>
        <td>90</td>
    </tr>
    <tr>
        <td>1.0</td>
        <td>220</td>
        <td>240</td>
        <td>160</td>
        <td>110</td>
    </tr>
  </tbody>
</table>


*Notes:* Figure presents the empirical version of Figure 1 for the base contract. The horizontal axis is the share of the population, ordered by costs of accepting a base CRP contract. Values are reported in dollars per acre per year. The *MC* curve is the inverse distribution function of the minimum cost to fulfill the base contract. *B* denotes the average value of the base contract action, calculated as described in Appendix F. $B \cdot \tau$ denotes the incremental value of contracting, relative to no contract, averaged at each quantile of the population distribution of the base cost. The vertical distance between the $B \cdot \tau$ and *MC* curves represents the social surplus from contracting at each quantile of the population distribution of costs. The upwards-sloping $B \cdot \tau$ curve illustrates the presence of adverse selection in the market. To illustrate adverse selection, figure also plots the average contract value curve (calculated as the average $B \cdot \tau$ of all landowners selecting into the market at any given price $p$, $\mathbb{E}[B \cdot \tau | MC \leq p]$). Both $B \cdot \tau$ and $\mathbb{E}[B \cdot \tau | MC \leq p]$ graphed with local polynomial regressions. The intersection of the *MC* and $B \cdot \tau$ curve denotes the socially-optimal uniform price, $p^*$. Triangle CDG represents social welfare gains under the socially-optimal price. The triangle GHI represents social welfare losses from mis-pricing at *B*.

37

Figure 8: Empirical Graphical Analysis: Heterogeneity Across Contracts and Observables

(a) Tree planting and maintenance contracts


<table>
    <tr>
        <th>Panel</th>
        <th>q</th>
        <th>MC ($ per acre-year)</th>
        <th>B · τ ($ per acre-year)</th>
        <th>Notes</th>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>0</td>
        <td>~20</td>
        <td>~40</td>
        <td>Point C</td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>0.2</td>
        <td>~40</td>
        <td>~50</td>
        <td></td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>0.4</td>
        <td>~60</td>
        <td>~65</td>
        <td>Point D</td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>0.6</td>
        <td>~90</td>
        <td>~80</td>
        <td></td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>q* (~0.65)</td>
        <td>p* (~100)</td>
        <td>~85</td>
        <td>Point E</td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>0.8</td>
        <td>~140</td>
        <td>~90</td>
        <td></td>
    </tr>
    <tr>
        <td>(a) Tree planting and maintenance</td>
        <td>1.0</td>
        <td>~250</td>
        <td>~100</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>0</td>
        <td>~10</td>
        <td>~40</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>0.2</td>
        <td>~20</td>
        <td>~45</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>0.4</td>
        <td>~30</td>
        <td>~50</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>0.6</td>
        <td>~40</td>
        <td>~55</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>0.8</td>
        <td>~55</td>
        <td>~60</td>
        <td></td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>q* (~0.9)</td>
        <td>p* (~70)</td>
        <td>~70</td>
        <td>Intersection</td>
    </tr>
    <tr>
        <td>(b) Below median soil productivity</td>
        <td>1.0</td>
        <td>~120</td>
        <td>~75</td>
        <td></td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>0</td>
        <td>~10</td>
        <td>~60</td>
        <td></td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>0.2</td>
        <td>~40</td>
        <td>~65</td>
        <td></td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>0.4</td>
        <td>~70</td>
        <td>~70</td>
        <td>Intersection (q*, p*)</td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>0.6</td>
        <td>~100</td>
        <td>~75</td>
        <td></td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>0.8</td>
        <td>~140</td>
        <td>~80</td>
        <td></td>
    </tr>
    <tr>
        <td>(c) Above median soil productivity</td>
        <td>1.0</td>
        <td>~230</td>
        <td>~85</td>
        <td></td>
    </tr>
</table>
Notes: Figure presents the empirical version of Figure 1. In panel (a), the MC curve is calculated as the inverse distribution function of the minimum cost to fulfill a tree planting and maintenance contract. The horizontal axis is the share of the population, ordered by costs of accepting a tree planting and maintenance contract. In panels (b) and (c), the MC curve is calculated as the inverse distribution function of the minimum cost to fulfill the base contract, split by whether landowners are below (b) or above (c) median estimated soil productivity. The horizontal axis in panels (b) and (c) is the share of the population, ordered by costs of accepting the base contract, within these sub-populations. Values are reported in dollars per acre per year. B · τ denotes the incremental value of contracting, relative to no contract, averaged at each quantile of the population distribution of cost of accepting a contract. The vertical distance between the B · τ and MC curves represents the social surplus from contracting at each quantile of the population distribution of costs. The upwards-sloping B · τ curve illustrates the presence of adverse selection in the market. B · τ graphed with local polynomial regressions. In panel (a), the efficient allocation defined in equation (5) cannot be implemented. In panels (b) and (c), the intersection of the MC and B · τ curve denotes the socially-optimal uniform price, p∗.

38

Figure 9: Social Welfare Under Alternative Auctions


<table>
  <thead>
    <tr>
        <th>Auction Type</th>
        <th>Social welfare gains per auction (million $)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>(1) Status quo</td>
        <td>120</td>
    </tr>
    <tr>
        <td>(2) Efficient allocation (not implementable)</td>
        <td>984</td>
    </tr>
    <tr>
        <td colspan="2">Vickrey auctions with scoring</td>
    </tr>
    <tr>
        <td>(3) Status quo scoring rule</td>
        <td>140</td>
    </tr>
    <tr>
        <td>(4) Re-weight contract incentives (ωⱼ)</td>
        <td>296</td>
    </tr>
    <tr>
        <td>(5) Re-weight zˢ (ωz)</td>
        <td>333</td>
    </tr>
    <tr>
        <td>(6) Add additional predictor τ(z)</td>
        <td>349</td>
    </tr>
    <tr>
        <td>(7) Reduce quantity</td>
        <td>440</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure presents estimates of the social welfare gains (defined in equation (16)) under status quo and alternative auctions. Results reported in million dollars per auction. All auctions impose that each landowner obtains at most one contract and that total contracts awarded cannot exceed the status quo, in each auction. Bar (1) simulates the status quo. Bar (2) calculates the social welfare gains under an efficient allocation that allocates contracts using all **z**<sub>i</sub> and (c<sub>i</sub>, κ<sub>i</sub>) to maximize equation (16). Due to adverse selection, this allocation may not be implementable. Bars (3)-(7) calculate social welfare under alternative Vickrey auctions with scoring (see Section V. for more details). Bars (3)-(6) hold quantity (the number of landowners allocated contracts) constant at the status quo and change the scoring rule s<sub>j</sub>(**z**<sub>i</sub>) defined in equation (17). Bar (3) uses the existing scoring rule s<sub>j</sub>(**z**<sub>i</sub>) = B<sub>j</sub> (**z**<sub>i</sub><sup>s</sup>). Bar (4) uses a scoring rule with the social-surplus maximizing incentives across contracts (ω<sub>j</sub>). Bar (5) uses a scoring rule with the social-surplus maximizing asymmetry terms across bidders using characteristics already in the scoring rule (**z**<sub>i</sub><sup>s</sup>). Bar (6) adds an additional characteristic to the scoring rule, a prediction of τ(z<sub>i</sub>, c<sub>i</sub>, κ<sub>i</sub>) based on immutable characteristics of landowners already collected by the USDA (deciles of soil productivity and wind and water erosion). Bar (7) uses the same scoring rule as bar (6) but reduces the number of contracts allocated to landowners: only landowners with positive scoring-rule-implied social surplus max<sub>j</sub> s<sub>j</sub>(**z**<sub>i</sub>) − c<sub>i</sub> − κ<sub>ij</sub> ≥ 0 are awarded contracts.

39

# Table 1: Summary Statistics


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">All agricultural land</th>
        <th colspan="2">Bidders</th>
        <th colspan="2">Bid fields</th>
    </tr>
    <tr>
        <th> </th>
        <th>Remote</th>
        <th>Admin</th>
        <th>Remote</th>
        <th>Admin</th>
        <th>Remote</th>
        <th>Admin</th>
    </tr>
    <tr>
        <th> </th>
        <th>sensing</th>
        <th> </th>
        <th>sensing</th>
        <th> </th>
        <th>sensing</th>
        <th> </th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
        <th>(6)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="7"><strong>Panel A. Land use</strong></td>
    </tr>
    <tr>
        <td>Share cropped</td>
        <td>0.29</td>
        <td>0.26</td>
        <td>0.22</td>
        <td>0.23</td>
        <td>0.24</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>Share corn</td>
        <td>0.11</td>
        <td>0.10</td>
        <td>0.08</td>
        <td>0.08</td>
        <td>0.07</td>
        <td>0.06</td>
    </tr>
    <tr>
        <td>Share soybean</td>
        <td>0.09</td>
        <td>0.09</td>
        <td>0.07</td>
        <td>0.08</td>
        <td>0.09</td>
        <td>0.09</td>
    </tr>
    <tr>
        <td>Share fallow</td>
        <td>0.02</td>
        <td>0.01</td>
        <td>0.03</td>
        <td>0.02</td>
        <td>0.04</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>Share nat. veg. or grassland</td>
        <td>0.57</td>
        <td> </td>
        <td>0.69</td>
        <td> </td>
        <td>0.63</td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="7"><strong>Panel B. Land characteristics</strong></td>
    </tr>
    <tr>
        <td>Size (acres)</td>
        <td colspan="2">160.7</td>
        <td colspan="2">250.6</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td colspan="2">(2690.7)</td>
        <td colspan="2">(506.5)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Soil productivity ($/acre)</td>
        <td colspan="2">92.4</td>
        <td colspan="2">86.9</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td colspan="2">(63.2)</td>
        <td colspan="2">(58.5)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Enviro sensitivity (points)</td>
        <td colspan="2">53.5</td>
        <td colspan="2">86.5</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td colspan="2">(29.8)</td>
        <td colspan="2">(33.7)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="7"><strong>Panel C. Bid characteristics</strong></td>
    </tr>
    <tr>
        <td>Rental rate ($/acre/year)</td>
        <td> </td>
        <td> </td>
        <td colspan="2">83.0</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td colspan="2">(56.4)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Acres bid</td>
        <td> </td>
        <td> </td>
        <td colspan="2">84.1</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td colspan="2">(136.3)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Share re-contracting</td>
        <td> </td>
        <td> </td>
        <td colspan="2">0.70</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Contract action = grasses</td>
        <td> </td>
        <td> </td>
        <td colspan="2">0.67</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Contract action = trees</td>
        <td> </td>
        <td> </td>
        <td colspan="2">0.12</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Contract action = habitat</td>
        <td> </td>
        <td> </td>
        <td colspan="2">0.21</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Share contracting</td>
        <td> </td>
        <td> </td>
        <td colspan="2">0.81</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>N bidders / auction</td>
        <td> </td>
        <td> </td>
        <td colspan="2">36,772</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>N</td>
        <td colspan="2">7,890,426</td>
        <td colspan="2">258,286</td>
        <td colspan="2">61,703</td>
    </tr>
  </tbody>
</table>


Notes: Table presents summary statistics of all agricultural landowners (columns (1)-(2)), bidding landowners (columns (3)-(4)), and bid fields (columns (5)-(6)), defined as the delineated land area entered into the mechanism to be awarded a CRP contract (observed only for bidders in the 2016 auction). Standard deviations in parenthesis. Panel A reports land use outcomes in the remote sensing (CDL) and admin (Form 578) data. All land use outcomes are reported for the year of the auction (prior to contract awards). Land use outcomes in columns (1)-(2) are averaged across auction years with land use outcomes among both eligible non-bidders and ineligible land. Land use categories follow Lark et al. (2017). Crop outcomes exclude alfalfa and hay. Soil productivity is calculated by NASS and is reported in dollars per acre. Environmental sensitivity points are the points given for characteristics of land in the scoring rule. Rental rate is reported in dollars per acre per year and is the dollar component of the bid in the auction. Acres bid is the total acreage entered into the auction to be awarded a CRP contract. Grasses, trees, and habitat contract indicators are aggregated over the menu of possible contracts.

40

# Table 2: Regression Discontinuity Coefficient Estimates


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Remote sensing</th>
        <th>Admin</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="3"><strong>Panel A: Share of land cropped vs. contracted</strong></td>
    </tr>
    <tr>
        <td>Share of land cropped pre-auction (placebo)</td>
        <td>0.002</td>
        <td>0.004</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.011)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (pooled)</td>
        <td>-0.075</td>
        <td>-0.097</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.007)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (average year effects)</td>
        <td>-0.081</td>
        <td>-0.101</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.007)</td>
    </tr>
    <tr>
        <td>Share of land contracted post-auction</td>
        <td>0.345</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.004)</td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="3"><strong>Panel B: Other outcomes</strong></td>
    </tr>
    <tr>
        <td>Share of land cropped corn</td>
        <td>-0.015</td>
        <td>-0.022</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land cropped soybeans</td>
        <td>-0.018</td>
        <td>-0.025</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land fallow</td>
        <td>-0.008</td>
        <td>-0.012</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.002)</td>
        <td>(0.002)</td>
    </tr>
    <tr>
        <td>Share of land in natural vegetation or grassland</td>
        <td>0.091</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="3"><strong>Panel C: Spillovers to non-bid fields</strong></td>
    </tr>
    <tr>
        <td>Share of non-bid fields cropped</td>
        <td>-0.007</td>
        <td>-0.007</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td>(0.011)</td>
    </tr>
    <tr>
        <td colspan="3">Observations in 25-point window</td>
    </tr>
    <tr>
        <td>N bidders</td>
        <td>41,269</td>
        <td>41,269</td>
    </tr>
    <tr>
        <td>N bidder-years</td>
        <td>495,228</td>
        <td>288,883</td>
    </tr>
    <tr>
        <td>Spillovers (bid fields observed): N bidders</td>
        <td>6,770</td>
        <td>6,770</td>
    </tr>
    <tr>
        <td>Spillovers (bid fields observed): N bidder-years</td>
        <td>81,240</td>
        <td>47,390</td>
    </tr>
  </tbody>
</table>


*Notes:* Table presents coefficient estimates from equations (6) and (7) with land use outcomes measured in the remotely sensed (column (1)) and administrative (column (2)) data. All results use a local linear regression on either side of the win threshold in the MSE-optimal bandwidth (Calonico et al., 2014). All estimates are from equation (7), except the "average year effects" row, which presents the average of the post-period year effects (coefficient estimates of $\beta_{r(i,t)}$ from (6)) weighting each year equally. The pooled post-period includes an average of 7-8 post-auction years. Natural vegetation or grassland is only observed in remotely sensed data. Share of land contracted post auction estimates the share of bidders' land contracting among winning bidders. Panel C estimates the effect of a CRP contract on non-bid, and therefore non-contracting, fields to test for spillovers; this analysis is restricted to the 2016 auction. Standard errors are clustered at the bidder level. Estimates of additionality are calculated by dividing the estimates for the share of land cropped by the estimates of the share of land contracted and equal 22-23% (remote sensing) and 28-29% (admin). Estimates using the share of land in natural vegetation or grasslands yields a similar estimate (26%).

41

# Table 3: Mean Landowner Costs of Accepting CRP Contracts


<table>
  <thead>
    <tr>
        <th> </th>
        <th>All</th>
        <th>Bidders with &gt; median soil productivity</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Base cost ($c_i$)</td>
        <td>67.21</td>
        <td>92.86</td>
    </tr>
    <tr>
        <td>Top-up cost ($\kappa_{ij}$)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Native grasses</td>
        <td>-0.03</td>
        <td>3.97</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>23.76</td>
        <td>27.33</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>15.06</td>
        <td>18.61</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>15.44</td>
        <td>18.91</td>
    </tr>
    <tr>
        <td>Wildlife food plot</td>
        <td>18.79</td>
        <td>15.09</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>17.97</td>
        <td>17.41</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents estimated mean landowner costs for the base cost $c_i$ and top-up cost $\kappa_{ij}$ in dollars per acre per year. The base contract of introduced grasses is normalized to zero. The cost of each contract $j$ is defined as $c_i + \kappa_{ij}$. Costs are estimated using revealed preferences in optimal bidding (equation (10)). Column (1) presents mean costs for all bidders across all auctions, and column (2) restricts to bidders with above median soil productivity.

# Table 4: Additionality as a Function of Landowner Costs


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="4">Estimates of $\tau (\mathbf{z}_i, c_i, \kappa_i)$</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>$\beta$: coefficient on base cost ($c_i$)</td>
        <td>0.0018<br/>(0.0001)</td>
        <td>0.0017<br/>(0.0002)</td>
        <td>0.0007<br/>(0.0002)</td>
        <td>-0.0008<br/>(0.0003)</td>
    </tr>
    <tr>
        <td>$\alpha$: coefficient on top-up cost ($\kappa_{ij}$)</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Trees</td>
        <td> </td>
        <td> </td>
        <td>0.0025<br/>(0.0003)</td>
        <td>0.0047<br/>(0.0004)</td>
    </tr>
    <tr>
        <td>Native grasses</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>0.0020<br/>(0.0004)</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>0.0032<br/>(0.0004)</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>0.0034<br/>(0.0005)</td>
    </tr>
    <tr>
        <td>Wildlife food plot</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>0.0030<br/>(0.0005)</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>0.0022<br/>(0.0005)</td>
    </tr>
    <tr>
        <td>Includes $\mathbf{z}_i^s$</td>
        <td>✓</td>
        <td>✓</td>
        <td>✓</td>
        <td>✓</td>
    </tr>
    <tr>
        <td>Includes prior land use and soil prod. $\mathbf{z}_i$</td>
        <td> </td>
        <td>✓</td>
        <td>✓</td>
        <td>✓</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents select coefficient estimates of $\tau (\mathbf{z}_i, c_i, \kappa_i)$ (equation (14)). Coefficients measure how additionality varies with a $1 per acre, per year change in costs. Positive coefficients indicate a positive correlation between costs and additionality. Parameter estimates obtained via Method of Simulated Moments using the remote sensing land use data (for losing bidders in the 2016 auction) and bids given simulated ($c_i, \kappa_i$) and optimal bidding in equation (10). All specifications include flexible controls for the components of the scoring rule excluding landowners’ Wildlife Priority Zone and Air Quality Zone status. Columns (2)-(4) control for the 32 cells of soil productivity, prior CRP status, and prior cropping status that parameterize bidder costs. Standard errors calculated by block bootstrap (at the bidder level) and incorporate estimation error in costs by drawing from the estimated variance-covariance matrix of the parameters in $F_{c,\kappa|\mathbf{z}}$ in the bootstrap procedure.

42

# Table 5: Outcomes Under Alternative Auctions


<table>
  <thead>
    <tr>
        <th rowspan="2"> </th>
        <th rowspan="2">Status<br/>quo</th>
        <th rowspan="2">Efficient<br/>alloca-<br/>tion</th>
        <th colspan="6">Vickrey auctions with scoring</th>
        <th rowspan="2">Status<br/>quo<br/>auc-<br/>tion</th>
        <th rowspan="2">Vickrey<br/>auc-<br/>tion<br/>with<br/>scoring<br/><br/>2016<br/>only<br/>(hold<br/>out)</th>
    </tr>
    <tr>
        <th>Status<br/>quo<br/>rule</th>
        <th>Re-<br/>weight<br/>contracts<br/>(ω<sub>j</sub>)</th>
        <th>Re-<br/>weight<br/>z<sup>s</sup><sub>i</sub><br/>(ω<sub>z</sub>)</th>
        <th>Add<br/>τ̂(z<sub>i</sub>)</th>
        <th>Reduce<br/>quan-<br/>tity</th>
        <th>Uniform<br/>multi-<br/>plier</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
        <th>(6)</th>
        <th>(7)</th>
        <th>(8)</th>
        <th>(9)</th>
        <th>(10)</th>
    </tr>
    <tr>
        <td colspan="11"><strong>Panel A. Welfare and spending (million $ per auction):</strong></td>
    </tr>
    <tr>
        <td>Social welfare</td>
        <td>120</td>
        <td>984</td>
        <td>140</td>
        <td>296</td>
        <td>333</td>
        <td>349</td>
        <td>440</td>
        <td>325</td>
        <td>236</td>
        <td>217</td>
    </tr>
    <tr>
        <td>USDA spending</td>
        <td>1,615</td>
        <td> </td>
        <td>2,419</td>
        <td>2,146</td>
        <td>2,250</td>
        <td>2,033</td>
        <td>877</td>
        <td>2,225</td>
        <td>1,373</td>
        <td>100</td>
    </tr>
    <tr>
        <td>Landowner surplus</td>
        <td>666</td>
        <td> </td>
        <td>1,470</td>
        <td>1,430</td>
        <td>1,559</td>
        <td>1,329</td>
        <td>523</td>
        <td>1,543</td>
        <td>596</td>
        <td>61</td>
    </tr>
    <tr>
        <td>Environmental value</td>
        <td>1,069</td>
        <td>1,461</td>
        <td>1,089</td>
        <td>1,012</td>
        <td>1,024</td>
        <td>1,052</td>
        <td>794</td>
        <td>1,007</td>
        <td>1,013</td>
        <td>256</td>
    </tr>
    <tr>
        <td colspan="11"><strong>Panel B. Other outcomes</strong></td>
    </tr>
    <tr>
        <td>Additionality | contract</td>
        <td>0.201</td>
        <td>0.447</td>
        <td>0.197</td>
        <td>0.198</td>
        <td>0.203</td>
        <td>0.209</td>
        <td>0.208</td>
        <td>0.201</td>
        <td>0.205</td>
        <td>0.305</td>
    </tr>
    <tr>
        <td>Share awarded contract</td>
        <td>0.81</td>
        <td>0.50</td>
        <td>0.81</td>
        <td>0.81</td>
        <td>0.81</td>
        <td>0.81</td>
        <td>0.62</td>
        <td>0.81</td>
        <td>0.81</td>
        <td>0.18</td>
    </tr>
  </tbody>
</table>


Notes: Table presents results under current and alternative auctions. Panel A tabulates social welfare (equation (16)), USDA spending, landowner surplus, and environmental value $\sum_i \sum_j B_j(z^s_i) \cdot \tau(z_i, c_i, \kappa_i) \cdot x_{ij}$ in million dollars per auction. Panel B tabulates average additionality of contracting landowners and the share of landowners with a contract. All auctions impose that each landowner obtains at most one contract and that total contracts awarded cannot exceed the status quo, in each auction. Columns (1)-(9) present results for all auctions in the sample. Column (10) presents results for the 2016 auction only. Column (1) simulates the status quo. Column (2) simulates an efficient allocation that allocates contracts using all $z_i$ and $(c_i, \kappa_i)$ to maximize equation (16). Due to adverse selection, this allocation may not be implementable. Columns (3)-(8) simulate alternative Vickrey auctions with scoring (see Section V. for more details). Columns (3)-(6) and (8)-(9) hold quantity (the number of landowners allocated contracts, in each auction) constant at the status quo and change the scoring rule $s_j(z_i)$ defined in equation (17). Column (3) uses the existing scoring rule $s_j(z_i) = B_j(z^s_i)$. Column (4) uses a scoring rule with the social-surplus maximizing incentives across contracts $(\omega_j)$. Column (5) uses a scoring rule with the social-surplus maximizing asymmetry terms across bidders using characteristics already in the scoring rule $(z^s_i)$. Column (6) adds an additional characteristic to the scoring rule, a prediction of $\tau(z_i, c_i, \kappa_i)$ using immutable characteristics of landowners already collected by the USDA but not all included in the status quo scoring rule (deciles of soil productivity and wind and water erosion). Column (7) uses the same scoring rule as column (6) but reduces the number of contracts allocated to landowners: only landowners with positive scoring-rule-implied social surplus $\max_j s_j(z_i) - c_i - \kappa_{ij} \geq 0$ are awarded contracts. Column (8) uses a scoring rule $s_j(z^s_i) = \theta \cdot B_j(z^s_i)$ for a uniform multiplier $\theta$ that maximizes equation (16). Column (9) implements the scoring rule in columns (6) and (7) in the status quo auction format. Column (10) implements a Vickrey auction with scoring using the limited information scoring rule. This rule is constructed exploiting cross-tract and cross-auction heterogeneity for pre-2016 auctions, measured in the administrative data (versus the remotely-sensed data used in estimation). Evaluation in column (10) uses the hold-out 2016 auction only (used to estimate the parameters in $\tau(z_i, c_i, \kappa_i)$).

43

**Supplemental Appendix for:**

Additionality and Asymmetric Information in Environmental Markets: Evidence from Conservation Auctions

Karl M. Aspelund and Anna Russo

44

# A Supplemental Material: Theoretical Framework

$\theta_i = (c_i, a_{i0})$ can be defined in the context of the following model. First, define $\pi$ as the net payoffs to a landowner from cropping a field. $\pi > 0$ implies expected revenues from harvested crops at the point of planting exceed input costs, whereas $\pi < 0$ implies the opposite. When a landowner chooses $x_i = 1$, she makes a long-term (binding, ten-year) promise to conserve the fields on her land. At the point of contracting, each landowner faces stochastic and heterogeneous net payoffs to cropping each of her fields over the contract period. Therefore, define $h_i(\pi)$ as the probability density function describing landowner $i$’s net payoffs from cropping across fields over the contract period. Finally, define $\eta_i$ as a landowner’s hassle cost type.

When unconstrained by the contract, landowners crop their land when it is profitable to do so. When constrained by the contract, landowners cannot crop — they give up the option — and may incur some hassle cost. Each landowner’s type $\theta_i = (c_i, a_{i0})$ can therefore be written as:<sup>49</sup>

$$ (19) \qquad c_i = \int_{0}^{\infty} \pi h_i(\pi) \, d\pi + \eta_i $$

$$ (20) \qquad a_{i0} = \int_{-\infty}^{0} h_i(\pi) \, d\pi. $$

Equations (19) and (20) clarify why the graphs in Section I. may feature upwards-sloping contract value curves ($\tau'(c) > 0$): both $c_i$ and $a_{i0}$ depend on $h_i(\pi)$. They also clarify why *both* $f_C$ and $\tau(c)$ must be estimated empirically. $\tau(c)$ is generally ambiguous, given $f_C$.

**Special Case** The microfoundation is useful to explore an important, one-dimensional special case with a single field without uncertainty or hassle costs. In this special case, $h_i(\pi)$ has mass only at a single point, $\pi_i$, and $\eta_i = 0$ for all $i$. Therefore, $c_i = \mathbb{1} \{\pi_i > 0\} \cdot \pi_i$ and $a_{i0} = \mathbb{1} \{\pi_i \leq 0\}$.

Appendix Figure A.1 illustrates this case in the graphical analysis of Section I.. There is a mass at zero, where $\pi_i < 0$, so $c_i = 0$ and $\tau(0) = 0$: these are landowners for whom accepting the contract *both* imposes no costs ($c_i = 0$) and has no impact ($\tau(0) = 0$). For any $c > 0$, $\tau(c) = 1$.

<sup>49</sup>Note that here, $a_{i0}$ is defined ex-ante at the time of contracting, corresponding to the definition of $c_i$, whereas in our empirical tests we observe the realization of the action $a_{i0}$. However, recall that our welfare criterion in the graphical analysis of Section I. and our estimation and counterfactual analysis is defined as a function of the conditional expectation function $\tau(c) = \mathbb{E} [1 - a_{i0} \mid c = c_i]$. We therefore take expectations across realizations among landowners with the same $c_i$, making the assumption that landowners’ expectations about the payoffs to cropping are correct on average, across years and among landowners with the same $c_i$.

45

Figure A.1: Graphical Analysis: One-Dimensional Special Case


<table>
  <thead>
    <tr>
        <th>Series</th>
        <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>MC</td>
        <td>Piecewise linear curve starting at 0 on the x-axis, then increasing linearly.</td>
    </tr>
    <tr>
        <td>B·τ</td>
        <td>Horizontal line at value B.</td>
    </tr>
    <tr>
        <td>Annotation</td>
        <td>B·τ(c) - c represents the vertical distance between B·τ and MC.</td>
    </tr>
    <tr>
        <td>Annotation</td>
        <td>No gains &amp; no costs indicates the region where MC is zero.</td>
    </tr>
  </tbody>
</table>


*Notes:* Figure describes a market characterized by a marginal cost ($MC = F_C^{-1}(q)$) and contract value ($B \cdot \tau$) curve for a one-dimensional case in which $\tau(0) = 0$ and $\tau(c) = 1$ for all $c > 0$. The horizontal axis is the share of the population ordered by costs $c_i$. $B$ denotes the social benefits of $a_{i1} = 1$. $B \cdot \tau$ denotes the incremental social benefits of contracting, relative to no contract, at each quantile of landowner costs $c_i$. The vertical distance between the $B \cdot \tau$ and $MC$ curves represents the social surplus from contracting, $B \cdot \tau(c) - c$. In this special case, social surplus is not monotonically decreasing in landowner cost. Therefore, while the efficient allocation can be implemented by $p^* = B$, in the presence of a binding quantity constraint, the efficient allocation cannot be implemented.

Figure A.1 illustrate three important points in this special case. First, setting prices at $p^* = B$, the social value of the conservation action (Pigouvian pricing), implements the efficient allocation. Second, $B \cdot \tau(c) - c$ is *not* monotonically decreasing. Given a binding quantity constraint, an efficient allocation subject to that constraint may therefore *not* be implementable. Finally, in Figure A.1, in contrast to the general model described above, knowledge of $B$ and $f_C$ alone are sufficient to evaluate social welfare.

Importantly, distinguishing between this special case and the more general model presented above is possible with a design that estimates additionality, $\tau(c)$, at a given quantile, or among *marginal* landowners. This is because in Figure A.1, $\tau(c)$ can only take values of zero or one. In Section A., we leverage a regression discontinuity design to estimate additionality among marginal landowners and reject this special case.

**Extension: Competitive Markets** We can extend the framework of Section I. to study settings where contracts are traded in competitive markets to capture a stylized model of a voluntary offset market. In a competitive market, adverse selection can prevent the competitive equilibrium price from implementing an efficient allocation, even when it is implementable with the price that solves equation (4) (Akerlof, 1970).

We explore this possibility by making the important simplifying assumption that buyers in this market also value the benefits of land retirement at their social benefits, $B$, and define a competitive

46

market price $p^c$ by the equilibrium condition: $p^c = \mathbb{E}[B \cdot \tau(c_i) \mid c_i \leq p^c]$. The key distinction is that in this stylized competitive market, the equilibrium price is determined by price-taking buyers taking expectations over the additionality of all market participants, not only those contracting at the margin.

Figure A.2 illustrates by adding the curve defined by $\mathbb{E}[B \cdot \tau(c_i) \mid c_i \leq p]$ to the population of landowners presented in Figure 1a.<sup>50</sup> Its intersection with the marginal cost curve defines the competitive market equilibrium, which differs from the socially-optimal price. In the presence of adverse selection, trade in this stylized competitive market will be limited and efficient contracting, with social welfare gains represented in triangle EFG, will not occur.

Figure A.2: Graphical Analysis: Competitive Markets


<table>
  <tbody>
    <tr>
        <td>Point/Tick</td>
        <td>X-axis (q)</td>
        <td>Y-axis ($)</td>
    </tr>
    <tr>
        <td>D</td>
        <td>0</td>
        <td>D</td>
    </tr>
    <tr>
        <td>C</td>
        <td>0</td>
        <td>C</td>
    </tr>
    <tr>
        <td>F</td>
        <td>qᶜ</td>
        <td>pᶜ</td>
    </tr>
    <tr>
        <td>E</td>
        <td>qᶜ</td>
        <td>E</td>
    </tr>
    <tr>
        <td>G</td>
        <td>q*</td>
        <td>p*</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure describes a market characterized by a marginal cost ($MC = F_C^{-1}(q)$) and a contract value ($B \cdot \tau$) curve. The horizontal axis is the share of the population ordered by costs $c_i$. $B$ denotes the social benefits of $a_{i1} = 1$. $B \cdot \tau$ denotes the incremental social benefits of contracting, relative to no contract, at each quantile of landowner costs. The vertical distance between the $B \cdot \tau$ and $MC$ curves represents the social surplus from contracting. Upwards-sloping $B \cdot \tau$ curves illustrate markets with adverse selection. The figure also includes a curve defining the average contract value of all landowners selecting into the market at any given price $p$, $\mathbb{E}[B \cdot \tau(c) \mid c \leq p]$. This defines the value of a contract to a price-taking buyer in a stylized competitive market. The intersection of the $MC$ and average contract value curves define a competitive market equilibrium price $p^c$. Adverse selection limits trade, with social welfare gains in triangle EFG that are not achieved.

# B Institutional Appendix: The CRP Mechanism

The scoring rule depends on characteristics of the land, the conservation action defined in the tract, and the dollar component of the bid (the bid rental rate). We describe the details associated with each of these components below. The details of the scoring rule are published each year in EBI Factsheets.<sup>51</sup>

**Land characteristics** The characteristics that influence the scoring rule include:

\*<sup>50</sup>This curve is defined as $\frac{1}{q} \int_0^q B \cdot \tau \left( F_C^{-1} (\tilde{q}) \right) d\tilde{q}$.

\*<sup>51</sup>See an EBI Factsheet for an example.

47

* **Whether a bidder is in a Wildlife Priority Zone (WPZ)**, defined high priority wildlife geographic areas. 30 points.
* **Whether a bidder is in a Water Quality Zone (WQZ)**, areas with high value to improving ground or surface water quality. 30 points.
* **Groundwater quality**: an evaluation of the predominant soils, potential leaching of pesticides and nutrients into groundwater, and the impact to people who rely on groundwater as a primary source of drinking water. Continuous score: 0 to 25 points.
* **Surface water quality**: an evaluation of the amount of sediment (and associated nutrients) that may be delivered into streams and other water courses. Continuous score: 0 to 45 points.
* **Erosion potential**: Continuous score of 0 to 100 points depending on the Erodibility Index.
* **Air quality**: an evaluation of the air quality improvements by reducing airborne dust and particulate caused by wind erosion from cropland. Continuous score of 0 to 30 points depending on wind speed, wind direction, and the duration of wind events and soil erodibility.
* **Whether a bidder is in an Air Quality Zone (AQZ)**. 5 points.

These characteristics depend on a bidder’s location and not their bid, i.e. they determine bidder asymmetry in the scoring rule. These characteristics are known for every agricultural field in the US.

**Heterogeneous contracts defined by conservation actions** Conservation actions can be grouped into two categories: a primary cover, described in Table B.1, which covers the total area offered into the CRP, and an (optional) additional upgrade action, described in Table B.2, which can be offered in addition to the primary cover on a smaller area. In total, there are 36 possible contracts: 12 primary covers interacted with three upgrade cover options (including no upgrade).

48

# Table B.1: Contract Action Choices: Primary Covers


<table>
  <thead>
    <tr>
        <th>Short name</th>
        <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Grasses 1</td>
        <td>Permanent introduced grasses and legumes (CP1): Existing stand of one to three species or planting new stand of two to three species of an introduced grass species.</td>
    </tr>
    <tr>
        <td>Grasses 2</td>
        <td>Permanent introduced grasses and legumes (CP1): Existing stand or planted mixture (minimum of four species) of at least 3 introduced grasses and at least one forb or legume species best suited for wildlife in the area.</td>
    </tr>
    <tr>
        <td>Grasses 3</td>
        <td>Permanent native grasses and legumes (CP2): Existing stand (minimum of one to three species) or planting mixed stand (minimum of three species) of at least two native grass species at least one forb or legume species beneficial to wildlife.</td>
    </tr>
    <tr>
        <td>Grasses 4</td>
        <td>Permanent native grasses and legumes (CP2): Existing stand or planting mixed stand (minimum of five species) of at least 3 native grasses and at least one shrub, forb, or legume species best suited for wildlife in the area.</td>
    </tr>
    <tr>
        <td>Trees 1</td>
        <td>Tree planting (softwoods) (CP3): Southern pines, northern conifers, or western pines – solid stand of pines/conifers/softwoods (existing, according to state developed standards, or planted at more than 550 (southern pines), 850 (northern conifers), or 650 (western pines) trees per acre).</td>
    </tr>
    <tr>
        <td>Trees 2</td>
        <td>Tree planting (softwoods) (CP3): Southern pines, northern conifers, or western pines – pines/conifers/softwoods existing or planted at a rate of 500-550 (southern pines), 750-850 (northern conifers), or 550-650 (western pines) per acre depending on the site index (state-developed standards) with 10-20% openings managed to a CP4D wildlife cover.</td>
    </tr>
    <tr>
        <td>Trees 3</td>
        <td>Hardwood tree planting (CP3A): Existing or planting solid stand of nonmast producing hardwood species.</td>
    </tr>
    <tr>
        <td>Trees 4</td>
        <td>Hardwood tree planting (CP3A): Existing or planting solid stand of single hard mast producing species.</td>
    </tr>
    <tr>
        <td>Trees 5</td>
        <td>Hardwood tree planting (CP3A): Existing or planting mixed stand (three or more species) or hardwood best suited for wildlife in the area or existing or planting stand of longleaf pine or atlantic white cedar – planted at rates appropriate for the site index.</td>
    </tr>
    <tr>
        <td>Habitat 1</td>
        <td>Permanent wildlife habitat, noneasement (CP4D): Existing stand or planting mixed stand (minimum of four species) of either grasses, trees, shrubs, forbs, or legumes planted in mixes, blocks, or strips best suited for various wildlife species in the area. A wildlife conservation plan must be developed with the participant.</td>
    </tr>
    <tr>
        <td>Habitat 2</td>
        <td>Permanent wildlife habitat, noneasement (CP4D): Existing stand or planting mixed stand (minimum of five species) or either predominantly native species including grasses, forbs, legumes, shrubs, or trees planted in mixes, blocks, or strips best suited to providing wildlife habitat. Only native grasses are authorized. A wildlife conservation plan must be developed with the participant.</td>
    </tr>
    <tr>
        <td>Habitat 3</td>
        <td>Rare and declining habitat restoration (CP25): Existing stand or seeding or planting will be best suited for wildlife in the area. Plant species selections will be based upon Ecological Site Description data.</td>
    </tr>
  </tbody>
</table>


*Notes: Table describes the menu of primary cover actions.*

49

# Table B.2: Contract Action Choices: Upgrades


<table>
  <thead>
    <tr>
        <th>Short name</th>
        <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>No upgrade</td>
        <td>Primary cover only</td>
    </tr>
    <tr>
        <td>Wildlife food plot</td>
        <td>Wildlife food plots are small plantings in a larger area</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>Existing stand or planting (minimum of .5 acres) of a diverse mix of multiple species suited for pollinators</td>
    </tr>
  </tbody>
</table>


*Notes: Table describes the menu of upgrade actions.*

We obtain the points associated with each of the contract options, defined by the actions in Tables B.1 and B.2 from the EBI Fact Sheets. The point values assigned to the different contracts can vary across bidders based on whether or not a bidder is in a Wildlife Priority Zone (WPZ).

**Bid rental rate** The scoring rule is non-linear in $r_i$. The existence of bid caps make some choices infeasible if $r_i > \bar{r}_i$, where $\bar{r}_i$ denotes the landowner specific bid cap. The scoring rule also includes non-linearities based on the amount a bidder bids below the bid cap with kinks at 10% and 15% below the bid cap.<sup>52</sup> The weight on this component is announced only after bids are collected, but it has remained essentially constant throughout our sample period, so we treat it as known.

**An example menu** The mechanism implies a menu of payments for each contract at each score. These menus differ by observable characteristics of landowners due to asymmetry in the existing rule. Table B.3 describes an example menu.

<sup>52</sup>We observe bunching at the kink points, suggesting that bidders understand the scoring rule and make sophisticated choices in the mechanism.

50

Table B.3: Payments (for a Given Target Score) and Market Shares Across Contracts


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Average payment at chosen score</th>
        <th>Market share</th>
        <th>Average payment at chosen score</th>
        <th>Market share</th>
        <th>Average payment at chosen score</th>
        <th>Market share</th>
    </tr>
    <tr>
        <th> </th>
        <th colspan="2">No upgrade</th>
        <th colspan="2">+ wildlife flood plot</th>
        <th colspan="2">+ pollinator habitat</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Intro Grasses 1</td>
        <td>25.02</td>
        <td>0.140</td>
        <td>31.90</td>
        <td>0.015</td>
        <td>50.36</td>
        <td>0.007</td>
    </tr>
    <tr>
        <td>Intro Grasses 2</td>
        <td>72.47</td>
        <td>0.104</td>
        <td>76.13</td>
        <td>0.022</td>
        <td>84.45</td>
        <td>0.020</td>
    </tr>
    <tr>
        <td>Native Grasses 1</td>
        <td>40.61</td>
        <td>0.067</td>
        <td>46.60</td>
        <td>0.005</td>
        <td>62.53</td>
        <td>0.010</td>
    </tr>
    <tr>
        <td>Native Grasses 2</td>
        <td>79.36</td>
        <td>0.201</td>
        <td>82.00</td>
        <td>0.023</td>
        <td>88.86</td>
        <td>0.057</td>
    </tr>
    <tr>
        <td>Trees 1</td>
        <td>63.05</td>
        <td>0.039</td>
        <td>67.52</td>
        <td>0.003</td>
        <td>77.87</td>
        <td>0.000</td>
    </tr>
    <tr>
        <td>Trees 2</td>
        <td>93.31</td>
        <td>0.020</td>
        <td>95.04</td>
        <td>0.003</td>
        <td>100.07</td>
        <td>0.001</td>
    </tr>
    <tr>
        <td>Trees 3</td>
        <td>71.49</td>
        <td>0.012</td>
        <td>74.79</td>
        <td>0.001</td>
        <td>83.51</td>
        <td>0.000</td>
    </tr>
    <tr>
        <td>Trees 4</td>
        <td>77.87</td>
        <td>0.002</td>
        <td>80.79</td>
        <td>0.000</td>
        <td>88.17</td>
        <td>0.000</td>
    </tr>
    <tr>
        <td>Trees 5</td>
        <td>96.74</td>
        <td>0.029</td>
        <td>98.43</td>
        <td>0.003</td>
        <td>103.32</td>
        <td>0.001</td>
    </tr>
    <tr>
        <td>Habitat 1</td>
        <td>73.48</td>
        <td>0.032</td>
        <td>77.01</td>
        <td>0.006</td>
        <td>85.06</td>
        <td>0.001</td>
    </tr>
    <tr>
        <td>Habitat 2</td>
        <td>80.10</td>
        <td>0.039</td>
        <td>82.67</td>
        <td>0.007</td>
        <td>89.37</td>
        <td>0.014</td>
    </tr>
    <tr>
        <td>Rare Habitat</td>
        <td>91.63</td>
        <td>0.077</td>
        <td>93.39</td>
        <td>0.009</td>
        <td>98.51</td>
        <td>0.025</td>
    </tr>
  </tbody>
</table>


Notes: Table presents the menu of all 36 possible contracts, split into 12 primary covers and three upgrade options. Table reports payments across contracts, calculated as the rental rate per acre per year to reach a given score (held fixed in this table at the observed score) with a given contract. Payments vary across bidders with heterogeneous $z_i^s$; this table calculates the averages across all bidders. Table also reports the market shares of each contract.

51

# C Data Appendix

**Agricultural Units: Tracts and Fields** All agricultural land is the US is divided into fields, or Common Land Units, by the USDA. A field is defined as the smallest unit of land that has: (i) a permanent, contiguous boundary, (ii) a common land cover and land management, and (iii) a common owner.<sup>53</sup> There are 37,480,917 fields in the US (as of 2016), with an average size of 33.82 acres. Each field, by definition, has a single land use.

A tract is a collection of fields under one common ownership that is operated as a farm or part of a farm (a tract is a landowner, or bidder, in our setting). The average tract includes 4.75 fields. Each tract can submit at most one bid into a CRP auction. This bid can include any subset of a tract's fields. A bid is not constrained to bid only entire fields; in principle, a bidder can bid any subset of their land, regardless of field delineations. In practice, a large share of bids follow field boundaries, as illustrated by Figure C.2. In our analyses, we therefore treat bid fields as defining the land offered into the mechanism.

Our dataset includes an identifier and the geolocation of each of the bidding tracts, and their subset fields, for all auctions. We only observe the identifiers of the bid fields in 2016. Figure C.1 provides an illustrative example of the various agricultural land units. Figure C.3a maps the geographic distribution of bids.

Figure C.1: Example: Tract, Fields, and Bid Fields

![Diagram showing a tract containing six fields (Field 1 to Field 6). Fields 1, 2, 5, and 6 contain corn icons. Fields 3 and 4 contain grass icons and are shaded green, representing CRP bid fields. The green shading in Field 3 only covers a portion of the field, while in Field 4 it covers the entire field.](page_53_image_1_v2.jpg)

*Notes:* Figure explains the various geographic units in our dataset. The blue outline is a single tract: this is the unit of landowner (bidder) in our analysis. This tract contains six fields, these are administrative delineations of a tract, each with a single land use. The green shaded area represents an example area bid into the CRP. This could follow field boundaries (as for field 4) or cut into fields (as for field 3).

\*<sup>53</sup>See the Common Land Unit Information Sheet published by the USDA for more details.

52

Figure C.2: Share of Bid Fields Bid into the Mechanism


<table>
  <thead>
    <tr>
        <th>Share of bid fields bid</th>
        <th>0</th>
        <th>0.1</th>
        <th>0.2</th>
        <th>0.3</th>
        <th>0.4</th>
        <th>0.5</th>
        <th>0.6</th>
        <th>0.7</th>
        <th>0.8</th>
        <th>0.9</th>
        <th>1.0</th>
        <th>1.1</th>
        <th>1.2</th>
        <th>1.3</th>
        <th>1.4</th>
        <th>1.5</th>
        <th>1.6</th>
        <th>1.7</th>
        <th>1.8</th>
        <th>1.9</th>
        <th>2.0</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Density</td>
        <td>0.2</td>
        <td>0.2</td>
        <td>0.2</td>
        <td>0.2</td>
        <td>0.2</td>
        <td>0.1</td>
        <td>0.1</td>
        <td>0.1</td>
        <td>0.1</td>
        <td>0.4</td>
        <td>13.0</td>
        <td>1.5</td>
        <td>0.4</td>
        <td>0.2</td>
        <td>0.1</td>
        <td>0</td>
        <td>0</td>
        <td>0</td>
        <td>0</td>
        <td>0</td>
        <td>0</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure shows a histogram of the share of the land of the bid fields that are bid into the CRP (the shaded green area as a share of the total area of fields 3 and 4 in Figure C.1). The mass point at one indicates that the vast majority of bidders bid the entire field.

53

# Figure C.3: Maps

## (a) Share of Land Bid for CRP Contract

![Map of the United States showing the share of land bid for CRP contract by county, with a legend indicating four levels of intensity from [0,0] to (.0093, .3098].](qdwl)

## (b) Share of Land Cropped (Remote Sensing)

![Map of the United States showing the share of land cropped based on remote sensing data by county, with a legend indicating four levels of intensity from [0, .02] to (.42, .92].](uovp)

## (c) Share of Land Cropped (Admin)

![Map of the United States showing the share of land cropped based on administrative data by county, with a legend indicating four levels of intensity from [0, .01] to (.39, .9].](mzyd)

Notes: Panel (a) presents the share of land on agricultural tracts that bid for a CRP contract, by county. Panels (b) and (c) plot the share of land cropped on agricultural tracts by county, using the remote sensing (CDL) and admin (Form 578) data, averaged across the years in which auctions occurred. Crop classifications follow Lark et al. (2017) and exclude alfalfa and hay.

54

**Remote Sensing Data (CDL)** Our first source of land use data is the Cropland Data Layer (CDL) from 2009 to 2020. The CDL is derived from annual satellite imagery at a 30m by 30m resolution (approximately one quarter acre) for the entire contiguous US. The dataset classifies each pixel into over 50 crop categories and over 20 non-crop categories. The CDL is produced by the National Agricultural Statistics Service (NASS), and is trained on administrative data submitted to the USDA for crop insurance purposes (Form 578, discussed in more detail below).<sup>54</sup>

Our primary analysis aggregates CDL classifications into super-classes of crop versus non-crop, following (Lark et al., 2017). Also following Lark et al. (2017), our crop classification excludes alfalfa, hay, fallow, and idle cropland. The super-class accuracy of the CDL is high with > 95% average producer’s (classified as cropped when truly cropped) and user’s (truly cropped when classified as cropped) accuracy in the years 2008-2016 (Lark et al., 2017). Despite this high super-class accuracy, remote sensing classifications are subject to measurement error in classification (Alix-Garcia and Millimet, 2022; Torchiana et al., 2024), particularly when analyzing land use transitions. Moreover, in order to improve accuracy, some states in some years use prior years’ CDL as an input into the training algorithm, providing a further source of bias stemming from the classification algorithm.

We merge the CDL to a shapefile of all agricultural fields in the US, which we can then aggregate to landowners (tracts) using USDA identifiers. We merge the CDL data to the geocoded location of the bidder, time-stamped at the point of bidding. Calculating land use outcomes at the tract level as either the share of pixels that fall into the crop super-class, or a weighted average of field-level (binary) cropping indicators produce similar results. We use the former in our main specifications. Figure C.3b maps the geographic distribution of cropland, as measured by the CDL.

**Form 578 Administrative Data** Our second source of data (from Form 578) is the administrative data submitted to the USDA that the CDL is trained on. The data consist of annual field-level reports of total acreage cropped in detailed crop categories and enrollment in USDA programs. Though Form 578 is self-reported, crop insurance payouts depend on these reports. Unlike the CDL, which has coverage over the entire US, field-level data is only submitted if there is an incentive to do so, i.e. if it is cropped and covered by crop insurance. We assume that all non-reporting fields are not cropped. This is the primary limitation of the administrative data relative to the CDL. Figure C.3c maps the geographic distribution of cropland, as measured by the Form 578 data.

We merge the Form 578 administrative data to bidders based on field and tract identifiers. The USDA administrative data do not include consistent panel identifiers. We therefore construct a panel that tracks changes in field identifiers over time using their geolocation. This is further reason why we prefer the CDL as our baseline data source.

**NAIP Imagery** Our final dataset is derived from the National Agriculture Imagery Program (NAIP) collected via Google Earth Engine. The NAIP is administered through the Forest Service

<sup>54</sup>See https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php for more details and Metadata.

55

Agency (FSA) of the USDA, and collects 0.6-1m resolution images of all agricultural land during growing season. We obtain NAIP images for enrolled land (the highlighted green area in Figure C.1) to assess compliance with CRP rules. We use high-resolution photographs as classification error in the derived (CDL) data product would mechanically bias toward finding non-compliance.

Figure C.4: Sample Images

(a) Enrolled field (b) Cropped field

![Aerial photograph of an enrolled field with a red boundary](page_57_image_1_v2.jpg) ![Aerial photograph of a cropped field with a red boundary](page_57_image_2_v2.jpg)

Notes: Example images for classification. For compliance with data privacy, neither of these are actual images of CRP enrolled fields.

**Validating Compliance** To assess compliance, we hired and trained two MIT undergraduates (the “reviewers”) to classify high resolution aerial photographs (NAIP images) of fields at 1m resolution (see Figure C.4 for examples). We focus on the 2016 auction and images taken between 2017 and 2021. Before asking the reviewers to classify any images, we provided them with a test set of hundreds of images of cropped and uncropped fields across the US. The reviewers used this test set to familiarize themselves with the visual patterns of cropped fields (see Figure C.4b). We then provided each of the reviewers with over 1,000 images of CRP enrolled fields and hundreds of placebo cropped fields as attention checks. The reviewers were blind to whether the images were of CRP enrolled fields or placebo cropped fields. Each of the two reviewers were provided with the same images.

Table C.1 presents results for the classification exercise. We restrict to the 83% of CRP images that the reviewers agreed upon for our assessment of compliance to minimize the potential for classification error. We find only 5% of fields to be out of compliance in all post-period years. Once we drop the two “transition” years from 2017-2018, we find even lower rates of non-compliance, and reject rates of non-compliance above 3%. We attribute the difference between columns (1) and (2) to be driven by the fact that fields appear different when they are transitioning out of cropland,

56

e.g. rows from row cropping may still be visible as new vegetation grows in. Rates of cropping are classified as substantially higher, at approximately 40%, on placebo cropped fields indicating that the reviewers were making meaningful classifications. We note, however, that this number is far below 100%. This is because we instructed the reviewers to be conservative in their assessment of non-compliance, operating under the null hypothesis that the program is enforced.

An important caveat is that this exercise only studies compliance on the base action, land retirement, not the top-up actions, which we cannot observe. We thus use this assessment of compliance to make an inference about the overall compliance regime.

Table C.1: Validation of Compliance


<table>
  <thead>
    <tr>
        <th> </th>
        <th>All post-period years</th>
        <th>Drop first two years</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Share of contracted fields classified as cropped</td>
        <td>0.054</td>
        <td>0.024</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.008)</td>
        <td>(0.005)</td>
    </tr>
    <tr>
        <td>Upper bound of 95% CI</td>
        <td>0.070</td>
        <td>0.034</td>
    </tr>
    <tr>
        <td>N fields classified (with agreement)</td>
        <td>925</td>
        <td>842</td>
    </tr>
    <tr>
        <td>Rate of agreement across reviewers</td>
        <td>0.825</td>
        <td>0.863</td>
    </tr>
  </tbody>
</table>


Notes: Table presents results from an exercise classifying aerial photographs of contracted fields as cropped or non-cropped among two reviewers, who also reviewed images of non-CRP fields and were blind to the distinction. Classification focuses on the 2016 auction. Column (1) includes photographs from 2017-2021. Column (2) includes only photographs from 2019-2021. Crop classifications are based on only fields in which the two reviewers agree (which occurred for 83-86% of fields). Fields more likely to be flagged as non-compliant (based on remote sensing data) were over-sampled, to be as conservative as possible.

57

# D Supplemental Figures and Tables

Figure D.1: Spillovers: Cropping on Non-Bid Fields


<table>
  <thead>
    <tr>
        <th colspan="3">(a) Remote sensing</th>
        <th colspan="3">(b) Admin</th>
    </tr>
    <tr>
        <th>Score relative to win threshold</th>
        <th>Share of land cropped (Left of 0)</th>
        <th>Share of land cropped (Right of 0)</th>
        <th>Score relative to win threshold</th>
        <th>Share of land cropped (Left of 0)</th>
        <th>Share of land cropped (Right of 0)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-40</td>
        <td>.25</td>
        <td> </td>
        <td>-40</td>
        <td>.20</td>
        <td> </td>
    </tr>
    <tr>
        <td>-20</td>
        <td>.23</td>
        <td> </td>
        <td>-20</td>
        <td>.19</td>
        <td> </td>
    </tr>
    <tr>
        <td>0</td>
        <td>.21</td>
        <td>.21</td>
        <td>0</td>
        <td>.16</td>
        <td>.16</td>
    </tr>
    <tr>
        <td>20</td>
        <td> </td>
        <td>.23</td>
        <td>20</td>
        <td> </td>
        <td>.17</td>
    </tr>
    <tr>
        <td>40</td>
        <td> </td>
        <td>.24</td>
        <td>40</td>
        <td> </td>
        <td>.18</td>
    </tr>
  </tbody>
</table>


Notes: Panels (a) and (b) present raw data and estimates from equation (7) for $r(i, t) > 0$ (post-auction). Regression is estimated at the bidder level, restricting to non-bid fields among bidding landowners. Estimates are restricted to the 2016 auction where delineations of bid and non-bid fields are observed. Land-use outcomes are measured as the share of the bidding land that is cropped using the remote sensing data (a) and administrative data (b). The running variable is the difference between each bidder's score and the threshold score. Positive numbers on the x-axis correspond to winning scores, negative numbers correspond to losing scores. Each observation is a bidder-year. Corresponding coefficient estimates and standard errors presented in Table 2.

58

# Figure D.2: Additional RD Plots: Remote-Sensing Data

![Three regression discontinuity plots showing the share of land for cropping corn, cropping soybeans, and fallow land relative to a win threshold.](page_60_image_1_v2.jpg)

Notes: Figure presents raw data and estimates from equation (7) for $r(i, t) > 0$ (post-auction). Land-use outcomes are measured using crop classifications in the remote sensing data. The running variable is the difference between each bidder's score and the threshold score. Positive numbers on the x-axis correspond to winning scores, negative numbers correspond to losing scores. Each observation is a bidder-year. Corresponding coefficient estimates and standard errors presented in Table 2.

59

# Figure D.3: Additional RD Plots: Admin Data

![Four regression discontinuity plots showing land-use outcomes (Cropping, Cropping corn, Cropping soybeans, and Fallow) relative to a win threshold score. Each plot displays raw data points and fitted regression lines with a discontinuity at the zero threshold.](page_61_image_1_v2.jpg)

Notes: Figure presents raw data and estimates from equation (7) for $r(i, t) > 0$ (post-auction). Land-use outcomes are measured using crop classifications in the Form 578 data reported to the USDA. The running variable is the difference between each bidder's score and the threshold score. Positive numbers on the x-axis correspond to winning scores, negative numbers correspond to losing scores. Each observation is a bidder-year. Corresponding coefficient estimates and standard errors presented in Table 2.

60

# Figure D.4: Re-bidding Hazard


(a) All losing bidders

<table>
  <thead>
    <tr>
        <th>Years since index bid</th>
        <th>Bid only</th>
        <th>Bid and win</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1</td>
        <td>0.10</td>
        <td>0.06</td>
    </tr>
    <tr>
        <td>2</td>
        <td>0.10</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.11</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.22</td>
        <td>0.14</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.22</td>
        <td>0.14</td>
    </tr>
  </tbody>
</table>



(b) Losing bidders near winning score threshold

<table>
  <thead>
    <tr>
        <th>Years since index bid</th>
        <th>Bid only</th>
        <th>Bid and win</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1</td>
        <td>0.09</td>
        <td>0.05</td>
    </tr>
    <tr>
        <td>2</td>
        <td>0.10</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.11</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.14</td>
        <td>0.14</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.14</td>
        <td>0.14</td>
    </tr>
  </tbody>
</table>


Notes: Figure plots the share of losing bidders who have rebid at least once in the years following an index auction, split by all bidders (beige) and successful bidders (blue). Panel (a) includes all losing bidders. Panel (b) restricts to bidders near (within 25 points) of the winning score threshold.

# Table D.1: RD Estimates: By Win Threshold of Bid Rental Rate for Base Contract


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">Remote-sensing</th>
        <th colspan="2">Admin</th>
    </tr>
    <tr>
        <th> </th>
        <th>Effect on share cropped</th>
        <th>Additionality</th>
        <th>Effect on share cropped</th>
        <th>Additionality</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Quartile 1 threshold bid (lowest)</td>
        <td>-0.039</td>
        <td>0.139</td>
        <td>-0.064</td>
        <td>0.227</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.013)</td>
        <td> </td>
        <td>(0.014)</td>
        <td> </td>
    </tr>
    <tr>
        <td>Quartile 2 threshold bid</td>
        <td>-0.059</td>
        <td>0.190</td>
        <td>-0.072</td>
        <td>0.231</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td> </td>
        <td>(0.013)</td>
        <td> </td>
    </tr>
    <tr>
        <td>Quartile 3 threshold bid</td>
        <td>-0.031</td>
        <td>0.126</td>
        <td>-0.046</td>
        <td>0.185</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td> </td>
        <td>(0.013)</td>
        <td> </td>
    </tr>
    <tr>
        <td>Quartile 4 threshold bid (highest)</td>
        <td>-0.075</td>
        <td>0.403</td>
        <td>-0.094</td>
        <td>0.507</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.015)</td>
        <td> </td>
        <td>(0.015)</td>
        <td> </td>
    </tr>
  </tbody>
</table>


Notes: Table presents pooled RD coefficients (Equation (7) for $r(i, t) > 0$ (post-auction) split by the bid rental rate required to achieve the threshold score with the base contract (quartiles). Columns (1) and (3) present estimates of the share of bidders' land that is cropped, and columns (2) and (4) present implied additionality estimates based on the share of bidders' land that obtains a CRP contract (in that quartile). This parameterizes heterogeneity in the location of the discontinuity across auctions and variation within auctions across bidders (based on $z_i^s$). Standard errors clustered at the bidder level.

61

# Table D.2: RD Estimates by Auction


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">Share cropped</th>
        <th>Share of land contracted</th>
        <th colspan="2">Additionality</th>
        <th>Share awarded contract</th>
    </tr>
    <tr>
        <th> </th>
        <th>Remote sensing</th>
        <th>Admin</th>
        <th> </th>
        <th>Remote sensing</th>
        <th>Admin</th>
        <th> </th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
        <th>(6)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Most restrictive: 2016</td>
        <td>-0.048<br/>(0.016)</td>
        <td>-0.079<br/>(0.015)</td>
        <td>0.389<br/>(0.013)</td>
        <td>0.122</td>
        <td>0.204</td>
        <td>0.185</td>
    </tr>
    <tr>
        <td>2011</td>
        <td>-0.068<br/>(0.012)</td>
        <td>-0.089<br/>(0.013)</td>
        <td>0.396<br/>(0.008)</td>
        <td>0.173</td>
        <td>0.226</td>
        <td>0.775</td>
    </tr>
    <tr>
        <td>2013</td>
        <td>-0.064<br/>(0.016)</td>
        <td>-0.106<br/>(0.017)</td>
        <td>0.323<br/>(0.009)</td>
        <td>0.197</td>
        <td>0.329</td>
        <td>0.870</td>
    </tr>
    <tr>
        <td>2012</td>
        <td>-0.077<br/>(0.014)</td>
        <td>-0.104<br/>(0.015)</td>
        <td>0.354<br/>(0.011)</td>
        <td>0.217</td>
        <td>0.294</td>
        <td>0.877</td>
    </tr>
    <tr>
        <td>Least restrictive: 2010</td>
        <td>-0.109<br/>(0.020)</td>
        <td>-0.120<br/>(0.020)</td>
        <td>0.261<br/>(0.011)</td>
        <td>0.416</td>
        <td>0.461</td>
        <td>0.918</td>
    </tr>
  </tbody>
</table>


Notes: Table presents RD coefficients (Equation (7) for $r(i, t) > 0$ (post-auction) for each of the five auctions with post-period land use data. Each row is an auction, ordered by the share awarded contracts. Columns (1)-(2) present estimates of the share of bidders' land that is cropped (measured in the remote sensing data and admin data), column (3) presents estimates of the share of land covered by a CRP contract, columns (4)-(5) report implied additionality estimates. Column (6) reports the share awarded a contract. Standard errors clustered at the bidder level.

62

Table D.3: RD Coefficient Estimates: Weighted by CRP Acreage Offered


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Remote sensing</th>
        <th>Admin</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
    <tr>
        <th colspan="3"><strong>Panel A: Share of land cropped vs. contracted</strong></th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Share of land cropped pre-auction (placebo)</td>
        <td>0.010</td>
        <td>0.021</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.013)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (pooled)</td>
        <td>-0.115</td>
        <td>-0.153</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.008)</td>
        <td>(0.008)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (average year effects)</td>
        <td>-0.122</td>
        <td>-0.157</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.008)</td>
        <td>(0.008)</td>
    </tr>
    <tr>
        <td>Share of land contracted post-auction</td>
        <td>0.544</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.006)</td>
        <td> </td>
    </tr>
    <tr>
        <th colspan="3"><strong>Panel B: Other outcomes</strong></th>
    </tr>
    <tr>
        <td>Share of land cropped corn</td>
        <td>-0.020</td>
        <td>-0.029</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land cropped soybeans</td>
        <td>-0.025</td>
        <td>-0.034</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land fallow</td>
        <td>-0.015</td>
        <td>-0.024</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.002)</td>
        <td>(0.002)</td>
    </tr>
    <tr>
        <td>Share of land in natural vegetation or grassland</td>
        <td>0.143</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.009)</td>
        <td> </td>
    </tr>
    <tr>
        <th colspan="3">Observations in 25-point window</th>
    </tr>
    <tr>
        <td>N bidders</td>
        <td>41,269</td>
        <td>41,269</td>
    </tr>
    <tr>
        <td>N bidder-years</td>
        <td>495,228</td>
        <td>288,883</td>
    </tr>
  </tbody>
</table>


*Notes:* Table presents coefficient estimates from equations (6) and (7) with land use outcomes measured in the remotely sensed (column (1)) and administrative (column (2)) data, weighted by the CRP offered acreage share. All results use a local linear regression on either side of the win threshold in the MSE-optimal bandwidth (Calonico et al., 2014). All estimates are from equation (7), except the "average year effects" row, which presents the average of the post-period year effects (coefficient estimates of $\beta_{r(i,t)}$ from (6)) weighting each year equally. The pooled post-period includes an average of 7-8 post-auction years. Natural vegetation or grassland is only observed in remotely sensed data. Share of land contracted post auction estimates the share of bidders' land contracting among winning bidders. Standard errors are clustered at the bidder level. Estimates of additionality are calculated by dividing the estimates for the share of land cropped by the estimates of the share of land contracted and equal 21-22% (remote sensing) and 28-29% (admin). Estimates using the share of land in natural vegetation or grasslands yields a similar estimate (26%).

63

# Table D.4: RD Coefficient Estimates | Bid $\geq$ Five Acres of Land


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Remote sensing</th>
        <th>Admin</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="3"><strong>Panel A: Share of land cropped vs. contracted</strong></td>
    </tr>
    <tr>
        <td>Share of land cropped pre-auction (placebo)</td>
        <td>0.005</td>
        <td>0.006</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.006)</td>
        <td>(0.010)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (pooled)</td>
        <td>-0.076</td>
        <td>-0.102</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.006)</td>
    </tr>
    <tr>
        <td>Share of land cropped post-auction (average year effects)</td>
        <td>-0.083</td>
        <td>-0.106</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.007)</td>
    </tr>
    <tr>
        <td>Share of land contracted post-auction</td>
        <td>0.345</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.004)</td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="3"><strong>Panel B: Other outcomes</strong></td>
    </tr>
    <tr>
        <td>Share of land cropped corn</td>
        <td>-0.016</td>
        <td>-0.022</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land cropped soybeans</td>
        <td>-0.021</td>
        <td>-0.026</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>Share of land fallow</td>
        <td>-0.009</td>
        <td>-0.014</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.002)</td>
        <td>(0.002)</td>
    </tr>
    <tr>
        <td>Share of land in natural vegetation or grassland</td>
        <td>0.097</td>
        <td> </td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="3"><strong>Panel C: Spillovers to non-bid fields</strong></td>
    </tr>
    <tr>
        <td>Share of non-bid fields cropped</td>
        <td>-0.007</td>
        <td>-0.007</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td>(0.011)</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Observations in 25-point window</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>N bidders</td>
        <td>37,776</td>
        <td>37,776</td>
    </tr>
    <tr>
        <td>N bidder-years</td>
        <td>453,312</td>
        <td>264,432</td>
    </tr>
    <tr>
        <td>Spillovers (bid fields observed): N bidders</td>
        <td>6,770</td>
        <td>6,770</td>
    </tr>
    <tr>
        <td>Spillovers (bid fields observed): N bidder-years</td>
        <td>81,240</td>
        <td>47,390</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents coefficient estimates from equations (6) and (7) with land use outcomes measured in the remotely sensed (column (1)) and administrative (column (2)) data, restricted to bidders who bid more then five acres into the mechanism (following Lark et al. (2017)). All results use a local linear regression on either side of the win threshold in the MSE-optimal bandwidth (Calonico et al., 2014). All estimates are from equation (7), except the "average year effects" row, which presents the average of the post-period year effects (coefficient estimates of $\beta_{r(i,t)}$ from (6)) weighting each year equally. The pooled post-period includes an average of 7-8 post-auction years. Natural vegetation or grassland is only observed in remotely sensed data. Share of land contracted post auction estimates the share of bidders' land contracting among winning bidders. Standard errors are clustered at the bidder level. Estimates of additionality are calculated by dividing the estimates for the share of land cropped by the estimates of the share of land contracted and equal 22-24% (remote sensing) and 30-31% (admin). Estimates using the share of land in natural vegetation or grasslands yields a similar estimate (28%).

64

# Figure D.5: Testing for Asymmetric Information, Admin Data


(a) Additionality vs. bids

<table>
  <thead>
    <tr>
        <th>Bid ($/acre/year)</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>30</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>40</td>
        <td>0.14</td>
    </tr>
    <tr>
        <td>45</td>
        <td>0.10</td>
    </tr>
    <tr>
        <td>48</td>
        <td>0.15</td>
    </tr>
    <tr>
        <td>52</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>55</td>
        <td>0.13</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.20</td>
    </tr>
    <tr>
        <td>70</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>80</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>92</td>
        <td>0.29</td>
    </tr>
    <tr>
        <td>105</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>115</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>125</td>
        <td>0.39</td>
    </tr>
    <tr>
        <td>135</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>145</td>
        <td>0.39</td>
    </tr>
    <tr>
        <td>155</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>165</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>180</td>
        <td>0.31</td>
    </tr>
    <tr>
        <td>195</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>205</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>215</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>235</td>
        <td>0.38</td>
    </tr>
  </tbody>
</table>



(b) Additionality vs. bids | observables

<table>
  <thead>
    <tr>
        <th>Bid ($/acre/year)</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>35</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>40</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>45</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>55</td>
        <td>0.18</td>
    </tr>
    <tr>
        <td>60</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>70</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>80</td>
        <td>0.24</td>
    </tr>
    <tr>
        <td>98</td>
        <td>0.29</td>
    </tr>
    <tr>
        <td>115</td>
        <td>0.30</td>
    </tr>
    <tr>
        <td>132</td>
        <td>0.31</td>
    </tr>
    <tr>
        <td>145</td>
        <td>0.35</td>
    </tr>
    <tr>
        <td>158</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>175</td>
        <td>0.36</td>
    </tr>
    <tr>
        <td>195</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>210</td>
        <td>0.40</td>
    </tr>
    <tr>
        <td>225</td>
        <td>0.41</td>
    </tr>
    <tr>
        <td>240</td>
        <td>0.41</td>
    </tr>
  </tbody>
</table>



(c) Additionality across contracts

<table>
  <thead>
    <tr>
        <th>Contract</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Native grasses</td>
        <td>-0.05</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>-0.23</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>-0.05</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>-0.02</td>
    </tr>
    <tr>
        <td>Food plot</td>
        <td>-0.03</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>-0.01</td>
    </tr>
  </tbody>
</table>



(d) Observable predictors of additionality

<table>
  <thead>
    <tr>
        <th>Deciles of estimated soil productivity</th>
        <th>Share cropped absent contract (1 - a₀)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>2</td>
        <td>0.08</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.14</td>
    </tr>
    <tr>
        <td>6</td>
        <td>0.16</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>8</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>10</td>
        <td>0.30</td>
    </tr>
  </tbody>
</table>


Notes: Figures present estimates from regression specifications in equation (8). Regressions in all panels control for landowner characteristics in the scoring rule: whether a bidder is in a wildlife priority zone, estimates of groundwater quality, estimates of surface water quality, estimates of wind and water erosion (deciles), air quality impacts, and whether or not a bidder is in an air quality zone. The outcome variable in all panels is a landowner-specific measure of additionality, calculated as the share of all fields bid into the CRP mechanism that are cropped post auction for rejected landowners. The sample is restricted to the 2016 auction, in which 82% of bidders are rejected and the delineations of bid fields are observed. Cropping on bid fields is measured in 2017-2019 in the administrative data, with each field and year equally weighted. Panel (a) is a binned scatterplot correlating the dollar bid (per acre, per year) with additionality, conditional on characteristics included in the scoring rule. Panel (b) adds controls for interaction terms of prior land use (quartiles of share of land cropped prior to bidding and re-enrolling CRP status) and deciles of estimated soil productivity. Panel (c) plots relative additionality by the chosen contract in the bid, relative to an omitted category of introduced grasses, conditional on characteristics included in the scoring rule. Panel (d) plots relative additionality by deciles of estimated soil productivity, conditional on characteristics included in the scoring rule. Standard errors clustered at the bidder level.

65

Figure D.6: RD Estimates: Tree-Related Contracts


<table>
  <tbody>
    <tr>
        <td>Auction</td>
        <td>Estimate</td>
    </tr>
    <tr>
        <td>Auction 1</td>
        <td>-0.010</td>
    </tr>
    <tr>
        <td>Auction 2</td>
        <td>-0.012</td>
    </tr>
    <tr>
        <td>Auction 3</td>
        <td>0.016</td>
    </tr>
    <tr>
        <td>Auction 4</td>
        <td>0.028</td>
    </tr>
    <tr>
        <td>Auction 5</td>
        <td>0.028</td>
    </tr>
  </tbody>
</table>


Notes: Figures presents RD coefficients (Equation (7) for $r(i, t) > 0$ (post-auction) for each of the five auctions with post-period land use data, restricted to bids for tree-related contracts. Each estimate is an auction, ordered over time from left to right. The outcome is the share of bidders’ land that is cropped (measured in the remote sensing data). Standard errors clustered at the bidder level. F statistic testing joint equality of coefficients across auctions (restricting to bids for tree contracts): 0.64.

66

Figure D.7: Tract-level Heterogeneity Across Auctions


### (a) By Estimates of Soil Productivity
<table>
  <thead>
    <tr>
        <th>Category</th>
        <th>Auction 1</th>
        <th>Auction 2</th>
        <th>Auction 3</th>
        <th>Auction 4</th>
        <th>Auction 5</th>
        <th>Auction 6</th>
        <th>Auction 7</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Quartile 2</td>
        <td>.03</td>
        <td>.04</td>
        <td>.04</td>
        <td>.01</td>
        <td>-.01</td>
        <td>.06</td>
        <td>.05</td>
    </tr>
    <tr>
        <td>Quartile 3</td>
        <td>.04</td>
        <td>.03</td>
        <td>.03</td>
        <td>.02</td>
        <td>.07</td>
        <td>.16</td>
        <td>.09</td>
    </tr>
    <tr>
        <td>Quartile 4</td>
        <td>.19</td>
        <td>.15</td>
        <td>.11</td>
        <td>.12</td>
        <td>.16</td>
        <td>.23</td>
        <td>.22</td>
    </tr>
  </tbody>
</table>


### (b) By Former CRP
<table>
  <thead>
    <tr>
        <th>Category</th>
        <th>Auction 1</th>
        <th>Auction 2</th>
        <th>Auction 3</th>
        <th>Auction 4</th>
        <th>Auction 5</th>
        <th>Auction 6</th>
        <th>Auction 7</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Former CRP</td>
        <td>-.20</td>
        <td>-.23</td>
        <td>-.20</td>
        <td>-.14</td>
        <td>-.19</td>
        <td>-.22</td>
        <td>-.16</td>
    </tr>
  </tbody>
</table>


### (c) By Prior Cropping Share
<table>
  <thead>
    <tr>
        <th>Category</th>
        <th>Auction 1</th>
        <th>Auction 2</th>
        <th>Auction 3</th>
        <th>Auction 4</th>
        <th>Auction 5</th>
        <th>Auction 6</th>
        <th>Auction 7</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Quartile 2</td>
        <td>.03</td>
        <td>.01</td>
        <td>.01</td>
        <td>.02</td>
        <td>.01</td>
        <td>.01</td>
        <td>.01</td>
    </tr>
    <tr>
        <td>Quartile 3</td>
        <td>.18</td>
        <td>.17</td>
        <td>.17</td>
        <td>.16</td>
        <td>.21</td>
        <td>.18</td>
        <td>.17</td>
    </tr>
    <tr>
        <td>Quartile 4</td>
        <td>.60</td>
        <td>.62</td>
        <td>.63</td>
        <td>.64</td>
        <td>.66</td>
        <td>.64</td>
        <td>.68</td>
    </tr>
  </tbody>
</table>


*Notes*: Figures present tract-level heterogeneity in cropping rates (measured in the remote sensing data) for $r(i, t) = 0$ (and $r(i, t) = -1$ for the 2021 auction, to be able to compare all seven auctions) across observable characteristics, by auction, ordered over time from left to right. Cropping rates reported relative to the omitted category: the lowest quartile of soil productivity (a), a new CRP participant (b), and the lowest quartile of share cropped prior to bidding (c). Standard errors clustered at the bidder level. F statistics testing joint equality of coefficients across auctions: 111 (a), 61 (b), 67 (c).

# E Model and Estimation Details

Figure E.1 provides empirical support for the assumed uncertainty in quantity cleared. Each Farm Bill specifies the total acreage that can be covered by CRP contracts at a given point in time. The quantity cleared in the auction is therefore determined jointly by the Farm Bill and prior contract expirations. It can vary substantially in ways that may not be obvious to bidders submitting their bids. To demonstrate this, we compare the 2013 and 2016 auctions. These two auctions had very different quantity thresholds, and thus very different threshold scores, denoted by the dashed lines in blue and beige. However, consistent with bidder uncertainty about the ex-post threshold score, the cumulative distribution functions (CDFs) of bidder scores across the two auctions are similar.

67

Figure E.1: CDF of Scores versus Winning Thresholds: 2013 versus 2016


<table>
  <thead>
    <tr>
        <th>Score</th>
        <th>2013</th>
        <th>2016</th>
        <th>2013 Threshold</th>
        <th>2016 Threshold</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>100</td>
        <td>0.00</td>
        <td>0.00</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.10</td>
        <td>0.15</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>210</td>
        <td> </td>
        <td> </td>
        <td>Threshold</td>
        <td> </td>
    </tr>
    <tr>
        <td>300</td>
        <td>0.85</td>
        <td>0.80</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>295</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>Threshold</td>
    </tr>
    <tr>
        <td>400</td>
        <td>1.00</td>
        <td>1.00</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>500</td>
        <td>1.00</td>
        <td>1.00</td>
        <td> </td>
        <td> </td>
    </tr>
  </tbody>
</table>


Notes: Figure presents ex-post win thresholds and cumulative distribution functions (CDFs) of ex-ante score distributions for the 2013 and 2016 auctions.

68

# Figure E.2: Scoring Rule Variation: Summarized

![Three bar charts showing scoring rule variation across 36 contracts for different target scores: (a) Target 2016 Threshold Score, (b) Target 1st Percentile Score, and (c) Target Average Threshold Score. Each chart plots Bid ($) on the y-axis against Contract number on the x-axis.](page_70_image_1_v2.jpg)

*Notes*: Figure presents sources of policy variation in the scoring rule by plotting the dollar bid (per acre, per year) required to reach a target score across all 36 contracts across variations of the scoring rule (see also Figure E.3). Conversion of points to bids at the threshold score calculated for a landowner with average land-based points and rate of substitution between points and dollar bids (abstracting from the kinked incentives in points bidders receive as a percentage of their bidcap, see Appendix F for more detail). Each set of 36 colored vertical bars represents a different variation of the scoring rule. Panels (a), (b), and (c) present the same sources of variation at the levels associated with different target scores: the 2016 threshold score (a), the lowest percentile score (b), and the average non-2016 threshold score (c).

69

Figure E.3: Specific Sources of Variation in the Scoring Rule

(a) Wildlife Priority Zone Variation


<table>
  <tbody>
    <tr>
        <td>Sign-up</td>
        <td>Treated actions, WPZ</td>
        <td>Untreated actions, WPZ</td>
        <td>Treated actions, non-WPZ</td>
        <td>Untreated actions, non-WPZ</td>
    </tr>
    <tr>
        <td>2010</td>
        <td>80</td>
        <td>100</td>
        <td>73</td>
        <td>100</td>
    </tr>
    <tr>
        <td>2011</td>
        <td>50</td>
        <td>100</td>
        <td>75</td>
        <td>100</td>
    </tr>
    <tr>
        <td>2015</td>
        <td>50</td>
        <td>100</td>
        <td>75</td>
        <td>100</td>
    </tr>
    <tr>
        <td>2020</td>
        <td>50</td>
        <td>100</td>
        <td>75</td>
        <td>100</td>
    </tr>
  </tbody>
</table>


(b) Mid-Mechanism Policy Change


<table>
  <tbody>
    <tr>
        <td>Cover</td>
        <td>Interim mechanism</td>
        <td>Final mechanism</td>
    </tr>
    <tr>
        <td>G1</td>
        <td>38</td>
        <td>39</td>
    </tr>
    <tr>
        <td>G2</td>
        <td>12</td>
        <td>11</td>
    </tr>
    <tr>
        <td>G3</td>
        <td>44</td>
        <td>44</td>
    </tr>
    <tr>
        <td>G4</td>
        <td>33</td>
        <td>37</td>
    </tr>
    <tr>
        <td>T1</td>
        <td>57</td>
        <td>61</td>
    </tr>
    <tr>
        <td>T2</td>
        <td>40</td>
        <td>43</td>
    </tr>
    <tr>
        <td>T3</td>
        <td>45</td>
        <td>50</td>
    </tr>
    <tr>
        <td>T4</td>
        <td>60</td>
        <td>60</td>
    </tr>
    <tr>
        <td>T5</td>
        <td>39</td>
        <td>39</td>
    </tr>
    <tr>
        <td>H1</td>
        <td>44</td>
        <td>48</td>
    </tr>
    <tr>
        <td>H2</td>
        <td>55</td>
        <td>59</td>
    </tr>
    <tr>
        <td>H3</td>
        <td>38</td>
        <td>40</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents sources of policy variation in the scoring rule that yield variation in payments across contracts differentiated by top-up conservation actions. Panel (a) plots average action points awarded for a set of “treated” actions, actions for which after the 2011 auction WPZ bidders no longer got WPZ points, and “untreated” actions, whose points remained the same, and the same average action points for non-WPZ bidders. Panel (b) plots the average rental rate that would be received for a target score among bidders under the interim mechanism before the introduction of Climate Smart Practice Incentives, and in the final mechanism after their introduction, for each of the twelve primary covers. G indicates grasses, T indicates trees, H indicates habitats.

# Estimation

**Step 0: Constructing the Scoring Rule** We only observe scores for chosen bids **b**<sub>i</sub>, so we construct the function $s(\mathbf{b}_i, \mathbf{z}_i^s)$ from the EBI Factsheets. Figure E.4 confirms that our reconstruction performs well: at observed actions, our scoring-rule-implied required bid rental rate to achieve the score chosen in the data predicts the observed bid rental rate with an $R^2$ of over 0.99.

Figure E.4: True versus Predicted Bid Rental Rate at Observed Scores and Contracts


<table>
  <tbody>
    <tr>
        <td>Predicted $ bid</td>
        <td>Actual $ bid</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>50</td>
        <td>50</td>
    </tr>
    <tr>
        <td>100</td>
        <td>100</td>
    </tr>
    <tr>
        <td>150</td>
        <td>150</td>
    </tr>
    <tr>
        <td>200</td>
        <td>200</td>
    </tr>
    <tr>
        <td>250</td>
        <td>250</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents a scatter plot of true versus predicted bid rental rates at observed contract and score choices to validate the construction of $s(\mathbf{b}_i, \mathbf{z}_i^s)$. Each dot represents 11 observations (per disclosure requirements).

70

**Step 1: Obtain Bidder Beliefs via Simulation** Our resampling procedure to simulate the probability of winning at any score, $G(S)$ follows Hortaçsu (2000); Hortaçsu and McAdams (2010). Specifically, we:

1. Fit a Beta distribution to the observed distribution of acreage thresholds across auctions. For this step, we use additional historic data on auctions starting in 2000. This provides us with 11 auctions.

2. Fit a Beta distribution to the observed distribution of number of opposing bidders across auctions. For this step, we again use additional historic data on auctions starting in 2000. This provides us with 11 auctions.

3. Draw an acreage threshold from the distribution fit in Step 1 and the number of opposing bidders, $N$, in Step 2. Then, for each auction $g$, sample with replacement $N$ bidders from the empirical distribution of bidders in that auction. Given the joint distribution of scores and acreage amounts among the $N$ resampled bidders, and the drawn acreage threshold, find the winning score threshold $S$.

4. Repeat Step 3 and obtain an auction specific probability of winning at any given score $G_g(S)$ using a kernel density fit to the distribution of simulated winning scores.<sup>55</sup>

Bidders form expectations about the distribution of competing scores without knowledge of their competitors’ identities or characteristics, consistent with the large and decentralized bidding process, so $G_g(S)$ is the same for all bidders. Figure E.5 plots the output of our simulation procedure for each auction (a) and compares the average CDF (across auctions) to the overall empirical score distribution (b).

Figure E.5: Probability of Winning at Score S

(a) $G(S)$ across auctions


<table>
  <thead>
    <tr>
        <th>S</th>
        <th>CDF</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>100</td>
        <td>0.15</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.25</td>
    </tr>
    <tr>
        <td>300</td>
        <td>0.85</td>
    </tr>
    <tr>
        <td>400</td>
        <td>1.00</td>
    </tr>
  </tbody>
</table>


(b) $G(S)$ vs. empirical score distribution


<table>
  <thead>
    <tr>
        <th>S</th>
        <th>CDF</th>
        <th>Frac</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>100</td>
        <td>0.22</td>
        <td>0.005</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.35</td>
        <td>0.045</td>
    </tr>
    <tr>
        <td>300</td>
        <td>0.80</td>
        <td>0.035</td>
    </tr>
    <tr>
        <td>400</td>
        <td>1.00</td>
        <td>0.002</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents CDFs of the simulated distribution of win probabilities at a given score across auctions (a) and on average versus the average empirical score distribution (b).

\*55 We use a Gaussian kernel and a bandwidth of 5 points.

71

We assume that bidders are able to form expectations about the distribution of competing scores. This might be challenging when the distribution of scores changes across auctions. To assess the implications for our estimates, we re-estimate costs using beliefs based on the empirical distribution in the prior auction (versus the current auction, as in baseline). Table E.1 compares results.

Table E.1: Mean Landowner Costs of Accepting CRP Contracts under Alternative Beliefs


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">Current auction beliefs</th>
        <th colspan="2">Prior auction beliefs</th>
    </tr>
    <tr>
        <th> </th>
        <th>All</th>
        <th>Bidders with &gt;<br/>median soil<br/>productivity</th>
        <th>All</th>
        <th>Bidders with &gt;<br/>median soil<br/>productivity</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Base cost ($c_i$)</td>
        <td>67.19</td>
        <td>90.86</td>
        <td>69.57</td>
        <td>97.69</td>
    </tr>
    <tr>
        <td>Top-up cost ($\kappa_{ij}$)</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Native grasses</td>
        <td>0.02</td>
        <td>3.91</td>
        <td>0.17</td>
        <td>4.50</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>23.57</td>
        <td>26.96</td>
        <td>23.76</td>
        <td>27.20</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>15.05</td>
        <td>18.34</td>
        <td>14.93</td>
        <td>18.29</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>15.59</td>
        <td>19.07</td>
        <td>15.68</td>
        <td>19.24</td>
    </tr>
    <tr>
        <td>Wildlife food plot</td>
        <td>18.87</td>
        <td>15.34</td>
        <td>18.80</td>
        <td>15.10</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>18.18</td>
        <td>17.56</td>
        <td>20.79</td>
        <td>18.33</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents estimated mean landowner costs for the base cost $c_i$ and top-up cost $\kappa_{ij}$ in dollars per acre per year. The base contract of introduced grasses is normalized to zero. The cost of each contract $j$ is defined as $c_i + \kappa_{ij}$. Costs are estimated using revealed preferences in optimal bidding (equation (10)) given beliefs from the baseline resampling procedure (columns (1) and (2)) and a modified resampling procedure where the distribution of competing bidders from the prior auction is used (columns (3) and (4)). Columns (1) and (3) presents mean costs for all bidders across all auctions (excluding the first auction) and columns (2) and (4) restricts to bidders with above median soil productivity.

## Step 2: Estimate $F_{c,\kappa|\mathbf{z}}$

Our estimation procedure is as follows:

1. **Construct a proposal distribution.** We begin by constructing a proposal distribution from which to draw proposal $(c_i, \kappa_i)$ draws. We obtain our proposal distribution by estimating a simplified version of the model. Bidders choose a score using only their expectations of their $\kappa_{ij}$ draws, then given that score, choose an optimal contract. In this model, estimation of $\kappa_{ij}$ and $c_i$ can be separated into a discrete choice problem and an inversion. We obtain parameter estimates from this simplified model, then set our proposal distribution to be independent normals with the estimated means and variances of this simplified model.

2. **Draw from proposal and solve the bidder’s problem.** Following the approach of Ackerberg (2009), we use a change of variables to draw simulations of $(c_i^k, \kappa_i^k)$ from the proposal distribution and solve the bidder’s problem in equation (10) for each bidder and each simulation draw. Bidders can only bid integer scores, so to solve equation (10), we search over all feasible score-contract combinations among integers in the support of observed scores. This change of variables allows us to solve the bidder’s problem only $N \times K$ times, once for each bidder and each simulation draw, instead of $N \times K \times R$ times, for each evaluation of the objective function ($R$ times).

3. **Coarsen choice probabilities.** Because the number of possible bids is large (on the order

72

of 10,000 choices), we face the challenge that the probability of simulating each bid observed in the data is low. We address this challenge by coarsening the bidder’s solution obtained in Step 2. We coarsen to the cartesian product of (i) deciles of the scoring rule and (ii) the five dimensions of $p_j$ when $u_j$ is the no upgrade option, plus the two upgrade options. Let $\widetilde{\mathbf{b}}_i^* = \left( \widetilde{S}_i, \widetilde{\mathbf{x}}_i \right)$ denote the optimal coarsened bid observed in the data.

4. **Reweight simulation draws.** We can then construct the importance sampling estimator by re-weighting simulation draws. The likelihood of observing the coarsened choice in the data, $\widetilde{\mathbf{b}}_i^* = \left( \widetilde{S}_i, \widetilde{\mathbf{x}}_i \right)$, given a parameter guess $\theta$, is:

$$ (21) \quad \mathcal{L}_i = \frac{1}{K} \sum \mathbb{1} \left( \widetilde{\mathbf{b}}_i^* = \widetilde{\mathbf{b}}_i^{*k} \mid \left( c_i^k, \kappa_i^k \right) \right) \frac{p \left( \left( c_i^k, \kappa_i^k \right) \mid \theta \right)}{g \left( \left( c_i^k, \kappa_i^k \right) \right)}, $$

where $\widetilde{\mathbf{b}}_i^{*k}$ is the coarsened optimal bid given simulation draw $\left( c_i^k, \kappa_i^k \right)$, the solution to the bidder’s problem in equation (10), and the coarsening described in Step 3. Equation (21) then re-weights simulation draws by $\frac{p \left( \left( c_i^k, \kappa_i^k \right) \mid \theta \right)}{g \left( \left( c_i^k, \kappa_i^k \right) \right)}$, where $p \left( \left( c_i^k, \kappa_i^k \right) \mid \theta \right)$ is the probability of observing simulation draw $\left( c_i^k, \kappa_i^k \right)$ given parameter guess $\theta$, and $g \left( \left( c_i^k, \kappa_i^k \right) \right)$ the probability of observing $\left( c_i^k, \kappa_i^k \right)$ given the proposal distribution from Step 1.

5. **Find $\theta$ to maximize the log likelihood.** We suppressed dependence in (21) on $\mathbf{z}_i$. We estimate $\theta$ separately for each of the 32 cells of observable heterogeneity for a sample of 1,000 bidders in each cell, across auctions (due to computational constraints on the USDA servers). An auxiliary benefit of the importance sampling approach of Ackerberg (2009) is that it yields a differentiable objective function.

6. **Repeat.** We repeat Steps 2-5 several times, using estimates from the solution to Step 5 as the new proposal distribution. Our final estimates use 10,000 simulation draws to mitigate simulation bias.

Table E.2 presents parameter estimates for selected observable cells $\mathbf{z}_i$.

**Step 3: Estimate $\tau (\mathbf{z}_i, c_i, \kappa_i)$** Our final step involves estimating the conditional expectation function $\tau (\mathbf{z}_i, c_i, \kappa_i) = \mathbb{E} \left[ 1 - a_{i0} \mid \mathbf{z}_i, c_i, \kappa_i \right] = \pi \cdot \mathbf{z}_i + \beta \cdot c_i + \alpha \cdot \kappa_i$. We match model implied moments of additionality to observed moments of additionality, $1 - a_{i0}$, among bidders who lose the auction. We search for $\theta^\tau = (\pi, \beta, \alpha)$ that minimizes $\hat{g} \left( \theta^\tau \right)' A \hat{g} \left( \theta^\tau \right)$ for weight matrix $A$ (the two-step optimal weight matrix) and $\hat{g} \left( \theta^\tau \right) = \hat{\mathbb{E}} \left[ m_i - \frac{1}{K} \sum_k m_i \left( \theta^\tau \mid c_i^k, \kappa_i^k \right) \right]$, where $\hat{\mathbb{E}}$ denotes the sample expectation, for $m_i$ equal to:

* Additionality at the award threshold: $(1 - a_{i0}) \cdot \mathbb{1} \left[ S - b < s \left( \mathbf{b}_i^*, \mathbf{z}_i^s \right) < S \right]$ for bandwidth $b$.

* Additionality by observable characteristics: $(1 - a_{i0}) \cdot \mathbb{1} \left[ s \left( \mathbf{b}_i^*, \mathbf{z}_i^s \right) < S \right] \cdot \mathbf{z}_i$.

* Covariance between additionality and chosen scores: $(1 - a_{i0}) \cdot s \left( \mathbf{b}_i^*, \mathbf{z}_i^s \right) \cdot \mathbb{1} \left[ s \left( \mathbf{b}_i^*, \mathbf{z}_i^s \right) < S \right]$.

* Additionality within chosen contracts: $(1 - a_{i0}) \cdot \mathbb{1} \left[ x_{ij} = 1 \right] \cdot \mathbb{1} \left[ s \left( \mathbf{b}_i^*, \mathbf{z}_i^s \right) < S \right]$.

73

Our estimation approach follows the following steps:

1. Draw simulations $(c_i^k, \kappa_i^k)$ from $F_{c,\kappa|\mathbf{z}}$ estimated in Step 2.

2. Calculate optimal bids $\mathbf{b}_i^*$ given $(c_i^k, \kappa_i^k)$ using equation (10).

3. Calculate $m_i \left(\theta^\tau | c_i^k, \kappa_i^k\right)$ by replacing $1 - a_{i0}$ with $\pi \cdot \mathbf{z}_i + \beta \cdot c_i + \alpha \cdot \kappa_i$ and observed bids with simulated optimal bids for each simulation draw $k$ and parameter guess $\theta^\tau$.

4. Minimize the objective $\hat{g} \left(\theta^\tau\right)' A \hat{g} \left(\theta^\tau\right)$.

Because we require an observation of bid fields to calculate $1 - a_{i0}$, we estimate $\tau (\mathbf{z}_i, c_i, \kappa_i)$ using only the auction where we observe bid fields (2016). Our primary estimates use the remote-sensing data from 2017-2020 to measure $1 - a_{i0}$. We weight each bidder's field and year equally. This approach relies on the assumption that the relationships estimated in $\tau (\mathbf{z}_i, c_i, \kappa_i)$ in this auction can be extrapolated to the other auctions in our sample, and that $\tau (\mathbf{z}_i, c_i, \kappa_i)$ can be estimated in only the four years following the auction. This may seem unappealing given the transition period in Figure 4, but we note that $1 - a_{i0}$ is calculated among losing bidders, not those transitioning into land retirement.

We calculate standard errors via block bootstrap, resampling at the bidder level. We incorporate estimation error in the earlier step by taking a new cost draw from the estimated variance-covariance matrix of the parameters of $F_{c,\kappa|\mathbf{z}}$ and re-calculating optimal bids for that draw for each bootstrap sample.

74

# Table E.2: $F_{c,\kappa|\mathbf{z}}$ Parameter Estimates (Select $\mathbf{z}_i$)


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="4">Former CRP = 0</th>
        <th colspan="4">Former CRP = 1</th>
    </tr>
    <tr>
        <th>Prior crop</th>
        <th>Q1</th>
        <th>Q2</th>
        <th>Q3</th>
        <th>Q4</th>
        <th>Q1</th>
        <th>Q2</th>
        <th>Q3</th>
        <th>Q4</th>
    </tr>
    <tr>
        <th>Soil prod.</th>
        <th>Q1</th>
        <th>Q2</th>
        <th>Q3</th>
        <th>Q4</th>
        <th>Q1</th>
        <th>Q2</th>
        <th>Q3</th>
        <th>Q4</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
        <th>(6)</th>
        <th>(7)</th>
        <th>(8)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>$c_i$</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Mean</td>
        <td>29.82</td>
        <td>41.65</td>
        <td>64.83</td>
        <td>131.94</td>
        <td>35.49</td>
        <td>44.98</td>
        <td>66.34</td>
        <td>124.49</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.02)</td>
        <td>(0.04)</td>
        <td>(0.05)</td>
        <td>(0.06)</td>
        <td>(0.02)</td>
        <td>(0.03)</td>
        <td>(0.04)</td>
        <td>(0.07)</td>
    </tr>
    <tr>
        <td>Log $\sigma_c$</td>
        <td>1.67</td>
        <td>2.57</td>
        <td>3.49</td>
        <td>3.69</td>
        <td>1.44</td>
        <td>2.47</td>
        <td>3.43</td>
        <td>3.90</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.004)</td>
        <td>(0.002)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.004)</td>
        <td>(0.002)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
    </tr>
    <tr>
        <td>$\kappa_{ij}$</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Means</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Native grasses</td>
        <td>0.35</td>
        <td>-4.83</td>
        <td>4.06</td>
        <td>1.62</td>
        <td>-4.30</td>
        <td>-6.40</td>
        <td>5.92</td>
        <td>-0.90</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.02)</td>
        <td>(0.03)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.03)</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>26.24</td>
        <td>25.91</td>
        <td>26.62</td>
        <td>33.38</td>
        <td>13.84</td>
        <td>18.77</td>
        <td>23.58</td>
        <td>31.76</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.02)</td>
        <td>(0.03)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>15.95</td>
        <td>11.90</td>
        <td>16.13</td>
        <td>20.53</td>
        <td>14.03</td>
        <td>12.95</td>
        <td>18.89</td>
        <td>13.62</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>15.98</td>
        <td>14.43</td>
        <td>18.50</td>
        <td>12.37</td>
        <td>18.10</td>
        <td>11.84</td>
        <td>22.31</td>
        <td>16.94</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.03)</td>
        <td>(0.04)</td>
        <td>(0.03)</td>
        <td>(0.02)</td>
        <td>(0.04)</td>
        <td>(0.04)</td>
        <td>(0.03)</td>
        <td>(0.03)</td>
    </tr>
    <tr>
        <td>Wildlife food plot</td>
        <td>24.53</td>
        <td>24.40</td>
        <td>12.38</td>
        <td>14.88</td>
        <td>23.78</td>
        <td>22.78</td>
        <td>14.00</td>
        <td>17.53</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.03)</td>
        <td>(0.03)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.03)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>14.36</td>
        <td>10.87</td>
        <td>13.65</td>
        <td>16.45</td>
        <td>21.83</td>
        <td>19.52</td>
        <td>17.71</td>
        <td>19.64</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
    </tr>
    <tr>
        <td>Log $\sigma_\kappa$</td>
        <td>2.60</td>
        <td>2.93</td>
        <td>2.81</td>
        <td>2.70</td>
        <td>2.76</td>
        <td>2.91</td>
        <td>2.80</td>
        <td>2.86</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
        <td>(0.001)</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents presents parameter estimates for 8 cells of $\mathbf{z}_i$. Standard errors calculated using the inverse of the negative Hessian, calculated numerically.

### Model Fit

We conduct a number of exercises to assess model fit. Figure E.6 investigates the fit of $F_{c,\kappa|\mathbf{z}}$ by comparing model-implied choices to the data. Figure E.6a demonstrates very good fit across the dimensions with parameterized mean differences in costs. The fit is reasonable when we look across all thirty-six possible actions (Figure E.6b), considering our parsimonious parameterization of $F_{c,\kappa|\mathbf{z}}$. The weakest fit of the model is for actions that have relatively low points within their coarsened category, as the model under-predicts these choices relative to the data. We see similar patterns in Figures E.6c and E.6d, which also compare model-implied choices to the data for monetary bids and scores. The fit is very good for the distribution of bids and reasonable for scores. This echoes the patterns in Figure E.6b: we under-predict low scores because we under-predict the share of bidders choosing low-scoring actions. While the fit could be straightforwardly improved by modeling additional mean differences across actions, we view the fit as reasonable for our objectives.

75

# Figure E.6: Model Fit: Costs


### (a) Choice probabilities: coarsened
<table>
  <thead>
    <tr>
        <th>Category</th>
        <th>Data</th>
        <th>Model</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Introduced grasses</td>
        <td>0.24</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>Native grasses</td>
        <td>0.28</td>
        <td>0.29</td>
    </tr>
    <tr>
        <td>Trees</td>
        <td>0.10</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>Habitat</td>
        <td>0.06</td>
        <td>0.05</td>
    </tr>
    <tr>
        <td>Rare habitat</td>
        <td>0.07</td>
        <td>0.06</td>
    </tr>
    <tr>
        <td>Food plot</td>
        <td>0.10</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>Pollinator habitat</td>
        <td>0.14</td>
        <td>0.16</td>
    </tr>
  </tbody>
</table>


### (b) Choice probabilities: all 36 contracts
<table>
  <thead>
    <tr>
        <th>Contract</th>
        <th>Data</th>
        <th>Model</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1</td>
        <td>0.14</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>2</td>
        <td>0.09</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.14</td>
        <td>0.17</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.20</td>
        <td>0.19</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.04</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>6</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>7</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>8</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>9</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>10</td>
        <td>0.04</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>11</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>12</td>
        <td>0.04</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>13</td>
        <td>0.08</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>14</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>15</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>16</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>17</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>18</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>19</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>20</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>21</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>22</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>23</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>24</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>25</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>26</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>27</td>
        <td>0.04</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>28</td>
        <td>0.06</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>29</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>30</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>31</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>32</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>33</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>34</td>
        <td>0.02</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>35</td>
        <td>0.03</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>36</td>
        <td>0.03</td>
        <td>0.03</td>
    </tr>
  </tbody>
</table>


### (c) Bids
(Scatter plot comparing Model (bid) on X-axis to Data (bid) on Y-axis, showing a strong linear correlation from approximately 25 to 240.)

### (d) Scores
(Scatter plot comparing Model (score) on X-axis to Data (score) on Y-axis, showing a positive correlation centered around a diagonal line from approximately 150 to 380.)

*Notes*: Figures summarize model fit by comparing simulated choices of contracts, bids, and scores to the data. Panel (a) plots model-implied and observed market shares among the coarsened top-up actions (five dimensions of $p_j$ and the two upgrade dimensions). Panel (b) plots model-implied and observed market shares for all possible contracts. Panels (c) and (d) present scatter plots for the monetary bids (c) and scores (d) comparing model simulations and data. Each dot represents 11 observations (per disclosure requirements).

Table E.3 conducts a second exercise to assess our model-implied costs, comparing them to some administrative (accounting) data collected by the USDA. Because this data is more limited, we aggregate to the three major categories—grasses, trees, and habitats—and compare their relative costs in our model and the data. We estimate the same rank order and generally similar magnitudes. This comparison provides further support that our model fit is reasonable.

## Table E.3: Comparison Between Estimated and Administrative Cost Estimates


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Estimates</th>
        <th>Median admin cost</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Tree primary covers (rel. to grasses)</td>
        <td>23.77</td>
        <td>26.46</td>
    </tr>
    <tr>
        <td>Habitat primary covers (rel. to grasses)</td>
        <td>15.20</td>
        <td>2.67</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents average revealed preference estimates of costs of aggregate primary cover categories, relative to grasses (column 1), compared to administrative data collected on the costs of these actions by the USDA (column 2).

76

Figures E.7 and E.8 investigate the fit and robustness of our parameterization of $\tau (\mathbf{z}_i, c_i, \kappa_i)$. For each rejected bidder, we can obtain the set of simulation draws for which the observed bid is chosen as optimal. We therefore condition estimates of costs on observables and *the observed bid* and then correlate the base cost with our observations of land use $(1 - a_{i0})$ in Figure E.7. Figure E.7 presents results, both without controls (a) and residualized of observable characteristics (b). Our baseline specification is linear, in order to treat $c_i$ and $\kappa_i$ symmetrically. But, we also assess robustness to parameterization of $\tau (\mathbf{z}_i, c_i, \kappa_i)$ as a function of $\ln(c)$ in Figure E.8 to investigate deviations from linearity. Results are similar.

Figure E.7: Model Fit: Additionality


(a) Land Use vs. Cost Estimates

<table>
  <thead>
    <tr>
        <th>Simulated $c_i$ | observables and bids</th>
        <th>Share cropped absent contract $(1 - a_{i0})$</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-100</td>
        <td> </td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>25</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.25</td>
    </tr>
    <tr>
        <td>75</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>125</td>
        <td>0.36</td>
    </tr>
    <tr>
        <td>150</td>
        <td>0.38</td>
    </tr>
    <tr>
        <td>175</td>
        <td>0.37</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>300</td>
        <td> </td>
    </tr>
  </tbody>
</table>


(b) Land Use vs. Cost Estimates, Residualized


<table>
  <thead>
    <tr>
        <th>Simulated $c_i$ | observables and bids</th>
        <th>Share cropped absent contract $(1 - a_{i0})$</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-100</td>
        <td> </td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.22</td>
    </tr>
    <tr>
        <td>25</td>
        <td>0.24</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.27</td>
    </tr>
    <tr>
        <td>75</td>
        <td>0.29</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>125</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>150</td>
        <td>0.34</td>
    </tr>
    <tr>
        <td>175</td>
        <td>0.33</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.32</td>
    </tr>
    <tr>
        <td>300</td>
        <td> </td>
    </tr>
  </tbody>
</table>


*Notes:* Figures plot landowner specific additionality for all fields of rejected bidders against the estimate of each bidders’ base cost, given parameter estimates $F_{c,\kappa|\mathbf{z}_i}$ and the observed bid. Panel (a) includes no controls (the raw correlation) and panels (b) includes observable controls (it plots the residual correlation).

Finally, Figure E.8 investigates robustness to the parameterization and estimation of $\tau (\mathbf{z}_i, c_i, \kappa_i)$ in the context of the empirical graphical analysis. Figure E.8 plots $B \cdot \tau$ curves across the baseline specification (a) and a specification where $\tau (\mathbf{z}_i, c_i, \kappa_i)$ is a function of $\ln(c_i)$ instead of linear in $c_i$ (b). They display similar $B \cdot \tau$ curves—both in their level and slope— indicating similar welfare conclusions across these different parameterizations. Perhaps unsurprisingly, the domain in which they differ most significantly is in extrapolation to the bottom decile of the cost distribution.

77

Figure E.8: Empirical Graphical Analysis: Robustness to Alternative $B \cdot \tau$


(a) Baseline
<table>
  <tbody>
    <tr>
        <td>q</td>
        <td>$ per acre-year / 100</td>
    </tr>
    <tr>
        <td>0</td>
        <td>50</td>
    </tr>
    <tr>
        <td>.2</td>
        <td>40</td>
    </tr>
    <tr>
        <td>.4</td>
        <td>45</td>
    </tr>
    <tr>
        <td>.6</td>
        <td>55</td>
    </tr>
    <tr>
        <td>.8</td>
        <td>65</td>
    </tr>
    <tr>
        <td>1</td>
        <td>70</td>
    </tr>
  </tbody>
</table>


(b) Robustness, ln (c) Parameterization
<table>
  <tbody>
    <tr>
        <td>q</td>
        <td>$ per acre-year / 100</td>
    </tr>
    <tr>
        <td>0</td>
        <td>75</td>
    </tr>
    <tr>
        <td>.2</td>
        <td>45</td>
    </tr>
    <tr>
        <td>.4</td>
        <td>50</td>
    </tr>
    <tr>
        <td>.6</td>
        <td>60</td>
    </tr>
    <tr>
        <td>.8</td>
        <td>75</td>
    </tr>
    <tr>
        <td>1</td>
        <td>85</td>
    </tr>
  </tbody>
</table>


(c) Robustness, Inversion
<table>
  <tbody>
    <tr>
        <td>q</td>
        <td>$ per acre-year / 100</td>
    </tr>
    <tr>
        <td>0</td>
        <td> </td>
    </tr>
    <tr>
        <td>.2</td>
        <td>55</td>
    </tr>
    <tr>
        <td>.4</td>
        <td>50</td>
    </tr>
    <tr>
        <td>.6</td>
        <td>65</td>
    </tr>
    <tr>
        <td>.8</td>
        <td>95</td>
    </tr>
    <tr>
        <td>1</td>
        <td>100</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure presents robustness of the $B \cdot \tau$ curve in the empirical implementation of the graphical analysis, which denotes the incremental value of contracting, relative to no contract, at each quantile of the population distribution of costs, graphed with local polynomial regressions. The horizontal axis is the share of the population, ordered by costs of accepting a CRP contract. In panels (a)-(b), landowners are ordered by the cost of the base contract. In panel (c) landowners are ordered by the cost of their chosen contract in the CRP auction mechanism. Values are reported in dollars per acre per year. Panel (a) presents our baseline parameterization of $\tau(\mathbf{z}_i, c_i, \kappa_i)$. Panel (b) presents an alternative parameterization of $\tau(\mathbf{z}_i, c_i, \kappa_i) = \pi \cdot \mathbf{z}_i + \beta \cdot \ln(c_i) + \alpha \cdot \kappa_i$. Panel (c) presents $B \cdot \tau$ by plotting the conditional expectation function of observed $B_j(\mathbf{z}_i^s)(1 - a_{i0})$ among landowners at each quantile of $\tilde{c}_i$ defined by equation (22).

Figure E.8c presents results from a simplified model with an alternative assumption of *exogenous contract choices*. While this model does not accurately capture the CRP auction setting, in which contract choice is perhaps the most important dimension of competition,<sup>56</sup> it is useful to consider as a robustness exercise because we can directly compute:

$$ (22) \qquad \tilde{c}_i = r_i + \frac{G(s((r_i, \mathbf{x}_i), \mathbf{z}_i^s))}{\frac{\partial G(s((r_i, \mathbf{x}_i), \mathbf{z}_i^s))}{\partial r_i}} $$

from a bidder’s first order condition with respect to their monetary bid, $r_i$. Note that this exercise

\*<sup>56</sup>The EBI Factsheets provided to landowners state: “The single most important producer decision involves determining which cover practice to apply to the acres offered. Planting or establishing the highest scoring cover mixture is the best way to improve the chances of offer acceptance.”

78

also assumes away bidcaps and non-linearities in the scoring rule, both of which are relevant and incorporated in our baseline estimation approach. The cost term $\tilde{c}_i$ is defined as the cost associated with a bidder's *chosen contract*. Even without the assumption of exogenous contract choice, we can first compare $\tilde{c}_i$ to our estimated costs of the chosen contract in the status quo mechanism, under our baseline parameterization (Figure E.9).

However, because $\tilde{c}_i$ is defined as the cost associated with a bidder's *chosen contract*, we caution this cost is only interpretable in the graphical welfare analysis under the assumption that contract choice is exogenous. Despite these shortcomings, equation (22) provides the opportunity to examine how additionality varies with costs without parametric assumptions on $\tau(\mathbf{z}_i, c_i, \kappa_i)$. This is because as we have an observation of $\tilde{c}_i$ and additionality (land use) for each rejected bidder. Figure E.8c therefore plots expected additionality by the population quantile of $\tilde{c}_i$.

Note that because of the observables in the scoring rule $\mathbf{z}_i^s$ and the choice of contract (here assumed exogenous), bidders with very low monetary bids and (inferred) costs could still be rejected. This would be the case for a bidder with both observables and a contract associated with low point values. However, these rejected low-cost bidders are selected on dimensions that we do not assume are exogenous in our baseline specification. Therefore, to highlight the domain in which we rely on parametric extrapolation in our baseline approach, we exclude quantiles of bidders where the majority are above the winning score threshold and therefore have no observation of $1 - a_{i0}$ in Figure E.8c. Similar to the conclusions in panels (a) and (b), the lowest quantiles of the cost distribution is where we are reliant on parametric extrapolation. However, Figure E.8c demonstrates that the shape of the $B \cdot \tau$ curve appears similar to baseline, providing further support that our conclusions are not being driven by our specific parameterization. Note that the level of $B \cdot \tau$ is slightly higher in (c) than in (a)-(b): this is because (c) conditions on the chosen contract, which has higher environmental value than the base contract (plotted in (a)-(b)).

Figure E.9: Estimated Costs at the Chosen Contract


<table>
  <thead>
    <tr>
        <th>Cost</th>
        <th>Baseline (Density)</th>
        <th>Inversion Exercise (Density)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>25</td>
        <td>0.018</td>
        <td>0.020</td>
    </tr>
    <tr>
        <td>50</td>
        <td>0.008</td>
        <td>0.007</td>
    </tr>
    <tr>
        <td>100</td>
        <td>0.004</td>
        <td>0.004</td>
    </tr>
    <tr>
        <td>150</td>
        <td>0.003</td>
        <td>0.002</td>
    </tr>
    <tr>
        <td>200</td>
        <td>0.002</td>
        <td>0.001</td>
    </tr>
    <tr>
        <td>300</td>
        <td>0.000</td>
        <td>0.000</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents a kernel density plot of estimates of the costs of the chosen CRP contract using our baseline parameterization and estimation approach (described in IV.) and the inversion exercise described above (equation (22)). Our baseline approach conditions on the chosen contract in the simulation, while the alternative approach conditions on the observed chosen contract. Figure restricts to estimates with positive costs.

79

# F Valuing Benefits

We assume that the weights in the scoring rule $B_j(\mathbf{z}_i^s)$ reflect the relative social benefits (in dollars) across $j$ and $\mathbf{z}_i^s$ under the assumption that $a_{i0} = 0$ for all $i$. The mechanism implicitly makes trade-offs in the scoring rule that monetize relative preferences across contracts and characteristics.

Using this logic requires two assumptions. First, we require the assumption that the scoring rule is indeed constructed under the assumption that $a_{i0} = 0$ for all $i$. This is motivated directly by a statement from a USDA report (Claassen et al., 2014), which writes: *Benefit-cost indices are used to rank applications for acceptance in all major USDA conservation programs... Existing indices, however, implicitly assume full additionality.* Second, we require that the weights in the scoring rule are not distorted to reduce expenditures. There is no evidence to support this (see Ribaudo et al. (2001) for a discussion of the scoring rule), and the USDA values transfers to agricultural landowners (as revealed by its other programs). We therefore assume that the USDA maximizes social welfare by announcing its preferences in the scoring rule. Of course, the USDA revealed-preferred values of $B_j(\mathbf{z}_i^s)$ may not necessarily align with the true environmental benefits for a variety of reasons, e.g. political concerns. We choose to take this USDA-revealed-preferred approach, versus calibrating $B_j(\mathbf{z}_i^s)$ from an external source, to focus on additionality as a source of social welfare loss.

To calculate these scoring-rule implied relative valuations, we note that scoring rule is separable in the actions incentivized by the heterogeneous contracts and the bid ($) rental rate

$$ (23) \qquad s(\mathbf{b}_i, \mathbf{z}_i^s) = \underbrace{s_a(\mathbf{x}_i, \mathbf{z}_i^s)}_{\text{action points}} + \underbrace{s_r(r_i)}_{\text{bid rental rate points}} $$

and construct a quasi-linear approximation to the scoring rule to obtain relative willingness to pay. The scoring rule departs from quasi-linearity because of kinked incentives in points bidders receive as a percentage of their bidcap. We "quasi-linearize" the scoring rule by taking the average of $s'_r(r_i)$ in the region without the added percentage points bonus and the region with the percentage point bonus (at the median bidcap value).<sup>57</sup>

Based on this procedure, we obtain estimates of how the USDA trades off higher costs with heterogeneous environmental benefits across contracts $j$ and observable characteristics $\mathbf{z}_i^s$. However, the scoring rule is not directly informative about the level of social benefits. We obtain this using estimates of the value of the CRP from the literature. We assume that all impacts of the CRP accrue only over the contract period. We use following values of the CRP from the literature, where our baseline estimates take the average across these three studies.

1. Our first estimate sums the recreational,<sup>58</sup> public works,<sup>59</sup> and air quality benefits<sup>60</sup> from

\*57 The fact that different bidders face different scoring rules based on their bidcap does not reflect differential valuation of environmental benefits across bidders.

\*58 This includes sport-fishing, small-game hunting, noncompetitive viewing, and waterfowl hunting.

\*59 This includes cost savings associated with reduced maintenance of roadside ditches, navigation channels, water treatment facilities, municipal water uses, flood damage, and water storage.

\*60 This includes reduced health risks and cleaning costs associated with blowing dust.

80

Feather et al. (1999) and adds estimates of the value of greenhouse gas reductions from sequestered CO<sub>2</sub> (over the 10-year contract) and reduced fuel and fertilizer use<sup>61</sup> monetized at $43 per metric ton. This leads to an overall estimated value of the CRP of $98.34 per acre, per year. This is likely to be an under-estimate because biodiversity is only valued insofar as it provides recreational benefits, and this estimate does not include water quality improvements from reduced run-off.

2. Our second estimate takes the valuation of the CRP from Hansen (2007), and adds estimates of the value of greenhouse gas reductions from sequestered CO<sub>2</sub> (over the 10-year contract) and reduced fuel and fertilizer use, which then equals $255.70, per acre, per year.

3. Our third takes the average of a conservative and generous value of the non-GHG CRP benefits from Johnson et al. (2016) and adds estimates of the value of greenhouse gas reductions from sequestered CO<sub>2</sub> (over the 10-year contract) and reduced fuel and fertilizer use. This leads to estimates of $367.96 and $456.04, per acre, per year. These may be an over-estimate because benefits are estimated in only one geographic area, which may have more environmentally sensitive land. We average these two effects.

The description above highlights the difficulties of monetizing the value of the all of the environmental benefits of the CRP. We emphasize that our focus is not on obtaining estimates of $B_j(\mathbf{z}_i^s)$, but rather on $\tau(\mathbf{z}_i, c_i, \kappa_i)$; results can be recalculated for any alternative valuation of $B_j(\mathbf{z}_i^s)$. Different estimates of $B_j(\mathbf{z}_i^s)$ will change quantitative conclusions about the social value of the CRP and alternative auctions. However, the main qualitative conclusions persist.

## G Supplemental Material: Counterfactuals

**Graphical Analysis** Appendix Figures G.1 and G.2 assess robustness of the graphical analysis. First, Figure G.1 replicates Figure 7 with valuations of environmental benefits that are halved relative to our baseline estimates (see the discussion above). There still exist social welfare gains, and adverse selection is still apparent. However, with lower valuations of environmental benefits the socially optimal incentives and market size are smaller, with smaller corresponding social welfare gains.

\*<sup>61</sup>See https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/EPAS/natural-resouces-analysis/nra-landing-index/2017-files/Environmental_Benefits_of_the_US_CRP_2017_draft.pdf.

81

# Figure G.1: Graphical Welfare Analysis: Robustness, Lower $B$


<table>
    <tr>
        <th>q</th>
        <th>MC (Baseline)</th>
        <th>$B \cdot \tau$ (Baseline)</th>
        <th>MC (Halved $B$)</th>
        <th>$B \cdot \tau$ (Halved $B$)</th>
    </tr>
    <tr>
        <td>0</td>
        <td>10</td>
        <td>40</td>
        <td>10</td>
        <td>20</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>25</td>
        <td>35</td>
        <td>25</td>
        <td>18</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>45</td>
        <td>40</td>
        <td>45</td>
        <td>20</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>75</td>
        <td>50</td>
        <td>75</td>
        <td>25</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>120</td>
        <td>60</td>
        <td>120</td>
        <td>30</td>
    </tr>
    <tr>
        <td>1.0</td>
        <td>200</td>
        <td>65</td>
        <td>200</td>
        <td>33</td>
    </tr>
</table>
*Notes*: Figure presents the empirical version of Figure 1 for the base contract, replicating the baseline empirical graphical analysis (Figure 7, re-produced in panel (a)), and an alternative specification with environmental benefits halved relative to baseline (panel (b)). The horizontal axis is the share of the population, ordered by costs of accepting a base CRP contract. Values are reported in dollars per acre per year. The $MC$ curve is the inverse distribution function of the minimum cost to fulfill the base contract. $B \cdot \tau$ denotes the incremental value of contracting, relative to no contract, averaged at each quantile of the population distribution of the base cost, graphed with a local polynomial regression. The vertical distance between the $B \cdot \tau$ and $MC$ curves represents the social surplus from contracting at each quantile of the population distribution of costs. The upwards-sloping $B \cdot \tau$ curve illustrates the presence of adverse selection in the market.

Second, Figure G.2 assesses the robustness of the conclusions in the empirical graphical analysis to estimation error. We do so with the following bootstrap procedure. We (1) re-sample from the population of bidders. Then, for this sample, we (2) estimate parameters with the procedure described in Section IV. and (3) simulate ($c_i, \kappa_i$) and $\tau(\mathbf{z}_i, c_i, \kappa_i)$. For each of these simulations, we then (4) re-create the graphical analysis in Figures 7 and 8a. Shaded outcomes represent the range between the 2nd and 98th percentile of 100 iterations of this procedure.

82

# Figure G.2: Empirical Graphical Analysis: Estimation Error


<table>
  <tbody>
    <tr>
        <td colspan="3">(a) Base Contract</td>
        <td colspan="3">(b) Tree Contract</td>
    </tr>
    <tr>
        <td>q</td>
        <td>MC ($)</td>
        <td>B · τ ($)</td>
        <td>q</td>
        <td>MC ($)</td>
        <td>B · τ ($)</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0</td>
        <td>50</td>
        <td>0</td>
        <td>0</td>
        <td>-10</td>
    </tr>
    <tr>
        <td>.2</td>
        <td>10</td>
        <td>45</td>
        <td>.2</td>
        <td>20</td>
        <td>25</td>
    </tr>
    <tr>
        <td>.4</td>
        <td>25</td>
        <td>40</td>
        <td>.4</td>
        <td>45</td>
        <td>45</td>
    </tr>
    <tr>
        <td>.6</td>
        <td>45</td>
        <td>45</td>
        <td>.6</td>
        <td>80</td>
        <td>60</td>
    </tr>
    <tr>
        <td>.8</td>
        <td>80</td>
        <td>55</td>
        <td>.8</td>
        <td>130</td>
        <td>75</td>
    </tr>
    <tr>
        <td>1</td>
        <td>130</td>
        <td>70</td>
        <td>1</td>
        <td>210</td>
        <td>95</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure presents the empirical version of Figure 1 for the base contract (a) and tree-planting and maintenance contracts (b). Shaded lines indicate the range between the 2nd and 98th percentile of bootstrap draws of the procedure described in the text. The horizontal axis is the share of the population, ordered by costs of accepting a CRP contract. Values are reported in dollars per acre per year. The *MC* curve is the inverse distribution function of the minimum cost to fulfill the contract. $B \cdot \tau$ denotes the incremental value of contracting, relative to no contract, averaged at each quantile of the population distribution of costs, graphed with local polynomial regressions. The vertical distance between the $B \cdot \tau$ and *MC* curves represents the social surplus from contracting at each quantile of the population distribution of costs. The upwards-sloping $B \cdot \tau$ curve illustrates the presence of adverse selection in the market.

**Vickrey Auctions with Scoring** Vickrey auctions with scoring maximize a definition of social welfare implied by the scoring rule $s_j(\mathbf{z}_i)$ (equation (18)) subject to two constraints. First, each landowner must obtain at most one contract $\sum_j x_{ij} \leq 1$. Second, no more landowners are allocated contracts, in each auction, than in the status quo. The VCG incentive payment that implements this allocation is:

$$ (24) \quad \sum_j s_j\left(\mathbf{z}_i\right) \cdot x_{i j}^*+\sum_{i^{\prime} \neq i} \sum_{j^{\prime}}\left(s_{j^{\prime}}\left(\mathbf{z}_{i^{\prime}}\right)-c_{i^{\prime}}-\kappa_{i^{\prime} j^{\prime}}\right) \cdot x_{i^{\prime} j^{\prime}}^*-\sum_{i^{\prime} \neq i} \sum_{j^{\prime}}\left(s_{j^{\prime}}\left(\mathbf{z}_{i^{\prime}}\right)-c_{i^{\prime}}-\kappa_{i^{\prime} j^{\prime}}\right) \cdot x_{i^{\prime} j^{\prime}}^{-i}, $$

where $\{x_{ij}^*\}$ denotes the allocation that maximizes $\sum_{i'} \sum_{j'} (s_{j'}(\mathbf{z}_{i'}) - c_{i'} - \kappa_{i'j'})$ given all reports of $(c_{i'}, \kappa_{i'j'})$ in the population and $\{x_{i'j'}^{-i}\}$ denotes the allocation that solves $\max_x \sum_{i' \neq i} \sum_{j'} (s_{j'}(\mathbf{z}_{i'}) - c_{i'} - \kappa_{i'j'}) \cdot x_{i'j'}^{-i}$.

Under this incentive payment, bidder $i$'s payoff for contract $j$ is

$$ (25) \quad \sum_j\left(s_j\left(\mathbf{z}_i\right)-c_i-\kappa_{i j}\right) \cdot x_{i j}^*+\sum_{i^{\prime} \neq i} \sum_{j^{\prime}}\left(s_{j^{\prime}}\left(\mathbf{z}_{i^{\prime}}\right)-c_{i^{\prime}}-\kappa_{i^{\prime} j^{\prime}}\right) \cdot x_{i^{\prime} j^{\prime}}^* $$
$$ -\sum_{i^{\prime} \neq i} \sum_{j^{\prime}}\left(s_{j^{\prime}}\left(\mathbf{z}_{i^{\prime}}\right)-c_{i^{\prime}}-\kappa_{i^{\prime} j^{\prime}}\right) \cdot x_{i^{\prime} j^{\prime}}^{-i}, $$

such that equations (18) and (25) are equivalent from the perspective of bidder $i$'s report. Bidders therefore truthfully report their vector of $(c_i, \kappa_i)$, then are ranked by $\max_j s_j(\mathbf{z}_i) - c_i - \kappa_{ij}$. The highest scoring bidders subject to the auction's quantity constraint are allocated the contract

83

$$ \arg \max_j s_j (\mathbf{z}_i) - c_i - \kappa_{ij}. $$

**Other Alternative Auctions** A key advantage of the Vickrey auctions with scoring is that the equilibrium allocations are simple to compute. However, it is useful to know how the alternative scoring rule would impact allocations under the status quo auction format, where each bidder solves equation (10). To implement this counterfactual, we assume that bidders’ beliefs about the acreage limit and the number of competing bidders is held fixed. However, we allow beliefs about the equilibrium distribution of scores to adjust using the following procedure:

1. Initialize an empirical distribution of scores. We begin with optimal bids, scored under the new scoring rule, given beliefs based on the status quo distribution of scores.

2. Solve the inner problem (see equation (11)), for all possible integer scores.

3. Iterate the following (for each step $k$):

    (a) Resample from step $k - 1$’s distribution of bidders’ scores in the procedure described in Step 1 of Appendix Section E. Estimate a kernel density of the resulting distribution of threshold scores using a Gaussian kernel and a bandwidth of 5, as in baseline ($G^k (S)$).

    (b) Solve for step $k$ scores: solve for each bidder’s optimal score, given the inner problem (solved in Step 2), the scoring rule, and beliefs from (a), $G^k (S)$ (see equation (12)),

    (c) Compare the difference between the estimated CDF at each score $S$ in step $k$ and $k - 1$. Repeat until the max absolute value of the difference (across integer scores) falls below a critical threshold.

We conduct this procedure for each auction. We hold fixed the draws of acreage limits, number of competing bidders, and bootstrap draws of bidders across iterations; for some auctions, we required a dampening parameter to achieve convergence. We then use these beliefs to calculate optimal bids and allocations, given the alternative scoring rule, reported in column (9) of Table 5.

**Cost of Public Funds** Figure G.3 considers the same auctions presented in Figure 9, but evaluates social welfare with a cost of funds equal to 0.15. Under this framework, 15% of all USDA spending is considered deadweight loss, motivated by the social costs of financing expenditures via distortionary taxation. With a cost of funds, the status quo auction reduces social welfare. However, social welfare gains become positive once the auction is designed to consider additionality. Note that Figure G.3 evaluates auctions using the same scoring rules as in Figure 9. With weights $\omega_j$ and $\omega_z$ re-optimized to reduce government spending, gains would be higher.

84

# Figure G.3: Social Welfare Under Alternative Auctions: Cost of Funds


<table>
  <thead>
    <tr>
        <th>Auction Type</th>
        <th>Social welfare gains per auction (million $)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>(1) Status quo</td>
        <td>-122</td>
    </tr>
    <tr>
        <td>(2) Efficient allocation (not implementable)</td>
        <td> </td>
    </tr>
    <tr>
        <td>(3) Status quo scoring rule</td>
        <td>-223</td>
    </tr>
    <tr>
        <td>(4) Re-weight contract incentives (ωj)</td>
        <td>-26</td>
    </tr>
    <tr>
        <td>(5) Re-weight zˢ (ωz)</td>
        <td>-5</td>
    </tr>
    <tr>
        <td>(6) Add additional predictor τ(z)</td>
        <td>44</td>
    </tr>
    <tr>
        <td>(7) Reduce quantity</td>
        <td>309</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure presents estimates of the social welfare gains (defined in equation (16), with the addition of a cost of funds equal to 0.15) under status quo and alternative auctions. Results reported in million dollars per auction. All auctions impose that each landowner obtains at most one contract and that total contracts awarded cannot exceed the status quo, in each auction. Bar (1) simulates the status quo. Bar (2) is excluded because USDA spending is undefined. Bars (3)-(7) calculate social welfare under alternative Vickrey auctions with scoring (see Section B. for more details). Bars (3)-(6) hold quantity (the number of landowners allocated contracts, in each auction) constant at the status quo and change the scoring rule $s_j(\mathbf{z}_i)$ defined in equation (17). Bar (3) uses the existing scoring rule $s_j(\mathbf{z}_i) = B_j(\mathbf{z}_i^s)$. Bar (4) uses a scoring rule with the social-surplus maximizing incentives across contracts $(\omega_j)$ (with social surplus defined in equation (16)). Bar (5) uses a scoring rule with the social-surplus maximizing asymmetry terms across bidders using characteristics already in the scoring rule $(\mathbf{z}_i^s)$. Bar (6) adds an additional characteristic to the scoring rule, a prediction of $\tau(z_i, c_i, \kappa_i)$ based on immutable characteristics of landowners already collected by the USDA (deciles of soil productivity and wind and water erosion). Bar (7) uses the same scoring rule as bar (6) but reduces the number of contracts allocated to landowners: only landowners with positive scoring-rule-implied social surplus $\max_j s_j(\mathbf{z}_i) - c_i - \kappa_{ij} \geq 0$ are awarded contracts. The allocations are the same as in Figure 9, but are evaluated under an objective that incorporates a cost of funds equal to 0.15.

**Posted Prices Mechanisms** Instead of administering a scoring auction, the USDA could post prices. Prices that vary across observables and the menu of contracts, $t_j(\mathbf{z}_i) = \omega_z \cdot \mathbf{z}_i + \omega_j$, achieve the same objective as the scoring rule, $s_j(\mathbf{z}_i)$. While posted prices do not meet the quantity constraint exactly, they are simple, used by the USDA (e.g. in Continuous CRP), and can also be constructed to satisfy a range of objectives that balance both efficiency and expenditures. Table G.1 illustrates. Columns (1) and (2) serve as benchmarks, and Columns (3)-(5) construct incentives $t_j(\mathbf{z}_i)$ to maximize objectives that — in contrast to our baseline counterfactuals in Table 5 — explicitly consider USDA spending.

85

# Table G.1: Posted Prices Mechanisms


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="5">tj(zi) to maximize:</th>
    </tr>
    <tr>
        <th> </th>
        <th>Status quo auction</th>
        <th>Social surplus<br/>(no quantity constraint)</th>
        <th>Social surplus<br/>with a cost of public funds</th>
        <th>Environmental value net of expenditures</th>
        <th>Dominating<br/>tj(zi)</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Social welfare</td>
        <td>120</td>
        <td>485</td>
        <td>460</td>
        <td>350</td>
        <td>403</td>
    </tr>
    <tr>
        <td>USDA spending</td>
        <td>1,615</td>
        <td>985</td>
        <td>583</td>
        <td>219</td>
        <td>1,556</td>
    </tr>
    <tr>
        <td>Landowner surplus</td>
        <td>666</td>
        <td>588</td>
        <td>336</td>
        <td>136</td>
        <td>853</td>
    </tr>
    <tr>
        <td>Environmental value</td>
        <td>1,069</td>
        <td>882</td>
        <td>707</td>
        <td>434</td>
        <td>1,105</td>
    </tr>
    <tr>
        <td>Share awarded contract</td>
        <td>0.81</td>
        <td>0.67</td>
        <td>0.55</td>
        <td>0.33</td>
        <td>0.81</td>
    </tr>
  </tbody>
</table>


*Notes*: Table presents results under current and alternative posted prices mechanisms. It tabulates social welfare (equation (16)), USDA spending, landowner surplus, and environmental value $\sum_i \sum_j B_j \left(\mathbf{z}_i^s\right) \cdot \tau\left(\mathbf{z}_i, c_i, \kappa_i\right) \cdot x_{i j}$ in million dollars per auction, and the share of landowners with a contract. Each landowner obtains at most one contract. Column (1) simulates the status quo. Column (2) uses social-surplus maximizing incentives across contracts, characteristics already in the scoring rule ($\mathbf{z}_i^s$), and the additionality factor. Columns (3)-(5) use the same observables as column (2) and vary the weights $\omega_z$ and $\omega_j$. Column (3) calculates $t_j\left(\mathbf{z}_i\right)$ to maximize $\sum_i \sum_j\left(B_j\left(\mathbf{z}_i^s\right) \cdot \tau\left(\mathbf{z}_i, c_i, \kappa_i\right)-c_i-\kappa_{i j}-0.15 \cdot t_j\left(\mathbf{z}_i\right)\right) \cdot x_{i j}$, and column (4) calculates $t_j\left(\mathbf{z}_i\right)$ to maximize $\sum_i \sum_j\left(B_j\left(\mathbf{z}_i^s\right) \cdot \tau\left(\mathbf{z}_i, c_i, \kappa_i\right)-t_j\left(\mathbf{z}_i\right)\right) \cdot x_{i j}$. Column (5) calculates $t_j\left(\mathbf{z}_i\right)$ to implement an allocation with higher social welfare, landowner surplus, and environmental value, and lower USDA spending, holding the quantity constant on average across auctions.

**Alternative Objectives** Our baseline assumption evaluates social welfare based on equation (16). We explore the possibility that the USDA may place higher weight on landowner surplus than environmental services procured (net of spending), and whether this could rationalize the performance of the status quo auction. We consider the following objective:

$$ (26) \quad \sum_i \sum_j\left(\frac{2}{1+\lambda} \cdot\left(B_j\left(\mathbf{z}_i^s\right) \cdot \tau\left(\mathbf{z}_i, c_i, \kappa_i\right)-r_{i j}\right)+\frac{2 \lambda}{1+\lambda} \cdot\left(r_{i j}-c_i-\kappa_{i j}\right)\right) \cdot x_{i j}, $$

where $r_{i j}$ is the transfer from the USDA to the landowner and $\lambda$ parameterizes the relative weight on landowner surplus versus environmental services procured net of costs. For $\lambda = 1$, equation (26) is equivalent to equation (16). In Figure G.4, we report the relative performance of the highest performing auction (bar (7) in Figure 9) relative to the status quo (bar (1) in Figure 9), evaluated based on equation (26) as we vary the weights $\lambda = \{1, 2, 10, \infty\}$.

86

Figure G.4: Evaluating Alternative vs. Status Quo Auctions under Alternative Objectives


<table>
  <thead>
    <tr>
        <th>Scenario</th>
        <th>Relative Performance (Best Alternative vs. Status Quo)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Equal Weight</td>
        <td>3.66</td>
    </tr>
    <tr>
        <td>2x Landowner</td>
        <td>1.22</td>
    </tr>
    <tr>
        <td>3x Landowner</td>
        <td>1.02</td>
    </tr>
    <tr>
        <td>5x Landowner</td>
        <td>.91</td>
    </tr>
    <tr>
        <td>10x Landowner</td>
        <td>.84</td>
    </tr>
    <tr>
        <td>All Landowner</td>
        <td>.79</td>
    </tr>
  </tbody>
</table>


*Notes:* Figure reports the ratio of the objective in equation (26) evaluated at the allocation under the best performing auction (bar (7) in Figure 9 versus the status quo (bar (1)) in Figure 9), for varying weights $\lambda = \{1, 2, 10, \infty\}$.

**Limited Information Scoring Rule** We assume that the function $\tau\left(\mathbf{z}_i, c_i, \kappa_i\right)$ is stable across time. Our baseline alternative scoring rule also requires knowledge of both the distribution $F_{c, \kappa} \mid \mathbf{z}$ and the function $\tau\left(\mathbf{z}_i, c_i, \kappa_i\right)$ to construct. This assumption and demanding information requirements could limit the use of the alternative scoring rules prospectively.

We therefore construct an alternative scoring rule that requires no knowledge of the distribution of $F_{c, \kappa} \mid \mathbf{z}$ (or our estimated $\tau\left(\mathbf{z}_i, c_i, \kappa_i\right)$) to construct. We can evaluate this scoring rule out-of-sample to assess our assumption about the stability of $\tau\left(\mathbf{z}_i, c_i, \kappa_i\right)$. As an additional benefit, it is also a far simpler procedure than our baseline approach and could reasonably be implemented by a program administrator. We estimate the rule as follows, pooling past auctions:

1. Estimate tract-level regressions of cropping rates on observables $\mathbf{z}_i$ in the year of the auction, prior to the contract (see estimates in Figure D.7). We use as observables the same 32 cells of prior land use, prior CRP status, and soil productivity as in our baseline parameterization. Call the fitted values from this regression $\tilde{\tau}(\mathbf{z}_i)$.

2. Collect the RD point estimates of additionality across past auctions $\hat{\beta}_g^a$ (see estimates in Table D.2). Define $\tau(\mathbf{z}_i) = \tilde{\tau}(\mathbf{z}_i) + v$ and estimate $v$ to match the RD additionality estimates, such that

$$ \mathbb{E}\left[\tau(\mathbf{z}_i) - \hat{\beta}_g^a \mid S_g - b < s\left(\mathbf{b}_i^*, \mathbf{z}_i^s\right) = < S_g + b\right] = 0 $$

for bandwidth $b$, with equal weight to each past auction.

3. Project onto characteristics used in the scoring rule. Regress $B_j\left(\mathbf{z}_i^s\right) \cdot \tau(\mathbf{z}_i)$ on the immutable characteristics used in the scoring rule and across contracts, obtaining different coefficients on the same observables as in our baseline approach (see equation (17)). We will refer to this scoring rule as the limited information scoring rule.

87

This approach uses only the raw data available to the USDA — matched bids and land use — without requiring estimates of costs or the ability to observe a large share of losers and delineations of their bid fields as in the 2016 auction and our baseline approach.

We construct the scoring rule using auctions pre-2016, and evaluate its performance on the held-out 2016 auction. 2016 is a useful year for testing because our estimates of $\tau (\mathbf{z}_i, c_i, \kappa_i)$ leverage data from only this most-restrictive auction. Figure G.5 presents binned scatterplots of the baseline preferred scoring rule and this alternative limited information scoring rule, estimated in the same remote-sensing data as in baseline estimation (a) and in the administrative data (b). While they do not match exactly, the two scoring rules are highly correlated. Unsurprisingly, using different datasets leads to a greater difference between the limited information and baseline rules. In particular, scoring rules constructed in the administrative data are higher on average, consistent with the larger regression discontinuity results of additionality in the administrative data (compare Table 2 column (2) versus column (1)).

## Figure G.5: Baseline vs. Limited Info Scoring Rule

(a) Constructed using Remote-Sensing Data (Same) (b) Constructed using Admin Data (Different)


<table>
  <thead>
    <tr>
        <th>Limited Information Score</th>
        <th>Baseline Score (Binned Scatter)</th>
        <th>45-degree line</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>20</td>
        <td>20</td>
        <td>20</td>
    </tr>
    <tr>
        <td>40</td>
        <td>40</td>
        <td>40</td>
    </tr>
    <tr>
        <td>60</td>
        <td>60</td>
        <td>60</td>
    </tr>
    <tr>
        <td>80</td>
        <td>70</td>
        <td>80</td>
    </tr>
    <tr>
        <td>100</td>
        <td>80</td>
        <td>100</td>
    </tr>
    <tr>
        <td>120</td>
        <td>100</td>
        <td>120</td>
    </tr>
    <tr>
        <td>140</td>
        <td>110</td>
        <td>140</td>
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
        <th>Limited Information Score</th>
        <th>Baseline Score (Binned Scatter)</th>
        <th>45-degree line</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>20</td>
        <td>10</td>
        <td>20</td>
    </tr>
    <tr>
        <td>40</td>
        <td>30</td>
        <td>40</td>
    </tr>
    <tr>
        <td>60</td>
        <td>50</td>
        <td>60</td>
    </tr>
    <tr>
        <td>80</td>
        <td>65</td>
        <td>80</td>
    </tr>
    <tr>
        <td>100</td>
        <td>75</td>
        <td>100</td>
    </tr>
    <tr>
        <td>120</td>
        <td>90</td>
        <td>120</td>
    </tr>
    <tr>
        <td>140</td>
        <td>110</td>
        <td>140</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents binned scatterplots of the baseline highest performing scoring rule (used in column (6) of Table 5) versus the limited information scoring rule described in this section, across bidders, contracts, and auctions. Panel (a) presents the limited information scoring rule estimated in the remote sensing data. Panel (b) presents the limited information scoring rule estimated in the administrative data (used in column (10) of Table 5).

Figure G.6 presents results. Figure G.6a evaluates the limited information scoring rule in the 2016 hold-out auction. We present social welfare gains — all evaluated using equation (16) and our estimates of $F_{c,\kappa} | \mathbf{z}$ and $\tau (\mathbf{z}_i, c_i, \kappa_i)$ — under the status quo auction (bar (1)), the Vickrey auction with scoring with the baseline rule (bar (2)), and the Vickrey auction with scoring with the limited information rule (bar (3)). We also use this approach to evaluate the sensitivity of our results to our choice of dataset by constructing the limited information scoring rule using the administrative data (bar (4)). We benchmark the performance gains against our baseline preferred scoring rule (bar (5)). We find that even this simple procedure to adjust the scoring rule yields substantial social welfare gains. Figure G.6b presents a similar conclusion, across all auctions.<sup>62</sup>

\*62The total quantity procured is constant across counterfactual auctions in Figure G.6a because the quantity

88

Figure G.6c also illustrates similar socially-optimal market sizes under the baseline preferred and limited information scoring rules in all seven auctions. If we construct the limited information scoring rule using the administrative data, we obtain slightly higher estimates of the socially-optimal market size, consistent with the slightly higher RD estimates of additionality in Section III.. However, social welfare gains under the scoring rule constructed in the administrative data, but evaluated in the remote sensing data, are still large (see bar (4) of Figure G.6b).

Figure G.6: Limited Information Scoring Rule: Evaluation

(a) Alternative Auctions, 2016 Evaluation (Hold Out)


<table>
  <thead>
    <tr>
        <th>Auction Type</th>
        <th>Social welfare gains per auction (million $)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>(1) Status quo</td>
        <td>91</td>
    </tr>
    <tr>
        <td>(2) Status quo rule (VCG)</td>
        <td>129</td>
    </tr>
    <tr>
        <td>(3) Limited info rule (VCG)</td>
        <td>215</td>
    </tr>
    <tr>
        <td>(4) Limited info rule, admin data (VCG)</td>
        <td>217</td>
    </tr>
    <tr>
        <td>(5) Baseline Preferred Scoring Rule (VCG)</td>
        <td>226</td>
    </tr>
  </tbody>
</table>


(b) Alternative Auctions, All Auctions


<table>
  <thead>
    <tr>
        <th>Auction Type</th>
        <th>Social welfare gains per auction (million $)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>(1) Status quo</td>
        <td>120</td>
    </tr>
    <tr>
        <td>(2) Baseline preferred</td>
        <td>440</td>
    </tr>
    <tr>
        <td>(3) Limited info</td>
        <td>424</td>
    </tr>
    <tr>
        <td>(4) Limited info, admin data</td>
        <td>382</td>
    </tr>
  </tbody>
</table>


(c) Estimated Market Size per Auction: Limited Info vs. Baseline


<table>
  <thead>
    <tr>
        <th>Auction</th>
        <th>Baseline Preferred Scoring Rule</th>
        <th>Limited Info Scoring Rule</th>
        <th>Limited Info Scoring Rule, Admin Data</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1</td>
        <td>.66</td>
        <td>.79</td>
        <td>.66</td>
    </tr>
    <tr>
        <td>2</td>
        <td>.65</td>
        <td>.78</td>
        <td>.64</td>
    </tr>
    <tr>
        <td>3</td>
        <td>.67</td>
        <td>.81</td>
        <td>.67</td>
    </tr>
    <tr>
        <td>4</td>
        <td>.71</td>
        <td>.87</td>
        <td>.76</td>
    </tr>
    <tr>
        <td>5</td>
        <td>.73</td>
        <td>.88</td>
        <td>.78</td>
    </tr>
    <tr>
        <td>6</td>
        <td>.69</td>
        <td>.85</td>
        <td>.72</td>
    </tr>
    <tr>
        <td>7</td>
        <td>.64</td>
        <td>.79</td>
        <td>.67</td>
    </tr>
  </tbody>
</table>


Notes: Figure presents estimates of the social welfare gains (panels (a) and (b)) and socially-optimal market sizes (panel (c)) under alternative auctions. Social welfare is defined in equation (16) and uses estimates of $F_{c,\kappa|z}$ and $\tau(z_i, c_i, \kappa_i)$; results reported in million dollars per auction. All auctions impose that each landowner obtains at most one contract and that total contracts awarded cannot exceed the status quo. The limited information scoring rule is constructed using only pre-2016 data (described in the appendix text). Panel (a) presents estimates for the 2016 auction only. This is the most restrictive auction used to estimate $\tau(z_i, c_i, \kappa_i)$, and is held out of the limited information scoring rule construction. Bar (1) simulates the status quo, bar (2) simulates a Vickrey auction with scoring using the status quo rule, bar (3) simulates a Vickrey auction with scoring using the limited information scoring rule estimated with the remote sensing data, bar (4) simulates a Vickrey auction with scoring using the limited information scoring rule estimated with the administrative data, and bar (5) simulates a Vickrey auction with scoring using the highest performing scoring rule in Figure (9). All bars of Panel (a) hold quantity constant at the status quo. Panel (b) presents estimates for all auctions. Bar (1) simulates the status quo and bar (2) simulates a Vickrey auction with scoring using the baseline preferred scoring rule (these replicate bars (1) and (7) of Figure (9)). Bars (3) and (4) simulate Vickrey auction with scoring using the limited information scoring rule estimated with the remote sensing data (bar (3)) and administrative data (bar (4)). Panel (c) presents estimates of the socially optimal market size (the share of landowners awarded contracts) under each scoring rule in each of the seven auctions (ordered chronologically).

constraint binds in 2016. In Figure G.6b, the auctions in bars (2)-(4) reject landowners with $\max_j s_j(z_i) - c_i - \kappa_{ij} < 0$, decreasing quantity procured relative to the status quo (bar (1)).

89

**Top-Up Actions not Affected by Additionality** For all analyses beyond those restricted to the base contract, we require an assumption about how additionality impacts the social value derived from the top-up actions that differentiate the contracts in the mechanism. This is due to fundamental data limitations. Our primary specification defines the social benefit of contracting as $B_j(\mathbf{z}_i^s) \cdot \tau(\mathbf{z}_i, c_i, \kappa_i)$. In this specification, no social benefits are generated when a landowner is not additional. This could be either because, as with land retirement, top-up actions (or close substitutes) would have occurred even absent a CRP contract. It could also be motivated by an assumption that land retirement and the top-up actions are complements in the USDA’s valuation of contracting. We view this assumption as reasonable for many of the important actions being incentivized, e.g. grassland and tree maintenance. For example, as discussed in the main text, we can test this assumption by leveraging the fact that we can distinguish between trees and grasses in the remote sensing data. We find that over 60% of rejected bidders choosing a tree contract have the majority of their bid land in trees, and 92% have some trees on their bid land.

However, in this section, we consider an alternative assumption in which the incremental actions incentivized by the contracts are always additional. Specifically, we consider an alternative valuation of contracts equal to $B_0(\mathbf{z}_i^s) \cdot \tau(\mathbf{z}_i, c_i, \kappa_i) + B^j(\mathbf{z}_i^s)$, where $B_0(\mathbf{z}_i^s)$ is the social benefit of the base action and $B^j(\mathbf{z}_i^s)$ is the incremental social benefit of the top-up action beyond the base action. This could be motivated by a scenario in which contracting impacted the specific species mix, which we assume the USDA values at $B^j(\mathbf{z}_i^s)$, even if it did not impact land retirement. Under this assumption, over one third of the total social surplus at stake is not impacted by additionality at all. This makes correctly incentivizing the top-up actions — whose relative valuations are derived solely from monetizing the scoring rule — matter substantially to the performance of the mechanism.

90

# Figure G.7: Social Welfare Under Alternative Auctions: Alternative Top-Up Assumption


<table>
  <thead>
    <tr>
        <th>Auction Type</th>
        <th>Social welfare gains per auction (million $)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>(1) Status quo</td>
        <td>630</td>
    </tr>
    <tr>
        <td>(2) Efficient allocation (not implementable)</td>
        <td>1311</td>
    </tr>
    <tr>
        <td colspan="2">Vickrey auctions with scoring</td>
    </tr>
    <tr>
        <td>(3) Status quo scoring rule</td>
        <td>915</td>
    </tr>
    <tr>
        <td>(4) Re-weight contract incentives (ωj)</td>
        <td>915</td>
    </tr>
    <tr>
        <td>(5) Re-weight zˢ (ωz)</td>
        <td>937</td>
    </tr>
    <tr>
        <td>(6) Add additional predictor τ(z)</td>
        <td>958</td>
    </tr>
    <tr>
        <td>(7) Reduce quantity</td>
        <td>970</td>
    </tr>
  </tbody>
</table>


*Notes*: Figure presents estimates of the social welfare gains (defined in equation (16), but replacing $B_j(\mathbf{z}_i^s) \cdot \tau(\mathbf{z}_i, c_i, \kappa_i)$ with $B_0(\mathbf{z}_i^s) \cdot \tau(\mathbf{z}_i, c_i, \kappa_i) + B^j(\mathbf{z}_i^s)$ where $B_0(\mathbf{z}_i^s)$ is the social benefit of the base action and $B^j(\mathbf{z}_i^s)$ is the incremental value of the top-up action) under status quo and alternative auctions. Results reported in million dollars per auction. All auctions impose that each landowner obtains at most one contract and that total contracts awarded cannot exceed the status quo, in each auction. Bar (1) simulates the status quo. Bar (2) calculates the social welfare gains under an efficient allocation that allocates contracts using all $\mathbf{z}_i$ and $(c_i, \kappa_i)$ to maximize equation (16). Due to adverse selection, this allocation may not be implementable. Bars (3)-(7) calculate social welfare under alternative Vickrey auctions with scoring (see Section B. for more details). Bars (3)-(6) hold quantity (the number of landowners allocated contracts, in each auction) constant at the status quo and change the scoring rule $s_j(\mathbf{z}_i)$ defined in equation (17). Bar (3) uses the existing scoring rule $s_j(\mathbf{z}_i) = B_j(\mathbf{z}_i^s)$. Bar (4) uses a scoring rule with the social-surplus maximizing incentives across contracts $(\omega_j)$. Bar (5) uses a scoring rule with the social-surplus maximizing asymmetry terms across bidders using characteristics already in the scoring rule $(\mathbf{z}_i^s)$. Bar (6) adds an additional characteristic to the scoring rule, a prediction of $\tau(\mathbf{z}_i, c_i, \kappa_i)$ based on immutable characteristics of landowners already collected by the USDA (deciles of soil productivity and wind and water erosion). Bar (7) uses the same scoring rule as bar (6) but reduces the number of contracts allocated to landowners: only landowners with positive scoring-rule-implied social surplus $\max_j s_j(\mathbf{z}_i) - c_i - \kappa_{ij} \geq 0$ are awarded contracts.

Figure G.7 re-creates Figure 9 under this alternative assumption. Social welfare under the status quo is higher, but the status quo still achieves less than half of the social welfare gains under the efficient allocation. The biggest difference between Figures 9 and G.7 is the large social welfare gains from the switch to the VCG mechanism, holding the scoring rule constant (bar (3)). This is because the Vickrey auction with scoring using the status quo rule now efficiently incentivizes choices across contracts. Because the top-up actions represent a large share of social welfare gains at stake in the mechanism, incentivizing them efficiently is important for the auction's performance. Adjusting the mechanism for heterogeneity in additionality is still quantitatively important: moving from bar (3) to bar (6) increases social welfare by 7% of the status quo, or 13% of the gains of the improvement between bar (1) and bar (6). Also as in our baseline estimates, quantity procured is higher than is socially optimal.

We emphasize that the exercise of Figure G.7 is not to document that results are quantitatively the same under this alternative assumption versus our baseline assumption. The assumptions are very different, so naturally lead to some different quantitative implications. Instead, we highlight that the insights from our baseline assumption are quantitatively relevant even when a large share of the surplus at stake in the mechanism ($B^j(\mathbf{z}_i^s)$) is not impacted by additionality.

91