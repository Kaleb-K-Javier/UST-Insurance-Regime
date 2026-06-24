# Mismeasuring Risk:
# The Welfare Effects of Flood Risk Information\*

Philip Mulder

The Wharton School, University of Pennsylvania

March 23 2022

**Abstract**

Rapidly improving data and models are giving homeowners more information about their disaster risk while also increasing insurance premiums for the highest risk homes. In this paper, I study the economic consequences of using better flood risk models to more accurately identify and price flood insurance for high-risk homes. I estimate my results with administrative flood insurance policy data and a novel survey measuring flood insurance demand, risk perceptions, and objective risk. To identify the effects of risk information, I use variation created by outdated elevation data and risk models that caused high-risk homes to be misclassified as low-risk. My findings show that flood risk classification provides valuable information not only for insurers, but also for homeowners. Misclassifying high-risk homes as low-risk causes owners to under-estimate their current and future flood risk, invest less in risk-reducing adaptation, and buy less flood insurance despite substantially lower premiums. Embedding these estimates in a sufficient statistics model with dynamic risk and endogenous risk beliefs and adaptation, I find that identifying and pricing the estimated six million high-risk homes outside the floodplain would increase social welfare by $138 billion.

\*I am grateful to Olivia Mitchell, Ben Keys, Ben Lockwood, and Arthur van Benthem for their patient and persistent guidance and invaluable contributions to this paper. I am indebted to Lynn Conell-Price for her collaboration designing the flood insurance demand and risk perceptions survey. I thank Susanna Berkouwer, Benjamin Collier, Donald Davis, Gilles Duranton, Arpit Gupta, Joe Gyourko, Sam Hughes, Sasha Indarte, Matt Kahn, Judd Kessler, Carolyn Kousky, Howard Kunreuther, Yanjun Liao, Corinne Low, Alex Rees-Jones, Matthew Turner and seminar participants at Wharton, Temple Fox, Philadelphia Fed, Cornell Nolan, Treasury OCC, Wisconsin Madison School of Business, American University Kogod, Florida Warrington, EPA NCEE, Berkeley Haas, CBO, New York Fed, NYU Stern, USC Marshall, Treasury OTA, and Treasury OFR for their many helpful suggestions. I thank the First Street Foundation team for their help with data access. I acknowledge financial support from the Wharton Risk Management Center’s Russell Ackoff Doctoral Student Fellowship and Wharton Behavioral Lab. All remaining errors and misclassifications are my own.

1

# 1 Introduction

Increasingly accurate risk models enable insurers and lenders to identify and appropriately price high-risk buyers. From vehicle monitoring devices that track driving habits in real time, to genetic tests that identify health risk factors, to algorithms that predict borrower default probabilities, there have never been more data about risk in financial markets.<sup>1</sup> Improved technology to predict risk changes not only supply-side behavior, but it also alters how consumers perceive and react to their risk. In the area of flooding, low-cost, accurate elevation data and hyrdological models have democratized access to property-level information about sea level rise and flood risk (First Street Foundation 2021; NOAA 2021).

Despite the promise of new flood models, it is uncertain how they will affect the dominant provider of flood insurance, the National Flood Insurance Program (NFIP). Despite insuring nearly $1.5 trillion in property value, the publicly run NFIP has long relied on out-of-date and infrequently updated flood maps to set premiums, regulations, and provide flood risk information to coastal residents.<sup>2</sup> Under ambitious but politically controversial ongoing reforms, the NFIP has recently started to incorporate newer data and models into its premiums. Flood insurance reform advocates hope that risk-rated premiums will inform coastal residents and encourage developers to invest in risk-reducing adaptation, yet critics fear that higher premiums will drive down already low flood insurance take-up rates (Flavelle, 2021).

This paper explores the impact of new risk information on homeowner risk perceptions, investment in adaptation, and welfare in the NFIP. Discerning the economic effects of improved risk rating requires understanding how homeowners will respond to new information about their risk. To do so, I build a dynamic disaster insurance model where homeowners’ risk perceptions endogenously respond to new information. A public insurer classifies homes as “high-risk” or “low-risk” to set their premiums. The insurer’s classification is not only a price but also an information signal for homeowners who use it to guide their insurance and adaptation decisions. The model identifies sufficient statistics for estimating the welfare effects of reducing the misclassification of high-risk homes.

The model emphasizes three novel effects of risk rating in insurance markets. First, reclassifying a household as high-risk has an ambiguous effect on its insurance take-up because, even as owners face higher premiums, reclassification provides information that changes their risk perceptions. Second, the information effect and premium incentives of high-risk classification increase investment in risk-reducing adaptation. Third, when changes in risk over time are correlated with current risk,

\*<sup>1</sup>See Blattner and Nelson (2021), Fuster et al. (2019), and Jin and Vasserman (2021) for examples of the economic effects of new risk rating technology in financial markets.

\*<sup>2</sup>See Armal et al. (2020) for a comparison of aggregate flood risk measures between the FEMA flood maps used by the NFIP and a current risk model.

2

misclassification causes owners to also misperceive their future risk.

A challenge to identifying the information effect of risk rating is that changes in risk classification are correlated with changes in premiums. The NFIP provides an ideal empirical setting to separately identify the price and information effects of risk classification. The NFIP’s flood maps classify homes as either inside the floodplain (i.e. high-risk) or outside the floodplain (low-risk). As these flood maps are updated with better data, many homes are reclassified from outside to inside the floodplain (from low-risk to high-risk).

Because the NFIP holds the premiums of homes newly mapped into the floodplain fixed for two years, I can identify the information effect of reclassification separately from the price effect of higher premiums. Using administrative policy data, I compare the change in the number of flood insurance policies for newly mapped floodplain homes with homes in the same zip codes that remained outside the floodplain. The flood insurance take-up rate of newly mapped homes more than doubles in the first two years after remapping relative to homes that remain outside the floodplain.

I use the gradual phase-out of the newly mapped floodplain subsidy to identify the price effect of risk reclassification. To identify the price elasticity of flood insurance demand, I compare the renewal rates of newly mapped policyholders who first bought flood insurance around the same time but face different premiums at renewal due to the structure of the newly mapped subsidy. I find a 1% increase in premiums decreases policy renewal by 0.17 percentage points.

Some of the higher flood insurance take-up inside the floodplain can be attributed to floodplain homeowners required to purchase flood insurance by their lenders. To directly identify the information effect separately from this regulatory requirement, I field a survey of 2,167 homeowners in high-risk, coastal areas. The survey instrument measuring flood insurance take-up distinguishes voluntary buyers from those subject to lender-mandated flood insurance, a distinction that is impossible to measure in administrative policy data. In addition, I elicit perceptions of current and future flood risk to directly estimate the effect of floodplain status on risk beliefs. The survey links the address of each respondent to risk information from their flood map, updated elevation data, and two independent risk models, to compare the risk perceptions and voluntary flood insurance demand of homeowners inside and outside the floodplain with similar risk.

I find that homeowners outside the floodplain are about 13 percentage points less likely to voluntarily buy flood insurance than floodplain homeowners with similar risk, despite enjoying 60% lower premiums. The survey’s risk perception measures show that non-floodplain homeowners perceive both their current and future flood risk as lower relative to floodplain owners with similar risk. The survey results show that homeowners infer their flood risk from their floodplain status, increasing the flood insurance demand of high-risk homeowners inside relative to those outside the floodplain.

3

Floodplain classification may not only affect risk perceptions, but also how new homes are adapted to reduce flood risk. Property elevation, the single most important predictor of flood damages (Wing et al., 2020), is priced into premiums inside but not outside the floodplain, creating an incentive for developers to adapt high-risk properties by placing them on taller foundations. Differences in perceived flood risk may further increase the relative demand for elevated homes inside the floodplain. The choice of home elevation at construction is particularly important given the high costs of raising an existing home, essentially making adaptation fixed over a home’s structural life.<sup>3</sup>

An empirical challenge to estimating the difference in adaptation between floodplain and non-floodplain homes is that comparing their claims conflates differences in underlying risk with differences in adaptation. I address this source of endogeneity by comparing the claims of newly mapped floodplain homes with homes that were built inside the floodplain, or “floodplain-built.” According to current flood maps, both sets of homes face similar flood risk, but only the floodplain-built homes were constructed under the information and premium incentives inside the floodplain.

To estimate the difference in adaptation between newly mapped and floodplain-built homes, I compare the claims of over 100,000 policies, controlling for specific flooding events at the census tract by floodplain level and a host of structure and policy characteristics. I find that newly mapped homes suffer 14% larger claims than their similar floodplain-built counterparts. The adaptation advantage of floodplain-built homes is even larger for new construction. Robustness tests rule out floodplain elevation mandates or adverse selection as driving these adaptation differences.

In total, these empirical results suggest that the misclassification of high-risk homes leads to less investment in their adaptation and decreases the risk perceptions and flood insurance take-up of their owners.

I then use these estimates as sufficient statistics to calculate the welfare effects of reducing misclassification. Correctly classifying a high-risk home produces two primary welfare benefits. First, it increases demand for flood insurance and adaptation by informing homeowners about their risk. Second, it reduces moral hazard by setting premiums that incentivize adaptation.

Correctly classifying a high-risk home at construction produces approximately $1,000 in social welfare gains in the first year. With increasing risk and dynamic misperceptions, these annual welfare gains rise to over $3,000 after 30 years. The present value welfare gains of eliminating risk misclassification is approximately $22,945 for each existing home and $48,840 for each new home that can adjust its adaptation. These gains represent 10-20% of median home values, suggesting that risk misclassification significantly distorts coastal housing markets.

<sup>3</sup>A 2008 FEMA report estimates that elevating a new home by an extra two feet increases construction costs by 1 to 5% (FEMA 2008). By contrast, contractors currently advertising house lifting for existing homes quote comprehensive costs of $60-$90 per square foot, or well over $100,000 for a 2,000 square foot home (Dawson Foundation Repair 2021).

4

Most closely related to this work is a series of new papers on information frictions in disaster insurance markets. Wagner (2021) finds that flood insurance take-up rates are lower than would be suggested by plausible risk aversion parameters, no evidence of adverse selection conditional on adaptation, and that homes built before the implementation of the NFIP’s minimum elevation standards have higher claims on average than those built after the program started. Lee (2021) shows that floodplain home sale disclosure requirements lower sales prices and increase vacancy rates inside the floodplain. In Hu (2021), the author finds that the digitization of flood maps and flooding in socially connected areas increased flood insurance take-up and decreased floodplain home sale prices.

My paper extends this growing literature along several dimensions. In Wagner (2021), both adaptation and risk misperceptions are treated as exogenous. These assumptions imply that risk rating premiums produces significant deadweight loss. I allow adaptation and risk misperceptions to respond to new risk information, and show that when risk classification informs homeowners, risk-rated premiums can be welfare improving. Both Lee (2021) and Hu (2021) allow new information to affect flood insurance demand, but not the adaptation of new construction. In contrast, I find that the endogenous response of adaptation to risk classification and risk-based premiums is the most important welfare effect of new risk information. Furthermore, by separately identifying and estimating adaptation, information, and price effects from risk reclassification, I calculate the first empirical welfare estimates of the effects of new risk information in an insurance setting with endogenous beliefs and adaptation as well as dynamic risk.

My study also adds to the literature on how climate risk affects housing markets. Studies have found that home prices reflect increasing risk from hurricanes, wildfires, flooding, and sea level rise (Bernstein et al., 2019; Addoum et al., 2021; Giglio et al., 2021b,a; Gibson and Mullins, 2020; Hino and Burke, 2021; Keys and Mulder, 2020; McCoy and Walsh, 2018; Ortega and Tapınar, 2018) with implications for lenders and governments (Goldsmith-Pinkham et al., 2019; Issler et al., 2020; Keenan and Bradt, 2020; Ouazad and Kahn, 2019).<sup>4</sup> By modeling insurance demand and adaptation in a real estate market with dynamic risk, this paper demonstrates that the effects of climate change will depend on the efficiency of disaster insurance markets and quality of risk information. My welfare model results show that quickly identifying and pricing high-risk areas can attenuate climate damages.

My results are related to an urban economics literature on adaptation to disaster risk as a function of income, location, and housing supply (Baylis and Boomhower, 2019, 2021; Boustan et al., 2012; Kahn, 2005). This paper shows that disaster insurance markets, through both their price-setting and information provision functions, are key drivers of adaptation. Furthermore, I place disaster

<sup>4</sup>For a contrasting study that finds no effect of sea level rise risk on home prices, see Murfin and Spiegel (2020).

5

adaptation in an empirically estimable model that calculates the welfare implications of new risk information with dynamic climate risk.

I also contribute to the literature on welfare in insurance markets, which has used insights from the sufficient statistics literature to estimate welfare counterfactuals from empirically estimable reduced form parameters (Chetty et al., 2009; Chetty, 2009; Einav et al., 2010; Handel et al., 2019; Kleven, 2021). I enrich these frameworks by allowing consumers’ risk beliefs and adaptation to endogenously respond to new information from insurers. Even with dynamic risk, prices, information, and moral hazard, my model’s results still turn on quasi-experimentally identified reduced-form parameters with minimal assumptions about information or expectations.

My model of insurance demand and adaptation uses the proposition, extending back to Ehrlich and Becker (1972), that consumers substitute between formal insurance and adaptation, or “self-protection”. Another of my model’s main motivations, that consumers learn from the same information insurers use to set prices, is reflected in Kunreuther and Pauly (2004). My paper combines these two insights, allowing new information from insurers to change consumers’ risk beliefs and their demand for self-protection. My results on the effect of risk rating on adaptation provide some of the first empirical evidence on how insurance pricing affects self-protection, or “ex-ante moral hazard” (Dave and Kaestner, 2009; Zweifel and Manning, 2000).

My paper adds to our understanding of how financial and risk literacy affect welfare in household finance. Previous studies have shown that consumers often underweight “low probability, high consequence” risks (Dimmock et al., 2021; Hogarth and Kunreuther, 1989; Meyer and Kunreuther, 2017). Risk misperceptions can cause households to make suboptimal financial and insurance decisions (Behrman et al., 2012; Brown et al., 2021). My survey evidence reveals that correct risk classification can change risk perceptions and increase demand for flood insurance, showing how information can reduce financial literacy biases.

Finally, my results add to a literature studying the determinants of flood insurance demand. This literature has identified a puzzlingly low flood insurance take-up rate and offered explanations such as risk misperceptions, crowd-out from disaster aid or mortgage default, and learning frictions (Bradt et al., 2021; Gallagher, 2014; Kousky, 2017; Kousky et al., 2018a; Liao and Mulder, 2021; Mulder, 2019; Netusil et al., 2021; Petrolia et al., 2013). I identify miclassification of high-risk properties as one contributor to low flood insurance demand. My survey provides the first evidence of the effect of floodplain designation on risk perceptions and insurance demand while controlling for differences in prices and lender-required insurance.

In what follows, I first discuss institutional details around the NFIP. Next, I model the endogenous relationships between risk classification, risk perceptions, and adaptation. Then, I describe the data,

6

empirical strategies, and results for calculating welfare from the model. Finally, I discuss welfare counterfactuals from using new risk information to improve risk classification.

# 2 The National Flood Insurance Program

The National Flood Insurance Program (NFIP) is a publicly run flood insurer. The program is the primary provider of flood insurance in the United States, writing over 95% of flood insurance policies (Kousky et al., 2018b). Below, I describe the NFIP’s rate-setting practices over the estimation sample period, before its recent “Risk Rating 2.0” reforms (FEMA 2021).

The program’s insurance underwriting is based on flood maps produced by the Federal Emergency Management Agency (FEMA). FEMA flood maps use elevation data and hydrological models to delineate the land area that has at least a 1% chance of flooding each year, and the corresponding flood depth with a 1% chance of being exceeded (“100-year flood depth”). Inside the floodplain, a property’s premiums are based on the elevation of its first floor relative to the 100-year flood depth. Outside the floodplain, premiums are independent of elevation or any other measure of flood risk and generally lower than premiums inside the floodplain.

The floodplain is not only used to set premiums but also to guide relevant regulations. The NFIP requires all floodplain homes built since the community’s first flood map, typically 1975-1985, to be built so that their first floor elevation equals or exceeds the 100-year flood depth. Mortgage borrowers inside the floodplain are often subject to mandatory flood insurance. Lenders who are federally regulated or selling a loan to a federally backed agency must require their borrowers inside the floodplain to carry flood insurance equal to the minimum of the structure’s replacement cost, loan value, or the coverage limit of $250,000.<sup>5</sup>

Starting in the early 2000s, new lidar data made it possible to gather inexpensive and increasingly accurate ground elevation data over large areas (GAO 2004). These new elevation data led to many map revisions, prompting the NFIP to offer the Newly Mapped Subsidy program in 2015. Under the Newly Mapped Subsidy, a homeowner whose property was remapped into the floodplain may still purchase flood insurance within two years of remapping at lower, non-floodplain premiums. Beginning in the second calendar year after the new flood map, renewing remapped policies are subject to approximately 15% annual premium increases. Even in the oldest remapped areas, premiums under the Newly Mapped program remain below their corresponding floodplain rates.

\*<sup>5</sup>A history of federal reports have suggested that the mandatory purchase requirement may not be well enforced, and no administrative data can actually measure compliance (GAO 2021). In my survey of coastal homeowners, I find that most floodplain mortgage borrowers report being subject to the mandatory purchase requirement.

7

# 3 Theoretical Framework

This model describes a housing market with disaster insurance and dynamic risk when beliefs, adaptation, and premiums are all endogenous to information. An insurer’s risk-rating technology provides public signals that simultaneously inform premiums and buyers’ beliefs about their risk. Buyers choose how much to insure and how much to invest in risk-reducing adaptation based on their beliefs and premiums. Although the setting here considers flood insurance, the model could be applied to wildfire risk, auto insurer driver monitoring devices, wellness incentives in health insurance, or the pricing of risk in lending.

## 3.1 A Model of Flood Insurance in Housing Markets with Endogenous Adaptation and Beliefs

Consider a model that proceeds over $t = 0, \dots, T$ periods with a population of homes $h$ each occupied by a homeowner $i$. Each home is endowed with a flood risk type $F_h \in \{0, 1\}$ denoting low-risk and high-risk, respectively. In each period $t \geq 1$, homes flood with probability $p$ causing flood damage $R_t^{F_h}$ where $R_t^0 < R_t^1$. At $t = 0$, homeowners can invest in adaptation $A_h \geq 0$ to decrease their home’s flood damage to $(1 - A_h)R_t^{F_h}$. Adaptation incurs $c_h(A)$ in maintenance costs each period where $\frac{dc_h}{dA} \geq 0$, $\frac{d^2c_h}{dA^2} \geq 0$, and $c_h(0) = 0$. For $t > 0$, adaptation is fixed. Before flooding is realized each period, homeowners can insure share $0 \leq I_{iht} \leq 1$ of their flood damage at rate $P_{ht}$ from a public insurer. After flooding is realized, owners pay their adaptation maintenance, insurance premiums, and any uninsured flood damage and consume their remaining per-period income $Y$.

Premiums are set according to a public signal $f_h \in \{0, 1\}$ at $t = 0$ from the insurer’s risk modeling technology correlated with each home’s true risk type $F_h$. We will assume that the risk model always correctly classifies low-risk homes, but misclassifies high-risk homes as low-risk with probability $\rho$.$^6$ If its risk model shows a home as high-risk, the insurer measures a property’s adaptation and sets its premiums equal to its expected costs, times a loading fee $\kappa$, denoted $D_{ht}$: $P_{ht}^1 = \kappa p(1 - A_h)R_t^1 = D_{ht}$. For low-risk homes, the insurer does not measure adaptation and sets a flat premium $P_{ht}^{F0} = \bar{P}_t$.$^7$

The insurer must be budget neutral at the end of each period and levies taxes or remits surpluses, equal to $\tau_t$ per household, to meet ex-post discrepancies between premiums and claims. The following budget constraint holds in expectation:

\*6For clarity of exposition and to focus on the empirical application of identifying and pricing homes with high flood risk, we ignore the case where the model misclassifies low-risk homes as high-risk. The model can be extended without loss general of generality to incorporate such two-sided misclassification, and the welfare effects of reducing low-to-high misclassification can be estimated in the same framework.

\*7Transaction costs are one reason an insurer would measure adaptation for high-risk but not low-risk homes. The cost of elevation certificates, estimated to average $600, has led the National Flood Insurance Program not to use elevation to set premiums outside the floodplain (Price, 2019).

8

$$ \overline{\tau_t} = E \left[ \int_h I_{iht} * (P_{ht} - D_{ht}) \right] \eqno(1) $$

Owners do not observe their homes’ risk types directly, but they do observe some private information about their risk type and the public classification signal $f_h$ from the insurer’s model. If the insurer’s model shows a low-risk signal, the owner perceives their risk type as $R_{iht}^{B0} = (1 - \alpha_h)R_t^0 + \alpha_h R_t^1$, where $\alpha_h$ is the probability that the owner’s home was misclassified as low-risk based on $\rho$ and their private information. If the model shows a high-risk signal, the owner knows they are high-risk with certainty and forms risk type beliefs $R_{iht}^{B1} = R_t^1$.

Let $w_i(R)$ be owner $i$’s willingness-to-pay to fully insure flood damage $R$, where $\frac{dw_i}{dR} > 0$, $\frac{d^2w_i}{dR^2} > 0$, and $w_i(0) = 0$.⁸ In each period, owners choose insurance take-up $I_{iht}$ to maximize their expected utility, which we approximate by the quasilinear function:⁹

$$ \underset{h, I_i(h)}{\text{argmax }} u_{i(ht)} = Y_i - c_h(A_h) - I_{iht} * P_{ht} - w_i((1 - I_{iht})(1 - A_h)R_{iht}^{BF}) \eqno(2) $$

Homeowner $i$’s insurance decision in each period is given by the first-order condition:

$$ P_{ht} = (1 - A_h)R_{iht}^{BF}w_i'((1 - I_{iht})(1 - A_h)R_{iht}^{BF}) \eqno(3) $$

Owners insure until the expected marginal utility of reducing losses equals the marginal price of insurance.

Next, we turn to owners’ adaptation decisions, which follow the first-order condition:

$$ \frac{dc(A_h)}{dA} = E \left[ \sum_{t=1}^T (1 - I_{iht})R_{iht}^{BF}w_i'((1 - I_{iht})(1 - A_h)R_{iht}^{BF}) - I_{iht} \frac{dP_{ht}^F}{dA} \right] \eqno(4) $$

Owners adapt until maintenance costs equal the expected future sum of benefits from reduced uninsured risk and lower premiums. Note that owners of homes classified as low-risk by the insurer have no premium incentive to adapt because $\frac{dP_h^0}{dA} = 0$.

Welfare estimation will require placing some structure on risk dynamics and owners’ beliefs. I assume that from $t = 0$ to $t = T$, low-risk homes’ flood damages remain constant while high-risk homes’ damages increase by a constant factor $\gamma$. That is:

\*⁸For a buyer with concave utility $u$ and income $Y$ facing a loss $R$ with probability $p$, $w$ is defined such that $u(Y - w) = (1 - p) * u(Y) + p * u(Y - R)$. This approximation and the quasilinear approximation used in equation (2) assume away insurance demand income effects.

\*⁹The insurance willingness-to-pay of owners classified as low-risk will be $\overline{w}(R^{B0}) = (1 - \alpha)w(R^0) + \alpha w(R^1)$, reflecting the probability of misclassification, while $\overline{w}(R^{B1}) = w(R^1)$. For notational simplicity I simply write $\overline{w}(R^{BF}) = w(R^{BF})$.

9

**Assumption 1** *The dynamic flood damages of low-risk and high-risk homes are, respectively:*

$$R_t^0 = R_0^0$$

$$R_t^1 = (1 + t * \gamma)R_0^1$$

*The respective flood damage expectation of buyers classified as low-risk and high-risk are:*

$$E[R_{iht}^{B0}] = (1 - \alpha_h)R_0^0 + \alpha_h(1 + t\gamma)R_0^1$$

$$E[R_{iht}^{B1}] = (1 + t\gamma)R_0^1$$

I will assume that the insurer adjusts premiums continuously for the changing expected costs according to the respective $\gamma$ terms, such that $P_{ht}^1 = (1 + t\gamma)P_{h0}^1$ and $\bar{P}_t = (1 + t\gamma^0)\bar{P}$.

Although assumption 1 does not incorporate much of the fundamental uncertainty or heterogeneity in climate projections, it does capture the most salient feature of relative flood risk dynamics between low-risk and high-risk homes. In section 6.1, I describe the calibration of $\gamma^1$ and provide evidence that assumption 1 is plausible. The First Street Foundation Flood Model, described in section 4.2, projects little change in the expected flood damages of homes with minimal flood exposure today through 2050. By contrast, a large majority of homes today with high flood risk today will see substantial increases in their expected losses by 2050.

## 3.2 Welfare Effects of Risk Information

Next, I describe how information from improved risk modeling technology affects social welfare by reducing the insurer’s misclassification rate $\rho$ of high-risk properties. A household’s classified risk type, $f \in \{0, 1\}$, determines its choices of insurance and adaptation, $I_{iht}^f$ and $A_{iht}^f$. Using this notation, we express each household’s net impact on social welfare by the sum of its private utility and net fiscal impact $v_{iht}^F(I_{iht}^f, A_h^f) = u_{iht}^F(I_{iht}^f, A_h^f) + \tau_{iht}^F(I_{iht}^f, A_h^f)$, where $F \in \{0, 1\}$ is the home’s actual risk type. Total welfare is:

$$TW = E \big[ \sum_{t=1}^T \beta^{t-1} \int_i v_{iht}^F(I_{iht}^f, A_h^f) \big]$$

Consider a counterfactual risk model improvement that reduces the misclassification rate by $d\rho$ and denote $df_h \in \{0, 1\}$ as the change in home $h$'s risk classification. I assume that all misclassified

10

homes are equally likely to be correctly classified by the improved model. I also assume that the change in welfare for households that continue to be classified as low-risk are negligible. Under these assumptions, the change in welfare from $d\rho$ is:

$$
\begin{aligned}
\frac{TW}{d\rho} = & E \big[ \sum_{t=1}^{T} \beta^{t-1} \int_{df_h=1} v_{iht}^1(I_{iht}^1, A_h^1) - v_{iht}^1(I_{iht}^0, A_h^0) \big] \\
= & E \big[ \sum_{t=1}^{T} \beta^{t-1} \int_{df_h=1} (v_{iht}^1(I_{iht}^1, A_h^1) - v_{iht}^1(I_{iht}^0, A_h^1)) + (v_{iht}^1(I_{iht}^0, A_h^1) - v_{iht}^1(I_{iht}^0, A_h^0)) \big] \quad (5) \\
= & E \big[ \sum_{t=1}^{T} \beta^{t-1} \int_{df_h=1} \int_{I_{iht}^0}^{I_{iht}^1} \frac{dv_{iht}^1(I, A_h^1)}{dI} dI + \int_{A_h^0}^{A_h^1} \frac{dv_{iht}^1(I_{iht}^0, A)}{dA} dA \big]
\end{aligned}
$$

Equation (5) shows that the welfare gains from better risk information are the sum of the effects from changing the insurance take-up and adaptation of otherwise misclassified households. How does reclassification change these decisions? Consider a high-risk household with higher premiums post-reclassification, so that $p\kappa(1 - A_1)R_t^1 > \overline{P}_t$.<sup>10</sup> Equation (3) implies that the sign of $I^1 - I^0$ is ambiguous. Reclassification causes an insurance price effect that dampens insurance demand as $P^1 > \overline{P}$, but also an information effect that increases insurance demand if $R^{B0} < R^1$. By contrast, the first-order condition in equation (4) unambiguously implies that $A^1 > A^0$. After being reclassified as high-risk, the household has an extra premium incentive to adapt, the insurance price effect leads the household to substitute from insurance to adaptation, and any information effect further moves the household to adapt by increasing their perceived uninsured risk.

We will simplify equation (5) by aggregating insurance demand and adaptation up to the market of reclassified households and approximating the differences in welfare with trapezoidal Harberger triangles. Let $I_t^f = \int_{df_h=1} I_{iht}^f$ and $A^f = \int_{df_h=1} A^f$, and $D_t^f = \int_{df_h=1} p(1 - A_h^f)\kappa R^1$. Define $W_t^f(I, A^f)$ as the insurance willingness-to-pay curve such that for insurance premium $P = W_t^f(I^q, A^f)$, $\int_{df_h=1} I_{iht}^f(P, A_h^0) = I^q$. We can approximate equation (5) as:<sup>11</sup>

\*<sup>10</sup>This describes the large majority of floodplain NFIP premiums relative to their counterfactual non-floodplain premiums.

\*<sup>11</sup>Equation (6) includes an additional second order welfare term, $\frac{1}{2}(A^1 - A^0) \frac{dv_t^1(I^0, A^1)}{dA}$, that I omit here for clarity.

11

$$ \frac{TW}{d\rho} = \sum_{t=1}^{T} \beta^{t-1} (\overbrace{\lambda I_t^0 * (D_t^0 - \overline{P}_t)}^{\text{Public Funds}} + \frac{1}{2} * \underbrace{((I_t^1 - I_t^0)(W_t^1(I_t^0, A^1) - D_t^1)}_{\text{Take-Up}} $$
$$ + \overbrace{\frac{(1 - A^1)}{(1 - A^0)} ((1 - I_t^0)(W_t^1(I_t^0, A^0) - E_{t=0}[W_t^0(I_t^0, A^0)])}^{\text{Adaptation}} + \underbrace{(1 + \lambda)I_t^0 D_t^0)}_{\text{Moral Hazard}})) \tag{6} $$

Equation (6) divides the welfare effects of improved risk-rating into four terms that reflect the deadweight losses caused by misclassification:

1. Public Funds: Raising public funds to cover net outlays on misclassified homes creates social costs, reflected by $\lambda$

2. Take-Up: Due to a combination of the price effect and information effect of misclassification, insurance take-up is too low ($I^1 > I^0$) or too high ($I^1 < I^0$) so that marginal willingness-to-pay does not equal marginal costs to the insurer

3. Adaptation: Households underinvest in adaptation because they underestimate their uninsured flood risk due to misclassification

4. Moral Hazard: Households underinvest in adaptation because they have no premium incentive to reduce their insured risk

The adaptation and take-up terms highlight the importance of accounting for the information effects of improved risk rating. If reclassification has no information effect, or $F^{B0} = R^1$, then the adaptation effect is 0 because $W_t^1 = E_{t=0}[W_t^0(I_t^0, A^0)]$. In the take-up effect, when $I^0 > I^1$ this implies that $W^1(I^0, A^1) < P^1$. In this case, the take-up can be evaluated as a standard deadweight loss triangle that results from pricing a good below its marginal cost.<sup>12</sup> By contrast, if $I^0 < I^1$ then $W^1(I^0, A^1) > P^1$. In this scenario, because the information effect overpowers the price effect, misclassified households would purchase too little insurance despite their lower premiums.

Equation (6) also shows how dynamic risk changes the welfare effects of risk information. Applying Assumption 1, $D_t^f = (1 + t\gamma)D_0^f$ for risk growth rate $\gamma$. As risk changes dynamically, so do risk perceptions. Increasing risk effectively erodes adaptation, so that $W_t^1(I, (1 - A)) = W_0^1(I, (1 + t\gamma) * (1 - A))$.<sup>13</sup> While correctly classified households update their insurance willingness-to-pay accord-

\*<sup>12</sup>However, one must still correctly estimate $W^1$ to estimate the area of the deadweight loss triangle. If there was an information effect so that $F^{B0} < R^1$, then $W^1(I^0, A^0) > \overline{P}$, whereas a revealed preference analysis assuming no information effect would conclude $W^1(I^0, A^0) = \overline{P}$.

\*<sup>13</sup>I express $W$ as a function of $(1 - A)$ rather than $A$ here to simplify its expression with $\gamma$.

12

ing to their increasing risk, misclassified households only have willingness-to-pay $W_t^0(I, (1 - A)) = W_t^0(I, (1 + t\gamma^0) * (1 - A))$. If $\gamma^0 < \gamma$, then the wedge between $W_t^1$ and $W_t^0$ will increase with $t$.

## 3.3 Heterogeneity and Sorting

As emphasized in the sufficient statistics literature dealing with biased agents, heterogeneous frictions can substantially affect welfare conclusions (Handel et al., 2019; Lockwood, 2020; Taubinsky and Rees-Jones, 2018). In the model described above, I allow for heterogeneity in the information effect of reclassification, $F^{B1} - F^{B0}$. My welfare calculations take the most conservative approach to how this heterogeneity might interact with welfare in terms of take-up. To illustrate, say the price effects and information effects exactly cancel out so that $I^0 = I^1$. The take-up effect in equation (6) would equal zero in this case. In reality, risk classification likely creates a more efficient ordering of households along the insurance demand curve, so that $I^1$ has a different distribution of take-up across households than $I^0$. Thus, the welfare effect would actually be positive unless all households maintain the exact same insurance take-up under $I^1$ and $I^0$.

The model assumes that risk information does not change where homeowners live and does not allow household to move. Allowing for sorting would increase the welfare effects of risk information. If risk information changes households’ willingness-to-pay for different homes, subsequent resorting represents an additional welfare effect similar to the take-up and adaptation terms in equation (6).¹⁴

Finally, implicit in the model is an assumption of no adverse selection for correctly classified high-risk households. I argue this assumption is plausible for flood insurance. As discussed in section 2, the last decade has seen rapid advances in flood risk modeling technology. The granular data on elevation and historical flooding used in advanced flood models surpass the information available to most homeowners. An extensive literature has documented homeowners’ difficulties gauging flood risk due to biased and costly learning about rare events and cognitive biases.¹⁵ More generally, the insurance literature has found mixed or weak evidence of selection due to asymmetric information in settings such as life, health, and auto insurance, even under minimal risk adjustment.¹⁶ Thus, it is reasonable to assume that under improved risk rating counterfactuals the flood insurance market would have minimal adverse selection on fully risk rated policies.

\*14 Bakkensen and Ma (2020) find evidence that households sort of flood insurance preferences in Miami-Dade county.
\*15 See Bakkensen and Barrage (2017); Gallagher (2014); Kunreuther and Pauly (2004); Mulder (2019); Wagner (2021) for examples of frictions in flood risk perceptions and the relationship between risk and insurance demand.
\*16 See for example Cohen (2005); Cutler et al. (2008); Einav et al. (2010); Finkelstein and Poterba (2004); Finkelstein and McGarry (2006); McCarthy and Mitchell (2010).

13

## 3.4 Empirical Parameters for Welfare Estimation

I will estimate welfare from my model described above in the setting of the National Flood Insurance Program (NFIP). The model’s risk types correspond to FEMA’s mapping of properties inside (“high-risk”) or outside (“low-risk”) the floodplain. The counterfactual of interest is the welfare effects of new technologies that allow the NFIP to identify high-risk areas more quickly and accurately.

Following the literature of sufficient statistics models, the welfare formula described in equation (6) is made up of terms that can be empirically estimated with reduced form methods (Chetty, 2009; Einav et al., 2010; Kleven, 2021; Wagner, 2021). Several of the parameters can be calibrated from data summary statistics, risk models, or adopted from relevant papers. Unlike these terms, however, the adaptation and insurance behavioral responses must be causally identified.

To calculate equation (6), I need to identify three causal effects: The price elasticity of insurance demand (“price effect”), the information effect of risk classification on insurance demand (“information effect”), and the effect of risk classification on adaptation investment (“adaptation effect”).

The next sections describe the data and empirical strategies used to estimate each of these causal effects.

# 4 Data

To estimate the sufficient statistics for welfare analysis, I combine several data sources to compare the flood insurance demand and adaptation of properties classified as low- and high-risk. First, I use administrative data on National Flood Insurance Program (NFIP) policies and claims in zip codes where homes outside the floodplain have been remapped into the floodplain. Second, I design and field a survey of the flood insurance demand and risk perceptions of homeowners in flood-exposed coastal areas. Survey responses are merged to two property-level risk models and flood map information. The section below describes how these data are used to construct the analysis samples and variables.

## 4.1 National Flood Insurance Program Policy and Remapping Data

I use data on National Flood Insurance Program (NFIP) policies and claims from the OpenFEMA Redacted Policies and Redacted Claims databases (OpenFEMA 2021). These data show the universe of policies and their respective claims from 2009-2020 with information on the amount of coverage purchased and deductible, the date the policy was originally written and most recently renewed, location at the census tract level, and the property year built and number of stories. The claims data record the date of loss and amount paid for each claim.

14

The primary empirical variation for estimating the Floodplain Adaptation Effect, Floodplain Demand Effect, and flood insurance demand function come from areas that have been newly mapped into the floodplain by the NFIP from 2015-2019. As described in Section 2, the Newly Mapped Subsidy program started by the NFIP in 2015 allows homes newly mapped into the floodplain to purchase flood insurance at their original non-floodplain rates. Two years after remapping, newly mapped homes’ premiums begin to increase by approximately 15% per year. The policy data have indicators for policies written under the Newly Mapped Subsidy program. Using these indicators and the schedule for Newly Mapped premium increases, I identify zip codes where properties were newly mapped into the floodplain and the year that the remapping occurred.

The top panel of Figure 1 maps the counties with zip codes with newly mapped policies and the year they were remapped. The estimation sample contains 359 counties with approximately 900,000 active flood insurance policies as of 2020. The bottom panel of Figure 1 plots the distribution of remapping years across newly mapped policies active in 2020. In these zip codes, average annual premiums are $530 and $1,330 outside and inside the floodplain, respectively, prior to remapping.

## 4.2 Flood Insurance Survey

Working with the Wharton Risk Center, I designed and fielded a survey of high-risk coastal homeowners to test the robustness of and mechanisms driving the Floodplain Demand Effect estimated from the newly mapped policy data. The survey was conducted through Online Qualtrics Panels from March-May 2021. Participants were screened for single-family homeowners living in one of 950 highest risk coastal zip codes identified from the First Street Foundation Flood Model, or FSF-FM (First Street Foundation 2021).<sup>17</sup>

2,167 participants completed the survey. Of these, the estimation sample was limited to the 1,080 respondents age 25 or older willing to volunteer their home address and who provided a purchase year for their homes within two years of CoreLogic deed records. Of these, the sample was further restricted to 885 participants who passed an attention check, and finally to 702 participants whose homes were within one mile of the floodplain boundary. Figure 2 maps the number of respondents by county in the survey estimation sample.

The survey elicited whether each participant had flood insurance. A common misperception among homeowners is that flood insurance is included in their standard homeowners insurance policy, but this is almost uniformly untrue. Among participants who reported having flood insurance, the survey asked if the policy was included in their homeowner’s insurance policy and whether they paid a separate premium for their flood insurance. Respondents who indicated that their flood insurance

\*<sup>17</sup>The detailed methodology behind the FSF-FM is described in First Street Foundation (2020).

15

was part of their homeowner’s policy and that they did not pay a separate flood insurance premium were not counted as flood insurance purchasers.<sup>18</sup>

A challenge assessing flood insurance demand is the mandatory purchase requirement imposed by most lenders on borrowers inside the floodplain. As described in Section 2, federally regulated lenders or lenders writing a loan securitized with federally backed enterprises must require floodplain borrowers to carry flood insurance. All respondents reporting flood insurance and a mortgage were asked if their lender requires flood insurance. Respondents indicating that they face a mortgage purchase requirement were asked how likely they would be to buy flood insurance if their lender didn’t require it on a 1-5 Likert scale. Only respondents that indicate they would be “Somewhat likely” or “Extremely likely” to buy flood insurance in the absence of the mandatory purchase requirement are counted as voluntary flood insurance purchasers. The survey asks participants about their flood risk perceptions. The primary risk perceptions measure is “What is the chance that your home will be damaged by a flood in the next ten years?”, which participants answered from 0 to 100%. Table A1 reports summary statistics from the survey estimation sample.

The survey also assesses risk perceptions with an incentivized hypothetical insurance purchase scenario. Participants were asked how much they would be willing to pay for an annual flood insurance policy on their homes. Each respondent was told that their (hypothetical) home may be damaged by a flood, which will be determined by a randomly selected claim from a zip code with risk similar to their (actual) home’s risk.<sup>19</sup> If they bought flood insurance, they suffered no damage from any claim but paid a randomly drawn premium instead. If they did not buy flood insurance, they paid any realized claim but no premium. Their maximum willingness-to-pay for this hypothetical insurance product was elicited through a series of potential prices that narrowed their minimum and maximum willingness-to-pay before asking their exact willingness-to-pay within that range. The bonus was paid as a 1% chance of having a $100 Amazon gift card mailed to their address, minus flood insurance premiums or flood losses.

Each survey respondent in the estimation sample was linked to their property-specific FSF-FM and KatRisk flood risk models. Both models estimate average annual flood losses and expected flood depths across different return periods (e.g. the “500-year” or “100-year” flood). The primary risk model measure is the average 100-year flood depth between the two models. Averaging the results of the two models attenuates the idiosyncracies of relying on any one flood risk estimation methodology. Reassuringly, even though the two models were built independently with different methodologies, their 100-year flood depths have a correlation coefficient of 0.58 in the estimation sample. The survey

\*<sup>18</sup>16% of respondents thought flood insurance was a standard part of their homeowner’s policy. This misperception was more common outside the floodplain (18%) than inside the floodplain (9%).

\*<sup>19</sup>The claims were randomly drawn from the last ten years of NFIP claims in a zip code that matches the predicted claims of the respondent’s zip code according to the FSF-FM.

16

also asked each participant if their current home had been damaged by a flood while they lived there.

The FSF-FM includes a climate projection of the changes in expected flood depths through 2050. I use the change in average annualized losses between 2020 and 2050 as a measure of the expected 30-year change in flood risk.

Respondents’ homes’ locations were linked to their most recent flood map and elevation data. I measure properties’ current FEMA floodzones and distances from the nearest floodplain boundary. For homes inside the floodplain, I record the associated 100-year flood depth. For homes outside the floodplain, I measure the 100-year flood depth of the nearest floodplain. The FSF-FM includes the most recent ground elevation of each property according to the USGS National Map data.

# 5 Estimating Sufficient Statistics for Welfare

In this section, I describe the methods and results for estimating the three sufficient statistics to calculate welfare with equation (6): The price effect, information effect, and adaptation effect.

## 5.1 Remapping, Take-Up, and Premiums

A challenge estimating the price elasticity of flood insurance demand is that different premiums can also imply different risk classifications, conflating the information and price effects. The National Flood Insurance Program’s Newly Mapped Subsidy program, described in section 4.1, delays flood insurance premium increases for homes newly remapped into the floodplain for two years, after which their rates increase by approximately 15% annually, independent of risk. This variation separates premium increases from initial reclassification, which I use to identify the price effect.

I first study how remapping affects flood insurance take-up and premiums for newly mapped properties using the sample of remapped counties described in section 4.1. Although providing suggestive evidence of a positive information effect and negative price effect, I will argue that this empirical exercise alone does not identify either effect. In subsequent sections, I will use additional data and variation to causally identify both effects.<sup>20</sup>

My estimating equation is given by:

$$ Policies_{inmt} = \alpha_{imt} + \alpha_{inm} + \sum_{t=m}^{t=2020} \lambda^{t-m} I[NewlyMapped_n = 1] + \epsilon_{inmt} \quad (7) $$

where $Policies_{inmt}$ is the inverse hyperbolic sine transformed count of active policies in county $i$ that were remapped into the floodplain ($n = 1$), “newly mapped”, or remained outside the floodplain

\*<sup>20</sup>This initial empirical exercise is still useful as the first empirical evaluation of the Newly Mapped Subsidy program, even if the welfare implications of its results are ambiguous.

17

($n = 0$), "non-floodplain", during remapping that occurred in year $m$ as of year $t$. To estimate changes in premiums, I replace policies with log average premiums as the dependent variable. $\alpha_{imt}$ is county by remapping year by year interacted fixed effects, and $\alpha_{inm}$ is county by newly mapped status by remapping year interacted fixed effects. $\lambda^{t-m}$ traces out the dynamic effects of remapping on the number of newly mapped policies from the year of remapping ($m = t$) through 2020.<sup>21</sup> The key identifying assumption is that, in the absence of remapping, the premiums and policy counts of these two sets of non-floodplain policies would have remained on parallel trends.

Figure 3 plots the estimated remapping coefficients from Equation (7) for policy counts (top panel) and premiums (bottom panel) of homes remapped into the floodplain relative to homes that remained outside the floodplain. By the first year after remapping, the relative number of policies in areas remapped into the floodplain approximately doubled. By contrast, premiums one year after remapping are less than 5% higher for homes moved inside the floodplain, an increase that reflects an extra 25$ policy fee levied on policies under the Newly Mapped Subsidy program.

In the second year after remapping, newly mapped premiums begin to increase more sharply as the statutory 15% annual price increase at policy renewal take effect. Five years after remapping, premiums are 45% higher on the policies of homes moved into the floodplain. As premiums increase, the relative increase in the number of policies on homes moved into the floodplain stalls and declines. By the fifth year after remapping, there is only a 55% relative increase in newly mapped policies.

The results in Figure 3 suggest a positive information effect from the initial increase in newly mapped policies and a price effect from their subsequent relative decline. These patterns, however, do not identify the price or information effects. As described in section 2, some newly mapped homeowners may have bought flood insurance to satisfy the federal mandatory purchase requirement on their mortgage, confounding the information effect. The declining flood insurance take-up result conflates the effect of increasing premiums with any non-premium dynamic remapping effects. In particular, some of the dynamic take-up effects may be specific to the Newly Mapped Subsidy program as any newly mapped homeowner buying flood insurance more than two years after remapping would buy a normal floodplain policy.

I return to the issue of identifying the information effect in section 5.2. Next, I describe how to separately identify the price effect from the remapped policy data.

<sup>21</sup>Note that equation 7 does not estimate the $\widehat{\lambda}$ coefficients from differences in the timing of flood map changes. Rather, the $\alpha_{imt}$ fixed effects restrict the empirical comparisons to variation between newly mapped and non-floodplain homes within the same county in the same year. This approach avoids the dynamic biases in staggered difference-in-difference settings (Baker et al., 2021; Goodman-Bacon, 2021).

18

### 5.1.1 Price Effect Estimation Strategy

Separating the price elasticity of flood insurance demand requires comparing newly mapped policies making similar renewal decisions but facing different premiums. The structure of the Newly Mapped Subsidy mimics this ideal premium variation with situations where newly mapped policies originally written in the same year nonetheless face different premium changes at renewal.

To illustrate, consider a “homeowner A” mapped into the floodplain in October 2015 who buys flood insurance under the Newly Mapped Subsidy in January 2016. When they decide whether to renew in January 2017, their premium will be 15% higher because the flood map will be two (calendar) years old. By contrast, consider a “homeowner B” mapped into the floodplain in January 2016 who buys flood insurance in April 2016. When they decide whether to renew in April 2017, their premium will be unchanged because their flood map will only be one (calendar) year old.

I identify the price elasticity from the extent to which homeowner A is less likely to renew than homeowner B. The estimating equation is:

$$ Renew_{iqt} = \alpha_{qt} + \beta X_{iqt} + \sigma \Delta P_{iqt} + \epsilon_{iqt} $$ (8)

where $Renew_{iqt}$ is an indicator variable for whether policy $i$ originally written in year $q$ renews in year $t$. The equation includes originally written year by renewal year interacted fixed effects and additional controls.<sup>22</sup> Standard errors are clustered by county.

$\Delta P_{iqt}$ is the percent increase in premiums according to the Newly Mapped Subsidy schedule. $\sigma$ is the price elasticity of flood insurance demand, or the change in the probability of renewal from a 1% increase in premiums. The key identifying assumption is that the policies with different underlying map years but written in the same year would have had the same propensity to renew, absent their different premium inflation.

### 5.1.2 Price Effect Results

Table 1 shows the results from estimating Equation (8). The baseline specification, with only first policy year by renewal year interacted fixed effects, suggests that a one percent premium increase decreases the probability of renewal by 0.15 percentage points. Subsequent columns add increasing controls: State fixed effects, controls for the past two years of claims in the renewing policy’s county, and fixed effects for the month of year written. Across all of these specifications, the renewal elasticity remains around 0.17.

<sup>22</sup>Similar to the discussion around equation 7, comparing renewal rates within the same renewal year, and not across years, avoids the dynamic biases in staggered difference-in-difference settings.

19

The results in Table 1 suggest that flood insurance demand is inelastic, a finding similar to other estimates which have found elasticities of 0.25 and 0.29 (Bradt et al., 2021; Wagner, 2021). Part of the reason for this low elasticity may because some policyholders cannot drop their coverage due to the mandatory purchase requirement associated with their mortgage. In section 5.2.3, I adjust the estimated price elasticity for the share of mandatory purchasers from the survey data described in section 4.2.

What does this estimated price elasticity project for the relative take-up rate of newly mapped policies once their premiums rise to floodplain levels? I use the average non-floodplain and floodplain premiums of $530 and $1,330 from the the sample of remapped zip codes described in section 4.1. Applying the estimated price elasticity of 0.17 to an isoelastic demand function implies that the estimated 100% relative increase in newly mapped policies one year after remapping in Figure 3 would remain elevated by 71% even after premiums fully adjust to floodplain levels.

## 5.2 Information Effect

The results in Figure 3 suggest that remapping homes into the floodplain increases flood insurance demand. This increase in demand may be driven by both changes in the risk perceptions of remapped homeowners (the “information effect”) and the mandatory purchase requirement for floodplain mortgages. Thus, although remapping and the newly mapped subsidy was useful for identifying the price elasticity of flood insurance demand, it cannot separately identify the information effect.

This section uses evidence from the flood insurance demand survey to identify the information effect. The key innovation of the survey is that it measures flood insurance demand independent of lender purchase requirements while including rich measures of objective flood risk and demographics to compare the voluntary flood insurance demand of floodplain homeowners with similar misclassified non-floodplain homeowners.

### 5.2.1 Voluntary Flood Insurance Demand Methodology

As described in section 4.2, the survey directly measured whether homeowners in high-risk areas were bound by the mandatory purchase requirement, creating a novel measure of voluntary flood insurance demand inside the floodplain. By linking respondents’ addresses to flood risk models and flood map data, I can estimate the difference in voluntary flood insurance demand for homes inside versus outside the floodplain but with similar risk. The estimating equation for voluntary flood insurance demand is:

20

$$VoluntaryInsured_i = \alpha_0 + \beta_0 X_i + \beta_1 PASTFLOOD_i + \beta_2 100YEAR_i + \beta_3 Floodplain_i + \epsilon_i \quad (9)$$

where $VoluntaryInsured_i$ is an indicator variable for respondent $i$ voluntarily buying flood insurance (i.e. not due to a mandatory purchase requirement). $X_i$ is self-reported controls for log income, education, risk preferences, liquidity, sex, age, time in home, political party affiliation, mortgage status, and log home structure value. $PASTFLOOD_i$ is an indicator for whether the respondent reports their home being previously flooded, and $100YEAR_i$ is the average 100-year flood depths from the two property-level risk models. $Floodplain_i$ is an indicator variable for whether respondent $i$ was inside the floodplain. $\beta_3$ identifies the difference in flood insurance demand inside and outside the floodplain, inclusive of both the negative price effect and any information effect. The identifying assumption is that the floodplain indicator and error term are uncorrelated outside of these price and information components. Standard errors are clustered by county.

A key innovation of my survey is the ability to identify homes with similar risk inside and outside the floodplain. By FEMA’s definition, any home outside the floodplain should have a less than 1% annual chance of flooding. However, according to the average 100-year flood depth measure from the two risk models, 52% of the non-floodplain homes in the survey estimation sample are above the 1% risk threshold, compared with 86% of floodplain homes. Figure A1 plots the average 100-year flood depth of floodplain and non-floodplain homes with at least a 1% annual chance of flooding (i.e. a positive 100-year flood depth). Although the expected 100-year depths of floodplain homes are generally higher than non-floodplain homes, there is substantial overlap between the two distributions.

A concern with this identification strategy is that the risk model flood depths may be measured with error, leading the floodplain coefficient to reflect measurement error in the risk controls rather than the pure information effect. To address this concern, I apply the “Obviously Related Instrumental Variables” (ORIV) method from Gillen et al. (2019). The intuition of the method is to efficiently address measurement error by exploiting the fact that I have two draws of each home’s risk from the two different flood models. The ORIV estimates $\beta_2$ in a stacked regression where each observation is duplicated (and standard errors appropriately clustered) such that, first, the Kat Risk depth instruments the First Street depth, and, second, the First Street depth instruments the Kat Risk depth.

As long as the measurement errors in the two flood depth measures are independent, the ORIV estimates will be consistent, unlike wit a simple average of the two measures, and more efficient

21

than arbitrarily choosing one measure to instrument the other. Fortunately for this design, the two risk models were developed by independent researchers using different methodologies, limiting the potential for correlated errors.

All standard errors are clustered by county, with 500 bootstrap samples in the ORIV specification.

## 5.2.2 Voluntary Flood Insurance Demand Results

Table 2 shows the results from estimating equation (9) with an indicator for voluntary insurance purchase as the dependent variable. Column (1) includes only demographic controls and shows that floodplain homes were about 23 percentage points more likely to voluntarily buy flood insurance than non-floodplain homes. Column (2) adds the average 100-year flood depth control calculated from the two flood models, an indicator term if both flood depths are 0, and an indicator for respondents who report their home being previously damaged by a flood. Even controlling for these differences in risk, floodplain homes were about 15 percentage points more likely to voluntarily buy flood insurance than non-floodplain homeowners. These results are surprising given that floodplain respondents reported paying $1,100 per year higher flood insurance premiums than their non-floodplain counterparts (Table A1).

Column (3) applies the ORIV method for the 100-year flood depth to address potential measurement error. This specification shows that floodplain households were 13 percentage points more likely to voluntarily buy flood insurance, an estimate that is similar to the OLS estimate in column (2).

The results in Table 2 suggest that higher floodplain take-up is not primarily driven by lender mandatory purchase requirements. To provide direct evidence for whether floodplain classification drives risk perceptions, I re-estimate Equation (9) with the incentivized risk perceptions elicitation described in Section 4.2 as the dependent variable, with results in Table A2. Under this measure, which incentivizes a hypothetical flood insurance purchase with past flood insurance claims of homes with similar risk as the respondent’s, floodplain homeowners are willing to pay over 40% more for flood insurance than non-floodplain homeowners with similar risk.

Given the correlation between current risk and expected future risk, it is plausible that floodplain classification may also affect future risk perceptions.<sup>23</sup> To estimate how risk classification affects future risk perceptions, I re-estimate Equation (9) with a binary indicator for whether the respondent believed their flood risk will be higher in 30 years. I replace the elevation difference controls with controls for the property’s change in flood risk as a quadratic function of the difference in the property’s average annualized loss between 2020 and 2050.

\*23 See section 6.1 for more discussion of the empirical relationship between current flood risk and projected future risk.

22

Table 3 shows the results from estimating Equation (9) with respondents’ future risk beliefs as the dependent variable. The estimate indicates that floodplain respondents were 12 percentage points more likely to believe their flood risk would be higher in 30 years, a 27% relative increase from the non-floodplain baseline rate.

### 5.2.3 The Magnitude of the Information Effect

The results in Table 2 are evidence of the information effect. Despite their much higher premiums, homeowners inside the floodplain (classified as high-risk) are 13 percentage points more likely to buy flood insurance than non-floodplain owners with similar risk (misclassified as low-risk).

One way to gauge whether this information effect is reasonable is by comparing it with the change in take-up caused by reclassification into the floodplain after premiums fully adjust implied by the newly mapped policy estimates. As discussed in section 5.1.2, the take-up and price elasticity estimates from Figure 3 and Table 1 imply that take-up would still be 71% higher for policies remapped into the floodplain relative to those remaining outside the floodplain even after premiums fully adjust. In the survey data, 17% of insured floodplain homeowners are involuntary purchasers, implying a relative increase of 41%, or 15 percentage point, increase in the number of voluntary purchasers. The newly mapped result’s implied 15 percentage point increase in voluntary take-up is consistent with the estimated 13 percentage point increase from the survey data.

We can use the flood insurance price elasticity estimate in Table 1 to calculate the implied difference in flood insurance willingness-to-pay between floodplain and non-floodplain high-risk homeowners. I calibrate an isoelastic flood insurance demand curve for homes moved inside the floodplain from the adjusted voluntary price elasticity of 0.37, described in section C, and the average floodplain premiums of $1,330 from the policy data in newly mapped zip codes described in section 4.1.

Given average non-floodplain premiums of $530, revealed preference shows that high-risk households have a marginal willingness-to-pay for flood insurance of $530 at an average take-up rate of 36% when misclassified as low-risk. However, once moved inside the floodplain, this same revealed preference argument suggests that their marginal willingness-to-pay for flood insurance is $1,330 at an average voluntary take-up rate of 50%. The isoelastic demand curve implies that marginal willingness-to-pay for flood insurance of correctly classified high-risk households would be $3,315 at the non-floodplain 36% take-up rate.

This exercise frames the information effect as a dollar value; Reclassifying a high-risk household from low-risk to high-risk increases its marginal willingness-to-pay for flood insurance from $530 to $3,315.

23

## 5.3 Adaptation Effect

This section describes the methodology for and results of estimating the adaptation effect. The adaptation effect is defined as the difference in relative claims between two homes with similar underlying risk but built with different high- versus low-risk classifications. In the context of the National Flood Insurance Program (NFIP), this means comparing the flood damages of homes with similar risk built inside (high-risk) versus outside (low-risk) the floodplain.

A challenge to estimating the adaptation effect is that flood risk outside the floodplain is usually much lower than inside the floodplain. Given that the NFIP claims and policy data lack granular location data, any comparison of claims inside and outside the floodplain would confound differences in adaptation with differences in underlying risk.

To address this challenge, I compare the claims of policies on homes newly mapped into the floodplain (“newly mapped”) between 2010-2019 with the claims of policies on homes that were built inside the floodplain (“floodplain-built”) in the same census tracts. Although newly mapped and floodplain-built homes now have the same risk-rating, differences in their adaptation investments at construction should be persistent because of the prohibitively high costs of elevating existing homes.<sup>24</sup> Whereas newly mapped homes were classified as low-risk with no premium incentives to elevate at construction, floodplain-built homes were built with risk-rated premiums and information about 100-year flood depths from FEMA flood maps.

To construct my estimation sample, I identify census tracts with both newly mapped and floodplain-built policies that experienced large floods, or “loss events”. I define a loss event as total flood insurance claims in a single day in a single county. I subset the data to the 479 largest loss events in the 2010-2019 claims data, accounting for about 85% of total claims over the period.<sup>25</sup> I omit all policies written on homes built prior to FEMA’s first flood map in their communities, usually introduced between 1975 through the mid 80s. I also omit homes built after the NFIP started its newly mapped policy in 2015. The estimation sample consists of the 102,527 policies with 18,978 claims.

Before estimating their difference in claims, I first present evidence that floodplain-built homes are better adapted than their newly mapped counterparts. The single most important predictor of a home’s damages during a flood is the net depth of water above its first floor. Even a few feet can make a large difference; the average flood damages of homes with a one-foot net flood depth was $17,000, whereas homes with three-feet net flood depths had $33,500 average damages.<sup>26</sup> The

\*<sup>24</sup>As cited in section 1, elevating a home at construction increases construction costs by 1-5%, whereas raising an existing home costs over $100,000 (FEMA 2008, Dawson Foundation Repair 2021).

\*<sup>25</sup>Results are similar when I expand the set of loss events to cover 95% of total claims.

\*<sup>26</sup>See Supplementary Table 2 of Wing et al. (2020).

24

strong relationship between flood depths and damages makes elevating a home’s first floor the most important adaptive investment to reduce flood risk.

Although the NFIP policy data do not record the first floor elevations of newly mapped homes, they do contain an indicator for whether newly mapped and floodplain-built homes qualify as an “elevated buildings.” A building is considered elevated by the NFIP if its living space is entirely above ground level (e.g. no basement) and it’s raised on a secure foundation.<sup>27</sup> In the estimation sample, the average first floor elevation above ground level is 4.5 feet higher for floodplain-built homes that qualify as elevated buildings, suggesting the variable is a good proxy for first floor height.

Figure A2 plots the share of floodplain-built and newly mapped homes in the estimation sample that qualify as elevated buildings by construction year, binned into five year intervals. For all construction vintages, floodplain-built homes are consistently two to three times more likely to be elevated than newly mapped homes. In addition, whereas the share of floodplain-built homes that are elevated has been increasing over time, the share of remapped homes that are elevated has remained relatively flat.

The trends in Figure A2 suggest that floodplain-built homes are more adapted than their newly mapped counterparts, and that this gap in adaptive investment is larger for newer construction. For the welfare-relevant adaptation effect, the key question is how these differences in adaptation affect relative damages.

## 5.3.1 Adaptation Effect: Estimation Strategy

I estimate the difference in flood insurance claims between policies covering newly mapped and floodplain-built homes conditional on census tract by loss event interacted fixed effects and a variety of property and policy characteristics from the NFIP policy and claims data. My identifying assumption is that newly mapped and floodplain-built homes have similar underlying risk conditional on being in the same census tract during the same loss event, so that any differences in claims reflect differences in adaptation investment.

To address the excess mass of zeroes and skewed distribution of claims (see Figure A3), I use a hurdle model to estimate the difference in claims between newly mapped and floodplain-built policies. The first stage of the model estimates the probability of having a claim in a linear probability model, and the second stage estimates the size of the log claim conditional on having a claim.<sup>28</sup>

The first stage of the hurdle model is given by:

<sup>27</sup>See the April 2020 NFIP Flood Insurance Manual Appendix C for examples of structures that would be considered elevated versus non-elevated. Available at https://www.fema.gov/flood-insurance/work-with-nfip/manuals/archive.

<sup>28</sup>Results are similar when I estimate the first stage with a fixed effects logit model.

25

$$ I[Claim > 0]_{iqL} = \alpha_{qL}^1 + \beta_i^1 X_i + A^1 * FB_i + \epsilon_{iqL} \tag{10} $$

and the second stage by:

$$ ln(Claim)_{iqL} = \alpha_{qL}^2 + \beta_i^2 X_i + A^2 * FB_i + \mu_{iqL} \tag{11} $$

In the equations above, $i$ indexes policies, $q$ census tracts, and $L$ loss events. Standard errors are clustered by loss event. $\alpha_{qL}$ are the census tract by loss event interacted fixed effects, and $X_i$ is a vector of fixed effects for the year the home was built and policy deductible, and the amount of coverage purchased in $10,000 increments.

The primary dependent variable is the $FB$ indicator, which equals 1 for policies on floodplain-built. The adaptation effect is the relative difference in claims implied by the estimated $\widehat{A^1}$ and $\widehat{A^2}$. In order to capture the differences in adaptation between homes built in different periods, I also estimate equations (10) and (11) interacting the $FB$ indicators with decade-of-construction.

### 5.3.2 Adaptation Effect: Results

Table 4 shows the baseline results of the floodplain adaptation effect from estimating equations (10) and (11). The first column shows the conditional differences in the probability of having a claim between floodplain-built and newly mapped homes. The floodplain-built coefficient on having a claim is close to zero and precisely estimated. Column (2) shows that, conditional on having a claim, floodplain-built homes had 12% lower claims. Combined with the null claim probability result in column (1), the hurdle model suggests that floodplain-built properties had approximately 12% lower claims on average than newly mapped properties.

To explore whether the floodplain adaptation effect differs between older and newer construction, I interact the $FB$ indicators in equations (10) and (11) with decade of construction (1975-1984, 1985-1994, 1995-2004, and 2005-2014). Figure 4 plots the estimated coefficients on the floodplain-built indicator by decade of construction coefficients, with newly mapped homes as the omitted category. The top panel shows results for the probability of having a claim, and the bottom panel shows results for the log claim conditional on having a claim.

As in Table 4, the results in the top panel of Figure 4 show little difference in the probability of having a claim regardless of period of construction. The bottom panel shows that, conditional on having a claim, floodplain-built homes tend to have smaller damages. Consistent with the differences in adaptation investment from Figure A2, the difference in expected claims between newly mapped and floodplain-built homes is larger for newer construction. Whereas floodplain-built homes

26

constructed in 1975-1984 have approximately 7% lower expected claims than newly mapped homes built over the same period, floodplain-built homes constructed in 2005-2014 have 24% lower expected claims than their newly mapped counterparts.

One plausible explanation for the difference in adaptation between floodplain-built and newly mapped homes is the National Flood Insurance Program’s (NFIP) minimum elevation standards. All homes built inside the floodplain must be elevated at least to the 100-year flood elevation, beyond which homes receive premium discounts for additional elevation. The welfare implications of the adaptation effect depend on whether its driven by the the minimum elevation requirement or the floodplain’s elevation-based premiums and risk information. Due to the lack of first floor elevation data, we cannot observe how many newly mapped homes are below the minimum elevation standard. The fact that approximately 97% of floodplain-built properties in the estimation sample are elevated above the minimum elevation suggests that this requirement is not binding.

To provide further evidence on whether the minimum elevation standard is driving floodplain adaptation, I infer the first floor elevations of newly mapped policies by re-estimating equations (10) and (11) with indicator variables for floodplain-built elevations as dependent variables. Figure A4 shows the results of this estimation, where the top panel shows the results for the probability of having a claim, and the bottom panel shows the results for log claims conditional on a positive claim. The figures plot the coefficients of each half-foot floodplain-built elevation indicator with newly mapped properties as the omitted category (labelled as “remapped” in the figure). Property elevation is defined as the difference between a home’s first floor height and the 100-year flood elevation.

In both models, newly mapped properties’ average claims were similar to those of floodplain-built properties one to two feet above the 100-year flood elevation. This suggests that the minimum elevation requirement, (i.e. a property elevation of zero) would not have been binding for most newly mapped properties. Thus, we can attribute most of the adaptation effect to risk-rated premiums and information rather than the minimum elevation requirement.

One concern about the results in the Table 4 is that they may reflect biased sample selection in which properties are insured due to asymmetric information. I only observe the claims of insured properties, so differential adverse selection between newly mapped and floodplain-built properties could bias adaptation estimates. As newly mapped premiums are uniformly low under the Newly Mapped Subsidy, we would expect any adverse selection to primarily affect floodplain-built homes, which would understate the adaptation effect. Studies in flood and other insurance settings have found little evidence of adverse selection (Cutler et al., 2008; Wagner, 2021), and the mandatory flood insurance purchase requirement further reduces the possibility of adverse selection.

27

Nevertheless, to directly test for adverse selection, I conduct a positive correlation test as described in Chiappori and Salanié (2000). The positive correlation test assesses whether expected losses and opting-in to higher insurance coverage are independent conditional on the information used by the insurer to set premiums. In the presence of adverse selection, there would be a positive correlation between these two outcomes. The test proceeds by estimating two models. One focuses on choosing more insurance coverage, while the other examines having a loss event including controls for all variables used by the insurer (and only those variables). The residuals of these two models form a test statistic for the null hypothesis of no asymmetric information.

Although I cannot compare the losses of homes with and without insurance, I can compare the losses of floodplain-built homes that do and do not choose to supplement their standard flood insurance policy with contents coverage. The dependent variable in the first model is choosing non-zero contents coverage; in the second it is having a claim on their primary structure coverage. Whereas the null hypothesis in a traditional positive correlation test can be rejected due to moral hazard as well as adverse selection, a benefit of my specification is that contents coverage is unlikely to induce moral hazard on structural damage claims, making it a plausibly pure test of adverse selection. The controls are property elevation by flood zone type indicators and the number of stories in the insured home.

In the data, 75% of floodplain-built homes in the sample opt-in to contents coverage. The share of homes with structure claims is actually slightly *lower* for homes with contents coverage, at 19%, compared to 19.4% of homes without contents coverage. The formal positive correlation test bears out the absence of adverse selection in this raw data.<sup>29</sup>

# 6 Welfare Results

This section embeds the sufficient statistics estimated in section 5 to equation 6 to calculate the welfare effects of better flood risk information. First, I describe the calibration of each welfare parameter. Then, I describe the welfare effects of using better risk information to reduce the misclassification of high-risk homes.

## 6.1 Fitting Empirical Estimates to Model Parameters

Below, I describe the empirical counterparts for the welfare parameters in equation (6), repeated below for the reader’s convenience. Table 5 provides a summary of these calibrated parameters and sources, which I describe in further detail below.

<sup>29</sup>The chi-squared test statistic p-value is 0.867.

28

$$ \frac{TW}{d\rho} = \sum_{t=1}^{T} \beta^{t-1} (\overbrace{\lambda I_{t}^{0} * (D_{t}^{0} - \overline{P_{t}})}^{\text{Public Funds}} + \frac{1}{2} * \underbrace{(I_{t}^{1} - I_{t}^{0})(W_{t}^{1}(I_{t}^{0}, A^{1}) - D_{t}^{1})}_{\text{Take-Up}} $$
$$ + \overbrace{\frac{(1 - A^{1})}{(1 - A^{0})} ((1 - I_{t}^{0})(W_{t}^{1}(I_{t}^{0}, A^{0}) - E_{t=0}[W_{t}^{0}(I_{t}^{0}, A^{0})])}^{\text{Adaptation}} + \underbrace{(1 + \lambda)I_{t}^{0}D_{t}^{0})}_{\text{Moral Hazard}}) $$

**Price Effect and Flood Insurance Demand**

In the survey data, the flood insurance take-up rate for non-floodplain homeowners who should be in the floodplain according to my elevation difference measure is 36% ($I^{0}$). The results in Table 2 show that under the counterfactual where these households were correctly classified inside the floodplain, voluntary flood insurance demand would be 50%. Including mandatory purchasers, counterfactual take-up is 58% ($I^{1}$).

The calibration of flood insurance demand from the estimated price elasticity (Table 1) adjusted for the share of mandatory purchasers in the survey (section C) is described in section 5.2.3. The demand curve implies that the marginal willingness-to-pay of high-risk households at 36% flood insurance take-up would be $3,315, corresponding to the $W^{1}(I^{0}, A^{1})$ parameter.<sup>30</sup>

In the terminology of Bernheim and Rangel (2009), $W^{0}$ is the “decision utility” of high-risk households misclassified as low-risk, whereas $W^{1}$ is their “true utility” under their correct risk classification. As such, I assume that take-up beyond the voluntary flood insurance demand of correctly classified high-risk households is inefficient. In equation 6, the mandate-induced take-up from 0.5 to 0.58 appears as a deadweight loss term where $W^{1}(0.58, A^{1}) < D^{1}$, attenuating the information effect welfare gains.<sup>31</sup>

It is possible that some mandatory purchase of flood insurance for high-risk households could be socially efficient, whether due to externalities imposed by uninsured flood losses or if $W^{1}$ still understates the private welfare value of flood insurance (Liao and Mulder, 2021; Wagner, 2021). Equation (6) can be easily extended to incorporate estimates of additional internalities and externalities.

\*<sup>30</sup>Rather than using the trapezoidal approximation for the take-up effect in equation (6), I calculate the exact area implied by the isoelastic functional form for $W^{1}$.

\*<sup>31</sup>Ultimately, the offsetting deadweight loss imposed by the mandatory purchase requirement is only about $10 per year.

29

# Adaptation Effect

The results in Table 4 imply that floodplain-built homes suffer 12% lower flood damages than their newly mapped counterparts ($\frac{1-A^1}{1-A^0} = 0.88$). However, the results in Figure 4 shows that these differences in adaptation are larger for newer construction. For new homes, I parameterize the adaptation effect as $\frac{1-A^1}{1-A^0} = 0.76$.

Given the high cost of elevating existing construction, I assume that reclassified existing homes cannot change their adaptation. Any new construction in a high-risk area will have 24% lower costs if correctly classified relative to if it is misclassified as low-risk. Applying these relative levels of adaptation to the average premiums data described in section 4.1, this implies that $D_0^0 = \frac{1330}{0.88} \approx \$1,510$ and $D_0^1 = 1510 * 0.76 \approx \$1,150$.

# Dynamic Risk and Perceptions

I calibrate expected risk dynamics using the First Street Foundation Flood Model (FSF-FM) described in Section 4.2. The details of FSF-FM climate projections are described in First Street Foundation (2020). I look at the FSF-FM's projections for the 1 million high-risk single family homes in Florida, defined as a home built on ground with at least a 1% annual chance of flooding.

To determine the annual risk growth rate, I compare high-risk properties' average annual flood losses (AAL) according to the model in 2020 and 2050. Over this period, the high-risk AALs increase 79% from $3,170 to $5,660. Assuming linearly increasing risk, this is an annual growth rate of 2.6% of the 2020 AAL ($\gamma = .026$).

Figure A5 plots 2020 and 2050 AALs for high-risk and low-risk properties. In contrast to the sharp increase in expected losses for already high-risk properties, low-risk homes see a muted increase from $5 in 2020 to $90.<sup>32</sup> The relatively flat AAL trajectory of low-risk homes justifies assumption 1, which holds that low-risk homes' expected losses will remain constant over time. The stark contrast in Figure A5 also forms the basis for the hypothesis that misclassification may cause households to underestimate the dynamic increase in their flood risk.

They dynamic risk perception results in Table 3 imply that misclassified high-risk households underestimate their increase in risk by $\frac{0.43}{0.43+0.12} = 22\%$. This implies $\gamma^0 = 0.78 * \gamma = .02$.

As discussed in section 3.2, increasing risk erodes adaptation over time. The demand curve $W^1$ predicts insurance take-up for a given proportional increase in risk and change in premiums for period $t$ relative to period one. The first-order condition in equation (3) implies households will increase their insurance take-up in response to their perceived increasing risk such that $P_t^f = W_t^f(I_t^f, A^f)$ in

<sup>32</sup>For low-risk homes built on ground with at least a 0.2% annual chance of flooding, AALs increase from $10 in 2020 to $125 in 2050.

30

all periods.<sup>33</sup>

## 6.2 Welfare Counterfactual Results

I use equation (6) to calculate the welfare effects under a counterfactual where the National Flood Insurance Program (NFIP) used better risk modeling technology to misclassify fewer high-risk homes as outside the floodplain. The welfare effects of this improved risk modeling technology are quantified in terms of the present value welfare gain over 30 years (2020-2049) per high-risk home that would otherwise be misclassified.

I consider three baseline misclassification scenarios: A new high-risk home misclassified for 30 years, a new high-risk home misclassified for 10 years, and an existing misclassified home that would continue to be misclassified for 30 years. Figure 5 plots the annual welfare gains of correctly classifying a high-risk home relative to these three baseline scenarios in panels (a)-(c). The total welfare effect is divided into four components corresponding to the terms in equation (6), as indicated by the legend in panel (d).

Panel (a) of Figure 5 shows the annual welfare gains of correct risk classification relative to the first baseline scenario: A new home that would otherwise be misclassified for the full 30 year window. Because the high-risk home is new, the homeowner can adjust its adaptation in response to their risk classification. In the first year, correct classification increases social welfare by nearly $1,000. Over 70% of this welfare gain comes from the adaptation bias term, which accounts for a $700 welfare gain, with the moral hazard, cost of public funds, and take-up bias terms each contributing approximately $100 to social welfare.

By 2049, flood risk will have increased 80%, but the annual welfare gains from correct risk-rating will increase even more quickly, tripling relative to 2020. Growth in the moral hazard and cost of public funds terms are driven by increasing flood insurance take-up and claims under the baseline scenario. The adaptation bias and take-up bias terms grow as the misclassified household’s dynamic risk misperceptions cause their flood insurance take-up to decline relative to its privately efficient level. The 30-year present value welfare gains in panel (a) accumulate to approximately $48,840.

Panel (a) emphasizes the importance of accounting for both dynamic risk and information effects when considering the welfare effects of new information. Treating improved risk-rating as a simple pricing reform - assuming that flood insurance demand was unbiased prior to the new risk information or that any bias was fixed - would ignore the substantial adaptation bias term and underestimate the take-up bias effects by assuming that take-up would decline slightly rather than

\*<sup>33</sup>I assume that buyers constrained by the mandatory purchase requirement proportionally become voluntary purchasers as insurance take-up increases over time.

31

increase substantially.

Ignoring risk dynamics would also understate the welfare benefits of correct risk classification. Simply extrapolating the 2020 effects through 2049 would underestimate the 30-year present value welfare gains by nearly 50%. Alternatively, assuming that the difference in counterfactual welfare grows linearly with risk would underestimate the 30-year present value welfare gains by nearly 30%.

Panel (b) calculates the welfare counterfactual of correctly mapping a new high-risk home inside the floodplain relative to a baseline where the home would be misclassified outside the floodplain and then moved inside the floodplain after 10 years. As in panel (a), correct classification generates substantial welfare gains in the first 10 years primarily driven by the adaptation bias term. After the home would be reclassified inside the floodplain under the baseline scenario, the annual welfare gains of correct classification drop from $1,540 in 2029 to only $260 in 2030. This decline is due to the homeowner correctly perceiving their risk after being remapped in 2030 even though they were initially misclassified in 2020 at construction. As a result, the take-up bias term disappears in 2030, as does the cost of public funds term because the high-risk home starts to face risk-rated premiums.

Nonetheless, there are still welfare gains in 2030 and beyond if the home were correctly placed inside the floodplain at construction. Because the home is already built, it is too late to re-adapt it in 2030 after it is moved into the floodplain, whereas it would have been efficiently adapted were it inside the floodplain at construction. Inefficient adaptation drives the moral hazard term, which grows to over $300 by 2049. The adaptation bias term nearly disappears because the homeowner increasing their insurance take-up after reclassification to compensate for their suboptimal adaptation.

Summarizing the results from panel (b), even if the new home were remapped 10 years later, the 30-year present value welfare gain from correctly placing it in the floodplain at construction would be approximately $16,905.

Panel (c) plots the welfare gains from remapping an existing non-floodplain high-risk home into the floodplain that would otherwise remain misclassified for 30 years. Under this baseline scenario, the home cannot be re-adapted after being remapped. As a result, there are no adaptation bias or moral hazard welfare gains from reclassification. Nonetheless, the present value 30-year welfare gains from correcting an existing home's misclassification total approximately $22,945. In addition to eliminating the cost of public funds, the welfare gains in panel (c) are driven mostly by the take-up bias term. The take-up bias is even larger relative to panel (a) precisely because the homeowner cannot re-adapt after being remapped, causing them to respond more along the take-up margin to compensate for their lower adaptation.

How economically significant are these effects? The present value welfare gains of nearly $50,000 in panel (a) is over 15% of the median home value. For another measure of economic significance,

32

the First Street Foundation estimates that approximately 6 million high-risk homes are currently misclassified outside the floodplain. The results in panel (c) imply that remapping these homes inside the floodplain would increase present value social welfare by nearly $138 billion.

What do these counterfactuals imply for the distributional consequences of new risk information and better risk rating? Under the baseline scenario in panel (c), remapping eliminates a misclassified homeowner’s present value premium subsidy of approximately $18,540. By providing them with better risk information, however, remapping also eliminates their inefficiently low flood insurance take-up. The present value of this “internality” is $17,770, approximately canceling out the remapped household’s subsidy losses.

Once we account for the likely effects of remapping on home values, however, the reclassified household is substantially worse off under the counterfactual. Remapping increases a home’s perceived present value cost of flood risk from $11,150 to over $45,000 dollars due to higher spending on flood insurance and expected uninsured flood damages. If these higher flood costs are capitalized into the home’s value, the homeowner will be worse off despite their better risk information. Nonetheless, because of the large fiscal externalities imposed by flood risk misclassification, social welfare as a whole is higher with better risk information.

# 7 Conclusion

This paper finds that better classification of high-risk households in the National Flood Insurance Program (NFIP) can create large welfare gains by informing households about their risk and increasing investment in adaptation to reduce risk. Risk rating technology can facilitate adaptation to climate change, as sophisticated disaster risk models provide valuable information to households about their current and future risk. This information can change insurance demand and adaptation by households exposed to increasing risk.

Even as better risk modeling technology leads to higher premiums on high-risk homes, it creates a countervailing information effect as homeowners increase their risk perceptions. Households moved inside the floodplain increased their flood insurance take-up even as their premiums rose. My survey evidence shows that high-risk households misclassified outside the floodplain underestimated their flood risk.

Better information and prices can also inform long-run adaptation decisions. Homes built inside the floodplain had lower flood insurance claims than nearby homes built outside the floodplain and later remapped inside. Providing price signals to inform adaptation decisions and incentivize efficient investment in risk reduction creates long-run welfare gains.

33

Disaster insurance markets struggle with low take-up rates and rising premiums. Given these challenges, policymakers fear that increasingly granular risk models will make high-risk communities less resilient to increasing risk. Yet, prices are not only incentives, but also signals. As new information drives higher premiums, it also drives changing risk perceptions and investment in adaptation.

<page_number>34</page_number>

# References

Addoum, Jawad M, Piet Eichholtz, Eva Steiner, and Erkan Yönder, “Climate Change and Commercial Real Estate: Evidence from Hurricane Sandy,” 2021.

Agency, Federal Emergency Management, “2008 Supplement to 2006 Evaluation of the National Flood Insurance Program’s Building Standards,” https://www.fema.gov/sites/default/files/2020-07/fema_nfip_2008_freeboard_report_0.pdf 2008.

\_ , “Risk Rating 2.0: Equity in Action,” https://www.fema.gov/flood-insurance/risk-rating 2021.

Armal, Saman, Jeremy R Porter, Brett Lingle, Ziyan Chu, Michael L Marston, and Oliver EJ Wing, “Assessing Property Level Economic Impacts of Climate in the US, New Insights and Evidence from a Comprehensive Flood Risk Assessment Tool,” *Climate*, 2020, 8 (10), 116.

Baker, Andrew, David F Larcker, and Charles CY Wang, “How Much Should We Trust Staggered Difference-In-Differences Estimates?,” *Available at SSRN 3794018*, 2021.

Bakkensen, Laura A and Lala Ma, “Sorting over flood risk and implications for policy reform,” *Journal of Environmental Economics and Management*, 2020, 104, 102362.

\_ and Lint Barrage, “Flood risk belief heterogeneity and coastal home price dynamics: Going under water?,” Technical Report, National Bureau of Economic Research 2017.

Baylis, Patrick and Judson Boomhower, “Moral hazard, wildfires, and the economic incidence of natural disasters,” Technical Report, National Bureau of Economic Research 2019.

\_ and \_ , “Building codes and community resilience to natural disasters,” 2021.

Behrman, Jere R, Olivia S Mitchell, Cindy K Soo, and David Bravo, “How financial literacy affects household wealth accumulation,” *American Economic Review*, 2012, 102 (3), 300–304.

Bernheim, B Douglas and Antonio Rangel, “Beyond revealed preference: choice-theoretic foundations for behavioral welfare economics,” *The Quarterly Journal of Economics*, 2009, 124 (1), 51–104.

Bernstein, Asaf, Matthew T Gustafson, and Ryan Lewis, “Disaster on the horizon: The price effect of sea level rise,” *Journal of financial economics*, 2019, 134 (2), 253–272.

35

Blattner, Laura and Scott Nelson, “How Costly is Noise? Data and Disparities in Consumer Credit,” *arXiv preprint arXiv:2105.07554*, 2021.

Boustan, Leah Platt, Matthew E Kahn, and Paul W Rhode, “Moving to higher ground: Migration response to natural disasters in the early twentieth century,” *American Economic Review*, 2012, *102* (3), 238–44.

Bradt, Jacob T, Carolyn Kousky, and Oliver EJ Wing, “Voluntary purchases and adverse selection in the market for flood insurance,” *Journal of Environmental Economics and Management*, 2021, *110*, 102515.

Brown, Jeffrey R, Arie Kapteyn, Erzo FP Luttmer, Olivia S Mitchell, and Anya Samek, “Behavioral impediments to valuing annuities: complexity and choice bracketing,” *Review of Economics and Statistics*, 2021, *103* (3), 533–546.

Chetty, Raj, “Sufficient statistics for welfare analysis: A bridge between structural and reduced-form methods,” *Annu. Rev. Econ.*, 2009, *1* (1), 451–488.

\_ , Adam Looney, and Kory Kroft, “Salience and taxation: Theory and evidence,” *American economic review*, 2009, *99* (4), 1145–77.

Chiappori, Pierre-André and Bernard Salanié, “Testing for asymmetric information in insurance markets,” *Journal of political Economy*, 2000, *108* (1), 56–78.

Cohen, Alma, “Asymmetric information and learning: Evidence from the automobile insurance market,” *Review of Economics and statistics*, 2005, *87* (2), 197–207.

Cutler, David M, Amy Finkelstein, and Kathleen McGarry, “Preference heterogeneity and insurance markets: Explaining a puzzle of insurance,” *American Economic Review*, 2008, *98* (2), 157–62.

Dave, Dhaval and Robert Kaestner, “Health insurance and ex ante moral hazard: evidence from Medicare,” *International journal of health care finance and economics*, 2009, *9* (4), 367.

Dimmock, Stephen G, Roy Kouwenberg, Olivia S Mitchell, and Kim Peijnenburg, “Household portfolio underdiversification and probability weighting: Evidence from the field,” *The Review of Financial Studies*, 2021, *34* (9), 4524–4563.

Ehrlich, Isaac and Gary S Becker, “Market insurance, self-insurance, and self-protection,” *Journal of political Economy*, 1972, *80* (4), 623–648.

36

Einav, Liran, Amy Finkelstein, and Mark R Cullen, “Estimating welfare in insurance markets using variation in prices,” *The quarterly journal of economics*, 2010, 125 (3), 877–921.

Finkelstein, Amy and James Poterba, “Adverse selection in insurance markets: Policyholder evidence from the UK annuity market,” *Journal of Political Economy*, 2004, 112 (1), 183–208.

\_ and Kathleen McGarry, “Multiple dimensions of private information: evidence from the long-term care insurance market,” *American Economic Review*, 2006, 96 (4), 938–958.

Flavelle, Christopher, “The Cost of Insuring Expensive Waterfront Homes is About to Skyrocket,” *The New York Times*, Sep 2021.

Foundation, First Street, “First Street Foundation Flood Model Technical Methodology Document,” https://assets.firststreet.org/uploads/2020/06/FSF_Flood_Model_Technical_Documentation.pdf 2020.

\_ , “Flood Factor,” https://floodfactor.com 2021.

Fuster, Andreas, Matthew Plosser, Philipp Schnabl, and James Vickery, “The role of technology in mortgage lending,” *The Review of Financial Studies*, 2019, 32 (5), 1854–1899.

Gallagher, Justin, “Learning about an infrequent event: evidence from flood insurance take-up in the United States,” *American Economic Journal: Applied Economics*, 2014, pp. 206–233.

Gibson, Matthew and Jamie T Mullins, “Climate risk and beliefs in new york floodplains,” *Journal of the Association of Environmental and Resource Economists*, 2020, 7 (6), 1069–1111.

Giglio, Stefano, Bryan Kelly, and Johannes Stroebel, “Climate finance,” 2021.

\_ , Matteo Maggiori, Krishna Rao, Johannes Stroebel, and Andreas Weber, “Climate change and long-run discount rates: Evidence from real estate,” *The Review of Financial Studies*, 2021, 34 (8), 3527–3571.

Gillen, Ben, Erik Snowberg, and Leeat Yariv, “Experimenting with measurement error: Techniques with applications to the caltech cohort study,” *Journal of Political Economy*, 2019, 127 (4), 1826–1863.

Goldsmith-Pinkham, Paul, Matthew Gustafson, Ryan Lewis, and Michael Schwert, “Sea level rise and municipal bond yields,” 2019.

Goodman-Bacon, Andrew, “Difference-in-differences with variation in treatment timing,” *Journal of Econometrics*, 2021.

37

Handel, Benjamin R, Jonathan T Kolstad, and Johannes Spinnewijn, “Information frictions and adverse selection: Policy interventions in health insurance markets,” *Review of Economics and Statistics*, 2019, 101 (2), 326–340.

Hino, Miyuki and Marshall Burke, “The effect of information about climate risk on property values,” *Proceedings of the National Academy of Sciences*, 2021, 118 (17).

Hogarth, Robin M and Howard Kunreuther, “Risk, ambiguity, and insurance,” *Journal of risk and uncertainty*, 1989, 2 (1), 5–35.

Hu, Zhongchen, “Salience and Households’ Flood Insurance Decisions,” *Available at SSRN 3759016*, 2021.

Issler, Paulo, Richard Stanton, Carles Vergara-Alert, and Nancy Wallace, “Mortgage markets with climate-change risk: Evidence from wildfires in california,” *Available at SSRN 3511843*, 2020.

Jin, Yizhou and Shoshana Vasserman, “Buying data from consumers: The impact of monitoring programs in us auto insurance,” Technical Report, National Bureau of Economic Research 2021.

Kahn, Matthew E, “The death toll from natural disasters: the role of income, geography, and institutions,” *Review of economics and statistics*, 2005, 87 (2), 271–284.

Keenan, Jesse M and Jacob T Bradt, “Underwaterwriting: from theory to empiricism in regional mortgage markets in the US.,” *Climatic Change*, 2020, 162 (4), 2043–2067.

Keys, Benjamin J and Philip Mulder, “Neglected no more: housing markets, mortgage lending, and sea level rise,” Technical Report, National Bureau of Economic Research 2020.

Kleven, Henrik J, “Sufficient statistics revisited,” *Annual Review of Economics*, 2021, 13.

Kousky, Carolyn, “Disasters as learning experiences or disasters as policy opportunities? Examining flood insurance purchases after hurricanes,” *Risk analysis*, 2017, 37 (3), 517–530.

\_ , Erwann O Michel-Kerjan, and Paul A Raschky, “Does federal disaster assistance crowd out flood insurance?,” *Journal of Environmental Economics and Management*, 2018, 87, 150–164.

\_ , Howard Kunreuther, Brett Lingle, and Leonard Shabman, “The emerging private residential flood insurance market in the United States,” *Wharton Risk Management and Decision Processes Center*, 2018.

38

Kunreuther, Howard and Mark Pauly, “Neglecting disaster: Why don’t people insure against large losses?,” *Journal of Risk and Uncertainty*, 2004, 28 (1), 5–21.

Lee, Seunghoon, “Adapting to Natural Disasters through Better Information: Evidence from the Home Seller Disclosure Requirement,” Technical Report, Mimeo 2021.

Liao, Yanjun and Philip Mulder, “What’s at Stake? Understanding the Role of Home Equity in Flood Insurance Demand,” *Understanding the Role of Home Equity in Flood Insurance Demand* (January 13, 2021), 2021.

Lockwood, Benjamin B, “Optimal income taxation with present bias,” *American Economic Journal: Economic Policy*, 2020, 12 (4), 298–327.

McCarthy, David and Olivia S Mitchell, “International adverse selection in life insurance and annuities,” in “Ageing in advanced industrial states,” Springer, 2010, pp. 119–135.

McCoy, Shawn J and Randall P Walsh, “Wildfire risk, salience & housing demand,” *Journal of Environmental Economics and Management*, 2018, 91, 203–228.

Meyer, Robert and Howard Kunreuther, *The ostrich paradox: Why we underprepare for disasters*, Wharton School Press, 2017.

Mulder, Philip, “Dynamic adverse selection in flood insurance,” *Available at SSRN 3435324*, 2019.

Murfin, Justin and Matthew Spiegel, “Is the risk of sea level rise capitalized in residential real estate?,” *The Review of Financial Studies*, 2020, 33 (3), 1217–1255.

Netusil, Noelwah R, Carolyn Kousky, Shulav Neupane, Will Daniel, and Howard Kunreuther, “The willingness to pay for flood insurance,” *Land Economics*, 2021, 97 (1), 17–38.

Oceanic, National and Atmospheric Administration, “Sea Level Rise Viewer,” https://coast.noaa.gov/digitalcoast/tools/slr.html 2021.

Office, Government Accountability, “Flood Map Modernization Program Strategy Shows Promise, but Challenges Remain,” 2004.

\_\_\_ , “Congress Should Consider Updating the Mandatory Purchase Requirement,” 2021.

Ortega, Francesc and Süleyman Taşpınar, “Rising sea levels and sinking property values: Hurricane Sandy and New York’s housing market,” *Journal of Urban Economics*, 2018, 106, 81–100.

39

Ouazad, Amine and Matthew E Kahn, “Mortgage Finance and Climate Change: Securitization Dynamics in the Aftermath of Natural Disasters,” Technical Report, National Bureau of Economic Research 2019.

Petrolia, Daniel R, Craig E Landry, and Keith H Coble, “Risk preferences, risk perceptions, and flood insurance,” *Land Economics*, 2013, 89 (2), 227–245.

Price, Josh, “What Does an Elevation Certificate Cost?,” https://www.massivecert.com/blog/what-does-elevation-certificate-cost 2019.

Repair, Dawson Foundation, “Cost of Elevating a House Above the Floodplain,” https://www.dawsonfoundationrepair.com/cost-elevating-house/ 2021.

Taubinsky, Dmitry and Alex Rees-Jones, “Attention variation and welfare: theory and evidence from a tax salience experiment,” *The Review of Economic Studies*, 2018, 85 (4), 2462–2496.

Wagner, Katherine, “Adaptation and Adverse Selection in Markets for Natural Disaster Insurance,” *American Economic Journal: Economic Policy*, 2021.

Wing, Oliver EJ, Nicholas Pinter, Paul D Bates, and Carolyn Kousky, “New insights into US flood vulnerability revealed from flood insurance big data,” *Nature communications*, 2020, 11 (1), 1–10.

Zweifel, Peter and Willard G Manning, “Moral hazard and consumer incentives in health care,” in “Handbook of health economics,” Vol. 1, Elsevier, 2000, pp. 409–459.

40

# Tables


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="4">Probability of Renewal</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Premium Increase (%)</td>
        <td>-0.148***</td>
        <td>-0.172***</td>
        <td>-0.172***</td>
        <td>-0.177***</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.046)</td>
        <td>(0.038)</td>
        <td>(0.038)</td>
        <td>(0.040)</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>78,174</td>
        <td>78,174</td>
        <td>78,174</td>
        <td>78,174</td>
    </tr>
    <tr>
        <td>R-squared</td>
        <td>0.013</td>
        <td>0.024</td>
        <td>0.024</td>
        <td>0.025</td>
    </tr>
    <tr>
        <td>State FEs</td>
        <td>N</td>
        <td>Y</td>
        <td>Y</td>
        <td>Y</td>
    </tr>
    <tr>
        <td>Past Claims Control</td>
        <td>N</td>
        <td>N</td>
        <td>Y</td>
        <td>Y</td>
    </tr>
    <tr>
        <td>Month Written FEs</td>
        <td>N</td>
        <td>N</td>
        <td>N</td>
        <td>Y</td>
    </tr>
  </tbody>
</table>

\*\*\* p<0.01, \*\* p<0.05, \* p<0.1

Table 1: Results from estimating the renewal probability of flood insurance policies newly mapped into the floodplain. The independent variable is an indicator variable for whether a policy renews. The dependent variable is the percent increase in the policy’s premiums from the previous year under the Newly Mapped Subsidy schedule. All models include first policy year by renewal year interacted fixed effects. Additional controls, described in the bottom section of the table, are progressively added across columns from left to right. Standard errors are clustered by county.

41

<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="3">Outcome: Voluntarily Insured</th>
    </tr>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Inside Floodplain</td>
        <td>0.231***</td>
        <td>0.153***</td>
        <td>0.128**</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.042)</td>
        <td>(0.046)</td>
        <td>(0.056)</td>
    </tr>
    <tr>
        <td>Had Past Flood</td>
        <td> </td>
        <td>0.169**</td>
        <td>0.166**</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.068)</td>
        <td>(0.071)</td>
    </tr>
    <tr>
        <td>Flood Depth</td>
        <td> </td>
        <td>0.024*</td>
        <td>0.041**</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.013)</td>
        <td>(0.030)</td>
    </tr>
    <tr>
        <td>I[Flood Depth = 0]</td>
        <td> </td>
        <td>-0.055</td>
        <td>-0.038</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.045)</td>
        <td>(0.052)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>702</td>
        <td>702</td>
        <td>702</td>
    </tr>
    <tr>
        <td>R-squared</td>
        <td>0.128</td>
        <td>0.149</td>
        <td>0.141</td>
    </tr>
    <tr>
        <td>Demographics</td>
        <td>Y</td>
        <td>Y</td>
        <td>Y</td>
    </tr>
    <tr>
        <td>ORIV</td>
        <td>N</td>
        <td>N</td>
        <td>Y</td>
    </tr>
  </tbody>
</table>


\*\*\* p<0.01, \*\* p<0.05, \* p<0.1

Standard errors clustered by county and bootstrapped for ORIV specification

Table 2: Results from estimating voluntary flood insurance demand from the survey of coastal homeowners. The dependent variable is an indicator for reporting having voluntarily purchased flood insurance. The primary independent variable of interest is an indicator for being inside the floodplain. Column (2) adds the average model 100-year flood depth, and column (3) instruments for the 100-year flood depth using the Obviously Related Instrumental Variables (ORIV) method. All models include controls for respondent demographics. Standard errors are clustered by county and bootstrapped in column (3).

42

<table>
  <thead>
    <tr>
        <th> </th>
        <th>Risk Increasing</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Floodplain</td>
        <td>0.121**</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.056)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>702</td>
    </tr>
    <tr>
        <td>R-squared</td>
        <td>0.098</td>
    </tr>
    <tr>
        <td>Demographics</td>
        <td>Y</td>
    </tr>
    <tr>
        <td>Future Risk Controls</td>
        <td>Y</td>
    </tr>
  </tbody>
</table>

\*\*\* p<0.01, \*\* p<0.05, \* p<0.1

Table 3: Results from estimating future flood risk beliefs from the survey of coastal homeowners. The dependent variable is a binary indicator for whether the respondent believes their flood risk will be higher in 30 years as the dependent variable. The primary independent variable of interest is an indicator for being inside the floodplain. The model includes controls for respondent demographics and a quadratic term for the change in average annualized losses between 2020 and 2050. Standard errors are clustered by county.

43

<table>
  <thead>
    <tr>
        <th>VARIABLES</th>
        <th>(1)<br/>Any Claim</th>
        <th>(2)<br/>Log Claims (Claim &gt; 0)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Floodplain-Built</td>
        <td>-0.001</td>
        <td>-0.133***</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.044)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>102,527</td>
        <td>18,978</td>
    </tr>
    <tr>
        <td>R-squared</td>
        <td>0.457</td>
        <td>0.480</td>
    </tr>
  </tbody>
</table>

\*\*\* p<0.01, \*\* p<0.05, \* p<0.1

Table 4: Results of the hurdle model estimating flood insurance claims. The independent variable is an indicator for being a floodplain-built as opposed to newly mapped home. The dependent variable in column (1) is the probability of having a positive claim and in column (2) is the log claim conditional on having a claim. All models include census tract by loss event interacted fixed effects and controls for property year built, deductible, and amount of coverage purchased. Standard errors are clustered by loss event.

44

<table>
  <thead>
    <tr>
        <th>Name</th>
        <th>Model Notation</th>
        <th>Estimate</th>
        <th>Source</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Non-floodplain Take-Up Rate</td>
        <td>$I^0$</td>
        <td>0.36</td>
        <td>Survey Data</td>
    </tr>
    <tr>
        <td>Floodplain Take-Up Rate</td>
        <td>$I^1$</td>
        <td>0.58</td>
        <td>Survey Estimates</td>
    </tr>
    <tr>
        <td>Adaptation Effect (New Construction)</td>
        <td>$\frac{1-A^1}{1-A^0}$</td>
        <td>-0.24</td>
        <td>Remapped Claims Estimates</td>
    </tr>
    <tr>
        <td>Floodplain Premium</td>
        <td>$P^1$</td>
        <td>$1,330</td>
        <td>Floodplain Policy Data</td>
    </tr>
    <tr>
        <td>Non-floodplain Premium</td>
        <td>$P^0$</td>
        <td>$530</td>
        <td>Non-floodplain Policy Data</td>
    </tr>
    <tr>
        <td>Flood Risk Growth Rate</td>
        <td>$1 + \gamma^1$</td>
        <td>1.026</td>
        <td>FSF-FM</td>
    </tr>
    <tr>
        <td>Misperceived Flood Risk Growth Rate</td>
        <td>$1 + \gamma^0$</td>
        <td>1.02</td>
        <td>Survey Risk Change Beliefs</td>
    </tr>
    <tr>
        <td>Cost of Public Funds</td>
        <td>$\lambda$</td>
        <td>0.3</td>
        <td>Poterba (1996)</td>
    </tr>
    <tr>
        <td>Discount Rate</td>
        <td>$\beta$</td>
        <td>0.99</td>
        <td>Giglio et al. (2021)</td>
    </tr>
  </tbody>
</table>


Table 5: Empirical parameters for welfare estimation

45

# Figures

![Map of the United States showing counties with zip codes where homes were remapped into the floodplain between 2015-2019, shaded by remapping year.](page_46_image_2_v2.jpg)


<table>
  <tbody>
    <tr>
        <td>Remapping Year</td>
        <td>Percent of Newly Mapped Policies</td>
    </tr>
    <tr>
        <td>2015</td>
        <td>7</td>
    </tr>
    <tr>
        <td>2016</td>
        <td>39</td>
    </tr>
    <tr>
        <td>2017</td>
        <td>26</td>
    </tr>
    <tr>
        <td>2018</td>
        <td>21</td>
    </tr>
    <tr>
        <td>2019</td>
        <td>7</td>
    </tr>
  </tbody>
</table>


Figure 1: The top panel maps counties with zip codes where homes were remapped into the floodplain between 2015-2019. Shading indicates year of remapping. Clear counties did not have floodplain newly mapped policies. The bottom panel shows the distribution of remapping years across the sample of 2020 remapped policies. Source: OpenFEMA policy data

46

![Map of the United States showing the number of respondents by county, with a legend indicating ranges from 1-4 to 31-42. The respondents are concentrated along the Gulf and Atlantic coasts.](page_47_image_1_v2.jpg)

Figure 2: Number of respondents by county in estimation sample of the flood insurance survey.

47

<table>
  <thead>
    <tr>
        <th>Years since Remapping</th>
        <th>Newly Mapped Coefficients Log Flood Insurance Policies</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-3</td>
        <td>0.08</td>
    </tr>
    <tr>
        <td>-2</td>
        <td>0.06</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>0</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.49</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.67</td>
    </tr>
    <tr>
        <td>2</td>
        <td>0.68</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.63</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.59</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.47</td>
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
        <th>Years since Remapping</th>
        <th>Newly Mapped Coefficients Log Flood Insurance Premiums</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>-3</td>
        <td>-0.02</td>
    </tr>
    <tr>
        <td>-2</td>
        <td>-0.01</td>
    </tr>
    <tr>
        <td>-1</td>
        <td>0</td>
    </tr>
    <tr>
        <td>0</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>2</td>
        <td>0.13</td>
    </tr>
    <tr>
        <td>3</td>
        <td>0.21</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.28</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.36</td>
    </tr>
  </tbody>
</table>


Figure 3: Coefficients estimating the effect of remapping on the inverse hyperbolic sine transformed number of policies (top panel) and log average premiums (bottom panel) by years since remapping for homes moved inside the floodplain relative to homes that remained outside the floodplain. Lines indicate 95% confidence intervals with standard errors clustered by county.

48

<table>
  <tbody>
    <tr>
        <td>Decade of Construction</td>
        <td>Floodplain Built vs Newly Mapped Probability of Claim Coefficient</td>
    </tr>
    <tr>
        <td>1975-1984</td>
        <td>-0.013</td>
    </tr>
    <tr>
        <td>1985-1994</td>
        <td>-0.007</td>
    </tr>
    <tr>
        <td>1995-2004</td>
        <td>0.006</td>
    </tr>
    <tr>
        <td>2005-2014</td>
        <td>0.007</td>
    </tr>
  </tbody>
</table>
<table>
  <tbody>
    <tr>
        <td>Decade of Construction</td>
        <td>Floodplain Built vs Newly Mapped Log Claim Coefficient</td>
    </tr>
    <tr>
        <td>1975-1984</td>
        <td>-0.08</td>
    </tr>
    <tr>
        <td>1985-1994</td>
        <td>-0.07</td>
    </tr>
    <tr>
        <td>1995-2004</td>
        <td>-0.16</td>
    </tr>
    <tr>
        <td>2005-2014</td>
        <td>-0.27</td>
    </tr>
  </tbody>
</table>


Figure 4: Results of the hurdle model estimating flood insurance claims allowing for heterogeneous effects by decade of construction. The figure plots the estimates and 95% confidence intervals for the coefficients on floodplain-built homes interacted with decade of construction, with newly mapped homes as the omitted category. In the top panel, the outcome variable is having a claim. In the bottom panel, the outcome variable is the log claim conditional on having a claim. Both models include census tract by loss event interacted fixed effects and controls for property year built, deductible, and amount of coverage purchased. Standard errors are clustered by loss event.

49

(a)
<table>
  <tbody>
    <tr>
        <td>Year</td>
        <td>Public Funds</td>
        <td>Take-Up Bias</td>
        <td>Adaptation Bias</td>
        <td>Moral Hazard</td>
    </tr>
    <tr>
        <td>2020</td>
        <td>150</td>
        <td>100</td>
        <td>650</td>
        <td>100</td>
    </tr>
    <tr>
        <td>2030</td>
        <td>200</td>
        <td>250</td>
        <td>1000</td>
        <td>150</td>
    </tr>
    <tr>
        <td>2040</td>
        <td>300</td>
        <td>450</td>
        <td>1400</td>
        <td>200</td>
    </tr>
    <tr>
        <td>2050</td>
        <td>350</td>
        <td>550</td>
        <td>1850</td>
        <td>250</td>
    </tr>
  </tbody>
</table>



(b)
<table>
  <tbody>
    <tr>
        <td>Year</td>
        <td>Public Funds</td>
        <td>Take-Up Bias</td>
        <td>Adaptation Bias</td>
        <td>Moral Hazard</td>
    </tr>
    <tr>
        <td>2020</td>
        <td>150</td>
        <td>100</td>
        <td>650</td>
        <td>100</td>
    </tr>
    <tr>
        <td>2029</td>
        <td>200</td>
        <td>200</td>
        <td>1000</td>
        <td>150</td>
    </tr>
    <tr>
        <td>2030</td>
        <td>50</td>
        <td>50</td>
        <td>0</td>
        <td>200</td>
    </tr>
    <tr>
        <td>2040</td>
        <td>100</td>
        <td>50</td>
        <td>0</td>
        <td>250</td>
    </tr>
    <tr>
        <td>2050</td>
        <td>150</td>
        <td>50</td>
        <td>0</td>
        <td>300</td>
    </tr>
  </tbody>
</table>



(c)
<table>
  <tbody>
    <tr>
        <td>Year</td>
        <td>Public Funds</td>
        <td>Take-Up Bias</td>
        <td>Adaptation Bias</td>
        <td>Moral Hazard</td>
    </tr>
    <tr>
        <td>2020</td>
        <td>150</td>
        <td>250</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>2030</td>
        <td>200</td>
        <td>500</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>2040</td>
        <td>250</td>
        <td>800</td>
        <td>0</td>
        <td>0</td>
    </tr>
    <tr>
        <td>2050</td>
        <td>350</td>
        <td>1200</td>
        <td>0</td>
        <td>0</td>
    </tr>
  </tbody>
</table>


(d)
![Legend for welfare effects: Moral Hazard (black), Adaptation Bias (grey), Take-Up Bias (light blue), Public Funds (green)](page_50_image_2_v2.jpg)

Figure 5: Stacked area plots describing the annual welfare gains from 2020-2049 from correctly mapping high-risk homes inside the floodplain rather than outside the floodplain. Panels (a)-(c) give the welfare effects of correct classification relative to different baseline scenarios, and panel (d) is the legend for the different welfare effects. The baseline scenarios are a new high-risk home that would otherwise be mapped outside the floodplain for 30 years (panel a), a new high-risk home that would otherwise be mapped outside the floodplain for 10 years (panel b), and an existing high-risk home that would otherwise stay outside the floodplain for 30 years (panel c).

50

# A Appendix Tables


<table>
  <thead>
    <tr>
        <th>Variable Means</th>
        <th>Outside Floodplain</th>
        <th>Inside Floodplain</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Income</td>
        <td>90672</td>
        <td>99433</td>
    </tr>
    <tr>
        <td>Has College Degree</td>
        <td>.66</td>
        <td>.57</td>
    </tr>
    <tr>
        <td>Age</td>
        <td>56</td>
        <td>57</td>
    </tr>
    <tr>
        <td>Risk Preferences</td>
        <td>4.71</td>
        <td>4.58</td>
    </tr>
    <tr>
        <td>10-Year Risk of Flooding</td>
        <td>17.5</td>
        <td>27.75</td>
    </tr>
    <tr>
        <td>Believe Flood Risk Higher in 30 Years</td>
        <td>0.43</td>
        <td>0.57</td>
    </tr>
    <tr>
        <td>Has Flood Insurance</td>
        <td>.30</td>
        <td>.66</td>
    </tr>
    <tr>
        <td>Insurance Required by Lender</td>
        <td>.06</td>
        <td>.39</td>
    </tr>
    <tr>
        <td>Voluntarily Buy Flood Insurance</td>
        <td>.29</td>
        <td>.55</td>
    </tr>
    <tr>
        <td>Reported Flood Insurance Premium</td>
        <td>665</td>
        <td>1762</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>543</td>
        <td>159</td>
    </tr>
  </tbody>
</table>


Table A1: Summary statistics for estimation sample from survey of flood insurance demand and risk perceptions. Risk Preferences are self-reported willingness to take risks from one to 10, where one is the most risk averse.

51

<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="3">Incentivized Flood Insurance Willingness-to-Pay</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Inside Floodplain</td>
        <td>0.785***</td>
        <td>0.492***</td>
        <td>0.418**</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.124)</td>
        <td>(0.122)</td>
        <td>(0.161)</td>
    </tr>
    <tr>
        <td>Had Past Flood</td>
        <td> </td>
        <td>0.855***</td>
        <td>0.845***</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.131)</td>
        <td>(0.154)</td>
    </tr>
    <tr>
        <td>Flood Depth</td>
        <td> </td>
        <td>0.071</td>
        <td>0.123</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.045)</td>
        <td>(0.083)</td>
    </tr>
    <tr>
        <td>I[Flood Depth = 0]</td>
        <td> </td>
        <td>-0.212*</td>
        <td>-0.163</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.113)</td>
        <td>(0.127)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>702</td>
        <td>702</td>
        <td>702</td>
    </tr>
    <tr>
        <td>R-squared</td>
        <td>0.112</td>
        <td>0.137</td>
        <td>0.133</td>
    </tr>
    <tr>
        <td>Demographics</td>
        <td>Y</td>
        <td>Y</td>
        <td>Y</td>
    </tr>
    <tr>
        <td>ORIV</td>
        <td>N</td>
        <td>N</td>
        <td>Y</td>
    </tr>
  </tbody>
</table>


Table A2: Results from estimating incentivized flood risk perceptions from the survey of coastal homeowners. The dependent variable is inverse hyperbolic sine transformed willingness-to-pay for flood insurance from the incentivized elicitation described in Section 4.2. The primary independent variable of interest is an indicator for being inside the floodplain. Column (2) adds the average 100-year flood depth from the two models, and column (3) applies the Obviously Related Instrumental Variable method to the estimation of the 100-year flood depth coefficient. All models include controls for respondent demographics. Standard errors are clustered by county and bootstrapped in column (3).

52

# B Appendix Figures


<table>
  <thead>
    <tr>
        <th>100-Year Flood Depth (Feet)</th>
        <th>Non-Floodplain</th>
        <th>Floodplain</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0-1</td>
        <td>0.47</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>1-2</td>
        <td>0.12</td>
        <td>0.05</td>
    </tr>
    <tr>
        <td>2-3</td>
        <td>0.04</td>
        <td>0.04</td>
    </tr>
    <tr>
        <td>3-4</td>
        <td>0.02</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>4-5</td>
        <td>0.01</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>5-6</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>6-7</td>
        <td>0.01</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>7-8</td>
        <td>0.00</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>8-9</td>
        <td>0.00</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>9-10</td>
        <td>0.00</td>
        <td>0.01</td>
    </tr>
  </tbody>
</table>


Figure A1: Distribution of average 100-year flood depths from the First Street Foundation and Kat Risk flood models in the flood insurance survey estimation sample.

53

<table>
  <thead>
    <tr>
        <th>Construction Year</th>
        <th>Floodplain built</th>
        <th>Remapped</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1975</td>
        <td>0.26</td>
        <td>0.08</td>
    </tr>
    <tr>
        <td>1980</td>
        <td>0.22</td>
        <td>0.08</td>
    </tr>
    <tr>
        <td>1985</td>
        <td>0.29</td>
        <td>0.11</td>
    </tr>
    <tr>
        <td>1990</td>
        <td>0.30</td>
        <td>0.09</td>
    </tr>
    <tr>
        <td>1995</td>
        <td>0.30</td>
        <td>0.08</td>
    </tr>
    <tr>
        <td>2000</td>
        <td>0.29</td>
        <td>0.07</td>
    </tr>
    <tr>
        <td>2005</td>
        <td>0.38</td>
        <td>0.06</td>
    </tr>
    <tr>
        <td>2010</td>
        <td>0.40</td>
        <td>0.06</td>
    </tr>
  </tbody>
</table>


Figure A2: Mean property elevation of floodplain-built structures in the loss events estimation sample by year built. Property elevation is defined as the difference between a home’s first floor elevation and its FEMA 100-year flood elevation.

54

<table>
  <thead>
    <tr>
        <th>Claim Value</th>
        <th>Frequency</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0 - 5000</td>
        <td>83</td>
    </tr>
    <tr>
        <td>5000 - 10000</td>
        <td>1</td>
    </tr>
    <tr>
        <td>10000 - 15000</td>
        <td>0.5</td>
    </tr>
    <tr>
        <td>15000 - 20000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>20000 - 25000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>25000 - 30000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>30000 - 35000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>35000 - 40000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>40000 - 45000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>45000 - 50000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>50000 - 55000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>55000 - 60000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>60000 - 65000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>65000 - 70000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>70000 - 75000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>75000 - 80000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>80000 - 85000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>85000 - 90000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>90000 - 95000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>95000 - 100000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>100000 - 105000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>105000 - 110000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>110000 - 115000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>115000 - 120000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>120000 - 125000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>125000 - 130000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>130000 - 135000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>135000 - 140000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>140000 - 145000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>145000 - 150000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>150000 - 155000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>155000 - 160000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>160000 - 165000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>165000 - 170000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>170000 - 175000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>175000 - 180000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>180000 - 185000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>185000 - 190000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>190000 - 195000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>195000 - 200000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>200000 - 205000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>205000 - 210000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>210000 - 215000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>215000 - 220000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>220000 - 225000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>225000 - 230000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>230000 - 235000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>235000 - 240000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>240000 - 245000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>245000 - 250000</td>
        <td>0.8</td>
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
        <th>Claim Value (Claim &gt; 0)</th>
        <th>Frequency</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0 - 5000</td>
        <td>11.5</td>
    </tr>
    <tr>
        <td>5000 - 10000</td>
        <td>7.2</td>
    </tr>
    <tr>
        <td>10000 - 15000</td>
        <td>4.8</td>
    </tr>
    <tr>
        <td>15000 - 20000</td>
        <td>3.6</td>
    </tr>
    <tr>
        <td>20000 - 25000</td>
        <td>3.1</td>
    </tr>
    <tr>
        <td>25000 - 30000</td>
        <td>3.2</td>
    </tr>
    <tr>
        <td>30000 - 35000</td>
        <td>3.3</td>
    </tr>
    <tr>
        <td>35000 - 40000</td>
        <td>3.9</td>
    </tr>
    <tr>
        <td>40000 - 45000</td>
        <td>4.5</td>
    </tr>
    <tr>
        <td>45000 - 50000</td>
        <td>4.9</td>
    </tr>
    <tr>
        <td>50000 - 55000</td>
        <td>4.7</td>
    </tr>
    <tr>
        <td>55000 - 60000</td>
        <td>4.5</td>
    </tr>
    <tr>
        <td>60000 - 65000</td>
        <td>4.3</td>
    </tr>
    <tr>
        <td>65000 - 70000</td>
        <td>3.7</td>
    </tr>
    <tr>
        <td>70000 - 75000</td>
        <td>3.3</td>
    </tr>
    <tr>
        <td>75000 - 80000</td>
        <td>3.0</td>
    </tr>
    <tr>
        <td>80000 - 85000</td>
        <td>2.7</td>
    </tr>
    <tr>
        <td>85000 - 90000</td>
        <td>2.3</td>
    </tr>
    <tr>
        <td>90000 - 95000</td>
        <td>2.0</td>
    </tr>
    <tr>
        <td>95000 - 100000</td>
        <td>1.7</td>
    </tr>
    <tr>
        <td>100000 - 105000</td>
        <td>1.6</td>
    </tr>
    <tr>
        <td>105000 - 110000</td>
        <td>1.3</td>
    </tr>
    <tr>
        <td>110000 - 115000</td>
        <td>1.2</td>
    </tr>
    <tr>
        <td>115000 - 120000</td>
        <td>1.1</td>
    </tr>
    <tr>
        <td>120000 - 125000</td>
        <td>1.0</td>
    </tr>
    <tr>
        <td>125000 - 130000</td>
        <td>0.9</td>
    </tr>
    <tr>
        <td>130000 - 135000</td>
        <td>1.1</td>
    </tr>
    <tr>
        <td>135000 - 140000</td>
        <td>0.8</td>
    </tr>
    <tr>
        <td>140000 - 145000</td>
        <td>0.7</td>
    </tr>
    <tr>
        <td>145000 - 150000</td>
        <td>0.6</td>
    </tr>
    <tr>
        <td>150000 - 155000</td>
        <td>0.5</td>
    </tr>
    <tr>
        <td>155000 - 160000</td>
        <td>0.4</td>
    </tr>
    <tr>
        <td>160000 - 165000</td>
        <td>0.4</td>
    </tr>
    <tr>
        <td>165000 - 170000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>170000 - 175000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>175000 - 180000</td>
        <td>0.3</td>
    </tr>
    <tr>
        <td>180000 - 185000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>185000 - 190000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>190000 - 195000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>195000 - 200000</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>200000 - 205000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>205000 - 210000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>210000 - 215000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>215000 - 220000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>220000 - 225000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>225000 - 230000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>230000 - 235000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>235000 - 240000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>240000 - 245000</td>
        <td>0.1</td>
    </tr>
    <tr>
        <td>245000 - 250000</td>
        <td>5.0</td>
    </tr>
  </tbody>
</table>


Figure A3: Histograms of the distribution of flood insurance claims across the loss events estimation sample. The top panel shows the claims distribution for all claims, while the bottom panel shows the distribution for claims greater than 0. Source: OpenFEMA claims data

55

<table>
  <thead>
    <tr>
        <th>Property Elevation (Feet)</th>
        <th>0</th>
        <th>0.5</th>
        <th>1</th>
        <th>1.5</th>
        <th>REMAPPED</th>
        <th>2</th>
        <th>2.5</th>
        <th>3</th>
        <th>3.5</th>
        <th>4</th>
        <th>4.5</th>
        <th>5</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Claim Probability Coefficients</td>
        <td>0.04</td>
        <td>0.024</td>
        <td>0.019</td>
        <td>0.004</td>
        <td>0</td>
        <td>-0.01</td>
        <td>-0.025</td>
        <td>-0.027</td>
        <td>-0.026</td>
        <td>-0.024</td>
        <td>-0.026</td>
        <td>-0.025</td>
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
        <th>Property Elevation (Feet)</th>
        <th>0</th>
        <th>0.5</th>
        <th>1</th>
        <th>REMAPPED</th>
        <th>1.5</th>
        <th>2</th>
        <th>2.5</th>
        <th>3</th>
        <th>3.5</th>
        <th>4</th>
        <th>4.5</th>
        <th>5</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Log Claim Coefficients</td>
        <td>0.04</td>
        <td>0.03</td>
        <td>0.02</td>
        <td>0</td>
        <td>-0.04</td>
        <td>-0.18</td>
        <td>-0.2</td>
        <td>-0.42</td>
        <td>-0.32</td>
        <td>-0.43</td>
        <td>-0.57</td>
        <td>-0.75</td>
    </tr>
  </tbody>
</table>


Figure A4: Results from hurdle model predicting flood insurance claims. The top panel shows results for having a claim as the dependent variable, and the bottom panel shows results for log claims conditional on having a claim as the dependent variable. The figures plot the coefficients of each half-foot elevation indicator with newly mapped properties as the omitted category. Both models include census tract by loss event interacted fixed effects and controls for property year built, deductible, and amount of coverage purchased. Standard errors are clustered by loss event.

56

<table>
  <thead>
    <tr>
        <th>Year</th>
        <th>High-Risk</th>
        <th>Low-Risk</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>2020</td>
        <td>3200</td>
        <td>0</td>
    </tr>
    <tr>
        <td>2050</td>
        <td>5700</td>
        <td>100</td>
    </tr>
  </tbody>
</table>


Figure A5: Average annual flood damages for high-risk and low-risk properties in 2020 and 2050 according to the First Street Foundation Flood Model (FSF-FM). High-risk properties are single family homes built on ground with at least a 1% annual chance of flooding.

57

# C Adjusted Price Elasticity of Voluntary Flood Insurance Demand

In this section, I adjust the price elasticity of flood insurance demand estimate from Table 1 for the share of floodplain flood insurance buyers subject to the mandatory purchase requirement according to the survey data. This adjusted elasticity represents the estimated percent of voluntary flood insurance purchasers who drop their policies in response to a 1% premium increase.

In the survey data, 36% of high-risk buyers outside the floodplain have flood insurance. Applying the newly mapped take-up estimates from Figure 3, this implies that their take-up rate would increase to 72% absent any premium increases one year after being remapped into the floodplain. Given that 39% of homeowners inside the floodplain reported being subject to a mandatory purchase requirement, this implies that $\frac{72-39}{72} = 46\%$ of purchasers are not subject to this mandate.

I assume that the estimated price elasticity in the newly mapped results comes entirely from purchasers not subject to the mandatory purchase requirement not renewing their policies. This implies that the 0.17% of all purchasers not renewing their policies in response to a 1% premium increase represents an elasticity of $\frac{0.17}{0.46} = 0.37$ among voluntary purchasers.

58