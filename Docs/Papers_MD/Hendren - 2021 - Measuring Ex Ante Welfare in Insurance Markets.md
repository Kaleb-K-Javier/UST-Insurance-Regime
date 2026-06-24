Copyedited by: ES MANUSCRIPT CATEGORY: Article

Review of Economic Studies (2021) **88**, 1193–1223 doi:10.1093/restud/rdaa015
© The Author(s) 2020. Published by Oxford University Press on behalf of The Review of Economic Studies Limited. This is an Open Access article distributed under the terms of the Creative Commons Attribution License (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted reuse, distribution, and reproduction in any medium, provided the original work is properly cited. For commercial re-use, please contact journals.permissions@oup.com
Advance access publication 16 June 2020

# Measuring Ex Ante Welfare in Insurance Markets

NATHANIEL HENDREN

Harvard University

*First version received April 2019; Editorial decision March 2020; Accepted March 2020 (Eds.)*

The willingness to pay for insurance captures the value of insurance against only the risk that remains when choices are observed. This article develops tools to measure the ex ante expected utility impact of insurance subsidies and mandates when choices are observed after some insurable information is revealed. The approach retains the transparency of using reduced-form willingness to pay and cost curves, but it adds one additional sufficient statistic: the percentage difference in marginal utilities between insured and uninsured. I provide an approach to estimate this additional statistic that uses only the reduced-form willingness to pay curve, combined with a measure of risk aversion. I compare the approach to structural approaches that require fully specifying the choice environment and information sets of individuals. I apply the approach using existing willingness to pay and cost curve estimates from the low-income health insurance exchange in Massachusetts. Ex ante optimal insurance prices are roughly 30% lower than prices that maximize observed market surplus. While mandates reduce market surplus, the results suggest they would actually increase ex ante expected utility.

**Key words**: Insurance, Adverse selection, Welfare.

**JEL Codes**: H00

## 1. INTRODUCTION

Revealed preference theory is often used as a tool for measuring the welfare impact of government policies. Many recent applications use price variation to estimate the willingness to pay for insurance (Einav et al., 2010; Hackmann et al., 2015; Finkelstein et al., 2019; Panhans, 2019). Comparing willingness to pay to the costs individuals impose on insurers provides a traditional measure of market surplus. This surplus potentially provides guidance on optimal insurance subsidies and mandates (Feldman and Dowd, 1982). If individuals are not willing to pay the costs they impose on the insurer, then greater subsidies or mandates will lower market surplus. From this perspective, subsidies and mandates would reduce welfare and be socially undesirable.

Measures of willingness to pay are generally a gold standard input into welfare analysis. But, in insurance settings they can be misleading. Insurance obtains its value by insuring the realization of risk. Often, individuals make insurance choices after learning some information about their risk. It is well-known that this can lead to adverse selection. What is less appreciated is that observed



The editor in charge of this paper was Adam Szeidl.

1193

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1193 1193–1223

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

1194 REVIEW OF ECONOMIC STUDIES
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

Copyedited by: ES MANUSCRIPT CATEGORY: Article



willingness to pay will not capture value of insuring against this learned information.¹ As a result, welfare conclusions based on market surplus can vary with the information that individuals have when the economist happens to observe choices. Policies that maximize observed market surplus will not generally maximize ex ante expected utility.

To see this, consider the decision to buy health insurance coverage for next year. Suppose some people have learned they need to undergo a costly medical procedure next year. Their willingness to pay will include the value of covering this known cost plus the value of insuring other future unknown costs. Market surplus—measured as the difference between observed willingness to pay and costs in the market—will equal the value of insuring their unknown costs. But, it will not include any insurance value from covering the known costly medical procedure. This risk has already been realized when willingness to pay is observed.

Now, consider an economist seeking to measure the welfare impact of extending health insurance coverage next year to everyone through a mandate or large subsidy. The market surplus or deadweight loss generated from the policy will depend on how much people have learned about their health costs at the time the economist happened to measure willingness to pay. Existing literature (and introspection) suggests that individuals know more about expected costs and events in the near future (e.g., Finkelstein et al., 2005; Hendren, 2013, 2017; Cabral, 2017). This means that if willingness to pay had been measured earlier, market surplus would be larger. This is because it would include the value of insuring against the costly medical procedure. While ex ante market surplus would be larger if it were measured earlier, the economic allocation generated by a mandate does not vary depending on when the economist measures willingness to pay. This means that the ex ante expected utility impact of a mandate would not depend on when the economist happens to measure willingness to pay. While the average willingness to pay tends to decline with the amount of information revealed at the time of making insurance market choices, expected utility should not change. Ex ante expected utility provides a consistent welfare framework to study optimal insurance policies that does not depend upon how much information individuals know at the time they choose to purchase insurance.

The goal of this article is to enable researchers to evaluate the impact of insurance market policies on ex ante welfare, defined as ex ante expected utility.² Traditional methods to estimating ex ante welfare would estimate a structural model. Among other things, the model would specify what individuals know when choosing whether to buy an insurance plan. It would then be estimated using observed insurance choices along with data on the realized utility-relevant outcomes, such as health and consumption.³ If one has a structural model and knows what information has been realized when individuals choose their insurance policies, one can infer the value of insuring the risk that has been revealed before making those choices. But, in practice it is especially difficult to observe individuals’ information sets when they make choices. This is especially true in insurance markets that suffer from asymmetric information.

This article develops a new approach to measure the ex ante welfare impact of insurance market policies. The approach does not require specifying structural assumptions about individuals’ information sets at the time of choice, nor does it require specifying a utility function or observing the distribution of utility-relevant outcomes in the economy. Instead, I exploit the information contained in reduced-form willingness to pay and cost curves as defined in Einav et al. (2010).



\* 1. This idea dates to Hirshleifer (1971), who shows that individuals may wish to insure against the realization of information that is revealed prior to making choices.

\* 2. Throughout the article, I adopt the common assumption in health insurance models that there is no aggregate risk and rational expectations. This means that the ex ante risk distribution corresponds to the realized cross-sectional distribution. As a result, ex ante welfare is also equivalent to measuring (ex post) utilitarian welfare.

\* 3. For example, see Handel et al. (2015) or Section IV of Einav et al. (2016).

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1194 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1195
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

Copyedited by: ES MANUSCRIPT CATEGORY: Article





In this environment, I characterize the minimal additional sufficient statistics required to measure the ex ante welfare impact of subsidies and mandates.

The first main result shows that one can measure ex ante welfare using one additional sufficient statistic: the percentage difference in marginal utilities of income for those who do versus do not buy insurance. This measures how much individuals wish to move money to the state of the world in which they buy insurance. In the example above, it reflects the ex ante desire to insure the costly medical procedure. These individuals have a higher demand for insurance and have a higher marginal utility of income.

In general, it is difficult to observe or measure differences in marginal utilities of income between those who do versus do not purchase insurance. The second result of the article addresses this issue by providing a benchmark estimation method that uses only the reduced-form willingness to pay curve combined with a measure of risk aversion. This additional risk aversion parameter can be assumed, or it can be inferred from the observed markup individuals are willing to pay for insurance, combined with the extent to which insurance reduces the variance in out-of-pocket expenditures.

This second main result follows two steps. First, building on the literature on optimal unemployment insurance (Baily, 1978; Chetty, 2006), I approximate differences in marginal utilities using measures of consumption differences between insured and uninsured combined with risk aversion. Second, since consumption is seldom observed, I provide conditions under which one can exploit the information in the reduced-form willingness to pay curve for insurance instead of using consumption data. When these conditions hold, a high willingness to pay for insurance signals a greater desire for money to help cover medical expenses. This enables the information in the willingness to pay curve to substitute for the consumption difference between the insured and uninsured.

I apply the framework to study the optimal subsidies and mandates for low-income health insurance in Massachusetts. Finkelstein et al. (2019) use price discontinuities as a function of income to estimate willingness to pay and cost curves for those with incomes near 150% of the federal poverty level (150% FPL). Their results show that an unsubsidized private insurance market would unravel.⁴ Without subsidies, the market would not exist. I use my approach to ask what types of insurance subsidies or mandates individuals would want from an ex ante perspective—prior to learning anything about their risk.

I evaluate the welfare impact of both budget neutral and non-budget neutral policies. Budget neutral insurance subsidies are financed by increased prices or penalties for those not purchasing insurance—this is the canonical set of policies studied in Einav et al. (2010). To set the stage, traditional market surplus is maximized when insurance premiums are $1,581 and 41% of those eligible for insurance choose to purchase. In contrast, I find that a 30% lower price of $1,117 with 54% of the market insured maximizes ex ante welfare. From behind a veil of ignorance, individuals value the ability to purchase insurance at lower prices if they end up having a high demand for insurance.

What about a mandate? The sum of willingness to pay across individuals when they are observed in the market is less than the cost they impose on the insurance company. Lowering relative prices enough to yield full coverage would lower the traditional market surplus measure by $45. However, from behind a veil of ignorance mandates increase ex ante welfare: individuals would be willing to pay $169 to have a full insurance mandate. The ex ante value of a mandate remains positive for a wide range of plausible risk aversion parameters (e.g., with coefficients of relative risk aversion above 1.7). This illustrates how an ex ante welfare perspective can lead

4. This unravelling is due to a combination of adverse selection and uncompensated care externalities.
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1195 1193–1223

1196 REVIEW OF ECONOMIC STUDIES

Copyedited by: ES MANUSCRIPT CATEGORY: Article



to very different normative conclusions about the desirability of commonly debated insurance policies.

As in many settings, insurance subsidies in Massachusetts were financed by general government revenue, not by imposing penalties on the uninsured who were eligible for the subsidies. To capture this, I estimate the marginal value of public funds (MVPF) of additional insurance subsidies. The MVPF for an additional insurance subsidy is the individual’s willingness to pay for it divided by its net cost to the government (Hendren, 2016). Comparisons of MVPFs across policies provide a method to assess the relative efficiency of the subsidies as a method of redistribution. For example, comparing the MVPF of insurance subsidies to low-income tax credits allows one to ask whether individuals at 150% FPL would prefer additional insurance subsidies or prefer a tax credit.

The results suggest the ex ante and traditional MVPF can differ significantly. For example, starting with low subsidies such that 30% of the market is insured, the MVPF of increasing subsidies is 1.2 if one uses observed willingness to pay. Individuals are willing to pay roughly 1.2 times the marginal cost they impose on the insurer to lower insurance prices. This is similar to the range of MVPF estimates for tax credits to low-income populations studied in Hendren and Sprung-Keyser (2020), which are around 0.9–1.3. Yet, from behind the veil of ignorance, individuals would be willing to pay 1.8 times the cost they impose on the insurer to lower insurance prices. Ex ante, individuals prefer that the government spend money lowering insurance prices for those at 150% FPL instead of providing them with a tax credit. In summary, ex ante measures of welfare can lead to different conclusions than those based on observed willingness to pay and traditional measures of market surplus.

Traditional approaches to measuring ex ante expected utility would estimate a structural model. This would involve fully specifying not only a utility function but also the information set of individuals at the time they make insurance choices. The economic primitives estimated from the model would then provide an ex ante measure of welfare. In contrast, the sufficient statistics approach developed here does not require researchers to know the exact utility function, nor does it require knowledge of individuals’ information sets when they make insurance choices. Information sets can be particularly tough to specify in settings of adverse selection where even insurers have trouble worrying about the unobserved knowledge of the applicant pool. In addition, the approach developed here can be implemented using aggregate data from insurers or governments on the cost and fraction of the market purchasing insurance at different prices as opposed to requiring individual-level data.

To further understand the relationship to the structural approach, I develop a fully specified structural model with moral hazard and adverse selection that can fully match the reduced form willingness to pay and cost curves in MA setting. The model builds upon the approach in Handel et al. (2015) but augments it with a moral hazard structure developed in Einav et al. (2013). I use the model to verify that the approach developed here recovers the true ex ante welfare quite well. However, the benchmark implementation relies on two key assumptions that may be violated in some applications. I use the structural environment to understand the impact of violating these assumptions and to validate proposed modifications to my approach that help recover ex ante welfare when the key assumptions are violated.



First, using the demand curve to proxy for differences in consumption requires that there are no differences in liquidity or income between the insured and uninsured. While this is perhaps a natural assumption in the context of subsidies to a given income level (*e.g.*, the example above where subsidies are provided to those at 150% FPL in MA), it is quite restrictive in many other settings where income differences may be a key driver of willingness to pay for insurance. In these cases, I show that one can recover ex ante welfare if one can measure the difference in average consumption between the insured and uninsured.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1196 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1197

Second, the benchmark implementation requires that individuals have common coefficients of relative risk aversion. However, previous literature has highlighted a role of preference heterogeneity as an important driver of insurance demand. In this case, the risk premium individuals are willing to pay to insure against ex ante risk may differ from the risk premium they are willing to pay to insure against risk that remains at the time they choose to buy insurance. This can generate bias in the benchmark implementation. This means in practice researchers will want to study the robustness of the results to a range of risk aversion parameters. But more generally, the first main result provided in Proposition 1 continues to hold even in the presence of preference heterogeneity. This provides a potential roadmap for future work to develop methods to measure the markup individuals are willing to pay for insurance against risk that is realized prior to making insurance coverage choices.

In the broader context of existing literature, the ideas developed in this article readily extend to other settings where individuals measure the value of insurance using principles of revealed preference. For example, often behavioural responses such as labour supply changes are used to measure the value of social insurance. The more individuals are willing to adjust their labour supply to become eligible for insurance, the more they value the insurance (e.g., Keane and Moffitt, 1998; Dague, 2014; Gallen, 2015). However, this approach only captures the value of insurance against the risk that remains after adjusting their labour supply. Similarly, other papers infer willingness to pay for social insurance from changes in consumption around a shock (e.g., Gruber, 1997; Meyer and Mok, 2019). When information is revealed over time, the consumption change may vary depending on the time horizon used (Hendren, 2017). In the extreme, there may be no change around the event (e.g., smooth consumption around onset of disability or retirement). Consumption should change when information about the event is revealed, not when the event occurs. The methods in this article can be useful to devise strategies to recover ex ante measures of welfare in these settings.

The rest of this article proceeds as follows. Section 2 provides a stylized example that develops the intuition for the approach. Section 3 provides the general modelling framework. Section 4 uses the model to define notions of ex ante welfare and provides the general result that the ex ante willingness to pay for insurance requires the percentage difference in marginal utilities between insured and uninsured. Section 5 provides a benchmark method to estimate this difference in marginal utilities using willingness to pay curve combined with a measure of risk aversion. Section 6 implements this approach to study optimal health insurance subsidies for low-income adults in Massachusetts using the estimates from Finkelstein et al. (2019). Section 7 develops a structural model to compare the validity of the proposed approach to the model’s measure of ex ante welfare and also uses the model to study the impact of violations of the implementation assumptions outlined in Section 5. Section 8 concludes.

## 2. STYLIZED EXAMPLE

I begin with a stylized example to illustrate the distinction between market surplus and ex ante expected utility and to summarize the article’s main results. Suppose individuals have $30 dollars but face a risk of losing $m dollars, where m is uniformly distributed between 0 and 10. I adopt a rational expectations framework with no aggregate risk. This means that the realized cross-sectional distribution in the economy corresponds to the ex ante distribution of risk. Let $D^{\text{ex ante}}$ denote the willingness to pay or “demand” for a full insurance contract that is measured prior to individuals learning anything about their particular realization of m. This solves

$$ u(30 - D^{\text{ex ante}}) = E[u(30 - m)], \quad (1) $$

where $E[u(30 - m)] = \frac{1}{10} \int_{0}^{10} u(30 - m) dm$ is the expected utility if uninsured.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1197 1193–1223

1198 REVIEW OF ECONOMIC STUDIES

A **Before Information Revealed**

$W^{Ex-Ante} = D^{Ex-Ante} - E[m] = 0.50$

<table>
  <tbody>
    <tr>
        <td>Fraction Insured (s)</td>
        <td>Demand D(s) ($)</td>
        <td>Cost C(s) ($)</td>
    </tr>
    <tr>
        <td>0</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
    <tr>
        <td>1</td>
        <td>5.50</td>
        <td>5.00</td>
    </tr>
  </tbody>
</table>

$s^{CE} = 1$


B **After Information Revealed**

No lost surplus from foregone trades

<table>
  <tbody>
    <tr>
        <td>Fraction Insured (s)</td>
        <td>Demand D(s) / Cost C(s) ($)</td>
        <td>Average Cost AC(s) ($)</td>
    </tr>
    <tr>
        <td>0</td>
        <td>10.0</td>
        <td>10.0</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>8.0</td>
        <td>9.0</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>6.0</td>
        <td>8.0</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>4.0</td>
        <td>7.0</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>2.0</td>
        <td>6.0</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0.0</td>
        <td>5.0</td>
    </tr>
  </tbody>
</table>

$s^{CE} = 0$

FIGURE 1
Example willingness to pay and cost curves

Suppose individuals have a utility function with a constant coefficient of relative risk aversion of 3 (i.e., $u(c) = \frac{1}{1-\sigma} c^{1-\sigma}$ and $\sigma = 3$). This implies individuals are willing to pay $D^{\text{ex ante}} = 5.50$ for an insurance policy that fully compensates for their loss. The cost of this policy would be $E[m] = 5$. Full insurance generates a market surplus of $0.50.

Figure 1A draws the demand and cost curves that would be revealed through random variation in prices in this environment, as formalized in Einav et al. (2010). The horizontal axis enumerates the population in descending order of their willingness to pay for insurance, indexed by $s \in [0, 1]$. The vertical axis reflects prices, costs, and willingness to pay in the market. Each individual is willing to pay $5.50 for insurance, reflected in the horizontal demand curve of $D(s) = \$5.50$. In addition, each person imposes an expected cost of $5 on the insurance company, which generates a flat cost curve of $C(s) = \$5$. If a competitive market were to open up in this setting, one would expect everyone to purchase insurance at a price of $5, depicted by the vertical line at $s^{\text{CE}} = 1$. This allocation would generate $W^{\text{ex ante}} = \$0.50$ of welfare, as reflected by the market surplus defined as the integral between demand and cost curve.

What happens if individuals learn about their costs before they choose whether to purchase insurance? For simplicity, consider the extreme case that individuals have fully learned their cost, $m$. Willingness to pay will equal individuals’ known costs, $D(s) = m(s)$. Those who learn they will lose $10 will be willing to pay $10 for “insurance” against their loss; individuals who learn they will lose $0 will be willing to pay nothing. The uniform distribution of risks generates a linear demand curve falling from $10 at $s = 0$ to $0 at $s = 1$. The cost imposed on the insurer by the type $s$, $C(s)$, will equal their willingness to pay of $D(s)$, as shown in Figure 1B.

If an insurer were to try to sell insurance, they would need to set prices to cover the average cost of those who purchase insurance. Let $S \sim \text{Uniform}[0, 1]$ be a uniform random variable and define the average cost of those with willingness to pay above $D(s)$ by $AC(s) = E[C(S) | S \le s]$. This average cost lies everywhere above the demand curve. Since no one is willing to pay the pooled cost of those with higher willingness to pay, the market would fully unravel. The unique competitive equilibrium would involve no one obtaining any insurance, $s^{\text{CE}} = 0$.

What is the welfare cost of this market unravelling? From a market surplus perspective, there is no welfare loss. There are no valuable foregone trades that can take place at the time insurance choices are made. This reflects an extreme case of a more general phenomenon identified in Hirshleifer (1971). The market demand curve does not capture the value of insurance against the portion of risk that has already been realized at the time insurance choices are made. This means

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1198 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1199

that policies that maximize market surplus may not maximize expected utility if one measures expected utility prior to when all information about $m$ is revealed to the individuals.

How can one recover measures of ex ante welfare? The traditional approach to measuring $D^{\text{ex ante}}$ and the value of other insurance market policies would require the econometrician to specify economic primitives, such as a utility function and an assumption about individuals' information sets at the time of choice. It would then also involve measuring the distribution of outcomes that enter the utility function, such as consumption, and use this information to infer the ex ante value of insurance from the model. If one knows the utility function, $u$, and the cross-sectional distribution of consumption ($30 - m$ in the example above), then one can use this information to compute $D^{\text{ex ante}}$ in equation (1). For recent implementations of this approach, see Handel et al. (2015), Section IV of Einav et al. (2016), or Finkelstein et al. (2019).

The goal of this article is to measure the expected utility impact of insurance market policies, such as optimal subsidies and mandates, without knowledge of the full distribution of structural primitives in the economy (e.g., utilities, outcomes, and beliefs). Rather, the article builds on the reduced form framework that uses price variation to identify demand and cost curves in the economy. I use these curves to calculate the sufficient statistics necessary to measure the utility impact insurance market policies.

The core idea can be seen in the following example of a budget-neutral expansion of the market. To expand the size of the insurance market in a budget neutral way, one needs to subsidize insurance purchases and tax those who do not purchase insurance. These transfers between insured and uninsured do not affect market surplus. The market surplus from expanding the size of the insurance market from $s$ to $s+ds$ is given by $D(s) - C(s)$. However, from an ex ante perspective, these transfers affect welfare if the marginal utility of income is different for the insured versus uninsured.

The first main result shows that if individuals had been asked their willingness to pay to have a large insurance market prior to learning their risk type, they would have been willing to pay not just what is measured when making choices to purchase insurance in the market, $D(s)$ but an additional amount $EA(s)$, where

$$ EA(s) = s(1-s)(-D'(s)) \frac{E[u_c|Ins] - E[u_c|Unins]}{E[u_c]} \eqno(2) $$

The first term, $-s(1-s)D'(s)$ characterizes the size of the transfer from uninsured to insured when expanding the size of the insurance market.⁵ The second term, $\frac{E[u_c|Ins] - E[u_c|Unins]}{E[u_c]}$, captures the value of this transfer using the difference in the marginal utilities of income between the insured and uninsured. If the insured have higher marginal utilities of income, then expanding the size of the insurance market by lowering the prices paid by the insured has ex ante value beyond what is captured in traditional measures of market surplus.

Constructing $EA(s)$ in equation (2) requires knowledge of the percentage difference in marginal utilities between insured and uninsured. Such differences are not directly observed. The second main result of the article shows that if consumption levels are the only determinant of marginal utilities, then one can approximate this difference in marginal utilities using the difference in consumption between insured and uninsured, multiplied by a coefficient of risk aversion.

\* 5. The term $D'(s)$ captures how changes in the size of the market translate into changes in the relative price of insurance, $p_I - p_U$. This is weighted by $s(1-s)$ to account for the fact that the size of price increase for the insured is inversely proportional to $1-s$ (a high $1-s$ means insurance prices decline rapidly when the market expands because more people pay $p_U$). Similarly, the price decrease for the uninsured is inversely proportional to $s$. As shown in Supplementary Appendix A, these two forces imply that the size of the transfer is $s(1-s)(-D'(s))$.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1199 1193–1223

1200 REVIEW OF ECONOMIC STUDIES
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>D(s)</th>
        <th>C(s)</th>
        <th>D^Ex-Ante(s)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>10</td>
        <td>10</td>
        <td>10</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>8</td>
        <td>7.5</td>
        <td>8.5</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>6</td>
        <td>5</td>
        <td>7</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>4</td>
        <td>2.5</td>
        <td>5.5</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>2</td>
        <td>0</td>
        <td>3.5</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0</td>
        <td>-2.5</td>
        <td>1.5</td>
    </tr>
  </tbody>
</table>


FIGURE 2
Recovering ex ante willingness to pay

Consumption is rarely observed in practice. However, notice that in the model those with high willingness to pay for insurance are those with lower consumption. Therefore, in a final step, I provide conditions under which information in the demand curve can proxy for the consumption difference. This leads to the formula:

$$ \frac{E[u_c|Ins] - E[u_c|Unins]}{E[u_c]} \approx \gamma(s)(D(s) - E[D(S)|S \geq s]), \tag{3} $$

where $\gamma(s) = \frac{-u''}{u'}$ is the coefficient of absolute risk aversion for those indifferent to purchasing insurance, and $D(s) - E[D(S)|S \geq s]$ is the difference in willingness to pay between the average uninsured individual and the marginal insured type. This latter term captures the difference in average consumption between the insured and uninsured. When 50% of the market own insurance, this difference is $2.50: the insured pay $5 for insurance and the uninsured pay $0 but on average experience a $2.5 loss, generating a difference in consumption of $2.5 on average.

The assumptions needed to generate equation (3) are stated formally in Section 5. Most notably, they require no income or liquidity differences between insured and uninsured and they assume no heterogeneity in risk aversion. While not without loss of generality, the formula provides a benchmark implementation to measure ex ante expected utility with only the addition of a risk aversion coefficient. Section 7.4 provides a practical discussion of violations of these assumptions and what types of additional data or parameters can be useful in those cases to recover ex ante welfare.

To illustrate how the formula for $EA(s)$ recovers ex ante welfare, Figure 2 calculates $EA(s)$ for all values of $s \in [0,1]$ using equations (2) and (3). The coefficient of risk aversion of 3 implies a coefficient of absolute risk aversion of $\gamma = \frac{3}{25}$, where 25 is the average consumption in the economy. At each value of $s$, $D^{\text{ex ante}}(s)$ measures the impact on ex ante expected utility of expanding the size of the insurance market from $s$ to $s+ds$. For example, when 50% of the market is insured, the formula suggests that individuals are willing to pay an additional $0.75 to expand the insurance market relative to what is revealed through the observed demand curve. The integral from $s=0$ to $s=1$ measures the ex ante willingness to pay to fully insure the market

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1200 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1201

(relative to having no one insured):

$$ D^{\text{ex ante}} = \int_{0}^{1} D^{\text{ex ante}}(s) ds = 5.50. $$

Numerically integrating the ex ante demand curve in Figure 2 yields approximately $5.50, which equals the integral under the demand curve in Figure 1A. The ex ante demand curve recovers the willingness to pay individuals would have for everyone to be insured ($s=1$) if they were asked this willingness to pay prior to learning $m$.

The model in this section is highly stylized. There is no moral hazard, no preference heterogeneity, and the model assumes all information about costs, $m$, was revealed at the time of making the insurance decision. The next three sections extend these derivations to capture more realistic features of insurance markets encountered in common empirical applications and apply them to the study of health insurance subsidies to low-income adults in Massachusetts. The main result of Section 4 will be to show that equation (2) continues to be the key additional sufficient statistic required to construct the ex ante willingness to pay for insurance. Section 5 will then establish conditions under which one can approximate this statistic using the demand and cost curves combined with a measure of risk aversion, as in equation (3) above.

# 3. GENERAL MODEL

This section develops a general model environment that can be applied to realistic empirical applications such as the health insurance exchange for low-income adults in Massachusetts studied in Finkelstein et al. (2019).

## 3.1. Setup

Individuals face evolving risk over periods $t=1, \dots, T$ that is captured by the realization of a random variable or "shock," $\Theta'_t$, whose realizations are in $\mathbb{R}^N$. The shock can be multi-dimensional ($N > 1$) so that it captures all aspects of an individual's life (level of utility, marginal utility of medical spending, information about future values of $\Theta'_{t'}$ for $t' > t$, etc.). Shocks may be correlated over time. I define $\Theta_t = \{\Theta'_1, \dots, \Theta'_t\}$ to be the history of shocks up to period $t$. I let $\theta_t$ denote particular realizations of $\Theta_t$. For notational brevity, I abstract from the distinction between the random variable and its realizations and generally refer to the variable by its realizations, $\theta_t$.⁶ As in Section 2, I assume rational expectations and no aggregate risk so that the distribution of $\theta_t$ is equal to the cross-sectional population distribution of realizations of $\theta_t$.⁷ I let period $t=0$ denote the ex ante period before anyone learns any shocks, so that individuals have identical beliefs that correspond to the population distributions of $\theta_t$.

Realized utility in period $t$, $u_t(c, m; \theta_t)$, is a function of the history of shocks up to period $t$, $\theta_t$, and choices of non-medical consumption, $c$, and medical consumption, $m$.⁸ For each $\theta_t$ and $t$, I assume $u_t$ is twice-continuously differentiable and strictly concave in $c$ and $m$ and strictly increasing⁹ in $c$ but not necessarily increasing in $m$ (so that fully insured individuals may choose

\*6. More formally, if $G_{\Theta_s}(\theta_s)$ describes the distribution of $\Theta_s$ given a particular realization of $\Theta_t = \theta_t$, I will use the notation $E[f(\theta_s) | \theta_t]$ to denote $\int f(\theta_s) dG_{\Theta_s}(\theta_s) d\theta_s$.

\*7. Note that for any $t > t'$, only the more recent realization governs beliefs, $E[f(\theta_s) | \theta_t, \theta_{t'}] = E[f(\theta_s) | \theta_t]$. For simplicity, I refer to realizations of $\theta_t$ as a "realization of a shock," even though the first $t-1$ components $\theta'_1, \dots, \theta'_{t-1}$ will be known to the individual in time $t$.

\*8. Note the utility function allows for discounting, e.g., $u_t(c, m; \theta_t) = \beta^t \tilde{u}(c, m; \theta_t)$.

\*9. To ensure existence of willingness to pay below, I assume $\lim_{c \to \infty} u_t(c, m; \theta_t) = \infty$ for each $m$.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1201 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article

1202 REVIEW OF ECONOMIC STUDIES

finite $m$). In each period $t$, individuals learn their realization of $\theta_t$ and then choose $c$ and $m$ subject to a constraint $g_t(c, m; \theta_t) \leq 0$. I assume $g_t(c, m; \theta_t)$ is twice continuously differentiable and weakly convex in both $c$ and $m$. I assume $g_t$ is strictly increasing in $c$ and weakly increasing in $m$ (which allows for full insurance contracts in period $t$).$^{10}$ The budget constraint can depend on the full history of shocks, $\theta_t$. For the model in the main text, I assume that the budget constraint in period $t$ is independent of consumption and medical spending choices in other periods (i.e., no savings). In Supplementary Appendix A.2, I discuss a generalization of the model that allows for savings and show that Proposition 1 continues to hold.$^{11}$

To this standard environment, I add the ability in period $t = \mu$ for individuals with $\theta_\mu \in M$ to decide whether or not to purchase insurance that affects their budget constraint in a single period $t = \nu > \mu$. In the example in Section 6, individuals with incomes near 150% FPL will have the opportunity in the fall of 2010 to purchase insurance in the Massachusetts subsidized health insurance exchange for the 2011 calendar year. In the notation, $\nu = 2011$, $\mu$ is the open enrolment period in the fall of 2010, and $M$ is the set of individuals whose incomes are near 150% FPL. Individuals who chose to purchase insurance for period $\nu$ have the budget constraint

$$c_\nu + x(m_\nu) \leq y_\nu(\theta_\nu) - p_I, \tag{4}$$

where $x(m_\nu)$ is the out of pocket costs required for gross medical spending of $m_\nu$, $y_\nu(\theta_\nu)$ is the individual's income, and $p_I$ is a price paid by the insured. I assume $x(m_\nu)$ is continuously differentiable, weakly increasing, and weakly convex in $m_\nu$. Individuals who chose to be uninsured have a budget constraint

$$c_\nu + m_\nu \leq y_\nu(\theta_\nu) - p_U, \tag{5}$$

where $p_U$ is the price of being uninsured.

After observing $\theta_\nu$, individuals choose $(c, m)$ to maximize their utility, leading to indirect utility functions for those who chose to be insured and uninsured:

$$v^I(p_I, \theta_\nu) = \max_{c,m} u(c, m; \theta_\nu) \text{ s.t. } (4)$$

$$v^U(p_U, \theta_\nu) = \max_{c,m} u(c, m; \theta_\nu) \text{ s.t. } (5)$$

and optimal allocations for the insured and uninsured given by $(c_\nu^I(p_I, \theta_\nu), m_\nu^I(p_I, \theta_\nu))$, and $(c_\nu^U(p_U, \theta_\nu), m_\nu^U(p_U, \theta_\nu))$.$^{12}$

For any individual who is eligible to purchase insurance, $\theta_\mu \in M$, and any price of being uninsured, $p_U$, I define their relative willingness to pay for insurance, $d(p_U, \theta_\mu)$, as the solution to

$$E\left[v^U(p_U, \theta_\nu) \middle| \theta_\mu\right] = E\left[v^I(d(p_U, \theta_\mu) + p_U, \theta_\nu) \middle| \theta_\mu\right] \tag{6}$$

10. I assume that the maximization program is bounded so that there exists values $\bar{c}, \bar{m}$ such that (a) for any $\tilde{m}$, $g_t(c, \tilde{m}; \theta_t) > 0$ for all $c \geq \bar{c}$ and (b) for any $\tilde{c}$, $u_t(\tilde{c}, m; \theta_t)$ is decreasing in $m$ for all $m > \bar{m}$. This ensures choices $c$ and $m$ lie below $\bar{c}$ and $\bar{m}$ for all $t$ and $\theta_t$.

11. While Proposition 1 continues to hold in the presence of more general constraints that allow for savings, the baseline implementation that uses the demand curve to proxy for differences in consumption between the insured and uninsured in equation (3) does not necessarily hold. Instead, one can recover ex ante welfare using information on consumption of the insured and uninsured (as discussed in Proposition 3).

12. These choices are guaranteed to exist, to be unique, and to be continuously differentiable in $p_U$ and $p_I$ because $x$ is continuously differentiable, increasing, and convex in $m$, and the utility function is strictly concave and twice differentiable in $m$.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1202 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1203

which equates expected utility of the uninsured to the insured who pay a relative price $d(p_U, \theta_\mu)$.¹³ Note that the model assumptions imply that $d$ is continuously differentiable in $p_U$.¹⁴

While much of the results will allow for a general utility function, it will be useful to also discuss the case where there are no income effects in period $t = \nu$ on choices of $m$ and the level of willingness to pay, $d$.

**Definition.** The utility function, $u_\nu(c, m; \theta_\nu)$ is said to have *no income effects* if there exists positive constants $a$ and $b$ and a function $\kappa(m, \theta_\nu)$ such that

$$u_\nu(c, m; \theta_\nu) = -ae^{-b[c+\kappa(m, \theta_\nu)]}$$

for all $\theta_\nu$.

A utility function that satisfies no income effects implies that $m_\nu^I(p_I, \theta_\nu)$, $m_\nu^U(p_U, \theta_\nu)$, and $d(p_U, \theta_\mu)$ do not depend on the amount of income individuals have in period $t = \nu$, and therefore these functions do not depend on $p_I$ or $p_U$.¹⁵ Assuming no income effects is restrictive, but it enables the environment to nest results in existing literature and provide more precise characterizations of some results below.

## 3.2. Aggregating to Market Willingness to Pay and Cost Curves

To aggregate the model to market-level willingness to pay and cost curves, I impose a smoothness condition that requires the population distribution of willingness to pay, $d(p_U, \theta_\mu)$, to be continuously distributed with positive mass throughout its support. Formally, let $\zeta(\Delta, p_U) = Pr\{d(p_U, \theta_\mu) > \Delta\}$ denote the fraction of the market that purchases insurance when prices are $p_U$ and $\Delta = p_I - p_U$. The assumption that $d(p_U, \theta_\mu)$ is continuously distributed means that $\zeta(\Delta, p_U)$ is continuously differentiable in $(\Delta, p_U)$. The assumption that $d$ has positive mass throughout its support means that $\frac{\partial \zeta}{\partial \Delta} < 0$ for each $(\Delta, p_U)$.¹⁶ The smoothness condition is an implicit assumption on the utility function and smoothness of the shock distribution (i.e., distribution of $\theta_\mu$). Section 7 provides a class of utility functions and type distributions where this smoothness condition is satisfied. In Supplementary Appendix I, I use the structural model in Section 7 to consider the case when this assumption is violated because of discontinuous demand curves.¹⁷

**Market willingness to pay curve** Let $D(p_U, s)$ denote the marginal price of insurance required for a fraction $s \in [0, 1]$ of the market to purchase insurance. This is given by the solution



\*13. Note that $d(p_U, \theta_\mu)$ is well-defined because utility is strictly increasing in $c$.

\*14. The envelope theorem (Milgrom and Segal, 2002) implies $\frac{\partial v^U}{\partial p_U} = -\frac{\partial u^\nu}{\partial c}(c_\nu^U(p_U, \theta_\nu), m_\nu^U(p_U, \theta_\nu))$ so that this follows from the fact that choices $c_\nu^U$ and $m_\nu^U$ are differentiable in $p_U$ and utility is twice continuously differentiable in $(c, m)$. Similarly, this holds for $v^I$ as well.

\*15. Supplementary Appendix A provides proofs of these statements. The fact that $m$ does not depend on transfers follows from quasi-linearity between $c$ and $\kappa(m, \theta_\nu)$; the fact that $d$ does not depend on transfers follows from the fact that $\frac{\partial u}{\partial c} = \gamma u$ (where $\gamma = ab$ is the coefficient of absolute risk aversion). Because $a$ and $b$ are constant, this specification also rules out income effects for ex ante willingness to pay for insurance ($W(s)$ defined below).

\*16. Moreover, $Pr\{d(p_U, \theta_\mu) > \Delta\} = Pr\{d(p_U, \theta_\mu) \geq \Delta\}$ so that the precise specification of inequalities does not affect the market size, $\zeta(\Delta, p_U)$.

\*17. I simulate an econometrician who estimates a continuously differentiable approximation to a discontinuous demand curve. The simulations suggest that applying my proposed method to continuously differentiable approximations to the demand curve provide relatively accurate approximations to the true measures of ex ante welfare ($W(s)$ defined below), even when aggregating across the points of discontinuity. But, as one would expect, estimates of the marginal willingness to pay (estimates of $W'(s)$ below) can be biased near the point of discontinuity in the demand curve.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1203 1193–1223

1204 REVIEW OF ECONOMIC STUDIES
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

to $\zeta(D(p_U, s), p_U) = s$.<sup>18</sup> The assumption that $\zeta(\Delta, p_U)$ is continuously differentiable in $\Delta$ means that $D$ is continuously differentiable by the implicit function theorem, and the fact that $\zeta(\Delta, p_U)$ is strictly decreasing in $\Delta$ implies $D$ is strictly decreasing in $\Delta$.

For any type $\theta_\mu$, I let $S(p_U, \theta_\mu) = \zeta(d(p_U, \theta_\mu), p_U)$ denote the fraction of the market that is insured when prices are such that type $\theta_\mu$ is indifferent to purchasing insurance. Because $d(p_U, \theta_\mu)$ is continuously distributed, its quantiles are uniquely defined and uniformly distributed over $[0, 1]$ with no point masses. Because the quantiles of willingness to pay correspond to $1 - S(p_U, \theta_\mu)$, this means that $S(p_U, \theta_\mu) \sim \text{Uniform}[0, 1]$ for each $p_U$. If the utility function has no income effects, I write $D(s)$ in place of $D(p_U, s)$ and $S(\theta_\mu)$ in place of $S(p_U, \theta_\mu)$.

**Cost of insured population** For any $s$, let $AC(p_U, s)$ denote the average cost when a fraction $s$ of the market owns insurance.

$$AC(p_U, s) = E \left[ m_\nu^I(D(p_U, s) + p_U, \theta_\nu) - x(m_\nu^I(D(p_U, s) + p_U, \theta_\nu)) \mid d(p_U, \theta_\mu) \geq D(p_U, s) \right], \tag{7}$$

where the expectation is taken with respect to all $\theta_\mu \in M$ that choose to purchase insurance when prices are such that a fraction $s$ of the market is insured, $d(p_U, \theta_\mu) \geq D(p_U, s)$.<sup>19</sup> Insurer costs are given by the average difference between an individual's medical costs, $m_\nu^I$, and the portion of these costs they pay out-of-pocket, $x(m_\nu^I)$. Since $d(p_U, \theta_\mu)$ is continuously distributed and continuously differentiable in $p_U$, $AC(p_U, s)$ is differentiable in $(p_U, s)$.<sup>20</sup>

## 4. MEASURING EX ANTE WELFARE

This section derives methods to measure the ex ante welfare impact (from the perspective of $t = 0$) of policies that change the prices $p_I$ and $p_U$ in the market for insurance that affects the budget constraint in period $t = \nu$. I define ex ante welfare formally as follows. For any sequence of shocks realized over an individual's lifetime, $\{\theta_t\}_{t=1}^T$, let $c_t^*(p_I, p_U, \theta_t)$ and $m_t^*(p_I, p_U, \theta_t)$ denote individuals' choices of consumption and medical spending in each period, $t$. For example, in period $t = \nu$ the optimal allocation of medical spending is<sup>21</sup>

$$m_\nu^*(p_I, p_U, \theta_\nu) = 1\{d(p_U, \theta_\mu) > p_I - p_U\} m_\nu^I(p_I, \theta_\nu) + 1\{d(p_U, \theta_\mu) \leq p_I - p_U\} m_\nu^U(p_U, \theta_\nu).$$



\*18. Note that the solution exists and is unique because $\zeta$ is strictly decreasing and continuous in $\Delta$ and there exists prices such that no one purchases insurance and everyone purchases insurance (e.g., $\zeta(x, p_U) = 1$ as $x \to -\infty$ and $\zeta(x, p_U) = 0$ as $x \to \infty$).

\*19. This is equivalent to the set of all $\theta_\mu$ such that $S(p_U, \theta_\mu) \leq s$.

\*20. This follows because the fact that $d$ is continuously distributed in the population means the quantiles $1 - s$ of willingness to pay can be represented by a uniform distribution. Total costs when $s$ of the market is insured is given by

$$sAC(p_U, s) = \int_0^s E[m_\nu^I(D(p_U, s) + p_U, \theta_\nu) - x(m_\nu^I(D(p_U, s) + p_U, \theta_\nu)) \mid S(p_U, \theta_\mu) = \tilde{s}] d\tilde{s}.$$

So, the derivative of total costs is given by

$$\frac{d}{ds} [sAC(p_U, s)] = E[m_\nu^I - x(m_\nu^I) \mid S(p_U, \theta_\mu) = s] + sE \left[ \frac{dm_\nu^I}{ds} (1 - x'(m_\nu^I)) \mid S(p_U, \theta_\mu) \leq s \right].$$

where $m_\nu^I$ is evaluated at $m_\nu^I(D(p_U, s) + p_U, \theta_\nu)$. Note that $\frac{dm_\nu^I}{ds} = \frac{\partial D}{\partial s} \frac{\partial m_\nu^I}{\partial p_I}$ exists because $m_\nu^I$ is continuously differentiable in $p_I$. Finally, if there are no income effects, then $\frac{dm_\nu^I}{ds} = 0$ so that the derivative of total cost is the cost of the marginal enrolees, as in Einav et al. (2010).

\*21. Note this is well-defined because $\theta_\mu$ is contained in $\theta_\nu$ so that $d(p_U, \theta_\mu)$ is implied by $p_U$ and $\theta_\nu$.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1204 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

# HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1205

In other periods $t \neq \nu$, these choices will not depend on $p_I$ and $p_U$ because there is no scope for savings behaviour in the baseline model. Here, I maintain $p_I$ and $p_U$ in the notation for choices in other periods and discuss the extension to allow for savings in Supplementary Appendix A.2.

Let $v_t(p_I, p_U, \theta_t)$ denote the utility level in period $t$ that is attained by an individual with realization $\theta_t$. This solves

$$v_t(p_I, p_U; \theta_t) = u_t(c_t^*(p_I, p_U, \theta_t), m_t^*(p_I, p_U, \theta_t); \theta_t)$$

The ex ante expected utility of having prices $p_I$ and $p_U$ is given by:

$$V(p_I, p_U) = E\left[\sum_{t=1}^T v_t(p_I, p_U; \theta_t)\right] \tag{8}$$

where the expectation is taken at $t=0$. The absence of aggregate risk and the assumption of rational expectations mean that the expectation is taken with respect to the population distribution of sequences of realizations of $\{\theta_t\}_{t=1}^T$.<sup>22</sup> This means that $V$ is not only ex ante welfare but also corresponds to (ex post) utilitarian welfare.

## 4.1. Budget Neutral Policies: "Ex Ante" Demand Curve

I consider two classes of policies that change the fraction of the market that is insured in period $t = \nu$: budget neutral and non-budget neutral policies. Budget neutral policies involve reductions in the price of insurance, $p_I$, financed by increases in the price of being uninsured, $p_U$, charged to those in the market $\theta_\mu \in M$. In a world where prices cover costs, $p_I(s)$ and $p_U(s)$ satisfy two equations:

$$D(p_U(s), s) = p_I(s) - p_U(s)$$

$$sAC(p_U(s), s) = [sp_I(s) + (1 - s)p_U(s)],$$

where the first equation requires the fraction insured, $s$, be consistent with the fraction that wish to purchases at prices $p_I(s) - p_U(s)$ and the second equation requires that the total cost of insurance equals the sum of premiums collected. Recall that $AC(p_U, s)$ is differentiable in $p_U$ and $s$. Therefore, I define the marginal cost of expanding the insurance market through budget neutral price changes as the derivative of total costs,

$$C(s) = \frac{d}{ds}[sAC(p_U(s), s)]$$

This derivative includes the impact of changes to the composition of who is insured and any income effects from the price changes on medical spending, $m_\nu^I$.

I define $W(s)$ to be the ex ante equivalent-variation measure of willingness to pay to have a fraction $s$ of the market insured. Formally, this is the amount of income that makes individuals



\*22. Ex ante utility measures preferences for prices $p_I$ and $p_U$ at time $t=0$ before individuals learn anything about themselves. But, it is equivalent measuring ex ante utility at any period $t'$ such that individuals do not yet have any particular knowledge about their particular values of $v_\nu(p_I, p_U; \theta_\nu)$. To see this, note that if for some $t < \nu$ we have $E[v_\nu(p_I, p_U; \theta_\nu) | \theta_t] = E[v_\nu(p_I, p_U; \theta_\nu)]$, for all realizations of $\theta_t$, then $E[v_\nu(p_I, p_U; \theta_\nu)]$ will correspond to $E[v_\nu(p_I, p_U; \theta_\nu) | \theta_t]$, where the expectation is evaluated at time $t$. In the MA example, this means that measuring ex ante welfare corresponds to measuring the subsidies or mandates that individuals would desire to have in the MA health insurance exchange if one asked them prior to learning anything about their particular utility-relevant risks.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1205 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article

1206 REVIEW OF ECONOMIC STUDIES

indifferent between being uninsured with additional income $W(s)$ and having prices of insurance given by $p_I(s)$ and $p_U(s)$. This solves:

$$V(p_I(s), p_U(s)) = V(\infty, -W(s)), \tag{9}$$

where the LHS is the ex ante expected utility of prices $p_I(s)$ and $p_U(s)$ and the RHS is the expected utility of giving a transfer of size $W(s)$ in the world where no one is insured.<sup>23</sup> Equation (9) implies that the size of the market that maximizes $W(s)$ is the size of the market maximizes ex ante welfare, $V(p_I(s), p_U(s))$.

**Sufficient statistic representation of W(s)** Traditional approaches to measuring $W(s)$ would follow a structural approach that specifies a utility function, $u$, budget constraints, and beliefs. It would then estimate the model using individual-level data on $c$ and $m$, combined with sufficient identifying variation to estimate all model components. In particular, this approach would attempt to separately estimate both utility and beliefs, which is often quite difficult and usually rests on the assumption that the econometrician perfectly observes individuals' information sets.

In contrast, I exploit the envelope theorem to characterize the derivative of $W(s)$ at each $s$, and use this to derive the minimal sufficient statistics required to measure the welfare impact of changes in the size of the insurance market.<sup>24</sup> Differentiating equation (9) with respect to $s$ yields

$$-W'(s) * \frac{\partial V}{\partial p_U}(\infty, -W(s)) = \frac{d}{ds} E \left[ \sum_{t \geq 1} v_t(p_I(s), p_U(s); \theta_t) \right]$$
$$= E \left[ \mathbb{1} \{ S(p_U(s), \theta_\mu) < s \} \frac{\partial u_\nu}{\partial c} (-p'_I(s)) + \mathbb{1} \{ S(p_U(s), \theta_\mu) \geq s \} \frac{\partial u_\nu}{\partial c} (-p'_U(s)) \bigg| \theta_\mu \in M \right], \tag{10}$$

where $\mathbb{1} \{ S(p_U, \theta_\mu) < s \}$ is an indicator for the event that an individual of type $\theta_\mu$ purchases insurance when a fraction $s$ of the market is insured and the uninsured pay $p_U$.<sup>25</sup> The second line invokes the envelope theorem: because prices $p_U$ and $p_I$ only affect the budget constraint in period $t = \nu$, the impact of expanding $s$ only affects ex ante utility through its mechanical effect on consumption in period $\nu$ as if individuals do not change their choices.<sup>26</sup> The key insight in equation 10 is that the price changes, $p'_I(s)$ and $p'_U(s)$, for the insured and uninsured are weighted by individual's marginal utilities of consumption, $\frac{\partial u_\nu}{\partial c}$. Transfers between insured and uninsured have value from an ex ante perspective to the extent to which they help move resources from

\*23. I write $p_I = \infty$ but formally the RHS of equation (9) can be written as $V(\bar{c}, -W(s))$ where $\bar{c}$ is defined in Footnote 10.

\*24. Because of the convexity of the constraints and differentiability of the utility function, the environment satisfies the assumptions outlined in Milgrom and Segal (2002) so that the envelope theorem holds. Supplementary Appendix A.2 shows this explicitly for the more general case that allows for savings.

\*25. The derivatives $\frac{\partial u_\nu}{\partial c}$ exist by assumption and the differentiability of the prices $p_I(s)$ and $p_U(s)$ follows from the differentiability of demand and total cost as a function of $s$.

\*26. Supplementary Appendix A.2 shows that equation (10) continues to hold even in a more general model that allows for endogenous savings decisions. Because of the envelope theorem, endogenous savings responses to changes in prices do not have first order impacts on $V$.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1206 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1207

states of the world with low marginal utilities of income to states of the world with high marginal utilities of income.

Proposition 1 provides a characterization of $W'(s)$ for all $s$ that illustrates how ex ante welfare depends crucially on the percentage difference in marginal utilities of consumption between the insured and uninsured.

**Proposition 1** *For budget neutral policies, the marginal welfare impact of expanding the size of the insurance market from $s$ to $s + ds$ is given by*

$$W'(s) = D(p_U(s), s) - C(s) + EA(s) + \delta_p(s), \tag{11}$$

*where $EA(s)$ is the additional ex ante value of expanding the size of the insurance market,*

$$EA(s) = s(1 - s) \left( -s \frac{\partial D}{\partial s} \right) \beta(s). \tag{12}$$

*$\beta(s)$ is the percentage difference in marginal utilities of income for the insured relative to the uninsured,*

$$\beta(s) = \frac{E \left[ \frac{\partial u_\nu}{\partial c} \middle| S(p_U(s), \theta_\mu) < s \right] - E \left[ \frac{\partial u_\nu}{\partial c} \middle| S(p_U(s), \theta_\mu) \geq s \right]}{E \left[ \frac{\partial u_\nu}{\partial c} \middle| \theta_\mu \in M \right]} \tag{13}$$

*and $\delta_p(s)$ is an adjustment for income effects. $\delta_p(s)$ is continuously differentiable in $s$ and $\delta_p(0) = 0$. If the utility function satisfies no income effects, then $\delta_p(s) = 0$ for all $s$.*

*Proof.* See Supplementary Appendix A. $\square$

Proposition 1 shows that the marginal welfare impact of expanding the size of the insurance market is given by the sum of four terms. The first two terms, $D(p_U(s), s) - C(s)$, in equation (11) correspond to traditional market surplus: expanding the size of the insurance market increases ex ante welfare to the extent to which individuals are willing to pay more than their costs for insurance. $EA(s)$ captures the additional ex ante value of expanding the size of the market through its impact on insurance prices. As in Section 2, expanding the insurance market induces a transfer from uninsured to insured of size $(1 - s) \left( -s \frac{\partial D}{\partial s} \right)$. The transfer is valued according to the difference in marginal utilities between the insured and uninsured, $\beta(s)$. The final term, $\delta_p(s)$, is an adjustment for the presence of income effects that leads to differences between equivalent variation and consumer surplus measures of welfare. This adjustment is equal to zero whenever the utility function satisfies no income effects. More generally, $\delta_p(0) = 0$ and $\delta_p(s)$ is continuously differentiable in $s$.<sup>27</sup>

**Sign of $\beta(s)$** Canonical models of insurance predict that $\beta(s) > 0$. This is because those who choose not to purchase insurance expect to have out of pocket medical spending that is lower than the marginal price of insurance, which implies that the consumption of the insured are

27. In Section 7.4.2, I consider a utility function that does not satisfy the no income effects assumption and the approximations shown in Figure 8 reveal that ignoring $\delta_p(s)$ provides a good measure of $W'(s)$. Moreover, the utility function in Section 2 did not satisfy the no income effects assumption, but nonetheless recovered ex ante utility quite well as shown in the numerical example in Figure 2.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1207 1193–1223

1208 REVIEW OF ECONOMIC STUDIES

Copyedited by: ES MANUSCRIPT CATEGORY: Article



lower than those of the uninsured. Concavity of the utility function then implies that the marginal utilities of the insured are higher than the uninsured, so that $\beta(s)>0$.

But, it is possible to have $\beta(s)<0$. For example, in the presence of advantageous selection whereby the insured are healthier than the uninsured, then it could be that the uninsured have a higher marginal utility of income than the insured, $\beta(s)<0$. Or, if liquidity effects were a driver of insurance purchase so that the uninsured choose to forego both medical spending and the purchase of insurance, it could be that the uninsured have a higher marginal utility of income. In these cases, expanding the size of the insurance market will transfer resources from the liquidity constrained to those who are less constrained, which would suggest that $EA(s)<0$.

## 4.2. Non-budget neutral policies: the MVPF

In many cases including the example in Section 6, insurance subsidies are redistributive: they are financed by those not in the insurance market (i.e., $\theta_{\mu} \notin M$), as opposed to being financed by higher prices to the uninsured in the market, $p_{U}$. This section asks whether health insurance subsidies are an efficient method of redistribution.

To do so, I construct the MVPF for lower insurance prices. The MVPF equals the ratio of individuals’ willingness to pay for higher subsidies normalized by the net cost to the government of increasing subsidies,

$$ \text{MVPF} = \frac{\text{Marginal WTP}}{\text{Marginal Govt Cost}}. $$

For every $1 of net government spending, the policy delivers MVPF dollars of welfare to the beneficiaries in units of their own willingness to pay. Hendren and Sprung-Keyser (2020) provide a library of MVPF estimates that allow one to compare across non-budget neutral policies. This means that one can assess whether the health insurance subsidies are an efficient method of redistribution by comparing the MVPF of lower health insurance prices to the MVPF of other policies that spend resources on low-income populations, such as the Earned Income Tax Credit (EITC).

I construct the MVPF for lower insurance prices in an environment where the uninsured pay no prices, $p_{U} = 0$. This means that the size of the insurance market, $s$, is given by the solution to $p_{I}(s) = D(0, s)$. The cost function, $C(s)$, is then given by the derivative of total costs as the size of the market expands, $C(s) = \frac{d}{ds} [sAC(0, s)]$. And, the net cost to the government is given by the difference between total costs and premiums collected, $sAC(0, s) - sp_{I}(s)$. Differentiating, this yields the marginal cost to the government of expanding the market:

$$ \begin{aligned} \text{Marginal Govt Cost} &= \frac{d}{ds} [sAC(s) - sp_{I}(s)] \\ &= -s \frac{\partial D}{\partial s} (0, s) + C(s) - D(0, s). \end{aligned} $$

The marginal cost to the government has two components. First, there is the mechanical cost of the lower prices $-sp'_{I}(s) = -s \frac{\partial D}{\partial s} (0, s)$. In addition, there is a cost (or benefit) from those induced to purchase insurance. Insuring these individuals increases costs by $C(s)$, from which we subtract the prices they pay, $p_{I}(s) = D(0, s)$.

To measure WTP, one would traditionally use the observed average willingness to pay for those in the market. A fraction $s$ of the market receives a price change of $-p'_{I}(s)$ so that the

marginal WTP is $-sp'_{I}(s)$. This would imply an MVPF of $\frac{-s \frac{\partial D}{\partial s} (0, s)}{-s \frac{\partial D}{\partial s} (0, s) + C(s) - D(0, s)} = \frac{1}{1 + \frac{C(s) - D(0, s)}{s \left( -\frac{\partial D}{\partial s} (0, s) \right)}}$.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1208 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1209

The MVPF exceeds 1 to the extent to which the marginal enrolees are willing to pay more than their marginal cost.<sup>28</sup>

But, this traditional approach ignores the ex ante value individuals obtain from having lower insurance prices. To measure ex ante marginal WTP, let $\tilde{W}(\tilde{s}, s)$ denote the ex ante willingness to pay for having a fraction $\tilde{s}$ of the market insured as opposed to having a fraction $s$ of the market insured. This is given by the solution to $V(p_I(\tilde{s}), 0) = V(p_I(s) - \tilde{W}(\tilde{s}, s), -\tilde{W}(\tilde{s}, s))$ so that individuals are indifferent to having $\tilde{s}$ insured and having $s$ insured but receive a transfer of $\tilde{W}(\tilde{s}, s)$ regardless of whether they are insured.<sup>29</sup> The ex ante marginal WTP is then given by $\frac{\partial \tilde{W}}{\partial \tilde{s}}|_{\tilde{s}=s}$. Proposition 2 shows that one can measure ex ante WTP by adding $(1-s)\beta(s)$ into the numerator.

**Proposition 2** *If $\frac{C(s)-D(0,s)}{s(-\frac{\partial D}{\partial s}(0,s))} > -1$, the MVPF of non-budget neutral reduction in $p_I$ combined with $p_U = 0$ when a fraction $s$ of the market is insured is given by*

$$ \text{MVPF}(s) \equiv \frac{\frac{\partial \tilde{W}}{\partial \tilde{s}}|_{\tilde{s}=s}}{\frac{d}{ds} [s p_I(s) - s AC(s)]} = \frac{1 + (1-s)\beta(s)}{1 + \frac{C(s)-D(0,s)}{s(-\frac{\partial D}{\partial s}(0,s))}}, \tag{14} $$

*where $\beta(s)$ is the percentage difference in marginal utilities of income for the insured relative to the uninsured given by equation (13). If $\frac{C(s)-D(0,s)}{s(-\frac{\partial D}{\partial s}(0,s))} \leq -1$, the MVPF is infinite, as lower insurance prices generate a Pareto improvement.*

**Proof.** See Supplementary Appendix B. $\square$

Proposition 2 shows that the ex ante marginal WTP differs from the observed willingness to pay by a factor of $1 + \beta(s)(1-s)$. The $\beta(s)(1-s)$ captures the additional markup that individuals are willing to pay for the ability to purchase insurance at lower prices.

# 5. IMPLEMENTATION

The key additional parameter required to construct measures of ex ante welfare is the percentage difference in marginal utilities between insured and uninsured, $\beta(s)$. This section provides a method for estimating $\beta(s)$ using the market demand curve, $D(p_U, s)$, combined with a measure of risk aversion. This provides a benchmark method for measuring ex ante welfare without needing to specify a utility function or the information sets of individuals in the economy. The implementation assumptions are not without loss of generality. To assess the quality of the fit and impact of violating these assumptions, I compare the estimates to those from a fully specified structural model in Section 7.

## 5.1. Estimating $\beta(s)$ using market demand and cost curves

To provide a method for estimating $\beta(s)$, I draw upon assumptions commonly used in the literature on optimal unemployment insurance. In particular, I begin by assuming that that the marginal utility of consumption depends only on $c$, not $m$ and $\theta \nu$.

\*28. When $\frac{C(s)-D(0,s)}{s(-\frac{\partial D}{\partial s}(0,s))} < -1$, expanding the size of the market actually leads to lower prices for the insured, which makes everyone better off. This case when benefits are positive and costs are negative is defined as an infinite MVPF in Hendren and Sprung-Keyser (2020). It implies that lower insurance prices would generate a Pareto improvement.

\*29. Note that $W(s)$ is well-defined because utility is strictly increasing in consumption.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1209 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

1210 REVIEW OF ECONOMIC STUDIES

**Assumption 1** *The marginal utility of consumption, $u_c$, depends only on the level of an individual’s consumption, $c$, so that there exists a function $f$ such that*

$$\frac{\partial u_\nu}{\partial c}(c, m; \theta_\nu) = f(c).$$

Assumption 1 implies that if one can observe the level of an individual’s consumption, then one can infer their marginal utility of income. This is a common assumption imposed in the literature on optimal unemployment insurance (e.g., Baily, 1978; Chetty, 2006), but this assumption is not without loss of generality. Most notably, it assumes away preference heterogeneity that is correlated with the marginal utility of income. I discuss violations of this assumption in Section 7.

Proposition 3 shows that when Assumption 1 holds, $\beta(s)$ can be written as the coefficient of absolute risk aversion multiplied by the average difference in consumption between the insured and uninsured.

**Proposition 3** *Suppose Assumption 1 holds. Then, $\beta(s)$ can be written as*

$$\beta(s) \approx \gamma \Delta c(s), \tag{15}$$

where “$\approx$” represents equality to a first-order Taylor approximation to $f(c)$,

$$\Delta c(s) = E[c_\nu(p_I(s), p_U(s), \theta_\nu) | S(p_U(s), \theta_\mu) \geq s] - E[c_\nu(p_I(s), p_U(s), \theta_\nu) | S(p_U(s), \theta_\mu) < s]$$

is the difference in consumption between the uninsured and insured, and $\gamma = \frac{-f'(E[c])}{f(E[c])}$ is the coefficient of absolute risk aversion ($-\frac{\frac{\partial^2 u_\nu}{\partial c^2}}{\frac{\partial u_\nu}{\partial c}}$) evaluated at the population average level of consumption, $E[c]$.

*Proof.* See Supplementary Appendix C. $\square$

Using consumption differences combined with a measure of risk aversion is analogous to the methods used to measure the welfare impact of unemployment insurance (Baily, 1978; Chetty, 2006). But, in practice, consumption is rarely observed. To provide an implementation method without consumption data, I make the additional assumption that incomes do not vary systematically between the uninsured and insured.

**Assumption 2** *No differences in average liquidity/income between the insured and uninsured,*

$$E[y_\nu(\theta_\nu) | S(p_U(s), \theta_\mu) \geq s] = E[y_\nu(\theta_\nu) | S(p_U(s), \theta_\mu) \leq s] \quad \forall s.$$

Assumption 2 is not without loss of generality. I return to a discussion of how one can use consumption data to relax Assumption 2 in Section 7.4.1 below.<sup>30</sup> The key advantage of Assumption 2 is that it allows the difference in market demand between the insured and uninsured to proxy for their differences in consumption, as in the stylized model in Section 2.

30. Assumption 2 will be a natural assumption in contexts like the MA health insurance subsidies for those with incomes at exactly 150% FPL. But, in many contexts this assumption may be violated.
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1210 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1211

**Proposition 4** *Suppose Assumptions 1 and 2 hold, suppose the utility function has no income effects. Then, the percentage difference in marginal utilities is given by*

$$ \beta(s) \approx \gamma (\Delta D(s) + \Delta x(s)), \tag{16} $$

where "$\approx$" represents equality to a first-order Taylor approximation to $f(c)$, $\Delta D(s)$ and $\Delta x(s)$ are given by:

$$ \begin{aligned} \Delta D(s) &= D(s) - E[D(S(\theta_\mu)) | S(\theta_\mu) \geq s] \\ \Delta x(s) &= E[x(m_\nu^I(\theta_\nu)) | S(\theta_\mu) < s] - E[x(m_\nu^I(\theta_\nu)) | S(\theta_\mu) \geq s] \end{aligned} $$

and $\gamma = \frac{-f'(E[c])}{f(E[c])}$ is the coefficient of absolute risk aversion. Moreover, for full insurance contracts (i.e., $x(m) = 0$), the Taylor approximation to $\beta(s)$ is given by

$$ \beta(s) \approx \gamma \Delta D(s). $$

*Proof.* See Supplementary Appendix C. $\square$

As in Section 2, the difference between the marginal willingness to pay and the average willingness to pay for the uninsured types can proxy for their difference in consumption. For the general case when $x(m) \neq 0$, one also needs to adjust for any difference in out of pocket spending between the insured and uninsured that would occur in a world where everyone made the choice of $m$ as if they were insured.

Proposition 4 provides a method of estimating ex ante willingness to pay using the demand curve, combined with a measure of risk aversion. The estimate of risk aversion can either be imported from external settings, or it can be estimated internally using the relationship between the markup individuals are willing to pay and the reduction in consumption variance provided by the insurance, as discussed in Supplementary Appendix D. The next section takes this approach to the data.

## 6. APPLICATION TO MA HEALTH INSURANCE SUBSIDIES

I apply the approach to study the optimal health insurance subsidies and mandates in the subsidized insurance marketplace for low-income adults in Massachusetts, Commonwealth Care. Developed as part of the 2006 Massachusetts health insurance reform, it later became a model for the health insurance exchanges for low-income adults constructed in the Affordable Care Act. The marketplace provides subsidies to low-income individuals who are not eligible for Medicaid and do not have access to employer-provided insurance. Finkelstein et al. (2019) provide further details. Importantly for the modelling purposes, these contracts involved virtually no cost sharing, $x(m) = 0$.

Using administrative data from Massachusetts, Finkelstein et al. (2019) exploit discontinuities in the health insurance subsidy schedule to estimate willingness to pay and cost curves. I focus here on the baseline estimates from Finkelstein et al. (2019), which use the empirical discontinuities in 2011 to measure $D(s)$ and $C(s)$, for those with incomes at 150% FPL.<sup>31</sup> In the language of Section 3, this means that the set of people eligible for the market, $M$, corresponds to individuals with incomes at 150% FPL, $\mu$ is the open enrolment period in the fall of 2010, and $\nu = 2011$.



\*31. 150% FPL corresponds to roughly $16K in income for an individual with no children.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1211 1193–1223

1212 REVIEW OF ECONOMIC STUDIES

A Willingness to Pay and Cost Curves
<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>D(s)</th>
        <th>C(s)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>3000</td>
        <td>2100</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>2400</td>
        <td>1900</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>1800</td>
        <td>1700</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>1200</td>
        <td>1300</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>600</td>
        <td>900</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0</td>
        <td>500</td>
    </tr>
  </tbody>
</table>



B Market Surplus
<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>D(s)</th>
        <th>C(s)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>3000</td>
        <td>2100</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>2400</td>
        <td>1900</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>1800</td>
        <td>1700</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>1200</td>
        <td>1300</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>600</td>
        <td>900</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0</td>
        <td>500</td>
    </tr>
  </tbody>
</table>


FIGURE 3
Willingness to pay and cost for health insurance for adults with incomes at 150% of federal poverty line in MA

Figure 3A presents the results for $D(s)$ and $C(s)$ in Finkelstein et al. (2019) plotted as a function of $s$.<sup>32</sup> The patterns reveal that those with the highest willingness to pay (low values of $s$) are willing to pay more than their marginal cost for insurance, $D(s) > C(s)$.<sup>33</sup> But those with the lowest willingness to pay are willing to pay less than the cost of their insurance. The model in Section (3) captures $D(s) < C(s)$ by allowing part of the cost to be driven by moral hazard: when $x'(m) < 1$, insured individuals may choose medical services that they don't value at their full resource cost.<sup>34</sup>

Finkelstein et al. (2019) show that this market would fully unravel without subsidies, so that no one obtains insurance ($s = 0$). This unravelling is the result of both adverse selection and uncompensated care externalities. Uncompensated care externalities arise in this environment because the total cost to a private insurer would not only include the average resource cost of those insured, $C(s)$, but also the cost of care that would have otherwise been provided through uncompensated care programs. Private insurance would have to pay these additional costs, which leads the average cost faced by the private insurer to lie everywhere above the demand curve. This generates a full unravelling of the market in the absence of subsidies.

The goal of this section is to evaluate the ex ante welfare impacts of subsidies and mandates in this market and compare the conclusions with a more traditional analysis of market surplus. I begin with the welfare impact of increasing subsidies for insurance if they are financed by increasing prices/penalties on the uninsured.

## 6.1. Budget neutral policies

Figure 3B shows that market surplus is maximized when $s = 41\%$ of the market is insured and the marginal price for insurance is $1,581. The market surplus from insuring 41% of the market

\*32. The estimates from Finkelstein et al. (2019) correspond to those that assume the government is the payer of uncompensated care, and they are scaled by a factor of 12 to correspond to yearly values.

\*33. Finkelstein et al. (2019) do not report the joint sampling distribution for $D(s)$ and $C(s)$, which prevents a formal construction of standard errors on the estimates I provide below. But, they test whether the negative slopes for $D(s)$ and $C(s)$ are statistically significant. They find high t-stats of 6.1 (47.3/7.7) at 150% FPL for the cost curve and 13.2 (1735/131) for the demand curve. Equation 3 shows that testing whether $EA(s)$ is positive is equivalent to testing whether $D(s)$ slopes downward. Therefore, the appropriate t-stat for testing $EA(s) > 0$ is 13.2.

\*34. In contrast, this moral hazard response is assumed not to exist in the stylized model of Section 2 and in other previous models studying reclassification risk and notions of ex ante expected utility (e.g., Handel et al., 2015; Einav et al., 2016).

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1213
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

### A Measuring Ex-Ante WTP
<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>D(s)+EA(s)</th>
        <th>C(s)</th>
        <th>D(s)</th>
        <th>EA(s)</th>
        <th>s</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>3000</td>
        <td>2100</td>
        <td>2100</td>
        <td> </td>
        <td>0</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>2600</td>
        <td>1850</td>
        <td>1750</td>
        <td> </td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>2000</td>
        <td>1600</td>
        <td>1400</td>
        <td> </td>
        <td>0.4</td>
    </tr>
    <tr>
        <td>0.5</td>
        <td>1523</td>
        <td>1439</td>
        <td>1232</td>
        <td>291</td>
        <td>0.5</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>1100</td>
        <td>1300</td>
        <td>1000</td>
        <td> </td>
        <td>0.6</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>500</td>
        <td>1000</td>
        <td>500</td>
        <td> </td>
        <td>0.8</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0</td>
        <td>700</td>
        <td>0</td>
        <td> </td>
        <td>1</td>
    </tr>
  </tbody>
</table>



### B Expected-Utility Maximizing Market Size
<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>D(s)+EA(s)</th>
        <th>C(s)</th>
        <th>D(s)</th>
        <th>Annotation</th>
        <th>s</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>3000</td>
        <td>2100</td>
        <td>2100</td>
        <td>$339</td>
        <td>0</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>2600</td>
        <td>1850</td>
        <td>1750</td>
        <td> </td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>2000</td>
        <td>1600</td>
        <td>1400</td>
        <td> </td>
        <td>0.4</td>
    </tr>
    <tr>
        <td>0.54</td>
        <td>1117</td>
        <td>1117</td>
        <td>850</td>
        <td>$Pea = $1,117</td>
        <td>0.54</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>1100</td>
        <td>1300</td>
        <td>1000</td>
        <td> </td>
        <td>0.6</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>500</td>
        <td>1000</td>
        <td>500</td>
        <td>$170</td>
        <td>0.8</td>
    </tr>
    <tr>
        <td>1</td>
        <td>0</td>
        <td>700</td>
        <td>0</td>
        <td> </td>
        <td>1</td>
    </tr>
  </tbody>
</table>


**FIGURE 4**
ex ante welfare of health insurance for low-income adults

is $182. But, expanding coverage beyond this would lower market surplus since those with $D(s) < 1581$ are not willing to pay the cost they impose on insurers. On net, mandates would lower total market surplus by $45.

What insurance prices maximize ex ante welfare? To measure this, one requires an estimate of risk aversion. For the baseline case, I take a common estimate from the health insurance literature of $\gamma = 5 \times 10^{-4}$ (e.g., similar to estimates in Handel et al. (2015)).<sup>35</sup> Table 1 presents estimates for a range of alternative risk aversion coefficients.

Figure 4 presents the ex ante demand curve, $D(s) + EA(s)$, using equation (16). Figure 4A illustrates the calculation of $EA(s)$ when 50% of the population owns insurance. The cost of the marginal enrolee is given by $C(0.5) = 1438$, willingness to pay is $D(0.5) = 1232$, and the slope of willingness to pay of the marginal enrolee is $D'(0.5) = -3405$.<sup>36</sup> The average $D(s)$ for those with $s > 0.5$ is 548. Equation (16) implies that the ex ante willingness to pay for a larger insurance market is

$$
\begin{aligned}
EA(s) &= (1-s)s(-D'(s))\gamma(D(s) - E[D(S)|S \geq s]) \\
&= 0.5(0.5 * 3405)\left(5 \times 10^{-4}\right)(1232 - 548) \\
&= 291.
\end{aligned}
$$

Individuals with median (0.5) levels of $D(s)$ are willing to pay $1,232 for insurance at the time the econometrician observes them in the market. But, from behind a veil of ignorance before knowing $D(s)$, everyone would have been willing to pay $2.91 to have the opportunity to purchase insurance at the prices that lead to 51% of the market insured instead of 50% of the market insured ($291 * (0.51 - 0.5)$).

Ex ante welfare is maximized when $W'(s) = 0$, or $D(s) + EA(s) = C(s)$. This occurs when 54% of the market owns insurance and the marginal price of insurance is $1,117, as shown in Figure 4B. This contrasts with the market surplus-maximizing size of the market of 41%. The ex ante optimal price is roughly 30% lower than the surplus-maximizing price of $1,580.

35. Handel et al. (2015) estimate this risk aversion coefficient for a relatively middle to high income population making choices over insurance plans. Under the natural assumption that absolute risk aversion decreases in consumption levels, this estimate is likely a lower bound on the size $\gamma$.

36. Finkelstein et al. (2019) estimate a piece-wise linear demand cure. To obtain smooth estimates of the slope of demand, I regress the estimates of $D(s)$ from Finkelstein et al. (2019) on a 10th-order polynomial in $s$. The results are similar for other smoothed functions.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1213 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article
1214 REVIEW OF ECONOMIC STUDIES

# TABLE 1
Alternative risk aversion specifications


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Market</th>
        <th>Baseline:</th>
        <th colspan="2">CARA coefficient</th>
        <th colspan="6">CRRA coefficient (150% FPL)</th>
    </tr>
    <tr>
        <th> </th>
        <th>surplus</th>
        <th>CARA = 5 × 10⁻⁴</th>
        <th>1 × 10⁻⁴</th>
        <th>10 × 10⁻⁴</th>
        <th>1</th>
        <th>2</th>
        <th>3</th>
        <th>4</th>
        <th>5</th>
        <th>10</th>
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
        <th>(9)</th>
        <th>(10)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Optimal market size</td>
        <td>0.41</td>
        <td>0.54</td>
        <td>0.45</td>
        <td>0.62</td>
        <td>0.43</td>
        <td>0.45</td>
        <td>0.47</td>
        <td>0.48</td>
        <td>0.49</td>
        <td>0.56</td>
    </tr>
    <tr>
        <td>Optimal price of insurance</td>
        <td>1581</td>
        <td>1117</td>
        <td>1432</td>
        <td>888</td>
        <td>1509</td>
        <td>1432</td>
        <td>1351</td>
        <td>1311</td>
        <td>1270</td>
        <td>1060</td>
    </tr>
    <tr>
        <td>WTP for optimal market size</td>
        <td>182</td>
        <td>339</td>
        <td>210</td>
        <td>525</td>
        <td>199</td>
        <td>217</td>
        <td>235</td>
        <td>254</td>
        <td>274</td>
        <td>379</td>
    </tr>
    <tr>
        <td>WTP for mandate</td>
        <td>−45</td>
        <td>169</td>
        <td>−2</td>
        <td>383</td>
        <td>−19</td>
        <td>7</td>
        <td>34</td>
        <td>60</td>
        <td>86</td>
        <td>217</td>
    </tr>
    <tr>
        <td>MVPF when 30% insured</td>
        <td>1.28</td>
        <td>1.79</td>
        <td>1.38</td>
        <td>2.29</td>
        <td>1.34</td>
        <td>1.41</td>
        <td>1.47</td>
        <td>1.53</td>
        <td>1.59</td>
        <td>1.90</td>
    </tr>
    <tr>
        <td>MVPF when 90% insured</td>
        <td>0.80</td>
        <td>0.81</td>
        <td>0.80</td>
        <td>0.81</td>
        <td>0.80</td>
        <td>0.80</td>
        <td>0.80</td>
        <td>0.80</td>
        <td>0.80</td>
        <td>0.81</td>
    </tr>
  </tbody>
</table>


*Notes:* This table presents the welfare estimates under alternative risk aversion coefficients. Column (1) presents market surplus estimates and column (2) presents the baseline estimates that use a coefficient of absolute risk aversion of $5 \times 10^{-4}$. Columns (3) and (4) use alternative coefficients of absolute risk aversion of $1 \times 10^{-4}$ and $10 \times 10^{-4}$. Columns (5)–(9) construct the ex ante welfare measures using coefficients of relative risk aversion ranging from 1 to 5 and column (10) considers a coefficient of relative risk aversion of 10. The coefficient of relative risk aversion specifications assume a consumption level of 150% FPL for singles in 2011, which corresponds to $\$10,890 \times 150\% = \$16,335$.

The ex ante welfare gain from insuring $s = 54\%$ of the market is large. Everyone would be willing to contribute $340 per person if they could live in a world in which insurance prices set at $p = \$1,117$ as opposed to having no one obtain insurance. This $340 is much larger than the loss of market surplus of $182 shown in Figure 4. The ex ante welfare cost of insuring the remaining 46% of the market is $170. This means that imposing a mandate would generate lower ex ante welfare than the optimal price of $1,117, but individuals would prefer a mandate relative to no insurance. Prior to learning their willingness to pay, individuals would pay an average of $169 per person to have a mandate instead of having no insurance. Mandates increase ex ante expected utility, but decrease market surplus.

**Alternative risk aversion values** Table 1 presents estimates of the above results for alternative risk aversion measures. Columns (1) and (2) present the market surplus and baseline ex ante welfare estimates for $\gamma = 5 \times 10^{-4}$. Columns (3) and (4) consider alternative coefficients of absolute risk aversion of $1 \times 10^{-4}$ and $10 \times 10^{-4}$ and columns (5)–(10) consider alternative coefficients of relative risk aversion ranging from 1 to 10.<sup>37</sup>

The baseline specification of $\gamma = 5 \times 10^{-4}$ is consistent with the estimates in Handel et al. (2015), but it implies a large coefficient of relative risk aversion of 8.2. Table 1 shows that a coefficient of relative risk aversion of 3 implies that the optimal insurance prices are $1,351, which is 15% lower than the optimal price from a market surplus perspective of $1,581. Such prices would lead to 47% of the market insured, which is less than the 54% of the market that would be insured if prices were set to maximize ex ante welfare in the baseline specification.

Although the precise optimal size of the market varies with the coefficient of risk aversion, the conclusion that mandates increase ex ante welfare remains fairly robust across specifications. Mandates increase ex ante expected utility as long as the coefficient of absolute risk aversion is above $1.05 \times 10^{-4}$—or, equivalently, coefficients of relative risk aversion above 1.7. This means that for a range of plausible coefficients of risk aversion, an ex ante welfare perspective leads to different normative conclusions about the optimal insurance subsidies and desirability of mandates.

\*37. To translate the coefficient of relative risk aversion into a coefficient of absolute risk aversion I multiply by $10,890 \times 1.5$, where 10,890 is the FPL for single adults.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1214 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1215
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>Market-Surplus MVPF</th>
        <th>Ex-Ante MVPF</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>30% Insured</td>
        <td>1.28</td>
        <td>1.79</td>
    </tr>
    <tr>
        <td>90% Insured</td>
        <td>0.80</td>
        <td>0.81</td>
    </tr>
  </tbody>
</table>


FIGURE 5
MVPF for health insurance subsidies for low-income adults

## 6.2. Non-budget neutral policies

The insurance subsidies in Massachusetts are not paid by low-income individuals at 150% FPL choosing to forego insurance, but by taxpayers at other income levels. The subsidies are a method of redistribution. To compare the welfare impact of these subsidies to other forms of redistribution such as tax credits, I construct the MVPF as described in Section 4.2. This is given by the formula:

$$ MVPF(s) = \frac{1 + (1 - s)\beta(s)}{1 + \frac{C(s) - D(s)}{s(-D'(s))}}, $$

where $\beta(s) = \gamma (D(s) - E[D(S) | S \geq s])$ is the difference in marginal utilities between the insured and uninsured.

Figure 5 presents the MVPF for the case when 30% and 90% of the market have insurance. When 30% of the market is insured, annual costs are given by $C(0.3) = 1,738$, willingness to pay is given by $D(0.3) = 1,978$, and the slope of willingness to pay is given by $D'(0.3) = -3,610$. The average willingness to pay for those with $s \geq 0.3$ is 853. Therefore, the MVPF is given by

$$ \begin{aligned} MVPF(0.3) &= \frac{1}{1 - \frac{1,978 - 1,738}{0.3 * 3,654}} \left( 1 + 0.7 * 5 \times 10^{-4} * (1,978 - 853) \right) \\ &= 1.282 * 1.394 = 1.79. \end{aligned} $$

Every $1 of subsidy generates $1.28 lower prices for the insured. This is greater than $1 because the marginal types that are induced to enrol from lower prices have a lower cost of being insured, $D(0.3) > C(0.3)$. Using observed WTP would imply an MVPF of 1.28, as shown in the left bar in Figure 5. Behind the veil of ignorance, individuals are willing to pay a 39.4% markup to have the ability to purchase insurance at lower prices. This means that from an ex ante perspective, individuals would be willing to pay $1.79 for every $1 of government spending on insurance subsidies.

For comparison, the MVPF of low-income tax cuts, such as expansions of the Earned Income Tax Credit (EITC) have MVPFs ranging between 0.9 and 1.3 (Hendren and Sprung-Keyser, 2020).

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1215 1193–1223

1216 REVIEW OF ECONOMIC STUDIES
Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

This suggests expanded insurance subsidies financed by a budget-neutral reduction in EITC would increase ex ante welfare when $s=0.3$.<sup>38</sup>

In contrast, the MVPFs are lower when prices are more heavily subsidized so that more of the market has insurance. When $s=0.9$, the willingness to pay of the marginal type is below her cost, $D(s) < C(s)$, so that $\frac{1}{1+\frac{C(s)-D(s)}{s(-D'(s))}} = 0.8$. And, the ex ante value of having marginally lower premiums

is smaller because insurance premiums are already low ($D(s)$ is similar to $E[D(S)|S \geq s]$ when $s$ is close to 1). Comparing this to the MVPF for the EITC, this suggests that subsidies leading to 90% of the market being insured are too generous: reducing health insurance subsidies and using the resources to expand tax credits to those with incomes near 150% FPL would increase ex ante welfare.

# 7. COMPARISON TO STRUCTURAL MODEL

The most common approach to measuring ex ante welfare estimates a structural model. If one knows the utility function, information sets, and distribution of outcomes, one can recover measures of ex ante expected utility.

In this section, I fit the estimated willingness to pay and cost curves to a fully specified structural model. The model is parsimonious but flexible enough to perfectly match the estimated willingness to pay and cost curves in Finkelstein et al. (2019). I first use the model to validate the sufficient statistics approach relative to this structural benchmark. Then, I use extensions of the model to evaluate violations of Assumptions 1 and 2.

## 7.1. Setup

The structural model follows the environment developed in Einav et al. (2013) that allows for both adverse selection and moral hazard.<sup>39</sup> In period $\nu$, individuals obtain a realization of $\theta_{\nu}$ and choose $c$ and $m$ to maximize utility, which is given by:

$$ u(c, m; \theta_{\nu}) = -\frac{1}{\gamma} e^{-\gamma \left[ (m - \lambda(\theta_{\nu})) - \frac{1}{2w} (m - \lambda(\theta_{\nu}))^2 + c(m) \right]}, \tag{17} $$

where $c(m) = y - p_I$ if the individual is insured and $c(m) = y - m - p_U$ if the individual is uninsured (recall $x(m) = 0$ for the MA setting). The realization of $\theta_{\nu}$ affects utility through the function $\lambda(\theta_{\nu})$, which determines an individual's demand for medical spending. To see this, note that the first order conditions for $m$ imply $m^I = \lambda + w$ and $m^U = \lambda$. This means that $\lambda(\theta_{\nu})$ is the baseline demand for medical spending by the uninsured who pay its full cost and $w$ is the causal effect of insurance on the individual's medical spending.<sup>40</sup>

Plugging in the choices of individuals, the utility functions for type $\theta_{\nu}$ are given by $-\frac{1}{\gamma} e^{-\gamma [y - \lambda(\theta_{\nu}) - p_U]}$ if they are uninsured. The realized utility for the insured is given by $-\frac{1}{\gamma} e^{-\gamma [\frac{1}{2}w + y - p_I]}$.



\*38. Table 1 shows that with an alternative specification of a coefficient of relative risk aversion of 3, the MVPF would be 1.47. This remains above the general values found for the EITC.

\*39. To my knowledge, no previous paper has evaluated the ex ante welfare impact of insurance market policies using a structural approach that includes moral hazard. Allowing for moral hazard is essential to match the fact that the demand curve lies below the cost curve for a broad range of the distribution, as shown in Figure 4.

\*40. I assume $w$, $y$, and $\gamma$ are constant across individuals. Allowing $w$ to be heterogeneous does not affect the results (conditional on matching the demand and cost curves). Section 7.4 explores the robustness of the results when one allows $y$ and $\gamma$ to vary across individuals.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1216 1193–1223

Copyedited by: ES    MANUSCRIPT CATEGORY: Article

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1217

## 7.2. Matching demand and cost curves

At the time of deciding whether to purchase insurance, individuals have some knowledge, given by θ<sub>μ</sub>, about their realization of λ(θ<sub>ν</sub>), which generates their marginal willingness to pay for insurance. As in Section 3, the population is ordered descending in their willingness to pay, given by D(S(θ<sub>μ</sub>)) where S(θ<sub>μ</sub>) = s corresponds to individuals with willingness to pay at the 1−s<sup>th</sup> quantile of the willingness to pay distribution. When a fraction s purchases insurance, all those with θ<sub>μ</sub> such that S(θ<sub>μ</sub>) < s will purchase insurance. The utility function in equation (17) satisfies the no income effects condition so that willingness to pay does not depend on p<sub>U</sub>.

The structural model requires the researcher to specify individuals’ beliefs about future realizations of θ<sub>ν</sub>. To do so, I assume that the realizations of λ(θ<sub>ν</sub>) are normally distributed with mean E[λ(θ<sub>ν</sub>)|s] = C(s) and variance Σ(s), where C(s) is the marginal cost curve in the economy and Σ(s) is a variance term. This variance will be set below to match the willingness to pay and cost curves. Both C(s) and Σ(s) are known to the individual at the time of insurance purchase, but they may vary for individuals with different levels of willingness to pay.

The utility function in equation (17) implies that the marginal willingness to pay for insurance solves:

$$D(s)=C(s)+\frac{w}{2}+\frac{\gamma \Sigma(s)}{2}.$$

Individuals are willing to pay their expected costs, C(s), plus half of the moral hazard induced spending, w/2, plus an additional term corresponding to the risk premium provided by insurance: to the extent to which the insurance reduces the variance of their consumption, Σ(s), the CARA utility structure implies that they value this reduction according to the risk aversion parameter, γ, divided by 2. The model matches heterogeneity in the markup (D(s)−C(s)) that individuals are willing to pay through heterogeneity in individuals’ belief variances, Σ(s).

I parameterize risk aversion to γ = 5 × 10<sup>−4</sup>. I set w to be equal to mean net costs of 1,336, which corresponds to roughly a 30% moral hazard effect on gross medical spending (roughly consistent with previous empirical findings). Given w, γ, and C(s), I set Σ(s) to be the unique value of Σ(s) that solves equation (18). This means that the model parameters perfectly match the reduced form cost curve, C(s), and willingness to pay curve, D(s), at each value of s. Moreover, the model structure satisfies the baseline implementation assumptions above.<sup>41</sup>

Finally, Proposition 5 shows that the structural model yields a solution for W(s) given by equation (9).

**Proposition 5**  *Under the modelling assumptions outlined above, W(s) in equation (9) is given by*

$$W(s)= -\frac{1}{\gamma}\left[\log\left(se^{\gamma p_I(s)e^{-\gamma w/2}}+(1-s)e^{\gamma p_U(s)}\int_s^1 e^{\gamma(C(\tilde{s})-w)+\frac{\gamma^2}{2}\Sigma(\tilde{s})}\,d\tilde{s}\right)-\log\left(\int_0^1\left[e^{\gamma(C(\tilde{s})-w)+\frac{\gamma^2}{2}\Sigma(\tilde{s})}\right]\,d\tilde{s}\right)\right]$$

Proof. See Supplementary Appendix F.

41. It is straightforward to show that the utility function satisfies the no income effects condition and Assumption 2. Moreover, the utility function does not exhibit complementarities between consumption and medical spending, u<sub>cm</sub> = 0, so that it satisfies Assumption 1 to first order.

1218 REVIEW OF ECONOMIC STUDIES

<table>
  <thead>
    <tr>
        <th>Fraction Insured</th>
        <th>Structural W'(s)</th>
        <th>Sufficient Statistics W'(s)</th>
        <th>Market Surplus D(s) - C(s)</th>
        <th>Optimal s (Structural)</th>
        <th>Optimal s (Sufficient Stats)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>900</td>
        <td>850</td>
        <td>750</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>850</td>
        <td>800</td>
        <td>400</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>450</td>
        <td>450</td>
        <td>0</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>0.41</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td>41%</td>
    </tr>
    <tr>
        <td>0.52</td>
        <td>0</td>
        <td> </td>
        <td> </td>
        <td>52%</td>
        <td> </td>
    </tr>
    <tr>
        <td>0.54</td>
        <td> </td>
        <td>0</td>
        <td> </td>
        <td> </td>
        <td>54%</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>-250</td>
        <td>-250</td>
        <td>-300</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>-500</td>
        <td>-500</td>
        <td>-500</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>1</td>
        <td>-650</td>
        <td>-650</td>
        <td>-650</td>
        <td> </td>
        <td> </td>
    </tr>
  </tbody>
</table>

FIGURE 6
Comparison to structural approach

## 7.3. Results

Figure 6 compares the value of $W'(s)$ from the structural model computed using Equation (19) (shown in the dash-dot brown line), to the value of $W'(s)$ computed using the sufficient statistics approach (in the solid red line).<sup>42</sup> The figure reveals that the sufficient statistics approach does a decent job of measuring the "true" ex ante measure of welfare implied by the structural model. The sufficient statistics correction clearly outperforms market surplus as a normative guide to ex ante welfare. Ex ante welfare is maximized in the structural model when $s = 52\%$ instead of $54\%$ in the sufficient statistics implementation (compared with $41\%$ for market surplus). The welfare gain from the optimal size of the insurance market is slightly larger in the structural approach ($350 versus $340), and the welfare gain from a full mandate is also slightly larger ($180 versus $170). Overall, the approach proposed in this article closely mirrors the ex ante welfare as measured in the structural model.

## 7.4. Violations of assumptions

While Assumptions 1 and 2 provide a benchmark method to measure $\beta(s)$, they are restrictive. Here, I use the structural model to explore the impact of violating these assumptions and discuss potential additional data elements that can recover $\beta(s)$ when these assumptions do not hold. Section 7.4.1 focuses on the impact of income or liquidity differences between the insured and uninsured, and Section 7.4.2 studies the implications of heterogeneity in risk aversion. In addition, Supplementary Appendix J discusses the implications of complementarities between the marginal utility of consumption and health status.

\*42. $W'(s)$ is computed using a numerical derivative. As shown in an earlier draft and available upon request, $W(s)$ is estimated to be smooth. Therefore, the results are robust to the methods used to compute this derivative.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1219

## 7.4.1. Income or liquidity differences.
If the insured have different income or liquidity than the uninsured, then their difference in willingness to pay for insurance will no longer proxy for their difference consumption.⁴³ In the MA example, the demand for insurance subsidies is estimated conditional on incomes of 150% FPL, which suggests this may not be a primary concern; but more generally income or liquidity may be a key determinant of insurance demand. In this case, I show how one can recover ex ante measures of willingness to pay if one can directly observe the difference in consumption or income between the insured and uninsured, as suggested in Proposition 3.

To see this, suppose that individuals with different willingnesses to pay have different incomes in period ν. For example, let ¯y(s) be the income of an individual who is indifferent to purchasing insurance when prices are such that a fraction s of the market owns insurance. Supplementary Appendix G modifies Proposition 5 to show that W(s) solves

$$
e^{-\gamma W(s)}\int_0^1 e^{\gamma(\bar{y}(\tilde{s})+C(\tilde{s})-w)+\frac{1}{2}\gamma^2\Sigma(\tilde{s})}d\tilde{s}
=e^{\gamma p_I(s)}e^{-\frac{1}{2}\gamma w}\int_0^s e^{-\gamma \bar{y}(\tilde{s})}d\tilde{s}+e^{\gamma p_U(s)}\int_s^1 e^{\gamma(\bar{y}(\tilde{s})+C(\tilde{s})-w)+\frac{1}{2}\gamma^2\Sigma(\tilde{s})}d\tilde{s}.
$$
(20)

I calibrate income heterogeneity by starting with a base income of $16,335, which corresponds to 150% FPL for a single adult in 2011 in Massachusetts. I then consider two cases corresponding to whether those with a higher willingness to pay for insurance have (a) higher incomes or (b) lower incomes. For the higher incomes case, I assume that those with the highest level of willingness to pay have an average income that is $1,382 higher than those with the lowest willingness to pay, where $1,382 is calibrated to be the mean health costs. In contrast, for the lower incomes case, I assume the opposite: those with the highest demand have incomes that are $1,382 lower than those with the highest willingness to pay. In both cases I assume average incomes are a linear function of s.

Figure 7A and B present the ex ante willingness to pay curve, W′(s), for these cases. When the insured have higher incomes, Figure 7A shows that the benchmark implementation over-states the true ex ante willingness to pay: it is optimal for just 44% of the market to be insured in contrast to the 54% implied in the baseline implementation. Conversely, Figure 7B shows how the pattern reverses when the insured have higher incomes. In this case, the optimal size of the market involves 65% of the market being insured.

In the presence of liquidity or income differences between the insured and uninsured, consumption data provide a path to accurate measurement of W′(s). The “modified β(s)” curve shows how using consumption or income data to measure Δc as in Proposition 3 recovers the true W′(s).⁴⁴ When the insured have higher incomes, using consumption data to measure the ex ante willingness to pay leads to a predicted optimal size of the market of 45%, close to the true optimal size of the market of 44%. Similarly, when the insured have lower incomes, using consumption data to measure ex ante willingness to pay implies an optimal size of the market of 65%, very close to the true optimum implied by the structural model.

43. A more subtle violation of Assumption 2 arises when individuals can save across periods. In this case, those who spend money on insurance may be able to borrow or reduce savings, increasing their consumption to help cover the cost of the insurance. This would imply the baseline approach would potentially over-state the ex ante value of insurance. But, an approach that directly measures the difference in consumption—as suggested below—would correctly recover ex ante welfare.

44. I use the formula $$\beta(s)=\gamma\left(D(s)-E[D(S)\mid S\geq s]+E[y_\nu(\theta_\nu)\mid S\leq s]-E[y_\nu(\theta_\nu)\mid S>s]\right)$$ where $$E[y_\nu(\theta_\nu)\mid S\leq s]-E[y_\nu(\theta_\nu)\mid S>s]$$ is the difference in disposable incomes between the insured and uninsured.

[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1219 1193–1223

1220 REVIEW OF ECONOMIC STUDIES

<table>
    <tr>
        <th>**A Higher WTP Have Higher Incomes**</th>
        <th>**B Higher WTP Have Lower Incomes**</th>
    </tr>
    <tr>
        <td>**Fraction Insured**</td>
        <td>**Modified β(s)**</td>
        <td>**True Ex-Ante WTP**</td>
        <td>**Market Surplus**</td>
        <td>**Baseline Approach**</td>
        <td>**Fraction Insured**</td>
        <td>**Modified β(s)**</td>
        <td>**True Ex-Ante WTP**</td>
        <td>**Market Surplus**</td>
        <td>**Baseline Approach**</td>
    </tr>
    <tr>
        <td>0</td>
        <td>900</td>
        <td>900</td>
        <td>900</td>
        <td>900</td>
        <td>0</td>
        <td>900</td>
        <td>900</td>
        <td>900</td>
        <td>900</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>600</td>
        <td>700</td>
        <td>500</td>
        <td>800</td>
        <td>0.2</td>
        <td>1000</td>
        <td>1200</td>
        <td>600</td>
        <td>1100</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>100</td>
        <td>200</td>
        <td>100</td>
        <td>300</td>
        <td>0.4</td>
        <td>600</td>
        <td>800</td>
        <td>200</td>
        <td>500</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>-300</td>
        <td>-300</td>
        <td>-300</td>
        <td>-300</td>
        <td>0.6</td>
        <td>0</td>
        <td>100</td>
        <td>-200</td>
        <td>-100</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>-500</td>
        <td>-500</td>
        <td>-500</td>
        <td>-500</td>
        <td>0.8</td>
        <td>-400</td>
        <td>-400</td>
        <td>-500</td>
        <td>-500</td>
    </tr>
    <tr>
        <td>1.0</td>
        <td>-600</td>
        <td>-600</td>
        <td>-600</td>
        <td>-600</td>
        <td>1.0</td>
        <td>-600</td>
        <td>-600</td>
        <td>-600</td>
        <td>-600</td>
    </tr>
</table>
FIGURE 7
Income or liquidity differences

### 7.4.2. Heterogeneity in risk preferences.
A classic problem in economics is to separate out risk preferences from beliefs. Individuals may be willing to pay a higher markup for insurance either because they have higher risk aversion, $\gamma$, or more uncertainty in their outcomes, $\Sigma$. To be consistent with Assumption 1, the specification in Section 7.2 ruled out heterogeneity in $\gamma$ and assumed differences in willingness to pay were due to differences in $\Sigma(s)$ (see equation (18)). Here, I assess the potential bias that arises when there is heterogeneity in $\gamma$. I provide intuition for the potential direction of this bias and make suggestions for how future work can overcome these potential biases.

I consider two specifications that parameterize heterogeneity in $\gamma$ as $\gamma \sim N\left(\mu_{\gamma}, \sigma_{\gamma}^{2}\right)$. In both specifications, mean risk aversion is $\mu_{\gamma}=5 \times 10^{-4}$, but they differ in their population standard deviations, $\sigma_{\gamma}$. The first "low heterogeneity" specification calibrates $\sigma_{\gamma}=5 \times 10^{-5}$ and the second "high heterogeneity" specification calibrates $\sigma_{\gamma}=1 \times 10^{-4}$ (close to the estimates in Handel et al. (2015)). I fit the model using a minimum distance estimator discussed in Supplementary Appendix H.

Figure 8 presents the results for the low heterogeneity case (Figure 8A) and the high heterogeneity case (Figure 8B). The dot-dash brown line corresponds to the true ex ante marginal surplus $W^{\prime}(s)$ that is implied by the structural framework with preference heterogeneity. The solid red line presents the benchmark implementation of $EA(s)$ using a homogeneous coefficient of relative risk aversion of $\gamma=5 \times 10^{-4}$, which corresponds to the population average in both specifications.

For both the high and low heterogeneity cases, the benchmark implementation over-states the ex ante value of insurance. These differences are larger for the case with a high degree of preference heterogeneity, as illustrated by the difference between Figure 8A and B. Ex ante welfare is maximized when 53% of the market owns insurance in the low-heterogeneity specification and 46% in the high-heterogeneity specification. These contrast with the optimal ex ante size of the market of 54% that is implied by the baseline implementation and the 41% that would maximize market surplus.

The true ex ante welfare is lower because the marginal utility of income is lower for the insured relative to uninsured than is implied in a model with fixed risk aversion. To see this, note that the marginal utility of consumption for CARA utility is $e^{-\gamma C}$, where $C$ is the net consumption (e.g., $C=(m-\lambda(\theta \nu))-\frac{1}{2 w}(m-\lambda(\theta \nu))^{2}+c(m)$). With heterogeneity in $\gamma$, those with high willingness

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1220 1193–1223

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1221

Copyedited by: ES MANUSCRIPT CATEGORY: Article






<table>
  <thead>
    <tr>
        <th colspan="5">Panel A: Low Heterogeneity ($\sigma_\gamma = 5 \times 10^{-5}$)</th>
        <th colspan="5">Panel B: High Heterogeneity ($\sigma_\gamma = 1 \times 10^{-4}$)</th>
    </tr>
    <tr>
        <th>Fraction Insured</th>
        <th>Baseline ($\gamma=5\times 10^{-4}$)</th>
        <th>Modified ($\beta(s)$)</th>
        <th>True Ex-Ante WTP</th>
        <th>Market Surplus</th>
        <th>Fraction Insured</th>
        <th>Baseline ($\gamma=5\times 10^{-4}$)</th>
        <th>Modified ($\beta(s)$)</th>
        <th>True Ex-Ante WTP</th>
        <th>Market Surplus</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0</td>
        <td>700</td>
        <td>700</td>
        <td>700</td>
        <td>700</td>
        <td>0</td>
        <td>700</td>
        <td>700</td>
        <td>700</td>
        <td>700</td>
    </tr>
    <tr>
        <td>.2</td>
        <td>650</td>
        <td>600</td>
        <td>650</td>
        <td>400</td>
        <td>.2</td>
        <td>650</td>
        <td>500</td>
        <td>650</td>
        <td>400</td>
    </tr>
    <tr>
        <td>.4</td>
        <td>400</td>
        <td>300</td>
        <td>400</td>
        <td>0</td>
        <td>.4</td>
        <td>400</td>
        <td>200</td>
        <td>400</td>
        <td>0</td>
    </tr>
    <tr>
        <td>.6</td>
        <td>-100</td>
        <td>-200</td>
        <td>-100</td>
        <td>-400</td>
        <td>.6</td>
        <td>-100</td>
        <td>-300</td>
        <td>-100</td>
        <td>-400</td>
    </tr>
    <tr>
        <td>.8</td>
        <td>-500</td>
        <td>-600</td>
        <td>-500</td>
        <td>-700</td>
        <td>.8</td>
        <td>-500</td>
        <td>-700</td>
        <td>-500</td>
        <td>-700</td>
    </tr>
    <tr>
        <td>1</td>
        <td>-800</td>
        <td>-900</td>
        <td>-800</td>
        <td>-900</td>
        <td>1</td>
        <td>-800</td>
        <td>-1000</td>
        <td>-800</td>
        <td>-900</td>
    </tr>
  </tbody>
</table>

FIGURE 8
Risk aversion heterogeneity

to pay for insurance are more likely to have a higher $\gamma$.<sup>45</sup> Because $e^{-\gamma C}$ is then declining in $\gamma$, this means the insured have a lower marginal utility of income relative to the uninsured, even conditional on their level of $C$.<sup>46</sup>

Stepping back, the core issue is that individuals may have different risk preferences over realizations of $\theta_\mu$ than their risk preferences over realizations of $\theta_\nu$ given $\theta_\mu$ (the latter is what is identified from insurance choices). The benchmark implementation above assumes risk aversion over these risks are the same. As these examples highlight, this need not be the case. However, Proposition 1 continues to hold even with heterogeneity in $\gamma$. This provides a potential roadmap for future work: one can seek to directly estimate the percentage difference in marginal utility of incomes between those who do versus do not purchase insurance.

To see this, the dashed blue line in Figure 8A and B presents estimates of ex ante welfare using the formula $EA(s) = s(1-s)D'(s)\beta(s)$, where $\beta(s)$ is taken to be the "true" percentage difference in marginal utilities of income between the insured and uninsured. The dashed blue line approximates the true ex ante welfare quite well in both specifications. The optimal size of the market implied by this implementation is 47% and 52% in the high and low heterogeneity specifications, respectively, which correspond closely to the true optimal size of the market of 46% and 53%. This shows that if one could observe the needed additional sufficient statistic, $\beta(s)$, one could recover ex ante welfare even in the presence of preference heterogeneity.

Recent and ongoing work develops a range of strategies for estimating percentage differences in marginal utilities of income. For example, in the unemployment insurance context, Hendren (2017) provides three potential approaches that use consumption data, ex ante realization of knowledge about future job loss, and spousal responses to infer the differences in marginal utilities. Landais and Spinnewijn (2019) also discuss a novel method using consumption responses to

\*45. Note that the model with heterogeneity in $\gamma$ continues to match the willingness to pay curve, $D(s)$, and cost curve, $E[\lambda(\theta_\nu)|s] = C(s)$, at each value of $s$. This means that the difference in average willingness to pay and costs between the insured and uninsured is held constant as one changes the amount of preference heterogeneity, $\sigma_\gamma$.

\*46. While the marginal utility of income declines in $\gamma$ in a benchmark CARA specification, $u(C;\gamma) = -\frac{1}{\gamma}e^{-\gamma C}$, it is also possible to have CARA specifications in which the marginal utility of income increases in $\gamma$. To see this, let $\bar{C}$ denote the upper bound of consumption, $C$, in the economy and let $u(C;\gamma) = -\frac{e^{\gamma \bar{C}}}{\gamma}e^{-\gamma C}$. This utility function also exhibits constant absolute risk aversion: $\frac{-u''(C)}{u'(C)} = \frac{\gamma e^{\gamma \bar{C}}e^{-\gamma C}}{e^{\gamma \bar{C}}e^{-\gamma C}} = \gamma$. But, $u'(C;\gamma) = e^{\gamma(\bar{C}-C)}$ in this case is increasing in $\gamma$. With this specification, the marginal utility of the insured would be higher than the marginal utility of the uninsured, conditional on consumption. This would lead the baseline approach to under-state ex ante willingness to pay.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1221 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article

1222 REVIEW OF ECONOMIC STUDIES

income shocks to infer percentage differences in marginal utilities of income. Future work could develop similar approaches tailored to the health insurance context and assess whether ex ante risk preferences differ systematically from those governing willingness to pay in observed insurance markets.

# 8. CONCLUSION

Traditional market surplus does not capture the value of insurance against risk that has been revealed at the time individuals choose insurance. In contrast, ex ante expected utility provides a consistent welfare framework to study optimal insurance policies. Ex ante measures of welfare differ from traditional market surplus because they measure expected utility before individuals learn their willingness to pay for insurance.

This article develops a set of tools to measure the ex ante welfare impact of insurance market policies and applies the approach to existing estimates of willingness to pay and cost curves for low-income health insurance in Massachusetts. Applying the model to the study of the Massachusetts health insurance exchange, the results show that an ex ante welfare perspective can lead to different normative conclusions. Policies that maximize ex ante welfare often involve lower insurance prices, a greater value of mandates, and a higher value of insurance subsidies.

Future work could measure the welfare consequences of contract distortions, such as the exclusion of high cost drugs for chronic conditions. It could also expand beyond the binary insurance decision considered here to consider menus of insurance contracts. One could also extend the results to normative frameworks that allow for behavioural biases (e.g., as in Spinnewijn (2017)), which have been shown to be important in health insurance settings.

Future work can also extend the ideas developed here to settings where prices are not observed. For example, many approaches use labour supply responses to infer the value of social insurance programs (e.g., Keane and Moffitt, 1998; Dague, 2014; Gallen, 2015). These approaches capture the value of insurance against only the risk that remains after choosing labour supply. Other approaches use changes in consumption around a shock to infer willingness to pay (e.g., Gruber, 1997; Meyer and Mok, 2019). But consumption should change when information about the event is revealed, not when the event occurs. The approaches developed here could be extended to measure ex ante expected utility in such settings.

Lastly, many macroeconomic welfare measures face similar conceptual issues. This includes the famous calculations of the welfare cost of business cycles in Lucas (2003). When consumption responds to information over time, the variance of consumption changes may understate measures of ex ante welfare. Future work could extend the tools in this article to measure the ex ante welfare cost of business cycles and other macroeconomic risk.

*Acknowledgments.* I am very grateful to Raj Chetty, David Cutler, Liran Einav, Amy Finkelstein, Ben Handel, Pat Kline, Tim Layton, Mark Shepard, Mike Whinston, seminar participants at the NBER Summer Institute and University of Texas, along with five anonymous referees and the editor, Adam Sziedl, for helpful comments and discussions. I also thank Kate Musen and Peter Ruhm for helpful research assistance. Support from the National Science Foundation CAREER Grant is gratefully acknowledged.

### Supplementary Data

Supplementary data are available at *Review of Economic Studies* online.

### REFERENCES

BAILY, M. N. (1978), “Some Aspects of Optimal Unemployment Insurance”, *Journal of Public Economics*, **10**, 379–402.
CABRAL, M. (2017), “Claim Timing and Ex Post Adverse Selection”, *Review of Economic Studies*, **84**, 1–44.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1222 1193–1223

Copyedited by: ES MANUSCRIPT CATEGORY: Article

HENDREN MEASURING EX ANTE WELFARE IN INSURANCE MARKETS 1223

CHETTY, R. (2006), “A General Formula for the Optimal Level of Social Insurance”, *Journal of Public Economics*, **90**, 1879–1901.

DAGUE, L. (2014), “The Effect of Medicaid Premiums on Enrollment: A Regression Discontinuity Approach”, *Journal of Health Economics*, **37**, 1–12.

EINAV, L., FINKELSTEIN, A. and CULLEN, M. R. (2010), “Estimating Welfare in Insurance Markets using Variation in Prices”, *Quarterly Journal of Economics*, **125**, 877–921.

EINAV, L., FINKELSTEIN, A., RYAN, S. P., SCHRIMPF, P. and CULLEN, M. R. (2013). Selection on Moral Hazard in Health Insurance. *American Economic Review*, **103**, 178–219.

EINAV, L., FINKELSTEIN, A. and WILLIAMS, H. (2016), “Paying on the Margin for Medical Care: Evidence from Breast Cancer Treatments”, *American Economic Journal: Economic Policy*, **8**, 52–79.

FELDMAN, R. D. and DOWD, B. E. (1982), “Simulation of a Health Insurance Market with Adverse Selection”, *Operations Research*, **30**, 1027–1042.

FINKELSTEIN, A., HENDREN, N. and LUTTMER, E. F. (2019), “The Value of Medicaid: Interpreting Results from the Oregon Health Insurance Experiment”, *Journal of Political Economy*, **127**, 2836–2874.

FINKELSTEIN, A., HENDREN, N. and SHEPARD, M. (2019), “Subsidizing Health Insurance for Low-Income Adults: Evidence from Massachusetts”, *American Economic Review*, **109**, 1530–67.

FINKELSTEIN, A., LUTTMER, E. F. and NOTOWIDIGDO, M. J. (2013), “What Good is Wealth without Health: The Effect of Health on the Marginal Utility of Consumption”, *Journal of the European Economic Association*, **11(s1)**, 221–258.

FINKELSTEIN, A., MCGARRY, K. and SUFI, A. (2005), “Dynamic Inefficiencies in Insurance Markets: Evidence from Long-term Care Insurance”, *American Economic Review*, **95**, 224–228.

GALLEN, T. S. (2015), “Using Participant Behavior to Measure the Value of Social Programs: The Case of Medicaid” (Working Paper).

GRUBER, J. (1997), “The Consumption Smoothing Benefits of Unemployment Insurance”, *American Economic Review*, **87(1)**, 192–205.

HACKMANN, M. B., KOLSTAD, J. T. and KOWALSKI, A. E. (2015), “Adverse Selection and an Individual Mandate: When Theory Meets Practice”, *American Economic Review*, **105**, 1030–1066.

HANDEL, B., HENDEL, I. and WHINSTON, M. D. (2015), “Equilibria in Health Exchanges: Adverse Selection Versus Reclassification Risk”, *Econometrica*, **83**, 1261–1313.

HENDREN, N. (2013), “Private Information and Insurance Rejections”, *Econometrica*, **81**, 1713–1762.

HENDREN, N. (2016), “The Policy Elasticity”, *Tax Policy and the Economy*, **30**, 51–89.

HENDREN, N. (2017), “Knowledge of Future Job Loss and Implications for Unemployment Insurance”, *American Economic Review*, **107**, 1778–1823.

HENDREN, N. and SPRUNG-KEYSER, B. (2020), “A Unified Welfare Analysis of Government Policies”, *Quarterly Journal of Economics*, forthcoming.

HIRSHLEIFER, J. (1971), “The Private and Social Value of Information and the Reward to Inventive Activity”, *American Economic Review*, **61**, 561–574.

KEANE, M. and MOFFITT, R. (1998), “A Structural Model of Multiple Welfare Program Participation and Labor Supply”, *International Economic Review*, **39**, 553–589.

LANDAIS, C. and SPINNEWIJN, J. (2019), “The Value of Unemployment Insurance” (Working Paper).

LUCAS, R. E. (2003), “Macroeconomic Priorities”, *American Economic Review*, **93**, 1–14.

MEYER, B. D. and MOK, W. K. (2019), “Disability, Earnings, Income and Consumption”, *Journal of Public Economics*, **171**, 51 – 69.

MILGROM, P. and SEGAL, I. (2002), “Envelope Theorems for Arbitrary Choice Sets”, *Econometrica*, **70**, 583–601.

PANHANS, M. (2019), “Adverse Selection in ACA Exchange Markets: Evidence from Colorado”, *American Economic Journal: Applied Economics*, **11**, 1–36.

SPINNEWIJN, J. (2017), “Heterogeneity, Demand for Insurance, and Adverse Selection”, *American Economic Journal: Economic Policy*, **9**, 308–343.

Downloaded from https://academic.oup.com/restud/article/88/3/1193/5815570 by guest on 24 May 2021
[15:08 8/5/2021 OP-REST200014.tex] RESTUD: The Review of Economic Studies Page: 1223 1193–1223