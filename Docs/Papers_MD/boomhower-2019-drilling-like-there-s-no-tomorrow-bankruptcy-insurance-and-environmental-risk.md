American Economic Review 2019, 109(2): 391–426
https://doi.org/10.1257/aer.20160346

# Drilling Like There’s No Tomorrow: Bankruptcy, Insurance, and Environmental Risk<sup>†</sup>

By JUDSON BOOMHOWER*

*This paper measures the effects of bankruptcy protection on industry structure and environmental outcomes in oil and gas extraction. Using administrative data from Texas, I exploit variation in an insurance requirement that reduced firms’ ability to avoid liability through bankruptcy. Among small firms, the policy substantially improved environmental outcomes and reduced production. Most production was reallocated to larger firms with better environmental records, but high-cost production where social cost may have exceeded social benefit decreased. These results suggest that incomplete internalization of environmental costs due to bankruptcy is an important determinant of industry structure and safety effort in hazardous industries. (JEL G22, G33, L25, L71, Q35, Q52)*

In most legal systems, the debts of insolvent parties can be eliminated through bankruptcy. Bankruptcy protection improves insolvent actors’ work incentives and mitigates coordination problems among creditors.<sup>1</sup> However, bankruptcy protection also distorts behavior by insulating actors from worst-case outcomes. For example, financial firms may become excessively leveraged or consumers may accumulate excessive personal debt. One important implication of bankruptcy protection is that firms in hazardous industries will take excessive environmental and public health risks. Since damages can be discharged in bankruptcy, firms with assets less than their worst-case liabilities face inadequate safety incentives. Economists call this the “judgment-proof problem” (Shavell 1986, 2002).

In addition to reducing precaution, the judgment-proof problem may also distort industry structure. The ability to avoid liability through bankruptcy creates a private cost advantage for small firms. This advantage may increase the share of output

\*Department of Economics, University of California, San Diego, 9500 Gilman Drive #0508, La Jolla, CA 92093 (email: jboomhower@ucsd.edu). This paper was accepted to the AER under the guidance of Penny Goldberg, Coeditor. I am grateful to Severin Borenstein, Lucas Davis, Meredith Fowlie, Max Auffhammer, Catherine Wolfram, Ryan Kellogg, Hunt Allcott, Reed Walker, Charlie Kolstad, Derek Lemoine, Mark Jacobsen, Michael Best, Walter Graf, Pat Baylis, and Paula Pedro; to seminar participants at the NBER Summer Institute, UC Berkeley, Carnegie Mellon, Chicago Harris, Columbia SIPA, Michigan, Northwestern Kellogg, Stanford, University of Arizona, University of British Columbia, UC Davis, UC San Diego, and UC Santa Barbara; and to the Railroad Commission of Texas. Research support from a National Science Foundation Graduate Research Fellowship, the Energy Institute at Haas, and the Stanford Institute for Economic Policy Research (SIEPR) is gratefully acknowledged. The author declares that he has no relevant or material financial interests that relate to the research described in this paper.

<sup>†</sup> Go to https://doi.org/10.1257/aer.20160346 to visit the article page for additional materials and author disclosure statement.

<sup>1</sup> For a review of the economics of bankruptcy, see White (2011). La Porta et al. (1998) and Armour and Cumming (2008) discuss bankruptcy regimes across countries.

391

392 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

produced by small firms, since it may allow them to outcompete larger producers with lower social costs of production. The judgment-proof problem may also encourage low-value production that would not be profitable for any firm if producers fully internalized expected damages.

There are many industries where potential environmental or health costs exceed firms’ ability to pay. In the United States, bankrupt polluters have avoided billions of dollars in liability under federal environmental laws. Example industries, of many, include chemicals manufacturing, dry cleaning, hazardous waste disposal, retail gasoline, and metals mining.² Taxi companies and drivers for ride-sharing services like Uber have avoided liability for pedestrian injuries through bankruptcy. Asbestos manufacturers have been liable for health damages that exceed their ability to pay, and dozens have declared bankruptcy. Railroads and other freight shippers are susceptible to bankrupting accidents like an explosion in Quebec in 2013 that killed 47 people.³ Discharging these companies’ liabilities achieves the goal of bankruptcy by giving owners a “fresh start.” However, this policy goal may come at the cost of weakened safety incentives and distorted industry composition.

One important industry where these concerns are likely to be relevant is fossil fuel production. Extraction of crude oil, natural gas, coal, and tar sands all feature pronounced environmental risk. These sectors also include many small producers. Davis (2015) describes how natural gas producers in the United States face potential environmental liability for groundwater contamination and other damages that far exceeds their assets. The dramatic growth in oil and gas production over the past decade due to the introduction of hydraulic fracturing has multiplied the potential for environmental damage in this industry. The avenues for water contamination in hydraulically fractured wells are the same as in the conventional wells considered in this study (Darrah et al. 2014).

Regulators often seek to mitigate the judgment-proof problem through policy interventions. One common approach is requiring firms to post bonds or to purchase insurance to cover damages exceeding the firm’s assets.⁴ When firms post their own assets as a bond, they face the threat of bond forfeiture. When they must purchase insurance, they face ongoing monitoring and risk-based underwriting by the insurer. By causing firms to internalize potential damages, these policies potentially change the composition of firms in the industry and the amount of precaution that firms exercise. The major fossil fuel-producing US states, US federal land managers, Canadian provinces, and other jurisdictions require this type of “financial assurance” from companies in oil and gas drilling. Such requirements are also common in other industries where the judgment-proof problem is a concern, including metals

\*² Pollution examples are from: US Government Accountability Office (2005), Report 05-658 (chemicals manufacturing, dry cleaning); GAO (2003), Report 03-761 (hazardous waste disposal); GAO (2007), Report 07-152 (retail gasoline); and US Environmental Protection Agency (2004), Report 2004-P-00005 (metals mining).

\*³ Health and safety examples are from: Christopher Drew and Andy Newman, “Taxi Owners Deftly Dodge Claims of Accident Victims,” *New York Times*, May 24, 1998 (taxis); Christopher B. Dolan, “Viewpoints: Uber Should Have Enough Insurance, Just Like Anyone Else,” *Sacramento Bee*, August 8, 2014 (ride-sharing); White (2014) (asbestos); and Betsy Morris, “Fiery Oil-Train Accidents Raise Railroad Insurance Worries,” *Wall Street Journal*, January 8, 2014 (railroads).

\*⁴ Alternative policies include direct safety regulation (Shavell 1984), minimum asset requirements (Shavell 2002), and vicarious liability for lenders or business partners (Kornhauser 1982, Pitchford 1995, Boyd and Ingberman 2003). See Shavell (2007) for a review of possible policy responses.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 393

mining, landfills, hazardous waste disposal, and chemical manufacturing. Despite the wide relevance of the issue and the range of policies enacted in response, empirical understanding of the judgment-proof problem and intended solutions is limited.

This paper measures the effect of bankruptcy protection on environmental outcomes and industry composition in the onshore oil and gas industry. I begin from a simple conceptual model of a hazardous industry with heterogeneous firms and projects (e.g., oil wells). Bankruptcy lowers the expected private costs of environmental damage for small producers. As a result, small firms exercise too little care in each project; acquire some projects that would be more efficiently operated by large firms; and develop additional high-cost projects where social cost exceeds social benefit. Requiring firms to purchase insurance to cover damages beyond their own assets mitigates these distortions. This model yields testable hypotheses that guide the empirical analysis. It also provides a framework for welfare analysis.

The empirical application uses data on the universe of onshore oil and gas producers in Texas, the largest oil- and gas-producing state in the United States. I merge administrative databases covering production, environmental outcomes, and entry and exit to generate a novel dataset that includes thousands of firms and hundreds of thousands of wells. To measure the effects of the judgment-proof problem, I exploit a natural experiment created by the introduction of an insurance requirement during 2002–2003. Firms were required to purchase an insurance product from private insurers that was specifically designed to address bankruptcy concerns. Insurers charged firm-specific premiums based on predicted risk, so this requirement caused firms to internalize expected environmental costs through ongoing premium payments.

I examine the effect of the insurance requirement on a range of outcomes related to industry composition and environmental protection. These outcomes include firm-level participation and output, well-level ownership and shutdown decisions, and environmental outcomes related to water pollution. The nature of the data varies across outcomes. These differences motivate a combination of empirical approaches, including regression discontinuity, event study analysis leveraging quasi-random variation in policy timing, and time series comparisons.

I find that the bond requirement caused striking changes in industry structure. About 5 percent of producers left the market immediately. These exiting firms were small and had poor environmental records. Among remaining firms, the bond requirement reduced production among the smallest 80 percent of firms by about 5 percent on average. At the individual well level, the primary effect of the policy was to reallocate wells from small to large producers. The bond requirement moved about 4 percent of wells operated by the smallest 80 percent of firms to new operators. Another 1 percent of the wells operated by these firms were shut down, primarily representing low-producing wells where social cost (including environmental risk) is most likely to exceed social benefit.

Environmental outcomes also improved sharply. Small producers caused a large share of environmental incidents industry-wide, even though they were a small share of total output. This meant that the bond requirement had a large effect on the total environmental cost of oil and gas development. I find clear evidence of an approximately 70 percent decrease in orphan wells, which are wells abandoned by insolvent owners without precautions needed to prevent pollution. Orphan wells are a major cause of groundwater contamination from oil and gas extraction. They are

394 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

also a source of atmospheric methane emissions that contribute to climate change and pose an explosion hazard. Violations of water protection rules also decreased by 25 percent industry-wide. The results suggest that by screening out firms and wells that insurers perceived to be high risk, and increasing accountability for remaining firms, the bond requirement mitigated the harmful incentive effects created by bankruptcy protection. A back-of-the-envelope comparison of the value of avoided environmental damages and compliance costs suggests that the policy yielded substantial welfare gains.

My results build on a small set of existing empirical studies. Ringleb and Wiggins (1990) find that the number of small firms in dangerous industries increased during the 1970s, coincident with more aggressive enforcement of liability claims for workplace carcinogen exposure. Alberini and Austin (2002) find that in states with strong liability rules, toxics spills are concentrated among small firms. They suggest that limited liability may induce specialization by small firms in risky activities. Using state-level panel data on annual releases from underground storage tanks, Yin, Pfaff, and Kunreuther (2011) finds decreased leaks in seven states after tank owners were required to purchase private liability insurance. Brooks (2002) documents contracting out of risky activities to small companies in ocean shipping. Finally, while not focused on limited liability, a working paper by Yin, Kunreuther, and White (2007) finds that stricter environmental regulations for underground tanks at service stations in Michigan led to increased exit by small firms.

This paper contributes to the literature about the judgment-proof problem in four ways. First, the small number of existing empirical studies have primarily focused on a single margin, such as accidents or firm size. This paper leverages project- and firm-level data on both industry composition and environmental outcomes to provide a comprehensive investigation of the judgment-proof problem in a single industry. This allows me to say more about the full impacts of limited liability than has been possible in previous studies. Second, the rich data allow for more detailed exploration of the economic mechanisms behind the observed effects. For example, I am able to track ownership of individual oil and gas leases as they transfer between firms or shut down in response to the policy change; and I am able to observe how the response to the policy change depends on a firm’s past environmental compliance history.

Third, the empirical analysis in this study requires weaker identifying assumptions than previous work. Earlier studies rely primarily on cross-sectional comparisons across jurisdictions, industries, or firms that are potentially vulnerable to omitted variables bias. For example, firm size may be correlated with experience or skill, which could also affect accident rates. In contrast, this paper focuses on comparisons immediately before and after a policy change, or between firms that have or have not yet been required to purchase insurance for exogenous reasons.

Finally, this study measures the significance of this market failure in one of the most important industries in the world. More than 15.3 million Americans have had an oil or gas well drilled within one mile of their home since 2000.<sup>5</sup> There has been a great deal of empirical research on the effects of this boom (Allcott and Keniston 2018; Darrah et al. 2014; Muehlenbachs, Spiller, and Timmins 2015). This study

<sup>5</sup> Russell Gold and Tom McGinty, “Energy Boom Puts Wells in America’s Backyards,” *Wall Street Journal*, October 25, 2013.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 395

targets a less-widely-explored question: do regulations induce oil and gas producers to balance profits and environmental risk appropriately? The results suggest that there would be benefits from increasing bond requirements in other oil- and gas-producing states to at least the level required in Texas. These findings are relevant for regions that are currently considering or implementing new bond requirements for oil and gas producers, including Pennsylvania, California, and Alberta. More broadly, they bolster concerns among economists and policymakers about limited liability problems in many sectors, including landfills, chemicals, small-scale manufacturing, and hazardous materials transportation.

The rest of the paper is organized as follows. Section I describes the oil and gas industry. Section II proposes a model for how bankruptcy protection affects industry composition and safety effort. Section III describes the empirical strategy and Section IV describes the data. Section V presents empirical results, Section VI discusses aggregate impacts and welfare, and Section VII concludes.

## I. The Onshore Oil and Gas Industry

The United States in 2015 produced 3.4 billion barrels of crude oil and 32.9 trillion cubic feet of natural gas, with a total value of $255 billion. The large majority (83 percent of the oil and 94 percent of the natural gas) was produced on land (“onshore”) and the remainder came from drilling in the ocean (“offshore”).⁶ While offshore drilling tends to be dominated by large multinational companies, onshore production requires a relatively modest investment and many wells belong to small independent firms. For example, in Texas during the period studied in this paper, more than 5,000 operators reported production each year and the large majority had annual revenues below $1 million. These firms often have few assets other than oil and gas income.

Onshore production in the United States comes from over one million wells.⁷ Production rates vary widely across wells due to differences in underlying resource quality and to natural declines in production over the life of a well. Because of the long production history in this country and the steep “decline curves” associated with more recent shale wells, many of the active wells in the United States today are very low-producing. Seventy-six percent of oil wells and 72 percent of gas wells produce less than 10 barrel-of-oil equivalents (BOE) per day; 39 percent of oil wells produce less than 1 BOE per day.⁸ Low-producing wells tend to be operated by small firms, as shown in online Appendix Section A. While their output of valuable products is lower, the environmental risks posed by these wells are the same as at much more productive sites.

The primary environmental concern in this industry is pollution of groundwater or surface water with crude oil, natural gas, drilling fluids, or concentrated saltwater

⁶ Statistics are from US EIA “Crude Oil Production” and “Natural Gas Gross Withdrawals.” Value is calculated using annual average prices at Cushing, OK and Henry Hub, LA, and assuming a natural gas heat content of 1,037 Btu per cubic foot.
⁷ The 2011 Annual Energy Review (EIA 2012) reports 536,000 active oil wells and 504,000 active natural gas wells (the most recent count).
⁸ US EIA (2018), Table 8. One BOE is one barrel of oil or 6,000 cubic feet of natural gas. One BOE represents the approximate energy content of a barrel of oil, and is a commonly used metric for combining oil and gas into a single measure.

396 *THE AMERICAN ECONOMIC REVIEW* FEBRUARY 2019

(an unavoidable byproduct from oil and gas reservoirs). This threat exists at all stages of production, from drilling through final site reclamation. During drilling, improper construction of the cement and steel well casing can increase the risk of leaks. Surface spills from storage tanks, waste pits, and pipes pose a threat throughout production. And aging wells, particularly those that are no longer producing, present a serious risk of groundwater pollution due to deterioration of the casing and migration of pollutants up the open well bore. This risk is reduced by plugging wells with cement once they are no longer producing. Plugging is costly and so some firms delay it for months or years, increasing the risk of contamination in the interim. In the extreme, wells may be left unplugged after the responsible firm is dissolved or insolvent, creating orphan wells. During 1983–2008, 6 percent of oil and gas groundwater contamination cases in Texas were from drilling and well completion activities; 33 percent from production; 43 percent from waste management and disposal; and 17 percent from orphan wells (Kell 2011). Orphan and inactive wells also emit methane to the atmosphere (Kang et al. 2016, Townsend-Small et al. 2016). Methane is a potent greenhouse gas, and also presents a risk of explosions.

The combination of many small firms, low-value projects, and large potential environmental damages have made the judgment-proof problem a pervasive challenge for regulators in Texas and other states. As I show in Section VA, small firms represent a large share of total environmental incidents even though they produce a small fraction of total output. Bankruptcy can make it difficult to hold firms accountable for damages. According to a 2003 report by the State Review of Oil and Natural Gas Environmental Regulations, during 2001 and 2002 (before the bond requirement), Texas was unable to collect 68 percent of the penalties assessed for oil and gas rules violations. The most common reason these fines were uncollectible was bankruptcy.<sup>9</sup>

In response to these challenges, regulators in all of the major oil and gas regions of the United States, as well as Canada and other countries, have enacted some form of bond or financial security requirements. These requirements vary in their stringency, with those in Texas being among the most strict (Dutzik, Davis, and Heeke 2013). A common way of complying with these requirements is to purchase an instrument called a surety bond from a private insurer at market premiums. Surety bonds obligate the insurer to pay the state up to the value of the bond if the insured firm goes out of business and leaves some environmental cost. Unlike liability insurance, if the state makes a claim against the bond, the insurer will attempt to recover repayment from the oil and gas firm. Thus, a surety bond transfers default risk from the state to the insurer, who becomes responsible for environmental damages that exceed the oil and gas firm’s ability to pay.<sup>10</sup>

<sup>9</sup> "RRC assessed $5,183,832 in penalties and collected $1,728,595... Penalties are most often uncollectible because the company has gone out of business and has no assets" (STRONGER 2003).
<sup>10</sup> Texas and many other states accept surety bonds or irrevocable letters of credit to meet bond requirements. The instruments are very similar. I follow convention and refer to both as "bonds."

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 397

# II. Conceptual Framework

This section presents a model of how bankruptcy affects hazardous industries. The goal is to provide a simple framework for how limited liability affects which projects are produced and who produces them, as well as the level of precaution.<sup>11</sup> This model yields testable predictions that I take to the data. It also guides the discussion of welfare effects in Section VI. The main results are presented here. More details and proofs are in online Appendix Section H.

A homogeneous good is produced from discrete projects (e.g., oil wells, landfills, mines). There are $J$ types of potential projects, with a finite number of each type. Project types vary in quality (potential output) $\rho$ and are indexed from lowest to highest quality by $j = 1, \dots, J$. Each project site is owned initially by a separate landowner. The industry is small relative to the world market for output, so price per unit of output is the world price $p$.

Firms buy project sites from landowners and produce output. There is an infinite number of potential entrants of two types. High-technology firms (hereafter, $H$-type firms) pay sunk entry cost $F_H$. Low-technology firms (hereafter, $L$-type firms) pay sunk entry cost $F_L < F_H$. Output from a project is deterministic and depends multiplicatively on firm type and project type: $f(A_i, \rho_j) = A_i \rho_j$. The technology shifter $A_i \in \{A_H, A_L\}$ depends on firm type, with $A_H > A_L$. The complementarity between firm productivity and input quality in this production function follows Becker (1973) and subsequent models. This feature of the model closely matches the stylized facts of the oil and gas industry, where there is a clear pattern of assortative matching between firms and projects (as illustrated in online Appendix Figure 1).

To simplify exposition, I assume that each firm specializes in one of the $J$ project types (this assumption is relaxed in the online Appendix). I use $q$ to indicate units of output from the firm and $k = q/(A_i \rho_j)$ to represent the number of projects for a firm of type $i$ with projects of type $j$. A firm's variable cost for $k$ projects is $c(k)$, with $c'(k) > 0$. In addition, $c''(k) > 0$ (for example, due to span of control problems as in Lucas 1978). Firms pay landowners an endogenously-determined price $r_j$ for sites of type $j$.

Each project creates stochastic environmental costs. Firms can reduce expected environmental costs by exerting costly safety effort. The level of safety effort in each project is a continuous variable $x$. The price of safety effort is normalized to 1, so that per-project expenditures on safety are also $x$. The cost of safety effort and the distribution of damages are the same for both types of firms. The environmental costs of a project are $h(x)$, a random variable with mean $\mu(x)$ and upper bound $\bar{h}$. Damages at each project are independent, so the firm's expected environmental costs are $\mu(x)k$.<sup>12</sup>

\*11 Existing models of the judgment-proof problem have had little to say about the choice of firm size or output. Shavell (2002) assumes that firms produce only a single unit, and Pitchford (1995) describes firms as considering a single project. Ganuza and Gomez (2011) and Che and Spier (2008) assume identical output across firms while allowing for strategic choice of asset level and capital structure, respectively. Van 't Veld (2006) considers optimal firm sizes with and without liability rules, but not heterogeneity in firms or projects, or an insurance requirement.
\*12 Potential damages are assumed to not depend on each project's output. This is consistent with evidence on the mechanisms of environmental damage in onshore oil and gas (Kell 2011, GWC 2014, CCST 2015, US EPA 2016). Unlike offshore incidents, where a well's entire production stream may flow into the environment, onshore water contamination generally occurs through isolated leaks of oil or gas, chemicals, or wastewater from fixed

398 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

![Line chart showing average cost curves (ACH, ACL, ACL') and revenue lines (pAHρj, pALρj) for heterogeneous firms in a hazardous industry, with optimal project counts kL* and kH* and rents rHj and rLj.](page_8_chart_1_v2.jpg)

FIGURE 1. A HAZARDOUS INDUSTRY WITH HETEROGENEOUS FIRMS

A firm’s expected profit function with technology $i$ and project type $j$ is

$$ (1) \quad pq - c\left(\frac{q}{A_i \rho_j}\right) - \frac{q}{A_i \rho_j}(r_j + x + \mu(x)) - F_i. $$

When firms are fully liable for damages, they choose the socially optimal level of safety effort, $x^*$. This effort level is given by $-h'(x) = 1$, which equates the marginal reduction in expected damages with the marginal cost of safety effort, 1.

Figure 1 illustrates the market for projects of a given type $j$. The horizontal axis represents the number of projects undertaken by a firm. The vertical axis represents revenues and costs. The solid horizontal line at $p A_H \rho_j$ represents the value of output from a project produced by a type $H$ firm at the fixed world price $p$. The dashed horizontal line shows the project-level revenues for the type $L$ firm with the same project type, $p A_L \rho_j$. Define “project costs” to include all costs except payments to landowners $r$. The two black curves indicate average project costs for each type of firm: that is, project costs divided by the number of projects. Competition for project sites implies that firms pay landowners the difference between average project costs and project revenues, earning zero profits. Free entry ensures that project costs are minimized. Each firm type’s optimal number of projects $k_i^*$ minimizes average production cost. Firms offer landowners the difference between project revenues and average production cost at $k_i^*$, shown as $r_{Hj}$ and $r_{Lj}$. If firms offered less, they would be outbid by a firm of the same type

equipment at the well site. This fixed equipment is present at wells of all production levels, and includes storage tanks, pipelines and hoses, wastewater pits, and the well itself, including the casing. The likelihood and impacts of these idiosyncratic equipment failures are not expected to depend on the well’s output. Furthermore, these releases are usually caused by human error, aging or poor-quality equipment, orphan wells, and inadequate spill response. None of these are more likely at higher-producing wells.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 399

operating at the efficient scale $k_i^*$. If they offered more they would earn negative long-run profits. All projects of type $j$ are then produced by whichever firm type can offer greater surplus to landowners.

Projects that would yield revenues below the average project cost of both firm types are not produced. To produce these without earning negative profits, a firm would have to earn above its average cost on another project. This is impossible due to competition. I define $\underline{\rho}$ as the minimum project quality cutoff for production.

Project costs do not depend on project quality, but project revenues do. The revenue difference between firm types for a given project type, $p(A_H - A_L) \rho_j$, increases with site quality. In the figure, the vertical distance between the two revenue lines grows as project quality increases. The greater returns to the advanced technology increasingly counteract its higher costs, so that both firm types can coexist in equilibrium even in the absence of any market failure. If both firm types are present in equilibrium, then there is an interior project quality cutoff $\bar{\rho}$. All projects with quality above $\bar{\rho}$ are produced by type $H$ firms and all projects with quality below $\bar{\rho}$ are produced by type $L$ firms. It is also possible for one firm type to be the efficient producer for all project types. The case where both types exist in equilibrium is the empirically relevant case, so that is where I focus attention.

## A. The Effects of Bankruptcy Protection

Now assume that when damages exceed the value of the assets that can be seized from the firm, the difference is discharged in bankruptcy. In the first period, the firm pays the sunk entry cost $F_i$ and purchases and develops project sites. In the second period, revenues are earned and damages are realized. The entry cost $F_i$ is sunk. Once developed, projects are assumed to have no value other than the output produced. Thus, the firm's assets that can be seized to pay damages are equal to its total revenue $pq$.<sup>13</sup> The firm is assumed to be the only party that is liable for environmental damages.<sup>14</sup>

PROPOSITION 1: *The availability of bankruptcy reduces safety effort if the firm's assets are less than worst-case damages. Assets represent a smaller share of worst-case damages for the low-technology firm than the high-technology firm.*

<sup>13</sup>The firm has an incentive to minimize asset exposure, so will be initially capitalized with an amount exactly sufficient to cover the first-period costs. Thus, when damages are realized the value of the firm equals revenues minus environmental damages.

<sup>14</sup>A separate theoretical literature considers whether "vicarious liability" for business partners and other third parties can mitigate the judgment-proof problem. See, for example, Kornhauser (1982), Pitchford (1995), and Boyd and Ingberman (2003). Note however that onshore oil and gas pollutants are explicitly exempt from the main US laws that extend environmental liability to third parties, the Comprehensive Environmental Response, Compensation, and Liability Act (CERCLA) and the Resources Conservation and Recovery Act (RCRA). These "petroleum exemptions" limit the liability of landowners and other third parties for pollution from onshore oil and gas extraction.

400 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

Let $v$ represent the firm's total realized environmental damages, with probability distribution function (PDF) $f(v; k, x)$.<sup>15</sup> The profit function when bankruptcy exists is

$$ (2) \quad pq - c\left(\frac{q}{A_i \rho_j}\right) - \frac{q}{A_i \rho_j}[r_j + x] - F_i - \begin{cases} \mu(x) \frac{q}{A_i \rho_j} & pq \geq \bar{h}k \\ [\int_0^{pq} vf(v)dv + \int_{pq}^{\bar{h}k} pqf(v)dv] & pq < \bar{h}k \end{cases} $$

When revenues are greater than worst-case damages $\bar{h}k$, the profit function is unchanged. However, when revenues are less than $\bar{h}k$, the firm considers a truncated damage distribution. For damage realizations above $pq$, the firm is bankrupt. Expected damages are replaced in equation (2) by the probability-weighted sum of damages from 0 to $pq$, plus $pq$ for all larger outcomes. The firm will choose safety effort less than $x^*$ because it does not fully internalize expected damages.

The amount of the distortion in safety effort depends on the ratio of total revenues to worst-case liabilities. When $pq < \bar{h}k$, decreases in $pq/(\bar{h}k)$ increase the gap between expected damages and expected private losses. This ratio can be rewritten as $pA_i \rho_j / \bar{h}$. Since $A_H > A_L$, and because the project types produced by $H$-type firms have larger $\rho$, the ratio is always larger for the $H$ type firm. The larger revenues generated by high-technology production lead high-technology firms to internalize a larger share of potential damages on each project.<sup>16</sup>

Having established that the type $L$ firm's incentives are relatively more affected by bankruptcy protection, I now apply an expositional simplification and treat the type $H$ firm as if its assets were sufficient to fully internalize worst-case damages. This is not a required assumption, but it simplifies the discussion. It also approximates the reality of the empirical application, where the assets of major international oil and gas companies are large relative to potential onshore damages.

The second proposition addresses industry composition.

PROPOSITION 2: *The availability of bankruptcy leads low-technology firms to produce projects with quality below the efficient minimum quality, $\underline{\rho}$, and above the efficient quality cutoff between producer types, $\bar{\rho}$.*

The availability of bankruptcy lowers private project costs for potentially judgment-proof firms. Returning to Figure 1, the average cost curve for type $L$ firms shifts down to the gray line labeled $AC'_L$. This affects production in two ways. First, it becomes privately profitable to produce lower quality projects. Projects yielding revenues between the minimum values of $AC_L$ and $AC'_L$ are produced, even though the social surplus from these sites is negative.

Second, the lower private costs for type $L$ firms increase the distance $r_{Lj}$. Limited liability allows type $L$ firms to outcompete type $H$ firms for some project types above $\bar{\rho}$. This is inefficient; these projects could be produced more safely at lower cost

\*<sup>15</sup>$f(v; k, x)$ is the distribution of the sum of damages from all the firm's projects. If the PDF of project-level damages given $x$ is $g(h; x)$, $f(v; k, x)$ is the $k$-fold convolution of $g(h; x)$.
\*<sup>16</sup>The online Appendix describes a second reason that the high-technology type is less affected by bankruptcy, which is that operating many projects reduces the volatility of total damages when project-level damages are independent.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 401

by type *H* producers. Both of the effects described in Proposition 2 lead to increases in the number of *L*-type firms, in order to produce these additional projects.<sup>17</sup>

## B. Effects of an Insurance Mandate

Now consider a policy that requires producers to purchase insurance to cover damages beyond their firm’s assets. Firms purchase surety bonds from insurers in a competitive market. Maximum payments are set by the coverage limit of the required bond, $\beta$, which is chosen by the state. Insurers sell surety bonds at a price $\pi$ that just covers expected losses plus underwriting expenses,

$$ (3) \quad \pi = \int_{pq}^{\beta+pq} [v - pq] f(v) \, dv + \int_{\beta+pq}^{\bar{h}k} \beta f(v) \, dv + u(k). $$

If damages are less than $pq$, the firm bears these costs and the insurer pays nothing. If damages exceed $pq$, the firm declares bankruptcy and contributes its assets $pq$ toward environmental costs. The first term in equation (3) represents the insurer’s losses when damages are between $pq$ and $\beta + pq$. The second term represents the insurer’s losses when damages exceed $\beta + pq$, and the insurer pays $\beta$.<sup>18</sup> Underwriting expenses are $u$.

PROPOSITION 3: *An insurance mandate improves incentives for judgment-proof producers and improves industry composition. Low-technology firms produce fewer projects with quality below $\underline{\rho}$ and above $\bar{\rho}$. Low-technology firms exercise greater safety effort on projects they do produce.*

Insurers will charge high premia to firms with few assets relative to their level of environmental risk. At the other extreme, the insurer will never pay out if the firm has assets greater than worst-case damages $\bar{h}k$, because the firm internalizes all possible environmental costs. In this case, the surety bond price will reflect only underwriting costs. Thus, one important factor in determining insurance premiums will be the ratio of assets to expected environmental costs.

Insurers will also price contracts based on observable indicators of safety effort, for example accidents and regulatory compliance. This risk-based pricing leads firms to increase their safety effort.<sup>19</sup> Firms have an incentive to avoid accidents and regulatory violations by exerting greater safety effort, up to the point that the reduction in premiums equals the marginal cost of safety effort.

\*<sup>17</sup>The model also predicts a decrease in type *H* firms since their share of projects decreases. A more realistic view of very large firms may be that average cost is essentially flat above some minimum efficient scale, so that these adjustments in large-firm output happen through changes in firm-level output, not the number of large firms (especially when the difference between “small” and “large” firms is large) (Bain 1956).

\*<sup>18</sup>So, unless the bond is set at or above worst-case damages, insurers also do not fully internalize expected damages.

\*<sup>19</sup>Previous theoretical work by Shavell (1986) shows that requiring full insurance leads to the optimal level of safety effort when effort is perfectly observable. When effort is imperfectly observable, the model of Shavell (1979) shows that the insurance requirement will also increase safety effort, although the resulting effort level may be less than $x^*$ (with insurance premiums reflecting this higher level of expected damages).

402 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

The bond requirement changes the industry in several ways. Because firms that were previously judgment-proof increase their safety effort, expected environmental costs fall while these firms’ private costs increase. Internalization of expected environmental costs changes the division of projects between large and small firms, reducing the number of high-quality projects (projects above $\bar{\rho}$) that are inefficiently produced by low-technology firms. The number of projects with quality below $\underline{\rho}$, which generate negative social surplus, decreases as previously judgment-proof firms internalize the social costs of these projects.<sup>20</sup>

A bond requirement could also inefficiently exclude some firms. Underwriting or other transaction costs increase bond prices above the insurer’s expected losses, potentially excluding some firms that would otherwise operate profitably. I return to this topic in Section VI.

## C. Testable Predictions

My empirical application examines the introduction of an insurance requirement for oil and gas producers in Texas. This model predicts that such a policy change will have several potentially observable effects. At the individual project level, industry composition will respond along the two margins described in Proposition 2.

*Prediction I.*—Some high-quality projects will be reallocated from small firms to large firms. These projects were inefficiently operated by small firms prior to the policy (i.e., projects with quality above $\bar{\rho}$).

*Prediction II.*—Some low-quality projects operated by small firms will be shut down. These are projects that would not be profitable for any fully accountable firm (i.e., projects with quality below $\underline{\rho}$).

The model also predicts changes in industry composition at the firm level.

*Prediction III.*—The number of small firms will decrease as the number of projects efficiently operated by small firms falls, following Proposition 2.

*Prediction IV.*—The decrease in industry participation will be larger for firms with observable indicators of low safety effort, such as poor historical environmental compliance, following the argument in Proposition 3.

*Prediction V.*—Production by remaining small firms will decrease as they shut down or divest projects that are no longer privately efficient.<sup>21</sup>

Finally, there will be changes in environmental outcomes.

\*<sup>20</sup>This last prediction is similar to a result in a theoretical paper by Polborn (1998), showing that requiring insurance corrects firms’ decisions about whether to engage in production.

\*<sup>21</sup>For simplicity, the description of the model assumes that firms specialize in projects of a single quality level. The online Appendix relaxes this to allow firms to operate multiple project types. In the empirical application, firms tend to operate wells of similar but not identical quality, so that previously judgment-proof producers may choose to divest or shut down some wells while continuing to operate others.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 403

*Prediction VI.—Environmental outcomes will improve for projects operated by small firms, following Proposition 3.*

These predictions apply to small firms. The market that I examine also includes large firms with very low risk of environmental bankruptcy. The model implies that large firms will be unaffected by the policy, beyond absorbing some projects from small firms. This is an additional testable prediction.

## III. Empirical Setting

The empirical analysis focuses on onshore oil and gas extraction in Texas. This setting is well suited to studying liability and bankruptcy. The industry involves major environmental risks and many small producers. Output is homogeneous and the industry is essentially perfectly competitive due to liquid international markets. Texas is the largest oil- and gas-producing state in the United States, allowing for meaningful statistical analysis.

Measuring the effects of the judgment-proof problem requires variation in firms’ ability to avoid liability that is uncorrelated with unobserved determinants of environmental outcomes and industry structure. This is a difficult empirical problem. Deep-pocketed firms may also be more experienced or skilled than financially-weak producers, invalidating simple cross-sectional comparisons. Similarly, comparisons across jurisdictions with different liability regimes may be biased if accident-prone firms locate in areas with weak liability rules. This paper addresses the identification problem by exploiting the introduction of a bond requirement for oil and gas producers in Texas during 2002–2003. When bonds are required, small firms are less able to escape environmental costs through bankruptcy. This policy change provides an opportunity to cleanly measure the effects of the judgment-proof problem.

Texas has changed its bond requirements for oil and gas producers twice. The first time was in 1991, when bond requirements were introduced for firms with poor environmental records. This rule affected a small share of firms. Most could avoid the requirement by paying a $100 annual fee called the “Good Guy Fee” or other minor fees. The second change was in 2001. The Texas Legislature passed Senate Bill 310, which required bonds for all oil and gas producers. This industry-wide bond requirement is the primary focus of my empirical analysis. The rule required firms to post assets that could be seized by the regulator in the event of environmental damage. The bond applied to all types of water pollution, requiring that “all oil and gas activities and operations shall be carried out so as to prevent pollution of any ground or surface water in the state.”<sup>22</sup> Firms could deposit their own assets with the regulator or purchase a surety bond from a private insurer at market premiums. As described in Section I, surety bonds obligate the insurer to pay the state up to the value of the bond if the insured firm goes out of business and leaves some environmental cost. Ninety-seven percent of firms chose surety bonds.

The bond amount required from each firm depended on the number and depth of wells operated. The formula was $2 per foot of well depth across all wells. Producers

<sup>22</sup>Railroad Commission of Texas, Form P-5PB(2), June 2002.

404 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

could also cover a number of wells with a “blanket bond.” Up to 10 wells could be covered with a $25,000 blanket bond; 11 to 99 wells with a $50,000 blanket bond; and over 100 wells with a $250,000 blanket bond.

The annual premium that insurers charge for a surety bond is typically 1–2.5 percent of the amount of the bond. However, for firms that are deemed to be high-risk because they are financially weak or have a poor safety record, premiums can exceed 10–15 percent and insurers may also require collateral (Gerard 2000, Boyd 2002, Kaiser and Snyder 2009, Gerard and Wilson 2009). As a concrete example, premiums for an operator with a combined 10,000 feet of well depth (2–3 wells) could range from $200 per year for low-risk firms to $3,000 per year plus collateral requirements for the highest-risk firms. Thus, the primary effect of the bond requirement was to increase the operating costs of firms that the market perceived to have a high risk of environmental damage and insolvency. Gerard and Wilson (2009) report that 48 private insurers operated in this market in Texas.<sup>23</sup>

The requirement was phased in over 12 months. Each firm had to comply at its first annual license renewal between March 2002 and February 2003.<sup>24</sup> These license renewal dates are determined by the regulator based on the date the firm was created and cannot be manipulated by firms.

The salience of the bond requirement to producers was reflected in news coverage. Bonding was controversial because of a perception that it pushed out small firms. An op-ed in the Midland Reporter Telegram stated, “Bonding is no problem for major oil companies and large publicly owned independent companies. The small independent, however, is finding bonding very difficult at best... If the Railroad Commission persists in its current bonding requirements it could put thousands of honest hardworking Mom and Pop operators out of business.” Under the headline, “Can’t Afford the Bond? Then Don’t Run a Well,” the San Antonio Express News editorialized, “Texas is better off if only companies that can afford to be responsible environmental stewards stay in the oil and gas business.”<sup>25</sup>

# IV. Data

I construct a novel dataset on market structure and environmental outcomes by merging several administrative databases from the Railroad Commission of Texas (RRC), the state agency that regulates oil and gas production. Online Appendix Section J describes the creation of the dataset in detail. The final merged dataset includes 257,318 leases active during 1993–2012, and 35,568,267 lease-month observations. A lease is a parcel of land on which the producer has negotiated

\*<sup>23</sup>In principle, the coverage formula could have favored consolidation since “blanket bonds” allow firms to operate additional wells without additional coverage. However, unless insurers judged a firm to be an environmental risk, bond premia were small compared to other costs and benefits of merging. Most firms with just a few wells continued operating as before. After the policy change, 45 percent of remaining firms still had three or fewer wells. Consolidation to take advantage of blanket bonds would also not explain the systematically poor environmental records among exiting firms, improvements in environmental outcomes among remaining small firms or the shutting down of low-producing, high-risk wells.
\*<sup>24</sup>As an interim measure through 2004, firms were technically allowed to avoid bonding by paying an annual fee equal to 12.5 percent of the required coverage amount. This had little effect since this fee was more than the private bond premiums faced by all but the very riskiest firms.
\*<sup>25</sup>“PBPA Members Detailing Problems Getting Bonds,” Midland Reporter Telegram, March 31, 2002; and “Can’t Afford the Bond? Then Don’t Run a Well,” San Antonio Express News, August 9, 2002.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 405

production rights, and includes one or more wells. I observe monthly production of crude oil and natural gas for each lease. Each lease-month observation is associated with one of 10,489 producers.<sup>26</sup> I observe the entry and exit dates for these producers based on their annual license applications. I define entry as the date of first application, and exit as 12 months after the final license renewal.

Environmental outcomes data also come from the RRC. Orphan well data come from the Railroad Commission’s March 2014 Orphan Well List. Data on rules violations come from records of field inspection violations. I consider violations related to Statewide Rules 8 and 14. Statewide Rule 8 (“Water Protection”) governs water quality protection during drilling and production. Statewide Rule 14 (“Plugging”) requires that idle wells be plugged properly and promptly to prevent pollution.<sup>27</sup> Orphan wells and rules violations are merged to the production data using unique operator identification numbers. Blowout data come from the RRC records of blowouts and well control problems. The blowout data are not consistently formatted, requiring me to merge on multiple fields as explained in the online Appendix. The final dataset includes 3,404 orphan wells, 11,101 violations, and 427 well blowouts during 1993–2012.

# V. Results

This section describes the empirical results. I begin in Section VA with a descriptive summary of environmental incidents according to firm size, showing how small firms account for the majority of total environmental damage and highlighting the potential importance of limited liability. I then proceed to the formal empirical analysis, which is guided by the predictions from the theoretical model. Section VB considers the predicted changes in industry composition at the firm level. Then Section VC leverages the lease-level production and ownership data to examine the predicted channels for these changes. Section VD examines the predicted changes in environmental outcomes. The nature of the data and predicted responses vary somewhat across the outcomes in Sections VB–VD, which motivates some differences in the empirical strategy. I discuss these differences in each section. Finally, Section VE discusses additional robustness checks using an alternative empirical design, which are consistent with the main results.

## A. Descriptive Evidence of Incident Rates for Small Firms

Figure 2 shows the share of total environmental damage associated with firms of different sizes. A large share of total environmental incidents come from the

\*<sup>26</sup>Every lease is assigned to one legally-identified operator, which is the firm that is responsible to the state for all compliance and environmental costs. Throughout the paper, I identify leases according to operatorship. There may be additional ownership interests in a given lease. In a robustness check in online Appendix Section F, I collapse operators that are known to be related through ownership into 9,888 firm groups and reproduce the main results with no meaningful differences.

\*<sup>27</sup>I focus on these rules to exclude minor procedural violations, such as paperwork delays or improper signage. Example violations include: “Well #3 Inactive/Unplugged. Well #2 open to atmosphere. Required signs not posted. Oil on ground inside and outside of firewall”; “Oil/oil saturated soil at well not cleaned up/remediated. Well inactive/unplugged. Vegetation on firewall a fire hazard”; and “Post signs; disk, turn or till oil saturated soil; equip well #1 w/ appr’d wellhead assembly; install a bradenhead observ valve.”

406 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

<table>
  <thead>
    <tr>
        <th>Cumulative share of production</th>
        <th>Orphan wells</th>
        <th>Rules violations</th>
        <th>Well blowouts</th>
        <th>Reference (Slope 1)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>0.0</td>
        <td>0.0</td>
        <td>0.0</td>
        <td>0.0</td>
        <td>0.0</td>
    </tr>
    <tr>
        <td>0.2</td>
        <td>1.0</td>
        <td>0.98</td>
        <td>0.41</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>0.4</td>
        <td>1.0</td>
        <td>1.0</td>
        <td>0.58</td>
        <td>0.4</td>
    </tr>
    <tr>
        <td>0.6</td>
        <td>1.0</td>
        <td>1.0</td>
        <td>0.78</td>
        <td>0.6</td>
    </tr>
    <tr>
        <td>0.8</td>
        <td>1.0</td>
        <td>1.0</td>
        <td>0.85</td>
        <td>0.8</td>
    </tr>
    <tr>
        <td>1.0</td>
        <td>1.0</td>
        <td>1.0</td>
        <td>1.0</td>
        <td>1.0</td>
    </tr>
  </tbody>
</table>


FIGURE 2. ENVIRONMENTAL INCIDENTS AND FIRM SIZE

*Notes:* For each firm, I calculate oil and gas production from March 1996 to February 2002 in barrel-of-oil equivalents. The horizontal axis represents cumulative production for all firms. Firms are ordered from left to right on this axis in order of average annual production. The vertical axis represents cumulative environmental incidents during this same period. The gray dashed line has a slope of 1.

smallest producers. The horizontal axis shows cumulative pre-period production across all firms. Operators are ordered along this axis from smallest to largest annual production so that, for example, 0.2 represents the 20 percent of total output that comes from the smallest firms. The vertical axis shows the cumulative share of environmental incidents. The dashed gray line has a slope of 1. One hundred percent of orphan wells, 98 percent of field rules violations, and 41 percent of blowouts are associated with the 20 percent of production that comes from the smallest firms. The strong concentration of environmental incidents among small producers illustrates the potential significance of limited liability problems in this industry.

## B. Industry Composition Effects at the Firm Level

I begin the presentation of the empirical results by considering the effects of the bond requirement on firms. Theory predicts that the bond requirement will affect the participation decisions and output of small firms. In this section I use annual operating license data and firm-level production to investigate these effects.

*Participation.*—Figure 3 shows the raw data on exit. The dots represent the number of firms leaving the market each month. Prior to the introduction of the partial bond requirement in September 1991, about 40–50 firms exited each month. With the bond requirement, exit increases sharply to over 100 firms per month and stays high for 12 months, then decreases sharply. The same pattern accompanies implementation of the universal bond mandate. In March 2002, exit almost doubles from about 60 to over 100 firms per month.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 407

![Scatter plot showing the number of firms exiting by month from 1987 to 2011, with annotations for partial and universal bond requirements.](page_17_image_1_v2.jpg)

FIGURE 3. NUMBER OF FIRMS EXITING BY MONTH

Notes: Exit date is 12 months after final annual license renewal. Vertical dashed lines show September 1991–August 1992 and March 2002–February 2003. Includes firms with production during 1990–2012; see online Data Appendix for description of 1990–1992 production data.

I measure the effect of the bond requirement by comparing exit rates among firms scheduled to renew their annual licenses just before and just after the policy took effect. Firms make this exit decision once per year, during the month assigned to them by the regulator. Importantly, the timing of these license renewal decisions is fixed in time by the firm's assigned renewal month. Firms scheduled to renew immediately prior to implementation of the bond requirement could remain in the industry for another year at no additional cost. Firms scheduled to renew after implementation faced a discrete, permanent increase in operating costs. A regression discontinuity (RD) estimator is well suited to measuring the effect of this discrete change. The estimating equation is

$$ (4) \quad \mathbf{1}[Exit]_{it} = \alpha + \beta_1 \mathbf{1}[Implemented]_t + \beta_2 T_t + X_t \beta_3 + \eta_{it}. $$

The dependent variable $\mathbf{1}[Exit]_{it}$ is an indicator variable equal to 1 if firm $i$ leaves the industry in month $t$. After firms exit they are removed from the sample. The variable $\mathbf{1}[Implemented]_t$ is an indicator variable equal to 1 in March 2002 and all later months; $T_t$ is a flexible function of the running variable, month. Covariates $X_t$ are included in some specifications to increase precision. They include monthly crude oil prices and month-of-year fixed effects.<sup>28</sup> The function $T_t$ is centered at

\*<sup>28</sup>Crude oil and natural gas prices are highly correlated during this period (correlation coefficient 0.8). To simplify interpretation of the price effects, I include oil prices only.

408 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

<table>
  <thead>
    <tr>
        <th>Month</th>
        <th>Share of firms exiting, residuals</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1998:1</td>
        <td>-0.015</td>
    </tr>
    <tr>
        <td>1999:7</td>
        <td>0.005</td>
    </tr>
    <tr>
        <td>2001:1</td>
        <td>0.008</td>
    </tr>
    <tr>
        <td>2002:7</td>
        <td>0.065</td>
    </tr>
    <tr>
        <td>2004:1</td>
        <td>-0.020</td>
    </tr>
    <tr>
        <td>2005:7</td>
        <td>0.005</td>
    </tr>
  </tbody>
</table>


FIGURE 4. THE EFFECT OF THE BOND REQUIREMENT ON EXIT

Notes: The sample includes March 1998 to February 2006. There is one observation per firm per year in the assigned license renewal month. Dots are monthly means of residuals from a regression of 1[Exit] on month-of-year fixed effects and monthly crude oil prices. The red fitted curves show separate quadratic polynomials for before and after March 2002, allowing an additional intercept after February 2003 for the end of the rollout period.

March 2002, so the constant term $\alpha$ gives the level of exit immediately prior to the policy change. In specifications that include a bandwidth larger than 12 months around the implementation threshold, I include an additional dummy variable for observations after February 2003, the end of the 12-month implementation period.

The error term $\eta_{it}$ includes unobserved determinants of $1[Exit]_{it}$. The identifying assumption is that $\eta_{it}$ does not change discontinuously at implementation. Under that assumption, $\beta_1$ cleanly measures the effect of the bond requirement on exit. This assumption is supported by the similar 12-month patterns of exit observed for the 1991 and 2002 policies in Figure 3. There are clear discontinuous increases in exit during each period, suggesting that the effect in 2002 is unlikely to be caused by an unobserved idiosyncratic shock. The timing of the two rollouts differ (one starts in September, the other in March) and both split calendar years.

Renewal dates are assigned by the RRC and cannot be changed by firms. The bond requirement was implemented according to assigned renewal dates; firms could not avoid it by submitting their renewal paperwork early. Online Appendix Section B shows the usual McCrary (2008) check for manipulation of the running variable. As expected, there is no evidence of firms manipulating their license renewal dates.

Figure 4 shows graphical results. The horizontal axis shows months before and after March 2002. The dots are monthly means of the residuals from a regression of $1[Exit]_{it}$ on a constant term, month-of-year fixed effects, and monthly oil prices. The fitted curves show separate second-degree polynomials in the running variable before and after March 2002. There is a clear increase of about five percentage

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 409

points in the share of firms exiting in March 2002. At the end of the implementation period, there is a decrease in exit to approximately pre-implementation levels.<sup>29</sup>

Figure 5 shows the effect by quintile of firm-level average annual oil and gas production. These quintiles are defined based on average annual production prior to the policy change, in barrel of oil equivalents (BOE).<sup>30</sup> The effect of the bond requirement is largest for small firms. For the first two quintiles of firm size, there is a clear increase in exit of more than 10 percentage points in March 2002. In the third quintile, there is a smaller but still clear increase of about 7 percentage points. In the fourth quintile there is a small increase of about 3 percentage points. In contrast, in the top quintile of firm size there is no distinguishable change in the share of firms exiting in March 2002.

Table 1 shows regression estimates. Following Lee and Lemieux (2010), I present results using both local linear regression with a rectangular kernel function and flexible polynomials with wider estimation bandwidths. Column 1 reports estimates using local linear regression with a 12-month bandwidth. The estimated effect of the bond requirement is a discontinuous increase in the exit rate of 5.3 percentage points. The baseline rate of exit before the policy change, given by the constant term, was 10.8 percent. Column 2 adds controls for crude oil prices in the assigned license renewal month, with little effect on the estimates. Column 3 shows results using separate second-degree polynomials in the running variable on each side of the threshold, and a 48-month bandwidth. Column 4 uses the same second-degree polynomial and bandwidth and controls for prices and calendar month fixed effects. The estimates of the policy effect vary only slightly across specifications, ranging from 4.8 to 6.6 percentage points. None are outside the 95 percent confidence intervals of the other estimates, and all of them are statistically different from zero. Throughout the rest of the paper I use the local linear regression specification, as recommended by Gelman and Imbens (forthcoming). Following Lee and Card (2008), the standard errors are clustered according to the running variable, month of sample. This allows for arbitrary correlation of residuals across firms in each month.

Online Appendix Section B includes additional robustness checks and alternative estimates. These include (i) additional RD bandwidths and polynomials; (ii) alternative variance estimators including two-way clustering and Newey-West heteroskedasticity and autocorrelation consistent estimates; and (iii) a survival regression. It also discusses entry and net entry (entry minus exit).

Table 2 presents regression results according to firm size. The results are consistent with the graphical evidence in Figure 5. In the first and second quintiles exit increases by 9.3 and 12.6 percentage points, while in the third and fourth quintiles it increases by 5.8 and 3.0 percentage points. The estimated effect of the policy

\*<sup>29</sup>Exit is lower during the last three months of the implementation period (December 2002–February 2003), creating a negative slope in the fitted polynomial. During these months, a legal challenge temporarily limited full enforcement of the policy for firms that had not already renewed their licenses. On November 27, a Travis County Court judge issued a temporary order requiring the Railroad Commission to accept $1,000 cash payments in lieu of bonds from operators who qualified for the “Good Guy” exemption. The bond requirement was eventually upheld, but producers coming up for renewal during this period may have stayed in the market another year in hopes of the rule being overturned.
\*<sup>30</sup>Average annual production is calculated using data from March 1996 to February 2002. The cutoffs are twentieth percentile: 1,100; fortieth: 3,550; sixtieth: 10,420; eightieth: 43,396; Maximum: 87,330,104. These same quintile definitions are used throughout the paper.

410 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

<table>
  <thead>
    <tr>
        <th colspan="6">Figure 5. The Effect of the Bond Requirement on Exit, by Firm Size</th>
    </tr>
    <tr>
        <th>Month</th>
        <th>Panel A: 1st Quintile</th>
        <th>Panel B: 2nd Quintile</th>
        <th>Panel C: 3rd Quintile</th>
        <th>Panel D: 4th Quintile</th>
        <th>Panel E: 5th Quintile</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1998:1</td>
        <td>-0.04</td>
        <td>-0.03</td>
        <td>-0.01</td>
        <td>0.00</td>
        <td>-0.02</td>
    </tr>
    <tr>
        <td>1999:7</td>
        <td>-0.03</td>
        <td>-0.02</td>
        <td>-0.01</td>
        <td>0.00</td>
        <td>-0.01</td>
    </tr>
    <tr>
        <td>2001:1</td>
        <td>0.01</td>
        <td>0.00</td>
        <td>-0.01</td>
        <td>0.01</td>
        <td>0.01</td>
    </tr>
    <tr>
        <td>2002:7 (Pre)</td>
        <td>0.08</td>
        <td>0.03</td>
        <td>0.01</td>
        <td>0.01</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>2002:7 (Post)</td>
        <td>0.16</td>
        <td>0.14</td>
        <td>0.07</td>
        <td>0.04</td>
        <td>0.03</td>
    </tr>
    <tr>
        <td>2004:1</td>
        <td>0.13</td>
        <td>0.09</td>
        <td>0.05</td>
        <td>0.03</td>
        <td>0.02</td>
    </tr>
    <tr>
        <td>2005:7</td>
        <td>-0.02</td>
        <td>-0.02</td>
        <td>-0.01</td>
        <td>-0.01</td>
        <td>-0.01</td>
    </tr>
  </tbody>
</table>


Figure 5. The Effect of the Bond Requirement on Exit, by Firm Size

Notes: Each panel reproduces Figure 4 for a single quintile of the output distribution. Output is calculated based on average annual production during March 1996 to February 2002 in barrel-of-oil equivalents (BOE).

change on exit is close to zero in the largest quintile. The background level of exit also decreases across quintiles, from 22 percent to 6 percent.

Table 3 shows regression results according to past environmental compliance history. Column 1 shows results for firms with fewer than 0.2 past violations of

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 411

Table 1—Effect of the Bond Requirement on Exit


<table>
  <thead>
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
        <td><strong>1</strong>[Implemented]</td>
        <td>0.053</td>
        <td>0.066</td>
        <td>0.048</td>
        <td>0.050</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td>(0.013)</td>
        <td>(0.009)</td>
        <td>(0.009)</td>
    </tr>
    <tr>
        <td>Oil price</td>
        <td> </td>
        <td>−0.194</td>
        <td> </td>
        <td>−0.097</td>
    </tr>
    <tr>
        <td>($100/bbl)</td>
        <td> </td>
        <td>(0.093)</td>
        <td> </td>
        <td>(0.040)</td>
    </tr>
    <tr>
        <td>Constant</td>
        <td>0.108</td>
        <td>0.095</td>
        <td>0.101</td>
        <td>0.099</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.007)</td>
        <td>(0.007)</td>
        <td>(0.006)</td>
        <td>(0.007)</td>
    </tr>
    <tr>
        <td>Month-of-year fixed effects</td>
        <td>No</td>
        <td>No</td>
        <td>No</td>
        <td>Yes</td>
    </tr>
    <tr>
        <td>Specification</td>
        <td>Local linear<br/>regression</td>
        <td>Local linear<br/>regression</td>
        <td>Quadratic<br/>polynomial</td>
        <td>Quadratic<br/>polynomial</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>12,200</td>
        <td>12,200</td>
        <td>48,619</td>
        <td>48,619</td>
    </tr>
    <tr>
        <td>Firms</td>
        <td>6,512</td>
        <td>6,512</td>
        <td>8,854</td>
        <td>8,854</td>
    </tr>
  </tbody>
</table>


Notes: This table reports the results of four separate regressions. The data are a monthly panel with one observation per firm per year, in the firm’s assigned license renewal month. The dependent variable is an indicator variable equal to 1 in the month of exit. **1**[Implemented] is an indicator variable equal to 1 starting in March, 2002. Columns 1 and 2 show the results of local linear regressions with a 12-month bandwidth and a rectangular kernel function. Columns 3 and 4 use a separate quadratic polynomial on either side of the implementation date, and a 48-month bandwidth (March 1998 to February 2006). Columns 3 and 4 also include an indicator variable equal to 1 starting in March 2003 to capture the end of the implementation period. Oil prices are monthly average Texas first purchase prices in the month of license renewal, in hundreds of 2010 dollars. Standard errors are clustered by month.

Table 2—Effect of the Bond Requirement on Exit by Output Quintile


<table>
  <thead>
    <tr>
        <th> </th>
        <th>Quintile 1<br/>(1)</th>
        <th>Quintile 2<br/>(2)</th>
        <th>Quintile 3<br/>(3)</th>
        <th>Quintile 4<br/>(4)</th>
        <th>Quintile 5<br/>(5)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td><strong>1</strong>[Implemented]</td>
        <td>0.093</td>
        <td>0.126</td>
        <td>0.058</td>
        <td>0.030</td>
        <td>0.007</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.041)</td>
        <td>(0.015)</td>
        <td>(0.024)</td>
        <td>(0.020)</td>
        <td>(0.013)</td>
    </tr>
    <tr>
        <td>Constant</td>
        <td>0.220</td>
        <td>0.158</td>
        <td>0.091</td>
        <td>0.061</td>
        <td>0.063</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.016)</td>
        <td>(0.012)</td>
        <td>(0.020)</td>
        <td>(0.018)</td>
        <td>(0.008)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>1,872</td>
        <td>2,064</td>
        <td>2,220</td>
        <td>2,424</td>
        <td>2,557</td>
    </tr>
    <tr>
        <td>Firms</td>
        <td>1,022</td>
        <td>1,092</td>
        <td>1,157</td>
        <td>1,257</td>
        <td>1,325</td>
    </tr>
  </tbody>
</table>


Notes: This table reports estimates separately by output quintile for the regression specification shown in column 1 of Table 1. All regressions are estimated using local linear regression with a 12-month bandwidth and a rectangular kernel function. Standard errors are clustered by month.

Table 3—Effect of the Bond Requirement on Exit by Environmental Record


<table>
  <thead>
    <tr>
        <th> </th>
        <th>No or few violations<br/>(1)</th>
        <th>Many violations<br/>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td><strong>1</strong>[Implemented]</td>
        <td>0.054</td>
        <td>0.121</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.015)</td>
        <td>(0.040)</td>
    </tr>
    <tr>
        <td>Constant</td>
        <td>0.104</td>
        <td>0.189</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.009)</td>
        <td>(0.032)</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>10,064</td>
        <td>1,073</td>
    </tr>
    <tr>
        <td>Firms</td>
        <td>5,310</td>
        <td>543</td>
    </tr>
  </tbody>
</table>


Notes: This table reports estimates separately by compliance history for the regression specification shown in column 1 of Table 1. Column 1 includes firms with fewer than 0.2 past violations per lease operated, as of the assigned license renewal date. Column 2 includes firms with 0.2 or more. All regressions are estimated using local linear regression with a 12-month bandwidth and a rectangular kernel function. Standard errors are clustered by month.

412 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

Statewide Rules 8 or 14 per lease operated. This includes 90 percent of firms (79 percent of firms have zero past violations). Among this group, the probability of exit increased by 5.4 percentage points. Column 2 shows results for the 10 percent of firms with the highest violation rates. Among these firms, exit increased by 12.1 percentage points.

Overall, firms’ exit decisions are consistent with Predictions III and IV from the theoretical model. Many firms left the industry, as predicted. Prediction III states that exit should be concentrated among small firms, because environmental bankruptcy risk is a primary determinant of insurance premiums. Small firms have fewer assets overall and they also tend to operate wells that generate little revenue relative to the environmental risks that they present. Large firms are predicted to show no exit effect, since large firms in this setting internalize all or nearly all environmental costs and thus represent very little risk to the insurer. Prediction IV states that observable predictors of low safety effort, such as past environmental violations, should also be correlated with exit. Tables 2 and 3 are consistent with all of these predictions.<sup>31</sup>

*Firm-Level Oil and Gas Production.*—This section considers the effect of the bond requirement on output among firms that remained in the industry and became bonded. Theory predicts that remaining small firms’ output will decrease in the short run, as they sell some projects to larger producers or shut down projects that are no longer profitable after internalizing environmental costs.

The output data differ in some ways from the license renewal data, which affects the empirical strategy. License renewal decisions are restricted to a specified month by the regulator, but firms are free to adjust output at any time. Since firms might have altered their production during the months before or after their assigned license renewal month, it is not appropriate to consider the discrete change between the final month of the pre-period and the first month of the implementation period as the impact of the policy. Furthermore, production is observed every month for every firm. This suggests a method that can exploit the variation in when firms became subject to the policy, comparing contemporaneous output of firms that have already had to comply and firms that have not yet had to comply. I use a difference-in-differences design that takes advantage of this 12-month quasi-randomized rollout (this design is often called an event study, following Jacobson, LaLonde, and Sullivan 1993 and subsequent studies). It leverages the panel nature of the monthly production data through firm and time fixed effects, which reduce noise and allow me to separate the effect of the bond requirement from other time-varying factors like oil and gas prices. I run the following regression:

$$ (5) \qquad \ln(Production_{it}) = \gamma + \psi \mathbf{1}[Bonded]_{it} + \delta_i + \tau_t + \nu_{it}. $$

\*31 Table 3 also shows that some firms with few violations left the industry, albeit at a lower rate than firms with many violations. This suggests that violations (or lack of them) provide a noisy measure of safety effort to insurers, which is not surprising. Inspections are relatively infrequent and not all unsafe operations are cited. Furthermore, firms with few assets relative to potential damages can represent a large risk for insurers even if the firms exercise high levels of precaution, since even relatively small damage outcomes can bankrupt these firms.

VOL. 109 NO. 2 *BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW* 413

Here, $1[Bonded]_{it}$ is an indicator variable equal to 1 for firm-month observations after firm $i$’s assigned license renewal date; $Production_{it}$ is the firm’s production (in barrel-of-oil equivalents) in month $t$; $\delta_i$ is a firm fixed effect, and $\tau_t$ is a month fixed effect. The sample is limited to the 12 months during the implementation year, and to firms that stayed in the industry after the bond mandate. In my preferred specification I interact $1[Bonded]_{it}$ and the month fixed effects with a categorical variable for firm size quintile. This estimates the effect of bonding separately for each size group, and allows for separate arbitrary time trends in production within each output quintile.

The identifying assumption in this analysis is that $\nu_{it}$ is independent of $1[Bonded]_{it}$, conditional on $\delta_i$ and $\tau_t$. As explained previously, $1[Bonded]_{it}$ is entirely determined by the firm’s assigned license renewal date and these dates are not manipulable by firms. As a further check on this assumption, online Appendix Section C includes a comparison of average output and number of leases across renewal month groups. There is no evidence of differences across groups.

Table 4 shows the effect of becoming bonded on production among remaining firms.<sup>32</sup> Column 1 shows the pooled estimate. Production decreases by 3.6 percent on average after firms become bonded. Column 2 estimates separate effects for firms above and below the eightieth percentile of firm size. Small firms reduce their production by about 4.8 percent, while the point estimate for large firms is near zero. Finally, Column 3 estimates separate effects for each quintile. There is a clear decreasing pattern with size, though the estimates are noisy because of how thinly the data have been divided in this final column. The largest estimated effects are in the bottom three quintiles: 10.8 percent, 5.9 percent, and 4.3 percent.

Online Appendix Section C includes additional validity checks, including an event study figure and a placebo analysis of the effect of license renewal on firm-level oil and gas production in other years. The placebo analysis shows no change after license renewal in other years. This test further supports the exogeneity of the timing of license renewal, since any omitted variables related to license renewal would also be expected to affect estimates for other years.

Anderson, Kellogg, and Salant (2018) show that production rates for oil wells are determined by geology, with little scope for intensive-margin adjustment by producers. Thus, these observed changes in production imply that small firms shut in some high-risk wells, sold wells to other producers, and/or decreased the rate at which they drilled or acquired new wells. This is consistent with Prediction V from the theoretical model. Prior to the bond requirement, small firms operated projects that they chose not to operate after internalizing environmental costs. On average, small firms that became bonded decreased their output by 4.8 percent. Large firms did not change their output after becoming bonded, again consistent with theoretical expectations.

\*32 Since some firms left the industry in response to the policy, these estimates should not be interpreted as the average output elasticity for the pre-policy firm population. Equation (5) consistently estimates the effect on output among firms that chose to remain in the industry. These estimates will be smaller in absolute value than the overall average output elasticity in the pre-policy firm population, unless one assumes (implausibly) that firms that chose to exit would have had a less negative output response than the firms that chose to stay.

414 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

# Table 4—Effect of Bonding on Oil and Gas Production for Bonded Firms


<table>
  <thead>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td rowspan="2">1[Bonded]</td>
        <td>-0.036</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td>(0.013)</td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintiles 1–4</td>
        <td> </td>
        <td>-0.048</td>
        <td> </td>
    </tr>
    <tr>
        <td>(0.017)</td>
        <td> </td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintile 1</td>
        <td> </td>
        <td> </td>
        <td>-0.108</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.065)</td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintile 2</td>
        <td> </td>
        <td> </td>
        <td>-0.059</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.036)</td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintile 3</td>
        <td> </td>
        <td> </td>
        <td>-0.043</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.032)</td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintile 4</td>
        <td> </td>
        <td> </td>
        <td>-0.018</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.018)</td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">1[Bonded] × Quintile 5</td>
        <td> </td>
        <td>-0.004</td>
        <td>-0.004</td>
    </tr>
    <tr>
        <td>(0.016)</td>
        <td>(0.016)</td>
        <td></td>
    </tr>
    <tr>
        <td rowspan="2">Constant</td>
        <td>6.647</td>
        <td>6.646</td>
        <td>6.639</td>
    </tr>
    <tr>
        <td>(0.014)</td>
        <td>(0.014)</td>
        <td>(0.013)</td>
    </tr>
    <tr>
        <td>Firm fixed effects</td>
        <td>Yes</td>
        <td>Yes</td>
        <td>Yes</td>
    </tr>
    <tr>
        <td>Month-by-quintile fixed effects</td>
        <td>Yes</td>
        <td>Yes</td>
        <td>Yes</td>
    </tr>
    <tr>
        <td>Observations</td>
        <td>45,539</td>
        <td>45,539</td>
        <td>45,539</td>
    </tr>
    <tr>
        <td>Firms</td>
        <td>4,467</td>
        <td>4,467</td>
        <td>4,467</td>
    </tr>
  </tbody>
</table>


Notes: This table reports the results of three separate regressions. The dependent variable is the log of monthly oil and gas production in barrel-of-oil equivalents. The sample is limited to March 2002–February 2003, and to firms that renewed their annual operating license during that period. **1**[Bonded] is an indicator variable equaling 1 in all months after a firm’s license renewal month and 0 in all months before; the month of license renewal is dropped. Standard errors are clustered at the operator level.

## C. Industry Composition Effects at the Individual Lease Level

In this section, I turn to the data on individual oil and gas leases to examine compositional effects in more detail. The theoretical model identifies two margins of adjustment at the project level. Prediction I is that there will be a reallocation of relatively high-quality projects from small to large firms (“transfers”). Prediction II is that there will be a reduction in low-quality projects operated by small firms (“shut-ins”).

Table 5 compares the rates of transfers and shut-ins during the implementation period to the average rates in surrounding years. Within each panel of the table, I report results separately by size of the firm that owned the lease at the beginning of each 12-month period. The smallest 60 percent of firms are pooled to increase precision, since these small firms own relatively few leases. Column 1 shows the number of lease-year observations in each group.

Panel A of the table shows transfers. Column 2 reports the share of all leases that changed operators during the implementation year, and Column 3 reports the average rate of operator changes during each of the three years preceding and three years following the implementation year. Column 4 shows the difference between 2 and 3, or the level of “excess” ownership transfers during the implementation year. The level of excess transfers for the smallest firms was 6.6 percentage points, a difference that is

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 415

Table 5—Industry Composition Effects at the Individual Lease Level


<table>
  <thead>
    <tr>
        <th>Firm size quintiles</th>
        <th>Lease-year observations (1)</th>
        <th>Implementation year rate (%) (2)</th>
        <th>Baseline in adjacent years (%) (3)</th>
        <th>Excess (%) (2) − (3) (4)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="5"><em>Panel A. Transfers, all leases</em></td>
    </tr>
    <tr>
        <td>1–3</td>
        <td>83,310</td>
        <td>16.1</td>
        <td>9.4</td>
        <td>6.6</td>
    </tr>
    <tr>
        <td>4</td>
        <td>86,442</td>
        <td>11.2</td>
        <td>9.8</td>
        <td>1.4</td>
    </tr>
    <tr>
        <td>5</td>
        <td>261,090</td>
        <td>10.0</td>
        <td>10.8</td>
        <td>−0.8</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="5"><em>Panel B. Transfers, high-quality leases</em></td>
    </tr>
    <tr>
        <td>1–3</td>
        <td>9,621</td>
        <td>13.3</td>
        <td>4.0</td>
        <td>9.3</td>
    </tr>
    <tr>
        <td>4</td>
        <td>11,505</td>
        <td>11.0</td>
        <td>10.2</td>
        <td>0.8</td>
    </tr>
    <tr>
        <td>5</td>
        <td>92,576</td>
        <td>11.2</td>
        <td>11.6</td>
        <td>−0.4</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="5"><em>Panel C. Shut-ins, all leases</em></td>
    </tr>
    <tr>
        <td>1–3</td>
        <td>83,310</td>
        <td>6.2</td>
        <td>4.2</td>
        <td>2.0</td>
    </tr>
    <tr>
        <td>4</td>
        <td>86,442</td>
        <td>3.8</td>
        <td>3.7</td>
        <td>0.0</td>
    </tr>
    <tr>
        <td>5</td>
        <td>261,090</td>
        <td>4.4</td>
        <td>4.1</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="5"><em>Panel D. Shut-ins, low-quality leases</em></td>
    </tr>
    <tr>
        <td>1–3</td>
        <td>35,787</td>
        <td>9.1</td>
        <td>6.7</td>
        <td>2.4</td>
    </tr>
    <tr>
        <td>4</td>
        <td>27,734</td>
        <td>5.9</td>
        <td>5.7</td>
        <td>0.2</td>
    </tr>
    <tr>
        <td>5</td>
        <td>38,433</td>
        <td>7.4</td>
        <td>7.3</td>
        <td>0.1</td>
    </tr>
  </tbody>
</table>


Notes: This table reports transfers and shut-ins of oil and gas leases by year and size of firm. Implementation year includes March 2002 to February 2003. Adjacent years include 36 months before and 36 months after the implementation year. Firm size is the size of the firm owning the lease at the start of the year. Panels A and B show the share of oil and gas leases with a change in operator. In panels C and D, shut-in dates are defined as the first month of a 24-month or longer period of no output from a previously producing lease. To focus on movements that were plausibly related to the bond requirement, this table ignores leases owned at the beginning of the year by the largest 2 percent of firms. This removes a handful of mergers involving thousands of leases.

statistically significant at the 1 percent level.<sup>33</sup> For fourth-quintile firms, the level of excess transfers was 1.4 percentage points. Among the largest firms transfer rates were similar across years. Theory also predicts that the projects reallocated from small to large producers by the policy will be relatively high quality. Panel B uses lease-level production rates as an indicator of lease quality to investigate this prediction. The sample in this panel is limited to the highest-producing 25 percent of leases in the state. The rate of excess transfers for high-quality leases owned by the smallest firms was 9.3 percent, confirming that excess transfers were particularly high for high-quality leases.

Panel C shows shut-ins. I define the shut-in date of a lease as the first month of a period of at least 24 months with no production. Among the smallest 60 percent of firms, the share of previously active leases that stopped producing was 2.0 percentage points higher than surrounding years. Fourth- and fifth-quintile producers shut in leases at roughly the same rate as in surrounding years. Panel D limits the sample to the lowest-producing 25 percent of leases in the state. These leases produce fewer than 40 BOE per month. Low-producing wells have high per-barrel costs, including environmental costs. Many of these wells produce large amounts of saltwater, which

<sup>33</sup>Inference in each row of the table is based on p-values from a regression of an indicator for transfer or shut-in on 1[*ImplementationYear*], with standard errors clustered at the firm level. Excess transfers and shut-ins for the smallest 60 percent of firms are statistically different from zero at least the 5 percent level in all four panels of the table. Differences are not statistically different from zero for fourth- or fifth-quintile firms, although the point estimates for fourth-quintile firms suggest small positive effects on transfers.

416 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

is costly to manage safely. They are also often old and thus at higher risk of leaks. Among the smallest firms, the level of excess shut-ins on low-quality leases was 2.4 percentage points (compared to 1.0 for all other leases operated by these firms, which are not reported separately in the table). Again, fourth- and fifth-quintile firms shut in low-quality leases at similar rates across years.

Industry composition responded to the bond requirement through both of the channels predicted by theory. Moreover, this method likely understates the effect of the policy on projects. Some oil leases contain more than one well; individual well shut-ins on multi-well leases will not be captured by this method. In addition, these results do not include future projects that would have been drilled or acquired by small firms in the absence of the policy. Thus, I interpret these results as a lower bound on the effects of the policy at the project level.

It is also possible to implement the event study design from Section VB for transfers and shut-ins. I discuss these results in online Appendix Section D. Transfers during the implementation year were concentrated during the months just before the selling firm's license renewal month, as would be expected if these excess transfers were related to the bond requirement. The power of the event study design to detect shut-in effects at the individual lease level is limited, since these are relatively uncommon events.

## D. Environmental Outcomes

This section examines the effect of the bond requirement on orphan wells, water protection rules violations, and well blowouts. These incidents are relatively infrequent, so I aggregate multiple years of pre- and post-program data to allow for empirically meaningful comparisons. I compare outcomes before and after the bond requirement, with a number of robustness checks to control for potential confounders and time trends.

*Orphan Wells.*—Orphan wells are a primary cause of groundwater pollution and a source of atmospheric methane emissions. From an empirical perspective, orphan wells are an attractive measure of environmental effort because there is no scope for underreporting or differential enforcement. The Railroad Commission knows the location of all modern oil and gas wells from drilling permit information. When wells are plugged, operators must submit certification of plugging by an RRC-approved cementing company.

Figure 6 shows the rate of well orphaning over time. The dots represent the share of exiting firms each month that left orphan wells. The dashed vertical line shows March 2003, the month after the end of the implementation period. This is the first month in which exiting firms would have been bonded under the policy (as of this month, all firms would have renewed their annual license). The horizontal gray lines show two-year averages before and after. Before bonding, about 7 percent of exiting operators left behind orphan wells. After, the average is about 3 percent, and in many months no exiting operators leave orphan wells. There is a clear fall in well orphaning exactly coincident with the bond requirement. Online Appendix Section E shows similar results for the number of orphan wells per lease operated and the total number of orphan wells, both of which show the same clear decrease.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 417

![Scatter plot showing the share of exiting operators orphaning wells by month from 1998 to 2008, with a vertical dashed line indicating that exiting firms are affected after February 2003. Gray horizontal bars show the mean share before and after this date.](page_27_image_1_v2.jpg)

FIGURE 6. SHARE OF EXITERS ORPHANING WELLS, BY MONTH

Notes: This figure shows the share of exiting firms each month that left orphan wells. The gray horizontal bars show means for two years before and two years after March 2003.

Table 6 shows regression estimates of the change in well orphaning. The sample includes firms that left the industry between 2000 and 2006. There is one observation per firm, in its exit month. The dependent variable is the number of orphan wells divided by the number of leases operated. The first column includes all firms. The constant term in this regression is 0.076, implying that prior to the bond requirement the average firm generated 0.076 orphan wells for each lease. The coefficient on **1**[Bonded] is $-0.052$, which represents a 68 percent decrease following the bond mandate. The next five columns show separate regressions by size quintile. The constant terms in each column show that prior to the bond requirement, rates of well orphaning were highest among small firms. Firms in the first quintile created 0.137 orphan wells per lease. These rates decrease across columns. The estimate for the fifth quintile is 0.017, over eight times smaller.<sup>34</sup> After the bond requirement, there were striking decreases in well orphaning for small firms. In the first quintile, the coefficient on **1**[Bonded] is $-0.087$. The decreases in the second and third quintiles are also large and statistically significant. The fourth quintile estimate is substantially smaller and the fifth quintile estimate is near zero. Online Appendix Section E includes additional specifications and robustness checks, including alternative sample periods and a time trend.

This large decrease in well orphaning exclusively among small producers is consistent with theory (Prediction VI). After the policy change, all firms still in the industry had been judged to be acceptable risks by some insurer, based on their

<sup>34</sup>The few fifth-quintile firms with orphan wells are among the smallest firms in this group, just above the quintile cutoff.

418 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

Table 6—Effect of the Bond Requirement on Orphan Wells


<table>
  <thead>
    <tr>
        <th> </th>
        <th> </th>
        <th colspan="5">By firm size quintile</th>
    </tr>
    <tr>
        <th> </th>
        <th>All firms</th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td><strong>1</strong>[Bonded]</td>
        <td>−0.052</td>
        <td>−0.087</td>
        <td>−0.058</td>
        <td>−0.049</td>
        <td>−0.011</td>
        <td>−0.007</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.013)</td>
        <td>(0.045)</td>
        <td>(0.020)</td>
        <td>(0.018)</td>
        <td>(0.021)</td>
        <td>(0.011)</td>
    </tr>
    <tr>
        <td>Constant</td>
        <td>0.076</td>
        <td>0.137</td>
        <td>0.082</td>
        <td>0.059</td>
        <td>0.046</td>
        <td>0.017</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.011)</td>
        <td>(0.039)</td>
        <td>(0.017)</td>
        <td>(0.016)</td>
        <td>(0.011)</td>
        <td>(0.008)</td>
    </tr>
    <tr>
        <td>Firms</td>
        <td>2,777</td>
        <td>653</td>
        <td>583</td>
        <td>456</td>
        <td>411</td>
        <td>422</td>
    </tr>
  </tbody>
</table>


Notes: This table reports six separate regressions. The sample includes firms that exited between March 2000 and February 2006. The dependent variable is the number of orphan wells left by the firm, divided by the number of leases operated. There is one observation per firm, in its month of exit. Standard errors are clustered by month.

financial position and compliance history. Insurers are able to continuously monitor firms’ assets, production, and environmental liabilities over time. If the firm accumulates many non-producing wells or its net assets begin to decrease, the insurer can raise premiums. This prevents firms from accumulating unsustainable plugging or other environmental costs over time and eventually declaring bankruptcy. Insurers in this market described this type of monitoring in interviews.

*Other Environmental Outcomes.*—Figure 7 shows water protection rules violations by month and firm size. Firms have an incentive to reduce violations as soon as they know that they will have to purchase insurance, because they expect insurers to price the firm’s compliance history into premiums.<sup>35</sup> Thus, I consider the date at which the rule passed the Texas Legislature as the onset date for the policy. The vertical dashed line shows the bill’s passage in June 2001. The horizontal gray lines show the mean number of violations during two years before and after the policy change. Small firms incur many more violations per lease than large firms. After the policy change, the rate of violations among small firms is lower, and there is a noticeable fall in violations at June 2001. The rate of violations among large firms is low and relatively similar across time.

Finally, Figure 8 shows reported well blowouts over time. Because blowouts occur most frequently during drilling, I normalize the number of blowouts by the number of active drilling rigs.<sup>36</sup> The vertical line represents the policy’s passage and the horizontal lines represent two-year averages before and after. Before 2001, the time series of blowouts is noisy but relatively flat. There is a sharp drop coincident with passage of the bond requirement, and then the blowout rate remains low. This outcome is tracked in less detail by the RRC than the other outcomes in the paper. Unlike orphan wells and rules violations, the RRC relies on firms to self-report blowouts. This introduces the possibility of underreporting by some firms, even though reporting is required by Commission rules. Furthermore, the inconsistently

\*35 In contrast, the decision to leave orphan wells accompanies the decision to leave the industry, so future reputation is less of a concern.
\*36 Data on the number of active rigs each month come from the Baker Hughes Historical Rig Count dataset, available at http://www.bakerhughes.com/rig-count.

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 419

![Scatter plot showing Water Protection Rules Violations per 1,000 leases by month from 1995 to 2007, with separate series for the smallest 80% of firms and largest 20% of firms. A vertical dashed line indicates when the universal bond rule became law in June 2001.](page_29_image_1_v2.jpg)

FIGURE 7. WATER PROTECTION RULES VIOLATIONS, BY MONTH

Notes: This figure shows the number of violations of Statewide Rules 8 and 14 per 1,000 leases, by month. The gray horizontal bars show means for two years before and after June 2001.

![Scatter plot showing Well Blowouts per active drilling rig by quarter from 1995 to 2007. A vertical dashed line indicates when the universal bond rule became law in June 2001.](page_29_image_2_v2.jpg)

FIGURE 8. WELL BLOWOUTS, BY QUARTER

Notes: This figure shows the number of blowouts per active drilling rig, by quarter. Rig data come from the Baker Hughes Historical Rig Count dataset. The gray horizontal bars show means for two years before and after June 2001.

formatted operator information in the blowout data makes it difficult to assess the heterogeneity of the change in blowouts across firms, as discussed in Section V. For these reasons I focus on the overall blowout rate. In addition to the graphical results

420 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

in Figures 7 and 8, regression estimates for the changes in rules violations and well blowouts are included in online Appendix Section E.

## E. Additional Robustness Checks: Alternative Empirical Design

To complement the main results and robustness checks in Sections VB–VD, I implement an additional robustness check that uses oil and gas producers in Louisiana as a comparison group. I reestimate the main results using an alternative empirical strategy of difference-in-differences, comparing Texas and Louisiana producers. The results from this alternative empirical design are very similar to the results using the preferred specifications, and are presented in online Appendix Section G. The robustness of the results to the inclusion of a comparison group means that the findings are unlikely to be driven by unobserved shocks to the oil and gas industry that happened to coincide with the introduction of Texas’s insurance requirement. For example, a contemporaneous shock occurring exactly at March 2002 could have affected the results on producer exit decisions, while the findings on firm-level output could have been affected by a series of contemporaneous shocks that altered each firm’s output exactly at its assigned license renewal month. The complementary difference-in-differences results rule out such contemporaneous industry-wide shocks. Online Appendix Section E.4 also includes an examination of other rules implemented in Texas by the RRC during this same time period, none of which are likely to have caused the observed changes in outcomes.

## VI. Discussion

The empirical results are consistent with greater internalization of environmental costs, as predicted by theory. Prior to the bond requirement, small, financially weak operators could produce oil and gas at low private cost by avoiding environmental costs through bankruptcy. Bonding mitigated this problem. Firms whose environmental costs were likely to exceed their assets, as judged by insurers, faced high bond premiums. Previously judgment-proof firms either became bonded and increased their safety effort, or they left the industry. These changes reallocated wells from small to large producers and reduced the number of high-cost projects where social cost is likely to exceed social benefit. There is striking evidence of reductions in orphan wells and suggestive evidence of other environmental improvements.

This section contextualizes the results and discusses welfare implications. Section VIA examines aggregate environmental benefits and Section VIB addresses industry composition and output. Section VIC considers other potential distortions in the market.

### A. The Approximate Value of Environmental Improvements

Small producers were substantially less likely to plug wells after production and more likely to violate water protection rules when firms could avoid environmental costs through bankruptcy. Despite affecting small firms, the bond requirement yielded large environmental improvements because small producers caused a disproportionate share of environmental problems. The bond requirement reduced the

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 421

number of orphan wells per lease among the smallest 80 percent of firms by 76 per-cent, decreasing the industry-wide orphan well rate by 65 percent. The number of rules violations per lease among the smallest 80 percent of firms fell by 33 percent, reducing the industry-wide violation rate by 25 percent.<sup>37</sup>

In this section I calculate a rough lower bound on the value of these avoided damages using a back-of-the-envelope estimate for pollution risk from orphan wells. Orphan wells are the most straightforward outcome to value because groundwater contamination is carefully tracked and scientists have estimated atmospheric methane emissions. While improvements in other environmental practices presumably had additional benefits, these are more difficult to quantify. The benefits of improved compliance with water safety rules depend on the resulting reduction in environmental damage, which is unknown. Regulators do not collect complete data on surface spills and other environmental damages that might result from noncompliant operations. I limit this benefits calculation to orphan wells for feasibility reasons, recognizing that this understates total benefits.

Prior to the policy change, small operators (the smallest 80 percent of firms) generated an average of 0.097 orphan wells per lease. At that time there were 34,109 leases belonging to small operators. If the pre-policy rate of well orphaning had continued, these leases would have been expected to create 3,322 orphan wells in future years as operators left the industry. This study’s results imply that the policy reduced this number by 76 percent, or 2,521 orphan wells. Between the implementation of the bond requirement and 2013 (the most recent year in the data), small firms drilled or acquired 18,526 additional leases. As a rough benchmark for benefits over this period, applying the same reduction in orphan wells to these new leases yields additional benefits of 1,369 avoided orphan wells. This implies an overall back-of-the-envelope number of 3,890 orphan wells avoided by the policy since enactment.

The expected economic damages from an orphan well depend on the cost and likelihood of groundwater contamination, the amount of methane emissions, and the costs to the state to eventually plug the well and remediate the site with public funds. I use the best available information to develop a rough estimate of expected damages in each of these categories. The largest category of damages is leakage to groundwater. Two recent studies imply that the damages from a groundwater contamination event due to oil and gas development are about $4 million (Krupnick and Siikamäki 2014; Muehlenbachs, Spiller, and Timmins 2015). Based on historical incidents of groundwater contamination from orphan wells reported in Kell (2011), I estimate the risk of groundwater contamination to be approximately 0.21 percent per orphan well-year. Full details of the assumptions and sources in this expected damage calculation are described in online Appendix Section I. After adding in the other categories of costs as described in that appendix, this back-of-the-envelope accounting yields expected costs of $119,900 per orphan well. Under this calculation, the approximate total economic value of the orphan wells avoided by the policy was $470 million. It is important to emphasize that these valuations are uncertain and reliant on strong

<sup>37</sup>All aggregate effects in this section are calculated using weighted versions of the regressions in Table 6 and Figure 7, where firm-level observations are weighted by the number of leases operated. Full details are included in online Appendix Section E.5. Weights are necessary for aggregate calculations because small firms operate fewer leases, so that an unweighted comparison of incident rates would overstate aggregate effects. For evaluating individual firm responses to the policy as predicted by theory, unweighted comparisons are more appropriate.

422 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

assumptions. The primary goal of this exercise is to establish order-of-magnitude estimates of total benefits and these numbers should be taken in that context.

The model in Section II shows that the net benefits of alleviating the judgment-proof problem are the avoided environmental damages minus firms’ additional spending on safety effort. For the particular case of plugging wells after production, cost estimates are available. As shown in the online Appendix, reported average costs range from $5,600 to $23,000 per well. This means that the aggregate cost to firms to plug the orphan wells averted by the policy was $22 to $90 million. These costs are much smaller than the avoided damages, implying large net benefits to mitigating the judgment-proof problem. The costs of other types of safety effort, such as actions that reduced the rate of rules violations, are not observable in this setting. However, as long as penalties were equal to or less than damages, profit maximization implies that the net of environmental benefits and effort costs is positive.<sup>38</sup>

## B. The Change in Industry Composition and Output

Reducing the ability to avoid liability through bankruptcy also changed the composition of the industry. Proposition 2 in the theoretical model describes how the judgment-proof problem increases the number of projects produced by small firms. Limited liability lowers private costs for small producers, leading them to produce projects that would be more efficiently produced by larger firms. Aggregating the results in Table 5 for the smallest 80 percent of producers implies that alleviating the judgment-proof problem shifted about 1,300 leases, or 4 percent of the leases previously owned by small firms, to new producers.

Proposition 2 also shows how artificially low private costs cause small firms to expand production into project sites that would otherwise not be profitable. The results imply that small firms chose to shut down about 400 low-producing leases after internalizing environmental costs via the bond requirement. Wells like these produce large volumes of hazardous saltwater, and are likely to be old and at greater risk of leaks. Both of these composition effects are conservatively estimated, since they do not account for future new projects that would have been operated by judgment-proof firms. The efficiency costs of these production distortions depend on the production costs of large and small firms. This study does not observe production costs, but this is an important area for future research.

Notably, the state’s total output of oil and gas was essentially unaffected by the policy. This is because the firms affected by the policy accounted for a small fraction of production, and because most of the valuable production associated with exiting firms was reallocated to other producers instead of shutting down.

## C. Preexisting Distortions

In a more concentrated industry, reducing the number of producers could raise competitiveness concerns. However, crude oil and natural gas are commodities that are sold in highly competitive markets. Even after the bond mandate, producers in

\*<sup>38</sup>In practice, expected fines are likely lower than damages because of imperfect detection and enforcement, as discussed in Shavell (2007).

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 423

Texas faced essentially perfect competition from thousands of other firms within the state and around the world. In hazardous industries that are more concentrated, efficient regulation would involve multiple policy instruments: one to prevent firms from avoiding liability through bankruptcy (e.g., an insurance requirement), and another to address competitiveness (e.g., an entry subsidy).

Imperfections in the insurance market could also affect welfare. Equation (3) in the theoretical model shows how transaction costs can lead the bond premiums paid by firms to exceed expected environmental costs to the insurer. Market power among insurers could also lead policies to be marked up above the insurer's expected cost. If these frictions were large, the additional costs for producers could potentially prevent participation by firms whose operations would otherwise have created positive social surplus. Several dozen firms offer surety bonds for oil and gas firms in Texas (Gerard and Wilson 2009), which suggests that pricing is likely to be close to competitive. However, transaction costs do exist and should be expected to affect premiums. In this industry, several facts make it unlikely that these expenses were a major barrier to participation. First, the industry continued to include many small firms that secured coverage and operated profitably. Even after the bond requirement, over half of producers had fewer than five leases. Second, firms that left the industry in response to the policy had systematically poor environmental records, while firms that remained increased their safety effort. These effects are consistent with premiums being determined primarily by perceived risk. Furthermore, from a welfare perspective, the importance of potential frictions in the surety market is limited by the fact that output was primarily reallocated, not reduced. Almost all of the leases operated by exiting firms were acquired by other firms that continued to produce.

Finally, it should be noted that requiring bonds is not the only available policy response to the judgment-proof problem. Alternative policies such as minimum asset requirements for industry participants or vicarious liability for lenders and business partners might also have achieved positive results in this industry. Shavell (2007) discusses the relative advantages of each of these types of policies. Empirical analysis of other instruments in markets where they have been adopted is an area for future research.

## VII. Conclusion

The ability to avoid liability through bankruptcy can weaken safety incentives and distort industry composition. This paper measures the effects of the judgment-proof problem in the oil and gas industry by exploiting a natural experiment in Texas. The introduction of an insurance requirement caused producers to internalize a greater share of environmental costs through premiums paid to private insurers. Reducing firms' ability to avoid liability through bankruptcy caused an immediate change in industry composition. About 5 percent of firms, especially small firms and firms with poor environmental records, left the industry. Production was reallocated from small to large producers. Very low-producing projects, which were most likely to have been socially inefficient after considering environmental costs, were shut down. However, the overall rate of oil and gas production in the industry was unaffected.

The policy also caused notable changes in environmental outcomes. The industry-wide rate of well orphaning fell by two-thirds, substantially reducing the

424 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

overall threat of groundwater pollution from the industry. The industry-wide rate of water safety rules violations fell by 25 percent. These improvements were concentrated among small firms, which accounted for a major share of total environmental damage despite their small share of total output.

This paper focuses on a historical case study, but the results are relevant today. Between 2006 and 2015, US oil and natural gas production increased by 85 percent and 40 percent, respectively. The boom has had economy-wide benefits. It also presents environmental challenges on a massive scale, as the growth in production creates more opportunities for accidents and strains regulatory capacity. At the same time, bond requirements in most jurisdictions remain low. The minimum bond requirements for oil and gas production on federal lands have not been increased since 1960, even to adjust for inflation (Davis 2015). Texas’s bond requirements are some of the strictest among major oil- and gas-producing states.<sup>39</sup> This study supports arguments to increase bonds in other jurisdictions to at least the amounts required in Texas. While it is impossible to extrapolate beyond the observed levels, it seems likely that somewhat higher bond requirements could yield further benefits given that Texas’ requirements are still below potential damages.

More broadly, the results suggest that bankruptcy should be taken seriously as a determinant of market structure in hazardous industries. Within the energy sector, these results have implications for the production of oil, natural gas, coal, and tar sands. They are also relevant to transportation of these products along with gasoline and other refined products by pipeline, road, and rail. Examples in other sectors include chemical manufacturing, hazardous waste management, and drivers for ride-share companies. Further work in other settings will help to gauge the generality of these results, but this study supports concerns about the incentive effects of bankruptcy in dangerous industries. Continuing to evaluate and address this market failure will be an important component of efficient safety regulation in some of the world’s most important industries.

### REFERENCES

Alberini, Anna, and David Austin. 2002. “Accidents Waiting to Happen: Liability Policy and Toxic Pollution Releases.” *Review of Economics and Statistics* 84 (4): 729–41.

Allcott, Hunt, and Daniel Keniston. 2018. “Dutch Disease or Agglomeration? The Local Economic Effects of Natural Resource Booms in Modern America.” *Review of Economic Studies* 85 (2): 695–731.

Anderson, Soren T., Ryan Kellogg, and Stephen W. Salant. 2018. “Hotelling under Pressure.” *Journal of Political Economy* 126 (3): 984–1026.

Armour, John, and Douglas Cumming. 2008. “Bankruptcy Law and Entrepreneurship.” *American Law and Economics Review* 10 (2): 303–50.

Bain, Joe S. 1956. *Barriers to New Competition: Their Character and Consequences in Manufacturing Industries.* Cambridge, MA: Harvard University Press.

Becker, Gary S. 1973. “A Theory of Marriage: Part I.” *Journal of Political Economy* 81 (4): 813–46.

Boomhower, Judson. 2019. “Drilling Like There’s No Tomorrow: Bankruptcy, Insurance, and Environmental Risk: Dataset.” *American Economic Review.* https://doi.org/10.1257/aer.20160346.

Boyd, James. 2002. “Financial Responsibility for Environmental Obligations: Are Bonding and Assurance Rules Fulfilling Their Promise?” In *An Introduction to the Law and Economics of Environmental Policy: Issues in Institutional Design*, Vol. 20, edited by Timothy Swanson, 417–85. Amsterdam: Elsevier.

<sup>39</sup> For a detailed state-by-state comparison of bonding requirements, see Dutzik, Davis, and Heeke (2013).

VOL. 109 NO. 2 BOOMHOWER: DRILLING LIKE THERE'S NO TOMORROW 425

Boyd, James, and Daniel E. Ingberman. 2003. “Fly By Night or Face the Music? Premature Dissolution and the Desirability of Extended Liability.” *American Law and Economics Review* 5 (1): 189–232.

Brooks, Richard R. W. 2002. “Liability and Organizational Choice.” *Journal of Law and Economics* 45 (1): 91–125.

California Council on Science & Technology (CCST). 2015. *An Independent Scientific Assessment of Well Stimulation in California, Volume II, Potential Environmental Impacts of Hydraulic Fracturing and Acid Stimulation*. Sacramento, CA: California Council on Science & Technology.

Che, Yeon-Koo, and Kathryn E. Spier. 2008. “Strategic Judgment Proofing.” *RAND Journal of Economics* 39 (4): 926–48.

Darrah, Thomas H., Avner Vengosh, Robert B. Jackson, Nathaniel R. Warner, and Robert J. Poreda. 2014. “Noble Gases Identify the Mechanisms of Fugitive Gas Contamination in Drinking-Water Wells Overlying the Marcellus and Barnett Shales.” *Proceedings of the National Academy of Sciences* 111 (39): 14076–81.

Davis, Lucas W. 2015. “Bonding Requirements for U.S. Natural Gas Producers.” *Review of Environmental Economics and Policy* 9 (1): 128–44.

Dutzik, Tony, Benjamin Davis, and Tom Yan Heeke. 2013. *Who Pays the Cost of Fracking? Weak Bonding Rules for Oil and Gas Drilling Leave the Public at Risk*. Denver, CO: Environment America Research & Policy Center.

Ganuza, Juan José, and Fernando Gomez. 2011. “Soft Negligence Standards and the Strategic Choice of Firm Size.” *Journal of Legal Studies* 40 (2): 439–66.

Gelman, Andrew, and Guido Imbens. Forthcoming. “Why High-Order Polynomials Should Not Be Used in Regression Discontinuity Designs.” *Journal of Business & Economic Statistics*.

Gerard, David. 2000. “The Law and Economics of Reclamation Bonds.” *Resources Policy* 26 (4): 189–97.

Gerard, David, and Elizabeth J. Wilson. 2009. “Environmental Bonds and the Challenge of Long-Term Carbon Sequestration.” *Journal of Environmental Management* 90 (2): 1097–1105.

Ground Water Protection Council (GWC). 2014. *State Oil & Natural Gas Regulations Designed to Protect Water Resources*. Oklahoma City, OK: Ground Water Protection Council.

Jacobson, Louis S., Robert J. LaLonde, and Daniel G. Sullivan. 1993. “Earnings Losses of Displaced Workers.” *American Economic Review* 83 (4): 685–709.

Kaiser, Mark J., and Brian Snyder. 2009. “Offshore-Bonds-1: Raising Supplemental Bonding Ups Small Company Liabilities.” *Oil & Gas Journal* 107 (33).

Kang, Mary, Shanna Christian, Michael A. Celia, Denise L. Mauzerall, Markus Bill, Alana R. Miller, Yuheng Chen, Mark E. Conrad, Thomas H. Darrah, and Robert B. Jackson. 2016. “Identification and Characterization of High-Methane-Emitting Abandoned Oil and Gas Wells.” *Proceedings of the National Academy of Sciences* 113 (48): 13636–41.

Kell, Scott. 2011. *State Oil and Gas Agency Groundwater Investigations and Their Role in Advancing Regulatory Reforms: A Two-State Review: Ohio and Texas*. Oklahoma City, OK: Ground Water Protection Council.

Kornhauser, Lewis A. 1982. “An Economic Analysis of the Choice between Enterprise and Personal Liability for Accidents.” *California Law Review* 70 (6): 1345–92.

Krupnick, Alan J., and Juha Siikamäki. 2014. “Would You Pay to Reduce the Risks from Shale Gas Development? Public Attitudes in Pennsylvania and Texas.” *Resources: Resources for the Future* (185): 38–43.

La Porta, Rafael, Florencio Lopez-de-Silanes, Andrei Shleifer, and Robert W. Vishny. 1998. “Law and Finance.” *Journal of Political Economy* 106 (6): 1113–55.

Lee, David S., and David Card. 2008. “Regression Discontinuity Inference with Specification Error.” *Journal of Econometrics* 142 (2): 655–74.

Lee, David S., and Thomas Lemieux. 2010. “Regression Discontinuity Designs in Economics.” *Journal of Economic Literature* 48 (2): 281–355.

Lucas, Robert E., Jr. 1978. “On the Size Distribution of Business Firms.” *Bell Journal of Economics* 9 (2): 508–23.

McCrary, Justin. 2008. “Manipulation of the Running Variable in the Regression Discontinuity Design: A Density Test.” *Journal of Econometrics* 142 (2): 698–714.

Muehlenbachs, Lucija, Elisheba Spiller, and Christopher Timmins. 2015. “The Housing Market Impacts of Shale Gas Development.” *American Economic Review* 105 (12): 3633–59.

Pitchford, Rohan. 1995. “How Liable Should a Lender Be? The Case of Judgment-Proof Firms and Environmental Risk.” *American Economic Review* 85 (5): 1171–86.

Polborn, Mattias K. 1998. “Mandatory Insurance and the Judgment-Proof Problem.” *International Review of Law and Economics* 18 (2): 141–46.

426 THE AMERICAN ECONOMIC REVIEW FEBRUARY 2019

Ringleb, Al H., and Steven N. Wiggins. 1990. “Liability and Large-Scale, Long-Term Hazards.” *Journal of Political Economy* 98 (3): 574–95.

Shavell, Steven. 1979. “On Moral Hazard and Insurance.” *Quarterly Journal of Economics* 93 (4): 541–62.

Shavell, Steven. 1984. “A Model of the Optimal Use of Liability and Safety Regulation.” *RAND Journal of Economics* 15 (2): 271–80.

Shavell, Steven. 1986. “The Judgment Proof Problem.” *International Review of Law and Economics* 6 (1): 45–58.

Shavell, Steven. 2002. “Minimum Asset Requirements.” National Bureau of Economic Research Working Paper 9335.

Shavell, Steven. 2007. “Liability for Accidents.” In *Handbook of Law and Economics*, Vol. 1, edited by A. Mitchell Polinsky and Steven Shavell, 139–82. Amsterdam: Elsevier.

State Review of Oil and Natural Gas Environmental Regulations (STRONGER). 2003. *Texas State Review*. State Review of Oil and Natural Gas Environmental Regulations.

Townsend-Small, Amy, Thomas W. Ferrara, David R. Lyon, Anastasia E. Fries, and Brian Lamb. 2016. “Emissions of Coalbed and Natural Gas Methane from Abandoned Oil and Gas Wells in the United States.” *Geophysical Research Letters* 43 (5): 2283–90.

United States Energy Information Administration (EIA). 2012. *Annual Energy Review 2011*. Washington, DC: US Energy Information Administration.

United States Energy Information Administration (EIA). 2018. “The Distribution of U.S. Oil and Natural Gas Wells by Production Rate.” https://www.eia.gov/petroleum/wells/pdf/full_report.pdf (accessed December 12, 2018).

United States Environmental Protection Agency (EPA). 2004. “Nationwide Identification of Hardrock Mining Sites.” Report 2004-P-00005. Washington, DC: United States Environmental Protection Agency.

United States Environmental Protection Agency (EPA). 2016. *Hydraulic Fracturing for Oil and Gas: Impacts from the Hydraulic Fracturing Water Cycle on Drinking Water Resources in the United States*. Washington, DC: United States Environmental Protection Agency.

United States Government Accountability Office (GAO). 2003. “Deep Injection Wells: EPA Needs to Involve Communities Earlier and Ensure That Financial Assurance Requirements Are Adequate.” Report 03-761. Washington, DC: US Government Printing Office.

United States Government Accountability Office (GAO). 2005. “Environmental Liabilities: EPA Should Do More to Ensure that Liable Parties Meet Their Cleanup Obligations.” Report 05-658. Washington, DC: US Government Printing Office.

United States Government Accountability Office (GAO). 2007. “Leaking Underground Storage Tanks: EPA Should Take Steps to Better Ensure the Effective Use of Public Funding for Cleanups.” Report 07-152. Washington, DC: US Government Printing Office.

van ’t Veld, Klaas. 2006. “Hazardous-Industry Restructuring to Avoid Liability for Accidents.” *International Review of Law and Economics* 26 (3): 297–322.

White, Michelle J. 2011. “Corporate and Personal Bankruptcy Law.” *Annual Review of Law and Social Science* 7: 139–64.

White, Michelle J. 2014. “Mass Tort Litigation: Asbestos.” In *Encyclopedia of Law and Economics*, edited by Jürgen Backhaus, 1–11. New York: Springer.

Yin, Haitao, Howard Kunreuther, and Matthew W. White. 2007. “Do Environmental Regulations Cause Firms to Exit the Market? Evidence from Underground Storage Tank (UST) Regulations.” Risk Management and Decision Process Center Working Paper 2017–10–17.

Yin, Haitao, Alex Pfaff, and Howard Kunreuther. 2011. “Can Environmental Insurance Succeed Where Other Strategies Fail? The Case of Underground Storage Tanks.” *Risk Analysis* 31 (1): 12–24.