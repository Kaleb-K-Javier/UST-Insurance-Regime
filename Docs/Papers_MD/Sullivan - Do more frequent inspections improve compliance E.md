# OCPA Working Paper Series¹
# Working Paper # 2017-05
# May 24, 2017

# Do more frequent inspections improve compliance? Evidence from underground storage tank facilities in Louisiana

Karen A. Sullivan²
Office of Communications, Partnerships and Analysis
Office of Land and Emergency Management
U.S. Environmental Protection Agency
1200 Pennsylvania Ave., N.W.
Washington, DC 20004
(202) 566-1259
sulllivan.karen@epa.gov

Achyut Kafle
ORISE Research Fellow
Hosted by the U.S. Environmental Protection Agency
kafle.achyut@epa.gov

\*¹ U.S. Environmental Protection Agency’s Office of Communications, Partnerships and Analysis (OCPA) publishes a working paper series authored by scientists in the Office of Land and Emergency Management (OLEM) or produced with OLEM funding on research related to the impacts of OLEM programs and policies. The working papers are distributed for purposes of information and discussion while they undergo the peer review process at academic journals. Any opinions and conclusions expressed herein are those of the author(s) and do not necessarily represent the views of the U.S. Environmental Protection Agency.

\*²Questions or comments on the draft paper can be emailed to: sullivan.karen@epa.gov.

# Do more frequent inspections improve compliance? Evidence from underground storage tank facilities in Louisiana

Karen A. Sullivan (Corresponding Author)
Office of Communications, Partnerships and Analysis
Office of Land and Emergency Management
U.S. Environmental Protection Agency
1200 Pennsylvania Ave., N.W.
Washington, DC 20004
(202) 566-1259
sulllivan.karen@epa.gov

Achyut Kafle
ORISE Research Fellow
Hosted by the U.S. Environmental Protection Agency
kafle.achyut@epa.gov

*Disclaimer: The views expressed in this paper are those of the authors and do not necessarily reflect the views or policies of the U.S. Environmental Protection Agency.*

## Abstract

This paper examines the effect of inspection frequency on compliance decisions in an environmental pollution prevention context by capitalizing on policy changes occurring under the Energy Policy Act of 2005 that increased inspection frequency requirements for underground storage tank [UST] facilities to at least once every three years. A censored bivariate probit model is estimated using data from Louisiana on inspection, compliance, releases and other socioeconomic and biophysical characteristics of UST localities to examine the relationship between increased inspection frequency and compliance. We find that increased inspection frequency improved compliance with UST requirements in Louisiana and this impact is heterogeneous based on a facility’s compliance status at the last inspection—larger impact for those facilities that were compliant than those that were noncompliant at the last inspection.

KEYWORDS: compliance; inspection; environment; government policy; impact analysis; firm behavior

ACKNOWLEDGEMENTS: Researchers conducted this analysis while supported by the AAAS Science and Technology Policy Fellowship Program, the Oakridge Institute for Science and Education Research Participation Program and the U.S. Environmental Protection Agency (US EPA). ArcGIS data work supported by funding from the US Environmental Protection Agency (contract GS-10F-0061N via Industrial Economics, Inc – Kate Doiron). We would like to acknowledge valuable information and comments provided by Sam Broussard at the Louisiana Department of Environmental Quality and Linda Gerber and Tim Smith at the US EPA as well as comments from participants at the National Tanks Conference and Expo 2015, Northeastern Agricultural and Resource Economics Association Meeting 2015, Southern Economics Association Meeting 2015, and the Society for Environmental Law and Economics Meeting 2016.

JEL CODES: K32 Energy, Environmental, Health, and Safety Law, Q5 Environmental Economics; Q53 Air Pollution –Water Pollution – Noise – Hazardous Waste – Solid Waste – Recycling; Q58 Government Policy

2

# 1. Introduction

Across the United States approximately 561,000 underground storage tanks (UST) store petroleum or hazardous substances at approximately 202,000 sites, and are regulated by the U.S. Environmental Protection Agency’s (EPA) UST Program (US EPA 2016). The majority of USTs are located at gas stations and some are located at facilities in other industries such as the commercial sector, manufacturing, transportation, wired telecommunications, electric utilities, and hospitals (US EPA 2011). The greatest potential hazard from a leaking UST is that petroleum or other hazardous substances can seep into the soil and contaminate groundwater, the source of drinking water for nearly half of all Americans (USGS 2003). A release from an UST can also present other health and environmental risks, including potential for fire and explosion.

EPA, states, and tribes work in partnership with industry to protect the environment and human health from potential UST releases. In 1984, Congress established the UST program to monitor the approximately 2.1 million tanks that were active at that time.<sup>1</sup> The U.S. EPA UST program is designed to prevent releases of petroleum and hazardous substances into the environment, detect releases when they occur, and clean up any contamination from releases. To monitor the large number of tanks, EPA enlisted states’ assistance in implementing and enforcing the program.<sup>2</sup> Despite early efforts, releases were common—from the beginning of the program until 2000 over 400,000 releases were reported. At an average cleanup cost of $152,000 that roughly represents an estimated 60.8 billion dollars in cleanup costs (US EPA 2015b).<sup>3</sup> This only represents a lower bound estimate of the costs from these releases as it does not include negative impacts on nearby property values, human health or ecosystem services

<sup>1</sup> Since the 1984 inception of the UST program, 1,832,048 USTs have been properly closed.
<sup>2</sup> As of 2016, 38 states and the District of Columbia and the Commonwealth of Puerto Rico have approved state UST programs. To obtain EPA approval, state programs must be at least as stringent as the federal requirements (US GPO e-CFR 2013)
<sup>3</sup> 2012 USD

3

(Jenkins et al. 2014; Guignet et al. 2016). In 2001, the United States General Accounting Office (GAO) investigated concerns raised by the United States Senate Committee on Environment and Public Works that the UST program was not effectively preventing leaks (US GAO 2001). One aim of the investigation was to determine the breadth of EPA’s and the states’ tank inspections. Physical inspections confirm whether tanks have been updated and are being properly operated and maintained to prevent and detect releases. The GAO’s survey of state UST programs showed that at the time 42% of states did not inspect all tanks on a regular basis and 20% of states inspected at intervals of 4 years or longer. EPA managers recommended that inspections take place annually or where resources are limited at a minimum of every three years but only 38% of states inspected all tanks at an interval of three years or less. Based on their findings, the GAO recommended that Congress may want to authorize EPA to establish a federal requirement for the physical inspection of all tanks on a periodic basis.

On August 8, 2005, President Bush signed the Energy Policy Act (EPAct) of 2005. With this came amendments to Subtitle I of the Solid Waste Disposal Act (SWDA), which is the original 1984 legislation that required the U.S. Environmental Protection Agency (EPA) to create a comprehensive regulatory program for USTs storing petroleum or certain other hazardous substances. Among other provisions, the UST provisions of EPAct added the requirement that all regulated UST facilities must be inspected to evaluate compliance with UST requirements at least once every three years.⁴ More frequent UST inspections are intended to improve facilities’ compliance with UST release detection and prevention requirements, and in doing so prevent accidental releases of harmful substances into the environment. This study examines how the resulting changes in inspection frequency impact compliance at UST facilities.

⁴ Other provisions include operator training, delivery prohibition, secondary containment, financial responsibility, and cleanup of releases that contain oxygenated fuel additives.

4

Today national compliance rates are higher than they were before the 3-year inspection requirement. At the end of fiscal year 2005, 66 percent of facilities were in operational compliance but by the end of fiscal year 2015 compliance rates reached 81 percent (US EPA 2005; US EPA 2015a). This trend, as depicted in Figure 1, represents a significant achievement but the extent to which this improvement in compliance is due to the increase in inspection frequency is unclear. There may be other factors affecting compliance rates such as the establishment of UST operator trainings or changes in individual state UST regulations that in some cases are more stringent than the UST federal requirements. Without controlling for other factors that may impact compliance, the role that increased inspection frequency has taken in these improvements cannot be clearly identified.

Previous empirical analyses consistently show that inspections combined with penalties for violations improve compliance across a variety of environmental regulations such as the Clean Air Act, the Clean Water Act, and hazardous and toxic waste regulations (Shimshack 2014). However, these studies do not explicitly investigate how changes in inspection frequency affect facilities’ compliance behavior. This analysis explicitly examines the effect of inspection frequency on compliance in a pollution prevention context by capitalizing on changes in inspection frequency occurring as a result of an exogenous policy change—the EPAct of 2005.⁵ Given the significant resources devoted to compliance inspections in the UST program, determining the effect of those inspections is critical to making future policy and funding

\*⁵ The impact of increasing inspection frequency on compliance has been studied in other contexts. Ko, Mendeloff, and Gray (2010) examine the effect of repeated Occupational Safety and Health Administration (OSHA) inspections and the time between inspections on noncompliance and find that the number of violations cited increased with each additional year since the prior inspection after controlling for other variables. The increases totaled approximately 15% over five years. Alberini et al. (2008) examine FDA inspections of seafood processors’ compliance with sanitation requirements and a new Hazard Analysis and Critical Control Points (HACCP) requirement. Anticipated inspection frequency represented by the hazard rate predicted from the inspection model increases the likelihood of compliance with the sanitation program but not with the newer HACCP program.

5

decisions. Similarly, this analysis may be useful to other environmental programs relying heavily on compliance inspections to monitor and enhance compliance.

A national analysis of the impact of the EPAct’s 3-year inspection requirement on compliance would be ideal but the data needed to do so is not available. States report aggregated state-level UST information periodically throughout the year to EPA. This reported data is useful to EPA for measuring UST performance, however, it cannot be used to conduct a national analysis of the impact of increasing inspection frequency on compliance due to limited data on inspection frequency in each state over time (i.e., the total annual number of inspections in each state was not reported to EPA by states until 2008—three years after the enactment of the EPAct of 2005). Furthermore, many state UST programs do not have inspection and compliance databases that contain sufficient data from prior to EPAct to be able to examine the impact of changes in inspection frequency on compliance.

This empirical analysis uses an UST facility-level dataset from the Louisiana Department of Environmental Quality (LADEQ) that includes facility characteristics and information on inspection, compliance, and releases from before and after EPAct (2001 to 2012) combined with data on the socioeconomic and biophysical characteristics of the facilities’ locations. Prior to the EPAct of 2005, Louisiana inspected all tanks at an interval of 4 years or longer, which allows us to capitalize on the exogenous implementation of the EPAct of 2005’s 3-year inspection mandate to examine the impact of increased inspection frequency on compliance (US GAO 2001). Results from a censored bivariate probit model show that increasing inspection frequency improved compliance of owners and operators at regulated UST facilities in Louisiana and this effect is heterogeneous based on the facility’s compliance status at the last inspection—larger impact for those facilities that were compliant than those that were noncompliant at the last inspection.

6

The remainder of this paper is organized as follows. Section 2 provides a brief discussion of related literature. The data used in this analysis as well as background information on Louisiana’s UST Program is described in Section 3. Section 4 describes the empirical approach taken, and the choice of explanatory variables is explained in Section 5. Results of the analysis are presented in Section 6, followed by conclusions in Section 7.

## 2. Related literature on inspection and compliance

The theoretical foundation for investigating the impact of environmental monitoring and enforcement on regulated firms’ compliance decisions is grounded in the economic theory of crime and punishment first formalized by Becker (1968). Becker’s model of crime and punishment assumes that a rational, risk-neutral agent evaluates the expected benefits and the expected costs of a private action, and then acts if the expected benefits exceed the expected costs.<sup>6</sup> Becker’s model was later adapted to an environmental context by Russell, Harrington and Vaughan (1986), and there have since been extensions to examine different aspects of environmental enforcement regimes (e.g., avoidance behavior (Malik 1990), self-reporting (Malik 1993; Kaplow and Shavell 1994; Innes 1999a, 1999b, 2001), and more frequent versus more thorough inspections (Heyes 1994)). Since the late 1980s, an extensive empirical literature on the impacts of environmental monitoring and enforcement has also developed. Interested readers may refer to Shimshack (2014) for a comprehensive review of the environmental monitoring and enforcement literature.

\*<sup>6</sup> An alternative theory—the behavioral model of compliance—from the social-legal tradition theorizes that inspections reduce accidents or injuries by spurring firms to pay more attention to safety. Firms may be found out of compliance and be eager to return to compliance due to the social norms—the desire to be a law abiding citizen (Cyert and March 1963; Scholz and Gray 1990; Mendeloff and Gray 2005).

7

Specific-deterrence based enforcement is a widely used enforcement regime in which regulators use inspections or threats of inspection and penalties for identified violations as mechanisms to enforce environmental regulations. Empirical studies focused on the impact of specific-deterrence based enforcement on compliance consistently show that inspections combined with penalties for violations improve compliance across a variety of environmental regulations such as the Clean Air Act (CAA), the Clean Water Act (CWA), and hazardous and toxic waste regulations.<sup>7</sup> For example, Gray and Deily (1996) and Deily and Gray (2007) showed that EPA monitoring and enforcement actions under the CAA led to improved compliance at steel mills in the early 1980s. At paper and pulp mills, Nadeau (1997) and Gray and Shadbegian (2005) found that EPA and state environmental monitoring and enforcement actions resulted in reductions in both duration and rate of air pollution noncompliance during the 1980s. Also, the threat of lawsuits reduced air pollution emissions at coal-fired power plants and various manufacturing industries (Keohane, Mansur and Voynov 2009; Hanna and Oliva 2010).

In the context of the CWA, Magat and Viscusi (1990) found that increased threats of inspections at U.S. pulp and paper mills improved water pollution compliance and Shimshack and Ward (2005; 2008) showed that formal enforcement actions with monetary penalties reduced water pollution discharges. At water treatment plants during the 1990s and at chemical facilities during the late 1990s and early 2000s, federal fines were also found to reduce pollution (Earnhart 2004a, 2004b; Glicksman and Earnhart 2007).

\* <sup>7</sup> This study focuses on specific-deterrence effect, which is the effect that inspections, sanctions or increased threats of inspections or sanctions have on an evaluated or sanctioned facility, as opposed to general deterrence. General deterrence effects occur when inspections or sanctions on a targeted facility spillover to other non-targeted facilities and lead to compliance improvements at the facilities that were not directly evaluated or sanctioned. In environmental context, Shimshack and Ward (2005; 2008) detected general deterrence effects of government enforcement whereas Langpap and Shimshack (2010) found that private enforcement (or private citizen suits) significantly crowded out government enforcement in case of wastewater treatment. Qualitative survey results also tend to support the evidence of general deterrence effects in environmental settings (Carlough 2004; Thornton, Gunningham and Kagan 2005).

8

Monitoring and enforcement has also been shown to affect hazardous and toxic waste emissions and compliance. Stafford (2002; 2003) found that increased liabilities or penalties under the Resource Conservation and Recovery Act (RCRA) have been shown to reduce plants’ violation probabilities and Alberini and Austin (1999; 2002) showed that increased threats of lawsuits and strict liability rules affected toxic waste releases. At regulated facilities in Michigan, Liu (2012) showed that RCRA inspections have a significantly positive effect on compliance, as well as evidence of positive cross-program effects (i.e., inspections under the CAA have a positive and significant effect on facility compliance with RCRA). Also, monitoring efforts have reduced oil spill frequency and spill size (Epple and Visscher 1984, Cohen 1987, Grau and Groves 1997) and federal cases against gas and liquid pipeline operators may have improved environmental performance in the late 2000s and early 2010s (Stafford 2014). Eckert (2004) examined the impact of inspections and warnings on compliance with storage inventory reconciliation regulations at above and underground petroleum tanks in Canada and found a small but positive impact (i.e., inspections and warnings deter future violations).

This brief summary of the environmental monitoring and compliance literature suggests that specific-deterrence enforcement mechanisms are an effective means to improve compliance. In these analyses, a variety of factors that may influence compliance such as compliance history, facility characteristics, and changes in penalties are examined, however, to the best of our knowledge these studies do not explicitly investigate how changes in the length of time between inspections may impact regulated facilities’ compliance behavior.<sup>8</sup> This analysis aims to address

\*8 Liu (2012) examines the impact of inspection frequency on compliance at facilities regulated under Resource Conservation and Recovery Act (RCRA) and the Clean Water Act (CAA). Liu finds that increasing the total number of RCRA inspections in the last year increases RCRA compliance, and that there is evidence of cross-program effects (i.e., increasing the total number of CAA inspections at a facility in the last year also increases RCRA compliance). However, we believe that Liu’s measure of inspection frequency, which is defined as the total number of inspections at a facility in the last four quarters, is limited because typically RCRA facilities are not inspected more than once in a given year. In fact, only 50% of facilities in her sample were formally inspected at all during the

9

this gap. More specifically, this analysis aims to quantify the impact that more frequent inspections have on compliance by capitalizing on the policy changes occurring under the exogenous implementation of the EPAct of 2005 that established a requirement for all states receiving Subtitle I funding for their UST programs that the time between a facility’s concurrent compliance inspections cannot exceed three years.

# 3. Data description and background

## 3.1 Data description

This analysis uses Louisiana Department of Environmental Quality’s (LADEQ) data on inspection, compliance, and releases at 4,424 UST facilities from fiscal year 2001 to 2012. The data includes information on facility specific characteristics, results of compliance inspections, and releases. The facilities’ addresses were geocoded and matched with location specific socioeconomic data obtained from the 2009-2013 U.S. Census American Community Survey 5-year estimates and biophysical data obtained from the Soil Survey Geographic Database (US American Communities Survey 2010; U.S. Department of Agriculture 2015). The final sample is an unbalanced panel that consists of 108,281 quarterly observations on 4,424 facilities that had at least one active petroleum UST subject to federal UST regulations between 2001 and 2012. On average each facility has 2.82 USTs with an average tank capacity of approximately 8,500 gallons. The average age of the oldest tank at a facility is 21.7 years.

***

study time period (2001-2010) and the mean number of RCRA and CAA inspections at a facility in the past four quarters were 0.19 and 0.17, respectively. Repeat RCRA inspections within a one year time period may be triggered by endogenous factors that simultaneously impact facilities’ compliance behavior. In our analysis, we are able to use an exogenous policy change affecting inspection frequency to explicitly examine the impact of changes in inspection frequency on compliance.

10

## 3.2 Background: Underground storage tank inspection and compliance in Louisiana

Louisiana’s UST state program was approved in 1992. During the late 1980s and 1990s, Louisiana focused on closure of substandard tanks and remediation activities. In 2000, the Louisiana State Legislature established requirements that 15% of active USTs be inspected each year. UST inspections are announced usually one week in advance in Louisiana. This notice is given with the purpose of providing the tank owner time to gather the required paperwork that will need to be examined. An inspection typically takes one to three hours, and the inspector goes through each step of the inspection with the facility owner or operator if they are available. All USTs at the facility are inspected. The inspector checks to see if the facility is compliant with a comprehensive list of requirements aimed at preventing and detecting releases such as standards for tanks and piping, spill and overfill prevention equipment, operation and maintenance of corrosion protection systems, release detection, record keeping, and so on. If a violation is identified during an inspection, the inspector will document the violations and confer with the LADEQ Enforcement Division to determine the appropriate type of enforcement action that will be issued. Usually a Notice of Deficiency (NOD) or Notice of Potential Delivery Prohibition (NOPDP) is issued.<sup>9</sup>

Facilities that do not return into compliance or do not respond to NODs or NOPDPs will receive Compliance Orders from the LADEQ Enforcement Division and those that had been issued a NOPDP are subject to having their tanks prohibited from receiving product deliveries, which is referred to as red tagged. When facilities refuse to return into compliance or certain egregious violations occur, the Enforcement Division has the discretion to issue either a formal

<sup>9</sup> If the facility has a temporarily closed tank or is an abandoned facility, the Enforcement Division may opt to issue a Compliance Order immediately rather than a Notice of Deficiency.

11

penalty notice or an Expedited Penalty Agreement.<sup>10</sup> One limitation of the UST data from the LADEQ is that data on enforcement actions beyond the initial compliance citations, NODs or NOPDPs, is not available until fiscal year 2004, and therefore we are only able to account for the initial NODs and NOPDPs in the analysis.<sup>11</sup>

Figure 2 shows the percent of facilities inspected (dotted line), the percent of inspected facilities that received at least one noncompliance citation (solid line), and the percent of facilities at which a release was confirmed (dashed line) in each year from 2001 to 2012. Prior to the EPAct of 2005, roughly 7-15% of Louisiana’s UST facilities were inspected each year. This coincides with the time frame during which the Louisiana State Legislature had a requirement that 15% of active USTs be inspected each year.<sup>12</sup> The EPAct of 2005 that included provisions on UST inspection frequency requirements was signed on August 8, 2005. The provisions included a transition phase from August 8, 2005 to August 8, 2007 during which states were required to inspect all active UST facilities that had not been inspected since 1998. The LADEQ began to focus inspections on these facilities just before Hurricanes Katrina and Rita hit Louisiana (August and September 2005, respectively). LADEQ diverted resources to deal with the hurricanes’ aftermath, and as a result only 7.6% of facilities were inspected in 2006. Once

\*<sup>10</sup> A facility has the option to sign an Expedited Penalty Agreement, which allows them to settle the violations for a reduced penalty by certifying that violation(s) was corrected within the 30 day timeframe allowed for in the agreement. Signing the agreement is strictly voluntary on the part of the regulated facility. Louisiana has a Delivery Prohibition (Red Tag) program that allows inspectors to red tag tanks at facilities that have certain egregious violations. The delivery prohibition can happen simultaneously with the enforcement actions listed above.
\*<sup>11</sup> If we were to include enforcement action data in the analysis, the sample would be reduced by approximately 25% from 5,769 to 4,324 observed inspections, and the observations lost would largely be inspections from prior to the change in inspection frequency that occurred as a result of EPAct. The loss of these pre-EPAct observations would significantly reduce the variation in inspection frequency in the sample and our ability to identify the impact of changes in inspection frequency on compliance.
\*<sup>12</sup> From approximately 2000 to the passing of EPAct in 2005, the Louisiana Regional Department of Environmental Quality staff identified 15% of the active UST in their region to inspect. Each region had their own systems of selecting the 15 percent. For example, some just went alphabetically down the site list while others went numerical by facility number. Also, if one region was overloaded with work and could not inspect 15% of their USTs facilities, then Louisiana would do more inspections in another region instead.

12

resources could be directed back to inspections, the LADEQ worked on inspecting those facilities that had not been inspected since 1998, and then towards meeting the requirement of inspecting each UST facility at least once every three years.

More frequent UST inspections are intended to improve facilities’ compliance with UST release detection and prevention requirements, and in doing so prevent accidental releases of harmful substances into the environment. From 2001 to 2012, on average each year 2.55 percent of facilities in the sample had a release confirmed. No clear trend in the percent of facilities with a release each year is visible, however, interestingly confirmed releases spike in 2008 when the LADEQ was focused on inspecting those facilities that had not been inspected since 1998. Trends in noncompliance are more apparent. In the years immediately following EPAct, the percent of inspected facilities that had at least one noncompliance citation issued increased—reaching a high of 56% in fiscal year 2008. This increase is likely due to the fact that many of the facilities inspected during those years were ones that had not been inspected since 1998. From 2009 to 2012, there is a downward trend in the percent of inspected facilities identified as noncompliant—reaching a low of 33.6% in 2012. Overall this improvement in compliance coincides with the establishment of the ongoing 3-year inspection requirement in Louisiana but in order to substantiate that this observed improvement is due to increased inspection frequency our empirical analysis will account for other factors that may also have impacted compliance.

## 4. The empirical model

There are three common challenges that arise when measuring the deterrence effects of environmental monitoring and enforcement: omitted variable bias, measuring facilities’ perceptions about the likelihood of inspections and enforcement, and reverse causality (Gray and

13

Shimshack 2011). The first, omitted variable bias can occur if factors not included in the model simultaneously affect both regulatory activity and facility compliance. Measuring facilities’ perceptions about the likelihood of inspection and enforcement is difficult because perceptions are not observable to researchers. The last issue—reverse causality—arises if there is targeting of facilities by regulators. To minimize these concerns, we use facility-level panel data in a censored bivariate probit model as detailed in Greene (1992) and Stafford (2002; 2012), include temporal lags (i.e., examine relationship between current compliance and an UST facility’s compliance status at the last inspection) and control for a variety of facility and location characteristics that may also affect compliance.

UST violations are detected when a facility is inspected, as opposed to the majority of the existing empirical work on environmental compliance that has used datasets with self-reported more frequent (e.g., monthly) observations of whether the firm is in compliance with a regulation (e.g., Hanna and Oliva 2010; Shimshack and Ward 2005, 2008). If an UST facility is not inspected, then there is no information about whether or not the facility is in compliance with regulated UST requirements. Because the data are censored and selection bias may arise if there is any inspection targeting by regulators, we construct a censored bivariate probit model. The censored bivariate probit model addresses the selection bias that could occur if there was any targeting of inspections based on unobserved characteristics of the facilities that would make them, for example, both more likely to be inspected and more likely to violate, particularly, in the pre-EPAct years when each regional office in Louisiana had their own systems of selecting facilities for inspections.

The censored bivariate probit consists of two equations—the selection equation and outcome equation. Here the selection equation is the probability of an inspection, and the

14

outcome equation is the probability of a noncompliance. As in Stafford (2002; 2012), we model the probability of noncompliance as the latent variable, $Y_{vjt}^* = X_{vjt}\beta_v + \varepsilon_{vjt}$, as well the probability of inspection as the latent variable, $Y_{Ijt}^* = X_{Ijt}\beta_I + \varepsilon_{Ijt}$. We define $Y_{vjt}$ and $Y_{Ijt}$ as binary variables that we observe at UST facility $j$ in quarter $t$ with respect to noncompliance and inspection, respectively. If facilities are selected for inspection based on unmeasured characteristics that also make them more likely to violate the UST regulations, the error terms ($\varepsilon_{vjt}$ and $\varepsilon_{Ijt}$) should be positively correlated. Given that a facility is inspected, the likelihood that a violation will be detected (i.e., $Y_{vjt} = Y_{Ijt} = 1$) is expressed as:

$$L_{Y_{vjt}=1, \ Y_{Ijt}=1} = \sum_{Y_{vjt}=1, \ Y_{Ijt}=1} \log\{\Phi_{VI} [X_v\beta_v, X_I\beta_I, \rho]\},$$

where $\Phi_{VI}$ is the bivariate normal cumulative distribution function and $\rho$ is the covariance between error terms, $\varepsilon_{vjt}$ and $\varepsilon_{Ijt}$. The likelihood that no violation will be detected when the facility is inspected (i.e., $Y_{vjt} = 0, Y_{Ijt} = 1$) is given by the following expression:

$$L_{Y_{vjt}=0, \ Y_{Ijt}=1} = \sum_{Y_{vjt}=0, \ Y_{Ijt}=1} \log\{\Phi_{VI} [-X_v\beta_v, X_I\beta_I, -\rho]\}.$$

If a facility is not inspected, the compliance status of the facility cannot be observed. Therefore, facilities that are not inspected (i.e., $Y_{Ijt} = 0$), regardless of whether they are compliant or noncompliant, are observationally equivalent and can be represented as:

$$L_{Y_{Ijt}=0} = \sum_{Y_{Ijt}=0} \log\{1 - \Phi_I [X_I\beta_I]\}.$$

where $\Phi_I$ is the univariate normal distribution for the inspection equation. The maximum likelihood function for the censored bivariate probit model can be given by:

15

$$ \begin{aligned} L = L_{Y_{Vjt}=1, \ Y_{Ijt}=1} &+ L_{Y_{Vjt}=0, \ Y_{Ijt}=1} + L_{Y_{Ijt}=0} \\ &= \sum_{Y_{Vjt}=1, \ Y_{Ijt}=1} \log \{ \Phi_{VI} [X_V \beta_V, X_I \beta_I, \rho] \} \\ &+ \sum_{Y_{Vjt}=0, \ Y_{Ijt}=1} \log \{ \Phi_{VI} [-X_V \beta_V, X_I \beta_I, -\rho] \} + \sum_{Y_{Ijt}=0} \log \{ 1 - \Phi_I [X_I \beta_I] \} \end{aligned} $$

For the censored bivariate probit model to be identified, at least one variable that affects the probability that a facility will be inspected but that does not affect the probability that a facility is noncompliant should be included in the inspection equation (Wooldridge 2002). The specific variable used in this case will be discussed at the end of the next section.

# 5. Explanatory variables: Definitions and hypothesized effect on noncompliance

As discussed the censored bivariate probit consists of two equations—here the inspection equation and the noncompliance equation. The dependent variable in the inspection equation is a dummy variable that is equal to one if facility $j$ is inspected in quarter $t$. In the violation equation, the dependent variable is a dummy variable that equals 1 if facility $j$ inspected in quarter $t$ received at least one noncompliance citation (i.e., if a Notice of Deficiency (NOD) or Notice of Potential Delivery Prohibition (NOPDP) is issued). For brevity, from here forward, we will refer to a facility as noncompliant if it had at least one noncompliance citation issued at its inspection.

Both the probability of inspection and noncompliance is expected to depend on a facility’s characteristics and history (i.e., inspection, compliance, and release history) as well as socioeconomic and biophysical attributes of the facility’s location. Table 1 presents descriptive statistics for the variables used in the inspection and noncompliance equations. We include a common set of variables (rows 1 to 21) in both equations to account for these factors as well as variables that are unique to each equation (rows 22 to 24). In this section, we define these variables and describe their expected relationships with noncompliance, which is the equation of primary interest in this analysis.

16

The main variable of interest, inspection frequency, is defined as the number of years since the last inspection (*Years_LastInspection*). This continuous measure of inspection frequency is used rather than a dummy variable that would indicate if the inspection was before or after the EPAct of 2005 3-year inspection requirement because there is no clear date that establishes a before period, when inspections were less frequent than three years, and an after period, when inspections were at least once every three years (i.e., the transition to the 3-year inspection requirement took several years). Increasing inspection frequency is expected to improve compliance. As more time passes since a facility’s last inspection, owners and operators may become more lax about keeping up with required standards and procedures, and therefore would be more likely to have a violation identified when inspected. The estimated coefficient on *Years_LastInspection* is expected to be positive, ceteris paribus.

*Last_Noncompliance* is a dummy variable that takes the value 1 if at least one violation was detected at a facility’s last inspection. The effect of noncompliance at the last inspection on the probability of noncompliance at the current inspection is not obvious. A noncompliance citation at the last inspection is expected to have a deterrent effect and to reduce the likelihood that an UST facility will violate at the current inspection, which means that the expected sign on the estimated coefficient on *Last_Noncompliance* is negative. However, if the facility believes that the cost of complying is greater than the benefits of complying, then the facility may return to a noncompliant state, which means we would expect the estimated coefficient on *Last_Noncompliance* to be positive. Therefore, the expected sign of the coefficient on *Last_Noncompliance* is ambiguous.

The magnitude of the effect that the time since the last inspection has on compliance at the current inspection may differ depending on whether or not the facility had a violation at the

17

last inspection. To allow for the heterogeneous effect of increasing inspection frequency for those that were identified as noncompliant at last inspection and those that were compliant, the interaction of inspection frequency and whether the facility was noncompliant at the last inspection (*Years_LastInspection*Last_Noncompliance*) is included.

The empirical model also accounts more broadly for the effect that a facility’s past experience with inspections may have on the facility owner’s or operator’s compliance behavior. Overall we would expect that the more compliance inspections a facility had experienced in the past, the lower the likelihood that facility would receive a noncompliance citation at the current inspection. The estimated coefficient on the cumulative number of previous inspections is expected be negative (*Total_Inspection*) because with each additional inspection we would expect the facility owner’s knowledge and understanding of the UST requirements and how to meet them may improve.

We also include the variable *Past_Noncompliance*, to account for the total number of past inspections at which a facility had at least one violation detected. *Past_Noncompliance* does not include the results of the last compliance inspection. While we would expect that over time as violations are identified at consecutive inspections, a facility’s compliance behavior would eventually improve, it may also be that those facilities with a high number of past inspections at which violations were identified are chronic offenders that will habitually violate. It may be that for that facility the cost of complying is greater than the benefits of complying. If the effect of facilities that violate all the time dominates, then the coefficient on *Past_Noncompliance* will be positive. If the learning effect dominates, then the coefficient will be negative.

After a facility has an accidental release from an UST, the owners may become more vigilant about following UST requirements to prevent and detect releases in order to avoid the

18

potential costs of a release that are now more concrete in the facility owner’s expected costs of noncompliance. To account for this, we include Last_Release, a dummy variable that is equal to 1 if there has been a release since the last inspection, and expect the coefficient on Last_Release to be negative.

The empirical model also accounts for the effect that UST facility characteristics may have on the likelihood of noncompliance. The expected cost of noncompliance with UST requirements depends on expected penalties and expected cleanup costs if a release occurs. Expected cleanup costs depend on the probability of a release and the size of a release, which depends on both tank technology and preventive measures taken by the owner or operator (e.g., reconciling inventories, inspecting and maintaining sumps and spill buckets, etc.). For each facility, we include the number of tanks (Number_Tanks), age of oldest tank (Age_OldestTank), and average capacity of the tanks (Mean_TankCapacity) at the facility.

Expected cleanup costs also depend on the likelihood that a release will contaminate groundwater. The average cleanup cost of a leaking UST is approximately $152,000 (2012 dollars) but the cost can be significantly higher if groundwater is affected (U.S. EPA 2015). A release is more likely to contaminate groundwater if the soil in the area is more permeable (Soil_MostPermeable) or if the water table is closer to the surface (Depth_WaterTable). We expect the coefficient on Soil_MostPermeable to be negative (i.e., facilities located in areas with the most permeable soils are less likely to be noncompliant), and the coefficent on Depth_WaterTable to be positive (i.e., facilities located in areas where the water table is further from the surface are more likely to be noncompliant).

Socioeconomic attributes of the communities surrounding UST facilities are included to proxy for the demand for petroleum product from nearby consumers. The higher the demand for

19

the product, the more frequent withdrawals and deliveries will be at the facility, and thus the more inventory oversight required (i.e., higher compliance costs). We include population density (*Density_Population*) and median income (*Income_Median*) in the census block group where the facility is located. Based on the increased demand for petroleum products that higher population density or median income would generate, we would expect the signs of the coefficent on both of these variables to be positive, however, in high income or highly populated areas there may also be higher demand for environmentally responsible businesses as well as higher potential costs to a facility should a release of petroleum occur. The sign of the coefficient on *Density_Population* and *Income_Median* will depend on which effect dominates—the increased demand for product leading to higher compliance costs or the increased demand for environmentally responsible businesses. Furthermore, we include Louisiana fiscal year quarterly dummy variables to control for seasonality in the demand for products at the UST facilities.

Staff at regional LADEQ offices are responsible for coordinating compliance inspections in their respective regions. We include regional dummy variables to account for any potential regional differences in how inspections are conducted by inspectors. In defining the regional dummy variables, we exclude the capital region (*Region_Capital*). We also include the distance of the facility to the LADEQ regional field office (*Distance_FieldOffice*). A facility located closer to the field office may have greater knowledge of UST requirements and awareness of regulator presence. We expect the coefficient on *Distance_FieldOffice* to be positive, that is; the closer a facility is to the field office, the lower the likelihood that the facility will violate.

Inspectors are either employees of the LADEQ or contracted by the LADEQ. Both types of inspectors receive the same training and use the same compliance evaluation inspection checklist to conduct inspections. The only difference is that the LADEQ does not allow contract

20

inspectors to issue compliance order letters or apply delivery prohibition red tags—instead the LADEQ will do that for them. To capture the effect that the type of inspector may have on the probability of a violation we include *Contract_Inspector* that takes the value 1 when the inspector was contracted by the LADEQ. We exclude *Contract_Inspector* from the inspection equation because we only observe inspector type when there is an inspection.

*State_OperatorTraining* is a dummy variable that takes the value 1 if a compliance inspection occurred after the start of operator trainings in Louisiana. The EPAct of 2005 also required state and territorial UST programs receiving federal funds to require UST systems to have designated UST system operators and to develop state-specific operator training requirements that meet EPA’s grant guidelines. The federal deadline to have designated UST system owners and operators trained was August 8, 2012, though some states, such as Louisiana, established earlier deadlines.<sup>13</sup> The first operator training was held in Louisiana on March 9, 2010.<sup>14</sup> Unfortunately, facility specific data on operator training status was not available. Therefore, to account for the fact that during this time period some operators and owners may have learned additional information on UST maintenance, testing, and recordkeeping, we include the dummy variable *State_OperatorTraining* in the compliance equation. We expect that after operator trainings were held in Louisiana the probability of a violation would be lower. We exclude *State_OperatorTraining* from the inspection equation as we would not expect the fact that operator trainings are being held in Louisiana to affect the probability of inspection at any given facility.

\*13 Louisiana had a phase-in period for operator training based on compliance inspection dates. Facilities inspected between February 20, 2010 and November 8, 2011 had to have their operators trained within 9 months of their inspection date. Everyone else had to be trained by August 8, 2012, which was the federal deadline to have designated UST system owners and operators trained.
\*14 After the state deadline, operator training requirements became part of compliance inspections. Since this added a new major component to the compliance inspection, we do not include compliance inspections conducted after the federal deadline (August 8, 2012).

21

For the censored bivariate probit model to be identified, at least one variable that affects the probability that a facility will be inspected but that does not affect the probability that a facility will be noncompliant should be included in the inspection equation (Wooldridge 2002). For identification purposes, we include the total annual number of hurricane related visits made by LADEQ UST inspectors to facilities, *State_TotalHurricaneVisits*, in the inspection equation but exclude it from the noncompliance equation. *State_TotalHurricaneVisits* reflects changes in the resource constraint of the LADEQ. We expect that when the total number of hurricane related visits is higher the resources available to conduct compliance inspections is reduced but that the total number of hurricane related visits would not affect the probability of a violation at an inspected facility. In fact, when *State_TotalHurricaneVisits* was included in the noncompliance equation, it was not significant (Coef.= -0.0002; p=0.254).

## 6. Results

Results of the censored bivariate probit regression are presented in Table 2.<sup>15</sup> In this section, we briefly discuss estimates from the inspection equation before turning the focus to the main results from the noncompliance equation. Louisiana’s inspection strategy over the study period, particularly post-EPAct, should be largely determined by the time since last inspection (*Years_LastInspection*). This is evident in the inspection equation estimates, where the coefficient on *Years_LastInspection* is positive and significant at the 1% level. Interestingly, results suggest that other factors affect the probability of an inspection. The probability of an inspection is higher when a facility has an older tank, a higher mean tank capacity, or fewer

\*<sup>15</sup> We used Stata 13’s *heckprobit* command, which estimates the censored bivariate probit model. We also use clustering to account for non-independence of inspections and compliance outcomes from a single UST facility in our unbalanced panel dataset to allow for potential within-groups (facilities) correlation while modeling econometric error (Rogers 1993; Williams 2000; Wooldridge 2002).

22

tanks; is located in an area where the water table is further from the surface; cumulatively has had more inspections; or has had a release since the last compliance inspection. The probability of an inspection is lower when the LADEQ's UST inspector resources are constrained (i.e., when a higher total annual number of hurricane related visits were conducted by LADEQ inspectors at UST facilities).

## 6.1 Effect of inspection frequency on compliance

We now turn to the main hypothesis of the paper: Does increasing inspection frequency improve compliance? Results suggest that increasing inspection frequency to at least every three years as required by EPAct has improved compliance with UST requirements in Louisiana. The results also show that the magnitude of the effect differed depending on whether or not the facility had a violation at the last inspection. For those facilities that were compliant at the last inspection (i.e., *Years_LastInspection* \* *Last_Noncompliance*=0), the coefficent on *Years_LastInspection* is positive and statistically significant at the 10% level (Table 2). For those facilities that were noncompliant at the last inspection, the effect (i.e., the linear combination of the *Years_LastInspection* and *Years_LastInspection* \* *Last_Noncompliance*) is also positive and statistically significant but lower in magnitude. This suggests that increasing inspection frequency increases the likelihood of noncompliance more at facilities that were compliant at the last inspection than those that were noncompliant. While this may seem counterintuitive, it suggests that for those facilities that were in compliance at the last inspection it may be easier for them to maintain compliance with UST prevention and release requirements than for facilities with a compliance violation at the last inspection that would need to take new actions to achieve compliance. To better illustrate the impact of the 3-year inspection requirement of EPAct on UST owners and operators compliance with UST regulations based on our results, we estimate

23

how changes in inspection frequency at a hypothetical representative facility in Louisiana affect the probability of noncompliance. The hypothetical representative facility has the mean values for all continuous explanatory variables, the mean value for noncompliance at the last inspection, and median values for all other discrete explanatory variables (see Table 1 for details). We use the estimates of the censored bivariate probit model presented in Table 2 and the representative facility’s characteristics to estimate the predicted probability that a facility will be noncompliant at the time of inspection for years since last inspection ranging from three to six years (Figure 3). We will focus on the difference in the predicted probability of noncompliance when the time since the last inspection has been six years, a representation of pre-EPAct, versus three years, the requirement under the EPAct of 2005. We use six years as representative of pre-EPAct because prior to the EPAct 3-year requirement, Louisiana had a requirement that 15% of the UST facilities in Louisiana be inspected each year, which is approximately equivalent to a 6-year cycle if they did not return to the same facility for a second inspection until all other facilities had been inspected. Predicted probabilities from the censored bivariate probit model estimated coefficients show that moving from a 6-year to a 3-year inspection cycle reduces the likelihood that a representative facility will receive a noncompliance citation at the time of inspection by about 11% (Table 3).

To illustrate the differing effect that increasing inspection frequency has on compliance depending on the results of a facility’s last compliance inspection, we also estimate predicted probabilities of noncompliance for a hypothetical representative facility that was noncompliant at the last inspection and for one that was compliant at the last inspection. The reduction in the likelihood of noncompliance moving from a 6-year to a 3-year inspection cycle is larger for

24

facilities that were compliant at their last inspection (about 13%) relative to the facilities that were noncompliant at their last inspection (about 9%).

## 6.2 Effect of other explanatory variables on compliance

As expected, *Total_Inspection* had a negative and statistically significant coefficient, suggesting that the more compliance inspections a facility has experienced in the past, the lower the probability that the facility would be noncompliant at the current inspection. The coefficients on *Last_Noncompliance* and *Past_Noncompliance* were both positive and statistically significant at the 1% level. If a facility was noncompliant at the last inspection or cumulatively had a greater number of past inspections where it was noncompliant, the probability of noncompliance at the current inspection is higher. This suggests that the effect of chronic offenders (i.e., those that habitually violate) dominates. It may also be that these variables are capturing an unobserved or omitted variable such as corporate culture that makes a facility consistently more or less likely to comply with requirements.

UST facilities with an older tank (*Age_OldestTank*) or a lower mean tank capacity (*Mean_TankCapacity*) were more likely to violate UST regulations. Older tanks and tanks with lower average capacity may have less advanced technologies that make it more challenging for facilities to maintain a compliant status. For example, older or smaller tanks may use a dip stick to reconcile tank inventory rather than an electronic inventory reconciliation device. Also, single facility owners are more likely to have older and smaller tanks and it may be challenging for single facility owners to meet UST requirements given all the other requirements simultaneously placed on them as a small business (e.g., Occupational Safety and Health Administration laws and regulations, fire prevention codes, food codes, tobacco and liquor sale laws, etc.).

25

We found no significant effect of the socioeconomic characteristics of communities and bio-physical characteristics around UST localities on the likelihood of noncompliance except for water table depth. As expected, a facility located in an area where the water table is further away from the surface was more likely to be noncompliant.

A facility inspected by a contracted inspector was more likely to be noncompliant than one inspected by a state-employed inspector (*Contract_Inspector*). This seems counterintuitive, however, state-employed inspectors may feel that they have the authority to allow a facility some leeway whereas the contracted inspector may not have such sense of authority. For example, at the time of inspection if there was only one minor issue that could be resolved while the inspector was onsite, a state-employed inspector may not issue a citation whereas the contracted inspector may feel obligated to cite the facility.

As expected the coefficient on *State_OperatorTraining*, which indicates whether or not an inspection occurred after Louisiana began holding operator trainings, was negative. This suggests that even though all owners and operators were not yet trained the presence of operator trainings in Louisiana reduced the likelihood that an inspected facility would have a violation detected. This effect on the likelihood of UST noncompliance is attributable to operator trainings to the extent that the dummy for the time period is not capturing other unobservable factors that are unique to that timeframe and influence UST compliance decisions.

In the censored bivariate probit model, *rho* measures the correlation of the residuals from the two equations. Here the coefficient on *rho* is not statistically significant. This suggests that the regulators decision to inspect a given UST facility in Louisiana may not in fact be

26

endogenous to inspection or compliance outcomes (Wald Test of Independent Equations ($\rho=0$), Chi-squared (1) = 0.47; p=0.4925).<sup>16</sup>

## 6.3 Sensitivity analysis

To assess the robustness of our findings with respect to inspection frequency and compliance, we explored a number of alternative models.<sup>17</sup> First, given the insignificance of the correlation coefficient between the residuals from the compliance and inspection equations, we estimate a probit model for the compliance equation. Second, given potential measurement error from defining a binary measure of noncompliance (i.e., at least one compliance citation indicates noncompliant) rather than the number of citations, we estimated a poisson regression for the noncompliance equation where the outcome variable was the number of citations. The results for both of these estimations are qualitatively similar and consistent with our main results. Most importantly, the coefficient on years since the last inspection remains positive and statistically significant. Lastly, one potential limitation of this analysis is our inability to account for enforcement actions beyond the initial compliance citations (NODs and NOPDPs) due to lack of data in pre-EPAct years. We estimate a censored bivariate probit model for the reduced sample (primarily consisting of post-EPAct inspections) with and without these enforcement action variables. The results using the reduced sample suggest that excluding these enforcement action variables does not change the effect that other explanatory variables have on noncompliance. Therefore, it is a reasonable assumption that our main results are robust to the exclusion of the additional enforcement action data.

\*<sup>16</sup> We also estimated the noncompliance equation using a probit model. Results are available upon request from authors. Note that while there are small changes in the magnitude of coefficients and significance level, the overall conclusions do not change between the probit and the censored bivariate probit estimation of the noncompliance equation.

\*<sup>17</sup> Results of these analyses are available upon request from the authors.

27

# 7. Conclusions

This paper examines the effect of inspection frequency on compliance decisions in an environmental pollution prevention context by capitalizing on policy changes occurring under EPAct of 2005 that increased inspection frequency requirements for UST facilities to at least once every three years. Specifically, we used facility-level data on inspection, compliance, releases and other socio-economic and biophysical characteristics of UST localities to examine this in Louisiana. A censored bivariate probit model was used to account for the censored nature of the inspection and compliance data and to account for potential bias in estimates due to inspection targeting that may have occurred, particularly in pre-EPAct years. Results suggest that increasing inspection frequency improved UST facilities’ compliance in Louisiana and this impact is heterogeneous based on a facility’s compliance status at the last inspection—larger impact for those facilities that were compliant than those that were noncompliant at their last inspection. This result is consistent with previous empirical literature that has consistently shown that inspections improve compliance across a variety of environmental regulation contexts (Shimshack 2014). Furthermore, our study illustrates the effect of more frequent inspections and finds a heterogeneous effect across facilities based on the compliance status at the last inspection.

The aim of increasing inspection frequency at UST facilities is to prevent and to reduce the size of accidental releases of petroleum and other hazardous substances into the environment. The greatest potential hazard from a leaking UST is that petroleum or other hazardous substances can seep into the soil and contaminate groundwater, the source of drinking water for nearly half of all Americans (USGS 2003). A release from an UST can also present other environmental and health risks, including potential for fire and explosion, neurological damage, blood disorders,

28

cancer, and other adverse health outcomes (Jenkins et al. 2014; Marcus 2016). Furthermore, when releases are prevented, remediation costs are avoided, which represents cost savings that accrue to owners, operators and public entities charged with remediating contaminated media at regulated facilities. Remediation costs will vary depending on site size and the media contaminated. The average cleanup cost of a leaking UST is approximately $152,000 but the cost can be significantly higher if groundwater is affected (US EPA 2015b).

While beyond the scope of this analysis, it would be informative for future research to examine the relationship between increased inspection frequency and the prevention of UST releases. Many empirical studies have examined the role of Occupational Safety and Health Administration inspections in the context of preventing workplace injuries (e.g., Viscusi 1979; Scholz and Gray 1990; Gray and Mendeloff 2005; Haviland et al. 2010; Levine, Toffel and Johnson 2012, etc.) but there are few in the context of preventing accidental releases of hazardous materials (Epple and Visscher 1984; Cohen 1987; Grau and Groves 1997; Talley, Jin and Kite-Powell 2005). Conclusions in the context of preventing workplace injuries have been mixed, while those in the context of reducing vessel oil transfer spills have been more consistent—generally, that Coast Guard inspection and enforcement activities have been effective in reducing spills during vessel oil transfer. Due to data limitations, it is difficult to identify the impact of increased inspection frequency on the prevention of UST releases. A key difference between data on releases at UST facilities and data on workplace injuries and oil transfer spills is that the dates recorded for workplace accidents and oil transfer spills are typically the actual date of occurrence whereas for UST releases the date recorded is the confirmed (or discovery) date of the release (i.e., the actual date when the UST release occurred is often unknown or uncertain). Therefore, it may be challenging to determine if a release

29

occurred before or after a particular inspection date and to identify the direct impact of increased inspections frequency on preventing releases at UST facilities.

Given the limited ability to analyze the impact of inspections on UST releases, a cost-benefit analysis is not feasible and beyond the scope of this analysis. However, for some perspective on the relative costs of inspections and potential benefits, consider the following back-of-the envelope calculation. The cost of conducting inspections is estimated at $96,348 per inspector with each completing 200 compliance inspections (US EPA 2000).<sup>18</sup> In Louisiana, there are roughly 4,400 UST facilities to be inspected. To inspect all of those facilities, the inspector cost is estimated to be roughly $2.12 million dollars. From fiscal year 2014-2016, 487 UST cleanups were completed in Louisiana with an average cleanup cost of $297,448 (Louisiana Department of Environmental Quality, personal communication, December 7, 2016). If the improved compliance from increased inspection frequency led to just 7.13 fewer releases that required cleanups over the course of 3 years, then the potential benefits of avoided cleanup costs would outweigh the direct cost of compliance inspections. Note that this comparison is for illustrative purposes only as it does not capture the full costs and benefits of UST compliance inspections. Specifically, it neither includes costs associated with training inspectors, enforcement, state administrative oversight nor UST owners’ compliance costs.<sup>19</sup> Furthermore, it does not include additional potential benefits accruing from avoided product loss and negative impacts on nearby property values, human health and ecosystem services that may be

\*18 In 2000, annual inspector cost were estimated at $70,000 includes salary, travel costs, benefits, managerial and secretarial support, and inspector equipment. To compare to the cleanup costs, which are the average from 2014-2016, this inspector cost of $70,000 was adjusted using the Consumer Price Index, and is equivalent to $96,348 in 2015 dollars.

\*19 Estimated direct compliance costs for individual facilities with UST release detection and prevention requirements in the final revisions to EPA’s Underground Storage Tank Regulations are small at approximately $715 per year for the average facility (US EPA 2015b).

30

substantial.20 This analysis provides evidence that increased inspection frequency due to the 3- year inspection mandate of the Energy Policy Act of 2005 improved compliance in Louisiana. Future research should examine the relationship between improved compliance and impact on UST release prevention.

> 20 For more information on estimated benefits of compliance with UST release detection and prevention requirements based on expert elicitations, refer to the “Assessment of the Potential Costs, Benefits, and Other Impacts of the Final Revisions to EPA’s Underground Storage Tank Regulations” (US EPA 2015b).

<page_number>31</page_number>

# References

Alberini, A., and D. Austin. 1999. “Strict liability as a deterrent in toxic waste management: Empirical evidence from accident and spill data.” *Journal of Environmental Economics and Management* 38:20–48.

Alberini, A., and D. Austin. 2002. “Accidents waiting to happen: Liability policy and toxic pollution releases.” *Review of Economics and Statistics* 84:729–74.

Alberini, A., E. Lichtenberg, D. Mancini, and G. Galinato. 2008. “Was it something I ate? Implementation of the FDA seafood HACCP program.” *American Journal of Agricultural Economics* 90 (1):28–41.

Becker, G.S. 1968. “Crime and punishment: An economic approach.” *Journal of Political Economy* 76:169–217.

Cohen, M.A. 1987. “Optimal enforcement strategy to prevent oil spills: An application of a principal-agent model with moral hazard.” *Journal of Law and Economics* 30:23–51.

Cyert, R., and J. G. March. 1963. *A Behavioral Theory of the Firm*. Englewood Cliffs, NJ: Prentice-Hall.

Deily, M.E., and W.B. Gray. 2007. “Agency structure and firm culture: OSHA, EPA, and the steel industry.” *Journal of Law and Economics* 23:685–709.

Earnhart, D. 2004a. “Panel data analysis of regulatory factors shaping environmental performance.” *Review of Economics and Statistics* 86:391–401.

Earnhart, D. 2004b. “Regulatory factors shaping environmental performance at publicly-owned treatment plants.” *Journal of Environmental Economics and Management* 48:655–81.

Eckert, H. 2004. “Inspections, warnings, and compliance: the case of petroleum storage regulation.” *Journal of Environmental Economics and Management* 47:232–59

Epple, D., and M. Visscher. 1984. “Environmental pollution: modeling occurrence, detection, and deterrence.” *Journal of Law and Economics* 27:29–60.

Glicksman, R.L., and D.H. Earnhart. 2007. “Comparative effectiveness of government interventions on environmental performance in the chemical industry.” *Stanford Environmental Law Journal* 26:317.

Greene, W. H. 1992. “A statistical model for credit scoring.” Working Paper no. EC-92-29, Department of Economics, Stern School of Business, New York University, New York, NY.

32

Grau M., and T. Groves. 1997. “The oil spill process: the effect of coast guard monitoring on oil spills.” *Environmental and Resource Economics* 10:315–39.

Gray, W., and M. Deily. 1996. “Compliance and enforcement: air pollution regulation in the US steel industry.” *Journal of Environmental Economics and Management* 31:96–111.

Gray, W., and J. Mendeloff. 2005. “The declining effects of OSHA inspections on manufacturing injuries, 1979–1998.” *Industrial and Labor Relations Review* 58: 571–87.

Gray, W., and R. Shadbegian. 2005. “When and why do plants comply? Paper mills in the 1980s.” *Law and Policy*, 27:238–61.

Gray, W., and J. Shimshack. 2011. “The effectiveness of environmental monitoring and enforcement: A review of the empirical evidence.” *Review of Environmental Economics and Policy* 5:3–24.

Guignet, Dennis, Robin R. Jenkins, Matthew Ranson, and Patrick J. Walsh. 2016. “Do Housing Values Respond to Underground Storage Tank Releases? Evidence from High-Profile Cases across the United States.” National Center for Environmental Economics Working Paper Number 2016-01.

Hanna, R., and P. Oliva. 2010. “The impact of inspections on plant-level air emissions.” *B.E. Journal of Economic Analysis and Policy* 10:1–31.

Haviland, A., R. Burns, W. Gray, T. Ruder, and J. Mendeloff. 2012. “A new estimate of the impact of OSHA Inspections on Manufacturing Injury Rates, 1998-2005.” *American Journal of Industrial Medicine* 55: 964-975.

Heyes, A. G. 1994. “Environmental enforcement when “inspectability” is endogenous: a model with over-shooting properties.” *Environmental and Resource Economics* 4:479-94.

Heyes, A. G. 2000. “Implementing environmental regulation: enforcement and compliance.” *Journal of Regulatory Economics* 17:107–29.

Innes, R. 1999a. “Remediation and self-reporting in optimal law enforcement.” *Journal of Public Economics* 72:379-93.

Innes, R. 1999b. “Self-policing and optimal law enforcement when violator remediation in valuable.” *Journal of Political Economy* 107: 1305-25.

Innes, R. 2001. “Violator avoidance activities and self-reporting in optimal law enforcement.” *Journal of Public Economics* 72:379-93.

Jenkins, Robin R., Dennis Guignet, and Patrick J. Walsh. 2014. “Prevention, Cleanup, and Reuse Benefits from the Federal UST Program.” National Center for Environmental Economics Working Paper No. 2014-05.

33

Kaplow, L., and S. Shavell. 1994. “Optimal law enforcement with self-reporting of behavior.” *Journal of Political Economy* 102:583-606.

Keohane, N. E. Mansur, and A. Voynov. 2009. “Averting regulatory enforcement: evidence from new source review.” *Journal of Economics and Management Strategy* 18:75–104.

Ko K., J. Mendeloff, and W. Gray. 2010. “The role of inspection sequence in compliance with US Occupational Safety and Health Administration’s (OSHA) standards: Interpretations and implications.” *Regulation & Governance* 4: 48-70.

Langpap C., and J. Shimshack. 2010. “Private citizen suits and public enforcement: Substitutes or complements?” *Journal of Environmental Economics and Management* 59: 235-249.

Levine, D.L., M.W. Toffel, and M.S. Johnson. 2012. “Randomized Government Safety Inspections Reduce Worker Injuries with No Detectable Job Loss.” *Science*, May 18.

Liu, L. 2012. “Spillover effects across environmental programs: the case of hazardous waste regulation in Michigan.” *Environmental Economics* 3(2):35-43.

Magat,W., and W.K. Viscusi. 1990. “Effectiveness of the EPA’s regulatory enforcement: The case of industrial effluent standards.” *Journal of Law and Economics* 33: 331–60.

Malik, A.S. 1990. “Avoidance, screening and optimum enforcement.” *Rand Journal of Economics* 21:341–53.

Malik, A.S. 1993. “Self-reporting and the design of policies for regulating stochastic pollution.” *Journal of Environmental Economics and Management* 24:241–57.

Marchus, M. “Going Beneath the Surface: Petroleum Pollution, Regulation, and Health” Job Market Paper, Brown University, Department of Economics. November 8, 2016.

Mendeloff, J., and W. Gray. 2005. “Inside the Black Box: How do OSHA Inspections Lead to Reductions in Workplace Injuries?” *Law and Policy* 27(2): 219-237.

Nadeau, L. 1997. “EPA effectiveness at reducing the duration of plant-level non-compliance.” *Journal of Environmental Economics and Management* 34:54–78.

Rogers, W. H. 1993. “Regression standard errors in clustered samples.” *Stata Technical Bulletin* 13: 19-23. Reprinted in *Stata Technical Bulletin Reprints*, vol. 3, 88-94.

Russell, C.W. Harrington, and W. J. Vaughan. 1986. *Economic models of monitoring and enforcement: Enforcing pollution control laws.* Resources for the Future, Washington, DC.

Scholz J. and W.B. Gray. 1990. OSHA enforcement and workplace injuries: A behavioral approach to risk assessment. *Journal of Risk Uncertainty* 3(3): 283-305.

34

Shimshack, J. 2014. “The economics of environmental monitoring and enforcement.” *Annual Review of Resource Economics* 6(1): 339-360.

Shimshack, J., and M.B. Ward. 2005. Regulator reputation, enforcement, and environmental compliance. *Journal of Environmental Economics and Management* 50: 519–40.

Shimshack, J., and M.B. Ward. 2008. “Enforcement and over-compliance.” *Journal of Environmental Economics and Management* 55 (1): 90–105.

Stafford, S.L. 2002. The effect of punishment on firm compliance with hazardous waste regulations. *Journal of Environmental Economics and Management.* 44: 290-308.

Stafford, S.L. 2003. Assessing the effectiveness of state regulations and enforcement of hazardous waste. *Journal of Regulatory Economics* 23: 27-41.

Stafford, S.L. 2012. Do carrots work? Examining the effectiveness of EPA’s compliance assistance program. *Journal of Policy Analysis and Management* 31(3): 533-555.

Stafford, S.L. 2014. Will additional federal enforcement improve the performance of pipelines in the U.S.? *International Review of Law and Economics.* 37(C): 137-146.

Talley, Wayne K., Di Jin, and Hauke Kite-Powell. 2005. Post OPA-90 Vessel Oil Transfer Spill Prevention: The Effectiveness of Coast Guard Enforcement. *Environmental & Resource Economics* 30: 93-114.

Thornton, D., N., Gunningham, and R. Kagan. 2005. General deterrence and corporate environmental behavior. *Law and Policy*, 27:262-288

U.S. Census Bureau. “Summary File.” 2009-2013 American Communities Survey. U.S. Census Bureau’s American Community Survey Office, 2015.

U.S. Department of Agriculture. Natural Resources Conservation Service, Soil Survey Staff. Web Soil Survey. http://websoilsurvey.nrcs.usda.gov/ (accessed February 2015).

U.S. Government Accountability Office. 2001. Improved Inspections and Enforcement Would Better Ensure the Safety of Underground Storage Tanks. GAO-01-464, Washington, D.C.

U.S. Geological Survey. 2003. *Ground-Water Depletion across the Nation.* U.S. Geological Survey Fact Sheet 103-03. http://pubs.usgs.gov/fs/fs-103-03/#pdf (last updated January 9, 2013).

U.S. Environmental Protection Agency. 2000. *Report to Congress on a Compliance Plan for the Underground Storage Tank Program.* EPA 510-R-00-001.

U.S. Environmental Protection Agency. 2005. *Semiannual Report of UST Performance Measures: End of Year Fiscal Year 2005 (April 1, 2005-September 30, 2005).* Washington, D.C.

35

U.S. Environmental Protection Agency. 2011. *Assessment of the Potential Costs, Benefits, and Other Impacts of the Proposed Revisions to EPA’s Underground Storage Tank Regulations*. Prepared by Industrial Economics, Incorporated. August. http://www.epa.gov/sites/production/files/2015-07/documents/regs2015-ria.pdf (accessed January 28, 2016).

U.S. Environmental Protection Agency. 2015a. *Semiannual Report of UST Performance Measures: End Fiscal Year 2015 (October 1, 2014-September 30, 2015)*. Washington, D.C.

U.S. Environmental Protection Agency. 2015b. *Assessment of the Potential Costs, Benefits, and Other Impacts of the Final Revision to EPA’s Underground Storage Tank Regulations*. Prepared by Industrial Economics, Incorporated. April. <u>http://www.regulations.gov/#!documentDetail;D=EPA-HQ-UST-2011-0301-0472</u> (accessed February 18, 2016)

U.S. Environmental Protection Agency. 2016. *Semiannual Report of UST Performance Measures: End Fiscal Year 2016 (October 1, 2015-September 30, 2016)*. Washington, D.C.

US Government Printing Office. 2013. Electronic Code of Federal Regulations Part 280. Accessed Dec 1, 2106 at <u>http://www.ecfr.gov</u>

Viscusi, W.K. 1979. :”The Impact of Occupational Safety and Health Safety and Health Regulation.” *Bell Journal of Economics* 10(1): 117-140.

Williams, R. L. 2000. “A note on robust variance estimation for cluster-correlated data.” *Biometrics* 56: 645-646.

Wooldridge, J. M. 2002. *Econometric Analysis of Cross Section and Panel Data*. Cambridge, MA: MIT Press.

36

**Table 1: Summary statistics of explanatory variables**


<table>
  <thead>
    <tr>
        <th>Variable</th>
        <th>Mean</th>
        <th>Std. Dev.</th>
        <th>Median</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Years_LastInspection</td>
        <td>3.65</td>
        <td>1.57</td>
        <td>3.02</td>
    </tr>
    <tr>
        <td>Total_Inspection†</td>
        <td>1.48</td>
        <td>0.64</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Last_Noncompliance†</td>
        <td>0.46</td>
        <td>0.50</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Past_Noncompliance†</td>
        <td>0.19</td>
        <td>0.43</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Last_Release†</td>
        <td>0.05</td>
        <td>0.23</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Number_Tanks†</td>
        <td>2.82</td>
        <td>1.17</td>
        <td>3</td>
    </tr>
    <tr>
        <td>Age_OldestTank (years)</td>
        <td>21.69</td>
        <td>9.77</td>
        <td>21.68</td>
    </tr>
    <tr>
        <td>Mean_TankCapacity (1000’s of gallons)</td>
        <td>8.36</td>
        <td>3.97</td>
        <td>8</td>
    </tr>
    <tr>
        <td>Depth_WaterTable (meters)</td>
        <td>0.47</td>
        <td>0.37</td>
        <td>0.31</td>
    </tr>
    <tr>
        <td>Soil_MostPermeable†</td>
        <td>0.43</td>
        <td>0.50</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Distance_FieldOffice</td>
        <td>20.94</td>
        <td>16.16</td>
        <td>17.08</td>
    </tr>
    <tr>
        <td>Density_Population (100’s people/sq mile)</td>
        <td>13.82</td>
        <td>19.53</td>
        <td>6.10</td>
    </tr>
    <tr>
        <td>Income_Median (1000’s of USDs)</td>
        <td>43.83</td>
        <td>19.63</td>
        <td>40.83</td>
    </tr>
    <tr>
        <td>Region_Acadiana†</td>
        <td>0.18</td>
        <td>0.38</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Region_NE†</td>
        <td>0.19</td>
        <td>0.39</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Region_NW†</td>
        <td>0.11</td>
        <td>0.31</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Region_SE†</td>
        <td>0.23</td>
        <td>0.42</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Region_SW†</td>
        <td>0.10</td>
        <td>0.30</td>
        <td>0</td>
    </tr>
    <tr>
        <td>LAFiscalYear_Q2†</td>
        <td>0.20</td>
        <td>0.40</td>
        <td>0</td>
    </tr>
    <tr>
        <td>LAFiscalYear_Q3†</td>
        <td>0.26</td>
        <td>0.44</td>
        <td>0</td>
    </tr>
    <tr>
        <td>LAFiscalYear_Q4†</td>
        <td>0.30</td>
        <td>0.46</td>
        <td>0</td>
    </tr>
    <tr>
        <td>Contract_Inspectorᵃ†</td>
        <td>0.49</td>
        <td>0.50</td>
        <td>0</td>
    </tr>
    <tr>
        <td>State_OperatorTrainingᵃ†</td>
        <td>0.46</td>
        <td>0.50</td>
        <td>0</td>
    </tr>
    <tr>
        <td>State_TotalHurricaneVisitsᵇ</td>
        <td>20.14</td>
        <td>117.09</td>
        <td>0</td>
    </tr>
  </tbody>
</table>


Notes: The variables marked by ᵃ were included only in the noncompliance equation whereas the variables marked by ᵇ was included only in the inspection equation. The variables marked by † are the discrete variables for which the median value was used in calculating the predicted probabilities in table 3.

37

**Table 2: Estimation results of the censored bivariate probit model**


<table>
<thead>
<tr>
<th> </th>
<th colspan="2">Inspection Equation</th>
<th colspan="2">Noncompliance Equation</th>
</tr>
<tr>
<th>Variable</th>
<th>Coefficient</th>
<th>Standard Error</th>
<th>Coefficient</th>
<th>Standard Error</th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>-2.9141***</td>
<td>0.053</td>
<td>-0.5047</td>
<td>0.549</td>
</tr>
<tr>
<td>Years_LastInspection</td>
<td>0.3322***</td>
<td>0.007</td>
<td>0.0814*</td>
<td>0.049</td>
</tr>
<tr>
<td>Years_LastInspection*Last_Noncompliance</td>
<td>0.0306**</td>
<td>0.012</td>
<td>-0.0407*</td>
<td>0.022</td>
</tr><tr>
<td>Total_Inspection</td>
<td>0.2077***</td>
<td>0.012</td>
<td>-0.1109**</td>
<td>0.047</td>
</tr><tr>
<td>Last_Noncompliance</td>
<td>-0.0272</td>
<td>0.030</td>
<td>0.5484***</td>
<td>0.090</td>
</tr><tr>
<td>Past_Noncompliance</td>
<td>-0.0004</td>
<td>0.014</td>
<td>0.1444***</td>
<td>0.051</td>
</tr><tr>
<td>Last_Release</td>
<td>0.3975***</td>
<td>0.038</td>
<td>-0.1126</td>
<td>0.093</td>
</tr><tr>
<td>Number_Tanks</td>
<td>-0.0212***</td>
<td>0.006</td>
<td>-0.0205</td>
<td>0.015</td>
</tr><tr>
<td>Age_OldestTank</td>
<td>0.0050***</td>
<td>0.001</td>
<td>0.0120***</td>
<td>0.002</td>
</tr><tr>
<td>Mean_TankCapacity</td>
<td>0.0135***</td>
<td>0.002</td>
<td>-0.0240***</td>
<td>0.005</td>
</tr><tr>
<td>Depth_WaterTable</td>
<td>0.0397**</td>
<td>0.018</td>
<td>0.1560***</td>
<td>0.052</td>
</tr><tr>
<td>Soil_MostPermeable</td>
<td>-0.0221</td>
<td>0.015</td>
<td>-0.0318</td>
<td>0.039</td>
</tr><tr>
<td>Distance_FieldOffice</td>
<td>-0.0002</td>
<td>0.000</td>
<td>0.0013</td>
<td>0.001</td>
</tr><tr>
<td>Density_Population</td>
<td>-0.0003</td>
<td>0.000</td>
<td>0.0007</td>
<td>0.001</td>
</tr><tr>
<td>Income_Median</td>
<td>-0.0002</td>
<td>0.000</td>
<td>-0.0002</td>
<td>0.001</td>
</tr><tr>
<td>Region_Acadiana</td>
<td>-0.0631***</td>
<td>0.021</td>
<td>0.0698</td>
<td>0.059</td>
</tr><tr>
<td>Region_NE</td>
<td>0.0385*</td>
<td>0.022</td>
<td>0.0809</td>
<td>0.058</td>
</tr><tr>
<td>Region_NW</td>
<td>0.1100***</td>
<td>0.021</td>
<td>0.1973***</td>
<td>0.069</td>
</tr><tr>
<td>Region_SE</td>
<td>0.0608***</td>
<td>0.023</td>
<td>0.1611***</td>
<td>0.058</td>
</tr><tr>
<td>Region_SW</td>
<td>-0.0415*</td>
<td>0.025</td>
<td>0.3287***</td>
<td>0.072</td>
</tr>
<tr>
<td>LAFiscalYear_Q2</td>
<td>-0.0956***</td>
<td>0.022</td>
<td>0.0049</td>
<td>0.055</td>
</tr>
<tr>
<td>LAFiscalYear_Q3</td>
<td>-0.0116</td>
<td>0.020</td>
<td>-0.0261</td>
<td>0.049</td>
</tr>
<tr>
<td>LAFiscalYear_Q4</td>
<td>0.0316</td>
<td>0.020</td>
<td>-0.0023</td>
<td>0.049</td>
</tr>
<tr>
<td>Contract_Inspector</td>
<td></td>
<td></td>
<td>0.1003***</td>
<td>0.037</td>
</tr>
<tr>
<td>State_OperatorTraining</td>
<td></td>
<td></td>
<td>-0.1835***</td>
<td>0.045</td>
</tr>
<tr>
<td>State_TotalHurricaneVisits</td>
<td>-0.0009***</td>
<td>0.000</td>
<td></td>
<td></td>
</tr>
<tr>
<td>ρ</td>
<td>-0.1125</td>
<td>0.164</td>
<td></td>
<td></td>
</tr>
</tbody>
</table>


Log-likelihood: -22,169
Number of facilities: 4,424
Censored Observations: 102,512
Uncensored Observations: 5,769

Notes: Cluster-robust standard errors. Statistical significance at the 1%, 5% and 10% are represented by \*\*\*, \*\*, and \*, respectively.

38

# Table 3: Predicted probability of noncompliance at a hypothetical representative facility


<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">Predicted Pr(Noncompliance)</th>
        <th> </th>
    </tr>
    <tr>
        <th> </th>
        <th colspan="2">Years Since Last Inspection</th>
        <th rowspan="2">Change in Predicted<br/>Pr (Noncompliance)</th>
    </tr>
    <tr>
        <th> </th>
        <th>6 Years</th>
        <th>3 Years</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Last_Noncompliance=Mean</td>
        <td>0.49***<br/>(0.026)</td>
        <td>0.38***<br/>(0.023)</td>
        <td>-0.11</td>
    </tr>
    <tr>
        <td>Last_Noncompliance=0</td>
        <td>0.44***<br/>(0.027)</td>
        <td>0.31***<br/>(0.023)</td>
        <td>-0.13</td>
    </tr>
    <tr>
        <td>Last_Noncompliance=1</td>
        <td>0.56***<br/>(0.029)</td>
        <td>0.47***<br/>(0.026)</td>
        <td>-0.09</td>
    </tr>
  </tbody>
</table>


Notes: The hypothetical representative facility has the mean values for all continuous explanatory variables, the mean value for noncompliance at the last inspection, and the median values for all other discrete explanatory variables. See Table 1 for means and medians. Standard errors in parentheses. Statistical significance at the 1%, 5% and 10% are represented by \*\*\*, \*\*, and \*, respectively.

39

# Figures

Figure 1. National underground storage tank compliance rate


<table>
  <thead>
    <tr>
        <th>Fiscal Year</th>
        <th>Percent of Facilities in Compliance (%)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>2005</td>
        <td>66.0</td>
    </tr>
    <tr>
        <td>2006</td>
        <td>62.0</td>
    </tr>
    <tr>
        <td>2007</td>
        <td>63.0</td>
    </tr>
    <tr>
        <td>2008</td>
        <td>66.0</td>
    </tr>
    <tr>
        <td>2009</td>
        <td>66.0</td>
    </tr>
    <tr>
        <td>2010</td>
        <td>68.5</td>
    </tr>
    <tr>
        <td>2011</td>
        <td>71.0</td>
    </tr>
    <tr>
        <td>2012</td>
        <td>71.5</td>
    </tr>
    <tr>
        <td>2013</td>
        <td>71.5</td>
    </tr>
    <tr>
        <td>2014</td>
        <td>72.5</td>
    </tr>
    <tr>
        <td>2015</td>
        <td>81.0</td>
    </tr>
  </tbody>
</table>


Note: The Energy Policy Act of 2005 provisions for underground storage tanks include a transition phase from August 8, 2005 to August 8, 2007 during which all states receiving Subtitle I funding for their UST programs are required to inspect all active UST facilities that had not been inspected since 1998. After August 8, 2007 the time between concurrent compliance inspections at an UST facility cannot exceed three years.

40

Figure 2. Louisiana inspection, compliance and confirmed releases (fiscal year 2001-2012)


<table>
  <thead>
    <tr>
        <th>Louisiana Fiscal Year</th>
        <th>Percent of Facilities Inspected</th>
        <th>Percent of Inspected Facilities Issued Noncompliance Citation(s)</th>
        <th>Percent of Facilities with a Confirmed Release</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>2001</td>
        <td>7</td>
        <td>35</td>
        <td>1</td>
    </tr>
    <tr>
        <td>2002</td>
        <td>12</td>
        <td>46</td>
        <td>1</td>
    </tr>
    <tr>
        <td>2003</td>
        <td>11</td>
        <td>45</td>
        <td>3</td>
    </tr>
    <tr>
        <td>2004</td>
        <td>16</td>
        <td>46</td>
        <td>3</td>
    </tr>
    <tr>
        <td>2005</td>
        <td>11</td>
        <td>39</td>
        <td>2</td>
    </tr>
    <tr>
        <td>2006</td>
        <td>8</td>
        <td>43</td>
        <td>2</td>
    </tr>
    <tr>
        <td>2007</td>
        <td>12</td>
        <td>47</td>
        <td>3</td>
    </tr>
    <tr>
        <td>2008</td>
        <td>25</td>
        <td>56</td>
        <td>4</td>
    </tr>
    <tr>
        <td>2009</td>
        <td>32</td>
        <td>48</td>
        <td>3</td>
    </tr>
    <tr>
        <td>2010</td>
        <td>37</td>
        <td>45</td>
        <td>3</td>
    </tr>
    <tr>
        <td>2011</td>
        <td>29</td>
        <td>39</td>
        <td>4</td>
    </tr>
    <tr>
        <td>2012</td>
        <td>35</td>
        <td>34</td>
        <td>3</td>
    </tr>
  </tbody>
</table>


*   **............** Percent of Facilities Inspected
*   **————** Percent of Inspected Facilities Issued Noncompliance Citation(s)
*   **- - - -** Percent of Facilities with a Confirmed Release

**Note:** The Energy Policy Act of 2005 provisions for underground storage tank inspections included a transition phase from August 8, 2005 to August 8, 2007 during which states were required to inspect all active UST facilities that had not been inspected since 1998. This transition period was delayed in Louisiana due to Hurricane Katrina and Rita (August and September 2005, respectively).

41

Figure 3. Predicted probability of noncompliance at typical facility


<table>
  <thead>
    <tr>
        <th>Years Since Last Inspection</th>
        <th>Predicted Probability of Noncompliance</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>3</td>
        <td>0.41</td>
    </tr>
    <tr>
        <td>4</td>
        <td>0.44</td>
    </tr>
    <tr>
        <td>5</td>
        <td>0.47</td>
    </tr>
    <tr>
        <td>6</td>
        <td>0.52</td>
    </tr>
  </tbody>
</table>

Note: Shaded area represnts the 95% confidence intervals.

Note: The hypothetical representative facility has the mean values for all continuous explanatory variables, the mean value for noncompliance at the last inspection, and the median values for all other discrete explanatory variables. See Table 1 for means and medians.

42