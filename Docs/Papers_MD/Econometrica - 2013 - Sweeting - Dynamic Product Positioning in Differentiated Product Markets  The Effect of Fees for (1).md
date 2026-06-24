Econometrica, Vol. 81, No. 5 (September, 2013), 1763–1803

Check for updates



# DYNAMIC PRODUCT POSITIONING IN DIFFERENTIATED PRODUCT MARKETS: THE EFFECT OF FEES FOR MUSICAL PERFORMANCE RIGHTS ON THE COMMERCIAL RADIO INDUSTRY

### BY ANDREW SWEETING<sup>1</sup>

This article predicts how radio station formats would change if, as was recently proposed, music stations were made to pay fees for musical performance rights. It does so by estimating and solving, using parametric approximations to firms’ value functions, a dynamic model that captures important features of the industry such as vertical and horizontal product differentiation, demographic variation in programming tastes, and multi-station ownership. The estimated model predicts that high fees would cause the number of music stations to fall significantly and quite quickly. For example, a fee equal to 10% of revenues would cause a 4.6% drop in the number of music stations within 2½ years, and a 9.4% drop in the long run. The size of the change is limited, however, by the fact that many listeners, particularly in demographics that are valued by advertisers, have strong preferences for music programming.

KEYWORDS: Product differentiation, dynamic oligopoly, value function approximation, radio, copyright.

## 1. INTRODUCTION

THIS ARTICLE DEVELOPS AND ESTIMATES a dynamic oligopoly model to predict the effect on product variety of legislation (the Performance Rights Act<sup>2</sup>), introduced into Congress in 2009 with the support of the Obama Administration and members of both parties, requiring music radio stations to pay fees for musical performance rights. This legislation provides one particularly clear example, among many, of a policy which, intentionally or unintentionally, changes the incentives of firms to offer particular types of product.<sup>3</sup> In the long run, the changes in product variety that these policies cause could have much larger effects on welfare than changes in the prices of existing products.

An analysis of variety requires a model with differentiated products, and when firms have to pay significant sunk costs to develop new products or reposition existing ones, a dynamic model is required to predict how a policy change

<sup>1</sup>I would like to thank Jerry Hausman, Igal Hendel, Aviv Nevo, Amil Petrin, Ariel Pakes, Rob Porter, Steve Berry, Pat Bayer, Peter Arcidiacono, Kate Ho, Allan Collard-Wexler, Paul Ellickson, Arie Beresteanu, three referees, and seminar participants for valuable comments. I would like to thank the National Association of Broadcasters and the Center for the Study of Industrial Organization at Northwestern University for financial support. All errors are my own.

<sup>2</sup>H.R. 848 and S. 379, 111th Congress.

<sup>3</sup>Other examples include gasoline taxes, fuel efficiency standards, and trade policies that affect the incentives of domestic automakers to produce particular types of vehicles (Berry, Levinsohn, and Pakes (1993)), and taxes, labeling, and advertising restrictions that increasingly affect the incentives of food and beverage manufacturers to make healthier products.

© 2013 The Econometric Society DOI: 10.3982/ECTA7473

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1764 ANDREW SWEETING

will affect an industry’s evolution.<sup>4</sup> Solving and estimating dynamic models with differentiated products is not straightforward because a large state space is required to capture the features of the market that are likely to affect how the industry evolves. For example, in my setting, it is necessary to allow for at least eight types of product (programming formats), a large number of multi-product firms (up to 18 firms per market in my data, owning up to eight stations each), and vertical differentiation (on average, the largest station in a market has more than 60 times as many listeners as the smallest station). I address the problem of the large state space by using parametric approximations to the value function when estimating and solving the model.<sup>5</sup> The use of value function approximation to solve dynamic models has been suggested by Benitez-Silva, Hall, Hitsch, Pauletto, and Rust (2000), Farias, Saure, and Weintraub (2012), and Arcidiacono, Bayer, Bugni, and James (2012)<sup>6</sup> and used in applied contexts by Hendel and Nevo (2006), Fowlie, Reguant, and Ryan (2011), and Barwick and Pathak (2012). I incorporate value function approximation into estimation procedures that build off the methods proposed by Aguirregabiria and Mira (AM hereafter) (2007, 2010) and Pakes, Ostrovsky, and Berry (POB) (2007). These procedures give quite similar estimates.<sup>7</sup> To provide additional confidence in my methods and results, I also compute estimates based on procedures that approximate value functions using forward simulation. I show that my preferred estimates lie within bounds on the structural parameters computed using forward simulation and the moment inequality approach proposed by Pakes, Porter, Ho, and Ishii (PPHI) (2011).

The Performance Rights Act was motivated by the declining revenues of the recording industry. The legislation proposed that commercial, broadcast radio stations, whose primary programming is music, should pay for musical performance rights, which are owned by musicians, performers, and record labels, in

<sup>4</sup>Static models may be appropriate for settings where firms can change their assortments quickly and easily. For example, Draganska, Mazzeo, and Seim (2009) used a static model to predict how the set of ice cream flavors that firms sell in particular markets would change after a merger, out of the set of flavors that firms already produce.

<sup>5</sup>In an earlier version of this paper, Sweeting (2011), I presented Monte Carlo results for my estimation and solution procedures based on a much-simplified version of my model.

<sup>6</sup>Outside of the economics literature, Bertsekas and Ioffe (1996), Bertsekas and Yu (2007), Bertsekas (2010), and Ma and Powell (2009) investigated the effectiveness of approximate dynamic programming approaches.

<sup>7</sup>AM (2007) proposed estimating dynamic models using an iterated nested pseudo-likelihood procedure where players’ conditional choice probabilities are updated during estimation. This procedure may be feasible when a nested fixed point procedure would not be feasible. AM (2010) considered a modified version of this procedure where other players’ choice probabilities are not updated during estimation. I present some results using estimators based on both of these approaches. POB presented evidence that a moment-based objective function, which matches entry rates that are averaged across states, may be superior to likelihood-based ones in small samples. I therefore also consider a version of the modified AM (2010) estimator where this type of objective function is used, rather than one based on the pseudo-likelihood.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING

1765
<page_number>1765</page_number>

addition to the fees that they currently pay for composition rights.<sup>8</sup> Requiring stations to pay fees for performance rights would bring the broadcast industry in the United States into line with the broadcast industries in most other countries, and the cable, satellite, and internet radio industries in the United States, where stations already pay fees. The legislation proposed that for music stations with revenues above some cap, fees would be determined as a percentage of advertising revenues, and would not depend on the exact amount of music that the station played. Noncommercial stations and stations that provide primarily talk programming were exempt. The legislation did not specify how high the fees should be, so that, in the absence of an agreement between the radio and recording industries, they would be determined by copyright judges. Media law experts have argued that existing case law might justify performance rights fees as high as 25% of advertising revenues, which would have much larger effects on stations’ incentives to play music than the 2–3% fees that music stations currently pay for composition rights.<sup>9</sup>

Not surprisingly, broadcasters argued that fees at this level would make it unprofitable for them to play music, and they predicted that many music stations would switch to nonmusic programming.<sup>10</sup> This claim cannot be evaluated using existing data because this level of fee has never been levied on broadcast stations in the U.S., and, in countries where performance rights fees are levied, programming is often affected by content regulation or the presence of state-owned broadcasters. For this reason, I develop, estimate, and solve a discrete-choice model of format choice to predict how much format variety and music listenership would change in a large sample of markets if fees equal to 10% or 20% of music station revenues were introduced. The model captures important features of the industry that should affect the response to fees. In particular, I let different demographic groups have heterogeneous programming tastes,

<sup>8</sup>For more details, see U.S. GAO (2010).

<sup>9</sup>See, for example, http://www.broadcastlawblog.com/2010/03/articles/music-rights/copyright-royalty-board-approves-settlement-for-sound-recording-royalty-rates-for-new-subscription-services-any-hints-as-to-what-a-broadcast-performance-royalty-would-be/ (accessed December 5, 2010). XM Sirius paid 8% of its subscription revenues for performance rights in 2010–2012, even though some of its programming is not musical, and this fee included a discount recognizing that satellite radio was struggling to become established (Federal Register vol. 75, p. 5513 (2010-02-03)). Companies providing audio programming on cable pay 15% of revenues (Federal Register vol. 75, p. 14075 (2010-03-24)). The legislation was not passed, but in June 2012, Clear Channel struck a landmark deal with the Big Machine record label in which it agreed to pay a fee when it plays Big Machine’s songs on its broadcast stations in exchange for a rebate when it broadcasts the label’s songs on the internet where performance fees are levied, although no more details of the deal were released (Wall Street Journal, June 6, 2012, accessed on that date). The same article noted that Pandora pays approximately 60% of its revenues in fees.

<sup>10</sup>For example, the National Association of Broadcasters Radio Board Chairman, Steven Newberry, stated before the House Judiciary committee that “the number of stations playing music would dramatically decrease” because of the Act (The Performance Rights Act, Hearing Before the Committee on the Judiciary, House of Representatives, 111th Congress, 1st session on HR 848, March 10, 2009, Oral Testimony of Steven Newberry).

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1766</page_number> ANDREW SWEETING

and for advertisers (broadcast stations’ primary source of revenue) to place different values on listeners with different demographics. As I illustrate using counterfactuals, taste heterogeneity has important effects because the listeners most valued by advertisers (white listeners aged 25–49, especially women) prefer stations in music formats (such as Adult Contemporary, Country, and Rock). A station that switches to a nonmusic format (such as News, Easy Listening, Religion, or Spanish-language programming) will therefore tend to attract less valuable listeners, while the remaining music stations will see their audiences and revenues increase, offsetting the effects of fees. The model also allows for the fact that format switching can be costly, as a station may lose the value in its established relationships with listeners and advertisers, as well as having to replace all of its on-air staff, its programming director, and many of its advertising sales staff.<sup>11</sup> I use counterfactuals to investigate how these costs affect how many stations switch formats when fees are introduced, and the speed of adjustment.

While one could study the effects of policies favoring particular types of product in many industries, several features of the radio industry make it ideal for estimating this type of model. First, the industry has many local markets, each with its own local stations. This provides thousands of station-level observations with which to estimate the main parameters of the model, with exogenous variation in market demographics, as well as station characteristics, helping to provide identification. These sources of identification are explicitly used in constructing the type of moment-based objective function suggested by POB. Second, widely accepted programming categories (formats) facilitate the estimation of a model of product positioning where firms make discrete choices. Third, there is excess demand for station licenses in most markets because of spectrum constraints. As a result, it is possible to abstract away from station entry and exit decisions to focus on repositioning across formats.<sup>12</sup>

Because of the desirability of reaching the types of listeners who like music, fees are predicted to have only moderate, but still significant, long-run effects on the number of music stations. For example, 10% fees are predicted to reduce the number of music stations after 20 years by 9.4%, with music listening falling by 6.3%. Fees of 20% would produce changes that are approx-

<sup>11</sup>The manager of a station in the Raleigh–Durham market that moved from Country to Sports programming in 2007 described how the station replaced all of its on-air staff and all of its advertising sales staff, and how it had chosen to play Rock music for a couple of months before moving to Sports to “kill off” its original audience so that it would face fewer complaints when it finally started its Sports programming. The switch also required an extensive, 9-month planning process which involved hiring format consultancies to advise the station’s owners on their strategic options. At the time, the owners predicted that the move would take at least two years to pay for itself.

<sup>12</sup>When evaluating the Performance Rights Act, the U.S. GAO (2010, p. 25) argued that a reduction in the number of stations was unlikely given that the FCC reported substantial excess demand for broadcast licenses even when advertising revenues sank substantially in 2008. Of course, exceptionally high fees would likely induce exit.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1767</page_number>

imately twice as large, and all of these effects would be much larger if there was less taste heterogeneity. My preferred estimates imply that the transition happens quite quickly with the majority of these long-run changes completed within 5 years. These counterfactual results contribute to the small literature that seeks to understand what determines product variety (examples include Borenstein and Netz (2002), George and Waldfogel (2003), Watson (2009), and also looking at radio Berry and Waldfogel (2001) and Sweeting (2010)), as well as the recent literature using static structural models to predict how specific policy-interventions or mergers would affect product characteristics (Fan (2012), Nishida (2012), and Datta and Sudhir (2012)). Jeziorski (2013) estimates a closely-related dynamic model to understand the benefits and costs of radio mergers.<sup>13</sup> While I do allow for multiple station ownership to affect format choices, my model is simpler than Jeziorski’s, which allows for endogenous future mergers whereas I assume that firms expect the current ownership structure of the industry to persist. For this reason I focus on a later data period (2002–2005) when there was substantially less merger activity than immediately following the 1996 Telecommunications Act.<sup>14</sup>

The paper is structured as follows. Section 2 describes the data and Section 3 presents the model. Section 4 outlines the procedures used to solve and estimate the model. Section 5 presents the baseline coefficient estimates, and compares them with alternative estimates from other procedures. Section 6 presents the results of the counterfactual experiments to investigate the effects of different levels of fees. Section 7 concludes. The Supplemental Material (Sweeting (2013)) provides details about implementation.

# 2. DATA

I estimate the model using data from 102 local radio markets from 2002–2005, and the data come from BIAfn’s *MediaAccess Pro* database (BIAfn; BIA Financial Network (2006)) unless otherwise noted. From the 274 markets that Arbitron surveyed throughout these years, the ten largest markets are dropped to reduce the computational burden, and I exclude 162 markets where more than 6% of listening was to stations based in other markets so as to avoid modeling cross-market interactions. By linking Arbitron’s market definitions (which usually correspond to Metropolitan Statistical Areas (MSAs)) to the U.S. Census’s *County Population Estimates*, I measure each market’s population in 18 mutually exclusive demographic groups, which are the product of three age categories (12–24, 25–49, and 50 plus), three ethnic categories (black,

<sup>13</sup>Mooney (2010a, 2010b) and O’Gorman and Smith (2008) also estimated structural models to understand the effects of mergers in the radio industry.

<sup>14</sup>Relative to Jeziorski, my model is also simpler in that I use a reduced form revenue function rather than structurally modeling the market for advertising, and I restrict the number of stations that a firm can move each period. With additional computation, both a structural model of advertising and endogenous mergers could be included in the current analysis.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1768  ANDREW SWEETING

white, and Hispanic) and gender.<sup>15</sup> The sample markets range in population from Casper, WY (with 55,000 people aged 12 and above in 2002) to Atlanta, GA (3.4 million).

The data contain Arbitron’s estimates of stations’ audience shares, which are based on listening by people aged 12 and above, in the Spring and Fall quarters each year. Potential listening by children younger than 12 is ignored. I use data from these quarters, so that there are two periods each year in my model, from Spring 2002 to Spring 2005. I exclude noncommercial stations (approximately 18% of all stations), as these stations lack audience data. I also drop 3% of stations for which Arbitron did not report share estimates in four or more of the sample quarters, because these estimates fell below its Minimum Reporting Standard.<sup>16</sup> I also drop AM stations that appear to have been simulcasting what was being broadcast on an FM station with the same owner. These deletions leave a sample of 2,375 stations and 16,566 station-market-period observations. I turn audience shares into market shares for estimating demand by assuming that every person in the market could listen to the radio for up to 6 hours per day (average listening is around 2½ hours per day) and by using Arbitron’s estimate of how many people listened to the radio in a given market-quarter.<sup>17</sup> BIAfn reports an estimate of advertising revenues, based on a proprietary formula, for 96% of station-years in the sample between 2002 and 2004. While it is impossible to know whether these estimates are accurate for individual stations, they are widely cited within the industry, so they should be useful for approximating how revenues per listener differ with demographics and across markets, which is how they are used here.

BIAfn lists a primary programming format for each station in each quarter, and categorizes these into 20 broader format categories.<sup>18</sup> I aggregate these categories into seven active formats, listed in Table I, which pick up the main demographic variations in listener tastes. For example, News/Talk stations at-

<sup>15</sup> The *County Population Estimates* are calculated for July each year. I choose to assign these to the Spring quarter and use linear interpolation to find estimates for the Fall. My estimation of the process by which demographics evolve explicitly addresses the fact that I only observe demographics every other period (see Appendix C of the Supplemental Material for details).

<sup>16</sup> The shares of stations that do not meet the MRS in a specific period are assumed to be 10% less than the smallest share that Arbitron does report the market in that quarter.

<sup>17</sup> Arbitron’s estimate of the proportion of time spent listening in each market (APR) is not reported in *MediaAccessPro*. However, I was able to collect APRs from M Street’s STAR database for 2002 and Spring 2003, and BIAfn were able to provide me with these numbers from Fall 2004. The APRs for Fall 2003 and Spring 2004 were interpolated, which is a reasonable approach, as they change very slowly over time.

<sup>18</sup> A music station may play (say) a Country music song without being in the Country format, or have a personality-based morning show without being in the Talk format. Instead, the format reflects the most common type of programming on the station, and in the proposed legislation it was assumed that music stations would pay fees based on all of their advertising revenues without any attempt to allocate them between periods of music and talk programming. In the counterfactuals, I ignore the fact that stations in nonmusic formats might choose to buy program-specific licenses in order to play a few hours of music programming.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

# TABLE I
## FORMAT AGGREGATION<sup>a</sup>

<table>
  <thead>
    <tr>
        <th rowspan="2">Aggregated Format<br/>(* = music format)</th>
        <th rowspan="2">BIAfn Format Categories</th>
        <th colspan="2">Number of Station-Qtrs</th>
        <th rowspan="2">Mean Audience Share</th>
        <th colspan="10" rowspan="2">% of Listeners in Demographic</th>
    </tr>
    <tr>
        <th>FM</th>
        <th>AM</th>
        <th>Male</th>
        <th>12–24</th>
        <th>25–49</th>
        <th>Black</th>
        <th>Hisp.</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>1. AC/CHR*</td>
        <td>Adult Contemporary,<br/>Contemporary Hit Radio</td>
        <td>3,299</td>
        <td>47</td>
        <td>5.3%</td>
        <td>39.1</td>
        <td>28.4</td>
        <td>49.8</td>
        <td>13.1</td>
        <td>15.4</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>2. Rock, AOR/Classic Rock*</td>
        <td>Rock, AOR/Classic Rock</td>
        <td>2,597</td>
        <td>6</td>
        <td>4.7%</td>
        <td>70.5</td>
        <td>25.6</td>
        <td>63.0</td>
        <td>2.3</td>
        <td>8.9</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>3. Country*</td>
        <td>Country</td>
        <td>1,799</td>
        <td>223</td>
        <td>6.3%</td>
        <td>47.4</td>
        <td>15.1</td>
        <td>44.2</td>
        <td>2.4</td>
        <td>5.0</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>4. Urban*</td>
        <td>Urban, Gospel</td>
        <td>1,172</td>
        <td>609</td>
        <td>4.5%</td>
        <td>43.5</td>
        <td>29.3</td>
        <td>50.2</td>
        <td>83.6</td>
        <td>5.0</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>5. News/Talk</td>
        <td>News, Talk, Sports</td>
        <td>267</td>
        <td>2,656</td>
        <td>3.2%</td>
        <td>60.7</td>
        <td>3.7</td>
        <td>38.1</td>
        <td>7.5</td>
        <td>5.1</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>6. Spanish</td>
        <td>Spanish-language</td>
        <td>562</td>
        <td>460</td>
        <td>2.2%</td>
        <td>51.4</td>
        <td>20.0</td>
        <td>59.9</td>
        <td>0.8</td>
        <td>91.3</td>
        <td colspan="5"></td>
    </tr>
    <tr>
        <td>7. Other Programming</td>
        <td>Oldies, Easy Listening, Variety,<br/>Classical, Jazz, Big Band,<br/>Religious (non-Gospel)</td>
        <td>1,873</td>
        <td>911</td>
        <td>3.2%</td>
        <td>47.1</td>
        <td>6.8</td>
        <td>36.4</td>
        <td>9.9</td>
        <td>10.2</td>
        <td colspan="5"></td>
    </tr>
  </tbody>
</table>

<sup>a</sup>Demographic percentages based on Arbitron’s *Radio Today* publications 2003–2006 and author’s calculations.

<page_number>1769</page_number>

DYNAMIC PRODUCT POSITIONING

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1770 ANDREW SWEETING

tract older, male listeners, Easy Listening and Religious stations attract an equal mix of men and women, and Urban and Spanish formatted stations attract black and Hispanic audiences, respectively. Comparing across markets, these differences in demographic tastes clearly affect format-switching, which will provide a source of identification when estimating the model. For example, ten times as many stations are moved to Spanish in markets with an above median proportion of Hispanics than in markets with a below median proportion. The table also shows that music stations tend to have higher audience shares, consistent with many listeners preferring music, and that AM band stations are primarily concentrated in nonmusic formats, particularly News/Talk. This reflects the fact that AM stations provide poorer audio quality for music programming, and the propensity of owners to switch AM stations to News/Talk will also provide a source of identification when the model is estimated. For the counterfactual, I assume that stations in the first four formats would have to pay fees.

Forty stations go temporarily off-air before returning to service, so I also include a nonactive “Dark” format in the choice set. On the other hand, only one station permanently closes during the sample, so I do not include an option of permanent exit in the model, and treat the exit of this station as an unanticipated shock.<sup>19</sup> I also do not model de novo entry into the industry, although 55 stations (almost all of which remain very small throughout the data) begin operating when the FCC grants new licenses. The entry of these stations is also treated as an unanticipated shock, and they are included in the Dark format in the period immediately before they start operating. A more complete model of entry and exit could be included with additional computation. BIAfn provides the ownership history of each station, including the month of each transaction, and in each period I assign ownership to the firm that owns the station at the end of that period.<sup>20</sup>

Stations in the same market-format can have quite different market shares. I allow for these differences to be explained by several observable variables, specifically AM band-format interactions, the proportion of the market’s population covered by the station’s signal (interacted with the station’s band), an “out of market” dummy for whether the station is based outside the geographic boundaries of the local market (e.g., in the surrounding countryside), and a dummy for whether the station has an imputed share in one or more periods. With the exception of the band-format interactions, firms treat these observed

<sup>19</sup>More generally, exit in the radio industry usually occurs for noneconomic reasons such as the death or incarceration of the owner of a small station or the revocation of a license by the FCC due to breaches of FCC regulations.

<sup>20</sup>While I allow for firms to own multiple stations in the same market, I ignore the fact that there are many large radio companies that own stations in multiple markets. Allowing for cross-market economies of scope complicates the model, and cannot be incorporated perfectly into the counterfactual without modeling the large number of markets that are not incorporated in the sample. *Sweeting (2011)* did allow for cross-market economies of scope from operating stations in different markets in the same format, and estimated them to be small.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1771</page_number>

characteristics as permanent and fixed, which is a reasonable approximation, as less than 1% of stations change signal strength or tower height between 2002 and 2006. The desirability of being a music station, in the absence of fees, is indicated by the fact that stations with the best signals tend to select into music formats; for example, on average, the signals of AC/CHR, Country, and Rock stations reach 85% of their market population, whereas the signals of Spanish or Other Programming stations only reach 70%.<sup>21</sup>

## *2.1. Summary Statistics*

*Table II* contains summary statistics on the main market and station variables. On average, 3.2% of stations switch between active programming formats each period. This rate is quite similar across markets of different sizes (2.9% in markets with more than 1 million people, and 3.3% in the remaining markets, and the difference is not statistically significant) even though station revenues tend to be much higher in larger markets. In order to allow some of the parameters to vary across markets, I define three "market-size groups" based on the 12+ population: above 1 million (26 markets), 0.25 million–1 million (43), and less than 0.25 million (33). Switching stations have lower market shares than other stations, and the market share of just under 60% of switching stations increases in the year after they switch. Markets are heterogeneous in size, ethnic composition, and total advertising revenues. The average owner in a market (a firm in the model) operates 2.5 stations, with the number varying from 1 to 8. There is a slight tendency for firms to cluster their stations in the same format (when two local stations with the same owner are drawn at random, the probability that they are in the same format is 0.195, compared with 0.160 for stations with different owners). However, multi-station firms are observed moving stations to formats where they own fewer stations almost as frequently as they are observed moving stations to formats where they own more stations (111 vs. 115 moves in the data). This balance explains why I find only slight evidence of economies of scope from operating stations in the same format when I estimate the model.

# 3. MODEL

## *3.1. Overview and Notation*

Radio station owners (firms) *o* = 1, ..., *O<sub>m</sub>* in each market $m$ play an infinite horizon discrete time game with periods *t* = 1, ..., ∞. Markets are assumed to be completely independent of each other, and I ignore the effects of common ownership across markets. The exogenous characteristics of market $m$ ($X_{mt}$) are its population size, the proportion of its population in each of 18 age-gender-race/ethnicity groups, the growth rates of its white, black, and Hispanic populations, and the value of each listener to advertisers. Market de-

<sup>21</sup>News station signals reach 85% of their market populations, on average.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>

1772
</page_number>


ANDREW SWEETING


# TABLE II
# SUMMARY STATISTICS

<table>
  <thead>
    <tr>
        <th>Variable</th>
        <th>Observations<br/>(market-periods)</th>
        <th>Mean</th>
        <th>Std. Dev.</th>
        <th>Min.</th>
        <th>Max.</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="6">Market Characteristics</td>
    </tr>
    <tr>
        <td>Number of stations in market</td>
        <td>714</td>
        <td>23.2</td>
        <td>5.8</td>
        <td>11</td>
        <td>38</td>
    </tr>
    <tr>
        <td>Number of different station owners in market</td>
        <td>714</td>
        <td>9.4</td>
        <td>3.2</td>
        <td>3</td>
        <td>18</td>
    </tr>
    <tr>
        <td>Population (12 and above, in millions)</td>
        <td>714</td>
        <td>0.73</td>
        <td>0.75</td>
        <td>0.06</td>
        <td>3.62</td>
    </tr>
    <tr>
        <td>Proportion of population black</td>
        <td>714</td>
        <td>0.11</td>
        <td>0.12</td>
        <td>0.002</td>
        <td>0.340</td>
    </tr>
    <tr>
        <td>Proportion of population Hispanic</td>
        <td>714</td>
        <td>0.09</td>
        <td>0.10</td>
        <td>0.005</td>
        <td>0.583</td>
    </tr>
    <tr>
        <td>Combined listening to sample stations (% of total market<sup>a</sup>)</td>
        <td>714</td>
        <td>35.0</td>
        <td>3.3</td>
        <td>22.8</td>
        <td>45.9</td>
    </tr>
    <tr>
        <td>BIAfn estimated annual market advertising revenues (2002–2005, $ m.)</td>
        <td>408</td>
        <td>53.5</td>
        <td>66.0</td>
        <td>3.9</td>
        <td>403.7</td>
    </tr>
    <tr>
        <td> </td>
        <td>Observations<br/>(station-periods)</td>
        <td> </td>
        <td> </td>
        <td> </td>
        <td> </td>
    </tr>
    <tr>
        <td colspan="6">Station Characteristics (note: statistics exclude Dark stations)</td>
    </tr>
    <tr>
        <td>Market share</td>
        <td>16,481</td>
        <td>1.5%</td>
        <td>1.3%</td>
        <td>0.0%</td>
        <td>10.5%</td>
    </tr>
    <tr>
        <td>BIAfn Estimated advertising revenues (station-year 2002–2004, $ m.)</td>
        <td>6,413</td>
        <td>2.4</td>
        <td>3.7</td>
        <td>0.003</td>
        <td>45.6</td>
    </tr>
    <tr>
        <td>Dummy for station located outside market boundaries</td>
        <td>16,481</td>
        <td>0.06</td>
        <td>0.24</td>
        <td>0</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Dummy for AM band</td>
        <td>16,481</td>
        <td>0.30</td>
        <td>0.46</td>
        <td>0</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Proportion of market covered by signal (out of market stations excluded)<sup>b</sup></td>
        <td>15,464</td>
        <td>0.79</td>
        <td>0.36</td>
        <td>0.002</td>
        <td>1.1</td>
    </tr>
    <tr>
        <td>Dummy for station that has imputed market share in at least one quarter</td>
        <td>16,481</td>
        <td>0.14</td>
        <td>0.35</td>
        <td>0</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Dummy for station that switches formats before next quarter</td>
        <td>14,120</td>
        <td>0.032</td>
        <td>0.18</td>
        <td>0</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Market share of a switching station</td>
        <td>454</td>
        <td>1.0%</td>
        <td>0.9%</td>
        <td>0.0%</td>
        <td>6.3%</td>
    </tr>
  </tbody>
</table>

<sup>a</sup>The total market includes the outside good of not listening to radio and listening to noncommercial and commercial nonsample stations. Market definition allows for each individual to spend up to 6 hours listening to the radio between 6am and midnight.

<sup>b</sup>Signal coverage is defined relative to the market population and I cap it at 1.1 to address outliers that appeared to distort the demand estimates.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1773</page_number>

mographics change over time due to these growth rates, and the growth rates evolve exogenously from period to period according to an AR(1) process, with normally distributed innovations.<sup>22</sup> For the rest of the presentation, I suppress the market index, except where absolutely necessary. A firm $o$ owns a set of stations $S^o$. The set of players and stations is assumed to remain the same over time, so that, in the model, firms do not expect new entry, permanent exit, or changes in ownership. Each station’s quality, which affects its audience, consists of three components. The first component depends on observed characteristics, such as signal coverage and whether the station is located inside the market, and has a common effect across formats and is assumed to be fixed over time. The second component reflects a format-specific quality effect for AM stations, as this band provides relatively poor audio quality in music formats. The first two components are denoted $X_{st}\gamma^S$. The final component is a one-dimensional level of time-varying quality $\xi_{st}$ that is assumed to be independent of observed station characteristics, and which evolves according to an AR(1) process that is not controlled by the firm, except that $\xi_{st}$ may change discretely when $s$ changes formats. There are $F = 0, 1, \dots, 7$ discrete formats, where format 0 is the Dark (temporarily off-air) format, and each station is in exactly one format each period. $F_{st}$ is a vector that indicates the format of station $s$ in period $t$.

Each period, local firms generate revenues by selling their stations’ audiences to advertisers. Station audiences are determined by the demographic make-up of the market, as demographics affect programming tastes, station quality characteristics, and station programming formats. Revenues are then determined by the market price for listeners and the relative price of listeners in each demographic group. Demographic tastes for programming, market prices, and relative prices are assumed to be fixed over time. Station formats and characteristics, market prices, and demographics are all publicly observed at time $t$, and in describing the model I use $\mathcal{M}_{j,o,t}$ to denote the collection of all of these publicly observed variables in a firm($o$)-specific state $j$ in period $t$. For ease of reference, Appendix A of the Supplemental Material contains a complete list of the state variables.

In the game, owners choose the formats of their stations for the next period. $A_o(\mathcal{M}_{j,o,t})$ denotes the discrete set of possible actions (next period format configurations) available to $o$. The choice set is state-dependent because I assume that each firm can move at most one station per period, which limits the choice set of multi-station firms.<sup>23</sup> Each possible action is associated with a private information, independent and identically distributed (i.i.d.) payoff shock $\varepsilon_{ot}(a)$

<sup>22</sup> This means that the six white (black/Hispanic) population subgroups grow at the rate of the white (black/Hispanic) population, which implies that the relative age and gender balance of the six subgroups remains the same over time. This pattern holds approximately in the data.

<sup>23</sup>While 99.5% of firm-period observations satisfy this constraint, there are 28 observations where firms move two stations and one observation where a firm moves three stations. These observations are ignored when calculating the pseudo-likelihood as part of the estimation pro-

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1774</page_number> ANDREW SWEETING

which is received when action $a$ is chosen. These shocks will be distributed Type 1 extreme value, scaled by a parameter $\theta^{\varepsilon}$ that can vary with market size.

## 3.2. Timing

Within each period $t$, the timing of the game is as follows:

1. each firm $o$ observes the current state, $\mathcal{M}_{j,t}$;

2. each firm $o$ pays fixed costs for each of its active stations. The cost of operating a station is reduced by $\theta^C$ when a station operates another station in the same format, creating a total cost saving of $C(\mathcal{M}_{j,o,t})\theta^C$, where $C(\mathcal{M}_{j,o,t})$ is simply a count of how many stations it operates in formats where it has multiple stations. Given my specification of repositioning costs, only $\theta^C$, and not the level of fixed costs, is identified, so I will proceed treating fixed costs as equal to zero;

3. each firm $o$ observes the private information shocks $\varepsilon_{ot}$ to its format choices, and makes its format choice $a_{ot}$;

4. each firm receives advertising revenues $\sum_{s \in S^o} R_s(\mathcal{M}_{j,t} | \gamma)$, where $\gamma$ are the parameters of the demand and revenue models, pays repositioning costs $W_o(a_{ot})\theta^W$, and receives the payoff shock $\varepsilon_{ot}(a_{ot})$;

5. $\mathcal{M}_{j,o,t}$ evolves to the state in the next period, reflecting firms' format choices, and the stochastic evolution of station qualities and the growth rates of the white, black, and Hispanic populations.

For the purposes of solving and estimating the model, it is useful to define the firm's flow profit function $\pi_{ot}$ as including payoffs accruing from point 3 in the current period to point 2 in the next period, as next period's fixed cost savings will depend deterministically on the action chosen in the current period:

$$
\begin{aligned}
(1) \quad & \pi_{ot}(a_{ot}, \mathcal{M}_{j,t}, \theta, \gamma) + \theta^{\varepsilon} \varepsilon_{ot}(a_{ot}) \\
& = \underbrace{\sum_{s \in S^o} R_s(\mathcal{M}_{j,t}, \gamma)}_{\text{period } t \text{ advertising revenues}} + \underbrace{\beta C_o(a_{ot})\theta^C}_{\text{fixed cost savings}} - \underbrace{W_o(a_{ot})\theta^W}_{\text{repositioning costs}} + \underbrace{\theta^{\varepsilon} \varepsilon_{ot}(a_{ot})}_{\text{payoff shock for } a_{ot}},
\end{aligned}
$$

where the $\theta$ parameters will be estimated using the dynamic model. $\beta = 0.95$, implying an annual discount factor of just above 0.9. This is lower than factors

cess, and it is assumed that all other local firms optimize assuming that other firms can only move one station each period. Relaxing this restriction in a dynamic model with eight formats would be burdensome, but I have investigated how allowing each firm to make two moves, rather than one move, affects the results in a two-period version of the model where firms only care about their revenues in the following period. This results in positive, but still fairly small, estimates of economies of scope, which is sensible as 9 out of the 28 two-move observations involve a firm moving two stations to the same format at the same time, which would be unlikely without scope economies. Jeziorski (2013), who was focused on the estimation of these economies, allowed for more moves by assuming that owners take format-switching decisions for different stations sequentially.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1775</page_number>

that are often used in dynamic models, but it is higher than the ones typically used to value radio stations (Albarran and Patrick (2005)).

## *3.3. Components of the Per-Period Payoff Function*

I now describe each component of the payoff function.

### 3.3.1. *Station Revenues* ($R_s(\mathcal{M}_{j,o,t}|\gamma)$)

A station’s revenues are the sum of the number of listeners that a station has in each demographic group, determined by a listener demand model, multiplied by the market price of each of these listeners, determined by a revenue function.

***Listener Demand.*** A station’s audience in each of 18 demographic groups is determined by a static, discrete-choice random coefficients logit model as a function of the state variables in its own market. Each consumer $i$ in this market chooses at most one station to listen to, and $i$’s utility if she listens to non-Dark station $s$ is

$$ (2) \quad u_{ist} = \gamma_i^R + X_{st}\gamma^S + F_{st}(\overline{\gamma^F} + \gamma_D^F D_i) + \xi_{st} + \varepsilon_{ist}^L $$

$$ (3) \quad = \delta_{st}(F_{st}, X_{st}, \gamma^S, \overline{\gamma^F}, \xi_{st}) + \gamma_i^R + F_{st}\gamma_D^F D_i + \varepsilon_{ist}^L, $$

where $\delta_{st}$ is the “mean utility” of the station for a consumer with baseline demographics (white, male, aged 12–24) and $\varepsilon_{ist}^L$ is an i.i.d. logit shock to individual preferences. $X_{st}$ and $\xi_{st}$ (unobserved quality, which can be inferred from the estimated demand model) are assumed to be valued by all consumers in the same way. $\overline{\gamma^F}$ are the format preferences of baseline demographic consumers, while $\gamma_D^F D_i$ allows format preferences to vary with age, gender, and race. $\gamma_i^R$, assumed to be distributed normal with mean zero and variance $\gamma^\sigma$, allows for heterogeneity in how much consumers value listening to the radio. Choice of the outside good, defined as not listening to one of the commercial stations included in the model, is assumed to give utility of $\varepsilon_{i0t}^L$.

This is a rich specification, but it makes two significant simplifications. First, consumers are assumed to choose at most one station, whereas, in reality, people listen to several stations for different lengths of time during a period (ratings quarter). This is a standard simplification when using aggregate data (e.g., Nevo (2001)).<sup>24</sup> Second, the model is entirely static, whereas listening

<sup>24</sup>It can be rationalized as a representation of consumers’ preferences during shorter time periods, which are aggregated to give period market shares. This representation, which assumes i.i.d. draws in preferences across the shorter time periods, is adequate as long as stations and advertisers are indifferent between audiences of the same size made up of either a few people who listen a lot, or a lot of people who listen a little.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1776 [space] ANDREW SWEETING

habits might make shares adjust slowly to changes in formats. This simplification seems reasonable, as I consider only major programming changes, which should be obvious to all consumers, and six-month time periods, which are likely to be longer than the time required for listeners to adjust.<sup>25</sup>

*Revenues per Listener.* The advertising revenue that a station *s* receives for a listener with demographics $D_d$ is determined by a parametric function

$$ (4) \quad r_{st}(D_d) = \gamma_m(1 + Y_{st}\gamma^Y)(1 + D_d\gamma_d), $$

where $\gamma_m$ is a market-year fixed effect and $\gamma_d$ allows advertiser valuations to vary with listener demographics.<sup>26</sup> In order to allow for format switching and market structure to affect revenues, the variables in $Y$ include the number of other stations that the firm has in the format, the number of other stations in the format, and a dummy variable for whether the station switched formats in the previous period. Total station revenues $R_{st}(\mathcal{M}_{j,o,t}|\gamma)$ are the sum, over the 18 demographic groups, of the number of listeners multiplied by these prices. Total station revenues (at the annual level), aggregate share data, and the predictions of the listener demand model about how many listeners the station will have in each demographic group are used to estimate these parameters, as station-level prices per listener by demographic are not available.

The listener utility and revenue specifications (2) and (4) do not contain time effects, even though broadcast radio audiences have been falling slowly since the late 1980s and revenues per listener were rising during the sample period. When I estimate these models, I do include time effects to avoid biasing other coefficients, but, when estimating the dynamic model, I assume that all firms expect the current values of these time effects to remain fixed into the future, as methods for solving and estimating dynamic games assume stationarity. This is an ad hoc simplification, but it is consistent with the fact that real revenues, which are what firms ultimately care about, changed very little during the sample, as the trends in listenership and revenues per listener approximately canceled out.<sup>27,28</sup>

<sup>25</sup>The example in footnote 11 suggests that stations that make major programming changes may take actions to encourage their old listeners to move to other stations, so as to avoid a protracted period of complaints.

<sup>26</sup>The coefficients of this reduced form function are assumed to stay the same when performance fees are introduced.

<sup>27</sup>The Radio Advertising Bureau estimates annual industry revenues from 2002 to 2006 of $19.4 bn., $19.6 bn., $20.0 bn., and $20.1 bn. (personal correspondence, November 29, 2010).

<sup>28</sup>Given my counterfactual, it is important that these trends are common across formats. This appears to be approximately the case in the data. For example, based on Arbitron’s *Radio Today* reports, time spent listening between 2002 and 2005 fell by 0.45% per period for the population as a whole (implying a drop in listening of 6.5 minutes per day over the entire sample period), 0.47% per period for blacks, and 0.35% for Hispanics who were being served by more Spanish language stations over this period. When I regress station revenues per listener on format dummies, market

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING

<page_number>1777</page_number>

## *3.3.2. Repositioning Costs ($W_o(a_{ot})\theta^W$)*

A firm pays a repositioning cost when it moves a station to a new format. I allow repositioning costs to vary across the market size groups, with the revenues of the station being moved and whether the station was moved in the previous period, and to depend on whether the switch is between active formats or to or from Dark.

## *3.3.3. Fixed Costs and Economies of Scope ($\beta C_o(a_{ot})\theta^C$)*

Because I estimate the value of repositioning costs to and from the Dark format, the common fixed cost incurred by all active stations is not identified, but any reduction in fixed costs from operating multiple stations in the same format is identified from the format choices of multi-station firms who must trade off this efficiency against audience cannibalization. As a simple specification for these economies of scope, I assume that the fixed costs of any station operating in the same format as one of its sister stations are reduced by $\theta^C$, which can vary across market size groups.

## *3.3.4. Payoff Shocks ($\theta^\varepsilon \varepsilon_{ot}(a_{ot})$)*

Firms receive i.i.d. (across firms and over time) private information shocks to their payoffs from each possible format choice, including keeping stations in the same format. These shocks are drawn from a Type 1 extreme value distribution, scaled by a parameter $\theta^\varepsilon$, which can vary across the market-size groups. These scale parameters are identified because revenues are treated as observed when estimating the dynamic model. These shocks should capture all factors that affect a firm’s format choice but are not captured in the expected revenue or cost functions assumed by the model. For example, the owner of the station that switched to Sports programming discussed in footnote 11 had an existing business that sold advertising for local sports facilities, leading to a possible synergy that is not captured by the model. In the same example, the firm tried to keep its plan to move to Sports programming secret until the move was made, which helps to rationalize the private information assumption.<sup>29</sup> The strong assumption is that the $\varepsilon$’s are serially uncorrelated, and this is required for tractability.

dummies, year dummies, and year-format interactions, the coefficients on the year-format interactions are jointly insignificant (*p-value 0.3142*), which also suggests that revenues per listener were changing in a similar way across formats over time.

<sup>29</sup>A standard objection to the private information assumption in static models (e.g., Seim (2006)) is that it can lead to firms experiencing ex post regret, because, for example, more firms choose the same location than was expected. However, in my model, the rate of switching is relatively low and in the data it is relatively rare for two firms in the same market to make switches that would have a large impact on the expected profitability of each other’s switch. In a dynamic model, firms are also able to quickly reverse choices that turn out to be particularly sub-optimal.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1778 ANDREW SWEETING

## *3.4. Evolution of the State Variables*

At the end of period $t$, the state variables evolve for the following period. Station formats change deterministically with firms’ choices. Unobserved station quality is assumed to evolve according to an AR(1) process with normally distributed innovations that are i.i.d. across stations:

$$ (5) \quad \xi_{st} = \rho^{\xi} \xi_{st-1} + v_{st}^{\xi} \quad \text{if } F_{st} = F_{st+1}, $$

$$ (6) \quad = \rho^{\xi} \xi_{st-1} + v_{st}^{\xi} + \gamma^{\xi} \quad \text{if } F_{st} \neq F_{st+1}, $$

where $v_{st}^{\xi} \sim N(0, \sigma_{v_{\xi}}^2)$.<sup>30</sup> The $\gamma^{\xi}$ term allows for a fixed shift in quality when the firm changes format. I assume that firms do not know the innovations $v_{st+1}^{\xi}$ when they make format choices in period $t$, which allows me to form moments for consistently estimating demand based on these innovations which should be valid even if the levels of $\xi$ affect format choices. By assumption, this rules out some forms of selection that might drive format repositioning. However, I provide evidence that my modeling assumptions are consistent with the data by showing that I can closely match the empirical distribution of share changes for stations that switch formats, even when I estimate the parameters $\rho^{\xi}$ and $\sigma_{v_{\xi}}^2$ using only stations that do not switch.

While listener demand depends on 18 mutually exclusive age-gender-ethnic/racial groups, it would be cumbersome to model the evolution of the population in so many groups. Instead, I model the growth rate for each ethnic/racial group (white, black, and Hispanic) and assume that the same growth rate applies to each of the associated age-gender groups. I assume that, for ethnic group $e$,

$$ (7) \quad \log \left( \frac{pop_{met}}{pop_{met-1}} \right) = \tau_0 + \tau_1 \log \left( \frac{pop_{met-1}}{pop_{met-2}} \right) + u_{met}, $$

which allows population growth for particular groups to have the type of serial correlation that is observed in the data.<sup>31</sup> This particular specification also allows me to address the problem that population estimates are annual (see Appendix C of the Supplemental Material for a detailed discussion).

<sup>30</sup>It is not necessary to assume that the innovations are normal, and the process is estimated without imposing normality. However, a normal probability plot indicates that the implied distribution of innovations matches a normal distribution very well except at percentiles below 5% and above 95%, and drawing from the empirical distribution of innovations gives very similar results.

<sup>31</sup>Alho and Spencer (*2005*, Chapter 7) discussed the application of time series models, including AR(1) to demographic growth rates. Models with additional lag terms would complicate the state space of the dynamic model.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1779</page_number>

## *3.5. Value Functions and Equilibrium Concept*

As in almost all of the literature following Ericson and Pakes (1995), I assume that firms use stationary Markov Perfect Nash Equilibrium (MPNE) pure strategies.<sup>32</sup> A stationary Markov Perfect strategy for a firm $o$, $\Gamma_o$, is a mapping from any state $(\mathcal{M}_{j,o,t}, \varepsilon_{ot})$ to an action $a_{ot}$ that does not depend on $t$. I use $\Gamma$ to denote the strategies of all firms. $V_o^\Gamma(\mathcal{M}_{j,o,t}, \varepsilon_{ot})$ defines a firm’s value in a particular state when it uses an optimal strategy and other firms use strategies defined in $\Gamma$. By Bellman’s optimality principle,

$$
\begin{aligned}
(8) \quad V_o^\Gamma & (\mathcal{M}_{j,o,t}, \varepsilon_{ot}) \\
& = \max_{a \in A_o(\mathcal{M}_{j,o,t})} \left[ \pi(a, \mathcal{M}_{j,o,t}) + \theta^\varepsilon \varepsilon_{ot}(a) \right. \\
& \quad \left. + \beta \int V_o^\Gamma(\mathcal{M}_{h,o,t+1}) g(\mathcal{M}_{h,o,t+1} | a, \Gamma_{-o}, \mathcal{M}_{j,o,t}) \, d\mathcal{M}_{h,o,t+1} \right],
\end{aligned}
$$

where $g(\cdot)$ is the transition density when $o$ chooses $a$ and other firms use strategies $\Gamma_{-o}$. Given the distribution of the payoff shocks, an optimal strategy for firm $o$ will map into conditional choice probabilities

$$
(9) \quad P_o^\Gamma(a, \mathcal{M}_{o,t}, \Gamma_{-o}) = \frac{\exp \left( \frac{v_o^\Gamma(a, \mathcal{M}_{j,o,t}, \Gamma_{-o})}{\theta^\varepsilon} \right)}{\sum_{a' \in A_o(\mathcal{M}_{j,o,t})} \exp \left( \frac{v_o^\Gamma(a', \mathcal{M}_{j,o,t}, \Gamma_{-o})}{\theta^\varepsilon} \right)},
$$

where $v_o^\Gamma(a, \mathcal{M}_{j,o,t}, \Gamma_{-o})$ is a choice-specific value function which excludes the current payoff shock

$$
\begin{aligned}
(10) \quad v_o^\Gamma & (a, \mathcal{M}_{j,o,t}, \Gamma_{-o}) \\
& = \pi(a, \mathcal{M}_{j,o,t}) \\
& \quad + \beta \int V_o^\Gamma(\mathcal{M}_{h,o,t+1}) g(\mathcal{M}_{h,o,t+1} | a, \Gamma_{-o}, \mathcal{M}_{j,o,t}) \, d\mathcal{M}_{h,o,t+1},
\end{aligned}
$$

and $V_o^\Gamma(\mathcal{M}_{h,o,t+1})$ is the firm’s value in state $\mathcal{M}_{h,o,t+1}$ before that period’s payoff shocks are realized.

<sup>32</sup>With continuous states, it is an assumption that a pure strategy MPNE exists. Doraszelski and Satterthwaite (2010) proved the existence of a pure strategy MPNE for a model with discrete states when the random component of payoffs has unbounded support. Conceptually, it would be possible to convert my model into one where existence was guaranteed by using an arbitrarily fine discretization of the continuous states. A similar argument was made by Jenkins, Liu, Matzkin, and McFadden (2004).

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1780</page_number> ANDREW SWEETING

# 4. METHODS USED TO SOLVE AND ESTIMATE THE MODEL

In this section, I outline the methods used to solve and estimate the model, focusing on how I incorporate parametric approximations to the value function into existing procedures that have been applied to dynamic models with smaller state spaces. Complete details are given in Appendices B and C of the Supplemental Material. I begin by discussing how the model is solved, as estimation of the dynamic model involves only small extensions to the solution procedure.

## *4.1. Solution Method*

For exposition, it is useful to define a firm’s expected flow profits (prior to the realization of the $\varepsilon$’s) in a particular state when it uses optimal choice probabilities $P_o$ as

$$
\begin{aligned}
(11) \quad \tilde{\pi}(P_o(\mathcal{M}_{j,o,t})) \\
&= \sum_{s \in S^o} R_s(\mathcal{M}_{j,o,t} | \gamma) + \sum_{a \in A_0(\mathcal{M}_{j,o,t})} P_o(a | \mathcal{M}_{j,o,t}) (B C_o(a) \theta^C \\
&\quad - W_o(a) \theta^W + \theta^\varepsilon (\varkappa - \log(P_o(a | \mathcal{M}_{j,o,t})))),
\end{aligned}
$$

where $\varkappa$ is Euler’s constant. If we were considering a finite set of states, then we could express value functions given a set of choice probabilities $P$ (for all firms in all states) as a set of equations

$$
(12) \quad V^P = [I - \beta E_P]^{-1} \tilde{\pi}(P),
$$

where $E_P$ is the Markov operator corresponding to policies $P$, so that the $(i, j)$th element is the probability of moving from state $i$ to state $j$ given strategies/choice probabilities $P$.

A standard way of trying to solve a dynamic model is to use policy iteration (Judd (1998), Rust (2000)), which involves repeatedly iterating two steps. At iteration $i$, in the first step (*policy valuation*), (11) and (12) are applied to calculate values $V^{P^i}$ associated with choice probabilities $P^i$ that may not be optimal. In the second step (*policy improvement*), these values $V^{P^i}$ are used to update $P^i$ by computing choice-specific value functions

$$
\begin{aligned}
(13) \quad v_o^{P^i}(a, \mathcal{M}_{j,o,t}) \\
&= \pi(a, \mathcal{M}_{j,o,t}) \\
&\quad + \beta \int V_o^{P^i}(\mathcal{M}_{h,o,t+1}) g(\mathcal{M}_{h,o,t+1} | P_{-o}^i, \mathcal{M}_{j,o,t}, a) d\mathcal{M}_{h,o,t+1},
\end{aligned}
$$

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1781</page_number>

where $g(\cdot)$ is now the transition density as a function of choice probabilities, and applying formula (9). Iteration continues until both values and choice probabilities converge, up to a prespecified numerical tolerance.

As the state space is exceptionally large and some state variables are continuous, it is impossible to solve for values at all states. Instead, I choose a specific set of $N$ states, and assume that the value function can be approximated by a parametric linear function of $K$ functions ($\phi$) of the state variables, that is,

$$ (14) \quad V_o^{P^*} (\mathcal{M}_{j,o,t}) \simeq \sum_{k=1}^K \lambda_k \phi_{ko} (\mathcal{M}_{j,o,t}). $$

Solving the value function now requires finding $K$ $\lambda$ coefficients rather than $N$ values. Stacking the equations for each of the $N$ selected states in matrix form, the following equations should hold for equilibrium strategies $P^*$:

$$ (15) \quad \Phi \lambda = \widetilde{\pi}(P^*) + \beta E_{P^*} \Phi \lambda, $$

where $\Phi$ is the matrix of the functions of the state variables and $E_{P^*} \Phi$ is a matrix with element $(j, k)$:

$$ (16) \quad E_{P^*} \Phi_{j,k} = \int \phi_{ko} (\mathcal{M}_{h,o,t+1}) g(\mathcal{M}_{h,o,t+1} | P^*, \mathcal{M}_{j,o,t}) \, d\mathcal{M}_{h,o,t+1}, $$

where row $j$ is associated with state $\mathcal{M}_{j,o,t}$. The choice of states $\mathcal{M}_{h,o,t+1}$ to approximate the integral is described in Appendix B of the Supplemental Material.

For the overidentified case ($N > K$), $\widehat{\lambda}^P$ can be found using an OLS estimator

$$ (17) \quad \widehat{\lambda}^P = ((\Phi - \beta E_P \Phi)' (\Phi - \beta E_P \Phi))^{-1} (\Phi - \beta E_P \Phi)' \widetilde{\pi}(P). $$

The parametric policy iteration procedure (Benitez-Silva et al. (2000)) now consists of iterating several steps. Before the procedure begins, I calculate $\Phi$ for the $N$ selected states, which include the observed states and duplicates of these states where the features that can vary over time are perturbed. The variables in the approximation include measures of revenues, firm and rival characteristics, and opportunities to increase revenues by switching formats. For the counterfactual, the model is solved market-by-market, so no restrictions that the approximating function has to be the same across markets are imposed. For an iteration $i$, the following steps are used:

1. calculate $\widetilde{\pi}(P^i)$ and $E_{P^i} \Phi$;

2. create matrices $(\Phi - \beta E_{P^i} \Phi)$ and use (17) to calculate $\widehat{\lambda}^{P^i}$;

3. use $\widehat{\lambda}^{P^i}$ to calculate the choice-specific value functions for each choice for each firm and the multinomial logit formula (9) to calculate updated probabilities $P'$;

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1782</page_number>

ANDREW SWEETING

4. if the maximum absolute difference between $P'$ and $P^i$ is sufficiently small (I use a tolerance of $1e-6$), the procedure stops and $\widehat{\lambda}^{P^i}$ is saved as $\lambda^*$; otherwise, $P^{i+1} = \psi P' + (1 - \psi)P^i$, and iteration $i + 1$ begins at step 1. I use $\psi = 0.1$.

This procedure solves for the conditional choice probabilities in the observed states (and the chosen duplicates). However, to perform the counterfactual, I need to simulate the model forward to states that will not have been included in this selection. Therefore, in each future period, $\lambda^*$ is used to solve for equilibrium choice probabilities. Full details of this procedure are in Appendix B of the Supplemental Material.

*4.2. Estimation*

Estimation of the model proceeds in two main stages. The first stage involves estimation of (i) the listener demand model and the process for the unobserved ($\xi$) component of station quality; (ii) the revenue function; (iii) an initial guess of firms' conditional choice probabilities; and (iv) the transition process governing demographics. The methods used are based on existing literature, and complete details are in Appendix C of the Supplemental Material. The main innovation is that I formulate demand moments that are based on *innovations* in unobserved quality. Under the timing assumption that firms have no knowledge of these innovations when they make format choices, this allows for consistent estimation of demand even when format choices are affected by the level of quality. The second stage involves estimation of repositioning costs and economies of scope using the dynamic model. As the particular method I use is new, I discuss the procedure in the text, but Appendix C provides additional detail. Appendix D describes the implementation of methods based on forward simulation that are used as robustness checks.

*4.2.1. Estimation of the Dynamic Model*

My preferred estimates come from an estimator that combines parametric approximation of the value function with a pseudo-likelihood procedure that follows AM (2010) (discussed in Aguirregabiria and Nevo (2012)). This procedure follows a similar iterative procedure to the one used to solve the model, with an added estimation step, but the choice probabilities of other firms ($P_{-o}$) are held constant at initial first-stage estimates.<sup>33</sup>

<sup>33</sup>I describe an iterative procedure to solving the dynamic model where the approximation to the value function is calculated each time the structural parameters $\theta$ are updated. This procedure works for the parametric functions that I consider. An alternative approach would be to estimate both the structural parameters and the parameters of the parametric approximation as part of a single maximization problem with a very large set of equality constraints describing the equilibrium conditions of the model (MPEC). Barwick and Pathak (2012) implemented this type of approach in a dynamic model with value function approximation. Egesdal, Lai, and Su (2012) and Su (2012) also implemented this approach to the estimation of games without value function approximation, and discussed its advantages.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

# DYNAMIC PRODUCT POSITIONING

<page_number>1783</page_number>

Before the procedure begins, I calculate $\Phi$ for the $N$ selected states, which include the observed states and nine duplicates of these states ($N = 60,610$) where the features that can vary over time are perturbed. The types of variables used in the approximation when solving the model are also used here, but, because it is necessary to pool markets together for estimation, the approximation also includes market-quarter fixed effects, and interactions between the other approximating variables and market characteristics (such as demographics).

In the iterated procedure, the following steps are followed in iteration $i$, where the current guess of the structural parameters is $\theta^i$ and the current guess of $o$’s choice probabilities is $P_o^i$ (see Appendix C of the Supplemental Material for full details):

1. calculate $\tilde{\pi}(P_o^i, \theta^i)$ and $E_{P^i} \Phi$;

2. create matrices $(\Phi - \beta E_{P^i} \Phi)$ and use (17) to calculate $\widehat{\lambda^i(\theta^i, P_o^i)}$;

3. use $\widehat{\lambda^i(\theta^i, P_o^i)}$ to approximate the future value from making each choice $a$:

$$
\begin{aligned}
(18) \quad & \text{FV}(a, M_{j,o,t}, P_o^i, P_{-o}) \\
& = \sum_{k=1}^K \left\{ \left( \int \phi_{ko}(M_{h,o,t+1}) g(M_{h,o,t+1} | P_o^i, P_{-o}, M_{j,o,t}, a) \, dM_{h,o,t+1} \right) \right. \\
& \quad \times \left. \widehat{\lambda_k^i(\theta^i, P_o^i)} \right\}
\end{aligned}
$$

(this is not quite the same as the choice-specific value function defined before, which also included current revenue and repositioning costs associated with action $a$);

4. estimate the structural parameters $\theta^i$ using a pseudo-likelihood estimator where the probability that an action $a$ is chosen is

$$
(19) \quad \frac{\exp \left( \frac{\text{FV}(a, M_{j,o,t}, P_o^i, P_{-o}) - W_o(a)\theta^{W'} - \beta C_o(a)\theta^{C'}}{\theta^{\varepsilon'}} \right)}{\sum_{a' \in A_{ot}} \exp \left( \frac{\text{FV}(a', M_{j,o,t}, P_o^i, P_{-o}) - W_o(a')\theta^{W'} - \beta C_o(a')\theta^{C'}}{\theta^{\varepsilon'}} \right)},
$$

where current revenues drop out because they are common across choices. I also report results based on using a moment-based estimator in this step, where a set of switching rates, which should be informative about the parameters, are matched (e.g., the overall switching rate between active formats, and the rate at which stations move to Spanish in markets with low and high Hispanic populations). POB argued that estimators based on this type of objective function may suffer from less bias in finite samples because of averaging across states. Observations for firms moving more than one station are excluded when calculating the objective function;

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1784 ANDREW SWEETING

<page_number>

1784
</page_number>

5. if the maximum absolute difference between $\theta'$ and $\theta^i$ and between $P'_o$ (based on (19)) and $P^i_o$ is less than $1\text{e}-4$, the procedure stops. Otherwise, the algorithm returns to step 1 using $P^{i+1}_o = \psi P'_o + (1 - \psi)P^i_o$ and $\theta^{i+1} = \psi \theta' + (1 - \psi)\theta^i$ where $\psi = 0.1$.

Standard errors are calculated using a bootstrap where markets are resampled (20 replications).

I also report results using the AM (2007) NPL procedure where $P_{-o}$ is also updated during estimation. This could be advantageous if the initial estimates of choice probabilities are inaccurate, but there is no guarantee that this type of procedure will cause the probabilities to converge to their true equilibrium values unless the initial estimates were already good, in which case the potential advantage should be limited, and the equilibrium satisfies a particular kind of stability condition (Aguirregabiria and Nevo (2012)). Pesendorfer and Schmidt-Dengler (2008, 2010) illustrated the problems that can result when NPL updating is applied to models with multiple equilibria, and multiple equilibria may certainly exist in my model. Therefore, I view the NPL estimates primarily as a robustness check.

# 5. EMPIRICAL RESULTS

This section presents the coefficient estimates, including robustness checks on the estimation of the dynamic parameters.

## 5.1. Listener Demand Model

Tables III and IV report the estimated parameters of the listener demand model. The very large value of $\gamma^\sigma$ implies that there is relatively little substitution between listening to commercial radio stations and the outside good.<sup>34</sup> The demographic-format coefficients show that there is a lot of demographic heterogeneity in programming tastes. The precision of these estimates reflects the use of demographic-format-specific moments, following Petrin (2002). Older listeners value all radio programming more than other listeners, but they particularly like News, Country, and Other Programming, while blacks and Hispanics prefer Urban and Spanish programming, respectively. The important implication of these coefficients for the counterfactual is that, when a music station switches to nonmusic programming, it will lose most of its younger, or black, listeners to the remaining music stations.

As expected, the station characteristic coefficients indicate that the AM band stations are better suited to News/Talk than other formats, that greater signal coverage increases quality, and that out-of-market stations, which likely only

<sup>34</sup>The coefficients on the time dummies, which are not reported, indicate that the utility from radio listening was decreasing slowly over time.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING

# TABLE III
# ESTIMATES OF FORMAT TASTE PARAMETERS<sup>a</sup>

<table>
  <thead>
    <tr>
        <th> </th>
        <th>Mean Tastes</th>
        <th>γ<sup>σ</sup></th>
        <th>Age 25–49</th>
        <th>Age 50 plus</th>
        <th>Female</th>
        <th>Black</th>
        <th>Hispanic</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Radio Listening</td>
        <td>−15.8753</td>
        <td>16.7749</td>
        <td>2.1404</td>
        <td>7.7839</td>
        <td>2.8197</td>
        <td>3.6171</td>
        <td>2.8563</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.9224)</td>
        <td>(1.3946)</td>
        <td>(0.2046)</td>
        <td>(0.7247)</td>
        <td>(0.2098)</td>
        <td>(0.3303)</td>
        <td>(0.4419)</td>
    </tr>
    <tr>
        <td colspan="8">Format Interactions (AC/CHR excluded)</td>
    </tr>
    <tr>
        <td>Rock</td>
        <td>0.6575</td>
        <td>–</td>
        <td>0.2813</td>
        <td>−0.4054</td>
        <td>−1.2581</td>
        <td>−1.8814</td>
        <td>−0.8157</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0580)</td>
        <td> </td>
        <td>(0.0087)</td>
        <td>(0.0158)</td>
        <td>(0.0043)</td>
        <td>(0.0292)</td>
        <td>(0.0402)</td>
    </tr>
    <tr>
        <td>Country</td>
        <td>−0.1187</td>
        <td>–</td>
        <td>0.4847</td>
        <td>1.3039</td>
        <td>−0.4428</td>
        <td>−1.9726</td>
        <td>−1.2733</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0646)</td>
        <td> </td>
        <td>(0.0095)</td>
        <td>(0.0175)</td>
        <td>(0.0046)</td>
        <td>(0.0330)</td>
        <td>(0.0524)</td>
    </tr>
    <tr>
        <td>Urban</td>
        <td>−1.2040</td>
        <td>–</td>
        <td>−0.7373</td>
        <td>−1.0188</td>
        <td>−0.3996</td>
        <td>3.9375</td>
        <td>0.6158</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0832)</td>
        <td> </td>
        <td>(0.0393)</td>
        <td>(0.0497)</td>
        <td>(0.0086)</td>
        <td>(0.0406)</td>
        <td>(0.0501)</td>
    </tr>
    <tr>
        <td>News/Talk</td>
        <td>−1.2918</td>
        <td>–</td>
        <td>1.6979</td>
        <td>3.1485</td>
        <td>−1.1171</td>
        <td>−0.7935</td>
        <td>−1.1071</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1443)</td>
        <td> </td>
        <td>(0.0080)</td>
        <td>(0.0144)</td>
        <td>(0.0049)</td>
        <td>(0.0275)</td>
        <td>(0.0385)</td>
    </tr>
    <tr>
        <td>Other Programming</td>
        <td>−0.9883</td>
        <td>–</td>
        <td>1.0958</td>
        <td>2.4600</td>
        <td>−0.5384</td>
        <td>−0.4204</td>
        <td>−0.2916</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0650)</td>
        <td> </td>
        <td>(0.0079)</td>
        <td>(0.0150)</td>
        <td>(0.0049)</td>
        <td>(0.0275)</td>
        <td>(0.0385)</td>
    </tr>
    <tr>
        <td>Spanish</td>
        <td>−2.7945</td>
        <td>–</td>
        <td>1.0300</td>
        <td>1.1111</td>
        <td>−0.3649</td>
        <td>−0.5138</td>
        <td>3.9489</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1955)</td>
        <td> </td>
        <td>(0.0264)</td>
        <td>(0.0506)</td>
        <td>(0.0163)</td>
        <td>(0.1519)</td>
        <td>(0.1694)</td>
    </tr>
  </tbody>
</table>

<sup>a</sup>16,481 observations, GMM objective function 2.90e-12, standard errors in parentheses. Mean tastes will reflect valuations of a white male aged 12–24.

<page_number>1785</page_number>

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1786</page_number> ANDREW SWEETING

# TABLE IV
# ESTIMATES OF STATION QUALITY PARAMETERS<sup>a</sup>

<table>
  <tbody>
    <tr>
        <td>AM * AC/CHR or Rock</td>
        <td>-0.7781</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.3174)</td>
    </tr>
    <tr>
        <td>AM * Country</td>
        <td>-0.4538</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1776)</td>
    </tr>
    <tr>
        <td>AM * Urban</td>
        <td>-0.3523</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1296)</td>
    </tr>
    <tr>
        <td>AM * News/Talk</td>
        <td>-0.0806</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1658)</td>
    </tr>
    <tr>
        <td>AM * Other</td>
        <td>-0.3811</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1196)</td>
    </tr>
    <tr>
        <td>AM * Spanish</td>
        <td>-0.2714</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1593)</td>
    </tr>
    <tr>
        <td>Signal Coverage<br/>(for stations located in the market)</td>
        <td>1.5938</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1004)</td>
    </tr>
    <tr>
        <td>FM * Signal Coverage</td>
        <td>0.6057</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1068)</td>
    </tr>
    <tr>
        <td>Small Station Dummy<br/>(shares imputed for some quarters)</td>
        <td>-0.4277</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0580)</td>
    </tr>
    <tr>
        <td>Out of Market Dummy</td>
        <td>-0.5082</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0834)</td>
    </tr>
    <tr>
        <td colspan="2">Transition Process for Unobserved Quality</td>
    </tr>
    <tr>
        <td>ρ<sup>ξ</sup></td>
        <td>0.8421</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0058)</td>
    </tr>
    <tr>
        <td>σ<sub>vξ</sub></td>
        <td>0.3132</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0020)</td>
    </tr>
    <tr>
        <td>Effect of Format Switch on Unobserved Quality</td>
        <td>-0.0501</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0103)</td>
    </tr>
  </tbody>
</table>

<sup>a</sup>16,481 observations, GMM objective function 2.90e-12. Time coefficients not reported. Std. errors in parentheses. AM * AC/CHR and *Rock combined due to small number of observations.

have partial coverage, have lower quality. Unobserved station quality is estimated to be quite persistent (*ρ*<sup>*ξ*</sup> = 0.84), while a format switch is estimated to cause a small but statistically significant drop in quality at the time of the switch.

## *Fit of the Listener Demand Model*

The listener demand model plays an important role in the counterfactual because it predicts a station’s audience in different formats. Figure 1 shows that the estimated model does well at matching the distribution of period-to-period share changes observed in the data for stations that switch formats, as well as those that remain in the same format. The transition process for unobserved station quality is estimated (apart from the intercept term) using only stations that remain in the same format, and the fact that predicted and

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1787</page_number>


<table>
  <caption>FIGURE 1.—Distribution of share changes: data (solid line), model prediction (dashed line, one simulation draw).</caption>
  <thead>
    <tr>
      <th rowspan="2">Change in Share (perc. point)</th>
      <th colspan="2">Switching Stations (Kernel Density)</th>
      <th colspan="2">Non-Switching Stations (Density)</th>
    </tr>
    <tr>
      <th>data</th>
      <th>model prediction</th>
      <th>data</th>
      <th>model prediction</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>-5.0</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
    </tr>
    <tr>
      <td>-2.5</td>
      <td>0.01</td>
      <td>0.00</td>
      <td>0.01</td>
      <td>0.01</td>
    </tr>
    <tr>
      <td>-1.5</td>
      <td>0.05</td>
      <td>0.02</td>
      <td>0.05</td>
      <td>0.05</td>
    </tr>
    <tr>
      <td>-1.0</td>
      <td>0.18</td>
      <td>0.08</td>
      <td>0.15</td>
      <td>0.15</td>
    </tr>
    <tr>
      <td>-0.5</td>
      <td>0.45</td>
      <td>0.40</td>
      <td>0.45</td>
      <td>0.45</td>
    </tr>
    <tr>
      <td>0.0</td>
      <td>1.22</td>
      <td>1.55</td>
      <td>1.58</td>
      <td>1.65</td>
    </tr>
    <tr>
      <td>0.5</td>
      <td>0.35</td>
      <td>0.40</td>
      <td>0.45</td>
      <td>0.45</td>
    </tr>
    <tr>
      <td>1.0</td>
      <td>0.08</td>
      <td>0.08</td>
      <td>0.15</td>
      <td>0.15</td>
    </tr>
    <tr>
      <td>1.5</td>
      <td>0.05</td>
      <td>0.02</td>
      <td>0.05</td>
      <td>0.05</td>
    </tr>
    <tr>
      <td>2.5</td>
      <td>0.01</td>
      <td>0.00</td>
      <td>0.01</td>
      <td>0.01</td>
    </tr>
    <tr>
      <td>5.0</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
    </tr>
  </tbody>
</table>


 


<table>
  <caption>FIGURE 1.—Distribution of share changes: data (solid line), model prediction (dashed line, one simulation draw).</caption>
  <thead>
    <tr>
      <th rowspan="2">Change in Share (perc. point)</th>
      <th colspan="2">Switching Stations (Kernel Density)</th>
      <th colspan="2">Non-Switching Stations (Density)</th>
    </tr>
    <tr>
      <th>data</th>
      <th>model prediction</th>
      <th>data</th>
      <th>model prediction</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>-5.0</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
    </tr>
    <tr>
      <td>-2.5</td>
      <td>0.02</td>
      <td>0.01</td>
      <td>0.01</td>
      <td>0.01</td>
    </tr>
    <tr>
      <td>-1.0</td>
      <td>0.15</td>
      <td>0.18</td>
      <td>0.10</td>
      <td>0.12</td>
    </tr>
    <tr>
      <td>-0.5</td>
      <td>0.45</td>
      <td>0.40</td>
      <td>0.35</td>
      <td>0.38</td>
    </tr>
    <tr>
      <td>-0.2</td>
      <td>0.95</td>
      <td>0.90</td>
      <td>0.90</td>
      <td>1.00</td>
    </tr>
    <tr>
      <td>0.0</td>
      <td>1.22</td>
      <td>1.45</td>
      <td>1.58</td>
      <td>1.65</td>
    </tr>
    <tr>
      <td>0.2</td>
      <td>0.90</td>
      <td>0.85</td>
      <td>0.85</td>
      <td>0.95</td>
    </tr>
    <tr>
      <td>0.5</td>
      <td>0.35</td>
      <td>0.30</td>
      <td>0.30</td>
      <td>0.35</td>
    </tr>
    <tr>
      <td>1.0</td>
      <td>0.10</td>
      <td>0.12</td>
      <td>0.08</td>
      <td>0.10</td>
    </tr>
    <tr>
      <td>2.5</td>
      <td>0.01</td>
      <td>0.01</td>
      <td>0.01</td>
      <td>0.01</td>
    </tr>
    <tr>
      <td>5.0</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
      <td>0.00</td>
    </tr>
  </tbody>
</table>


FIGURE 1.—Distribution of share changes: data (solid line), model prediction (dashed line, one simulation draw).

actual distributions fit well suggests that my assumption that the processes are the same is reasonable.<sup>35,36</sup>

## *5.2. Revenue Function*

Table V shows the coefficient estimates from two specifications of the revenue function (market-year coefficients are not reported). The first specification assumes that only demographics affect a listener’s value to a station, and the effects are measured relative to white males aged 25–49, while the second specification, used in the rest of the analysis, allows for format switching, common ownership, and the number of stations in the same format (competition).

<sup>35</sup>For example, if I assumed that a station received a new draw of ξ when it switched formats, then, because the standard deviation of the ξ’s is much greater than the standard deviation of the innovations (0.86 vs. 0.31), the variance of share changes for switching stations would be much greater, whereas it is only slightly greater. It is harder to rule out the possibility that firms know something about the innovations that their stations will receive in different formats when choices are made, because of the complicated forms of selection that this might introduce. Allowing for this type of selection in entry models is a topic of ongoing research (e.g., Roberts and Sweeting (2012)).

<sup>36</sup>One can also calculate the correlation between the share change predicted by the model (based on one simulation draw of ν) and the share change observed in the data for switching stations. This correlation is 0.28, and it is statistically significant at any standard significance level.

1788                ANDREW SWEETING

<page_number>14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License</page_number>

# TABLE V
## PARAMETER ESTIMATES FOR THE REVENUE FUNCTION<sup>a</sup>

<table>
  <thead>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="3"><em>Demographics</em></td>
    </tr>
    <tr>
        <td>Female</td>
        <td>0.1797</td>
        <td>0.1917</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0368)</td>
        <td>(0.0374)</td>
    </tr>
    <tr>
        <td>Age 12–24</td>
        <td>–0.5811</td>
        <td>–0.5883</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.1075)</td>
        <td>(0.1084)</td>
    </tr>
    <tr>
        <td>Age 50+</td>
        <td>–0.4531</td>
        <td>–0.4572</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0577)</td>
        <td>(0.0581)</td>
    </tr>
    <tr>
        <td>Black</td>
        <td>–0.1964</td>
        <td>–0.1961</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0148)</td>
        <td>(0.0155)</td>
    </tr>
    <tr>
        <td>Hispanic</td>
        <td>–0.1593</td>
        <td>–0.1596</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.0159)</td>
        <td>(0.0159)</td>
    </tr>
    <tr>
        <td colspan="3"> </td>
    </tr>
    <tr>
        <td colspan="3"><em>Station Characteristics and Competition</em></td>
    </tr>
    <tr>
        <td>Number of stations with same owner in format</td>
        <td>–</td>
        <td>0.0064</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.0047)</td>
    </tr>
    <tr>
        <td>Number of other stations in format</td>
        <td>–</td>
        <td>–0.0019</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.0018)</td>
    </tr>
    <tr>
        <td>Format switch in previous quarter</td>
        <td>–</td>
        <td>–0.1045</td>
    </tr>
    <tr>
        <td> </td>
        <td> </td>
        <td>(0.0279)</td>
    </tr>
    <tr>
        <td>$R^2$ (compared to a model with only market-year<br/>fixed effects)</td>
        <td>0.3182</td>
        <td>0.3208</td>
    </tr>
  </tbody>
</table>

<sup>a</sup>4,483 annual station observations (observations with imputed shares excluded). Market-year coefficients not reported. Standard errors corrected for imprecision in the demand parameters.

to affect revenues. The demographic coefficients are similar in both specifications, and they indicate that advertisers place different values on listeners with different demographics. In particular, a female listener aged 25–49 is 17–18% more valuable than a male of the same age, a black or Hispanic is 20% less valuable than a white listener, and young and old listeners are worth less than those aged 25–49.<sup>37</sup> The fact that male, older, and Hispanic listeners are less valuable will limit how many stations will want to switch from music programming when performance fees are introduced. In the second specification, a format switch is predicted to reduce station revenues by 10%, which may reflect the fact that switching stations discount the price of commercials while they develop new relationships, but the other coefficients have no statistically significant effect.

<sup>37</sup>The age effects are particularly large. The discount for young listeners is consistent with the fact that these listeners are particularly likely to switch stations to avoid commercials (Speck and Elliott (1997)). The local advertising market for older consumers may be more competitive, because they tend to consume several local media. For example, the Radio Advertising Bureau estimates that people aged above 64 are twice as likely to read a local newspaper as those aged 18–34 (www.rab.com/whyradio/mfdetails.cfm?id=8, accessed November 24, 2010).

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING

<page_number>1789</page_number>

The reported $R^2$s indicate that the model only explains some of the within-market variation in per listener revenues. However, the model does a reasonable job of predicting how station revenues change over time. For example, conditional on the observed change in audience, the correlation between observed year-to-year changes in station revenues and those predicted by the model for stations switching formats is 0.55.

## *5.3. Dynamic Parameters*

Repositioning costs and economies of scope are estimated using the dynamic model. After presenting the results based on estimates that use parametric approximations to the value function, I present the results from the alternative estimators that approximate the value function using forward simulation.

### *5.3.1. Estimates Based on Parametric Approximations of the Value Function*

Table VI shows the coefficient estimates from three different estimators where the value function is approximated using the same parametric function. Specifications (1) and (3) are the modified and iterated estimators with a pseudo-likelihood objective function, while specification (2) is the modified estimator where the objective function is based on matching various switching rates following POB, and these estimates should be compared with the estimates in column (1).

As stations are rarely moved to Dark, the key parameters for the counterfactual are the costs of switching between active formats and the scale parameter of the $\epsilon$s. All of the estimates indicate that these parameters vary systematically with market size, which allows the model to match the stylized fact that format switching rates are similar in small and large markets, even though average firm revenues (and hence values) are quite different.$^{38}$ One interpretation is that format switching is costly primarily because of the costs of marketing the station to new listeners (advertising will cost more in larger markets) and losing the goodwill in relationships with existing advertisers, as the value of goodwill may be proportional to revenues. The positive, but statistically insignificant, coefficients on the prior revenue of the station being moved (the mean of this variable is 0.92) is also consistent with this interpretation. Only one of the economies of scope coefficients is statistically significant at the 5% level, although five out of six coefficients are positive for markets with popula-

<sup>38</sup>Mean annual station advertising revenues were \$4.8 m., \$1.4 m., and \$0.6 m. for the three market groups. Based on the 10-K reports of publicly listed radio companies during my data period, operating income constituted between 20% and 40% of advertising revenues. However, it is likely that stations owned by smaller firms were less profitable.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>

1790
</page_number>

ANDREW SWEETING

# TABLE VI
# PARAMETER ESTIMATES FROM THE DYNAMIC MODEL

<table>
  <thead>
    <tr>
        <th> </th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
    </tr>
    <tr>
        <th> </th>
        <th>Modified Procedure</th>
        <th>Modified Procedure</th>
        <th>Iterated Procedure</th>
    </tr>
    <tr>
        <th>Specification</th>
        <th>P-Likelihood</th>
        <th>Moments</th>
        <th>P-Likelihood</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="4"><em>Costs of Move to Active Format ($ m.)</em></td>
    </tr>
    <tr>
        <td>Markets with pop. 1 m. +</td>
        <td>2.524</td>
        <td>1.897</td>
        <td>1.467</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.380)</td>
        <td>(0.441)</td>
        <td>(0.409)</td>
    </tr>
    <tr>
        <td>\* Recent Format Switch</td>
        <td>−0.001</td>
        <td>−0.001</td>
        <td>−0.000</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.098)</td>
        <td>(0.110)</td>
        <td>(0.065)</td>
    </tr>
    <tr>
        <td>Markets with pop. 0.25–1 m.</td>
        <td>0.669</td>
        <td>0.446</td>
        <td>0.388</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.157)</td>
        <td>(0.167)</td>
        <td>(0.102)</td>
    </tr>
    <tr>
        <td>\* Recent Format Switch</td>
        <td>0.077</td>
        <td>−0.213</td>
        <td>−0.044</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.082)</td>
        <td>(0.145)</td>
        <td>(0.067)</td>
    </tr>
    <tr>
        <td>Markets with pop. &lt; 0.25 m.</td>
        <td>0.233</td>
        <td>0.069</td>
        <td>0.135</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.099)</td>
        <td>(0.048)</td>
        <td>(0.063)</td>
    </tr>
    <tr>
        <td>\* Recent Format Switch</td>
        <td>0.030</td>
        <td>−0.0310</td>
        <td>−0.017</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.023)</td>
        <td>(0.105)</td>
        <td>(0.054)</td>
    </tr>
    <tr>
        <td>\* Revenue of Switching Station (all markets)</td>
        <td>0.034</td>
        <td>0.116</td>
        <td>0.022</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.168)</td>
        <td>(0.211)</td>
        <td>(0.118)</td>
    </tr>
    <tr>
        <td colspan="4"><em>Additional Cost of Move to Active From Dark ($ m.)</em></td>
    </tr>
    <tr>
        <td>Markets with pop. 1 m. +</td>
        <td>−0.501</td>
        <td>−0.445</td>
        <td>−0.291</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.220)</td>
        <td>(0.183)</td>
        <td>(0.095)</td>
    </tr>
    <tr>
        <td>Markets with pop. 0.25–1 m.</td>
        <td>−0.255</td>
        <td>−0.495</td>
        <td>−0.147</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.166)</td>
        <td>(0.098)</td>
        <td>(0.069)</td>
    </tr>
    <tr>
        <td>Markets with pop. &lt; 0.25 m.</td>
        <td>−0.061</td>
        <td>−0.078</td>
        <td>−0.035</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.091)</td>
        <td>(0.032)</td>
        <td>(0.042)</td>
    </tr>
    <tr>
        <td colspan="4"><em>Cost of Moving From Active to Dark ($ m.)</em></td>
    </tr>
    <tr>
        <td>Markets with pop. 1 m. +</td>
        <td>2.704</td>
        <td>3.126</td>
        <td>1.572</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.744)</td>
        <td>(0.964)</td>
        <td>(0.503)</td>
    </tr>
    <tr>
        <td>Markets with pop. 0.25–1 m.</td>
        <td>0.636</td>
        <td>1.636</td>
        <td>0.369</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.230)</td>
        <td>(0.582)</td>
        <td>(0.130)</td>
    </tr>
    <tr>
        <td>Markets with pop. &lt; 0.25 m.</td>
        <td>0.199</td>
        <td>0.654</td>
        <td>0.115</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.054)</td>
        <td>(0.322)</td>
        <td>(0.022)</td>
    </tr>
    <tr>
        <td>Revenue of Switching Station</td>
        <td>1.108</td>
        <td>3.105</td>
        <td>0.642</td>
    </tr>
    <tr>
        <td> </td>
        <td>(1.330)</td>
        <td>(1.506)</td>
        <td>(1.003)</td>
    </tr>
    <tr>
        <td colspan="4"><em>Economies of Scope</em></td>
    </tr>
    <tr>
        <td>Markets with pop. 1 m. +</td>
        <td>0.134</td>
        <td>−0.046</td>
        <td>0.078</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.099)</td>
        <td>(0.072)</td>
        <td>(0.075)</td>
    </tr>
    <tr>
        <td>Markets with pop. 0.25–1 m.</td>
        <td>0.026</td>
        <td>0.102</td>
        <td>0.015</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.018)</td>
        <td>(0.021)</td>
        <td>(0.010)</td>
    </tr>
    <tr>
        <td>Markets with pop. &lt; 0.25 m.</td>
        <td>−0.006</td>
        <td>−0.014</td>
        <td>−0.033</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.012)</td>
        <td>(0.010)</td>
        <td>(0.029)</td>
    </tr>
    <tr>
        <td colspan="4"><em>Scale of ε’s</em></td>
    </tr>
    <tr>
        <td>Markets with pop. 1 m. +</td>
        <td>0.517</td>
        <td>0.408</td>
        <td>0.300</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.180)</td>
        <td>(0.063)</td>
        <td>(0.087)</td>
    </tr>
    <tr>
        <td>Markets with pop. 0.25–1 m.</td>
        <td>0.144</td>
        <td>0.091</td>
        <td>0.083</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.055)</td>
        <td>(0.037)</td>
        <td>(0.026)</td>
    </tr>
    <tr>
        <td>Markets with pop. &lt; 0.25 m.</td>
        <td>0.050</td>
        <td>0.014</td>
        <td>0.028</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.014)</td>
        <td>(0.007)</td>
        <td>(0.009)</td>
    </tr>
  </tbody>
</table>

DYNAMIC PRODUCT POSITIONING

1791

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

tions above 0.25 m. These weak results are consistent with the fact that, in this sample of data, and using my format definitions, firms are not systematically making their stations more clustered in particular formats.<sup>39</sup>

As suggested in Section 4, a priori arguments can be made for favoring each of these specifications. For example, the pseudo-likelihood estimators may be more efficient, but the moment-based estimator might be expected to be more robust in small samples and it does provide a closer match to some of the main sources of variation in the data.<sup>40</sup> For example, in the data, 10 times as many stations move to Spanish in markets with above median Hispanic populations as in markets with below median Hispanic populations. The moment-based estimates predict that 7.2 times as many stations should move, whereas the estimates in column (1), which imply more random switching because the $\theta^{\epsilon}$'s are larger, only predict that 2.6 times as many stations should move. For blacks and the number of stations switching to Urban, the equivalent data, moment, and modified pseudo-likelihood predictions are 5, 6.5, and 2.9, respectively. The iterated pseudo-likelihood-based estimates in column (3) make predictions that are more like the moment-based estimates (7.0 for Hispanics/Spanish and 5.9 for blacks/Urban).

On the other hand, when the $\theta^{\epsilon}$'s are too small, problems of multiple equilibria become more acute, as each firm’s choice probabilities become more sensitive to how they expect other stations should move. For example, the estimates from an iterated version of the moment-based estimator appeared unstable<sup>41</sup> and so are not reported, and there was also some evidence of this type of instability performing the counterfactuals for smaller markets with the estimates in column (2). For this practical reason, I proceed using the estimates in column (1), drawing confidence from the fact that the estimates in the remaining columns are qualitatively similar.

## *5.3.2. Estimates Based on Forward-Simulation Approximations to the Value Function*

An alternative approach to estimating dynamic games with a rich state space involves approximating the value function via forward simulation based on

<sup>39</sup> My specification only allows for economies from operating stations in the same format. O’Gorman and Smith (2008) estimated that there are significant economies to owning stations in the different formats. Jeziorski (2013), who used information on both which mergers are consummated and format switching decisions, found that both types of economy are significant.

<sup>40</sup> One reason why the moment-based and the pseudo-likelihood estimates are quite similar here may well be that the parametric approximation itself involves some averaging across states, which is one of the main advantages of the moment-based method.

<sup>41</sup> This instability may also arise from the fact that the moment-based objective function is less well-behaved than the one based on the pseudo-likelihood, so that the iterative algorithm sometimes moves between local minima associated with different parameters.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1792</page_number> ANDREW SWEETING

initial first-stage estimates of the conditional choice probabilities (e.g., Bajari, Benkard and Levin (2007, BBL), and the applications by Ryan (2012), Snider (2009), and to radio by Jeziorski (2013)). Estimation is based on the inequalities, or moment inequalities, that are implied by the equilibrium assumption that each firm’s actual strategy, reflected in its estimated conditional choice probabilities, should result in a higher value than any alternative, given the strategies of other players. Here, I report estimates of repositioning costs, economies of scope, and the scale of payoff shocks based on this type of approximation. This provides a robustness check on my coefficients, but it also more generally provides evidence on the performance of different approaches to estimating dynamic games. Here, I give a very brief overview of the method, with full details in Appendix D of the Supplemental Material.

As a firm’s payoffs are linear in the parameters, a firm’s value when it uses strategy $\Gamma_o$ and other firms use strategies $\Gamma_{-o}^*$ can be expressed as

$$ \begin{aligned} V_o(M_{j,o,t} | \Gamma_o, \Gamma_{-o}^*, \theta) \\ &= \mathbf{V}_{o, \Gamma_o^*, \Gamma_{-o}^*} \theta \\ &= \mathbf{R}_{o, \Gamma_o^*, \Gamma_{-o}^*} - \theta^W \mathbf{W}_{o, \Gamma_o^*, \Gamma_{-o}^*} + \theta^C \mathbf{C}_{o, \Gamma_o^*, \Gamma_{-o}^*} + \theta^\varepsilon \boldsymbol{\varepsilon}_{o, \Gamma_o^*, \Gamma_{-o}^*}^F, \end{aligned} $$

where $\mathbf{R}_{o, \Gamma_o^*, \Gamma_{-o}^*} = E_{o, \Gamma_o^*, \Gamma_{-o}^*} \sum_{t'=0}^\infty \beta^{t'} \sum_{s \in S^o} R_s(\mathcal{M}_{o,t+t'} | \gamma)$, the expected discounted sum of future revenues, and $\mathcal{M}_{o,t+t'}$ is $o$’s state at time $t + t'$. $\mathbf{W}$ (expected discounted repositioning costs), $\mathbf{C}$ (economies of scope), and $\boldsymbol{\epsilon}$ are defined similarly. The equilibrium restrictions used in estimation are that

$$ (20) \quad V_o(M_{j,o,t} | \Gamma_o^*, \Gamma_{-o}^*, \theta) - V_o(M_{j,o,t} | \Gamma_o^a, \Gamma_{-o}^*, \theta) \geq 0 \quad \forall \Gamma_o^a, M_{j,o,t}, $$

where $\Gamma^*$ are equilibrium strategies and $\Gamma_o^a$ is a particular alternative policy. Empirical implementation involves constructing estimates of $\mathbf{R}$, $\mathbf{W}$, $\mathbf{C}$, and $\boldsymbol{\epsilon}$ based on observed policies (i.e., first-stage estimates of the conditional choice probabilities, and the demand and revenue models) and a finite number of alternatives (the ones used are detailed in the Appendix) using forward simulation. I consider two estimators of the parameters: the one proposed by BBL,

$$ (21) \quad \widehat{\theta^{BBL}} = \arg \min_\theta \sum_o \sum_{\forall a} \max \{ (\mathbf{V}_{o, \Gamma_o^a, \Gamma_{-o}^*} - \mathbf{V}_{o, \Gamma_o^*, \Gamma_{-o}^*}) \theta, 0 \}^2, $$

and one that follows the moment-inequality approach of PPHI which finds the set of parameters that satisfy the following linear moment inequalities:

$$ (22) \quad \widehat{\theta^{PPHI}} \text{ is the set of } \theta \quad \text{where} \quad \frac{1}{O} \sum_o (\mathbf{V}_{o, \Gamma_o^*, \Gamma_{-o}^*} - \mathbf{V}_{o, \Gamma_o^a, \Gamma_{-o}^*}) \theta \geq 0 \quad \forall \Gamma_o^a. $$

The difference between these estimators is that the PPHI estimator uses the equilibrium implication that observed policies should do better on average

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1793</page_number>

(across states),<sup>42</sup> whereas the BBL estimator uses the fact that the equilibrium implies that they should do better in every state. In a similar way to POB’s comparison between a moment-based entry rate matching estimator and a likelihood-based estimator, the PPHI estimator sacrifices information, but, because of averaging, it may be more robust to approximation errors arising from either forward simulation or inaccuracies in the first-stage choice probabilities that result from either specification bias (a parametric specification is imposed) or the finite sample.

As has been noted in the prior literature, it can be difficult to estimate more than a few parameters using these approaches. I therefore estimate the model separately for the three market-size groups, and, for each group, I only estimate a cost of switching to an active format, the economy of scope parameter, and the scale of the payoff shocks (this parameter is restricted to be nonnegative).<sup>43</sup> The estimates, based on six alternative policies that are chosen to be intuitively informative about these parameters, are reported in Table VII (note that exactly the same simulations and alternative policies are used for both types of estimator).

The PPHI estimates are sets because, for each group of markets, there is a convex set of parameters that satisfies all six of the linear moment inequalities, and the reported estimates are the highest and lowest values of each of the parameters that satisfy each of the inequalities. The BBL numbers are point estimates, because no parameters satisfy all of the inequalities (the proportion violated is reported in the table).

Two features of these estimates deserve attention. First, the preferred estimates using value function approximation lie within or close to the bounds implied by the PPHI estimator. In this sense, the preferred estimates in Table VI are consistent with those of a quite different estimation methodology, while having the advantage of being point estimates that can be easily used in the counterfactuals. Second, for the large and small market groups, the BBL estimates imply substantially higher repositioning costs and more volatile payoff shocks than either the value function approximation estimates or the upper bounds of the PPHI estimates. As these estimates of repositioning costs

<sup>42</sup>To be clear, this means that there is one inequality for a particular type of deviation a (e.g., a policy that involves more switching than the estimated CCPs). One could create additional inequalities by interacting the difference between the optimal and alternative values with instruments (here, the instrument is a constant). This would shrink the size of the parameter set that satisfies all of the inequalities, sometimes to a point, but experimentation revealed that such estimates were sensitive to the alternative policies and instruments used.

<sup>43</sup>In simplifying the model, I assume that the cost of moving from Dark to an active format is the same as moving between a pair of active formats, and that there is no cost to moving to Dark. I have estimated specifications with separate coefficients for these costs, but without the imposition of additional constraints, found that the estimates produced were often completely implausible (e.g., a cost of \$100 million for switching to Dark).

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License
1794 ANDREW SWEETING

TABLE VII
ESTIMATES BASED ON FORWARD SIMULATION

<table>
  <thead>
    <tr>
        <th> </th>
        <th>Repositioning Cost</th>
        <th>Scope Economy</th>
        <th>Scale of Payoff Shock</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td colspan="4"><em>Markets With Pop. &gt; 1 m.</em></td>
    </tr>
    <tr>
        <td>Moment inequality bounds (PPHI)</td>
        <td>[2.194, 11.031]</td>
        <td>[−0.081, 0.056]</td>
        <td>[0.074, 1.729]</td>
    </tr>
    <tr>
        <td>95% CI</td>
        <td>[0.652, 13.877]</td>
        <td>[−0.126, 0.102]</td>
        <td>[0.011, 1.793]</td>
    </tr>
    <tr>
        <td>BBL point estimate</td>
        <td>18.668</td>
        <td>0.337</td>
        <td>3.771</td>
    </tr>
    <tr>
        <td>std. error</td>
        <td>(1.765)</td>
        <td>(0.048)</td>
        <td>(0.342)</td>
    </tr>
    <tr>
        <td>proportion of inequalities violated</td>
        <td>24.4%</td>
        <td>–</td>
        <td>–</td>
    </tr>
    <tr>
        <td colspan="4"><em>Markets With Pop. 0.25 m.–1 m.</em></td>
    </tr>
    <tr>
        <td>Moment inequality bounds (PPHI)</td>
        <td>[0.464, 3.421]</td>
        <td>[−0.071, 0.031]</td>
        <td>[0.015, 0.549]</td>
    </tr>
    <tr>
        <td>95% CI</td>
        <td>[0.232, 4.035]</td>
        <td>[−0.082, 0.042]</td>
        <td>[0, 0.568]</td>
    </tr>
    <tr>
        <td>BBL point estimate</td>
        <td>3.046</td>
        <td>0.013</td>
        <td>0.630</td>
    </tr>
    <tr>
        <td>std. error</td>
        <td>(0.190)</td>
        <td>(0.006)</td>
        <td>(0.043)</td>
    </tr>
    <tr>
        <td>proportion of inequalities violated</td>
        <td>10.0%</td>
        <td>–</td>
        <td>–</td>
    </tr>
    <tr>
        <td colspan="4"><em>Markets With Pop. &lt; 0.25 m.</em></td>
    </tr>
    <tr>
        <td>Moment inequality bounds (PPHI)</td>
        <td>[0.230, 1.541]</td>
        <td>[−0.022, 0.008]</td>
        <td>[0.005, 0.251]</td>
    </tr>
    <tr>
        <td>95% CI</td>
        <td>[0.081, 1.690]</td>
        <td>[−0.027, 0.014]</td>
        <td>[0, 0.258]</td>
    </tr>
    <tr>
        <td>BBL point estimate</td>
        <td>2.148</td>
        <td>0.011</td>
        <td>0.455</td>
    </tr>
    <tr>
        <td>std. error</td>
        <td>(0.242)</td>
        <td>(0.004)</td>
        <td>(0.051)</td>
    </tr>
    <tr>
        <td>% of BBL inequalities violated</td>
        <td>22.4%</td>
        <td>–</td>
        <td>–</td>
    </tr>
  </tbody>
</table>

also look implausibly high, and we would not expect the BBL estimates to lie outside the PPHI bounds if averaging was only sacrificing information, rather than affecting bias, it seems appropriate to proceed using the estimates based on parametric value function approximation.<sup>44</sup>

# 6. COUNTERFACTUAL: THE EFFECT OF THE PROPOSED PERFORMANCE RIGHTS ACT ON FORMAT CHOICES

With the estimates in hand, I now use the model to predict how performance rights fees would affect format choices. I assume that these fees would be calculated as a percentage of advertising revenues for music stations, as assumed in U.S. GAO (2010).<sup>45</sup> I assume that these fees were imposed as an unanticipated shock in Fall 2004, and, having solved the model, I simulate markets forward 40 periods from that date. The reported results are based on 51 of the 102 markets in my data.<sup>46</sup> Of course, fees were not imposed in 2004, but the

<sup>44</sup> Jeziorski (2013) also estimated repositioning costs using BBL, although these are not the focus of his study. In large markets, his estimate is larger than mine, but it is smaller in smaller markets.

<sup>45</sup> The legislation did envisage that stations with very low revenues, which would apply to most stations outside urban markets, would be charged flat fees.

<sup>46</sup> Markets are ordered by their 2002 Arbitron market rank (based on population), and I then select every other market. This market selection is done to reduce the computational burden,

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1795</page_number>

industry is sufficiently similar now that the results should be informative about what would happen if they were introduced now.<sup>47</sup> I abstract away from other ongoing demand changes by assuming that demographics remain fixed at their current levels.

I present two sets of results. The first set examines what would happen to the number of music stations and music audiences with no fees, 10% fees, and 20% fees based on the estimates in column (1) of Table VI. I then examine how the effects of a 10% fee vary with some of the model’s parameters, such as the level of repositioning costs and the degree of heterogeneity in listener tastes.

## *6.1. Implementation*

As markets are independent and I do not need to pool information from across markets in the way that I did for estimation, I solve the model for each market separately. Full details are in Appendix B of the Supplemental Material, but I provide an overview here. The first step, following the description in Section 3, solves for $\lambda_{m}^{*, \text{FEE}}$, which will depend on the level of the fee. This is done using the states observed in the data for Fall 2004, and 499 duplicates of this market-quarter where station formats and qualities are permuted. With fees, states with more nonmusic stations are oversampled as markets are expected to evolve in this direction.

The second step is to simulate the model forward. The first step provides firms’ equilibrium choice probabilities in Fall 2004, and these probabilities and the estimated transition process for unobserved quality are used to move the model forward one period. However, this takes the market to a format/quality configuration that was not used in the first step, so $\lambda_{m}^{*, \text{FEE}}$ is used to solve for equilibrium choice probabilities in this new market structure, and these are used to simulate the model forward one more period. The process of solving for choice probabilities and forward simulating continues for 40 periods (20 years) after the introduction of the fees. For each market, the forward simulation process is repeated 10 times, and the results below are based on the mean and the standard deviation of these simulations.<sup>48</sup>

while still allowing me to make broad statements about what would happen in the industry as a whole.

<sup>47</sup>The most obvious change that has taken place is that broadcast stations increasingly compete with satellite and online radio (where fees are paid). If this competition affects formats differently, then this would affect format choices, and possibly the effects of fees. However, nationally, the amount of listening to the different formats that I use was very similar in 2010 and 2004, based on the numbers reported by Arbitron in its 2011 *Radio Today* report (Arbitron (2011)), although there were some shifts in the popularity of different types of programming within these aggregated categories.

<sup>48</sup>The computational burden of the two steps is high, so I do not try to account for the estimation error in the parameters.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

<page_number>1796</page_number>

ANDREW SWEETING

TABLE VIII
EVOLUTION OF THE NUMBER OF MUSIC STATIONS UNDER DIFFERENT PERFORMANCE RIGHTS FEES<sup>a</sup>

<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="3">Music Stations</th>
        <th colspan="3">Music Listening</th>
        <th colspan="3">Nonmusic Listening</th>
    </tr>
    <tr>
        <th>Fee Level:</th>
        <th>0%</th>
        <th>10%</th>
        <th>20%</th>
        <th>0%</th>
        <th>10%</th>
        <th>20%</th>
        <th>0%</th>
        <th>10%</th>
        <th>20%</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Period prior to introduction (Fall 2004)</td>
        <td>713</td>
        <td>713</td>
        <td>713</td>
        <td>0.254</td>
        <td>0.254</td>
        <td>0.254</td>
        <td>0.095</td>
        <td>0.095</td>
        <td>0.095</td>
    </tr>
    <tr>
        <td>+1 period</td>
        <td>714</td>
        <td>701</td>
        <td>693</td>
        <td>0.254</td>
        <td>0.250</td>
        <td>0.246</td>
        <td>0.094</td>
        <td>0.097</td>
        <td>0.101</td>
    </tr>
    <tr>
        <td> </td>
        <td>(3.7)</td>
        <td>(4.6)</td>
        <td>(4.6)</td>
        <td>(0.002)</td>
        <td>(0.003)</td>
        <td>(0.003)</td>
        <td>(0.002)</td>
        <td>(0.003)</td>
        <td>(0.004)</td>
    </tr>
    <tr>
        <td>+5 periods</td>
        <td>715</td>
        <td>682</td>
        <td>626</td>
        <td>0.253</td>
        <td>0.243</td>
        <td>0.228</td>
        <td>0.094</td>
        <td>0.103</td>
        <td>0.110</td>
    </tr>
    <tr>
        <td> </td>
        <td>(6.1)</td>
        <td>(7.2)</td>
        <td>(7.1)</td>
        <td>(0.003)</td>
        <td>(0.003)</td>
        <td>(0.005)</td>
        <td>(0.002)</td>
        <td>(0.003)</td>
        <td>(0.004)</td>
    </tr>
    <tr>
        <td>+10 periods (5 years)</td>
        <td>717</td>
        <td>665</td>
        <td>595</td>
        <td>0.253</td>
        <td>0.238</td>
        <td>0.220</td>
        <td>0.095</td>
        <td>0.106</td>
        <td>0.116</td>
    </tr>
    <tr>
        <td> </td>
        <td>(8.0)</td>
        <td>(9.8)</td>
        <td>(9.8)</td>
        <td>(0.004)</td>
        <td>(0.004)</td>
        <td>(0.006)</td>
        <td>(0.003)</td>
        <td>(0.003)</td>
        <td>(0.005)</td>
    </tr>
    <tr>
        <td>+20 periods (10 years)</td>
        <td>716</td>
        <td>651</td>
        <td>582</td>
        <td>0.252</td>
        <td>0.237</td>
        <td>0.220</td>
        <td>0.096</td>
        <td>0.106</td>
        <td>0.117</td>
    </tr>
    <tr>
        <td> </td>
        <td>(10.1)</td>
        <td>(11.5)</td>
        <td>(11.8)</td>
        <td>(0.004)</td>
        <td>(0.005)</td>
        <td>(0.006)</td>
        <td>(0.003)</td>
        <td>(0.003)</td>
        <td>(0.003)</td>
    </tr>
    <tr>
        <td>+40 periods (20 years)</td>
        <td>720</td>
        <td>652</td>
        <td>578</td>
        <td>0.253</td>
        <td>0.237</td>
        <td>0.219</td>
        <td>0.096</td>
        <td>0.107</td>
        <td>0.118</td>
    </tr>
    <tr>
        <td> </td>
        <td>(10.4)</td>
        <td>(10.9)</td>
        <td>(12.0)</td>
        <td>(0.004)</td>
        <td>(0.005)</td>
        <td>(0.005)</td>
        <td>(0.003)</td>
        <td>(0.004)</td>
        <td>(0.004)</td>
    </tr>
  </tbody>
</table>

<sup>a</sup> Standard deviations across 10 simulations in parentheses. Results based on sample of 51 markets. Music listening measured as the average combined market share of music stations across markets.

### *Equilibrium Selection*

Multiple equilibria are a common feature of games, and except in relatively simple games, it is rarely possible to enumerate all of the equilibria. In this paper, I do not attempt this type of enumeration and will instead rely on the equilibrium that my solution method, detailed in Appendix B, finds for different levels of fees.<sup>49</sup>

## *6.2. The Effects of Performance Fees on Market Structure*

Table VIII shows how the number of contemporary music stations and the average (across markets) combined market share of music and nonmusic stations (based on the market definition used in estimating the model) are predicted to change for the different levels of fees. In the absence of fees, the total number of music stations is predicted to remain approximately the same, although this masks the fact that, in certain markets, the model predicts significant changes in the number of stations in particular formats that look under-

<sup>49</sup> Experimentation using some example markets indicated that, when I could find multiple equilibria, they have similar implications for changes in the number of music stations and amount of music listening, although they differed in which stations tend to move first. However, there may be some equilibria which my solution method, based on the iteration of best responses, can never find. Aguirregabiria and Ho (2012) implemented an alternative method, proposed in Aguirregabiria (2012), for finding counterfactual equilibria based on Taylor expansions.

DYNAMIC PRODUCT POSITIONING <page_number>1797</page_number>
14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

or over-served given the demand/revenue models and current demographics. The model predicts that, over 20 years (which I will call the long run in what follows), 10% and 20% fees would reduce the number of music stations by 9.4% and 20%, respectively, relative to the no fee case, or by 1.3 and 2.8 stations per market.<sup>50</sup> Based on the preferred estimates, $93 million (std. deviation $11 million) more is spent on repositioning costs with 10% fees than with no fees (no discounting). This cost increase is greater, by around 20%, than what one would expect based on the change in the number of music stations alone, because these switches cause some additional churn, with some stations moving from nonmusic to music formats, and others between nonmusic formats, to avoid greater competition. Relative to the long-run format structure with no fees, the AC, Country, and Rock formats lose roughly equal numbers of stations, while the Urban format loses the least (3% of its stations with 10% fees). This is consistent with the fact that black listeners are relatively unwilling to substitute to other formats, and in markets where Urban stations are common, blacks make up a large proportion of the population. All three nonmusic formats gain stations, with the gain in Spanish stations concentrated in markets with large existing Hispanic populations (recall that, for the purposes of the counterfactual, I assume that demographics remain constant).

The predicted changes in music listenership are roughly 30% smaller than the changes in the number of stations, reflecting the fact that a listener who likes a music station that switches to a nonmusic format will often switch to one of the remaining music stations.<sup>51</sup> Most of the remaining listeners switch to nonmusic programming, rather than switching off their radios, reflecting the high value of $\gamma^{\sigma}$, although combined radio listening falls slightly. Unfortunately, as listeners are not observed paying prices for listening to the radio, it is not possible to quantify a dollar welfare effect of these changes in listenership.

The long-run adjustment does not take place immediately, but, for both 10% and 20% fees, at least 40% of the long-run change in the number of music stations is completed within 5 periods ($2\frac{1}{2}$ years). The adjustment in music audiences is predicted to happen more quickly; for example, with 20% fees, 74% of the change in the amount of music listening takes place in 5 periods, compared with 63% of the change in the number of stations. This reflects the fact that, at least in the equilibria that I find, higher quality stations are more likely to switch formats when fees are initially put in place, than without fees (or in the data). For example, with no fees the average per-period revenues of

<sup>50</sup>The number of Dark stations per market is predicted to increase by 0.07 and 0.11, respectively, under these fees.

<sup>51</sup>These results imply that a higher level of fees would raise performers’ and record companies’ total revenues from fees. However, assessing whether higher fees would be in the interests of these parties also requires knowing how sales of recorded music and concert tickets are affected by airplay. Different studies have reached contrasting conclusions about this elasticity (Dertouzos (2008)).

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1798  ANDREW SWEETING <page_number>1798</page_number>

# TABLE IX

EVOLUTION OF THE NUMBER OF MUSIC STATIONS AND MUSIC AUDIENCES UNDER 10% FEES FOR DIFFERENT ASSUMPTIONS ON THE STRUCTURAL PARAMETERS RELATIVE TO EVOLUTION WITH NO FEES (FALL 2004 INDEXED TO 1)<sup>a</sup>

<table>
  <thead>
    <tr>
        <th> </th>
        <th colspan="2">True Parameters (“Base case”)</th>
        <th colspan="2">Less Taste Heterogeneity</th>
        <th colspan="2">Higher Repositioning Costs</th>
        <th colspan="2">Higher Repositioning Costs and High σε</th>
    </tr>
    <tr>
        <th>Column:</th>
        <th>(1)</th>
        <th>(2)</th>
        <th>(3)</th>
        <th>(4)</th>
        <th>(5)</th>
        <th>(6)</th>
        <th>(7)</th>
        <th>(8)</th>
    </tr>
    <tr>
        <th> </th>
        <th> </th>
        <th>Music</th>
        <th> </th>
        <th>Music</th>
        <th> </th>
        <th>Music</th>
        <th> </th>
        <th>Music</th>
    </tr>
    <tr>
        <th> </th>
        <th>Stations</th>
        <th>Audiences</th>
        <th>Stations</th>
        <th>Audiences</th>
        <th>Stations</th>
        <th>Audiences</th>
        <th>Stations</th>
        <th>Audiences</th>
    </tr>
  </thead>
  <tbody>
    <tr>
        <td>Fall 2004</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
        <td>1</td>
    </tr>
    <tr>
        <td>+1 period</td>
        <td>0.981</td>
        <td>0.984</td>
        <td>0.941</td>
        <td>0.950</td>
        <td>0.986</td>
        <td>0.992</td>
        <td>0.986</td>
        <td>0.993</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.006)</td>
        <td>(0.005)</td>
        <td>(0.010)</td>
        <td>(0.012)</td>
        <td>(0.002)</td>
        <td>(0.003)</td>
        <td>(0.004)</td>
        <td>(0.002)</td>
    </tr>
    <tr>
        <td>+5 periods</td>
        <td>0.954</td>
        <td>0.961</td>
        <td>0.875</td>
        <td>0.895</td>
        <td>0.967</td>
        <td>0.978</td>
        <td>0.971</td>
        <td>0.980</td>
    </tr>
    <tr>
        <td> </td>
        <td>(0.010)</td>
        <td>(0.009)</td>
        <td>(0.014)</td>
        <td>(0.013)</td>
        <td>(0.006)</td>
        <td>(0.006)</td>
        <td>(0.008)</td>
        <td>(0.008)</td>
    </tr>
    <tr>
        <td>+10 periods</td>
        <td>0.930</td>
        <td>0.941</td>
        <td>0.810</td>
        <td>0.851</td>
        <td>0.940</td>
        <td>0.959</td>
        <td>0.952</td>
        <td>0.964</td>
    </tr>
    <tr>
        <td>(5 years)</td>
        <td>(0.015)</td>
        <td>(0.010)</td>
        <td>(0.017)</td>
        <td>(0.015)</td>
        <td>(0.010)</td>
        <td>(0.009)</td>
        <td>(0.014)</td>
        <td>(0.012)</td>
    </tr>
    <tr>
        <td>+40 periods</td>
        <td>0.906</td>
        <td>0.937</td>
        <td>0.801</td>
        <td>0.841</td>
        <td>0.910</td>
        <td>0.942</td>
        <td>0.918</td>
        <td>0.945</td>
    </tr>
    <tr>
        <td>(20 years)</td>
        <td>(0.014)</td>
        <td>(0.010)</td>
        <td>(0.018)</td>
        <td>(0.016)</td>
        <td>(0.011)</td>
        <td>(0.009)</td>
        <td>(0.016)</td>
        <td>(0.009)</td>
    </tr>
  </tbody>
</table>

<sup>a</sup> Results based on sample of 51 markets. Standard deviations across 10 simulations in parentheses. Music listening measured as the average combined market share of music stations across markets.

a station that changes format in the first 5 periods is $0.9 million, while with 10% fees the average (gross) revenue is $1.4 m.<sup>52</sup>

*The Effect of Repositioning Costs, Payoff Shocks, and Tastes Heterogeneity on the Transition*

I investigate how these features of the model affect the predicted speed of adjustment. [Table IX] shows how changes in the number of music stations and music listening when 10% fees are introduced are affected by several counterfactuals, where, for ease of comparison, values with no fees are indexed to 1.<sup>53</sup> Columns (1) and (2) reflect what happens under 10% fees in [Table VIII].

*Taste Heterogeneity*

The fact that most listeners, and particularly those in the most desirable demographics, prefer music reduces the incentives of stations to switch to nonmusic formats to avoid fees. To understand how much more repositioning would

<sup>52</sup> Quality can also be calculated using the demand model. Measured by the exponent of the sum of $\xi_{st}$ and the fixed quality components (e.g., signal coverage), the average quality of a switching station with no fees is 1.09, and with 10% fees it is 1.30.

<sup>53</sup> I have also investigated the effects of advertisers’ heterogeneous valuations. If advertisers value all demographics in the same way, there are substantial flows of stations into nonmusic formats (which attract older and Hispanic listeners) without fees. These flows become larger with fees, but there is no clear effect of the different valuations on the speed of adjustment.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

# DYNAMIC PRODUCT POSITIONING

<page_number>1799</page_number>

take place if there was less taste heterogeneity, I resolve the model (for no fees and 10% fees) when all of the demographic-format parameters are equal to half of their estimated values ($\gamma^\sigma$ takes its estimated value, so commercial radio listening remains almost fixed).<sup>54</sup> Columns (3) and (4) of Table IX show how the industry adjusts given these hypothetical parameters. With less taste heterogeneity, the short-run and long-run changes in the number of music stations are roughly twice as large as in the base case. Because of reduced format loyalty, this results in an even larger decline in the amount of listening to music radio.

**Repositioning Costs**

Columns (5) and (6) report the effects of fees when the cost of repositioning stations is increased by 25%. In the long run, this change has little effect on the number of music stations, consistent with listener tastes, advertiser valuations, fees, and the importance of choice-specific payoff shocks being the long-run determinants of the format structure in each market. However, higher repositioning costs slow the speed of the transition that takes place when fees are introduced. This can be rationalized by the fact that, when repositioning costs are high, there is a greater incentive for a firm to wait to see what its competitors will do before making a desired switch, unless it receives a very favorable draw of the associated payoff shock.

**Repositioning Costs and Scale of Payoff Shocks**

As can clearly be seen in all of the different estimates from Section 5, higher estimates of repositioning costs are accompanied by larger estimates of the scale of the payoff shocks, as these changes together allow the predicted rate of switching to be relatively unchanged. It is therefore interesting to ask whether changing both of these parameters affects the predictions. In columns (7) and (8), both repositioning costs and the scale of the $\varepsilon$'s are increased by 25%. While these changes result in approximately the same amount of switching as the base parameters with no fees, the predictions with fees are not the same as in the base case. In particular, the model predicts a smaller long-run change in the number of music stations, and music listening, as the choice-specific payoff shocks become more important, and there are more choices associated with music formats than nonmusic ones. The net transition is also slower, as the value of waiting for a favorable $\varepsilon$ draw associated with a desired move increases with the variance of these shocks.

<sup>54</sup>This simulation is performed with values for the linear demand parameters, δ<sub>st</sub> and ξ<sub>st</sub>, which would be estimated if the random coefficients had these hypothetical values. The parameters of the revenue function, and the repositioning cost and economies of scope parameters, are the same as in the base model.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1800 ANDREW SWEETING

# 7. CONCLUSION

This article uses a dynamic model to predict how the format structure of local radio markets would change if broadcast music radio stations had to pay fees for musical performance rights, as proposed in legislation that received broad political support in 2009. This setting provides a natural one for modeling the effects of a policy that favors a particular type of product, because the sets of available products (stations) and possible product types (formats) are well-defined, and, even in the absence of fees, significant product repositioning is observed. My results suggest that fees equal to 10% or 20% of revenues would have significant, and fairly rapid, effects on the number of music stations, but that the declines would not be as dramatic as some people in the broadcasting industry have suggested, for the simple reason that lots of people prefer music programming, including many listeners who are particularly valued by advertisers. Of course, all of the counterfactual results are predictions, based on a particular set of modeling assumptions and also a method for approximating the solution. It will therefore be both interesting and important to test the accuracy of the model’s predictions if and when performance fees are eventually introduced.

Estimating and solving a dynamic game that captures the types of rich horizontal and vertical differentiation that are features of the radio industry requires some form of approximation. This article’s approach is to approximate value functions using a parametric linear function of variables that reflect the current state of the industry. In the counterfactual, this method produces plausible results. In estimating the main parameters of the game, I combine this type of approximation with different estimation routines suggested in the literature, and I compare the resulting estimates with ones based on methods that approximate the value function by forward simulation. While these approaches do not produce identical estimates, many of them are similar and plausible (e.g., a cost of changing formats equal to somewhere between 30% and 60% of annual station revenues).<sup>55</sup> This is very encouraging, because many nontrivial choices are required to implement either approach (e.g., choice of a set of variables for approximating the value function or the choice of alternative policies), and the results should provide confidence that these methods can be used in settings where a large state space is required to capture the features that may affect an industry’s evolution.

<sup>55</sup>In practice, the computational burden of implementing these alternatives also needs to be considered. An advantage of the parametric approximation methods that I use is that almost the same code can be used to estimate and solve the model. On the other hand, the forward-simulation procedures can be implemented quite quickly on a large computational cluster. The choice also depends on the importance of having point estimates rather than bounds on the parameters. This will be application-specific.

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1801</page_number>

# REFERENCES

AGUIRREGABIRIA, V. (2012): “A Method for Implementing Counterfactual Experiments in Models With Multiple Equilibria,” *Economics Letters*, 114 (2), 190–194. [1796]

AGUIRREGABIRIA, V., AND C.-Y. HO (2012): “A Dynamic Oligopoly Game of the US Airline Industry: Estimation and Policy Experiments,” *Journal of Econometrics*, 168 (1), 156–173. [1796]

AGUIRREGABIRIA, V., AND P. MIRA (2007): “Sequential Estimation of Dynamic Discrete Games,” *Econometrica*, 75 (1), 1–53. [1764,1784]

——— (2010): “Stability of Equilibria and Properties of Sequential Estimators: Problems and Solutions,” Report, University of Toronto. [1764,1782]

AGUIRREGABIRIA, V., AND A. NEVO (2012): “Recent Developments in Empirical IO: Dynamic Demand and Dynamic Games,” Report, University of Toronto. [1782,1784]

ALBARRAN, A. B., AND W. L. PATRICK (2005): “Assessing Radio Station Value: A Review of Academic Literature and Analysis of Contemporary Industry Models,” *Journal of Radio Studies*, 12 (1), 3–13. [1775]

ALHO, J. M., AND B. D. SPENCER (2005): *Statistical Demography and Forecasting*. New York: Springer. [1778]

ARBITRON COMPANY (various years): *Radio Today*. New York: Arbitron Company. [1795]

ARCIDIACONO, P., P. J. BAYER, F. A. BUGNI, AND J. JAMES (2012): “Approximating High-Dimensional Dynamic Models: Sieve Value Function Iteration,” Report, Duke University. [1764]

BAJARI, P., L. BENKARD, AND J. LEVIN (2007): “Estimating Dynamic Models of Imperfect Competition,” *Econometrica*, 75 (5), 1331–1370. [1792]

BARWICK, P. J., AND P. PATHAK (2012): “The Cost of Free Entry: An Empirical Study of Real Estate Agents in Greater Boston,” Report, MIT. [1764,1782]

BENITEZ-SILVA, H., G. HALL, G. HITSCH, G. PAULETTO, AND J. RUST (2000): “A Comparison of Discrete and Parametric Approximation Methods for Continuous-State Dynamic Programming Problems,” Report, Yale University. [1764,1781]

BERRY, S. T., AND J. WALDFOGEL (2001): “Do Mergers Increase Product Variety? Evidence From Radio Broadcasting,” *Quarterly Journal of Economics*, 116 (3), 1009–1025. [1767]

BERRY, S. T., J. LEVINSOHN, AND A. PAKES (1993): “Applications and Limitations of Some Recent Advances in Empirical Industrial Organization: Price Indexes and the Analysis of Environmental Change,” *American Economic Review*, 83 (2), 240–246. [1763]

BERTSEKAS, D. (2010): “Approximate Policy Iteration: A Survey and Some New Methods,” Report 2833, Laboratory for Information and Decision Systems, MIT. [1764]

BERTSEKAS, D., AND S. IOFFE (1996): “Temporal Differences-Based Policy Iteration and Applications in Neuro-Dynamic Programming,” Report 2349, Laboratory for Information and Decision Systems, MIT. [1764]

BERTSEKAS, D., AND H. YU (2007): “Solution of Large Systems of Equations Using Approximate Dynamic Programming Methods,” Report 2754, Laboratory for Information and Decision Systems, MIT. [1764]

BIA FINANCIAL NETWORK, INC. (2006): *Media Access Pro User’s Manual*, version 3.0. Chantilly, VA: BIA Financial Network, Inc.

BORENSTEIN, S., AND J. S. NETZ (2002): “Maximum or Minimum Differentiation? Location Patterns of Retail Outlets,” *Review of Economics and Statistics*, 84 (1), 162–175. [1767]

DATTA, S., AND K. SUDHIR (2012): “The Agglomeration-Differentiation Tradeoff in Spatial Location Choice,” Report, Yale University. [1767]

DERTOUZOS, J. N. (2008): “Radio Airplay and the Record Industry: An Economic Analysis,” paper prepared for the National Association of Broadcasters, Washington, DC. [1797]

DORASZELSKI, U., AND M. SATTERTHWAITE (2010): “Computable Markov-Perfect Industry Dynamics,” *RAND Journal of Economics*, 41 (2), 215–243. [1779]

DRAGANASKA, M., M. MAZZEO, AND K. SEIM (2009): “Beyond Plain Vanilla: Modeling Joint Pricing and Product Assortment Choices,” *Quantitative Marketing and Economics*, 7 (2), 105–146. [1764]

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

1802 <page_number>1802</page_number> ANDREW SWEETING

EGESDAL, M., Z. LAI, AND C.-L. SU (2012): "Estimating Dynamic Discrete-Choice Games of Incomplete Information," Report, Harvard University. [1782]

ERICSON, R., AND A. PAKES (1995): "Markov Perfect Industry Dynamics: A Framework for Empirical Work," *Review of Economic Studies*, 62 (1), 53–82. [1779]

FAN, Y. (2012): "Ownership Consolidation and Product Characteristics: A Study of the U.S. Daily Newspaper Market," Report, University of Michigan. [1767]

FARIAS, V., D. SAURE, AND G. Y. WEINTRAUB (2012): "An Approximate Dynamic Programming Approach to Solving Dynamic Oligopoly Models," *RAND Journal of Economics*, 43 (2), 253–282. [1764]

FOWLIE, M., M. REGUANT, AND S. RYAN (2011): "Market-Based Emissions Regulation and the Evolution of Market Structure," Report, MIT. [1764]

GEORGE, L., AND J. WALDFOGEL (2003): "Who Affects Whom in Daily Newspaper Markets?" *Journal of Political Economy*, 111 (4), 765–784. [1767]

HENDEL, I., AND A. NEVO (2006): "Measuring the Implications of Sales and Consumer Inventory Behavior," *Econometrica*, 74 (6), 1637–1673. [1764]

JENKINS, M., P. LIU, R. L. MATZKIN, AND D. L. MCFADDEN (2004): "The Browser War: Econometric Analysis of Markov Perfect Equilibrium in Markets With Network Effects," Report, Stanford University. [1779]

JEZIORSKI, P. (2013): "Estimation of Cost Efficiencies From Mergers: An Application to US Radio," Report, University of California at Berkeley. [1767,1774,1791,1792,1794]

JUDD, K. L. (1998): *Numerical Methods in Economics*. Cambridge, MA: MIT Press. [1780]

MA, J., AND W. B. POWELL (2009): "Convergence Proofs of Least Squares Policy Iteration Algorithm for High-Dimensional Infinite Horizon Markov Decision Process Problems," Report, Princeton University. [1764]

MOONEY, C. T. (2010a): "Cost Sharing and Strategic Programming Choices in Broadcast Radio," Report, University of Oklahoma. [1767]

——— (2010b): "A Two-Sided Market Analysis of Radio Ownership Caps," Report, University of Oklahoma. [1767]

NEVO, A. (2001): "Measuring Market Power in the Ready-to-Eat Cereal Industry," *Econometrica*, 69 (2), 307–342. [1775]

NISHIDA, M. (2012): "Estimating a Model of Strategic Network Choice: The Convenience-Store Industry in Okinawa," Report, Johns Hopkins University. [1767]

O'GORMAN, C., AND H. SMITH (2008): "Efficiency Gain From Ownership Deregulation: Estimates for the Radio Industry," Report, University of Oxford. [1767,1791]

PAKES, A., M. OSTROVSKY, AND S. T. BERRY (2007): "Simple Estimators for the Parameters of Discrete Dynamic Games (With Entry/Exit Examples)," *RAND Journal of Economics*, 38 (2), 373–399. [1764]

PAKES, A., J. PORTER, K. HO, AND J. ISHII (2011): "Moment Inequalities and Their Application," Report, Harvard University. [1764]

PESENDORFER, M., AND P. SCHMIDT-DENGLER (2008): "Asymptotic Least Squares Estimators for Dynamic Games," *Review of Economic Studies*, 75 (3), 901–928. [1784]

——— (2010): "Sequential Estimation of Dynamic Discrete Games: A Comment," *Econometrica*, 78 (2), 833–842. [1784]

PETRIN, A. (2002): "Quantifying the Benefits of New Products: The Case of the Minivan," *Journal of Political Economy*, 110 (4), 705–729. [1784]

ROBERTS, J. W., AND A. SWEETING (2012): "The Potential Entry Defense in Airline Mergers," Report, Duke University. [1787]

RUST, J. (2000): "Parametric Policy Iteration: An Efficient Algorithm for Solving Multidimensional DP Problems?" Report, Yale University. [1780]

RYAN, S. P. (2012): "The Costs of Environmental Regulation in a Concentrated Industry," *Econometrica*, 80 (3), 1019–1061. [1792]

SEIM, K. (2006): "An Empirical Model of Firm Entry With Endogenous Product-Type Choices," *RAND Journal of Economics*, 37 (3), 619–640. [1777]

14680262, 2013, 5, Downloaded from https://onlinelibrary.wiley.com/doi/10.3982/ECTA7473 by Univ of California Lawrence Berkeley National Lab, Wiley Online Library on [17/06/2026]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library for rules of use; OA articles are governed by the applicable Creative Commons License

DYNAMIC PRODUCT POSITIONING <page_number>1803</page_number>

SNIDER, C. (2009): “Predatory Incentives and Predation Policy: The American Airlines Case,” Report, University of California at Los Angeles. [1792]

SPECK, P. S., AND M. T. ELLIOTT (1997): “Predictors of Advertising Avoidance in Print and Broadcast Media,” *Journal of Advertising*, 26 (3), 61–76. [1788]

SU, C.-L. (2012): “Estimating Discrete-Choice Games of Incomplete Information: A Simple Static Example,” Report, University of Chicago. [1782]

SWEETING, A. T. (2010): “The Effects of Mergers on Product Positioning: Evidence From the Music Radio Industry,” *RAND Journal of Economics*, 41 (2), 372–397. [1767]

——— (2011): “Dynamic Product Positioning in Differentiated Product Markets: The Effect of Fees for Musical Performance Rights on the Commercial Radio Industry,” Report, Duke University. [1764,1770]

——— (2013): “Supplement to ‘Dynamic Product Positioning in Differentiated Product Markets: The Effect of Fees for Musical Performance Rights on the Commercial Radio Industry’,” *Econometrica Supplemental Material*, 81, http://www.econometricsociety.org/ecta/supmat/7473_miscellaneous.pdf; http://www.econometricsociety.org/ecta/supmat/7473_data_and_programs.zip. [1767]

UNITED STATES GOVERNMENT ACCOUNTABILITY OFFICE (2010): “The Proposed Performance Rights Act Would Result in Additional Costs for Broadcast Radio Stations and Additional Revenue for Record Companies, Musicians, and Performers,” GAO-10-826, August 2010. [1765,1766,1794]

WATSON, R. (2009): “Product Variety and Competition in the Retail Market for Eyeglasses,” *Journal of Industrial Economics*, 57 (2), 217–251. [1767]

*Dept. of Economics, University of Maryland, College Park, MD 20742, U.S.A., Duke University, and NBER; sweeting@econ.umd.edu.*

*Manuscript received October, 2007; final revision received July, 2012.*