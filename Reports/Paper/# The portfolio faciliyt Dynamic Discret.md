# The portfolio faciliyt Dynamic Discrete Choice Model:

## Model Goals:
-- We want to run the following counterfactuals 
1. What would texas had look like it it stayed a flat fee state?
2. What does the first best premium look like (intenralizeing the externality that even insurere miss)
The next two polices are to think about the other policy tools being used do:
3. What does subsidizing the replacement of SW tanks do/look like?
4. How does mandated removeal by Age A look like?

We want to see the welfare effects of these policies and how they compare to the current policy environment.

Here is what i had thought the model should look like given current ccp and firm behabior in the data.
Note when we can we shold motivate the model choices.

## Model Setup:
### State Space:
- The state space should include the following variables:
A facily will track its total tanks counts by wall type and age. This is to ensure premiums and hazard varition are captured in the model.
1. Tank age type counts ( a vector of counts for each age bin and wall type)
1.a Age of the tank ( discrete bins to match premium schedules)
1.b Number of tank wall type ( SW or DW)
The state space then is Age x Wall type vector of counts.
2. Discretized total facility gallans ( tank counts are important for risk but dont capture the scale differnce total capacilty does)
We need to descriteze this i need to see the distrtion of total capacity in the data. I think it fine to have 4 or 5 bins here. Maybe 3 bins for small, medium, and large facilities. Empirical question
3. Insurance regme - (Flat fee (FF), or risk based (RB))

Formally the state space can be represented as:

$$
 S = \{(A x W, G, R) \} 
 $$

Where:
- $A$ is the vector of tank age type counts
- $W$ is the vector of tank wall type counts
- $G$ is the vector of discretized total facility gallons
- $R$ is the insurance regime (Flat fee (FF) or risk based (RB))

So a facility is then described by what age and type of tanks it has, how much total capacity it has, and what insurance regime it is in.
note we need to make sure the agent thinks about SWtanks that are ABC age etc. 

### Action Space:
This is where i am still trying to expand the model. Currnlyt firms only matinin, replae (but stricly SW to DW or exit permanlty)
This model actions space needs to be refeind:
1) the intensive margion spae needs to get more granular. I think we can have the following actions:
-) Downsize (remove  one of the tanks in the portfolio not all and keep operating)
-) replace/upgrade (replace one of the tanks in the portfolio with a new tank of the same type or upgrade a SW tank to a DW tank)
2) exit (permanently close the facility)
3) maintain (keep the same portfolio of tanks and continue operating)

My one challange that i dont undersantd if i can do it in the DDCM is on the tank closing action if i need to seperate those actions into two distint actiosn or can i put the choice to close a tank and then condition the followup action : dont replace with a new tank or replace with a new tank.

The question i want you to help me think is can we colpase downsize and rplace into one action that is kind of conditiaonl like close a tank and condiatl on that repalce or not? Or is it better to have them as two distinct actions? I am not sure if the DDCM framework can handle this kind of conditional action or if it would be better to have them as separate actions.
