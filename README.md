# Welcome!

## About the App
This Shiny app allows users to complete the Facets of Altruistic Behaviors (FAB) Scale (Windmann et al., 2021), a psychometric scale assessing altruistic behavioral tendencies across three subscales. The app is available in German and English, with slightly different item selections per language reflecting validated subscale structures for each cultural context.

## Subscales:
🇩🇪 German version: Help Giving (HG), Peer Punishment (PP), Moral Courage (MC)  
🇬🇧 English version: Costly Rewarding (CR), Costly Punishment (CP), Costly Countercontrol (CC)

## Norm data: 
Based on N = 5,806 participants (3,757 German-speaking, 2,049 English-speaking).

## App flow:
1) Select language (German/English)
2) Optionally provide demographic information (age, gender, education) to enable a matched comparison group
3) Answer 15 items on a 6-point rating scale (items are presented in randomized order)
4) View results as percentile ranks visualized as gauge charts — one per subscale


## Comparison Group Filtering (3 levels)
Same gender + age ±10 years + same education level (min. 20 persons)  
Same gender + age ±15 years (min. 20 persons)   
All participants of the same language group  

If no demographic information is provided, the full language-matched sample is used as the reference group.  

## Percentile Rank
percentile = mean(ref_scores <= user_score) * 100
A percentile rank of e.g. 75 means the user scores higher than 75% of the comparison group.


-------
Windmann, S., Binder, L., & Schultze, M. (2021). Constructing the Facets of Altruistic Behaviors (FAB) Scale. Social Psychology, 52(5), 299–313. https://doi.org/10.1027/1864-9335/a000460
