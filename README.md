# Motor flexibility, not laterality, predicts paw preference on lateralized lever task in California ground squirrels
Code for analyzing paw preferences on a lateralized lever task in California ground squirrels


## Data:
- combined.data.csv: contains data on solving behavior of squirrels
  columns:
  - subject: unique identifier for each squirrel
  - lever_side: left lever or right lever used for solving
  - body_part: whether it used the right paw, left paw or both paws
  - reaction: behavior following a solve - either exit, no reaction, flinch or alert
  - cumulative_count: cumulative number of solves specific to squirrels (proxy for experience)
  - colony: site within the whole population where puzzle box was placed (not used for analysis here)
  - sex: F for female, M for male
  - age: A for adult, P for juvenile (pup)
  - body_part_red: left and right paw is collapsed into a single category (one paw) versus both paws
 
- num.vistits.csv: contains the number of visits to the puzzle box for each squirrel and the number of times it was trapped during the field season
- trap.behav.csv: contains data on individual squirrels' behavior in the trap upon approach of a human observer:
  columns:
  - beh_chat: whether it chattered (1/0)
  - beh_call: whether it called (1/0)
  - beh_strg: whether it struggled (1/0)
  - subject: unique identifier for each squirrel
 
## Figures output:
contains all figures generated with the R code

## model output:
contains the R Data objects of the two statistical models

## CODE: Behav laterality and cognitive performance
Code to replicate all analyses and figures
