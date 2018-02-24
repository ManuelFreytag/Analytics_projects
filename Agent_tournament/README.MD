# DMC 2017
This repository contains an R script to simulate an tournament of multple Agents within a tournament for an iterative prisoners dilema.

# The Idea
This video explains the concept of the iterative prisoners dilema nicely.

[i[Repeated Prisoners Dilema](https://img.youtube.com/vi/Elqs5xDu6ZI/0.jpg)]
https://www.youtube.com/watch?v=Elqs5xDu6ZI

There is a multitude of approaches possible for these kinds of problems.

This video explains some of the basic techniques and strategies of the iterative prisoners dilema:
https://www.youtube.com/watch?v=BOvAbjfJ0x0

Some are totally observable for pattern recognition tools with given information as they are purely based on logic.
Others incorporate randomness and thus incorporate a default error.
As seen in the video, previous approaches the Tit for Tat strategy proved to be a good approximation in a mixed strategy environment.
I want to observe if a machine learning approach might be more robust.

# The ML Agent
In this setting a typical problem is a "cold start".
This means that the ML agent does not have sufficient information in the beginning of the tournament.
Therefore, it starts the first 10 rounds with an optimistic tit for tat strategy.

Our agent considers the bids of the opponent agent versus our agent and versus its previous opponents.
This is necessary as 10 rounds might not be enougth to sufficiently ensure that the ML agent had a previous encounter with all other agents.
In this case the tree learner would have no data set for the interaction between us and him.
Thus, he considers the behaviour of the opponent agent by looking in the playbook.

In particular, our agent takes into account:
1. The last bids of the opponent agent 1 round ago
2. The last bids of the opponent agent 2 round ago
3. The last bids of the opponent agent versus our agent 1 round ago
4. The last bids of the opponent agent versus our agent 2 round ago
5. General percentage of “defect” bids

# The tournament
For a proof and to improve of our agent concept we elaborated the tournament structure and implemented multiple rational agent concepts (see attached .R File).
Our self-made tournament consists of 10 agents.
* 3 of them follow the titfortat strategy.
* 3 of them focus on the frequency strategy
* 2 of them is playing random
* 2 of them is our machine learning agent

# The script
The script contains three main parts:
- The participating agents as R6Class
- A tournament structure that allows the participation of multiple agents
- An evaluation part that provides feedback for the Agents and winning strategies


# It is dependent on four libraries:
1. R6: New lean implementation for object oriented programming in R
2. mlr: Uniform maschine learning framework incorporating multiple other ML packages and allowing for one syntax
3. rpart ML library incormporating multiple algorithms. We are making use of the implemented tree learner.
4. plyr: Toolbox for data preprocessing. Here only used for the lean renaming functionality of columns.

# Conclusion
This script does not proof superiourity to the tit for tat agent.
However, it the strategy seems to be at least equaly effective.
The limited diversity of strategies limits conclusions.
Additionally, the implementation of the strategy is not computationally efficient (recalculation of the whole dataframe)
Therefore, a run comparison for a large number of iterations is difficult.
