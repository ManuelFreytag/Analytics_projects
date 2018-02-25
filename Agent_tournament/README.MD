# Agent tournament
## The Idea
The goal of this project is to determine if a machine learning agent might be a valid approach for the iterative prisoners dilema.
This video explains the concept of the iterative prisoners dilema nicely.

[![Repeated Prisoners Dilema](https://img.youtube.com/vi/Elqs5xDu6ZI/0.jpg)](https://www.youtube.com/watch?v=Elqs5xDu6ZI "Iterative prisoners dilema")

This video explains some of the basic techniques and strategies within the iterative prisoners dilema.
[![Basic strategies](https://img.youtube.com/vi/BOvAbjfJ0x0/0.jpg)](https://www.youtube.com/watch?v=BOvAbjfJ0x0 "Basic strategies")

As some strategies are purely based on logic, they are totally observable for pattern recognition tools if the betting schema is publicly available. Others incorporate randomness and thus a certain bayes error > 0.
As seen in the video, the tit-for-tat strategy proved to be a good approximation in a mixed strategy environment.
I want to observe if a machine learning approach might be more robust.

## The ML Agent
Before each iteration, the machine learning algorithm builds a new dataframe with the following information of his opponent:
1. The last bids of the opponent agent 1 round ago
2. The last bids of the opponent agent 2 round ago
3. The last bids of the opponent agent versus our agent 1 round ago
4. The last bids of the opponent agent versus our agent 2 round ago
5. General percentage of “defect” bids

I selected a simple tree learner as the strategies of the opponent agents are not sufficticated.
A tree learner is more than capable of recognizing all rational strategies in our environment.

In this setting a typical problem is a "cold start".
This means that the ML agent does not have sufficient information in the beginning of the tournament.
Therefore, it starts the first 10 rounds with an optimistic tit for tat strategy.

The implementation of the strategy is not computationally efficient (recalculation of the whole dataframe)
Building a playbook for each agent and iteratively enhancing it might have been more sensible.
Therefore, a run comparison for a large number of iterations is difficult.

## The tournament
The tournament is already explained in the video.
Here, the agents compete in 1000 iteration against each other. The pairings for each iterations differ and are chosen randomly.

Our self-made tournament consists of 10 agents.
* 3 of them follow the titfortat strategy.
* 3 of them focus on the frequency strategy
* 2 of them is playing random
* 2 of them is our machine learning agent

## The script
The script contains three main parts:
- The participating agents as R6Class
- A tournament structure that allows the participation of multiple agents
- An evaluation part that provides feedback for the Agents and winning strategies

It is dependent on four libraries:
1. R6: New lean implementation for object oriented programming in R
2. mlr: Uniform maschine learning framework incorporating multiple other ML packages and allowing for one syntax
3. rpart ML library incormporating multiple algorithms. We are making use of the implemented tree learner.
4. plyr: Toolbox for data preprocessing. Here only used for the lean renaming functionality of columns.

## Conclusion
This script does not proof superiourity to the tit for tat agent.
However, it the strategy seems to be at least equaly effective.
The limited diversity of strategies limits conclusions.
