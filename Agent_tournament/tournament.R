library(R6)
library(mlr)
library(plyr)
library(rpart)

#
Agent = R6Class("Agent",
                
                public = list(
                  name = NULL,
                  bid = NULL,
                  matchups = NULL,
                  
                  #standart initialization is missing
                  initialize = function(name=NA){
                    self$name = name
                    self$matchups = data.frame("id1" = character(), "id2" = character())
                  },
                  get_bid = function(){
                    bid_vector = c("cooperate", "defect")
                    self$bid = sample(bid_vector,1)
                  }
                )
)

AgentFreq = R6Class("Agent",
                    
                    public = list(
                      name = NULL,
                      bid = NULL,
                      matchups = NULL,
                      
                      initialize = function(name=NA){
                        self$name = "Freq"
                        self$matchups = data.frame("id1" = character(), "id2" = character())
                      },
                      
                      actions_of_id = function(id){
                        dec1_1 = subset(book[book$id1 == id,], select = c("tradeno","bid1"))
                        dec1_2 = subset(book[book$id2 == id,], select = c("tradeno","bid2"))
                        dec1_1 = rename(dec1_1, c("bid1"="bid"))
                        dec1_2 = rename(dec1_2, c("bid2"="bid"))
                        dec = rbind(dec1_1,dec1_2)
                        return(dec)
                      },
                      
                      get_bid = function(){
                        opID = id1
                        #filter both ids out
                        dec = self$actions_of_id(id1)
                        dec2 = self$actions_of_id(id2)
                        
                        
                        self$matchups = rbind(self$matchups, data.frame("id1" = id1, "id2" = id2))
                        
                        if(nrow(self$matchups[self$matchups["id1"] == id1 | self$matchups["id2"] == id1, ]) == nrow(self$matchups)){
                          dec = dec2
                          opID = id2
                        }
                        
                        #decide based on the frequency of each decision of the opponent
                        if (nrow(dec) == 0) {
                          bid_vector = c("cooperate","defect")
                          self$bid = sample(bid_vector,1)
                        } else {
                          self$bid = sample(as.character(dec[["bid"]]), 1)
                        }
                      }
                    )
)

AgentTitForTat = R6Class("AgentTitForTat",
                         
                         public = list(
                           name = NULL,
                           bid = NULL,
                           matchups = NULL,
                           
                           initialize = function(name=NA){
                             self$name = "TitForTat"
                             self$matchups = data.frame("id1" = character(), "id2" = character())
                           },
                           
                           getPrevInteractionFlex = function(interaction.df, opID, gap){
                             prevOpInteraction = NA
                             if(nrow(interaction.df)>(gap-1)){
                               values = interaction.df[nrow(interaction.df)-(gap-1),]
                               if(values[["id1"]] == opID){
                                 prevOpInteraction = values[["bid2"]]
                               } else {
                                 prevOpInteraction = values[["bid2"]]
                               }
                             }
                             return(prevOpInteraction)
                           },
                           
                           get_bid = function(){
                             #get the tit for tat
                             opID = id1
                             self$matchups = rbind(self$matchups, data.frame("id1" = id1, "id2" = id2))
                             
                             if(nrow(self$matchups[self$matchups["id1"] == id1 | self$matchups["id2"] == id1, ]) == nrow(self$matchups)){
                               opID = id2
                             }
                             
                             #get the past decision of our opponent
                             interaction.df = book[(book[["id1"]] == id1 | book[["id1"]] == id2)& (book[["id2"]] == id1 | book[["id2"]] == id2),]
                             
                             if(nrow(interaction.df) > 0){
                               self$bid = as.character(self$getPrevInteractionFlex(interaction.df, opID, 1))
                             } else {
                               self$bid = "cooperate"
                             }
                           }
                         )
)

AgentImp = R6Class("AgentImp",
                   
                   public = list(
                     name = NULL,
                     bid = NULL,
                     matchups = NULL,
                     
                     
                     initialize = function(name="cheese-stick"){
                       #Used to create a new agent instance
                       self$name = name
                       self$matchups = data.frame("id1" = character(), "id2" = character())
                     },
                     
                     
                     #learn from recent pattern not only total sum of things
                     actions_of_id = function(id){
                       #This function returns all previous decisions of an id as an vector
                       #
                       #@param id         agent id of which the bid vector should be returned
                       
                       dec1_1 = subset(book[book$id1 == id,], select = c("tradeno","bid1"))
                       dec1_2 = subset(book[book$id2 == id,], select = c("tradeno","bid2"))
                       dec1_1 = rename(dec1_1, c("bid1"="bid"))
                       dec1_2 = rename(dec1_2, c("bid2"="bid"))
                       dec = rbind(dec1_1,dec1_2)
                       return(dec)
                     },
                     
                     relDefectFrequency = function(vector){
                       #This function is used to determine the relative frequency of "defect" in one vector.
                       #If we have no occurance we return 0.
                       #
                       #@param vector         Vector consisting of "defects" and "cooperates"
                       
                       freq.df = count(vector)
                       freq.defect = freq.df[freq.df[1] == "defect",][["freq"]]
                       freq.all = sum(freq.df["freq"])
                       
                       if(identical(freq.defect, integer(0))){
                         return(0)
                       }
                       return(freq.defect/freq.all)
                     },
                     
                     preceedingDecision = function(id,gap){
                       #Get the previous bid of the agent (gap) round ago
                       #
                       #@param id             ID of agent in question
                       #@param gap            gap between the current interaction and the return interaction
                       
                       prevOpAction.df = book[book[["id1"]] == id | book[["id2"]] == id,]
                       if(prevOpAction.df[nrow(prevOpAction.df)-gap+1,][["id1"]] == id){
                         prevOpAction = prevOpAction.df[nrow(prevOpAction.df)-gap+1,"bid1"]
                       } else {
                         prevOpAction = prevOpAction.df[nrow(prevOpAction.df)-gap+1,"bid2"]
                       }
                       return(prevOpAction)
                     },
                     
                     getPrevInteractionFlex = function(interaction.df, opID, gap){
                       #Get the previous bid of the opponent in an direct interaction (gap) rounds ago.
                       #
                       #@param interaction.df
                       #@param opID           ID of the current opponent
                       #@param gap            gap between the current interaction and the return interaction
                       
                       prevOpInteraction = NA
                       if(nrow(interaction.df)>(gap-1)){
                         values = interaction.df[nrow(interaction.df)-(gap-1),]
                         if(values[["id1"]] == opID){
                           prevOpInteraction = values[["bid2"]]
                         } else {
                           prevOpInteraction = values[["bid2"]]
                         }
                       }
                       return(prevOpInteraction)
                     },
                     
                     past_opponent_actions_flex = function(id,gap){
                       #Get a vector describing all previous actions (gap) rounds ago for all previous bids.
                       #
                       #@param id             ID of agent in question
                       #@param gap            gap between the current interaction and the return interaction
                       
                       #Filter the actions of the current opponent
                       tmpDf = book[book["id1"]==id | book["id2"] == id,]
                       tradeno = c()
                       prevOpAction = c()
                       #track all filtered ids
                       oppIDs = unique(c(tmpDf[["id1"]],tmpDf[["id2"]]))
                       #remove the own
                       oppIDs = oppIDs[oppIDs != id]
                       
                       for (x in oppIDs) {
                         #Get the actions of the opponent of our current opponent
                         opActions = book[book["id1"]==x | book["id2"] == x,]
                         #Get all previous opponent decitions
                         gapTid = rep(NA, gap)
                         
                         for(i in 1:nrow(opActions)) {
                           pTid = gapTid[1]
                           #Check if the current row is an interaction with our current opponent, if so save the line before
                           if((opActions[i,][["id1"]] == id) | (opActions[i,][["id2"]] == id)){
                             #Save tradenumber for tracing
                             tradeno = c(tradeno,opActions[i,][["tradeno"]])
                             pDec = NA
                             
                             row = opActions[opActions["tradeno"]==pTid,]
                             if(nrow(row) == 1){
                               if(row[["id1"]] == x){
                                 pDec = as.character(row[["bid1"]])
                               } else {
                                 pDec = as.character(row[["bid2"]])
                               }
                             }
                             prevOpAction = c(prevOpAction, pDec)
                           }
                           #Generate new stack structure. The length depends on the given gap
                           if (gap != 1) {
                             gapTid = c(gapTid[2:length(gapTid)],opActions[i,][["tradeno"]])
                           } else {
                             gapTid = c(opActions[i,][["tradeno"]])
                           }
                         }
                       }
                       #save the results in a dataframe and name the columns uniquely
                       ret = data.frame("tradeno" = integer(),"prevOpAction" = character())
                       ret = rbind(ret, data.frame(tradeno, prevOpAction))
                       colnames(ret) = c("tradeno", paste("prevOpAction",gap,sep=""))
                       return(ret)
                     },
                     
                     past_direct_decisions_flex = function(id,gap){
                       #Get the the (gap)th previous bid of the agent in an direct comparison with his other opponent
                       #
                       #@param id             ID of agent in question
                       #@param gap            gap between the current interaction and the return interaction
                       
                       #Filter the actions of the current opponent.
                       tmpDf = book[book["id1"]==id | book["id2"] == id,]
                       tradeno = c()
                       prevOpAction = c()
                       
                       #track all filtered ids
                       oppIDs = unique(c(tmpDf[["id1"]],tmpDf[["id2"]]))
                       #remove the own
                       oppIDs = oppIDs[oppIDs != id]
                       for (x in oppIDs) {
                         
                         #Get all interactions of our opponent and his current opponent
                         interactions = tmpDf[tmpDf["id1"]==x | tmpDf["id2"] == x,]
                         #Get all previous opponent decitions
                         gapTid = rep(NA, gap)
                         for(i in 1:nrow(interactions)){
                           pTid = gapTid[1]
                           if(is.na(pTid) == FALSE){
                             #save tradenumber for later tracing
                             tradeno = c(tradeno, interactions[i,][["tradeno"]])
                             pDec = NA
                             row = interactions[interactions["tradeno"]==pTid,]
                             
                             if(row[["id1"]] == x){
                               pDec = as.character(row[["bid1"]])
                             } else {
                               pDec = as.character(row[["bid2"]])
                             }
                             prevOpAction = c(prevOpAction, pDec)
                           }
                           #Generate new stack structure. The length depends on the given gap
                           if (gap != 1) {
                             gapTid = c(gapTid[2:length(gapTid)],interactions[i,][["tradeno"]])
                           } else {
                             gapTid = c(interactions[i,][["tradeno"]])
                           }
                         }
                       }
                       
                       #save the results in a dataframe and name the columns uniquely
                       ret = data.frame("tradeno" = integer(),"prevOpInteraction" = character())
                       ret = rbind(ret, data.frame(tradeno, prevOpAction))
                       colnames(ret) = c("tradeno", paste("prevOpInteraction",gap,sep=""))
                       return(ret)
                     },
                     
                     past_opponent_probs = function(id){
                       #Get the previous tendency of deflect of the agent with id = id at the time of each round.
                       #
                       #@param id             ID of agent in question
                       
                       #Filter the actions of the current opponent
                       tmpDf = book[book["id1"]==id | book["id2"] == id,]
                       tradeno = c()
                       prevOpTrend = c()
                       
                       #track all filtered ids
                       oppIDs = unique(c(tmpDf[["id1"]],tmpDf[["id2"]]))
                       #remove the own
                       oppIDs = oppIDs[oppIDs != id]
                       
                       for(x in oppIDs){
                         #get all relevant tradenumbers
                         tmp.dat = book[book["id1"] == x | book["id2"] == x,]
                         rel.tradeno = tmp.dat[tmp.dat["id1"] == id | tmp.dat["id2"] == id,][["tradeno"]]
                         
                         for(y in rel.tradeno){
                           #get decisions before the relevant tradenumber
                           rel.dec = tmp.dat[tmp.dat["tradeno"] < y,]
                           
                           if(nrow(rel.dec) > 0){
                             dec1 = as.character(rel.dec[rel.dec["id1"] == x,][["bid1"]])
                             dec2 = as.character(rel.dec[rel.dec["id2"] == x,][["bid2"]])
                             dec = c(dec1,dec2)
                             #save the results
                             
                             rel.freq = self$relDefectFrequency(dec)
                             
                             if(identical(rel.freq, numeric(0))){
                               prevOpTrend = c(prevOpTrend, 0)
                               tradeno = c(tradeno, y)
                             } else {
                               prevOpTrend = c(prevOpTrend, self$relDefectFrequency(dec))
                               tradeno = c(tradeno, y)
                             }
                           }
                         }
                       }
                       ret = data.frame("tradeno" = integer(), "prevOpTrend" = double())
                       ret = rbind(ret, data.frame(tradeno, prevOpTrend))
                       return(ret)
                     },
                     
                     get_bid = function(){
                       #final bid function creating a bid according to the opponents past decisions
                       
                       #1)  Check what id our agent is (as the agent objects are not initialized with this information)
                       opID = id1
                       ownID = id2
                       train.df = self$actions_of_id(id1)
                       train2.df = self$actions_of_id(id2)
                       
                       self$matchups = rbind(self$matchups, data.frame("id1" = id1, "id2" = id2))
                       
                       if(nrow(self$matchups[self$matchups["id1"] == id1 | self$matchups["id2"] == id1, ]) == nrow(self$matchups)){
                         train.df = train2.df
                         opID = id2
                         ownID = id1
                       }
                       
                       #2)  Check if our agent played at least 10 rounds. If not result in a simple optimistic Tit for Tat strategy
                       if(nrow(self$matchups) < 10){
                         #get the past decision of our opponent
                         interaction.df = book[(book[["id1"]] == id1 | book[["id1"]] == id2)& (book[["id2"]] == id1 | book[["id2"]] == id2),]
                         
                         if(nrow(interaction.df) > 0){
                           self$bid = as.character(self$getPrevInteractionFlex(interaction.df, opID, 1))
                         } else {
                           self$bid = "cooperate"
                         }
                         return()
                       }
                       #3)    If the Knowledgebase of our agent is sufficiently large (>10 rounds) we result to the predictive strategy
                       #3.1)  Create the new dataframe with label and descriptive attributes
                       #Build new dataframe, bid is the label
                       #get all past actions of the opponent id one round before
                       tmp.df = na.omit(self$past_opponent_actions_flex(opID,1))
                       #get all past actions of the opponent id two rounds before
                       tmp.df = merge(tmp.df,na.omit(self$past_opponent_actions_flex(opID,2)),by="tradeno", all = TRUE)
                       #get all the previous decision in the exact interactions
                       tmp.df = merge(tmp.df,self$past_direct_decisions_flex(opID,1),by="tradeno", all = TRUE)
                       #get all the previous decisions 2 rounds before
                       tmp.df = merge(tmp.df,self$past_direct_decisions_flex(opID,2),by="tradeno", all = TRUE)
                       #get all posterior probabilities for a label of the opponents opponent
                       tmp.df = merge(tmp.df, self$past_opponent_probs(opID), by="tradeno", all = TRUE)
                       #Join all the entries to the past decisions of the opponent
                       train.df = merge(train.df,tmp.df,by="tradeno")
                       
                       #3.2) If our opponent decided for one bid strategy only return the same
                       if(length(unique(train.df[["bid"]])) == 1){
                         #Check if the decision is uniform (empty factor level), if so always choose this!
                         self$bid = as.character(train.df[["bid"]][1])
                       } else {
                         #3.3) If the opponent played at least once "defect" and "cooperate" then try to find structure in it
                         #Get all the necessary information about our client that our opponent could know
                         interaction.df = book[(book["id1"] == id1 | book["id2"] == id1) & (book["id1"] == id2 | book["id2"] == id2),]
                         pred.df = data.frame(tradeno = as.integer(nrow(train.df)+1),
                                              prevOpAction1 = self$preceedingDecision(ownID, 1),
                                              prevOpAction2 = self$preceedingDecision(ownID, 2),
                                              prevOpTrend = self$relDefectFrequency(self$actions_of_id(ownID)[["bid"]]),
                                              prevOpInteraction1 = self$getPrevInteractionFlex(interaction.df, opID, 1),
                                              prevOpInteraction2 = self$getPrevInteractionFlex(interaction.df, opID, 2),
                                              bid = "defect" #this is only a placeholder
                         )
                         
                         #Combine both training and prediction data to remove the possibility of unknown prediction occurances
                         df = rbind(train.df, pred.df)
                         
                         #Use the mlr package
                         task = makeClassifTask(id = "tradeno", data = df, target = "bid")
                         lrn = makeLearner("classif.rpart")
                         model = train(lrn, task, subset = c(1:(nrow(df)-1)))
                         
                         prediction = predict(model, task = task, subset = c(nrow(df)))
                         self$bid = as.character(prediction$data[["response"]])
                       }
                     }
                   )
)

# TESTING
# Initialize the run
agent_list = c()

#Make the agents and add to a list to sample
for (i in 1:2) {
  new_agent = Agent$new()
  agent_list[[i]] = new_agent
}

for (i in 3:5) {
  new_agent = AgentFreq$new()
  agent_list[[i]] = new_agent
}

for (i in 6:8) {
  new_agent = AgentTitForTat$new()
  agent_list[[i]] = new_agent
}

for (i in 9:10) {
  new_agent = AgentImp$new()
  agent_list[[i]] = new_agent
}


id_list = c()
book = data.frame("id1"=integer(),"id2"=integer(),"tradeno"=integer(),"bid1"=character(),"bid2"=character())

# each agent gets to play 100 times
contestants = c()
for (i in 1:100) {
  
  id_list=1:10
  shuffle = sample(id_list)
  
  while (length(shuffle) > 0) {
    # shuffle the list in place
    
    el1 = shuffle[[1]]
    shuffle = shuffle[-1]
    el2 = shuffle[[1]]
    shuffle = shuffle[-1]
    
    contest = c(el1,el2)
    contestants[[length(contestants)+1]] = contest
    
  }
}


# Create the payoff table
x = c("cooperate","cooperate","defect","defect")
y = c("cooperate","defect","cooperate","defect")
z = c(1,-2,2,-1)
w = c(1,2,-2,-1)
df = data.frame("bid1"=x,"bid2"=y, payoff1 = z, payoff2 = w)

# Run the Tournament
x = length(contestants)
# Contestants is a list of list
# The outer list is a list of contests
# The inner list is a list of two agents
for (i in 1:x) {
  # find the first contest
  # get the list, not the slice
  contest = contestants[[i]]
  
  # get the ideas
  id1 = contest[[1]]
  id2 = contest[[2]]
  
  # get the agents
  agent1 = agent_list[[id1]]
  agent2 = agent_list[[id2]]
  
  # get the bids
  agent1$get_bid()
  agent2$get_bid()
  
  mybid1 = agent1$bid
  mybid2 = agent2$bid
  
  # find the payoffs
  payoffs = df[df["bid1"] == mybid1 & df["bid2"] == mybid2,]
  #payoffs =subset(df, bid1 == mybid1 & bid2 == mybid2)
  payoff1 = payoffs$payoff1
  payoff2 = payoffs$payoff2
  
  # record the transaction
  book = rbind(book,data.frame("id1"=id1,"id2"=id2,"tradeno"= i,"bid1"=mybid1,"bid2"=mybid2,"payoff1" =payoff1, "payoff2"=payoff2))
}

#get sum of all ids as a seperate value
maxsum=1
minsum=1
for(i in id_list){
  dec1_1 = book[book$id1 == i,][["payoff1"]]
  dec1_2 = book[book$id2 == i,][["payoff2"]]
  dec = c(dec1_1,dec1_2)
  
  y = c(y, sum(dec))
  
  # determine y-axis
  if(sum(dec)>maxsum){
    maxsum <- sum(dec)
  }
  if(sum(dec)<minsum){
    minsum <- sum(dec)
  }  
  
  #Histogram of Tournament
  #make dynamic variables
  nam <- paste("AG",i, sep="")
  assign(nam, 1:10)
  
  #make cumulate vector (start=0)
  dec = c(0, dec)
  assign(nam, cumsum(dec))
}


#=== Plots ===

#== 1. global Results sort by Agent Id
#barplot(y,names.arg = as.character(x))

#== 2. cumulated results during Tournament

norounds <- length(AG1)-1
i=c(0:norounds)
plot(i,AG1,ylim=c(minsum,maxsum),type="l",col="orange",xlab = "Runden",ylab = "Agenten")
lines(i,AG2,col="orange")
lines(i,AG3,col="blue")
lines(i,AG4,col="blue")
lines(i,AG5,col="blue")
lines(i,AG6,col="green")
lines(i,AG7,col="green")
lines(i,AG8,col="green")
lines(i,AG9,col="red", lwd=5)
lines(i,AG10,col="red", lwd=5)

#===