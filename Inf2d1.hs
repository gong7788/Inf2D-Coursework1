-- Inf2d Assignment 1 2018-2019
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(6,4),(5,5)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth=35
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
-- There are 36 grids in total, the start position is (1,1) and its depth is 0,
-- so the maximum depth to reach the goal is 35.

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch = [node:branch  -- joining the node(valid) into current branch
              | node <- fourWays,  -- node picked from fourWays
                notElem node branch, -- node doesn't repeat
                --Auxiliary Function
                isValid node]  -- node is in grid and not faulty
  where
    (x,y) = head branch  -- pick the current position
    fourWays = [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]  -- robot can move right, left, up, down, 4 ways



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing -- fail when branch is empty
breadthFirstSearch destination next branches exploredList
    | destination `elem` badNodesList = Nothing  -- if destination is faulty then fail
    | not $ null path = Just (head path) -- return first branch if solution exist
    | heads `elem` exploredList = breadthFirstSearch destination next (tail branches) exploredList -- if node is already in exploredList, skip it
    | otherwise = breadthFirstSearch destination next newBranches (heads : exploredList)  -- expand child nodes
    where
      heads = head $ head branches  -- the current node

      newBranches = tail branches ++ next (head branches) -- expand the current state and add all child branch into branches

      path = [branch | branch <- branches, checkArrival destination $ head branch]  -- list of branches that can reach the goal



-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch

depthFirstSearch destination next [] exploredList = Nothing -- fail when branch is empty
depthFirstSearch destination next branches exploredList
    | destination `elem` badNodesList = Nothing  -- fail if destination is faulty
    | not $ null path = Just (head path) -- return first branch if solution exist
    | heads `elem` exploredList = depthFirstSearch destination next (tail branches) exploredList -- if node is already in exploredList, skip it
    | otherwise = depthFirstSearch destination next newBranches (heads : exploredList)  -- expand child nodes
    where
      heads = head $ head branches  -- the current node

      newBranches = next (head branches) ++ tail branches  -- expand the current state and add all child branch in the front of branches

      path = [branch | branch <- branches, checkArrival destination $ head branch]  -- list of branches that can reach the goal

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch

depthLimitedSearch destination next [] d = Nothing -- fail when branch is empty
depthLimitedSearch destination next branches d
    | destination `elem` badNodesList = Nothing  -- fail if destination is faulty
    | not $ null path = Just (head path) -- return first solution if solution exist
    | d > 0 = depthLimitedSearch destination next newBranches (d-1)  -- iterating until a solution or fail
    | otherwise = Nothing
    where
      heads = head $ head branches  -- the current node

      newBranches = next (head branches) ++ tail branches  -- expand the current state and add all child branch in the front of branches

      path = [branch | branch <- branches, checkArrival destination $ head branch]  -- list of branches that can reach the goal


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
    | d > maxDepth = Nothing  -- No solution under maxDepth
    | depthLimitedSearch destination next [[initialNode]] d == Nothing
      = depthLimitedSearch destination next [[initialNode]] (d+1)  -- no solution, depth + 1
    | otherwise
      = depthLimitedSearch destination next [[initialNode]] d  -- return a solution

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs(x-a) + abs(y-b)
  where
    (x,y) = position
    (a,b) = destination

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] exploredList = Nothing
bestFirstSearch destination next heuristic branches exploredList
    | curNode == destination = Just $ head branches -- return a solution is current node is destination
    | curNode `elem` exploredList = bestFirstSearch destination next heuristic (tail branches) exploredList
    | otherwise = bestFirstSearch destination next heuristic sortedBran newExpList  -- iterating until a solution
    where
    --heuristic::Node -> Int
    --heuristic = manhattan destination
    curNode = head $ head branches  -- the current node

    sortedBran = sorting newBranches  --sorting the branches by heuristic value
        where
        sorting = sortBy (\a b -> compare (heuristic (head a)) (heuristic (head b)))
        newBranches = tail branches ++ next (head branches)  -- rest nodes + childen node of current node

    newExpList = curNode : exploredList  -- add current node into exploredList


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing
aStarSearch destination next heuristic cost branches exploredList
  | curNode == destination = Just $ head branches -- return a solution is current node is destination
  | curNode `elem` exploredList = aStarSearch destination next heuristic cost (tail branches) exploredList
  | otherwise = aStarSearch destination next heuristic cost sortedBran newExpList  -- iterating until a solution
  where
--  heuristic::Node -> Int
--  heuristic = manhattan destination
  totalCost branch = cost branch + heuristic (head branch)
  curNode = head $ head branches  -- the current node

  sortedBran = sorting newBranches  --sorting the branches by total cost
      where
      sorting = sortBy (\a b -> compare (totalCost a) (totalCost b))
      newBranches = tail branches ++ next (head branches)  -- rest nodes + childen node of current node

  newExpList = curNode : exploredList -- add current node into exploredList


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch - 1


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game =undefined

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player =undefined


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player =undefined


-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =undefined



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player =undefined



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



	-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
isValid:: Node -> Bool
-- Checking next position is the grid and not faulty
isValid (x,y) = and [x > 0, x <= gridLength_search,
                    y > 0, y <= gridWidth_search,
                    notElem (x,y) badNodesList]
