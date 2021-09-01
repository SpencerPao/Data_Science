# XGBoost
- Demonstrating what XGBoost is and how it can be applied to a Classification Problem
- Notebook walkthrough (and walk-along) on applying XGBoost
- Basic application for XGBoost Regression and Classification
- 
# Logic Behind XGBoost Classification

# a) Start by building a tree (You stop growing the tree by limiting the number of levels set in the beginning). You do this by...

	1) You start by clustering the observations my residuals.

		First makes a prediction (You use this as the previous probability for the first tree development)

		Calculate the similarity score = Sum of residuals squared / (sum previous probability * (1 - Previous Probability)) + Lambda)

			Lambda (A regularization term) reduces similiarity score (Making the nodes easier to prune)
			Previous probability is the prediction.

	2) Calculate the gain value, Left Similarity + Right Similarity - Root Similarity (Root starts at 0)
		You want the gain value to be the largest possible. (you can change this by tuning the threshold)
		You then keep that branch with the largest gain value.
	
	3) Cover is calculated by: (sum previous probability * (1 - Previous Probability))
		This decides whether you will keep that branch or leaf. 

# b) Once you have wrote the tree, now you can start pruning the tree.
	1) Gain  - gamma. (You would only prune if Gain - gamma is a negative number)

# c) Calculate the output value for each node. : Sum of Residuals / (Sum (Previous Probability * (1 - Previous Probability)) + Lambda)

# d) PREDICTIONS: Like other boosting methods, XGBoost for classification makes new predictions by startig w/ initial prediction
	# Need to convert the probability to a log odds value log(probability / (1 - Probability)) = log(odds)
	- 1) Output (log odds prediction) = log(odds) of prediction + learning rate (aka eta) * output value from c) 
	- 2) Calculate probability = exp(log(odds)) / (1 + exp(-log(odds))



-------------------------------------------------------
i) Do same thing for N trees.
	Some things that have changed:
		Use the previous predicted probabilities of the old tree into the equation: Sum of residuals squared / (sum previous probability(root) * (1 - Previous Probability)) + Lambda)
	Follow same steps to get new predictions; keep building trees until the residuals have become minimized.

	# Summary:
	Calculate similiarity scores
	Use similarity scores from each node to calculate gain to split data
	Prune tree via Gain - gamma
	Calculate output values from the leaves
	Calculate the predictions via logodds

-------------------------------------------------------




Associated with the following YouTube video (Regression): https://youtu.be/QUp8EYNkHFU
