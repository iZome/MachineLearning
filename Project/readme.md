# Project - Machine Learning

In the world today there are many methods that fall under the category of "Machine Learning", some being very similar and some very different. All of them share in common that they use data to produce a model that can be used to tell us something on previously unseen data. When successful this is very appealing in today’s society where we got lots of data on situations where there is likely to be a underlying pattern, another requirement for learning. There is broad agreement that Machine Learning is a good way to make prediction and classification models, and often the only computational feasible way to do so. This makes it a task to decide which method in machine learning to choose for a given problem. The answer is not always the same and several methods can be good, but for different reasons. In this report I will utilize several of the most used machine learning algorithms and show how they perform on classifying images of handwritten digits.

![alt text][digits_front]

[digits_front]: R_scripts/data/images_digits.png "Handwritten digits from training set"

## Methods implemented
Below are the methods implemented to predict the right numbers in a test set based on the data processed in the training.
They are listed in order of accuracy, with the best performing listed first.
Format:"Method name" "Error-rate"

 Nr. | Method name | Error-rate
--- | --- | ---
1 | Convolutional Neural Network | 3.6%
2 | Support Vector Machine | 5.6%
3 | Random Forest | 8.1%
4 | Artificial Neural Network | 9.1%
5 | K-Nearest Neighbours | 10.1%
6 | Bagging | 10.3%
7 | Boosting | 11.9%
8 | Classification Tree | 37.1%

#### Code
The code can be found in the **R_scripts/** folder. Example run in terminal using:

```
 > Rscript Random_Forest.R
