# Team Name: YOLO
## Team-leads: Chang Li and Agaz Wani
### Team Members: Alex Dean, Nathan W. Van Bibber, Yibo Dong, Peter Radulovic, 
### Cosulting Team: Dr. Xiaoming Lui and Dr. Greg Herbert


## Objectives
In this project we will try to answer some questions using machine learning methods.
1. Is there an association between gut microbiome and obesity? 
   - In other words, can we predict age from gut microbiome composition using machine learning?
2. How well can we distinguish between lean, overweight, and obese when analyzing OTUs by machine learning?
   - Can our model identify important bacteria and replicate previous findings?
   - Is there systematic differences between monozygotic and dizygotic twins with regard to their microbiota composition, controlling for other confounders?
3. How accurately can we predict ancestry using gut microbiome and machine learning

Data were collected from a twin study with 281 samples and more than 5000 OTUs


## Methods and Implementation
### Preprocessing
Microbiome data goes through pre-processing before machine learning implementation.  Pre-processing transforms input data into a suitable form which the network can identify and use. Data processing goals could include reduction of input space size, smoother relationships among data, normalization, feature extraction, and noise reduction. Microbiome data produced from high-throughput sequencing often leads to a variable number of reads per sample.  Normalization ensures comparability of data across samples with differences in read depth. 

### PCA
### Machine Learning
Machine learning describes the use of algorithms to recognize, classify, and predict patterns from data (Tarca, 2007).

~~#### Random Forest
Random forest describes a supervised machine learning strategy that splits samples into successively smaller groups based on specific features and associated threshold values.~~

#### Linear SVM
Linear SVM was preformed using 10 fold cross validation with 3 repeats.

Linear Support Vector Machine (SVM) classifier: Project samples into a higher dimensional space so that they are linearly separable.

Support Vector Machines (SVM) is a method of supervised machine learning that is useful for classification, regression, and detection of outliers.  SVMs are effective in higher dimensions where the dimensions are greater than the numbers of samples.

#### KNN
K-nearest neighbor classifier (KNN): The classification of disease status is made by majority vote of close-by data points (n = K).

K-nearest neighbors (KNN) is a machine learning algorithm that can be used for classification and regression.



#### AdaBoost
Adaboost classifier: Train multiple tree classifiers (each tree has a subset of available features) and add more weight to those misclassified samples in the next training loop.

Adaptive Boosting (AdaBoost) is a machine learning meta-algorithm that can be used to improve performance of other machine learning algorithms.



### Shiny app


## Operation (requirements for code)



