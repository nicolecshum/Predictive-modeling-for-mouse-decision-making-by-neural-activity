Decision making based on visual perception in mice is correlated with neural activity from a wide variety of brain areas. 
In this report, we present an analysis of a data set from Steinmetz et al. (2019.) This data set comprises of the data 
recorded from hundreds of trials of mouse decision-making, with neural activity in the form of brain spikes recorded by 
Neuropixels probes. This analysis aims to develop and evaluate a predictive model for success in decision making based on 
neural activity. Exploratory data analysis is first used to gain insights into the distribution, relationship, and patterns 
within the dataset, through descriptive statistic visualizations and correlation analyses. In order to build predictive models, 
XGBoost modeling and generalized linear models are utilized. The performance of each model is evaluated based on accuracy, 
confusion matrices, precision-recall curves, and AUC. Overall, the most suitable approach to predicting feedback type was 
using models based on average neuron spikes per session-specific brain area, trained through XGBoost modeling. Additionally, 
hypotheses are made on the most important factors on the predictive performance of the model, providing insights for future 
studies or analysis.

This report was made with R markdown.
