import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVR
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.ensemble import VotingRegressor
from sklearn.ensemble import VotingClassifier
import matplotlib.pyplot as plt
import seaborn as sns
import time
df = pd.read_csv('C:/Users/Grego/Documents/scripts/star_classification.csv')

# df.head()
# df.tail()
# df.info()
# df.isnull().sum()

# drop unnecessary columns
df.drop(['obj_ID','run_ID','rerun_ID','cam_col','field_ID','fiber_ID'], axis = 1, inplace=True)

df.corr()

df.describe()

print(df.columns)

print(df['class'].value_counts())

# ax=sns.countplot(x=df['class'])
# ax.bar_label(ax.containers[0])

# sns.pairplot(df,hue="class")

# # save the plot to a file
# plt.savefig('plots.png')

# mapping numbers to class types
df["class"]=df["class"].map({"GALAXY":0,"STAR":1,"QSO":2})

# check the correlation of class column to other columns
abs(df.corr()["class"].sort_values(ascending=False))

from imblearn.over_sampling import SMOTE
# assign x and y
x = df.drop(['class'], axis = 1)
y = df.loc[:,'class'].values

# using oversampling with SMOTE to deal with imbalanced data
sm = SMOTE(random_state=42)
x, y = sm.fit_resample(x, y)

# Plot the oversampled data
ax=sns.countplot(y)
ax.bar_label(ax.containers[0])

# plt.show()

from sklearn.preprocessing import StandardScaler
ssc = StandardScaler()
scale = ssc.fit_transform(x) # use standard scaler to scale the data

# assign x and y values
x = df.drop("class",axis=1)
y = df[['class']]

from sklearn.model_selection import train_test_split
# split the data to train and test data
X_train, X_test, y_train, y_test = train_test_split(x, y, test_size = 0.2, random_state = 42)
y_train = np.ravel(y_train)
y_test = np.ravel(y_test)
print(X_train.shape,X_test.shape,y_train.shape,y_test.shape)

# Classification Algorithms
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.naive_bayes import GaussianNB, BernoulliNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier

# define the classification algorithms that you want to use
classifiers = [GaussianNB(), BernoulliNB(), KNeighborsClassifier(), DecisionTreeClassifier(), RandomForestClassifier(), GradientBoostingClassifier()]

# create a list of pipelines, where each pipeline consists of a scaler and a classifier
pipelines = [
    Pipeline([("scaler", StandardScaler()),
              ("classifier", classifier)]) for classifier in classifiers]

# define the parameter grids for each classifier
param_grids = [
    {},  # no parameters for GaussianNB
    {},  # no parameters for BernoulliNB
    {"classifier__n_neighbors": [2, 3, 5, 7]},  # n_neighbors parameter for KNeighborsClassifier
    {"classifier__max_depth": [7, 10, 20]},  # max_depth parameter for DecisionTreeClassifier
    {"classifier__max_depth": [7, 10, 20], "classifier__n_estimators": [30, 40, 50, ]},  # max_depth and n_estimators parameters for RandomForestClassifier
    {"classifier__learning_rate": [0.1, 0.5,], "classifier__n_estimators": [70, 100]},  # learning_rate and n_estimators parameters for GradientBoostingClassifier
]
best_models = []
index = 0
classifiers_names = ['GaussianNB()', 'BernoulliNB()', 'KNeighborsClassifier()', 'DecisionTreeClassifier()', 'RandomForestClassifier()', 'GradientBoostingClassifier()']

for pipeline, param_grid in zip(pipelines, param_grids):

    print(f'STARTING {classifiers_names[index]}')
    print('startimg timer')
    # Start the timer
    start_time = time.time()
    # Create the GridSearchCV object
    gs = GridSearchCV(pipeline, param_grid, cv=3)
    
    # Fit the cross-validation object to the training data
    gs.fit(X_train, y_train)

    # Get the best model
    best_model = gs.best_estimator_
    
    # Evaluate the model on the test set
    test_score = best_model.score(X_test, y_test)
    
    # Print the best parameters and score
    print(f'{classifiers_names[index]} Best parameters: {gs.best_params_}')
    print(f'{classifiers_names[index]} Best score: {gs.best_score_}')
    
    # Make predictions on the data
    y_pred = best_model.predict(X_test)

    # Calculate the R2 score of the model
    r2 = r2_score(y_test, y_pred)

    # Calculate the MSE of the model
    mse = mean_squared_error(y_test, y_pred)

    # Calculate the MAE of the model
    mae = mean_absolute_error(y_test, y_pred)
    
    # add the best models to a list
    if gs.best_estimator_ is not None and r2 > 0.7:
        best_models.append(best_model)

    print(f'{classifiers_names[index]} R2 score: {r2}')
    print(f'{classifiers_names[index]} MSE: {mse}')
    print(f'{classifiers_names[index]} MAE: {mae}')

    # Stop the timer
    end_time = time.time()
    print(f'Time elapsed: {end_time - start_time}\n\n')
    # index our classifiers index by one at the end
    index += 1


# Create a list of the models
ensemble_models = [('model{}'.format(i+1), model) for i, model in enumerate(best_models)]

# # Create a list of the models
# ensemble_models = [('model1', best_models[0]), ('model2', best_models[1])]

# Create the VotingClassifier
ensemble = VotingClassifier(ensemble_models)

# Fit the ensemble to the training data
ensemble.fit(X_train, y_train)

# Make predictions on the test data
y_pred = ensemble.predict(X_test)

# Calculate the R2 score of the model
r2 = r2_score(y_test, y_pred)

# Calculate the MSE of the model
mse = mean_squared_error(y_test, y_pred)

# Calculate the MAE of the model
mae = mean_absolute_error(y_test, y_pred)

print(f'R2 score: {r2}')
print(f'MSE: {mse}')
print(f'MAE: {mae}')
