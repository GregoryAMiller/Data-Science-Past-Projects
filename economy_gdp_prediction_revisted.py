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
import matplotlib.pyplot as plt

# Load the data from the CSV file into a Pandas DataFrame
df = pd.read_csv('C:/Users/Grego/Documents/scripts/modeling_data_outliers.csv')

# df = pd.read_csv('C:/Users/Grego/Documents/scripts/finalData.csv')

# df = pd.read_csv('C:/Users/Grego/Documents/scripts/removedOutliers.csv')

# Select the features to use in the model
X = df.drop(columns=['DATE', 'GDP'])

# Select the target variable
y = df['GDP']

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create a list of pipelines
pipelines = [
    Pipeline([
        ('pca', PCA()),
        ('model', RandomForestRegressor())
    ]),
    Pipeline([
        ('pca', PCA()),
        ('model', LinearRegression())
    ]),
    Pipeline([
        ('pca', PCA()),
        ('model', SVR())
    ])
]

# ,
#     Pipeline([
#         ('pca', PCA()),
#         ('model', LogisticRegression())
#     ])

param_grids = [
    {
        'pca__n_components': [7, 8, 9, 10, 11, 12],
        'model__n_estimators': [10, 50, 100],
        'model__max_depth': [None, 5, 10],
        'model__min_samples_split': [2, 5, 10],
        'model__max_features': ['sqrt', 'log2']
    },
    {
        'pca__n_components': [5, 7, 8, 9, 10, ],
        'model__fit_intercept': [True, False],
    },
    {
        'pca__n_components': [5, 7, 8, 9, 10, 11, ],
        'model__kernel': ['linear', 'poly', 'rbf', 'sigmoid'],
        'model__degree': [2, 3, 4],
        'model__gamma': ['scale', 'auto'],
        'model__tol': [1e-4, 1e-3, 1e-2],
    }
]

    # {
    #     'pca__n_components': [2, 5, 10],
    #     'model__C': [0.1, 1.0, 10.0],
    #     'model__fit_intercept': [True, False],
    #     'model__intercept_scaling': [0.1, 1.0, 10.0],
    #     'model__class_weight': [None, 'balanced'],
    #     'model__solver': ['newton-cg', 'lbfgs', 'liblinear', 'sag', 'saga'],
    #     'model__tol': [1e-4, 1e-3, 1e-2],
    # }
# Create a cross-validation object using the pipeline and the hyperparameter grid
# Loop through the pipelines and hyperparameter grids
best_models = []
i = 0
for pipeline, param_grid in zip(pipelines, param_grids):
    # Create the GridSearchCV object
    gs = GridSearchCV(pipeline, param_grid, cv=3)
    
    # Fit the cross-validation object to the training data
    gs.fit(X_train, y_train)

    # Get the best model
    best_model = gs.best_estimator_

    # add the best models to a list
    best_models.append(best_model)
    
    # Evaluate the model on the test set
    test_score = best_model.score(X_test, y_test)
    
    # Print the best parameters and score
    print(f'Best parameters: {gs.best_params_}')
    print(f'Best score: {gs.best_score_}')
    # print(f'Test score: {test_score}')

    # # Get the PCA object from the pipeline
    # pca = pipeline.named_steps['pca']

    # # Loop through the components
    # for i, component in enumerate(pca.components_):
    # # Select the top 3 features for each component
    #     top_features = np.abs(component).argsort()[-3:][::-1]
    
    # # Print the top features for each component
    #     print(f'Top features for component {i}: {df.columns[top_features]}')

    # Make predictions on the data
    y_pred = best_model.predict(X_test)

    # Calculate the R2 score of the model
    r2 = r2_score(y_test, y_pred)

    # Calculate the MSE of the model
    mse = mean_squared_error(y_test, y_pred)

    # Calculate the MAE of the model
    mae = mean_absolute_error(y_test, y_pred)

    print(f'R2 score: {r2}')
    print(f'MSE: {mse}')
    print(f'MAE: {mae}')

    # Create a scatterplot of the original and predicted values
    plt.scatter(y_test, y_pred)

    # Add labels and a title
    plt.xlabel('Original values')
    plt.ylabel('Predicted values')
    plt.title('Predicted vs original values')

    # Show the plot
    plt.show()

    # # Create a new dataframe with the original test data, the true values, and the predicted values
    # df_pred = pd.DataFrame(X_test, columns=df.columns[:2])
    # df_pred['y_true'] = y_test
    # df_pred['y_pred'] = y_pred

    # # Save the dataframe to a CSV file

    # df_pred.to_csv(f'model_{i}_pred.csv', index=False)
    # i += 1

# print(best_models)

# Create a list of the models
ensemble_models = [('model1', best_models[1]), ('model2', best_models[2])]

# Create the VotingRegressor
ensemble = VotingRegressor(ensemble_models)

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

# Create a scatterplot of the original and predicted values
plt.scatter(y_test, y_pred)

# Add labels and a title
plt.xlabel('Original values')
plt.ylabel('Predicted values')
plt.title('Predicted vs original values')

# Show the plot
plt.show()
