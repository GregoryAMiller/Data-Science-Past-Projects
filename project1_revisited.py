import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# Load the dataset
data = pd.read_csv('C:/Users/Grego/Documents/scripts/covid_data.csv')

# Check for missing values
# print(data.isna().sum())

# Drop missing values
data = data.dropna()

# Describe the data
# print(data.describe())

# # Visualize the data
# sns.pairplot(data)
# plt.show()

# # Create a correlation matrix
# corr_matrix = data.corr()
# sns.heatmap(corr_matrix, annot=True, cmap="YlGnBu")
# plt.show()

# # Create boxplots and histograms for each variable
# for col in data.columns:
#     fig, axs = plt.subplots(1, 2, figsize=(10, 4))
#     axs[0].boxplot(data[col])
#     axs[0].set_title(col + ' Boxplot')
#     axs[1].hist(data[col], bins=10)
#     axs[1].set_title(col + ' Histogram')
#     plt.show()

# # Create boxplots and histograms for each variable
# for col in data.select_dtypes(include=np.number).columns:
#     data[col].replace([np.inf, -np.inf], np.nan, inplace=True)  # replace infinite values with NaNs
#     data[col].dropna(inplace=True)  # drop rows with NaNs
#     fig, axs = plt.subplots(1, 2, figsize=(10, 4))
#     axs[0].boxplot(data[col])
#     axs[0].set_title(col + ' Boxplot')
#     axs[1].hist(data[col], bins=10)
#     axs[1].set_title(col + ' Histogram')
#     plt.show()


from sklearn.preprocessing import StandardScaler

# Select all numeric columns except the first column
numeric_cols = data.select_dtypes(include=np.number).columns[1:]
standardized_data = data
# Apply StandardScaler to numeric columns
scaler = StandardScaler()
standardized_data[numeric_cols] = scaler.fit_transform(data[numeric_cols])
# print("standardized_data")
# print(standardized_data)

# Calculate IQR for each column
Q1 = data.quantile(0.25)
Q3 = data.quantile(0.75)
IQR = Q3 - Q1

# Select all numeric columns except the first column
numeric_cols = data.select_dtypes(include=np.number).columns

# Remove outliers from each column
removed_outliers_data = data[~((data < (Q1 - 1.5 * IQR)) | (data > (Q3 + 1.5 * IQR))).any(axis=1)]
# print("removed_outliers_data")
# print(removed_outliers_data)

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

list_of_models = []

def model_pipeline(data, target_col, model_names=[], model_params={}, list_of_models=[]):
    """
    Creates a pipeline for testing different models on different datasets
    
    Parameters:
    data (pandas.DataFrame): the dataset to use for modeling
    target_col (str): the name of the target column in the dataset
    model_names (list): a list of model names to run (default is all models)
    model_params (dict): a dictionary of model parameters to use (default is default parameters)
    
    Returns:
    None
    """

    
    # Split the data into training and testing sets
    X = data.drop([target_col, 'Country_Region'], axis=1)
    # X = data.drop('Country_Region', axis=1)
    # print(X)
    y = data[target_col]
    # print(y)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    
    # Define the models to run
    models = {
        'Linear Regression': LinearRegression(),
        'Decision Tree Regressor': DecisionTreeRegressor(),
        'Random Forest Regressor': RandomForestRegressor()
    }
    
    # Update the default model parameters with any given parameters
    for model_name, params in model_params.items():
        if model_name in models:
            models[model_name].set_params(**params)
    
    # Only run specified models
    if model_names:
        models = {k: v for k, v in models.items() if k in model_names}
    
    # Define the pipeline for each model
    pipelines = {}
    for model_name, model in models.items():
        pipeline = Pipeline([
            ('scaler', StandardScaler()),
            ('model', model)
        ])
        pipelines[model_name] = pipeline
    
    # Train and test each model
    for model_name, pipeline in pipelines.items():
        pipeline.fit(X_train, y_train)
        y_pred = pipeline.predict(X_test)
        mse = mean_squared_error(y_test, y_pred)
        mae = mean_absolute_error(y_test, y_pred)
        r2 = r2_score(y_test, y_pred)
        print(f"{model_name} 'Mean Squared Error': {mse:.2f}, 'Mean Absolute Error': {mae:.2f}, 'R-squared': {r2:.2f}")
        list_of_models.append([model_name, mse, mae, r2])
        
def model_pipeline(data, target_col, model_params):
    X = data.drop(columns=[target_col])
    y = data[target_col]
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    
    results = []
    best_params = {}
    
    for model_name, param_grid in model_params.items():
        model = get_model(model_name)
        grid_search = GridSearchCV(model, param_grid, cv=5, n_jobs=-1, scoring='r2')
        grid_search.fit(X_train, y_train)
        best_params[model_name] = grid_search.best_params_
        model = get_model(model_name, grid_search.best_params_)
        model.fit(X_train, y_train)
        eval_metrics = evaluate_model(model, X_test, y_test)
        results.append((model_name, eval_metrics))
    
    # Create a bar chart of the evaluation metrics for each model
    metrics = ['Mean Squared Error', 'Mean Absolute Error', 'R-squared']
    fig, ax = plt.subplots(figsize=(8, 6))
    x_pos = [i for i, _ in enumerate(model_params)]
    for i, metric in enumerate(metrics):
        values = [result[1][metric] for result in results]
        ax.bar(x_pos + i * 0.2, values, width=0.2, label=metric)
    ax.set_xticks(x_pos)
    ax.set_xticklabels([f"{result[0]}\n{best_params[result[0]]}" for result in results])
    ax.legend()
    ax.set_title('Model Evaluation Metrics')
    ax.set_xlabel('Model and Best Parameters')
    ax.set_ylabel('Value')
    plt.show()
    
    return results    
# Load the data
# data = pd.read_csv('data.csv')

# Define the target column
target_col = 'vacRate'

# Run only the Random Forest Regressor with custom parameters
model_names = ['Random Forest Regressor', 'Linear Regression', 'Decision Tree Regressor']
model_params = {
    'Decision Tree Regressor': {
        'max_depth': [5,10],
        'min_samples_split': [10,20]
    },
    'Random Forest Regressor': {
        'n_estimators': [50,100],
        'max_depth': [10,20]
    }
}

# Run all models with default parameters
print('Default data w/ default parameters:')
model_pipeline(data, target_col, list_of_models=list_of_models)
print('standardized data w/ default parameters:')
model_pipeline(standardized_data, target_col, list_of_models=list_of_models)
print('removed outliers w/ default parameters:')
model_pipeline(removed_outliers_data, target_col, list_of_models=list_of_models)

# Run all models with custom parameters
print('Default data w/ new parameters:')
model_pipeline(data, target_col, model_names=model_names, model_params=model_params, list_of_models=list_of_models)
print('standardized data w/ new parameters:')
model_pipeline(standardized_data, target_col, model_names=model_names, model_params=model_params, list_of_models=list_of_models)
print('removed outliers w/ new parameters:')
model_pipeline(removed_outliers_data, target_col, model_names=model_names, model_params=model_params, list_of_models=list_of_models)

models = list_of_models

# Extract the performance metrics for each model
model_names = [model[0] for model in models]
mse_values = [model[1] for model in models]
mae_values = [model[2] for model in models]
r2_values = [model[3] for model in models]

# Set the width of the bars
bar_width = 0.2

# Set the position of the bars on the x-axis
r1 = np.arange(len(mse_values))
r2 = [x + bar_width for x in r1]
r3 = [x + bar_width for x in r2]

# Create the bar chart
plt.bar(r1, mse_values, color='b', width=bar_width, edgecolor='white', label='MSE')
plt.bar(r2, mae_values, color='g', width=bar_width, edgecolor='white', label='MAE')
plt.bar(r3, r2_values, color='r', width=bar_width, edgecolor='white', label='R2')

# Add xticks on the middle of the group bars
plt.xticks([r + bar_width for r in range(len(mse_values))], model_names, rotation=45, ha='right')

# Add labels and legend
plt.xlabel('Model')
plt.ylabel('Performance Metric')
plt.title('Comparison of Model Performance Metrics')
plt.legend()

# Show the plot
plt.show()
