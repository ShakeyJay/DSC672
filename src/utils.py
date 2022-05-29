"""
This module holds all of the functions that were created for the
project notebook.
"""

from collections import Counter

import pandas as pd
from sklearn.metrics import accuracy_score


def read_data(path):
    all_files = glob.glob(path + "/*.csv")

    li = []  # list for the .csv

    for filename in all_files:
        df = pd.read_csv(filename, index_col=None, header=0, na_values=0)
        li.append(df)  # append all .csv to dataframe

    frame = pd.concat(li, axis=0, ignore_index=True)
    return frame


def print_model_acc(y_true, knn_preds, dt_preds, rf_preds, svm_preds):
    print(
        f"KNN Accuracy = {accuracy_score(y_true, knn_preds)} \n"
        + f"Decision Tree Accuracy = {accuracy_score(y_true, dt_preds)}  \n"
        + f"Random Forest Accuracy = {accuracy_score(y_true, rf_preds)}  \n"
        + f"SVM Accuracy = {accuracy_score(y_true, svm_preds)}  \n"
    )


def voting_system(svc_array, knn_array, dt_array, rf_array):
    # At first we just have each of them have equal weighting.
    # Eventually I think giving the better classes some extra pull.
    # Something like SVC = 2 KNN = 2 dt = 1? or giving each a % of confidence?
    # instead of saying we are 100% sure on each it is like we are KNN is 60% sure
    # SVM is 35% sure and DT is 50% sure then we multiply those values by their weights
    # and choose the option that is highest?
    voting_array = []
    for x in zip(svc_array, knn_array, dt_array, rf_array):
        voting_array.append(Counter(x).most_common(1)[0][0])

    return voting_array
