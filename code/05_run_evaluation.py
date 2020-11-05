

#outputs will be stored in the scores folder. Evaluation outputs stored in the task2_evaluation.csv file
#save your groundtruth file into the submission folder as task2_ground.csv  (e.g. ./submission/task2_ground.csv)
#save your submission set in the submission folder as task2_submission.csv (e.g. ./submission/task2_submission.csv)


# load test dataset
import pandas as pd
from sklearn import metrics
from sklearn.metrics import log_loss
from sklearn.metrics import confusion_matrix

datadir = "./self-evaluation/"
outputdir = "./self-evaluation/"

# compute F1, cross entropy and confusion matrix
preds = pd.read_csv(datadir + "task2_submission.csv")
obs = pd.read_csv(datadir + "task2_ground.csv")

# compute cross entropy
ce_preds = preds.pivot(index="indvdID", columns="taxonID", values="probability")
log_loss = log_loss(obs["speciesID"], ce_preds, labels = ce_preds.columns)
# get class from majority vote and compute F1 and confusion matrix
idx = preds.groupby(["indvdID"])["probability"].transform(max) == preds["probability"]
preds = preds[idx]
evaluation_data = preds.merge(obs, left_on="indvdID", right_on="ID")
confusion_matrix = confusion_matrix(
evaluation_data["taxonID"], evaluation_data["speciesID"]
)

classification_report = metrics.classification_report(
evaluation_data["taxonID"], evaluation_data["speciesID"], output_dict=True
)

df = pd.DataFrame(classification_report).transpose()
df = df.rename(index={"macro avg": "macro F1", "weighted avg": "micro F1"})
df.to_csv(outputdir + "/task2_evaluation.csv")
print(df)


y_true = [0]
y_pred = [[0.51, 0.49]]
log_loss(y_true, y_pred, labels = [0, 1])