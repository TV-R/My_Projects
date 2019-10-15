# -*- coding: utf-8 -*-
"""



"""
#
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.model_selection import train_test_split
import datetime
datafile = pd.read_csv('telco_train.csv',sep=',')
# splitting the dataset into train and test set
train_data, test = train_test_split(datafile, test_size=0.2,random_state=42,stratify=datafile['CHURN'])
#drop finanical state and id variable
train=train_data.copy()
train.info()
train_columns= train.columns[train.isna().any()].tolist()
train = train.drop(['FIN_STATE','ID'],axis=1)

#Processing for people with no data connections
train['AVG_DATA_3MONTH'].fillna(1, inplace=True)
train['COUNT_CONNECTIONS_3MONTH'].fillna(0, inplace=True)
train['AVG_DATA_1MONTH'].fillna(-1, inplace=True)

train['DATA_PRESENCE'] = train['AVG_DATA_3MONTH'].apply(lambda x: 0 if x==1 else 1)



fig, axs = plt.subplots(ncols=2,nrows=2)

#various plots
train['DAYS_PAYMENT_DELAYS_CURRENT']=-train['DAYS_PAYMENT_DELAYS_CURRENT']
sns.barplot(x='PREPAID',y='DAYS_PAYMENT_DELAYS_1YEAR',hue='CHURN',data=train,ax=axs[0][0])
sns.barplot(x='PREPAID',y='COUNT_PAYMENT_DELAYS_1YEAR',hue='CHURN',data=train,ax=axs[1][0])
sns.barplot(x='PREPAID',y='COUNT_PAYMENT_DELAYS_CURRENT',hue='CHURN',data=train,ax=axs[1][1])
sns.barplot(x='PREPAID',y='DAYS_PAYMENT_DELAYS_CURRENT',hue='CHURN',data=train,ax=axs[0][1])
fig.show()
train['DAYS_PAYMENT_DELAYS_CURRENT']=-train['DAYS_PAYMENT_DELAYS_CURRENT']
np.corrcoef(train.DAYS_PAYMENT_DELAYS_1YEAR,train.COUNT_PAYMENT_DELAYS_1YEAR)
np.corrcoef(train.DAYS_PAYMENT_DELAYS_1YEAR,train.COUNT_PAYMENT_DELAYS_CURRENT)
corr_matrix_payment = train[['DAYS_PAYMENT_DELAYS_1YEAR','COUNT_PAYMENT_DELAYS_1YEAR','DAYS_PAYMENT_DELAYS_CURRENT','COUNT_PAYMENT_DELAYS_CURRENT']].corr()

train_payment =train[['DAYS_PAYMENT_DELAYS_1YEAR','COUNT_PAYMENT_DELAYS_1YEAR','DAYS_PAYMENT_DELAYS_CURRENT','COUNT_PAYMENT_DELAYS_CURRENT']]
#Prinicipal component analysis

from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

train=train.reindex(axis=0)
pca=PCA(n_components=1)

pipeline = Pipeline([('scaling', StandardScaler()), ('pca', PCA(n_components=1))])
principalComponents = pipeline.fit_transform(train[['DAYS_PAYMENT_DELAYS_1YEAR','COUNT_PAYMENT_DELAYS_1YEAR','DAYS_PAYMENT_DELAYS_CURRENT','COUNT_PAYMENT_DELAYS_CURRENT']])

principalDf = pd.DataFrame(data = principalComponents
             , columns = ['principal component 1'])
pca=pipeline.steps[1][1]
coefficients=pca.components_.T * np.sqrt(pca.explained_variance_)
train['PAYMENT_COL']= train['DAYS_PAYMENT_DELAYS_1YEAR']*coefficients[0][0]+train['COUNT_PAYMENT_DELAYS_1YEAR']*coefficients[1][0]+train['DAYS_PAYMENT_DELAYS_CURRENT']*coefficients[2][0]+train['COUNT_PAYMENT_DELAYS_CURRENT']*coefficients[3][0]
train=train.drop(['DAYS_PAYMENT_DELAYS_1YEAR','COUNT_PAYMENT_DELAYS_1YEAR','DAYS_PAYMENT_DELAYS_CURRENT','COUNT_PAYMENT_DELAYS_CURRENT'],axis=1)

#Date variable conversion

for i in range(4000):
    s=(train.iloc[i,1].split('-'))
    s=(''.join(s))
    train.iloc[i,1] = datetime.date(year=int(s[0:4]), month=int(s[4:6]), day=int(s[6:8]))
    
enddate=datetime.date(2013,10,31)
train['START_DATE'] = train['START_DATE'].apply(lambda x: abs((enddate-x).days))

plt.scatter(range(4000),train['START_DATE'])
plt.xlabel('index')
plt.ylabel('DAYS ELAPSED')
#Complaint variable editing
COMPLAINT=train.columns[train.columns.str.contains('COMPLAINT')].tolist()
train_COMPLAINT= train[['COMPLAINT_1WEEK',
 'COMPLAINT_2WEEKS',
 'COMPLAINT_1MONTH',
 'COMPLAINT_3MONTHS',
 'COMPLAINT_6MONTHS','CHURN']]
sns.pairplot(train_COMPLAINT,hue='CHURN')
train_NOCOMPLAINT= train.loc[(train_COMPLAINT['COMPLAINT_1WEEK']==0) &
 (train['COMPLAINT_2WEEKS']==0) &
 (train['COMPLAINT_1MONTH']==0)&
 (train['COMPLAINT_3MONTHS']==0) &
 (train['COMPLAINT_6MONTHS']==0)]
train_NOCOMPLAINT.groupby(['CHURN']).count()


train_COMPLAINT= train.loc[(train_COMPLAINT['COMPLAINT_1WEEK']!=0) |
 (train['COMPLAINT_2WEEKS']!=0) |
 (train['COMPLAINT_1MONTH']!=0)|
 (train['COMPLAINT_3MONTHS']!=0) |
 (train['COMPLAINT_6MONTHS']!=0)]
train_COMPLAINT.groupby(['CHURN']).count()

#binning of start date

v=pd.DataFrame()
v['START']= np.sort(train['START_DATE'])
vv=pd.qcut(v['START'],8)
train['START_DATE']=pd.cut(train['START_DATE'],[0,141,219,320,455,594,831,1343,np.inf],labels=[1,2,3,4,5,6,7,8])
smm = train.groupby(['START_DATE','CHURN']).mean()

train_churn_date_nocomplaint = train_NOCOMPLAINT[['CHURN','START_DATE','PREPAID']].groupby(['CHURN','START_DATE']).count()
train_churn_date_complaint = train_COMPLAINT[['CHURN','START_DATE','PREPAID']].groupby(['CHURN','START_DATE']).count()

churn_year_grouped = pd.read_csv('Book1.csv',sep=',')



plt.scatter(x=churn_year_grouped['START_DATE'],y=churn_year_grouped['COMPL/NO COMPL'],c=churn_year_grouped['CHURN'])
plt.legend([0,1])
plt.xlabel('START_YEAR')
plt.ylabel('RATIO OF COMPLAINT TO NO COMPLAINT')

complaint_list=['COMPLAINT_1WEEK',
 'COMPLAINT_2WEEKS',
 'COMPLAINT_1MONTH',
 'COMPLAINT_3MONTHS',
 'COMPLAINT_6MONTHS','CHURN']


    
  

trainx=train.drop(['PAYMENT_COL','COMPLAINT_1WEEK',
 'COMPLAINT_2WEEKS',
 'COMPLAINT_1MONTH',
 'COMPLAINT_3MONTHS',
 'COMPLAINT_6MONTHS','PREPAID'],axis=1)
#Call and message part
call_and_message_mean = trainx.groupby(['CHURN']).mean()
pivot_call = call_and_message_mean.T

sns.boxplot(x='CHURN',y='AVG_MINUTES_OUT_OFFNET_1MONTH',data=trainx)
sns.boxplot(x='CHURN',y='AVG_MINUTES_OUT_ONNET_1MONTH',data=trainx)
       
X=train.drop(['CHURN'],axis=1)
Y=train['CHURN']

zero_calls_offnet = trainx[trainx['AVG_MINUTES_OUT_OFFNET_1MONTH']==0].groupby('CHURN').count()

zero_calls_onnet = trainx[trainx['AVG_MINUTES_OUT_ONNET_1MONTH']==0].groupby('CHURN').count()
sns.relplot(x='COUNT_ONNET_CALLS_1WEEK',y='COUNT_OFFNET_CALLS_1WEEK',hue='CHURN',data=trainx)
sns.relplot(x='COUNT_ONNET_CALLS_1WEEK',y='COUNT_OFFNET_CALLS_1WEEK',col='CHURN',data=trainx)

zero_calls_offnet_oneweek = trainx[trainx['COUNT_OFFNET_CALLS_1WEEK']==0].groupby('CHURN').count()
zero_calls_onnet_oneweek = trainx[trainx['COUNT_ONNET_CALLS_1WEEK']==0].groupby('CHURN').count()
zero_calls_onnetoroffnet_oneweek = trainx[(trainx['COUNT_ONNET_CALLS_1WEEK']==0) & (trainx['COUNT_OFFNET_CALLS_1WEEK']==0)].groupby('CHURN').count()

greater_calls_onnetoroffnet_oneweek = trainx[trainx['COUNT_ONNET_CALLS_1WEEK']>trainx['COUNT_OFFNET_CALLS_1WEEK']].groupby('CHURN').count()
trainx['COUNT_SMS_INC_ONNET_6']=trainx['COUNT_SMS_INC_ONNET_1MONTH']*6

sns.relplot(x='COUNT_SMS_INC_ONNET_6',y='COUNT_SMS_INC_ONNET_6MONTH',col='CHURN',data=trainx)
sns.lmplot(x='COUNT_SMS_INC_ONNET_6',y='COUNT_SMS_INC_ONNET_6MONTH',hue='CHURN',data=trainx)

trainx2=trainx[trainx['DATA_PRESENCE']==1]
trainx2['AVG_DATA_1MONTH_3MONTH']=trainx2['AVG_DATA_1MONTH']*3
sns.relplot(x='AVG_DATA_1MONTH_3MONTH',y='AVG_DATA_3MONTH',hue='CHURN',data=trainx2)


#processing the test set

def preprocess(dataframe):
    test1=dataframe.copy()
    test1 = test1.drop(['FIN_STATE','ID'],axis=1)
    test1['AVG_DATA_3MONTH'].fillna(1, inplace=True)
    test1['COUNT_CONNECTIONS_3MONTH'].fillna(0, inplace=True)
    test1['AVG_DATA_1MONTH'].fillna(-1, inplace=True)
    test1['DATA_PRESENCE'] = test1['AVG_DATA_3MONTH'].apply(lambda x: 0 if x==1 else 1)
    for i in range(len(test1)):
        s=(test1.iloc[i,1].split('-'))
        s=(''.join(s))
        test1.iloc[i,1] = datetime.date(year=int(s[0:4]), month=int(s[4:6]), day=int(s[6:8]))
    enddate=datetime.date(2013,10,31)
    test1['START_DATE'] = test1['START_DATE'].apply(lambda x: abs((enddate-x).days))
    test1['DAYS_PAYMENT_DELAYS_CURRENT']=-test1['DAYS_PAYMENT_DELAYS_CURRENT']
    test1['PAYMENT_COL']= test1['DAYS_PAYMENT_DELAYS_1YEAR']*coefficients[0][0]+test1['COUNT_PAYMENT_DELAYS_1YEAR']*coefficients[1][0]+test1['DAYS_PAYMENT_DELAYS_CURRENT']*coefficients[2][0]+test1['COUNT_PAYMENT_DELAYS_CURRENT']*coefficients[3][0]
    test1=test1.drop(['DAYS_PAYMENT_DELAYS_1YEAR','COUNT_PAYMENT_DELAYS_1YEAR','DAYS_PAYMENT_DELAYS_CURRENT','COUNT_PAYMENT_DELAYS_CURRENT'],axis=1)
    test1['START_DATE']=pd.cut(test1['START_DATE'],[0,141,219,320,455,594,831,1343,np.inf],labels=[1,2,3,4,5,6,7,8])
 
    return test1




test1= preprocess(test)
X_test =test1.drop(['CHURN'],axis=1)
y_test =test1['CHURN']
#random forest classifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import accuracy_score,f1_score
from sklearn.metrics import confusion_matrix,roc_auc_score,roc_curve,make_scorer
roc_auc = make_scorer(roc_auc_score)
rvc=RandomForestClassifier(oob_score=True,class_weight='balanced',random_state=42)
params = {'max_depth':list(range(5,15,3)),'n_estimators':[100,250,500]}

gs=GridSearchCV(rvc,param_grid=params,scoring=roc_auc)
gs.fit(X,Y)
best_params_rf= gs.best_params_
gs.cv_results_


accuracy_score(Y,gs.predict(X))


rf_pred_test=gs.predict(X_test)
accuracy_score(y_test,rf_pred_test)
roc_auc_score(Y,gs.predict_proba(X)[:,1])
roc_auc_score(y_test,gs.predict_proba(X_test)[:,1])
f1_score(Y,gs.predict(X))
f1_score(y_test,gs.predict(X_test))
predictions_test = gs.predict_proba(X_test)
fpr, tpr, _ = roc_curve(y_test, predictions_test[:,1])
z= roc_auc_score(y_test, predictions_test[:,1])
cm_test_random_forest= confusion_matrix(y_test,gs.predict(X_test))

predictions_train= rvc.predict_proba(X)
fpr, tpr, _ = roc_curve(Y, predictions_train[:,1])
z1= roc_auc_score(Y, predictions_train[:,1])

plt.scatter(range(4000),train['START_DATE'])
plt.xlabel('index')
plt.ylabel('DAYS ELAPSED')

#Gradient Boosting
from sklearn.ensemble import GradientBoostingClassifier

gbrt = GradientBoostingClassifier()
params_gbrt = {'n_estimators':[100,200,250],'learning_rate':[0.03,0.05,0.07],'max_depth':[3,4,5]}
gs_gbrt=GridSearchCV(gbrt,param_grid=params_gbrt,scoring=roc_auc)
gs_gbrt.fit(X,Y)
best_params_gbrt= gs_gbrt.best_params_
gs_gbrt.cv_results_
accuracy_score(Y,gs_gbrt.predict(X))
accuracy_score(y_test,gs_gbrt.predict(X_test))
predictions_gbrt_train = gs_gbrt.predict_proba(X)
z_gbrt_train= roc_auc_score(Y, predictions_gbrt_train[:,1])

predictions_gbrt_test = gs_gbrt.predict_proba(X_test)
z_gbrt_test= roc_auc_score(y_test, predictions_gbrt_test[:,1])
f1_score(Y,gs_gbrt.predict(X))
f1_score(y_test,gs_gbrt.predict(X_test))
confusion_matrix_gbrt=confusion_matrix(y_test,gs_gbrt.predict(X_test))

#Naive Bayes
from sklearn.naive_bayes import GaussianNB
Bayes_model = GaussianNB(priors=[2843/4000,1157/4000])
Bayes_model.fit(X,Y)

accuracy_score(Y,Bayes_model.predict(X))
accuracy_score(y_test,Bayes_model.predict(X_test))
predictions_NaiveBayes_train = Bayes_model.predict_proba(X)
z_Bayes_model_train= roc_auc_score(Y, predictions_NaiveBayes_train[:,1])

predictions_NaiveBayes_test = Bayes_model.predict_proba(X_test)
z_Bayes_model_test= roc_auc_score(y_test, predictions_NaiveBayes_test[:,1])
f1_score(Y,Bayes_model.predict(X))
f1_score(y_test,Bayes_model.predict(X_test))

confusion_matrix_NaiveBayes=confusion_matrix(y_test,Bayes_model.predict(X_test))
confusion_matrix_NaiveBayes_train=confusion_matrix(Y,Bayes_model.predict(X))
#Comparison of Random Forest and Gradient Boosting Predictions
accuracy_score(gs.predict(X_test),gs_gbrt.predict(X_test))
confusion_matrix(gs.predict(X_test),gs_gbrt.predict(X_test))

