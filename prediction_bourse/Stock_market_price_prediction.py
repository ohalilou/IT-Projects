import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Dropout
dataset_train = pd.read_csv("C:\Users\hlilo\IT-Projects\prediction_bourse\appl_train")
print(dataset_train)

train_set = dataset_train.iloc[:,1:2].values
print(train_set)

#Using MinMax Scaler function from sklearn
sc = MinMaxScaler(feature_range = (0,1))
training_scaled = sc.fit_transform(train_set)
print(training_scaled)

X_train = []
Y_train = []

for a in range(60,1258):
  X_train.append(training_scaled[a-60:a,0])
  Y_train.append(training_scaled[a,0])

X_train,Y_train = np.array(X_train),np.array(Y_train)
print(X_train.shape)

# Using Tensorflow
regressor = Sequential()
regressor.add(LSTM(units = 50,return_sequences = True,input_shape = (X_train.shape[1],1)))
regressor.add(Dropout(0.2))

regressor.add(LSTM(units = 50,return_sequences = True))
regressor.add(Dropout(0.2))

regressor.add(LSTM(units = 50,return_sequences = True))
regressor.add(Dropout(0.2))

regressor.add(LSTM(units = 50))
regressor.add(Dropout(0.2))

regressor.add(Dense(units = 1))

regressor.compile(optimizer = 'adam',loss = 'mean_squared_error')

regressor.fit(X_train,Y_train,epochs = 100, batch_size = 32)

#Using Test dataset
dataset_test =pd.read_csv("C:\Users\hlilo\IT-Projects\prediction_bourse\appl_test.csv")
real_stock_price = dataset_test.iloc[:,1:2].values
dataset_total = pd.concat((dataset_train['Open'],dataset_test['Open']),axis = 0)
print(dataset_total)

inputs = dataset_total[len(dataset_total) - len(dataset_test)-60:].values
print(inputs)

inputs = inputs.reshape(-1,1)
print(inputs)

# Transforming inputs
inputs = sc.transform(inputs)
print(inputs.shape)

X_test = []
for a in range(60,185):
    X_test.append(inputs[a-60:a,0])

X_test = np.array(X_test)
print(X_test.shape)

X_test = np.reshape(X_test, (X_test.shape[0],X_test.shape[1],1))
X_test.shape

predicted_price = regressor.predict(X_test)
predicted_price = sc.inverse_transform(predicted_price)
predicted_price

plt.plot(real_stock_price,color = 'red', label = 'Real Stock Price Of Apple')
plt.plot(predicted_price, color = 'blue', label = 'Predicted Price Of Apple')
plt.title('Apple Stock Price Prediction')
plt.xlabel('Time')
plt.ylabel('Apple Stock Price')
plt.legend()
plt.show()