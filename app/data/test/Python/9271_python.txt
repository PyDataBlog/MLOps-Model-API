# -*- coding:UTF-8 -*-

from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation
from keras.layers import Embedding
from keras.layers import LSTM,Flatten
import numpy as np
import loadpkl

(X_train, y_train),(X_test, y_test) = loadpkl.load_data()
#print "hello"
X_train = np.array(X_train)
y_train = np.array(y_train)
print X_train.shape
print y_train.shape
t0=X_train.shape[0]/300
#print t0
X_train = X_train.reshape(t0,50,6) #训练集行数
y_train = y_train.reshape(t0,50)
print X_train.shape
print y_train.shape

X_test = np.array(X_test)
y_test = np.array(y_test)
t1=X_test.shape[0]/300
print t1
X_test = X_test.reshape(t1,50,6)#测试集行数 、
y_test = y_test.reshape(t1,50)

#print (X_train, y_train)
#print (X_test, y_test)
#print X_train
#print y_train
Y_train = y_train
#Y_test = y_test


#model = Sequential()
#model.add(Dense(200, input_dim = 30))
#model.add(Activation('tanh'))
#model.add(Dense(100))
#model.add(Activation('sigmoid'))
#model.add(Dense(50))
#model.add(Activation('tanh'))
#model.add(Dense(30))
#model.add(Activation('tanh'))
#model.add(Dense(20))
#model.add(Activation('tanh'))
#model.add(Dense(6))
#model.add(Activation('softmax'))
#model.compile(optimizer = 'rmsprop', loss = 'mse', metrics=['accuracy'])
#model.fit(X_train, Y_train, batch_size=10, nb_epoch=100, verbose=1, validation_split=0.2, shuffle=True)
#prob = model.predict(X_test)
#prob=model.evaluate(X_test, Y_test, batch_size=32, verbose=1)
#print prob

model = Sequential()
model.add(LSTM(256, input_shape=(50,6),return_sequences=True))# 32
#model.add(Dropout(0.5))
model.add(LSTM(128,return_sequences=True))

#model.add(Dropout(0.5))
model.add(LSTM(64,return_sequences=True))
model.add(Dropout(0.5))
model.add(LSTM(50,return_sequences=True))
#model.add(Flatten())
#model.add(Dense(50))

model.compile(optimizer = 'rmsprop', loss = 'mse', metrics=['accuracy'])
model.fit(X_train, Y_train, batch_size=100, nb_epoch=10, verbose=1, validation_split=0.2, shuffle=True)

#json_string = model.to_json()
#open('my_model_architecture.json', 'w').write(json_string)
#model.save_weights('my_model_weights.h5')


prob = model.predict(X_test)
#n=0
#for i in prob:
#    if n<100:
#        print i
#        n=n+1

#    else:break
print prob


#model = Sequential()
#model.add(LSTM(32, input_shape=(6,5),return_sequences=True))
#model.add(Activation('sigmoid'))
#model.add(LSTM(16,return_sequences=True))
#model.add(Activation('sigmoid'))
#model.add(LSTM(8,return_sequences=True))
#model.add(Activation('sigmoid'))
#model.add(LSTM(6))
#model.add(Activation('tanh'))
#model.compile(optimizer = 'rmsprop', loss = 'mse', metrics=['accuracy'])
#model.fit(X_train, Y_train, batch_size=100, nb_epoch=100, verbose=1, validation_split=0.2, shuffle=True)
#prob = model.predict(X_test)
#n=0
#for i in prob:
#    if n<100:
#        print i
#        n=n+1

#    else:break
#print prob
