#!/usr/bin/env python2.7
# -*- coding:utf-8 -*-

import sklearn.datasets as skds
import numpy as np
import random
import theano.tensor as T
import theano
import matplotlib.pyplot as plt
import math

#I don't know what the jesus 'housing.data' means so I used self-generated dataset
x = np.arange(-50., 50., 1)
y = np.array(map(lambda tmp: 1.0/(1 + math.exp(-3 * tmp + 5.0)), x))
noise = np.random.uniform(-0.1, .1, size=len(x))
y += noise


print x
print y

#declarations
theta = theano.shared(np.random.uniform(-0.1, 0.1))
omega = theano.shared(np.random.uniform(-0.1, 0.1))
X = T.dscalar('X')
Y = T.dscalar('Y')

#functions
prediction = 1/(1 + T.exp(-omega * X + theta))
loss1 = -Y * T.log(prediction)
loss2 = 1/2.0 * (prediction - Y) ** 2
predict = theano.function([X], prediction)
calculate_loss = theano.function([X, Y], loss2)
print predict(1.0)

#derivatives
dX = T.grad(loss2, X)
dtheta = T.grad(loss2, theta)
domega = T.grad(loss2, omega)
epsilon = .01

#gradient function
gradient_step = theano.function(
            [X, Y],
            updates=((omega, omega - epsilon * domega),
                     (theta, theta - epsilon * dtheta)))

#optimization
for i in range(100):
    loss = 0
    for j in range(len(x)):
        gradient_step(x[j], y[j])
        loss += calculate_loss(x[j], y[j])
    print 'loss after' + str(i) + 'iterations.' + str(loss)

print x
print y

mul = 1 - 1/len(x)

plt.xlim(x.min() * mul, x.max() * mul)
plt.ylim(y.min() * mul, y.max() * mul)
plt.xlabel('x')
plt.ylabel('y')
plt.title('lr test')
plt.plot(x, y, 'ro')
xx = np.arange(x.min(), x.max(), 0.1)
yy = map(lambda abc: predict(abc), xx)
plt.plot(xx, yy, 'b')
plt.show()

# vim: ts=4 sw=4 sts=4 expandtab
