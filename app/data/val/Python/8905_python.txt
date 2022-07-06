# -*- coding: utf-8 -*-
# 3章 ニューラルネットワーク
import numpy as np


class NeuralTrain:
    def step_function(self, x):
        return np.array(x > 0, dtype=np.int)

    def sigmoid_function(self, x):
        return 1 / (1 + np.exp(-x))

    def relu_function(self, x):
        return np.maximum(0, x)