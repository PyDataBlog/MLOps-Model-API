__author__ = 'bruno'
import unittest
import algorithms.graphs.complementGraph as ComplementGraph


class TestComplementGraph(unittest.TestCase):
    def setUp(self):
        pass

    def test_complement_graph_1(self):
        graph = {'a': {'b': 1, 'c': 1},
                 'b': {'a': 1, 'c': 1, 'd': 1},
                 'c': {'a': 1, 'b': 1, 'd': 1},
                 'd': {'b': 1, 'c': 1}}
        self.assertEqual({'a': {'d': 1}, 'd': {'a': 1}},
                         ComplementGraph.make_complement_graph(graph))

    def test_complement_graph_2(self):
        graph = {'a': {'b': 1, 'd': 1},
                 'b': {'a': 1, 'c': 1},
                 'c': {'b': 1, 'd': 1},
                 'd': {'a': 1, 'c': 1}}
        complement = {'a': {'c': 1},
                      'b': {'d': 1},
                      'c': {'a': 1},
                      'd': {'b': 1}}
        self.assertEqual(complement, ComplementGraph.make_complement_graph(graph))

    def test_complement_graph_3(self):
        graph = {'a': {'c': 1, 'd': 1},
                 'b': {'c': 1, 'd': 1},
                 'c': {'a': 1, 'b': 1},
                 'd': {'a': 1, 'b': 1, 'e': 1, 'f': 1},
                 'e': {'d': 1, 'f': 1},
                 'f': {'d': 1, 'e': 1}}

        complement = {'a': {'b': 1, 'e': 1, 'f': 1},
                      'b': {'a': 1, 'e': 1, 'f': 1},
                      'c': {'e': 1, 'd': 1, 'f': 1},
                      'd': {'c': 1},
                      'e': {'a': 1, 'c': 1, 'b': 1},
                      'f': {'a': 1, 'c': 1, 'b': 1}}
        self.assertEqual(complement, ComplementGraph.make_complement_graph(graph))

    def test_complement_graph_4(self):
        graph = {'a': {'b': 1, 'f': 1},
                 'b': {'a': 1, 'c': 1},
                 'c': {'b': 1, 'd': 1},
                 'd': {'c': 1, 'e': 1},
                 'e': {'d': 1, 'f': 1},
                 'f': {'a': 1, 'e': 1}}

        complement = {'a': {'c': 1, 'e': 1, 'd': 1},
                      'b': {'e': 1, 'd': 1, 'f': 1},
                      'c': {'a': 1, 'e': 1, 'f': 1},
                      'd': {'a': 1, 'b': 1, 'f': 1},
                      'e': {'a': 1, 'c': 1, 'b': 1},
                      'f': {'c': 1, 'b': 1, 'd': 1}}
        self.assertEqual(complement, ComplementGraph.make_complement_graph(graph))

    def test_complement_graph_5(self):
        graph = {'a': {'b': 1, 'c': 1, 'd': 1, 'e': 1},
                 'b': {'a': 1, 'c': 1, 'd': 1, 'e': 1},
                 'c': {'a': 1, 'b': 1, 'd': 1, 'e': 1},
                 'd': {'a': 1, 'b': 1, 'c': 1, 'e': 1},
                 'e': {'a': 1, 'b': 1, 'c': 1, 'd': 1, 'f': 1},
                 'f': {'e': 1}}

        complement = {'a': {'f': 1},
                      'b': {'f': 1},
                      'c': {'f': 1},
                      'd': {'f': 1},
                      'f': {'a': 1, 'c': 1, 'b': 1, 'd': 1}}
        self.assertEqual(complement, ComplementGraph.make_complement_graph(graph))