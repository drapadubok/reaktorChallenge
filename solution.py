"""
Created on Tue May 10 10:33:51 2016

@author: smirnod1
"""
from __future__ import division

import math
from collections import defaultdict

import numpy as np
import requests
import networkx as nx


def toRadians(a):
    return a * math.pi / 180.0


def convertToCartesian(point, r=6371):
    """
    point[0] - latitude
    point[1] - longitude
    point[2] - altitude
    """
    rho = r + point[2]
    lat = toRadians(point[0])
    lng = toRadians(point[1])
    
    x = rho * math.cos(lat) * math.cos(lng)
    y = rho * math.cos(lat) * math.sin(lng)
    z = rho * math.sin(lat)
    
    return np.array([x, y, z])


def isIntersectingGlobe(P1, P2, Q, r):
    """
    P1, P2 - points defining line segment
    Q - sphere centre
    r - radius
    """

    V = P2 - P1
    
    a = np.dot(V, V)
    b = 2 * np.dot(V, P1-Q)
    c = np.dot(P1, P1) + np.dot(Q, Q) - 2 * np.dot(P1, Q) - r**2
    
    d = b**2 - 4 * a * c
    
    if d < 0:
        return False # line misses the circle completely, not even extended
    sqrt_d = math.sqrt(d)
    t1 = (-b + sqrt_d) / (2 * a)
    t2 = (-b - sqrt_d) / (2 * a)
    # Make sure it's line segment, otherwise you spend 2 days debugging like i did
    if not (0 <= t1 <= 1 or 0 <= t2 <= 1):
        return False
    return True
    
    
def vectorLength(a, b):
    return math.sqrt(sum((b-a)**2))


## Execute
if __name__ == '__main__':

    url = 'https://space-fast-track.herokuapp.com/generate'
    
    # Load data, split lines
    print("Loading the data from: " + url)
    data = requests.get(url).content
    print("Got it, check this out: ")
    print(data)
    lines = data.split("\n")
    slines = [i.split(",") for i in lines]
    
    # Keep the seed, parse the start and end coordinates
    seed = slines.pop(0)[0].split(" ")[1]
    routes = slines.pop()
    start = convertToCartesian(np.array([routes[1],routes[2],5], dtype=float))
    end = convertToCartesian(np.array([routes[3],routes[4],5], dtype=float))
    
    # For easier lookup, store SAT# -> coordinates map
    points = defaultdict()
    for i, line in enumerate(slines):
        points[i] = convertToCartesian(np.array([line[1],line[2],line[3]], dtype=float))
    
    # c will be the center of the sphere (earth)
    c = np.array([0.0, 0.0, 0.0])
    r = 6371
    
    # append the start and end nodes
    points[20] = start
    points[21] = end
    
    # generate adjacency matrix
    ndim = len(points)
    A = np.zeros([ndim, ndim])
    for i in points.iterkeys():
        for j in points.iterkeys():
            if i != j:
                if isIntersectingGlobe(points[i], points[j], c, r):
                    A[i,j] = 0.0
                else:
                    vlen = vectorLength(points[i], points[j])
                    A[i,j] = vlen
                    print("Found link between nodes: {0} and {1}".format(i,j))
    
    # Dijstra shortest path algorithm for rescue
    G = nx.from_numpy_matrix(A, create_using=nx.DiGraph())
    p = nx.dijkstra_path(G, 20, 21)
    # Throw out the start and end node
    p.pop(0)
    p.pop()
    print("Using Dijkstra's shortest path algorithm, resulting path is: ")
    result = ",".join(["SAT" + str(i) for i in p])
    print(result)
    
