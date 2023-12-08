import math
import json
import time
import random
import argparse
import networkx as nx
import mplcursors
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi


def distance(p, q):
    return math.sqrt((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2)


def generate(num_nodes):
    random.seed(int(time.time()))
    positions = [[random.uniform(0, 1), random.uniform(0, 1)] for _ in range(num_nodes)]

    for i in range(len(positions)):
        c = 0
        while any(distance(positions[i], q) < 0.2 for q in positions[:i]):
            positions[i] = [random.uniform(0, 1), random.uniform(0, 1)]
            c += 1
            if c > 10000:
                raise AssertionError("Cannot find space for new point")

    graph = nx.Graph()
    vor = Voronoi(positions)
    for simplex in vor.ridge_vertices:
        if -1 not in simplex:
            i, j = simplex
            p = vor.vertices[i]
            q = vor.vertices[j]
            if 0 <= p[0] <= 1 and 0 <= p[1] <= 1 and 0 <= q[0] <= 1 and 0 <= q[1] <= 1:
                dist = distance(p, q)
                graph.add_node(i, pos=p)
                graph.add_node(j, pos=q)
                graph.add_edge(i, j, weight=dist)
    return graph


def export(graph, filename):
    data = {
        "nodes": [
            (id, (v["pos"][0] * 7, v["pos"][1] * 4)) for id, v in graph.nodes(data=True)
        ],
        "edges": [(u, v, uv["weight"]) for (u, v, uv) in graph.edges(data=True)],
    }

    with open(filename, "w") as json_file:
        json.dump(data, json_file, indent=2)


def show(graph):
    pos = nx.get_node_attributes(graph, "pos")
    nx.draw_networkx(graph, pos=pos, node_size=10, with_labels=False)
    mplcursors.cursor(hover=True)
    plt.show()


def on_pick(event, graph, pos):
    print("hello")
    if isinstance(event.artist, plt.Circle):
        x, y = event.mouseevent.xdata, event.mouseevent.ydata
        node = find_nearest_node(graph, pos, x, y)
        if node is not None:
            picked_node["node"] = node
            picked_node["pos"] = pos[node]


def on_motion(event, graph, pos):
    if "node" in picked_node:
        x, y = event.xdata, event.ydata
        node = picked_node["node"]
        pos[node] = (x, y)
        update_plot(graph, pos)


def find_nearest_node(graph, pos, x, y):
    distances = [
        (node, (x - pos[node][0]) ** 2 + (y - pos[node][1]) ** 2)
        for node in graph.nodes()
    ]
    node, _ = min(distances, key=lambda x: x[1])
    return node


def update_plot(graph, pos):
    plt.clf()
    nx.draw_networkx(graph, pos=pos, node_size=10, with_labels=False)
    mplcursors.cursor(hover=True)
    plt.draw_idle()


picked_node = {}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate a random geometric graph with weights and export it to JSON."
    )
    parser.add_argument(
        "-n", "--num_nodes", type=int, default=30, help="Number of nodes in the graph"
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        default="json/graph.json",
        help="Path to JSON output",
    )
    parser.add_argument(
        "-q", "--quiet", action="store_true", help="Do not show the graph"
    )
    args = parser.parse_args()

    graph = generate(args.num_nodes)

    if not args.quiet:
        fig, ax = plt.subplots()
        show(graph)
        fig.canvas.mpl_connect("pick_event", lambda event: on_pick(event, graph, pos))
        fig.canvas.mpl_connect(
            "motion_notify_event", lambda event: on_motion(event, graph, pos)
        )
        plt.show()

    export(graph, args.output_file)
