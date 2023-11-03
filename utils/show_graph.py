import json
import networkx as nx
import matplotlib.pyplot as plt
import sys
import random
import argparse

parser = argparse.ArgumentParser()
parser.add_argument(
    "-p",
    "--print",
    action="store_true",
    help="show a graph representation",
)
parser.add_argument(
    "in_file", nargs="?", type=argparse.FileType("r"), default=sys.stdin
)
args = parser.parse_args()

jsondoc = json.load(args.in_file)

G = nx.Graph()
for n in jsondoc["nodes"]:
    G.add_node(n)
for edge in [
    {"u_of_edge": edge[0], "v_of_edge": edge[1], "weight": edge[2]}
    for edge in jsondoc["edges"]
]:
    G.add_edge(**edge)

components = {c[0]: c[1] for c in jsondoc["components"]}


mst_forest = nx.minimum_spanning_tree(G)
msts = [
    nx.subgraph_view(
        mst_forest, filter_node=lambda node, mst_nodes=mst_nodes: node in mst_nodes
    )
    for mst_nodes in list(nx.connected_components(G))
]
msts_components = {max(mst.nodes): mst.size(weight="weight") for mst in msts}

# print(f"erl={components}")
# print(f"truth={msts_components}")

if args.print:
    graph_layout = nx.kamada_kawai_layout(G, weight=None)
    nx.draw_networkx(G, graph_layout, node_color="#60cdd1")
    edge_labels = nx.get_edge_attributes(G, "weight")
    nx.draw_networkx_edge_labels(G, graph_layout, edge_labels)

    # Set margins for the axes so that nodes aren't clipped
    ax = plt.gca()
    ax.margins(0.20)
    plt.axis("off")
    plt.show()

sys.exit(0 if components == msts_components else -1)
