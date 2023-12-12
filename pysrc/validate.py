import json
import networkx as nx
import matplotlib.pyplot as plt
import sys
import io
import argparse


def get_msts(json_msts) -> dict[int, nx.DiGraph]:
    msts = {}
    for jsonMST in json_msts:
        mst = nx.DiGraph()
        for jsonNode in jsonMST["nodes"]:
            mst.add_node(jsonNode[0], pos=jsonNode[1])
        for edge in jsonMST["edges"]:
            mst.add_edge(u_of_edge=edge[0], v_of_edge=edge[1], weight=edge[2])
        msts[max(mst.nodes)] = mst
    return msts


def get_truth_msts(Graph) -> dict[int, nx.Graph]:
    mst_forest = nx.minimum_spanning_tree(Graph)
    msts = [
        nx.subgraph_view(
            mst_forest, filter_node=lambda node, mst_nodes=mst_nodes: node in mst_nodes
        )
        for mst_nodes in list(nx.connected_components(Graph))
    ]
    return {max(mst.nodes): mst for mst in msts}


parser = argparse.ArgumentParser()
parser.add_argument(
    "-p",
    "--print",
    action="store_true",
    help="show a graph representation",
)
parser.add_argument(
    "graph_file", type=argparse.FileType("r"), default="./json/graph.json"
)
parser.add_argument(
    "msts_file", nargs="?", type=argparse.FileType("r"), default=sys.stdin
)
args = parser.parse_args()

json_mst = json.load(fp=args.graph_file)
try:
    json_msts = json.load(io.TextIOWrapper(args.msts_file.buffer, encoding="utf-8-sig"))
except:
    json_msts = json.load(args.msts_file)

Graph = nx.Graph()
for jsonNode in json_mst["nodes"]:
    Graph.add_node(jsonNode[0], pos=jsonNode[1])
for edge in json_mst["edges"]:
    Graph.add_edge(u_of_edge=edge[0], v_of_edge=edge[1], weight=edge[2])

msts = get_msts(json_msts)
truth_msts = get_truth_msts(Graph)

keys = set.union(set(msts.keys()), set(truth_msts.keys()))

result = all(
    [
        id1 == id2
        for isomorphism in [
            nx.algorithms.isomorphism.rooted_tree_isomorphism(
                msts[key].to_undirected(as_view=True), key, truth_msts[key], key
            )
            for key in keys
        ]
        for id1, id2 in isomorphism
    ]
)

if args.print:
    pos = nx.get_node_attributes(Graph, "pos")
    roots = [node for mst in msts.values() for node, d in mst.out_degree() if d == 0]
    nx.draw_networkx(
        Graph,
        pos,
        node_size=70,
        font_size=6,
        node_color="#60cdd1",
        edge_color="#d1cfcf",
    )
    nx.draw_networkx_nodes(
        Graph, pos, nodelist=roots, node_size=100, node_color="#d1af60"
    )
    for mst in msts.values():
        nx.draw_networkx_edges(mst, pos, node_size=70, arrows=True)

    # Set margins for the axes so that nodes aren't clipped
    ax = plt.gca()
    ax.margins(0.20)
    plt.axis("off")
    plt.show()

print(f"{result}")
sys.exit(0 if result else -1)
