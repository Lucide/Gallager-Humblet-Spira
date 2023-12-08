import networkx as nx

from manim.constants import *



class Layout:
    nodes = None
    links = None
    positions = None

    def __init__(self, config, timeline):
        # discover nodes and links
        self.nodes = []
        self.links = []
        for l in timeline.locations:
            if type(l) == tuple:
                self.links.append(l)
            else:
                self.nodes.append(l)

        # create nx graph
        graph = nx.Graph()
        for p in self.nodes:
            graph.add_node(p)
        for (p, q) in self.links:
            graph.add_edge(p, q)

        # compute layout
        internal_scaling = 1.0
        if config.layout == 'neato':
            layout = {}
            for e in timeline.events:
                if e.is_node and e.x != None and e.y != None:
                    layout[e.location] = [e.x, e.y]
            for p in self.nodes:
                if layout[p] == None:
                    print(f"Warning: at location {p} has no event with coordinates x,y.")
                    layout[p] = [0,0]
        elif config.layout == 'circular':
            layout = nx.circular_layout(graph)
        elif config.layout == 'kamada_kawai':
            layout = nx.kamada_kawai_layout(graph)
        elif config.layout == 'spring':
            layout = nx.spring_layout(graph)
        elif config.layout == 'spectral':
            layout = nx.spectral_layout(graph, weight = None)
        elif config.layout == 'shell':
            layout = nx.shell_layout(graph)
        elif config.layout in {'sfdp', 'dot', 'fdp', 'circo', 'twopi'}:
            layout = nx.nx_agraph.graphviz_layout(graph, prog = config.layout)
            internal_scaling = 0.005
        else:
            assert false, f"Unknown layout {self.layout_schema}"

        # compute node positions
        self.positions = {}
        for p in self.nodes:
            self.positions[p] = RIGHT * layout[p][0] * internal_scaling / config.objects_scale + \
                                UP * layout[p][1] * internal_scaling / config.objects_scale
