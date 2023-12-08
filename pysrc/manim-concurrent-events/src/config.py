from utils_yamljson import load

from manim.utils.color import *



class Config:
    layout = 'shortest-path'
    objects_scale = 1

    foreground = 'BLACK'
    background = 'WHITE'

    timestamps = 'logical'
    lex_order_on = []
    time_scale = 0.5

    synch_slide = 0
    pre_delay = 0
    post_delay = 0

    dot_grow_ratio = 2
    dot_shrink_ratio = 0
    line_grow_ratio = 2
    line_shrink_ratio = 0
    tip_grow_ratio = 2
    tip_shrink_ratio = 0


    def __init__(self):
        pass

    def __init__(self, filename):
        file = load(filename)

        if 'layout' in file:
            self.layout = file['layout']
        if 'objects_scale' in file:
            self.objects_scale = float(file['objects_scale'])

        if 'foreground' in file:
            self.foreground = globals()[file['foreground']]
        if 'background' in file:
            self.background = globals()[file['background']]

        if 'timestamps' in file:
            self.timestamps = file['timestamps']
        if 'lex_order_on' in file:
            self.lex_order_on = file['lex_order_on']
        if 'time_scale' in file:
            self.time_scale = float(file['time_scale'])

        if 'synch_slide' in file:
            self.synch_slide = float(file['synch_slide'])
        if 'pre_delay' in file:
            self.pre_delay = float(file['pre_delay'])
        if 'post_delay' in file:
            self.post_delay = float(file['post_delay'])

        if 'dot_grow_ratio' in file:
            self.dot_grow_ratio = float(file['dot_grow_ratio'])
        if 'dot_shrink_ratio' in file:
            self.dot_shrink_ratio = float(file['dot_shrink_ratio'])
        if 'line_grow_ratio' in file:
            self.line_grow_ratio = float(file['line_grow_ratio'])
        if 'line_shrink_ratio' in file:
            self.line_shrink_ratio = float(file['line_shrink_ratio'])
        if 'tip_grow_ratio' in file:
            self.tip_grow_ratio = float(file['tip_grow_ratio'])
        if 'tip_shrink_ratio' in file:
            self.tip_shrink_ratio = float(file['tip_shrink_ratio'])
