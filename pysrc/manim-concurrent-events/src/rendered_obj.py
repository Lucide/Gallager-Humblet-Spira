from math import pow
from config import Config
from layout import Layout

from manim import VMobject, Dot, Line
from manim.constants import *
from manim.utils.space_ops import *



class RenderedObj(VMobject):
    def __init__(self, config, layout, event, grown=False, shrunk=False):
        # constructs a node, i.e. a Dot instance
        if event.is_node:
            self.__class__ = Dot
            Dot.__init__(self, color = event.color, z_index = 2)

            self.set_stroke(event.color, width = 0, opacity = 0)
            self.set_fill(event.color)
            self.set_opacity(event.opacity)
            if grown:
                self.scale(event.dot_size * config.dot_grow_ratio)
            elif shrunk:
                self.scale(event.dot_size * config.dot_shrink_ratio)
            else:
                self.scale(event.dot_size)
            self.move_to(layout.positions[event.location])

        # constructs a movable token, i.e. a Dot instance with target
        elif event.is_link and event.effect == 'token': 
            if type(event.color) in {tuple, list}:
                from_color = event.color[0]
                to_color = event.color[-1]
                if len(event.color) > 2:
                    print(f"Warning: token effect with gradient with more than 2 colors is not yet implemented (two colors will be used).")
            else:
                from_color = to_color = event.color

            self.__class__ = Dot
            Dot.__init__(self, color = from_color, z_index = 2)

            self.set_stroke(from_color, width = 0, opacity = 0)
            self.set_fill(from_color)
            self.set_opacity(event.opacity)
            if grown:
                self.scale(event.dot_size * config.dot_grow_ratio)
            elif shrunk:
                self.scale(event.dot_size * config.dot_shrink_ratio)
            else:
                self.scale(event.dot_size)
            self.move_to(layout.positions[event.location[0]])
            self.generate_target()
            self.target.move_to(layout.positions[event.location[1]])
            self.target.set_stroke(to_color, width = 0, opacity = 0)
            self.target.set_fill(to_color)

        # constructs a link, i.e. a Line instance with a possible Tip
        elif event.is_link: 
            from_pos = layout.positions[event.location[0]]
            to_pos = layout.positions[event.location[1]]
            vector = to_pos - from_pos
            angle = angle_of_vector(vector)
            if grown:
                shortened_from_pos = from_pos + rotate_vector(RIGHT * event.from_gap * config.tip_grow_ratio, angle)
                shortened_to_pos = to_pos + rotate_vector(LEFT * event.to_gap * config.tip_grow_ratio, angle)
            else:
                shortened_from_pos = from_pos + rotate_vector(RIGHT * event.from_gap, angle)
                shortened_to_pos = to_pos + rotate_vector(LEFT * event.to_gap, angle)

            self.__class__ = Line
            if event.tip:
                Line.__init__(self, shortened_from_pos, shortened_to_pos, z_index = 1)
            else:
                Line.__init__(self, shortened_from_pos, shortened_to_pos)
            
            if event.shifted:
                self.shift(rotate_vector(UP * event.shift_amount, angle))

            if grown:
                self.set_stroke(event.color, width = event.line_width * config.line_grow_ratio)
            elif shrunk:
                self.set_stroke(event.color, width = event.line_width * config.line_shrink_ratio)
            else:
                self.set_stroke(event.color, width = event.line_width)
            self.set_opacity(event.opacity)

            if event.tip:
                self.add_tip(tip_length = event.tip_length, tip_width = event.tip_width)
                self.tip.set_fill(event.color)
                self.tip.set_stroke(event.color, width = 0, opacity = 0)
                self.tip.set_opacity(event.opacity)
                if grown:
                    self.tip.scale(config.tip_grow_ratio)
                elif shrunk:
                    self.tip.scale(config.tip_shrink_ratio)
