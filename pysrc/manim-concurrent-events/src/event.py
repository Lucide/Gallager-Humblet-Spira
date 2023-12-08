from copy import copy
from utils_globals import string_to_obj
from utils_timestamps import timestamp, partial_order



class Event:
    idx = None
    is_node = None
    is_link = None
    location = None
    
    x = None
    y = None

    effect = 'appear'
    persistent = True
    color = string_to_obj('color.BLACK')
    opacity = 1
    animation_rate = string_to_obj('rate_functions.smooth')

    from_time = float('-inf')
    to_time = float('-inf')
    from_time_offset = 0
    to_time_offset = 0
    from_time_slide = False
    to_time_slide = False
    
    tip = False
    shifted = False

    dot_size = 1
    line_width = 2
    from_gap = 0.18
    to_gap = 0.18
    tip_length = 0.1
    tip_width = 0.06
    shift_amount = 0.03


    def __init__(self, raw_event):
        assert 'idx' in raw_event, \
               f"Event {raw_event} must have 'idx' propery."
        self.idx = raw_event['idx']
        self.is_node = 'at' in raw_event
        self.is_link = 'from' in raw_event and 'to' in raw_event
        assert self.is_node or self.is_link, \
               f"Event {raw_event} must have 'at' property or both 'from' and 'to' properties."
        self.location = raw_event['at'] if self.is_node else (raw_event['from'], raw_event['to']) 

        if 'x' in raw_event:
            self.x = raw_event['x']
        if 'y' in raw_event:
            self.y = raw_event['y']

        if 'effect' in raw_event:
            self.effect = raw_event['effect']
        if 'persistent' in raw_event:
            self.persistent = raw_event['persistent']
        if 'color' in raw_event:
            if type(raw_event['color']) == str:
                self.color = string_to_obj('color.' + raw_event['color'])
            elif type(raw_event['color']) in {tuple, list}:
                self.color = [string_to_obj('color.' + c) for c in raw_event['color']]
        if 'opacity' in raw_event:
            self.opacity = raw_event['opacity']
        if 'animation_rate' in raw_event:
            self.animation_rate = string_to_obj('rate_functions.' + raw_event['animation_rate'])

        if 'from_time' in raw_event and 'to_time' in raw_event:
            self.from_time = timestamp(raw_event['from_time'])
            self.to_time = timestamp(raw_event['to_time'])
        if 'from_time_offset' in raw_event:
            self.from_time_offset = raw_event['from_time_offset']
        if 'to_time_offset' in raw_event:
            self.to_time_offset = raw_event['to_time_offset']
        if 'from_time_slide' in raw_event:
            self.from_time_pause = raw_event['from_time_slide']
        if 'to_time_slide' in raw_event:
            self.to_time_pause = raw_event['to_time_slide']

        if 'tip' in raw_event:
            self.tip = raw_event['tip']
        if 'shifted' in raw_event:
            self.shifted = raw_event['shifted']

        if 'dot_size' in raw_event:
            self.dot_size = raw_event['dot_size']
        if 'line_width' in raw_event:
            self.line_width = raw_event['line_width']
        if 'from_gap' in raw_event:
            self.from_gap = raw_event['from_gap']
        if 'to_gap' in raw_event:
            self.to_gap = raw_event['to_gap']
        if 'tip_length' in raw_event:
            self.tip_length = raw_event['tip_length']
        if 'tip_width' in raw_event:
            self.tip_width = raw_event['tip_width']
        if 'shift_amount' in raw_event:
            self.shift_amount = raw_event['shift_amount']


    def copy(self):
        return copy(self)

