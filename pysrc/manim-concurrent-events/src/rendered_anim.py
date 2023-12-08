from config import Config
from layout import Layout
from rendered_obj import RenderedObj

from manim import Dot, Line
from manim.animation.fading import FadeIn, FadeOut
from manim.animation.animation import Wait, Animation
from manim.animation.transform import MoveToTarget
from manim.animation.indication import ShowPassingFlash
from manim.animation.composition import Succession



class RenderedAnim:
    config = None
    layout = None
    obj = None
    persistent = None
    transient = None
    time = None

    def __init__(self, config, layout):
        self.config = config
        self.layout = layout
        self.obj = None
        self.persistent = []
        self.transient = []
        self.time = 0

    def add(self, config, event):
        if event.persistent and self.time > event.from_time:
            print(f"Event #{event.idx} was planned to start at {event.from_time}, before current time {self.time}.")
            print(f"This should not have happened and I am skipping this event.")
            return

        duration = (event.to_time - event.from_time) - (config.pre_delay + config.post_delay)

        anim = [Wait(run_time = config.pre_delay)]

        if event.effect == 'appear':
            if event.persistent:
                if self.obj == None:
                    self.obj = RenderedObj(self.config, self.layout, event)
                    anim.append(FadeIn(self.obj, run_time = 0))
                else:
                    self.obj.target = RenderedObj(self.config, self.layout, event)
                    anim.append(MoveToTarget(self.obj, run_time = 0))
                anim.append(Wait(run_time = duration))
            else:
                tmp_obj = RenderedObj(self.config, self.layout, event)
                anim.append(FadeIn(tmp_obj, remover = True, run_time = 0))
                anim.append(Wait(run_time = duration))
                anim.append(FadeOut(tmp_obj, introducer = True, run_time = 0))

        elif event.effect == 'fade':  
            if event.persistent:      
                if self.obj == None:
                    self.obj = RenderedObj(self.config, self.layout, event)
                    anim.append(FadeIn(self.obj, run_time = duration))
                else:                 # seems to not work with existing lines (they don't get overwritten)
                    self.obj.target = RenderedObj(self.config, self.layout, event)
                    anim.append(MoveToTarget(self.obj, run_time = duration))
            else:
                if self.obj == None:
                    tmp_obj = RenderedObj(self.config, self.layout, event)
                    anim.append(FadeIn(tmp_obj, remover = True, run_time = duration * 0.5))
                    anim.append(FadeOut(tmp_obj, introducer = True, run_time = duration * 0.5))
                else:
                    tmp_obj = self.obj.copy()
                    tmp_obj.target = RenderedObj(self.config, self.layout, event)
                    anim.append(FadeIn(tmp_obj, remover = True, run_time = 0))
                    anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration * 0.5))
                    anim.append(FadeOut(tmp_obj, introducer = True, run_time = duration * 0.5))

        elif event.effect == 'grow':
            if event.persistent:
                if self.obj == None:
                    self.obj = RenderedObj(self.config, self.layout, event, shrunk = True)
                    anim.append(FadeIn(self.obj, run_time = 0))
                self.obj.target = RenderedObj(self.config, self.layout, event, grown = True)
                anim.append(MoveToTarget(self.obj, run_time = duration * 0.5))
                self.obj.target = RenderedObj(self.config, self.layout, event)
                anim.append(MoveToTarget(self.obj, run_time = duration * 0.5))
            else:
                if self.obj == None:
                    tmp_obj = RenderedObj(self.config, self.layout, event, shrunk = True)
                else:
                    tmp_obj = self.obj.copy()
                tmp_obj.target = RenderedObj(self.config, self.layout, event, grown = True)
                tmp_obj.target.target = tmp_obj.copy()
                anim.append(FadeIn(tmp_obj, remover = True, run_time = 0))
                anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration * 0.5))
                anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration * 0.5))

        elif event.effect == 'shrink':
            if event.persistent:
                if self.obj == None:
                    anim.append(Wait(run_time = duration))
                else:
                    self.obj.generate_target()
                    anchor1 = self.obj.get_center() if isinstance(self.obj, Dot) else self.obj.get_end()
                    anchor2 = self.obj.get_center() if isinstance(self.obj, Dot) else self.obj.get_start()
                    self.obj.target.scale(0)
                    self.obj.target.move_to(anchor1)
                    anim.append(MoveToTarget(self.obj, remover = True, run_time = duration * 0.5))
                    self.obj = RenderedObj(self.config, self.layout, event)
                    self.obj.generate_target()
                    self.obj.scale(0)
                    self.obj.move_to(anchor2)
                    anim.append(MoveToTarget(self.obj, run_time = duration * 0.5))
            else:
                tmp_obj = RenderedObj(self.config, self.layout, event, grown = (self.obj != None and isinstance(obj, Line)))
                anchor1 = tmp_obj.get_center() if isinstance(tmp_obj, Dot) else tmp_obj.get_start()
                anchor2 = tmp_obj.get_center() if isinstance(tmp_obj, Dot) else tmp_obj.get_end()
                tmp_obj.generate_target()
                tmp_obj.scale(0)
                tmp_obj.move_to(anchor1)
                anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration * 0.5))
                tmp_obj.generate_target()
                tmp_obj.target.scale(0)
                tmp_obj.target.move_to(anchor2)
                anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration * 0.5))

        elif event.effect == 'token' and not event.persistent and event.is_link:
            tmp_obj = RenderedObj(self.config, self.layout, event)
            anim.append(FadeIn(tmp_obj, remover = True, run_time = 0))
            anim.append(MoveToTarget(tmp_obj, remover = True, introducer = True, run_time = duration))
            anim.append(FadeOut(tmp_obj, introducer = True, run_time = 0))

        elif event.effect == 'message' and not event.persistent and event.is_link:
            if type(event.color) in {tuple, list}:
                print(f"Warning: message effect with gradient not yet implemented (one color will be used).")
                event.color = event.color[0]
            tmp_event = event.copy()
            tmp_event.tip = False
            tmp_obj = RenderedObj(self.config, self.layout, tmp_event, grown = True)
            anim.append(ShowPassingFlash(tmp_obj, run_time = duration, time_width = 0.7))

        else:
            print(f"Warning: {'persistent' if event.persistent else 'transient'} {event.effect} effect not yet implemented.")
            anim.append(Wait(run_time = duration))

        anim.append(Wait(run_time = config.post_delay))

        if event.persistent:
            self.persistent.extend([Wait(run_time = event.from_time - self.time), \
                                    Succession(*anim)])
            self.time = event.to_time
        else:
            self.transient.append(Succession(Wait(run_time = event.from_time), *anim))

