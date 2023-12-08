from rules import Rules
from config import Config
from timeline import Timeline
from layout import Layout
from rendered_obj import RenderedObj
from rendered_anim import RenderedAnim
from slides import Slides

from utils_fit import camera_fit, bounding_box

from subprocess import run
from argparse import ArgumentParser

from manim import MovingCameraScene
from manim.animation.composition import AnimationGroup, Succession
from manim.utils.file_ops import open_file 
from manim import config



class RenderedEvents(MovingCameraScene):
    rules = None
    config = None
    timeline = None
    layout = None
    animation = None

    def __init__(self, rules_filename, config_filename, events_filename, debug_filename):
        MovingCameraScene.__init__(self)
        # load rules, configuration, and events, and compute timeline and layout
        self.rules = Rules(rules_filename)
        self.config = Config(config_filename)
        self.timeline = Timeline(self.rules, self.config, events_filename, debug_filename)
        self.layout = Layout(self.config, self.timeline)

    def construct(self):
        # configure camera to fit all rendered objects that may appear
        self.camera.background_color = self.config.background
        self.camera.init_background()
        objects = [RenderedObj(self.config, self.layout, event) for event in self.timeline.events]
        camera_fit(self.camera.frame, bounding_box(objects, buff=0.2))

        # generate sequences of animations for each location
        animations_per_location = {}
        for loc in self.timeline.locations:
            animations_per_location[loc] = RenderedAnim(self.config, self.layout)
        for loc in self.timeline.locations:
            for event in self.timeline.sorted_events_per_location[loc]:
                animations_per_location[loc].add(self.config, event)


        # merge animations and play them
        self.animation = AnimationGroup(*[Succession(*animations_per_location[loc].persistent) for loc in self.timeline.locations], \
                                        *[anim for loc in self.timeline.locations for anim in animations_per_location[loc].transient], \
                                        lag_ratio = 0)
        self.play(self.animation)        
        self.wait(1) 



if __name__ == '__main__':
    arg_parser = ArgumentParser(description='Manim-based renderer of events')
    arg_parser.add_argument('--events', type=str, default='events.yaml', help='Name of .yaml file with events (default: events.yaml)')
    arg_parser.add_argument('--rules', type=str, default='rules.yaml', help='Name of .yaml file with completion rules for events (default: rules.yaml)')
    arg_parser.add_argument('--config', type=str, default='config.yaml', help='Name of .yaml file with configurations for the rendering (default: config.yaml)')
    arg_parser.add_argument('--debug', type=str, default='', help='Name of .yaml debug file (default: empty)')
    arg_parser.add_argument('--media', type=str, default='__media__', help='Folder where media files will be written (default: __media__)')
    arg_parser.add_argument('--slides', type=str, default='__slides__', help='Folder where html slides will be written (default: __slides__)')
    arg_parser.add_argument('--skip-slides', action='store_true', help='skip slides generation')
    arg_parser.add_argument('--quality', type=str, default='low_quality', help='Quality of media (default: low_quality)')
    arg_parser.add_argument('--verbosity', type=str, default='WARNING', help='Verbosity (default: WARNING)')
    args = arg_parser.parse_args()

    config.media_dir = args.media
    config.quality = args.quality
    config.verbosity = args.verbosity

    # first create an .mp4 video with normal background and splits into slides
    scene = RenderedEvents(args.rules, args.config, args.events, args.debug)
    scene.render() 
    if not args.skip_slides:
        # open_file(scene.renderer.file_writer.movie_file_path)
        slides = Slides(scene.config, scene.timeline.events, \
                        f"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents.mp4", \
                        args.slides)

    # then create a .mov video with transparent background and export it to .png images (animation + first and last frames)
    config.transparent = 1.0
    scene = RenderedEvents(args.rules, args.config, args.events, args.debug)
    scene.render() 
    run(f"ffmpeg -i \"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents.mp4.mov\" -f apng -plays 0 -y " + \
        f"\"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents.png\"", shell=True)
    run(f"ffmpeg -i \"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents.mp4.mov\" -y -vframes 1 -f image2 " + \
        f"\"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents-first-frame.png\"", shell=True)
    run(f"ffmpeg -i \"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents.mp4.mov\" -y -update 1 -q:v 1 " + \
        f"\"{args.media}/videos/{scene.renderer.file_writer.get_resolution_directory()}/RenderedEvents-last-frame.png\"", shell=True)
