from os import makedirs
from os.path import dirname, exists, realpath
from moviepy.video.io.VideoFileClip import VideoFileClip



class Slides:
    intervals = None

    def __init__(self, config, events, video_filename, slides_dir):
        # extract the set of from_time/to_time properties with corresponding pause set to true
        min_timepoint = 0
        max_timepoint = max(e.to_time for e in events)
        set_of_timepoints = {min_timepoint, max_timepoint}
        set_of_timepoints.update(e.from_time for e in events if not e.from_time_slide)
        set_of_timepoints.update(e.to_time for e in events if e.to_time_slide)

        # store timepoints into a sorted array
        timepoints = {min(max(t + config.synch_slide, min_timepoint), max_timepoint) for t in set_of_timepoints}
        sorted_timepoints = [t for t in timepoints]
        sorted_timepoints.sort()

        # compute intervals of adjacent timepoints
        self.intervals = [(x, y) for (x, y) in zip(sorted_timepoints[0:], sorted_timepoints[1:]) if x < y]

        # split video into chunks 
        split_video(video_filename, slides_dir + "/assets/slide", self.intervals)

        # save html
        template_dir = dirname(realpath(__file__)) + "/assets/"
        template_pre = load(template_dir + "template_pre.html")
        template_sep = load(template_dir + "template_sep.html")
        template_post = load(template_dir + "template_post.html")
        html = template_pre
        for i in range(len(self.intervals)):
            html += "assets/slide_" + str(i + 1) + ".mp4"
            if i < len(sorted_timepoints[1:]) - 1:
                html += template_sep
        html += template_post

        save(slides_dir + "/index.html", html)



def load(filename):
    with open(filename, 'r') as file:
        return file.read()


def save(filename, data):
    with open(filename, 'w') as file:
        file.write(data)


def split_video(input_file, output_prefix, intervals):
    # Load the video clip
    video_clip = VideoFileClip(input_file)

    # Ensure the output directory exists
    output_dir = dirname(output_prefix)
    if not exists(output_dir):
        makedirs(output_dir)

    # Split the video at specified time points
    for i, interval in enumerate(intervals):
        start_time = interval[0]
        end_time = interval[1]
        assert start_time <= end_time, f"Unexpected error: cannot split video with interval."
        print(f"Splitting [{start_time}, {end_time}]\n")

        # Create a subclip
        subclip = video_clip.subclip(start_time, end_time)

        # Output file name
        output_file = f"{output_prefix}_{i + 1}.mp4"

        # Write the subclip to a file
        subclip.write_videofile(output_file, codec="libx264")

    # Close the video clip
    video_clip.close()
