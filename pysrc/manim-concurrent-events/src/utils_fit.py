import numpy as np



def bounding_box(objects, buff=0):
    box = np.array([[float("inf"), float("+inf"), 0], [float("-inf"), float("-inf"), 1]])
    for o in objects:
        box[0][0] = min(box[0][0], o.get_left()[0] - buff)
        box[1][0] = max(box[1][0], o.get_right()[0] + buff)
        box[0][1] = min(box[0][1], o.get_bottom()[1] - buff)
        box[1][1] = max(box[1][1], o.get_top()[1] + buff)
    return box

def move_to_fit(objects, box):
    scene = np.array([[-7, -4, 0], [7, 4, 1]])
    for o in objects:
        o.move_to((o.get_center() - box[0]) * (scene[1] - scene[0]) / (box[1] - box[0]) + scene[0])

def proportionmove_to_fit(objects, box):
    scene = np.array([[-7, -4, 0], [7, 4, 1]])
    coeff = min((scene[1][0] - scene[0][0]) / (box[1][0] - box[0][0]),
                (scene[1][1] - scene[0][1]) / (box[1][1] - box[0][1]))
    for o in objects:
        o.move_to((o.get_center() - 0.5 * (box[0] + box[1])) * coeff + 0.5 * (scene[0] + scene[1]))

def scale_to_fit(objects, box):
    scene = np.array([[-7, -4, 0], [7, 4, 1]])
    for o in objects:
        o.stretch((scene[1][0] - scene[0][0]) / (box[1][0] - box[0][0]), dim=0)
        o.stretch((scene[1][1] - scene[0][1]) / (box[1][1] - box[0][1]), dim=1)

def proportionscale_to_fit(objects, box):
    scene = np.array([[-7, -4, 0], [7, 4, 1]])
    coeff = min((scene[1][0] - scene[0][0]) / (box[1][0] - box[0][0]),
                (scene[1][1] - scene[0][1]) / (box[1][1] - box[0][1]))
    for o in objects:
        o.scale(coeff)


def camera_fit(camera_frame, box):
    scene = np.array([[-7, -4, 0], [7, 4, 1]])
    coeff = min((scene[1][0] - scene[0][0]) / (box[1][0] - box[0][0]),
                (scene[1][1] - scene[0][1]) / (box[1][1] - box[0][1]))
    camera_frame.scale(1 / coeff)
    camera_frame.move_to((box[0] + box[1]) / 2)
