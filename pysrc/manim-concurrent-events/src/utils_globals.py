from manim.utils import color, rate_functions



def string_to_obj(s):
    tokens = s.split('.')
    obj = globals()[tokens[0]]
    for tok in tokens[1:]:
        obj = getattr(obj, tok)
    return obj
