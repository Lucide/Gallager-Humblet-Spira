import functools



# convert an object (e.g. string) into a proper timestamp
def timestamp(t):
    if t == '-inf':
        return float('-inf')
    elif t == '+inf':
        return float('+inf')
    elif isinstance(t, list):
        return [timestamp(x) for x in t]
    elif isinstance(t, dict):
        return {x: timestamp(y) for x, y in t.items()}
    elif isinstance(t, set):
        print(f"Warning: timestamp {t} of type set is not yet parsed/supported correctly.")
        return {timestamp(x) for x in t.items()}
    else:
        return t


# Compares two logical clocks with the appropriate partial order:
# - returns 0 if clocks are equal
# - returns None if clocks are incomparable 
# - returns -1 if first clock is strictly below second clock
# - returns +1 if first clock is strictly above second clock
# Timestamps can be boolean, integers, floats, lists, dictionaries, or sets
def partial_order(time1, time2, lex_order_on = []):
    if len(lex_order_on) > 0:
        k = lex_order_on[0]
        if type(time1) in {list, tuple}:
            t1 = time1[k] if k < len(time1) else float('-inf')
        elif type(time1) in {dict}:
            t1 = time1[k] if k in time1 else float('-inf')
        else:
            t1 = time1
        if type(time2) in {list, tuple}:
            t2 = time2[k] if k < len(time2) else float('-inf')
        elif type(time2) in {dict}:
            t2 = time2[k] if k in time2 else float('-inf')
        else:
            t2 = time2
        lex_ordering = partial_order(t1, t2)
        if lex_ordering in {-1, +1}:
            return lex_ordering
        else:
            return partial_order(time1, time2, lex_order_on[2:])

    # base case: both timestamps are atomic
    if type(time1) in {bool, int, float} and type(time2) in {bool, int, float}:
        if time1 < time2:
            return -1
        elif time1 > time2:
            return 1
        else:
            return 0
    # otherwise, we have at least one nested timestamp
    smaller = True
    greater = True
    # list/tuple vs list/tuple
    if type(time1) in {list, tuple} and type(time2) in {list, tuple}:
        keys = range(max(len(time1), len(time2)))
        for k in keys:
            t1 = time1[k] if k < len(time1) else float('-inf')
            t2 = time2[k] if k < len(time2) else float('-inf')
            po = partial_order(t1, t2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # dict vs dict
    elif type(time1) == dict and type(time2) == dict:
        keys = set().union(time1.keys(), time2.keys())
        for k in keys:
            t1 = time1[k] if k in time1 else float('-inf')
            t2 = time2[k] if k in time2 else float('-inf')
            po = partial_order(t1, t2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # set vs set
    elif type(time1) == set and type(time2) == set:
        keys = set().union(time1, time2)
        for k in keys:
            t1 = 1 if k in time1 else 0
            t2 = 1 if k in time2 else 0
            po = partial_order(t1, t2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # atomic vs list/tuple
    elif type(time1) in {bool, int, float} and type(time2) in {list, tuple}:
        for t2 in time2:
            po = partial_order(time1, t2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # atomic vs dict
    elif type(time1) in {bool, int, float} and type(time2) == dict:
        for t2 in time2.values():
            po = partial_order(time1, t2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # atomic vs list/tuple
    elif type(time2) in {bool, int, float} and type(time1) in {list, tuple}:
        for t1 in time1:
            po = partial_order(t1, time2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # atomic vs dict
    elif type(time2) in {bool, int, float} and type(time1) == dict:
        for t1 in time1.values():
            po = partial_order(t1, time2)
            if po == -1:
                greater = False
            elif po == +1:
                smaller = False
    # incomparable types
    else:
        greater = False
        smaller = False

    if smaller and greater:
        return 0
    elif smaller:
        return -1
    elif greater:
        return +1
    else:
        return None
    