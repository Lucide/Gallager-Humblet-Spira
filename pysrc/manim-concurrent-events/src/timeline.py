from event import Event
from utils_yamljson import load, save
from utils_timestamps import partial_order



class Timeline:
    raw_events = None
    events = None
    locations = None
    sorted_events_per_location = None

    def __init__(self, rules, config, events_filename, debug_filename):
        self.raw_events = load(events_filename)

        # complete attributes of raw_events using rules
        # and convert raw_events into Event objects
        self.events = []
        for i, raw_event in enumerate(self.raw_events):
            raw_event['idx'] = i
            rules.complete(raw_event)
            if 'skip' not in raw_event or not raw_event['skip']:
                self.events.append(Event(raw_event))

        # gather all locations appearing in events
        self.locations = set()
        for e in self.events:
            self.locations.add(e.location)

        # check that endpoints of links appear also as locations
        for e in self.events:        
            if e.is_link:
                assert e.location[0] in self.locations, \
                       f"Node {e.location[0]} was not explicitly declared, but appears in a link."
                assert e.location[1] in self.locations, \
                       f"Node {e.location[1]} was not explicitly declared, but appears in a link."

        # process physical and logical timestamps
        if config.timestamps == 'physical':
            self.normalize_physical_timestamps()
        elif config.timestamps == 'logical':
            self.logical_to_physical(config)

        # log sorted events for debugging
        if debug_filename != '':
            self.debug_timeline(events_filename, debug_filename)

        # check that all timestamps are now int/floats
        for e in self.events:
            assert type(e.from_time) in {int, float} and type(e.to_time) in {int, float}, \
                   f"Event #{e.idx} must have 'from_time' and 'to_time' properties as numbers."

        # add offsets to from_time and to_time and scale them as specified in config
        for e in self.events:
            e.from_time += e.from_time_offset
            e.to_time += e.to_time_offset
            e.from_time *= config.time_scale
            e.to_time *= config.time_scale
            if e.from_time > e.to_time:
                print(f"Warning: event #{e.idx} has negative interval.")
                e.to_time = e.from_time

        # regroup events by their location and sort each group using physical from_time
        self.sorted_events_per_location = {}
        for loc in self.locations:
            self.sorted_events_per_location[loc] = []
        for event in self.events:
            self.sorted_events_per_location[event.location].append(event)
        for loc in self.locations:
            self.sorted_events_per_location[loc].sort(key=lambda e: e.from_time)

        # correct overlapping intervals for events on the same location
        for loc in self.locations:
            for i in range(len(self.sorted_events_per_location[loc])):
                for j in range(i + 1, len(self.sorted_events_per_location[loc])):
                    e1 = self.sorted_events_per_location[loc][i]
                    e2 = self.sorted_events_per_location[loc][j]
                    assert e1.from_time <= e2.from_time, \
                           f"Something wrong with the ordering of events {e1.idx} and {e2.idx}"
                    if e1.persistent and e2.persistent and e1.location == e2.location and \
                       e1.from_time < e2.from_time < e1.to_time:
                        print(f"Warning: events #{e1.idx} and #{e2.idx} overlap at location {e1.location}")
                        e1.to_time = e2.from_time


    # normalize physical timestamps in the range [0, 1]
    def normalize_physical_timestamps(self):
        if any(float('-inf') < e.from_time < float('+inf') for e in self.events) and \
           any(float('-inf') < e.to_time < float('+inf') for e in self.events):
            min_timestamp = min([e.from_time for e in self.events if float('-inf') < e.from_time < float('+inf')])
            max_timestamp = max([e.to_time for e in self.events if float('-inf') < e.to_time < float('+inf')])
        else:
            min_timestamp = 0.0
            max_timestamp = 1.0

        for e in self.events:
            if float('-inf') < e.from_time < float('+inf'):
                e.from_time = (e.from_time - min_timestamp) / (max_timestamp - min_timestamp) + 0.01 if min_timestamp < max_timestamp else 0.01
            elif e.from_time == float('-inf'):
                e.from_time = 0.0
            elif e.from_time == float('+inf'):
                e.from_time = 1.0
            if float('-inf') < e.to_time < float('+inf'):
                e.to_time = (e.to_time - min_timestamp) / (max_timestamp - min_timestamp) + 0.01 if min_timestamp < max_timestamp else 0.01
            elif e.to_time == float('-inf'):
                e.to_time = 0.0
            elif e.to_time == float('+inf'):
                e.to_time = 1.0


    # convert logical timestamps of events into physical timestamps
    def logical_to_physical(self, config):
        # first we create start- and end-subevents from the events
        subevents = [(True, event) for event in self.events] + \
                    [(False, event) for event in self.events]

        # define a comparing function for subevents (will be used below)
        def cmp_subevents(flag1, event1, flag2, event2):
            time1 = event1.from_time if flag1 else event1.to_time
            time2 = event2.from_time if flag2 else event2.to_time
            po = partial_order(time1, time2, config.lex_order_on)
            if po == 0:
                if flag1 and not flag2:
                    po = -1
                elif flag2 and not flag1:
                    po = 1
            return po

        # then we groups subevents by eagerly forming maximal sets of incomparable elements.
        grouped_subevents = []
        while subevents:
            curr_group = []
            new_subevents = []
            for flag, event in subevents:
                if any((flag, event) != (flag2, event2) and cmp_subevents(flag, event, flag2, event2) == 1 \
                       for flag2, event2 in subevents):
                    new_subevents.append((flag, event))
                else:
                    curr_group.append((flag, event))

            assert len(curr_group) > 0, f"Unexpected empty group."

            grouped_subevents.append(curr_group)
            subevents = new_subevents

        # finally, in the original list of events we replace from_time/to_time 
        # with index of group to which corresponding subevent belongs to
        n = len(grouped_subevents)
        for i, group in enumerate(grouped_subevents):
            for flag, event in group:
                if flag:
                    event.from_time = i / n
                else:
                    event.to_time = (i + 1) / n


    def debug_timeline(self, events_filename, debug_filename):
        def get_from_time(event):
            return event.from_time
        # raw_events = load(events_filename)
        sorted_events = sorted(self.events, key=get_from_time)
        # sorted_raw_events = [self.raw_events[e.idx] for e in sorted_events]
        # save(debug_filename, sorted_raw_events)
        save(debug_filename, sorted_events)

