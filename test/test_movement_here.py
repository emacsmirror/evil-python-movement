class Mapping:                              # [[[[
    def __init__(self, iterable):
        pass

    def update(self, iterable):
        pass

    __update = update                       # []

class Reverse:                              # [[ or [m[m
    def __init__(self, data):               # [m
        self.data = data
        self.index = len(data)              # [M

    def __iter__(self):                     # <--- CURSOR
        return self                         # ]M

    def __next__(self):                     # ]m
        if self.index == 0:
            raise StopIteration
        self.index = self.index - 1
        return self.data[self.index]        # ][

class MappingSubclass(Mapping):             # ]] or ]m]m

    def update(self, keys, values):
        pass
