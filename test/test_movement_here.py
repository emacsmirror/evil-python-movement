# Originally from https://stackoverflow.com/a/28284564
# This file is licensed under the terms of CC BY-SA 3.0
# See https://stackoverflow.com/help/licensing 
# See https://creativecommons.org/licenses/by-sa/3.0/
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
