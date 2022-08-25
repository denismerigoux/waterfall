#  Copyright 2022 Denis Merigoux and Antoine Devulder
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

from tkinter import W
from typing import List, Optional, Tuple
import matplotlib.pyplot as plt  # type: ignore
from matplotlib.ticker import StrMethodFormatter  # type: ignore
import numpy as np
from abc import ABC, abstractmethod


class Actor:
    def __init__(self, break_even: float, name: str):
        assert break_even >= 0.0
        self.break_even = break_even
        self.name = name

    def __str__(self) -> str:
        return self.name


class Slot:
    # The negociated percentage of a Slot is the proportion of the Slice
    # revenue that the Slot is entitled to.
    def __init__(self,
                 negotiated_percentage: float,
                 actor: Actor) -> None:
        assert negotiated_percentage >= 0.0 and negotiated_percentage <= 1.0
        self.actor = actor
        self.negotiated_percentage = negotiated_percentage
        self.income = 0.0

    # The slot will take all the income that's being given to him at the call.
    def take_income(self, income: float) -> None:
        self.income = income

    def get_income(self) -> float:
        return self.income

    def get_actor(self) -> Actor:
        return self.actor

    def get_negotiated_percentage(self) -> float:
        return self.negotiated_percentage

    def reset_income(self):
        self.income = 0.0

    def __str__(self) -> str:
        return "{}: {:.2f}€/{:.0%}".format(self.actor, self.income, self.negotiated_percentage)


class Slice(ABC):
    @abstractmethod
    def distribute_income(self, income: float) -> float:
        pass

    @abstractmethod
    def reset_income(self):
        pass

    @abstractmethod
    def get_actor_income(self, actor: Actor) -> float:
        pass

    @abstractmethod
    def __str__(self):
        pass


class FixedTotalIncomeSlice(Slice):
    # If max_total_income is None then no limit to the slice's income
    def __init__(self, slots: List[Slot], max_total_income: Optional[float]):
        assert (True if max_total_income is None else max_total_income >= 0.0)
        assert sum([slot.get_negotiated_percentage() for slot in slots]) == 1.0
        self.slots = slots
        self.max_total_income = max_total_income

    def distribute_income(self, income: float) -> float:
        original_income = income
        if not (self.max_total_income is None) and income > self.max_total_income:
            # Then everybody can be served fully
            for slot in self.slots:
                slot_income = self.max_total_income * slot.get_negotiated_percentage()
                slot.take_income(slot_income)
                income -= slot_income
            return income
        else:
            # The cut for each slot is proportional to their share in the
            # total take of the slice
            for slot in self.slots:
                slot_income = original_income * slot.get_negotiated_percentage()
                slot.take_income(slot_income)
                income -= slot_income
            return 0.0

    def reset_income(self):
        for slot in self.slots:
            slot.reset_income()

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slot in self.slots:
            if slot.get_actor() == actor:
                actor_income += slot.get_income()
        return actor_income

    def __str__(self):
        return "["+" | ".join(["{}".format(slot) for slot in self.slots]) + "] max " + "∞ €" if self.max_total_income is None else "{: .2f}€".format(self.max_total_income)


class SliceWithActorCutoffs(FixedTotalIncomeSlice):
    def __init__(self, slots_with_cutoffs: List[Tuple[Slot, float]], other_slots: List[Slot]):
        assert len(slots_with_cutoffs) > 0
        self.slots_with_cutoffs = slots_with_cutoffs
        self.other_slots = other_slots

        self.slots = [slot for (slot, _) in slots_with_cutoffs]
        self.slots.extend(other_slots)
        self.max_total_income = max([cutoff / cutoff_slot.get_negotiated_percentage()
                                    for (cutoff_slot, cutoff) in slots_with_cutoffs])
        self.income = 0.0
        assert sum([slot.get_negotiated_percentage() for slot in self.slots]) == 1.0

    def __str__(self):
        return "["+" | ".join(["{} max {:.2f}€".format(slot, cutoff) for (slot, cutoff) in self.slots_with_cutoffs]) + " | " + \
            " | ".join(["{}".format(slot)
                       for slot in self.other_slots]) + "]"


class SlicePairedWithCutoffs(FixedTotalIncomeSlice):
    def __init__(self, slot_with_cutoff: Slot, slots_outside: List[Slot], cut_off:float, other_slots: List[Slot]):
        assert len(slots_outside) > 0

        self.slot_with_cutoff = slot_with_cutoff
        self.slots_outside = slots_outside
        self.other_slots = other_slots
        self.cut_off = cut_off
        self.slots = other_slots
        self.slots.append(slot_with_cutoff)

        global_income = self.slot_with_cutoff.get_income()
        for slot in self.slots_outside:
            global_income += slot.get_income()

        self.max_total_income = (self.cut_off - global_income)/self.slot_with_cutoff.get_negotiated_percentage()
        self.income = 0.0
        assert sum([slot.get_negotiated_percentage() for slot in self.slots]) == 1.0

    def __str__(self):
        return "["+" | ".join(["{}".format(slot) for slot in self.other_slots]) + "] ({}".format(self.slot_with_cutoff) + " paired with " + " | ".join(["{}".format(slot) for slot in self.slots_outside])+" with cutoff " + "{: .2f}€".format(self.cut_off)+")"


class Waterfall:
    def __init__(self, name: str, slices: List[Slice]):
        self.slices = slices
        self.name = name

    def distribute_income(self, income: float) -> float:
        for slice in self.slices:
            income = slice.distribute_income(income)
        return income

    def reset_income(self):
        for slice in self.slices:
            slice.reset_income()

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slice in self.slices:
            actor_income += slice.get_actor_income(actor)
        return actor_income

    def __str__(self):
        return "{}:\n".format(self.name) + "\n".join(["{}".format(slice) for slice in self.slices])


class MovieRevenue:
    def __init__(self, waterfalls: List[Waterfall]):
        self.waterfalls = waterfalls

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for waterfall in self.waterfalls:
            actor_income += waterfall.get_actor_income(actor)
        return actor_income

    def distribute_income(self, target_waterfall: Waterfall, income: float) -> float:
        for waterfall in self.waterfalls:
            if waterfall == target_waterfall:
                new_income = waterfall.distribute_income(income)
                return new_income
        return income

    def reset_income(self, target_waterfall: Waterfall):
        for waterfall in self.waterfalls:
            if waterfall == target_waterfall:
                new_income = waterfall.reset_income()

    def __str__(self):
        return "\n\n".join(["{}".format(waterfall) for waterfall in self.waterfalls])


distributor = Actor(break_even=100_000, name="Distributeur")
producer = Actor(break_even=5_000_000, name="Producteur")
canal_plus = Actor(break_even=2_000_000, name="Canal+")
sofica = Actor(break_even=500_000, name="SOFICA")

TV_control_slot_with_cutoff=Slot(actor=distributor, negotiated_percentage=0.20)
cinema_control_slot_with_cutoff=Slot(actor=producer, negotiated_percentage=0.50)

cinema_first_slice = SlicePairedWithCutoffs(
    cinema_control_slot_with_cutoff,
    slots_outside=[TV_control_slot_with_cutoff],
    cut_off= 100.0,
    other_slots=[
        Slot(actor=sofica, negotiated_percentage=0.10),
        Slot(actor=canal_plus, negotiated_percentage=0.40)
    ])
cinema_second_slice = FixedTotalIncomeSlice(slots=[
    Slot(actor=distributor, negotiated_percentage=0.15),
    Slot(actor=distributor, negotiated_percentage=0.75),
    Slot(actor=producer, negotiated_percentage=0.10)],
    max_total_income=None)

TV_first_slice = SlicePairedWithCutoffs(
    TV_control_slot_with_cutoff,
    slots_outside=[cinema_control_slot_with_cutoff],
    cut_off= 100.0,
    other_slots=[
        Slot(actor=sofica, negotiated_percentage=0.50),
        Slot(actor=canal_plus, negotiated_percentage=0.30)
    ])
TV_second_slice = FixedTotalIncomeSlice(slots=[
    Slot(actor=distributor, negotiated_percentage=0.35),
    Slot(actor=distributor, negotiated_percentage=0.25),
    Slot(actor=producer, negotiated_percentage=0.40)],
    max_total_income=None)

cinema = Waterfall(name="Cinéma",
                   slices=[cinema_first_slice, cinema_second_slice])

TV = Waterfall(name="TV",
                   slices=[TV_first_slice, TV_second_slice])

movie = MovieRevenue(
    waterfalls=[cinema, TV]
)

movie.distribute_income(cinema, 100000)
movie.distribute_income(TV, 1000)
movie.distribute_income(TV, 1000)

print(movie)


def get_actor_income(waterfall: Waterfall, total_income: float, actor: Actor) -> float:
    waterfall.reset_income()
    waterfall.distribute_income(total_income)
    actor_income = waterfall.get_actor_income(actor)
    waterfall.reset_income
    return actor_income


#incomes = np.arange(0.0, 10_000_000.0, 10000.0)
#producer_incomes = []
#for income in incomes:
#    movie.reset_income(cinema)
#    movie.distribute_income(cinema, income)
#    producer_incomes.append(movie.get_actor_income(producer))
#    movie.reset_income(cinema)
#plt.plot(incomes, producer_incomes)
#plt.xlabel('Recettes totales')
#plt.ylabel('Revenus du producteur')
#plt.title('Rémunération du producteur en fonction des recettes')
#plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
#plt.gca().xaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
#plt.show()
