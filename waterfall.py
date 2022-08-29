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
import math


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
                 actor: Actor,
                 name: str) -> None:
        assert negotiated_percentage >= 0.0 and negotiated_percentage <= 1.0
        self.actor = actor
        self.negotiated_percentage = negotiated_percentage
        self.income = 0.0
        self.name = name

    # The slot will take all the income that's being given to him at the call.
    def take_income(self, income: float) -> None:
        self.income += income

    def get_income(self) -> float:
        return self.income

    def get_actor(self) -> Actor:
        return self.actor

    def get_negotiated_percentage(self) -> float:
        return self.negotiated_percentage

    def reset_income(self):
        self.income = 0.0

    def __str__(self) -> str:
        return "{} ({}): {:.2f}€/{:.0%}".format(self.actor, self.name, self.income, self.negotiated_percentage)


class Slice(ABC):
    @abstractmethod
    def max_total_income(self) -> Optional[float]:
        pass

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
        self.max_total_income_value = max_total_income
        self.income = 0.0

    def max_total_income(self):
        return self.max_total_income_value

    def distribute_income(self, income: float) -> float:
        distance_to_slice_max_total_income = self.get_distance_to_slice_max_total_income()
        original_income = income
        if (not (distance_to_slice_max_total_income is None)) and \
                income > distance_to_slice_max_total_income:
            # Then everybody can be served fully
            for slot in self.slots:
                slot_income = distance_to_slice_max_total_income * slot.get_negotiated_percentage()
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

    def get_distance_to_slice_max_total_income(self) -> Optional[float]:
        if self.max_total_income() is None:
            return None
        else:
            return self.max_total_income() - self.get_slice_income()

    def reset_income(self):
        for slot in self.slots:
            slot.reset_income()

    def get_slice_income(self) -> float:
        slice_income = 0.0
        for slot in self.slots:
            slice_income += slot.get_income()
        return slice_income

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slot in self.slots:
            if slot.get_actor() == actor:
                actor_income += slot.get_income()
        return actor_income

    def __str__(self):
        return "["+" | ".join(["{}".format(slot) for slot in self.slots]) + ("] max " + "∞ €" if self.max_total_income() is None else "] max{: .2f}€".format(self.max_total_income()))


class SliceWithSlotCutoffs(FixedTotalIncomeSlice):
    def __init__(self, slots_with_cutoffs: List[Tuple[Slot, float]], other_slice_slots: List[Slot]):
        assert len(slots_with_cutoffs) > 0
        # Properties for SliceWithActorCutoffs
        self.slots_with_cutoffs = slots_with_cutoffs
        self.other_slice_slots = other_slice_slots

        # Properties for FixedTotalIncomeSlice
        self.income = 0
        self.slots = [slot for (slot, _) in slots_with_cutoffs]
        self.slots.extend(other_slice_slots)
        self.max_total_income_value = \
            max([cutoff / cutoff_slot.get_negotiated_percentage()
                 for (cutoff_slot, cutoff) in slots_with_cutoffs])
        assert math.isclose(sum([slot.get_negotiated_percentage()
                                 for slot in self.slots]), 1.0
                            )

    def __str__(self):
        return "["+" | ".join(["{} cutoff {:.2f}€".format(slot, cutoff) for (slot, cutoff) in self.slots_with_cutoffs]) + " | " + \
            " | ".join(["{}".format(slot)
                       for slot in self.other_slice_slots]) + "]"


class SliceWithOneCutoffSlotTakingIncomeFromForeignSlots(FixedTotalIncomeSlice):
    def __init__(self, slot_with_cutoff: Slot, cut_off: float, foreign_slots: List[Slot],  other_slice_slots: List[Slot]):
        # Properties for SliceWithOneCutoffSlotTakingIncomeFromForeignSlots
        assert len(foreign_slots) > 0
        self.slot_with_cutoff = slot_with_cutoff
        self.foreign_slots = foreign_slots
        self.other_slice_slots = other_slice_slots
        self.cut_off = cut_off

        # Properties for FixedTotalIncomeSlice
        self.slots = other_slice_slots
        self.slots.append(slot_with_cutoff)
        self.income = 0.0
        assert math.isclose(sum([slot.get_negotiated_percentage()
                                 for slot in self.slots]), 1.0
                            )

    def max_total_income(self) -> float:
        # The max total income depends on the cutoff but also on the income
        # already present in foreign slots!
        income_counted_toward_cutoff = 0
        for slot in self.foreign_slots:
            income_counted_toward_cutoff += slot.get_income()
        max_income = (self.cut_off - income_counted_toward_cutoff) / \
            self.slot_with_cutoff.get_negotiated_percentage()
        return max_income

    def __str__(self):
        cutoff_slot_str = "{}".format(self.slot_with_cutoff) + " paired with [" + \
            " | ".join(["{}".format(slot) for slot in self.foreign_slots]) + \
            " with cutoff " + "{:.2f}€".format(self.cut_off) + "]"
        return "["+" | ".join([cutoff_slot_str if slot == self.slot_with_cutoff else "{}".format(slot) for slot in self.slots]) + "]"


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
netflix = Actor(break_even=200_000, name="NETFLIX")

tv_control_slot_with_cutoff = Slot(
    actor=distributor, negotiated_percentage=0.20, name="contrôle")
cinema_control_slot_with_cutoff = Slot(
    actor=producer, negotiated_percentage=0.50, name="contrôle")
netflix_control_slot_with_cutoff = Slot(
    actor=netflix, negotiated_percentage=1.0, name="contrôle")

cinema_first_slice = SliceWithOneCutoffSlotTakingIncomeFromForeignSlots(
    slot_with_cutoff=cinema_control_slot_with_cutoff,
    cut_off=100.0,
    foreign_slots=[tv_control_slot_with_cutoff,
                   netflix_control_slot_with_cutoff],
    other_slice_slots=[
        Slot(actor=sofica, negotiated_percentage=0.10, name="ciné"),
        Slot(actor=canal_plus, negotiated_percentage=0.40, name="ciné")
    ])
cinema_second_slice = SliceWithSlotCutoffs(
    slots_with_cutoffs=[(Slot(actor=distributor, negotiated_percentage=0.2, name="ciné#1"), 100),
                        (Slot(actor=distributor, negotiated_percentage=0.7, name="ciné#2"), 20)],
    other_slice_slots=[
        Slot(actor=sofica, negotiated_percentage=0.1, name="ciné")]
)

tv_first_slice = SliceWithOneCutoffSlotTakingIncomeFromForeignSlots(
    slot_with_cutoff=tv_control_slot_with_cutoff,
    cut_off=100.0,
    foreign_slots=[cinema_control_slot_with_cutoff,
                   netflix_control_slot_with_cutoff],
    other_slice_slots=[
        Slot(actor=sofica, negotiated_percentage=0.50, name="télé"),
        Slot(actor=canal_plus, negotiated_percentage=0.30, name="télé")
    ])

tv_second_slice = FixedTotalIncomeSlice(slots=[
    Slot(actor=distributor, negotiated_percentage=0.35, name="télé#1"),
    Slot(actor=distributor, negotiated_percentage=0.25, name="télé#2"),
    Slot(actor=producer, negotiated_percentage=0.40, name="télé")],
    max_total_income=None)

platform_first_slice = FixedTotalIncomeSlice(
    slots=[netflix_control_slot_with_cutoff],
    max_total_income=100
)

cinema = Waterfall(name="Cinéma",
                   slices=[cinema_first_slice, cinema_second_slice])

tv = Waterfall(name="TV",
               slices=[tv_first_slice, tv_second_slice])

platform = Waterfall(name="Platforme",
                     slices=[platform_first_slice])


movie = MovieRevenue(
    waterfalls=[cinema, tv, platform]
)

print("== Initial ==\n", movie)
#movie.distribute_income(platform, 50)
#print("== 50 € pour plateforme ==\n" + "{}".format(movie) + "\n\n")
movie.distribute_income(platform, 500)
print("== 50 € pour cinéma ==\n" + "{}".format(movie) + "\n\n")
#movie.distribute_income(cinema, 1000)
#print("== 10 € pour cinéma ==\n" + "{}".format(movie) + "\n\n")
#movie.distribute_income(cinema, 300)
#print("== 300 € pour cinéma ==\n" + "{}".format(movie) + "\n\n")
#movie.distribute_income(tv, 100)
#print("== 100 € pour télé ==\n" + "{}".format(movie) + "\n\n")
#movie.distribute_income(tv, 1000)
#print("== 1000 € pour télé ==\n" + "{}".format(movie))

def dichotomic_search(waterfall: Waterfall, slot:Slot, target:float , delta:float, max_x:float)->float:
    min_x = 0
    current_x = max_x / 2
    waterfall.reset_income
    waterfall.distribute_income(current_x)
    while slot.get_income() < target - delta or slot.get_income() > target + delta :
      if  slot.get_income() < target - delta:
         max_x = current_x
         current_x = (max_x + min_x) / 2 
         waterfall.reset_income
         waterfall.distribute_income(current_x)
      else:
         min_x = current_x
         current_x =  (max_x + min_x) / 2
         waterfall.reset_income
         waterfall.distribute_income(current_x)
    return current_x

a=dichotomic_search(platform, netflix_control_slot_with_cutoff, 30.0, 5.0, 80.0)
print(a)

def get_actor_income(waterfall: Waterfall, total_income: float, actor: Actor) -> float:
    waterfall.reset_income()
    waterfall.distribute_income(total_income)
    actor_income = waterfall.get_actor_income(actor)
    waterfall.reset_income
    return actor_income



# incomes = np.arange(0.0, 10_000_000.0, 10000.0)
# producer_incomes = []
# for income in incomes:
#     movie.reset_income(cinema)
#     movie.distribute_income(cinema, income)
#     producer_incomes.append(movie.get_actor_income(producer))
#     movie.reset_income(cinema)
# plt.plot(incomes, producer_incomes)
# plt.xlabel('Recettes totales')
# plt.ylabel('Revenus du producteur')
# plt.title('Rémunération du producteur en fonction des recettes')
# plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
# plt.gca().xaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
# plt.show()
