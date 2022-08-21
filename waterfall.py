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

from typing import List
import matplotlib.pyplot as plt
from matplotlib.ticker import StrMethodFormatter
import numpy as np


class Actor:
    def __init__(self, break_even: float, name: str):
        assert break_even >= 0.0
        self.break_even = break_even
        self.name = name

    def __str__(self) -> str:
        return self.name


class Slot:
    def __init__(self,
                 negotiated_percentage: float,
                 cut_off: float,
                 actor: Actor) -> None:
        assert negotiated_percentage >= 0.0 and negotiated_percentage <= 1.0
        assert cut_off >= 0
        self.actor = actor
        self.negotiated_percentage = negotiated_percentage
        self.cut_off = cut_off
        self.income = 0.0

    def take_from_income_as_negotiated(self, income: float) -> None:
        self.income = income

    def get_income(self) -> float:
        return self.income

    def get_negotiated_percentage(self) -> float:
        return self.negotiated_percentage

    def reset_income(self):
        self.income = 0.0

    def get_cut_off(self) -> float:
        return self.cut_off

    def __str__(self) -> str:
        return "{} ({:.2f}€ avec un couloir de {:.2f})".format(self.actor, self.income, self.negotiated_percentage)


class Slice:
    def __init__(self, slots: List[Slot]):
        self.slots = slots
        self.income = 0.0
        self.max_cut_off_percented = max([slot.get_cut_off()/slot.get_negotiated_percentage()
                                          for slot in self.slots])

    def take_from_income_as_negotiated(self, income: float) -> float:
        total_income = income
        if self.max_cut_off_percented > 0:
            if income > self.max_cut_off_percented:
                # Then everybody can be served fully
                for slot in self.slots:
                    slot_income = self.max_cut_off_percented * slot.get_negotiated_percentage()
                    slot.take_from_income_as_negotiated(slot_income)
                return total_income - self.max_cut_off_percented
            else:
                # The cut for each slot is proportional to their share in the
                # total take of the slice
                for slot in self.slots:
                    slot_income = total_income * slot.get_negotiated_percentage()
                    slot.take_from_income_as_negotiated(slot_income)
                return 0
        else:
            for slot in self.slots:
                slot_income = total_income * slot.get_negotiated_percentage()
                slot.take_from_income_as_negotiated(slot_income)
            return 0

    def reset_income(self):
        for slot in self.slots:
            slot.reset_income()

    def get_max_cut_off_percented(self) -> float:
        return self.max_cut_off_percented

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slot in self.slots:
            if slot.actor == actor:
                actor_income += slot.income
        return actor_income

    def __str__(self):
        return " | ".join(["{}".format(slot) for slot in self.slots])


class Waterfall:
    def __init__(self, slices: List[Slice]):
        self.slices = slices

    def take_from_income_as_negotiated(self, income: float) -> float:
        for slice in self.slices:
            income = slice.take_from_income_as_negotiated(income)
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
        return "\n".join(["{}".format(slice) for slice in self.slices])


distributor = Actor(break_even=100_000, name="Distributeur")
producer = Actor(break_even=5_000_000, name="Producteur")
canal_plus = Actor(break_even=2_000_000, name="Canal+")
sofica = Actor(break_even=500_000, name="SOFICA")

first_slice = Slice(slots=[
    Slot(actor=producer, cut_off=0.0, negotiated_percentage=0.15),
    Slot(actor=distributor, cut_off=5000.0, negotiated_percentage=0.85)
])
second_slice = Slice(slots=[
    Slot(actor=distributor, negotiated_percentage=0.15, cut_off=0.0),
    Slot(actor=distributor, negotiated_percentage=0.75, cut_off=0.0),
    Slot(actor=producer, negotiated_percentage=0.10, cut_off=0.0)])

waterfall = Waterfall(
    slices=[first_slice, second_slice])

waterfall.take_from_income_as_negotiated(100000)
print(waterfall)


def get_actor_income(waterfall: Waterfall, total_income: float, actor: Actor) -> float:
    waterfall.reset_income()
    waterfall.take_from_income_as_negotiated(total_income)
    actor_income = waterfall.get_actor_income(actor)
    waterfall.reset_income
    return actor_income


incomes = np.arange(0.0, 10_000_000.0, 10000.0)
producer_incomes = [get_actor_income(
    waterfall, x, distributor) for x in incomes]
plt.plot(incomes, producer_incomes)
plt.xlabel('Recettes totales')
plt.ylabel('Revenus du producteur')
plt.title('Rémunération du producteur en fonction des recettes')
plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
plt.gca().xaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}€'))
plt.show()
get_actor_income(waterfall, 100000, distributor)
