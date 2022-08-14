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
                 negotiated_retribution: float,
                 negotiated_margin: float,
                 actor: Actor) -> None:
        assert negotiated_margin <= 1.0 and negotiated_margin >= 0.0
        assert negotiated_retribution >= 0.0
        self.actor = actor
        self.negotiated_retribution = negotiated_retribution
        self.negotiated_margin = negotiated_margin
        self.income = 0.0
        self.total_take = self.negotiated_retribution * \
            (1 + self.negotiated_margin)

    def take_from_income_as_negotiated(self, income: float) -> float:
        if income > self.total_take:
            self.income = self.total_take
            return income - self.total_take
        else:
            self.income = income
            return 0

    def get_income(self) -> float:
        return self.income

    def reset_income(self):
        self.income = 0.0

    def get_total_take(self) -> float:
        return self.total_take

    def __str__(self) -> str:
        return "{} ({:.2f}€/{:.2f}€+{:.0%})".format(self.actor, self.income, self.negotiated_retribution, self.negotiated_margin)


class Slice:
    def __init__(self, slots: List[Slot]):
        self.slots = slots
        self.income = 0.0
        self.total_take = sum([slot.get_total_take()
                              for slot in self.slots])

    def take_from_income_as_negotiated(self, income: float) -> float:
        if income > self.total_take:
            # Then everybody can be served fully
            for slot in self.slots:
                income = slot.take_from_income_as_negotiated(income)
            return income
        else:
            # The cut for each slot is proportional to their share in the
            # total take of the slice
            total_income = income
            for slot in self.slots:
                slot_income = total_income * \
                    (slot.get_total_take() / self.total_take)
                income = slot.take_from_income_as_negotiated(slot_income)
            return income

    def get_total_take(self) -> float:
        return self.total_take

    def reset_income(self):
        for slot in self.slots:
            slot.reset_income()

    def __str__(self):
        return " | ".join(["{}".format(slot) for slot in self.slots])

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slot in self.slots:
            if slot.actor == actor:
                actor_income += slot.income
        return actor_income


class Waterfall:
    def __init__(self, slices: List[Slice]):
        self.slices = slices
        self.total_take = sum([slice.get_total_take()
                               for slice in self.slices])

    def get_total_take(self) -> float:
        return self.total_take

    def take_from_income_as_negotiated(self, income: float) -> float:
        for slice in self.slices:
            income = slice.take_from_income_as_negotiated(income)
        return income

    def reset_income(self):
        for slice in self.slices:
            slice.reset_income()

    def __str__(self):
        return "\n".join(["{}".format(slice) for slice in self.slices])

    def get_actor_income(self, actor: Actor) -> float:
        actor_income = 0.0
        for slice in self.slices:
            actor_income += slice.get_actor_income(actor)
        return actor_income


distributor = Actor(break_even=100_000, name="Distributeur")
producer = Actor(break_even=5_000_000, name="Producteur")
canal_plus = Actor(break_even=2_000_000, name="Canal+")
sofica = Actor(break_even=500_000, name="SOFICA")

first_slice = Slice(slots=[
    Slot(actor=distributor, negotiated_retribution=100000, negotiated_margin=0.15)])
second_slice = Slice(slots=[Slot(actor=producer, negotiated_retribution=1_000_000,
                     negotiated_margin=0.0)])
third_slice = Slice(slots=[Slot(actor=canal_plus, negotiated_retribution=2_000_000, negotiated_margin=0.2),
                           Slot(actor=sofica, negotiated_retribution=500_000, negotiated_margin=0.04)])
fourth_slice = Slice(slots=[Slot(
    actor=producer, negotiated_retribution=1_000_000, negotiated_margin=0.10)])

waterfall = Waterfall(
    slices=[first_slice, second_slice, third_slice, fourth_slice])

# waterfall.take_from_income_as_negotiated(5_000_000)
# print(waterfall)
# print("{:.2f}€".format(waterfall.get_actor_income(producer)))


def get_actor_income(waterfall: Waterfall, total_income: float, actor: Actor) -> float:
    waterfall.reset_income()
    waterfall.take_from_income_as_negotiated(total_income)
    actor_income = waterfall.get_actor_income(actor)
    waterfall.reset_income
    return actor_income


incomes = np.arange(0.0, 10_000_000.0, 10000.0)
producer_incomes = [get_actor_income(waterfall, x, producer) for x in incomes]
plt.plot(incomes, producer_incomes)
plt.xlabel('Recettes totales')
plt.ylabel('Revenus du producteur')
plt.title('Rémunération du producteur en fonction des recettes')
plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.2f}€'))
plt.gca().xaxis.set_major_formatter(StrMethodFormatter('{x:,.2f}€'))
plt.show()
