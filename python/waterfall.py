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

from pickle import NONE
from telnetlib import DO
from tkinter import W
from typing import List, Optional, Tuple
import matplotlib.pyplot as plt  # type: ignore
from matplotlib.ticker import StrMethodFormatter  # type: ignore
import numpy as np
from math import floor
from abc import ABC, abstractmethod
import math


#### MOVIE INDUSTRIE SPECIFICATION
tsa = 0.107
admission_price = 6.15
distributor_theater_ratio = 0.5
bareme_soutien_prod=1.0
bareme_soutien_distrib=1.0

def tickets_to_income(tickets:float) -> float:
    return tickets*admission_price*(1-tsa)*distributor_theater_ratio
    
def income_to_tickets(income:float) -> float:
    return floor(income/(admission_price*(1-tsa)*distributor_theater_ratio))

def tickets_to_soutien_prod(tickets:float) -> float:
    coeff_global=float(tsa*bareme_soutien_prod*admission_price)
    seuil_1=1.1187*coeff_global*1_499_999
    seuil_2=(1.1187*1_499_999+0.8502*(4_999_999-1_499_999))*coeff_global

    if tickets < 1_500_000:
        return 1.1187*coeff_global*tickets
    else:
        if tickets>=1_500_000 and tickets<5_000_000:
            return seuil_1+0.8502*coeff_global*(tickets-1_499_999)
        else:
            if tickets>=5_000_000:
                return seuil_2+0.0895*coeff_global*(tickets-4_999_999)

def tickets_to_soutien_distrib(tickets:float)->float:
    coeff_global=tsa*bareme_soutien_distrib*admission_price
    seuil_1=2.0836*coeff_global*49_999
    seuil_2=seuil_1+coeff_global*1.3259*(99_999-49_999)
    seuil_3=seuil_2+coeff_global*1.1365*(199_999-99_999)
    seuil_4=seuil_3+coeff_global*0.4735*(499_999-199_999)
    seuil_5=seuil_4+coeff_global*0.2841*(699_999-499_999)
    seuil_6=seuil_5+coeff_global*0.0947*(999_999-699_999)

    if tickets < 50_000:
        return 2.0836*coeff_global*tickets
    else:
        if tickets>=50_000 and tickets<100_000:
            return seuil_1+1.3259*coeff_global*(tickets-49_999)
        else:
            if tickets>=100_000 and tickets<200_000:
                return seuil_2+1.1365*coeff_global*(tickets-99_999)
            else:
                if tickets>=200_000 and tickets<500_000:
                    return seuil_3+0.4735*coeff_global*(tickets-199_999)
                else:
                    if tickets>=500_000 and tickets<700_000:
                        return seuil_4+0.2841*coeff_global*(tickets-499_999)
                    else:
                        if tickets>=700_000 and tickets<1_000_000:
                            return seuil_5+0.0947*coeff_global*(tickets-699_999)
                        else:
                            if tickets>=1_000_000:
                                return seuil_6

def soutien_prod_to_tickets(soutien_prod:float)->float:
    coeff_global=tsa*bareme_soutien_distrib*admission_price
    seuil_1=1.1187*coeff_global*1_499_999
    seuil_2=seuil_1+0.8502*coeff_global*(4_999_999-1_499_999)

    if soutien_prod>seuil_2:
        return floor((soutien_prod-seuil_2+coeff_global*0.0895*4_999_999)/(0.0895*coeff_global))
    else:
        if soutien_prod>seuil_1:
            return floor((soutien_prod-seuil_1+coeff_global*0.8502*1_499_999)/(coeff_global*0.8502))
        else:
            return floor(soutien_prod/(1.1187*coeff_global))

def soutien_distrib_to_tickets(soutien_distrib:float)->float:
    coeff_global=tsa*bareme_soutien_distrib*admission_price 
    seuil_1=2.0836*coeff_global*49_999
    seuil_2=seuil_1+coeff_global*1.3259*(99_999-49_999)
    seuil_3=seuil_2+coeff_global*1.1365*(199_999-99_999)
    seuil_4=seuil_3+coeff_global*0.4735*(499_999-199_999)
    seuil_5=seuil_4+coeff_global*0.2841*(699_999-499_999)
    seuil_6=seuil_5+coeff_global*0.0947*(999_999-699_999)

    if soutien_distrib>seuil_6:
        return 1_000_000
    else:
        if soutien_distrib>seuil_5:
            return floor((soutien_distrib-seuil_5+coeff_global*0.0947*699_999)/(coeff_global*0.0947))
        else:
            if soutien_distrib>seuil_4:
                return floor((soutien_distrib-seuil_4+coeff_global*0.2841*499_999)/(coeff_global*0.2841))
            else:
                if soutien_distrib>seuil_3:
                    return floor((soutien_distrib-seuil_3+coeff_global*0.4735*199_999)/(coeff_global*0.4735))
                else:
                    if soutien_distrib>seuil_2: 
                        return floor((soutien_distrib-seuil_2+coeff_global*1.1365*99_999)/(coeff_global*1.1365))
                    else:
                        if soutien_distrib>seuil_1: 
                            return floor((soutien_distrib-seuil_1+coeff_global*1.3259*49_999)/(coeff_global*1.3259))
                        else:
                            return floor(soutien_distrib/(coeff_global*2.0836))
             
        
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

class SliceWithTicketsCutoff(FixedTotalIncomeSlice):
    def __init__(self, slots: List[Slot], slices:List[Slice], tickets_cut_off:float):
        # Properties for SliceWithTicketsCutoff
        assert len(slots) > 0
        self.tickets_cut_off = tickets_cut_off
        self.global_income_cut_off = tickets_to_income(self.tickets_cut_off)
        self.slices=slices

        # Properties for FixedTotalIncomeSlice
        self.slots = slots
        self.income = 0.0
        assert math.isclose(sum([slot.get_negotiated_percentage()
                                 for slot in self.slots]), 1.0
                            )

    def max_total_income(self) -> float:
        # The max total income depends on the tickets_cut_off

        income_counted_toward_cutoff = 0
        for slice in self.slices:
            income_counted_toward_cutoff += slice.get_slice_income()
        max_income = (self.global_income_cut_off - income_counted_toward_cutoff)
        return max_income

    def __str__(self):
        return "["+" | ".join(["{}".format(slot) for slot in self.slots]) + "] max{: .2f} entrées ".format(self.tickets_cut_off) + "/{: .2f} €".format(self.global_income_cut_off)
                            
class SliceWithFillingForeignSlicesConditions(FixedTotalIncomeSlice):
    def __init__(self, slots: List[Slot], slices:List[Slice]):
        # Properties for WithFillingForeignSlicesConditions
        assert len(slots) > 0
        self.slices=slices

        # Properties for FixedTotalIncomeSlice
        self.slots = slots
        self.income = 0.0
        assert math.isclose(sum([slot.get_negotiated_percentage()
                                 for slot in self.slots]), 1.0
                            )

    def max_total_income(self) -> float:
        # The max total income depends on filling the other slices
        for slice in self.slices:
            if not(slice.get_distance_to_slice_max_total_income()==0 or slice.get_distance_to_slice_max_total_income()==None):
                return None

        return self.get_slice_income()

    def __str__(self):
        return "["+" | ".join(["{}".format(slot) for slot in self.slots]) + "] si " + " | ".join(["{}".format(slice) for slice in self.slices]) +" remplie.s"

class Waterfall:
    def __init__(self, name: str, slices: List[Slice]):
        self.slices = slices
        self.name = name

    def distribute_income(self, income: float) -> float:
        for slice in self.slices:
            income = slice.distribute_income(income)
        return income

    def get_waterfall_income(self):
        waterfall_income = 0.0
        for slice in self.slices:
            waterfall_income += slice.get_slice_income()
        return waterfall_income

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
                if waterfall == soutien_prod:
                   old_soutien_prod=waterfall.get_waterfall_income()
                   old_tickets=soutien_prod_to_tickets(old_soutien_prod)
                   new_tickets=income_to_tickets(income)
                   total_tickets=old_tickets+new_tickets    
                   total_soutien_prod=tickets_to_soutien_prod(total_tickets)
                   new_soutien_prod = total_soutien_prod-old_soutien_prod
                   new_soutien_prod = waterfall.distribute_income(new_soutien_prod) 
                   return new_soutien_prod
                else:
                    if waterfall == soutien_distrib:
                        old_soutien_distrib=waterfall.get_waterfall_income()
                        old_tickets=soutien_distrib_to_tickets(old_soutien_distrib)
                        new_tickets=income_to_tickets(income)
                        total_tickets=old_tickets+new_tickets    
                        total_soutien_distrib=tickets_to_soutien_distrib(total_tickets)
                        new_soutien_distrib = total_soutien_distrib-old_soutien_distrib
                        new_soutien_distrib = waterfall.distribute_income(new_soutien_distrib) 
                        return new_soutien_distrib
                        
                    else:
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

platform_first_slice = FixedTotalIncomeSlice(
    slots=[netflix_control_slot_with_cutoff],
    max_total_income=100
)

soutien_prod_slice1 = FixedTotalIncomeSlice(
    slots=[Slot(actor=producer, negotiated_percentage=0.5, name="prod 1"),
           Slot(actor=producer, negotiated_percentage=0.5, name="prod 2")],
    max_total_income=150000
)

soutien_prod_slice2 = FixedTotalIncomeSlice(
    slots=[Slot(actor=producer, negotiated_percentage=0.4, name="prod 1"),
           Slot(actor=producer, negotiated_percentage=0.4, name="prod 2"),
           Slot(actor=distributor, negotiated_percentage=0.2, name="distrib")],
    max_total_income=None
)

cinema_first_slice = SliceWithOneCutoffSlotTakingIncomeFromForeignSlots(
    slot_with_cutoff=cinema_control_slot_with_cutoff,
    cut_off=100.0,
    foreign_slots=[tv_control_slot_with_cutoff,
                   netflix_control_slot_with_cutoff],
    other_slice_slots=[
        Slot(actor=sofica, negotiated_percentage=0.10, name="ciné"),
        Slot(actor=canal_plus, negotiated_percentage=0.40, name="ciné")
    ])
cinema_second_slice = SliceWithTicketsCutoff(
    slots=[Slot(actor=distributor, negotiated_percentage=0.2, name="ciné#1"),
                        Slot(actor=distributor, negotiated_percentage=0.7, name="ciné#2"),
                        Slot(actor=sofica, negotiated_percentage=0.1, name="ciné")],
    slices=[cinema_first_slice],
    tickets_cut_off = 1000
)
cinema_third_slice = SliceWithFillingForeignSlicesConditions(
    slots=[Slot(actor=producer, negotiated_percentage=0.1, name="ciné"),
           Slot(actor=distributor, negotiated_percentage=0.9, name="ciné")],
    slices=[platform_first_slice, soutien_prod_slice1]
)

cinema_fourth_slice = FixedTotalIncomeSlice(
    slots=[Slot(actor=producer, negotiated_percentage=0.3, name="ciné"),
           Slot(actor=distributor, negotiated_percentage=0.3, name="ciné"),
           Slot(actor=sofica, negotiated_percentage=0.4, name="ciné")],
    max_total_income=None
)

soutien_distrib_slice1 = FixedTotalIncomeSlice(
    slots=[Slot(actor=distributor, negotiated_percentage=1.0, name="distrib")],
    max_total_income=None
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

cinema = Waterfall(name="Cinéma",
                   slices=[cinema_first_slice, cinema_second_slice, cinema_third_slice, cinema_fourth_slice])

soutien_prod = Waterfall(name="SP",
                   slices=[soutien_prod_slice1, soutien_prod_slice2])

soutien_distrib = Waterfall(name="SD",
                            slices=[soutien_distrib_slice1])

tv = Waterfall(name="TV",
               slices=[tv_first_slice, tv_second_slice])

platform = Waterfall(name="Platforme",
                     slices=[platform_first_slice])


movie = MovieRevenue(
    waterfalls=[cinema, tv, platform, soutien_prod, soutien_distrib]
)

print("== Initial ==\n", movie)
movie.distribute_income(cinema, 1_000_000)
print("== 1 ==\n", movie)
movie.distribute_income(platform, 1000)
movie.distribute_income(soutien_prod, 800_000)
print("== 2 ==\n", movie)
movie.distribute_income(cinema, 100)
print("== 3 ==\n", movie)
#movie.distribute_income(soutien_distrib, 50)


#print(tickets_to_income(1500000))
#print(income_to_tickets(11_100_000))

def dichotomic_search(waterfall: Waterfall, slot:Slot, target:float , delta:float, max_x:float)->float:
    min_x = 0
    current_x = max_x / 2
    waterfall.reset_income()
    waterfall.distribute_income(current_x)
    while slot.get_income() < target - delta or slot.get_income() > target + delta :
      if  slot.get_income() > target + delta:
         max_x = current_x
         current_x = (max_x + min_x) / 2 
         waterfall.reset_income()
         waterfall.distribute_income(current_x)
      else:
         min_x = current_x
         current_x =  (max_x + min_x) / 2
         waterfall.reset_income()
         waterfall.distribute_income(current_x) 
    
    target_approx = slot.get_income()
    waterfall.reset_income()
    current_x = 0

    while slot.get_income() < target_approx:
        current_x += 1.0
        waterfall.reset_income()
        waterfall.distribute_income(current_x)
    return current_x


#a=dichotomic_search(cinema, cinema_control_slot_with_cutoff, 100.0, 0.5, 5000.0)
#print(a)



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
