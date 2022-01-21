from collections import namedtuple


game_state_variables = ["player_hp", "boss_hp", "mana", "damage", 
        "shield_timer", "poison_timer", "recharge_timer", "turn"]
GameState = namedtuple("GameState", game_state_variables)

"""
Magic Missile costs 53 mana. It instantly does 4 damage.
Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
"""
INITIAL_GAME_STATE = GameState(player_hp=50, boss_hp=71, mana=500,
        damage=10, shield_timer=0, poison_timer=0, recharge_timer=0,
        turn="player")
# INITIAL_GAME_STATE = GameState(player_hp=50, boss_hp=71, mana=500,
#         damage=8, shield_timer=0, poison_timer=0, recharge_timer=0,
#         turn="player")

class Spell:
    def __init__(self):
        pass

    def cast(self, gs):
        pass

    def is_castable(self, gs):
        pass

    def effect(self, gs):
        return gs 


class MagicMissile(Spell):
    cost = 53
    damage = 4

    def cast(self, gs):
        new_mana = gs.mana - self.cost
        new_boss_hp = gs.boss_hp - self.damage
        return gs._replace(mana=new_mana, boss_hp=new_boss_hp)

    def is_castable(self, gs):
        return gs.mana >= self.cost


class Drain(Spell):
    cost = 73
    heal = 2
    damage = 2

    def cast(self, gs):
        new_mana = gs.mana - self.cost
        new_player_hp = gs.player_hp + self.heal
        new_boss_hp = gs.boss_hp - self.damage
        return gs._replace(mana=new_mana, player_hp=new_player_hp, boss_hp=new_boss_hp)

    def is_castable(self, gs):
        return gs.mana >= self.cost

class Shield(Spell):
    cost = 113
    duration = 6
    buff = 7

    def cast(self, gs):
        new_mana = gs.mana - self.cost
        new_shield_timer = self.duration
        return gs._replace(mana=new_mana, shield_timer=new_shield_timer)

    def is_castable(self, gs):
        return (gs.mana >= self.cost and gs.shield_timer <= 1)

    def effect(self, gs):
        if gs.shield_timer >= 1:
            new_damage = max(1, INITIAL_GAME_STATE.damage - self.buff)
            new_shield_timer = gs.shield_timer - 1
            return gs._replace(damage=new_damage, shield_timer=new_shield_timer)
        elif gs.damage != INITIAL_GAME_STATE.damage:
            return gs._replace(damage=INITIAL_GAME_STATE.damage)
        else:
            return gs


class Poison(Spell):
    cost = 173
    duration = 6
    damage = 3

    def cast(self, gs):
        new_mana = gs.mana - self.cost
        new_poison_timer = self.duration
        return gs._replace(mana=new_mana, poison_timer=new_poison_timer)

    def is_castable(self, gs):
        return (gs.mana >= self.cost and gs.poison_timer <= 1)

    def effect(self, gs):
        if gs.poison_timer >= 1:
            new_boss_hp = gs.boss_hp - self.damage
            new_poison_timer = gs.poison_timer - 1
            return gs._replace(boss_hp=new_boss_hp, poison_timer=new_poison_timer)
        else:
            return gs


class Recharge(Spell):
    cost = 229
    duration = 5
    recharge = 101

    def cast(self, gs):
        new_mana = gs.mana - self.cost
        new_recharge_timer = self.duration
        return gs._replace(mana=new_mana, recharge_timer=new_recharge_timer)

    def is_castable(self, gs):
        return (gs.mana >= self.cost and gs.recharge_timer <= 1)

    def effect(self, gs):
        if gs.recharge_timer >= 1:
            new_mana = gs.mana + self.recharge
            new_recharge_timer = gs.recharge_timer - 1
            return gs._replace(mana=new_mana, recharge_timer=new_recharge_timer)
        else:
            return gs


SPELLS = [MagicMissile(), Drain(), Poison(), Shield(), Recharge()]