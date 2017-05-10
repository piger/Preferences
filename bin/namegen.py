#!/usr/bin/env python
from __future__ import print_function
import sys
import random


# Adjectives
LEFT = [
    'mellifluo',
    'raggiante',
    'spilorcio',
    'subdolo',
    'goloso',
    'vorace',
    'sagace',
    'astuto',
    'pignolo',
    'saputello',
    'agile',
    'simpatico',
    'buffo',
    'rotondo',
    'insistente',
    'ingordo',
    'allegro',
    'generoso',
    'salterino',
    'stonato',
]

# Names
RIGHT = [
    'kirk',
    'mccoy',
    'scott',
    'spock',
    'uhura',
    'sulu',
    'chekov',
    'riker',
    'picard',
    'laforge',
    'troi',
    'crusher',
    'data',
    'worf',
    'obrien',
    'guinan',
    'sisko',
    'janeway',
    'archer',
    'tpol',
    'tucker',
    'reed',
    'mayweather',
    'khan',
]


def main():
    random.seed()
    result = "{}_{}".format(random.choice(LEFT), random.choice(RIGHT))
    print(result)
    sys.exit(0)


if __name__ == '__main__':
    main()
