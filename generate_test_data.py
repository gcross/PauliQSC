from sys import argv

indentation = ' '*int(argv[1])
number_of_bits = int(argv[2])
number_of_operators = int(argv[3])

paulis = ['I','X','Z','Y']

from random import choice

for i in xrange(number_of_operators):
    operator = ''.join(choice(paulis) for _ in xrange(number_of_bits))
    first_character = ',' if i != 0 else '['
    print "{}{}read \"{}\"".format(indentation,first_character,operator)

print indentation + ']'
