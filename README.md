# Warcraft III Programming Language

A Turing Tarpit language based on the WC3 video game.

## Basics

The execution model reads from two files. The first is the map
like `simple_map.w3n` and the second is a file of instructions
like `hello_world.w3pl`. You can think of the map as the tape
of the turing machine, and the instructions file as the actual
instructions about how to manipulate the machine.

### Execution Model

Like in Warcraft III you start off with 500 Gold and 150 Lumber.
Unlike Warcraft III you have no peasants to start. Gold allows
you to create new peasants (thread-like) which can gather more
gold or lumber and build buildings. From the buildings you can
train more useful units such as the footman (thread++) who can
do input and output as well as some basic arithmetic.

If I have time I'll add more buildings and units, but for now
I think this is well enough.

The execution model views the instructions as being N separate
lists of of instructions, where N is the number of spawn commands.
At the beginning of each 'cycle' the runtime will try to spawn
a peasant if there is a spawn command left otherwise it will update
each unit based on the instructions given to it.

### List Of Instructions

* `s` - Spawn a new peasant at the town hall.
* `>` `<` `^` `v` - Movement commands (right, left, up, down)
* `g` - Gather the resource at your current location
* `r` - Return the resource to your town hall
* `bf` - Build a Farm to create more food supply to support more units
* `bb` - Build a Barracks to allow creation of footmen
* `t` - Train a new footman
* `x` - Die, kill the unit, freeing up food resources
* `[` - If "value" at current location is zero jump past matching `]`
* `]` - If "value" at current location is non-zero jump back to matchin `[`

### Maps

Maps consist of a town hall (`t`), a gold mine (`g`), and one or more woods (`w`).
You gather gold from the gold mine and lumber from the woods with a peasant,
and return those resources back to your town hall.

Here's an example of a 10x10 map with a woods, a gold mine, and a town hall:

```
.wg.......
..h.......
..........
..........
..........
..........
..........
..........
..........
..........
```

### Your Programming Language Is Bad And You Should Feel Bad

Yep you're right, but that's kind of the point. It's a turing tarpit.

But with any luck we'll have a DotA port in no time.
