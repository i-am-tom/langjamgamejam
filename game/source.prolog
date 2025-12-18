is-start-location("room").
is-win-state :- current-location("outside").

# Outside.

has-name("outside", "the great outdoors").
has-description("outside", "the end of the game!").

# The room.

has-name("room", "the first room").
has-description("room", "where are we?").

can-travel-to("room", "outside")
  :- state("door", "is-open").

## Table.

has-name("table", "a nice-looking table").
has-description("table", "a nice place for a key.").
contains("room", "table").

has-name("key", "a mystery key").
has-description("key", "this might open a door.").
contains("table", "key") :- \+ has-item("key").
can-pick-up("key") :- contains("table", "key").

## Door.

contains("room", "door").
has-name("door", "a big door").
has-description("door", "a big, scary door.").
can-open-with("door", "key").
