# Batch statement syntax

BEGIN

q<sub>1</sub>;

q<sub>2</sub>;

q<sub>3</sub>;

...

q<sub>n</sub>;

END

where q<sub>i</sub> = some Lib2 query, i = 1, 2, ..., n.

# Marshalling notes

If the current state is `Uninitialized` the state will be marshalled into a single `uninit` query.

Otherwise the state is marshalled into a batch of two queries:

1. uninit
   - this is to reset the state before a new one is loaded, however this logic could be inside of the `load` stateTransition, but that way it would not satisfy the property of being able to paste the contents of the saved file and replicating the logic of `load`
2. init VHS Rental Store:Catalog:`ml`
   - where `ml` is the list of movies

# REPL showcase with loading

```
Welcome! Press [TAB] for auto completion, type ':paste' for multiline input
>>> show
Uninitialized
>>> init VHS Rental Store:Catalog:Pavadinimas,1999,Action,PG,Available                                                          
Successfully initialized
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| addMovies Filmas,1989,Comedy,R,Available,KitasFilmas,1926,Family,G,Available;                                                 
| takeMovie Pavadinimas,1999,Action,PG,Available;                   
| show;
| END
|
Success
Successfully updated
Store VHS Rental Store:Catalog:KitasFilmas,1926,Family,G,Available,Filmas,1989,Comedy,R,Available,Pavadinimas,1999,Action,PG,Rented
>>> save
State saved successfully
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| uninit;
| show;
| END
|
Uninitializing state
Uninitialized
>>> load
Uninitializing state
Successfully initialized
>>> show
Store VHS Rental Store:Catalog:KitasFilmas,1926,Family,G,Available,Filmas,1989,Comedy,R,Available,Pavadinimas,1999,Action,PG,Rented
>>>
Goodbye!
```

State is saved to a file called `state.txt`

`state.txt` content after saving:

```
BEGIN
uninit;
init VHS Rental Store:Catalog:KitasFilmas,1926,Family,G,Available,Filmas,1989,Comedy,R,Available,Pavadinimas,1999,Action,PG,Rented;
END

```