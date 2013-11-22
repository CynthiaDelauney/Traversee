## Traversee

Resolution du problème des explorateurs et des adorateurs

### Compilation

```
ocamlopt deQueue.ml -o traversee traversee.ml
```

### Usage

* Pour voir le chemin pour ```n``` explorateurs/adorateurs et une barque de capacité ```k```

```
./traversee -chemin n k
```

* Pour afficher la courbe de temps d'exécution pour tous les chemins jusqu'à ```N```, k est fixée à 4

```
./traversee -courbe N
```
