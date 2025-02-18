## Génération des fichiers à partir de GPIO

Pour générer les fichiers sources du composant GPIO, utilisez la commande suivante :

```sh
make
```

*Ceci génère les fichiers `portd.c` et `portd.h`.*

---

## Génération des fichiers de test pour `portd`

Accédez au dossier `samples` et exécutez :

```sh
cd samples
make
```

*Ceci génère les fichiers de test pour `portd`.*

---

## Tester avec l'environnement csim
Basculer sur dossier courant
```sh
./csim-run atmega328p/samples/test_portd.elf -b atmega328p/samples/test_portd.yaml
```

## Réinitialisation du dossier `samples`

Pour nettoyer les fichiers générés et rétablir l'état initial du dossier `samples`, utilisez :

```sh
cd samples
make clean
```

> **Remarque :** Actuellement, les fichiers de test ne fonctionnent pas avec la carte **ATmega328P**.