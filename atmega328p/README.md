## Génération des fichiers à partir de GPIO

Pour générer les fichiers sources du composant GPIO, utilisez la commande suivante :

```sh
make
```

*Ceci génère les fichiers `portd.c` et `portd.h`, et le fichier `test_portd.elf` dans le dossier samples*

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

## Réinitialisation du dossier `atmega328p`

Pour nettoyer les fichiers générés et rétablir l'état initial du dossier, utilisez :

```sh
make clean
```

Ou si vous souhaitez nettoyer le dossier `samples`, il est possible de le faire à distance avec la commande :

```sh
make distclean
```

> **Remarque :** Actuellement, les fichiers de test ne fonctionnent pas avec la carte **ATmega328P**.
