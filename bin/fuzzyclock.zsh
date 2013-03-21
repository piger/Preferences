#!/usr/bin/env zsh
# Fuzzyclock.zsh 1.2
# Un orologio approssimativo localizzato in romanaccio.
# Trattasi di porting dello script python FuzzyClock, dall'idea
# dell'applet per OS X FuzzyClock. Riscritto in zsh per "ottimizzare".
# Daniel Kertesz <daniel@spatof.org> - Ottobre 2009
#
# ChangeLog:
# 1.0 - Prima versione apparentemente funzionante.
# 1.1 - Pulizia codice e fix minori.
# 1.2 - Migliorata la traduzione in romanaccio.

# in zsh il primo elemento di un array e' 1, con questa opzione
# diventa 0, pero' cambia sintassi: $array[0] diventa ${array[0]}.
setopt KSH_ARRAYS
setopt extended_glob	# per pattern matching con backreference

# CONFIGURAZIONE
default_fuzzyness=1	# valori possibili: 1-4

# %0 viene sostituito con l'ora attuale,
# %1 con l'ora successiva.
nomiMinuti=(
    "%0 spaccate"
    "%0 eccinque"
    "%0 eddieci"
    "%0 enquarto"
    "%0 evventi"
    "%0 evventicinque"
    "%0 emmezza"
    "%0 ettrentacinque"
    "%1 meno venti"
    "%1 menonquarto"
    "%1 meno dieci"
    "%1 meno cinque"
    "quasi che %1"
)

nomiOre=(
    "l'una"
    "e due"
    "e tre"
    "e quattro"
    "e cinque"
    "'e sei"
    "e sette"
    "l'otto"
    "e nove"
    "e dieci"
    "l'unnici"
    "a mezza"
)

giornata=(
    "E' svario tardi"
    "E' madina presto"
    "E' madina"
    "E' quasi la mezza"
    "E' mezzoggiorno"
    "E' pomeriggio"
    "Er crepuscolo"
    "E' nnotte"
)

settimana=(
    "L'inizio de sta settimana demmerda"
    "In mezzo a sta cazzo de settimana"
    "Sta settimana demmerda che sta a fini'"
    "Weekend!"
)

# MAIN()
timearray=${(z)$(date +"%H %M %w")}
ore=${timearray[0]}
minuti=${timearray[1]}
giorno=${timearray[2]}
sector=0

parse_options() {
    o_fuzzyness=(-f $default_fuzzyness)

    zparseopts -K -- f:=o_fuzzyness h=o_help
    if [[ $? != 0 || "$o_help" != "" ]]; then
		echo Usage: $(basename "$0") "[-f fuzzyness level] [-h]"
		exit 1
    fi

    fuzzyness=${o_fuzzyness[1]}
}

fuzzyClock() {
	if (( $fuzzyness == 1 || $fuzzyness == 2 )); then
		if (( $fuzzyness == 1 )); then
			if (( $minuti > 2 )); then
				((sector = (minuti - 3 ) / 5 + 1))
			fi
		else
			if (( $minuti > 6 )); then
				((sector = ((minuti - 7) / 15 + 1) * 3))
			fi
		fi

		if [[ ${nomiMinuti[$sector]} = *%(#b)([0-9])* ]]; then
			delta=${match[0]}
		else
			print "Missing time delta (%0 or %1) in time string \"${nomiMinuti[$sector]}\""
			exit 1
		fi

		if (( (($ore + $delta) % 12 ) > 0 )); then
			((realhour = (ore + delta) % 12 - 1))
		else
			((realhour = 12 - ((ore + delta) % 12 + 1)))
		fi

		# WORKAROUNDS
		# "l'una spaccate" -> "l'una precisa"
		if (( $realhour == 0 )); then
			nomiMinuti[0]="%0 precisa"
		fi

		sub="%[0-9]"
		fuzzyTime=${${nomiMinuti[$sector]}/${~sub}/${nomiOre[$realhour]}}

	elif (( $fuzzyness == 3 )); then
		fuzzyTime=${giornata[(($ore / 3))]}

	else
		if (( $giorno == 1 )); then
			fuzzyTime=${settimana[0]}
		elif (( $giorno >= 2 && $giorno <= 4 )); then
			fuzzyTime=${settimana[1]}
		elif [[ $giorno == 5 ]]; then
			fuzzyTime=${settimana[2]}
		else
			fuzzyTime=${settimana[3]}
		fi
	fi

	print $fuzzyTime
}

parse_options $*
fuzzyClock
