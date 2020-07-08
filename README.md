# progetto-corso-linguaggi-e-compilatori
Progetto sviluppato attraverso l'utilizzo di diverse tecnologie (quali Haskell, Happy, Alex, BNFC, etc.) per il corso di "linguaggi e compilatori" durante il percorso di laurea magistrale in informatica.  
Nella presente repository è caricato solo il lavoro svolto per la quarta parte del progetto.  
Di seguito è riportata una breve descrizione della richiesta del progetto, per avere informazioni più complete riguardo la soluzione proposta si veda il [report](report.pdf) relativo.


## Descrizione
Il progetto richiedeva di creare un linguaggio di programmazione a partire dalla sintassi del linguaggio <b>E</b>, ovvero si chiedeva si costruire l'intero front-end di un compilatore in grado di trasformare il codice del linguaggio sorgente creato in three-address code (TAC).  
In particolare i macro passaggi richiesti erano:
* progettare una sintassi adeguta alle operazioni richieste, eventualmente estendendo quella del linguaggio <b>E</b> nel caso questo non le possieda nativamente
* progettare un type-system adeguato
* implementare un lexer (attraverso l'utilizzo di Alex) e un parser (attraverso l'utilizzo di Happy) del linguaggio creato
* implementre un type-checker del linguaggio creato
* implementare il layer di trasformazione dal linguaggio creato al TAC
* predisporre dei testcase significativi

Diverse restrizioni erano applicate ad ogni passaggio sopra citato.
