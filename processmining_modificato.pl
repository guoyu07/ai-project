/***
	@descr Implementazione dell'algoritmo di Process Mining descritto nell'articolo "Rediscovering Workflow Models from Event-Based Data"
	@author Nicola Sanitate
	@date 19/10/2011
*/
:- use_module(library(lists)).

/**
	@descr Start:
	 ricava il modello di workflow partendo da un log di workflow tramite i seguenti passi:
	 <ol>
	  <li>lettura del log di workflow da file workflow.log</li>
	  <li>costruzione della D/F-Table</li>
	  <li>induzione del D/F-Graph</li>
	  <li>generazione della WF-Net</li>
	  <li>stampa della rete trovata su file wf-net-out e del log del processo su file processmining.log</li>
	 </ol>
	@form start
*/
start :-
	write('Inserire la percentuale di rumore: '),
	read(PercentualeRumore),
	FattoreRumore is PercentualeRumore / 100,
	open('workflow.log',read,InputStream),
	write('Lettura del file di input\n'),
	lettura(InputStream,Sequenze),
	close(InputStream),
	flatten(Sequenze,TransizioniRegistrate),
	remove_duplicates(TransizioniRegistrate,TransizioniDistinte),
	length(Sequenze,RigheLog),
	length(TransizioniDistinte,NumeroTransizioni),
	Soglia is 1 + round(FattoreRumore * RigheLog / NumeroTransizioni),
	inversione(Sequenze,SequenzeInvertite),
	open('processmining.log', write, OutputStream),
	write('Costruzione della D/F-Table\n'),
	write(OutputStream,'\tD/F-Table\n'),
	costruzione_dftable(TransizioniDistinte,TransizioniDistinte,Sequenze,SequenzeInvertite,OutputStream),
	write('Induzione del D/F-Graph\n'),
	write(OutputStream,'\n\n\tD/F-Graph\n\n'),
	induzione_dfgraph(TransizioniDistinte,FattoreRumore,Soglia,OutputStream),
	write('Generazione della WF-Net\n'),
	write(OutputStream,'\n\n\tWF-Net\n\n'),
	generazione_wfnet(TransizioniDistinte,Soglia,OutputStream),
	close(OutputStream),
	write('Scrittura del file di output\n'),
	scrittura,
	write('Terminato con successo\n'),
	abolish(riga/3),
	abolish(arco/2),
	abolish(split/2),
	abolish(join/2),
	abolish(in/2),
	abolish(out/2).

/**
	@descr Lettura del file di input:
	 legge il file di input per prelevare le sequenze di transizioni
	@form lettura(+InputStream,-Sequenze)
*/
lettura(InputStream,[]) :-
	at_end_of_stream(InputStream).
lettura(InputStream,[Sequenza|AltreSequenze]) :-
	lettura_sequenza(InputStream,0,Sequenza),
	lettura(InputStream,AltreSequenze).
	
/**
	@descr Lettura di una sequenza:
	 legge il file di input per prelevare le transizioni di una sequenza
	@form lettura_sequenza(+InputStream,+FineRiga,-Transizioni)
*/
lettura_sequenza(InputStream,1,[]).
lettura_sequenza(InputStream,0,[Transizione|AltreTransizioni]) :-
	lettura_transizione(InputStream,FineRiga,ListaCaratteri),
	atom_chars(Transizione,ListaCaratteri),
	lettura_sequenza(InputStream,FineRiga,AltreTransizioni).
	
/**
	@descr Lettura di una transizione:
	 legge il file di input per prelevare i caratteri di una transizione
	@form lettura_transizione(+InputStream,-FineRiga,-ListaCaratteri)
*/
lettura_transizione(InputStream,FineRiga,ListaCaratteri) :-
	get_char(InputStream,Carattere),
	lettura_transizione(InputStream,Carattere,FineRiga,ListaCaratteri).
/**
	@descr Lettura di una transizione:
	 legge il file di input per prelevare i caratteri di una transizione
	@form lettura_transizione(+InputStream,+Carattere,-FineRiga,-Transizioni)
*/
lettura_transizione(_,end_of_file,1,[]).
lettura_transizione(_,'\n',1,[]).
lettura_transizione(_,',',0,[]).
lettura_transizione(InputStream,Carattere,FineRiga,[Carattere|AltriCaratteri]) :-
	get_char(InputStream,NuovoCarattere),
	lettura_transizione(InputStream,NuovoCarattere,FineRiga,AltriCaratteri).

/**
	@descr Inversione delle sequenze:
	 inverte le sequenze di transizioni
	@form inversione(+Sequenze,-SequenzeInvertite)
*/
inversione([],[]).
inversione([Sequenza|AltreSequenze],[SequenzaInvertita|AltreSequenzeInvertite]) :-
	reverse(Sequenza,SequenzaInvertita),
	inversione(AltreSequenze,AltreSequenzeInvertite).

/**
	@descr Costruzione della D/F-table:
	 costruisce la D/F-Table partendo dalle sequenze di transizioni trovate nel file di log
	@form costruzione_dftable(+Transizioni,+Transizioni,+Sequenze,+SequenzeInvertite,+OutputStream)
*/
costruzione_dftable([],_,_,_,_).
costruzione_dftable([Transizione|AltreTransizioni],Transizioni,Sequenze,SequenzeInvertite,OutputStream) :-
	frequenza(Transizione,Sequenze,Frequenza),
	write(OutputStream,'\nA = '),
	write(OutputStream,Transizione),
	write(OutputStream,'\n#A = '),
	write(OutputStream,Frequenza),
	write(OutputStream,'\n_____________________________________________________________\n'),
	write(OutputStream,'B\tB<A\tA>B\tB<<<A\tA>>>B\tA→B\n'),
	write(OutputStream,'_____________________________________________________________\n'),
	sottotabella(Transizione,Transizioni,Sequenze,SequenzeInvertite,Frequenza,Sottotabella,OutputStream),
	assert(riga(Transizione,Frequenza,Sottotabella)),
	write(OutputStream,'_____________________________________________________________\n'),
	costruzione_dftable(AltreTransizioni,Transizioni,Sequenze,SequenzeInvertite,OutputStream).
	
/**
	@descr Calcolo della frequenza nella lista di sequenze:
	 calcola la frequenza di una transizione nelle sequenze
	@form frequenza(+Transizione,+Sequenze,-Frequenza)
*/
frequenza(_,[],0).
frequenza(Transizione,[Sequenza|AltreSequenze],Frequenza) :-
	frequenza_sequenza(Transizione,Sequenza,FrequenzaSequenza),
	frequenza(Transizione,AltreSequenze,FrequenzaAltreSequenze),
	Frequenza is FrequenzaSequenza + FrequenzaAltreSequenze.

/**
	@descr Calcolo della frequenza all'interno di una sequenza:
	 calcola la frequenza di una transizione all'interno di una sequenza
	@form frequenza_sequenza(+Transizione,+Sequenza,-Frequenza)
*/
frequenza_sequenza(_,[],0).
frequenza_sequenza(Transizione,[Transizione|AltreTransizioni],Frequenza) :-
	frequenza_sequenza(Transizione,AltreTransizioni,FrequenzaAltreTransizioni),
	Frequenza is FrequenzaAltreTransizioni + 1.
frequenza_sequenza(Transizione,[_|AltreTransizioni],Frequenza) :-
	frequenza_sequenza(Transizione,AltreTransizioni,Frequenza).

/**
	@descr Costruzione della sottotabella:
	 calcola la riga della tabella per ogni transizione
	@form sottotabella(+TransizioneEsaminata,+Transizioni,+Sequenze,+SequenzeInvertite,+Frequenza,-Sottotabella,+OutputStream)
*/
sottotabella(_,[],_,_,_,[],_).
sottotabella(TransizioneEsaminata,[Transizione|AltreTransizioni],Sequenze,SequenzeInvertite,Frequenza,[[Transizione,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita]|AltreRighe],OutputStream) :-
	successione_diretta(TransizioneEsaminata,Transizione,SequenzeInvertite,PrecedenzaDiretta),
	successione_diretta(TransizioneEsaminata,Transizione,Sequenze,SuccessioneDiretta),
	successione(TransizioneEsaminata,Transizione,SequenzeInvertite,Precedenza,DecrementoCausalita),
	successione(TransizioneEsaminata,Transizione,Sequenze,Successione,IncrementoCausalita),
	Causalita is (IncrementoCausalita - DecrementoCausalita) / Frequenza,
	write(OutputStream,Transizione),
	write(OutputStream,'\t'),
	write(OutputStream,PrecedenzaDiretta),
	write(OutputStream,'\t'),
	write(OutputStream,SuccessioneDiretta),
	write(OutputStream,'\t'),
	write(OutputStream,Precedenza),
	write(OutputStream,'\t'),
	write(OutputStream,Successione),
	write(OutputStream,'\t'),
	write(OutputStream,Causalita),
	nl(OutputStream),
	sottotabella(TransizioneEsaminata,AltreTransizioni,Sequenze,SequenzeInvertite,Frequenza,AltreRighe,OutputStream).

/**
	@descr Calcolo della successione diretta nella lista di sequenze:
	 calcola la successione diretta di una transizione rispetto ad un altra;
	 se usata con le sequenze invertite calcola la precedenza diretta
	@form successione_diretta(+TransizioneEsaminata,+Transizione,+Sequenze,-SuccessioneDiretta)
*/
successione_diretta(_,_,[],0).
successione_diretta(TransizioneEsaminata,Transizione,[Sequenza|AltreSequenze],SuccessioneDiretta) :-
	successione_diretta_sequenza(TransizioneEsaminata,Transizione,Sequenza,SuccessioneDirettaSequenza),
	successione_diretta(TransizioneEsaminata,Transizione,AltreSequenze,SuccessioneDirettaAltreSequenze),
	SuccessioneDiretta is SuccessioneDirettaSequenza + SuccessioneDirettaAltreSequenze.

/**
	@descr Calcolo della successione diretta all'interno di una sequenza:
	 calcola la successione diretta all'interno di una sequenza;
	 se la sequenza è invertita calcola la precedenza diretta
	@form successione_diretta_sequenza(+TransizioneEsaminata,+Transizione,+Sequenza,-SuccessioneDiretta)
*/
successione_diretta_sequenza(_,_,[],0).
successione_diretta_sequenza(TransizioneEsaminata,Transizione,[TransizioneEsaminata,Transizione|AltreTransizioni],SuccessioneDiretta) :-
	successione_diretta_sequenza(TransizioneEsaminata,Transizione,[Transizione|AltreTransizioni],SuccessioneDirettaAltreSequenze),
	SuccessioneDiretta is SuccessioneDirettaAltreSequenze + 1.
successione_diretta_sequenza(TransizioneEsaminata,Transizione,[_|AltreTransizioni],SuccessioneDiretta) :-
	successione_diretta_sequenza(TransizioneEsaminata,Transizione,AltreTransizioni,SuccessioneDiretta).

/**
	@descr Calcolo della successione nella lista di sequenze:
	 calcola la successione di una transizione rispetto ad un altra;
	 se usata con le sequenze invertite calcola la precedenza
	@form successione(+TransizioneEsaminata,+Transizione,+Sequenze,-Successione,-IncrementoCausalita)
*/
successione(_,_,[],0,0).
successione(TransizioneEsaminata,Transizione,[Sequenza|AltreSequenze],Successione,IncrementoCausalita) :-
	successione_sequenza(TransizioneEsaminata,Transizione,Sequenza,SuccessioneSequenza,IncrementoCausalitaSequenza),
	successione(TransizioneEsaminata,Transizione,AltreSequenze,SuccessioneAltreSequenze,IncrementoCausalitaAltreSequenze),
	Successione is SuccessioneSequenza + SuccessioneAltreSequenze,
	IncrementoCausalita is IncrementoCausalitaSequenza + IncrementoCausalitaAltreSequenze.
	
/**
	@descr Calcolo della successione all'interno di una sequenza:
	 calcola la successione all'interno di una sequenza;
	 se la sequenza è invertita calcola la precedenza
	@form successione_sequenza(+TransizioneEsaminata,+Transizione,+Sequenza,-Successione,-IncrementoCausalita)
*/
successione_sequenza(_,_,[],0,0).
successione_sequenza(TransizioneEsaminata,Transizione,[TransizioneEsaminata|AltreTransizioni],Successione,IncrementoCausalita) :-
	analisi_successione(TransizioneEsaminata,Transizione,AltreTransizioni,SuccessioneTransizione,TransizioniIntermedie),
	successione_sequenza(TransizioneEsaminata,Transizione,AltreTransizioni,SuccessioneAltreTransizioni,IncrementoCausalitaAltreTransizioni),
	Successione is SuccessioneTransizione + SuccessioneAltreTransizioni,
	IncrementoCausalita is (0.8 ** TransizioniIntermedie) * SuccessioneTransizione + IncrementoCausalitaAltreTransizioni.
successione_sequenza(TransizioneEsaminata,Transizione,[_|AltreTransizioni],Successione,IncrementoCausalita) :-
	successione_sequenza(TransizioneEsaminata,Transizione,AltreTransizioni,Successione,IncrementoCausalita).

/**
	@descr Analisi della successione:
	 calcola la distanza di una successione;
	 se la sequenza è invertita calcola la distanza di una precedenza
	@form analisi_successione(+TransizioneEsaminata,+Transizione,+Sequenza,-Successione,-TransizioniIntermedie)
*/
analisi_successione(_,_,[],0,0).
analisi_successione(_,Transizione,[Transizione|_],1,0).
analisi_successione(TransizioneEsaminata,_,[TransizioneEsaminata|_],0,0).
analisi_successione(TransizioneEsaminata,Transizione,[_|AltreTransizioni],Successione,TransizioniIntermedie) :-
	analisi_successione(TransizioneEsaminata,Transizione,AltreTransizioni,Successione,TransizioniIntermedieAltreTransizioni),
	TransizioniIntermedie is TransizioniIntermedieAltreTransizioni + 1.

/**
	@descr Induzione del D/F-graph:
	 costruisce la D/F-Graph partendo dalle D/F-Table ottenute precedentemente
	@form induzione_dfgraph(+Transizioni,+FattoreRumore,+Soglia,+OutputStream)
*/
induzione_dfgraph([],_,_,_).
induzione_dfgraph([Transizione|AltreTransizioni],FattoreRumore,Soglia,OutputStream) :-
	riga(Transizione,Frequenza,Sottotabella),
	induzione_transizione(Transizione,Frequenza,Sottotabella,FattoreRumore,Soglia,OutputStream),
	induzione_dfgraph(AltreTransizioni,FattoreRumore,Soglia,OutputStream).

/**
	@descr Induzione della parte del grafo inerente ad una transizione:
	 costruisce la porzione di D/F-Graph inerente ad una transizione
	@form induzione_transizione(+Transizione,+Frequenza,+Sottotabella,+FattoreRumore,+Soglia,+OutputStream)
*/
induzione_transizione(_,_,[],_,_,_).
induzione_transizione(Transizione,Frequenza,[Riga|AltreRighe],FattoreRumore,Soglia,OutputStream) :-
	applicazione_euristica(Transizione,Frequenza,Riga,FattoreRumore,Soglia,OutputStream),
	induzione_transizione(Transizione,Frequenza,AltreRighe,FattoreRumore,Soglia,OutputStream).
induzione_transizione(Transizione,Frequenza,[_|AltreRighe],FattoreRumore,Soglia,OutputStream) :-
	induzione_transizione(Transizione,Frequenza,AltreRighe,FattoreRumore,Soglia,OutputStream).

/**
	@descr Applicazione dell'euristica:
	 applica l'euristica più opportuna
	@form applicazione_euristica(+Transizione,+Frequenza,+Riga,+FattoreRumore,+Soglia,+OutputStream)
*/
applicazione_euristica(Transizione,Frequenza,[Transizione,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita],FattoreRumore,Soglia,OutputStream) :-
	euristica2(Transizione,Frequenza,PrecedenzaDiretta,SuccessioneDiretta,Causalita,FattoreRumore,OutputStream).
applicazione_euristica(TransizionePartenza,Frequenza,[TransizioneDestinazione,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita],FattoreRumore,Soglia,OutputStream) :-
	euristica1(TransizionePartenza,TransizioneDestinazione,PrecedenzaDiretta,SuccessioneDiretta,Causalita,FattoreRumore,Soglia,OutputStream).
applicazione_euristica(TransizionePartenza,Frequenza,[TransizioneDestinazione,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita],FattoreRumore,Soglia,OutputStream) :-
	euristica3(TransizionePartenza,TransizioneDestinazione,Frequenza,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita,FattoreRumore,Soglia,OutputStream).

/**
	@descr Euristica 1:
	 implementazione della prima euristica;
	 l'arco trovato verrà stampato su file processmining.log
	@form euristica1(+TransizionePartenza,+TransizioneDestinazione,+PrecedenzaDiretta,+SuccessioneDiretta,+Causalita,+FattoreRumore,+Soglia,+OutputStream)
*/
euristica1(TransizionePartenza,TransizioneDestinazione,PrecedenzaDiretta,SuccessioneDiretta,Causalita,FattoreRumore,Soglia,OutputStream) :-
	Causalita >= FattoreRumore,
	SuccessioneDiretta >= Soglia,
	PrecedenzaDiretta =< Soglia,
	assert(arco(TransizionePartenza,TransizioneDestinazione)),
	write(OutputStream,[TransizionePartenza,TransizioneDestinazione]),
	write(OutputStream,'  \t → euristica 1\n').

/**
	@descr Euristica 2:
	 implementazione della seconda euristica;
	 l'arco trovato verrà stampato su file processmining.log
	@form euristica2(+Transizione,+Frequenza,+PrecedenzaDiretta,+SuccessioneDiretta,+Causalita,+FattoreRumore,+OutputStream)
*/
euristica2(Transizione,Frequenza,PrecedenzaDiretta,SuccessioneDiretta,Causalita,FattoreRumore,OutputStream) :-
	abs(Causalita) =< FattoreRumore,
	(PrecedenzaDiretta + SuccessioneDiretta) > (0.5 * Frequenza),
	abs(PrecedenzaDiretta - SuccessioneDiretta) =< FattoreRumore * ((PrecedenzaDiretta + SuccessioneDiretta)/2),
	assert(arco(Transizione,Transizione)),
	write(OutputStream,[Transizione,Transizione]),
	write(OutputStream,'  \t → euristica 2\n').

/**
	@descr Euristica 3:
	 implementazione della terza euristica;
	 l'arco trovato verrà stampato su file processmining.log
	@form euristica3(+TransizionePartenza,+TransizioneDestinazione,+Frequenza,+PrecedenzaDiretta,+SuccessioneDiretta,+Precedenza,+Successione,+Causalita,+FattoreRumore,+Soglia,+OutputStream)
*/
euristica3(TransizionePartenza,TransizioneDestinazione,Frequenza,PrecedenzaDiretta,SuccessioneDiretta,Precedenza,Successione,Causalita,FattoreRumore,Soglia,OutputStream) :-
	abs(Causalita) =< FattoreRumore,
	SuccessioneDiretta >= Soglia,
	abs(PrecedenzaDiretta - SuccessioneDiretta) =< FattoreRumore * ((PrecedenzaDiretta + SuccessioneDiretta)/2),
	Successione >= (0.4 * Frequenza),
	abs(Precedenza - Successione) =< FattoreRumore * ((Precedenza+Successione)/2),
	assert(arco(TransizionePartenza,TransizioneDestinazione)),
	write(OutputStream,[TransizionePartenza,TransizioneDestinazione]),
	write(OutputStream,'  \t → euristica 3\n').

/**
	@descr Generazione della WF-net:
	 costruisce la WF_Net partendo dal D/F-Graph ottenuto precedentemente
	@form generazione_wfnet(+TransizioniDistinte,+Soglia,+OutputStream)
*/
generazione_wfnet(TransizioniDistinte,Soglia,OutputStream) :-
	write(OutputStream,'Split:\n\n'),
	divisioni(TransizioniDistinte,Soglia,OutputStream),
	write(OutputStream,'\n\nJoin:\n\n'),
	unioni(TransizioniDistinte,Soglia,OutputStream),
	setof(X,join(X,[]),TransizioniIniziali),
	asserzione_sb(TransizioniIniziali),
	setof(X,split(X,[]),TransizioniFinali),
	asserzione_se(TransizioniFinali),
	asserzioni(TransizioniDistinte,1).

/**
	@descr Classificazione delle divisioni (split):
	 trova, valuta e stampa su file processmining.log tutte le divisioni del grafo
	@form divisioni(+Transizioni,+Soglia,+OutputStream)
*/	
divisioni([],_,_).
divisioni([Transizione|AltreTransizioni],Soglia,OutputStream) :-
	findall(X,arco(Transizione,X),TransizioniDestinazione),
	insiemi(TransizioniDestinazione,Soglia,DivisioneTransizione),
	assert(split(Transizione,DivisioneTransizione)),
	write(OutputStream,Transizione),
	write(OutputStream,'\t'),
	stampa_insieme(DivisioneTransizione,OutputStream),
	nl(OutputStream),
	divisioni(AltreTransizioni,Soglia,OutputStream).
	
/**
	@descr Classificazione delle unioni (join):
	 trova, valuta e stampa su file processmining.log tutte le unioni del grafo
	@form divisioni(+Transizioni,+Soglia,+OutputStream)
*/
unioni([],_,_).
unioni([Transizione|AltreTransizioni],Soglia,OutputStream) :-
	findall(X,arco(X,Transizione),TransizioniPartenza),
	insiemi(TransizioniPartenza,Soglia,UnioneTransizione),
	assert(join(Transizione,UnioneTransizione)),
	write(OutputStream,Transizione),
	write(OutputStream,'\t'),
	stampa_insieme(UnioneTransizione,OutputStream),
	nl(OutputStream),
	unioni(AltreTransizioni,Soglia,OutputStream).

/**
	@descr Costruzione degli insiemi:
	 crea gli insiemi utili a definire la tipologia di una divisione o di una unione
	@form insiemi(+Transizioni,+Soglia,-Insiemi)
*/
insiemi([],_,[]).
insiemi([Transizione|AltreTransizioni],Soglia,Insiemi) :-
	insiemi(AltreTransizioni,Soglia,VecchiInsiemi),
	collocazione(Transizione,Soglia,VecchiInsiemi,Insiemi).

/**
	@descr Collocazione di una transizioni nell'insieme più opportuno:
	 colloca una transizione nel giusto insieme
	@form collocazione(+Transizione,+Soglia,+Insiemi,-NuoviInsiemi)
*/
collocazione(Transizione,_,[],[[Transizione]]).
collocazione(Transizione,Soglia,[Insieme|AltriInsiemi],[[Transizione|Insieme]|AltriInsiemi]) :-
	valutazione(Transizione,Soglia,Insieme).
collocazione(Transizione,Soglia,[Insieme|AltriInsiemi],[Insieme|NuoviInsiemi]) :-
	collocazione(Transizione,Soglia,AltriInsiemi,NuoviInsiemi).

/**
	@descr Valutazione dei requisiti di inserimento di una transizioni in un insieme:
	 termina se una transizione può essere inserita in un insieme, altrimenti fallisce
	@form valutazione(+Transizione,+Soglia,+Insieme)
*/
valutazione(_,_,[]).
valutazione(Transizione,Soglia,[ElementoInsieme|AltriElementiInsieme]) :-
	riga(Transizione,_,TabellaTransizione),
	ricerca_successione_diretta(ElementoInsieme,TabellaTransizione,SuccessioneDirettaTransizione),
	!,
	SuccessioneDirettaTransizione < Soglia,
	riga(ElementoInsieme,_,TabellaElementoInsieme),
	ricerca_successione_diretta(Transizione,TabellaElementoInsieme,SuccessioneDirettaElementoInsieme),
	!,
	SuccessioneDirettaElementoInsieme < Soglia,
	valutazione(Transizione,Soglia,AltriElementiInsieme).

/**
	@descr Ricerca della successione diretta relativa ad un transizione in una sottotabella:
	 ricerca la successione diretta inerente ad una specifica transizione in una specifica sottotabella della D/F-Table
	@form cerca_successione_diretta(+Transizione,+Tabella,-SuccessioneDiretta)
*/
ricerca_successione_diretta(Transizione,[[Transizione,_,SuccessioneDiretta,_,_,_]|_],SuccessioneDiretta).
ricerca_successione_diretta(Transizione,[_|AltreRighe],SuccessioneDiretta) :-
	ricerca_successione_diretta(Transizione,AltreRighe,SuccessioneDiretta).

/**
	@descr Stampa di un insieme:
	 stampa un insieme in forma preposizionale su file processmining.log
	@form stampa_insieme(+Insieme,+OutputStream)
*/
stampa_insieme([],OutputStream) :-
	write(OutputStream,'()').
stampa_insieme([Sottoinsieme],OutputStream) :-
	write(OutputStream,'('),
	stampa_sottoinsieme(Sottoinsieme,OutputStream),
	write(OutputStream,')').
stampa_insieme([Sottoinsieme|AltriSottoinsiemi],OutputStream) :-
	write(OutputStream,'('),
	stampa_sottoinsieme(Sottoinsieme,OutputStream),
	write(OutputStream,')'),
	write(OutputStream,' AND '),
	stampa_insieme(AltriSottoinsiemi,OutputStream).

/**
	@descr Stampa di un sottoinsieme:
	 stampa un sottoinsieme in forma preposizionale su file processmining.log
	@form stampa_sottoinsieme(+Sottoinsieme,+OutputStream)
*/
stampa_sottoinsieme([Elemento],OutputStream) :-
	write(OutputStream,Elemento).
stampa_sottoinsieme([Elemento|AltriElementi],OutputStream) :-
	write(OutputStream,Elemento),
	write(OutputStream,' OR '),
	stampa_sottoinsieme(AltriElementi,OutputStream).

/**
	@descr Asserzione degli archi iniziali:
	 asserisce tutti gli archi iniziali della WF-Net
	@form asserzione_sb(+TransizioniIniziali)
*/
asserzione_sb([]).
asserzione_sb([Transizione|AltreTransizioni]) :-
	assert(in(sb,Transizione)),
	asserzione_sb(AltreTransizioni).
	
/**
	@descr Asserzione degli archi finali:
	 asserisce tutti gli archi finali della WF-Net
	@form asserzione_se(+TransizioniFinali)
*/
asserzione_se([]).
asserzione_se([Transizione|AltreTransizioni]) :-
	assert(out(Transizione,se)),
	asserzione_se(AltreTransizioni).

/**
	@descr Asserzioni degli archi:
	 asserisce tutti gli archi della WF-Net creando incrementalmente i posti necessari
	@form asserzioni(+TransizioniDistinte,+NumeroPosto)
*/
asserzioni([],_).
asserzioni([Transizione|AltreTransizioni],NumeroPosto) :-
	split(Transizione,DivisioneTransizione),
	valutazione_divisione(Transizione,DivisioneTransizione,NumeroPosto,NuovoNumeroPosto),
	asserzioni(AltreTransizioni,NuovoNumeroPosto).

/**
	@descr Valutazione di una divisione:
	 valuta gli archi uscenti da una specifica transizione
	@form valutazione_divisione(+Transizione,+DivisioniTransizione,+NumeroPosto,+NuovoNumeroPosto)
*/
valutazione_divisione(_,[],NumeroPosto,NumeroPosto).
valutazione_divisione(Transizione,[[TransizioneDestinazione|_]|AltreDivisioniTransizione],NumeroPosto,NuovoNumeroPosto) :-
	out(Transizione,Posto),
	in(Posto,TransizioneDestinazione),
	valutazione_divisione(Transizione,AltreDivisioniTransizione,NumeroPosto,NuovoNumeroPosto).
valutazione_divisione(Transizione,[DivisioneTransizione|AltreDivisioniTransizione],NumeroPosto,NuovoNumeroPosto) :-
	number_codes(NumeroPosto,CaratteriNumeroPosto),
	atom_chars(Posto,[p|CaratteriNumeroPosto]),
	NumeroPostoAggiornato is NumeroPosto + 1,
	asserzione_confini_posto(Transizione,DivisioneTransizione,Posto),
	valutazione_divisione(Transizione,AltreDivisioniTransizione,NumeroPostoAggiornato,NuovoNumeroPosto).

/**
	@descr Asserzione dei confini di un posto:
	 ottiene tutte le transizioni connesse ad un posto e asserisce tali connessioni
	@form asserzione_confini_posto(+Transizione,+TransizioniDestinazione,+Posto)
*/
asserzione_confini_posto(Transizione,[TransizioneDestinazione|AltreTransizioniDestinazione],Posto) :-
	join(TransizioneDestinazione,TransizioniPartenza),
	sottoinsieme_transizione(Transizione,TransizioniPartenza,UnioneTransizione),
	asserzione_in([TransizioneDestinazione|AltreTransizioniDestinazione],Posto),
	asserzione_out(UnioneTransizione,Posto).

/**
	@descr Ricerca del sottoinsieme in cui compare una transizione:
	 cerca il sottoinsieme di un insieme in cui compare una transizione
	@form sottoinsieme_transizione(+Transizione,+Insieme,-Sottoinsieme)
*/	
sottoinsieme_transizione(Transizione,[Sottoinsieme|_],Sottoinsieme) :-
	member(Transizione,Sottoinsieme).
sottoinsieme_transizione(Transizione,[_|AltriSottoinsiemi],Sottoinsieme):-
	sottoinsieme_transizione(Transizione,AltriSottoinsiemi,Sottoinsieme).

/**
	@descr Asserzione di archi da un posto ad un insieme di transizioni:
	 asserisce tutti gli archi in entrata da uno specifico posto
	@form asserzione_in(+TransizioniDestinazione,+Posto)
*/
asserzione_in([],_).
asserzione_in([TransizioneDestinazione|AltreTransizioniDestinazione],Posto) :-
	assert(in(Posto,TransizioneDestinazione)),
	asserzione_in(AltreTransizioniDestinazione,Posto).

/**
	@descr Asserzione di archi da un insieme di transizioni ad un posto:
	 asserisce tutti gli archi in uscita verso uno specifico posto
	@form asserzione_out(+TransizioniPartenza,+Posto)
*/
asserzione_out([],_).
asserzione_out([TransizionePartenza|AltreTransizioniPartenza],Posto) :-
	assert(out(TransizionePartenza,Posto)),
	asserzione_out(AltreTransizioniPartenza,Posto).

/**
	@descr Scrittura della WF-net nel file di output:
	 riporta la wf-net generata sul file di output
	@form scrittura
*/
scrittura :-
	telling(VecchioStream),
	tell('wf-net-out'),
	listing(in/2),
	listing(out/2),
	told,
	tell(VecchioStream).
