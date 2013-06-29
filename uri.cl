;-*-Lisp-*-
(defstruct uri
  scheme
  userinfo
  host 
  port 
  path
  query 
  fragment
)

;In pratica, l' applicazione di (schemer) restituisce una lista di liste, dove ogni sottolista e' una parte dell' uri; esse vengono normalizzate, dalla seconda, che
;rappresenta l' authority, sono estratti user e porta ed in seguito l' host, tutte le altre parti sono gia' ben definite; notiamo che sono riconvertite a stringa per
;poi formare gli slot dell' uri
(defun parse-uri (string)
 (let ((a (schemer (coercetolist string) '())))
   (let ((b (mapcar 'normalize a)))
     (let ((uhp (userHostPort (second b))))
       (let ((user (coercetostring (first uhp))))
	 (let ((port (coercetostring (third uhp))))
	   (let ((host (coercetostring (second uhp))))
	     (let ((scheme (coercetostring (first b))))
	       (let ((path (coercetostring (third b))))
		 (let ((query (coercetostring (fourth b))))
		   (let ((fragment (coercetostring (fifth b))))
		     (make-uri :scheme scheme :userinfo user  :host host :port port :path path :query query :fragment fragment))))))))))))

;Questa funzione s' occupa del riconoscimento dello scheme: riceve a parametro due liste, una 
;contenente la lista dell' uri, l' altra, inizialmente vuota, dove vengono impilati i caratteri
;dello scheme.
;In caso di caratteri non da identificatore e' sollevato un errore; quando e' trovato il delim. o
;si e' esaurita la lista uri, si passa una lista contenente le due liste come sottoliste all' 
;apposita funzione di setting. 
;Se la lista non e' vuota e si continua l' azione il delimitatore e' scartato.
(defun schemer (uri scheme)
  (cond ((eq (first uri) #\:) 
	 (setter1 (append (list (cdr uri)) (list (nreverse scheme)))))
        ((null (first uri))
         (error "Invalid scheme structure"))
	((eq (first uri) #\#) 
	 (error "~S is not a valid scheme char" (first uri)))
	((eq (first uri) #\@) 
	 (error "~S is not a valid scheme char" (first uri)))
	((eq (first uri) #\/) 
	 (error "~S is not a valid scheme char" (first uri)))
        ((eq (first uri) #\?) 
	 (error "~S is not a valid scheme char" (first uri)))
	(T (schemer (cdr uri) (append (list (first uri)) scheme)))))

;Questa funzione riceve una lista contenente due sottoliste: la seconda contenente lo scheme, che 
;non puo' essere vuoto, la prima contenente l' uri rimanente.
;Se l' uri rimanente e' vuoto, allora e' gia' stata esaminata l' intera lista, e quindi ritorniamo
;il risultato, contenente il solo schema, grazie ad (append)
;Altrimenti dobbiamo verificare la presenza dell' authority, e quindi dell' host, se lo scheme e'
;uno dei quattro caratteristici, oppure iniziare subito il riconoscimento del path.
;In caso di auth vuoto poniamo una lista con il carattere di spazio (se mettessimo una lista '()
;sparirebbe al primo append).
(defun setter1 (skimAndRest)
  (cond ((and (null (first skimAndRest)) (not (null (second skimAndRest)))) 
	 (append (list (second skimAndRest)) (list '(#\Space)) (list '(#\Space)) (list '(#\Space)) (list '(#\Space))))
        ((null (second skimAndRest)) (error "Empty scheme"))
	(T (authr0 (first skimAndRest) (second skimAndRest) '()))))

;Con questa funzione verifichiamo che sia presente l' authority: infatti condizione 
;necessaria ma non sufficiente e' che vi sia '//'; se cosi' e' possiamo passare a 
;riconoscere l' auth vero e proprio, dopo avere saltato questi due caratteri
(defun authr0 (uri skim auth)
  (if (and (eq (first uri) #\/) (eq (second uri) #\/)) (authr1 (cdr (cdr uri)) skim auth) (pathr uri skim '(#\Space) '())))

;Iniziamo ad impilare i caratteri, stando attenti ai delimitatori delle parti restanti ed
;al fatto che possono esserci o meno
(defun authr1 (uri skim auth)
  (if (or (eq (first uri) #\/) (eq (first uri) #\?) (eq (first uri) #\#) (null (first uri)))
      (setter2 (append (list uri) (list skim) (list (nreverse auth))))
      (authr1 (cdr uri) skim (append (list (first uri)) auth))))

;Questa funzione riconosce il delimitatore che ha posto fine all' authority, ed agisce
;di conseguenza; notiamo che l' host non puo' essere vuoto e che e' possibile anche che
;sia stato esaurito l' uri
(defun setter2 (authAndRest)
  (cond ((eq (first (first authAndRest)) #\/) (pathr (cdr (first authAndRest)) (second authAndRest) (last authAndRest) '()))
	((eq (first (first authAndRest)) #\?) (queryr (cdr (first authAndRest)) (second authAndRest) (last authAndRest) '(#\Space) '()))
	((eq (first (first authAndRest)) #\#) (fragmentr (cdr (first authAndRest)) (second authAndRest) (last authAndRest) '(#\Space) '(#\Space) '()))
	((null (last authAndRest)) (error "Invalid authority"))
	(T (append (list (second authAndRest)) (list (last authAndRest))))))
	

;Questa funzione riceve quattro argomenti, ed immagazzina il path.
;Nel caso in cui si incontrino il carattere '?' o il carattere '#', allora il path e' terminato
;e bisogna passare a riconoscere o la query o il fragment (della scelta si preoccupa la funzione 
;di setting).
(defun pathr (uri skim auth path)
  (cond ((or (eq (first uri) #\?) (eq (first uri) #\#) (null (first uri))) 
	 (setter3 (append (list uri) (list skim) (list auth) (list (nreverse path)))))
	((eq (first uri) #\@) 
	 (error "~S is not a valid path char" (first uri)))
	((eq (first uri) #\:) 
	 (error "~S is not a valid path char" (first uri)))
	(T (pathr (cdr uri) skim auth (append (list (first uri)) path)))))

;Alla funzione arriva una porzione di uri che ha ancora a primo carattere quello che ha causato
;il cambiamento di stato: ci serve saperlo poiche' puo' essere '?' o '#'; ci preoccupiamo poi di
;levarlo; se la path e' vuota torniamo una lista con " "
(defun setter3 (pathAndRest)
  (cond ((and (null (first pathAndRest)) (validatePath0 (normalize (last pathAndRest))))
	 (append (list (second pathAndRest)) (list (third pathAndRest)) (list (validatePath1 (last pathAndRest) (normalize (third pathAndRest))))))
	((and (validatePath0 (normalize (fourth pathAndRest))) (eq (first (first pathAndRest)) #\?))
	 (queryr (cdr (first pathAndRest)) (second pathAndRest) (third pathAndRest) (validatePath1 (last pathAndRest) (normalize (third pathAndRest))) '()))
	((and (validatePath0 (normalize (fourth pathAndRest))) (eq (first (first pathAndRest)) #\#))
	 (fragmentr (cdr (first pathAndRest)) (second pathAndRest) (third pathAndRest) (validatePath1 (last pathAndRest) (normalize (third pathAndRest))) '(#\Space) '()))
	(T (error "Invalid path structure"))))

;Le cose funzionano in modo del tutto analogo per il riconoscimento della query. 
(defun queryr (uri skim auth path query)
  (if (or (eq (first uri) #\#) (null (first uri))) 
      (setter4 (append (list uri) (list skim) (list auth) (list path) (list (nreverse query))))
      (queryr (cdr uri) skim auth path (append (list (first uri)) query))))

;Il problema di evitare la presenza di '?' ma una query vuota e' sorpassato;imponendo che l' accumulatore ricevuto non sia una lista nulla    
(defun setter4 (queryAndRest) 
  (cond ((equal (last queryAndRest) '(nil)) (error "Invalid query"))
	((null (first queryAndRest))
	 (append (list (second queryAndRest)) (list (third queryAndRest)) (list (fourth queryAndRest)) (list (last queryAndRest)) (list '(#\Space))))
	(T (fragmentr (first queryAndRest) (second queryAndRest) (third queryAndRest) (fourth queryAndRest) (nreverse (last queryAndRest)) '()))))

;Il fragment, ha la particolarita' che ogni carattere e' accettato e la funzione di setting restituisce il tutto
(defun fragmentr (uri skim auth path query frag)
  (if (null (first uri)) 
      (setter5 (append (list skim) (list auth) (list path) (list query) (list (nreverse frag))))
      (fragmentr (cdr uri) skim auth path query (append (list (first uri)) frag))))

;Diversamente dalla risoluzione del problema analogo per la query, per 
;evitare fragment malposti esaminiamo che cosa sia: se e' costituito solo
;dal delimitatore si da' errore, altrimenti ripassiamo le singole parti ed
;il fragment depennato del delimitatore, che non deve apparire
(defun setter5 (fragAndRest)
  (if (equal (normalize (last fragAndRest)) '(nil)) 
      (error "Invalid fragment")
      (append (list (first fragAndRest)) (list (second fragAndRest)) (list (third fragAndRest)) (list (fourth fragAndRest)) (list (normalize (last fragAndRest))))))


;Secondo la grammatica path richiesta:
;simple-path ::= [/]<identificatore>[/simple-path]* | <vuoto>
;'/' e' ovviamente un carattere valido, ma non possono esserci parti del tipo '///', o una path iniziante o terminante per '//' 
(defun validatePath0 (path)
  (cond ((< (length path) 3) T)
	((equal '(#\/ #\/ #\/) (list (first path) (second path) (third path))) nil)
	(T (validatePath0 (cdr path)))))
;Attenzione alle liste '(nil), non piacciono a (coerce).
;Dobbiamo fare un po' di lavorio per stabilire la natura della path, se simple-path o path-after-authority ed agire di
;conseguenza
(defun validatePath1 (path auth)
  (cond ((and (equal (normalize path) '(nil)) (equal '(#\Space) (normalize auth))) '(#\Space))
	((and (equal (normalize path) '(nil)) (not (equal '(#\Space) (normalize auth)))) (append '(#\/) '(#\Space)))
	((equal (normalize path) '(#\/)) (error "Invalid path structure"))
	((equal (append (list (first (normalize path))) (list (second (normalize path)))) '(#\/ #\/)) (error "Invalid path structure"))
	((equal (append (list (first (reverse (normalize path)))) (list (second (reverse (normalize path))))) '(#\/ #\/)) (error "Invalid path structure"))
	((not (equal auth '(#\Space))) (append '(#\/) (normalize path)))
	(T path)))

;Con questa funzione determiniamo se l' host sia un nome o un ipv4
(defun hostr0 (host) (if (hostr1 host) host (error "Invalid host")))
(defun hostr1 (host)
  (if (and (not (null host)) (or (name host) (ip host))) T nil))

;Un username e' un identificatore...
(defun userr0 (user) (if (and (not (null user)) (userr1 user)) user (error "Invalid user")))
(defun userr1 (user)
  (if (name user) T nil))

;Una porta puo' essere composta di soli 0-9
(defun portr0 (port) (if (and (not (null port)) (portr1 port)) port (error "Invalid port")))
(defun portr1 (port)
  (cond ((digitp (first port)) (portr1 (cdr port)))
	((null port) T)
	(T nil)))



;Forse banalmente e ridondantemente rispetto al codice gia' scritto, verifichiamo che 
;l' host contenga solo caratteri propri d' un identificatore
(defun name (name)
  (if (and 
       (null (member #\@ name))
       (null (member #\/ name))
       (null (member #\? name))
       (null (member #\# name))
       (null (member #\: name))) T nil))

;La verifica dell' ip si articola in queste fasi:
;-la lista contenente il potenziale ip e' normalizzata cosi' da poter essere convertita
; a stringa
;-questa stringa e' spezzata secondo '.'; quest' operazione restituisce una lista di 
; stringhe delle singole parti in cui e' stata spezzata, che convertiamo ancora ad una 
; lista di di liste
;-essa non dev' essere che lunga 4, perche' 4 sono le unita' N[N][N] che devono essere 
; presenti in un ip
;-prendiamo ciascuna di queste 4 componenti e verifichiamo che sia valida, e quindi che:
; -sia di lunghezza maggiore di zero e minore di quattro (puo' essere N, NN, NNN)
; -N sia effettivamente un numero
;Come in prolog non mi preoccupo, nonstante il nome della funzione, che siano 
;effettivamente ottetti, non controllo il valore decimale di N[N][N], non e' richiesto
;dalle specifiche
(defun ip (ip)
  (let ((a (split-by-one-x (coerce (normalize ip) 'string) '#\.)))
    (let ((b (mapcar 'coerceToList a)))
      (if (not (eq 4 (length b))) 
	  nil
	  (let ((x (first b))
	        (y (second b))
	        (w (third b))
                (z (fourth b)))
	    (if (and (octet0 x) (octet0 y) (octet0 w) (octet0 z)) T nil))))))

(defun octet0 (nnn)
  (if (and (> (length nnn) 0) (< (length nnn) 4))
      (octet1 nnn)
      nil))
(defun octet1 (nnn)
  (cond ((null nnn) T)
	((digitp (first nnn)) (octet1 (cdr nnn)))
	((not (digitp (first nnn))) nil)))



;Questa funzione si preoccupa di estrapolare le parti dell' auth: 
;-se esso e' nullo, vuol dire che abbiamo riconosciuto la sequenza '://' e quindi ci aspettiamo un authority non vuota ma cosi' non e' e 
; solleviamo errore
;-se e' invece '(#\Space) vuol dire che l' abbiamo messa noi vuota, poiche' non presente, ed allora ritorniamo di conseguenza
;-altrimenti possiamo avere al massimo un' occorrenza di '@' ed una di ':' che richiamano porta ed user, se ve ne sono di piu' solleviamo 
; errore; a seconda della loro presenza ci preoccupiamo di rompere la lista dopo averla convertita a stringa per pescare le giuste parti,
; che vanno poi verificate carattere per carattere
(defun userHostPort (auth)
  (cond ((equal auth '(nil)) (error "Invalid auth"))
        ((equal auth '(#\Space)) (append (list '(#\Space)) (list '(#\Space)) (list '(#\Space))))
	((and (not (eq (count #\@ auth) 0)) (not (eq (count #\: auth) 0)))
	 (let ((a (split-by-one-x (coercetostring auth) #\@)))
	   (if (not (eq (length a) 2)) (error "Invalid auth")
	     (let ((b (split-by-one-x (second a) #\:)))
	       (if (not (eq (length b) 2)) (error "Invalid auth")
		 (append (list (userr0 (coercetolist (first a)))) (list (hostr0 (coercetolist (first b)))) (list (portr0 (coercetolist (second b))))))))))
	((and (eq (count #\@ auth) 0) (not (eq (count #\: auth) 0)))
	 (let ((a (split-by-one-x (coercetostring auth) #\:)))
	   (if (not (eq (length a) 2)) (error "Invalid auth")
	     (append (list '(#\Space)) (list (hostr0 (coercetolist (first a)))) (list (portr0 (coercetolist (second a))))))))
	((and (not (eq (count #\@ auth) 0)) (eq (count #\: auth) 0))
	 (let ((a (split-by-one-x (coercetostring auth) #\@)))
	   (if (not (eq (length a) 2)) (error "Invalid auth")
	     (append (list (userr0 (coercetolist (first a)))) (list (hostr0 (coercetolist (second a)))) (list '(#\Space))))))
	(T (append (list '(#\Space)) (list (hostr0 auth)) (list '(#\Space))))))



;"Returns a list of substrings of string
;divided by ONE space each.
;Note: Two consecutive spaces will be seen as
;if there were an empty string between them."
;Credits: The Common Lisp Cookbook, http://cl-cookbook.sourceforge.net ,
;mod. by me
(defun split-by-one-x (string char)
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))


(defun coerceToList (string) (coerce string 'list))
(defun coerceToString (list) (coerce list 'string))


(defun digitp (numb)
  (or (eq numb #\0) 
      (eq numb #\1) 
      (eq numb #\2) 
      (eq numb #\3) 
      (eq numb #\4) 
      (eq numb #\5) 
      (eq numb #\6)
      (eq numb #\7)
      (eq numb #\8)
      (eq numb #\9)))

  

;funzione per ricavare il "cuore" d' una lista multipla, es: '(((lol))) -> '(lol)	
(defun normalize (list) (if (atom (first list)) list (normalize (first list))))
			 
			 	  
	
