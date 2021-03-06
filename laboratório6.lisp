;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2017 / 2018
;;;; Ficha de Laborat�rio n�6 - Apoio ao 1� projeto
;;;; Autor: 


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (dimensao 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
	(make-list dimensao :initial-element (make-list dimensao :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 14x14 com 4 quadrados 1x1, 1 quadrado 2x2 e 1 cruz"
	'(
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(1 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(2 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(3 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(4 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(5 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(6 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(7 0 0 0 0 0 0 0 0 0 1 0 0 0)
	(8 0 0 0 0 0 0 0 0 1 1 1 0 0)
	(9 0 0 0 0 0 0 0 0 0 1 0 0 0)
	(10 0 0 0 0 0 0 0 1 1 0 0 0 0)
	(11 0 0 0 0 0 0 0 1 1 0 1 0 0)
	(12 0 0 0 0 0 0 0 0 0 1 0 1 0)
	(13 0 0 0 0 0 0 0 0 0 0 0 0 1)
	)
)


;;; Exercicios
(defun linha (indice tabuleiro)
  (cond
    ((= indice 0) (car tabuleiro))
    (t (linha (- indice 1) (cdr tabuleiro)))
  )
)

(defun coluna (indice tabuleiro)
  (mapcar #'(lambda (linha) (nth indice linha)) tabuleiro) 
)

(defun celula (indiceLinha indiceColuna tabuleiro)
  (linha indiceLinha (coluna indiceColuna tabuleiro))
)

(defun casa-vaziap (indiceLinha indiceColuna tabuleiro)
  (= (celula indiceLinha indiceColuna tabuleiro) 0)
)

(defun verifica-casas-vazias (tabuleiro listaCelulas)
  (mapcar #'(lambda (celula) (casa-vaziap (car celula) (car (cdr celula)) tabuleiro)) listaCelulas)  
)

;;; Fun��o que recebe um �ndice, uma lista e um valor (por default o valor � 1) e substitui pelo valor pretendido nessa posi��o
(defun substituir-posicao (indice lista &optional (valor 1))
  (cond
     ((= indice 0) (cons valor (cdr lista)))
     (t (cons (car lista) (substituir-posicao (- indice 1) (cdr lista) valor)))
   )
)

;;; Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � 1). A fun��o dever� retornar o tabuleiro com a c�lula substitu�da pelo valor pretendido.
(defun substituir (indiceLinha indiceColuna tabuleiro &optional (valor 1))
  (cond
     ((= indiceLinha 0) (cons (substituir-posicao indiceColuna (car tabuleiro) valor) (cdr tabuleiro)))
     (t (cons (car tabuleiro) (substituir (- indiceLinha 1) indiceColuna (cdr tabuleiro) valor)))
   )
)

;;; Fun��o que recebe dois �ndices e um tipo de pe�a (quadrado-1x1, quadrado-2x2 ou cruz) e retorna uma lista
; com os pares de �ndices correspondentes �s posi��es que ir� ser colocada a pe�a. Para todas as pe�as vamos assumir que os �ndices
; passados como argumento para a fun��o ser�o referentes � casa do canto superior esquerdo. No caso da cruz assumimos uma
; mancha de um quadrado 3x3 cujo ponto de refer�ncia ser� o canto superior esquerdo apesar de ser uma casa que n�o ficar�
; preenchida, mas que mant�m uniforme o ponto de refer�ncia para todas as pe�as
(defun peca-casas-ocupadas (indiceLinha indiceColuna tipoPeca)
  (cond
     ((eql tipoPeca 'quadrado-1x1) (cons (list indiceLinha indiceColuna) '()))
     ;;((eql tipoPeca 'quadrado-2x2) (cons (cons (cons () (list (+ indiceLinha 1) )) (list (+ indiceLinha 1) (+ indiceColuna 1)) '()))

     ((eql tipoPeca 'quadrado-2x2) (append (peca-casas-ocupadas indiceLinha indiceColuna 'quadrado-1x1)
                                           (peca-casas-ocupadas indiceLinha (+ indiceColuna 1) 'quadrado-1x1)
                                           (peca-casas-ocupadas (+ indiceLinha 1) indiceColuna 'quadrado-1x1)
                                           (peca-casas-ocupadas indiceLinha (+ indiceColuna 1) 'quadrado-1x1)))
  )
)

