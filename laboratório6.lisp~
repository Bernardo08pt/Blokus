;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2017 / 2018
;;;; Ficha de Laboratório nº6 - Apoio ao 1º projeto
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


