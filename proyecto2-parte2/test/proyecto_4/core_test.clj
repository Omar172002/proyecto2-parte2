  (require '[clojure.java.io :as io])

;funcion para leer un archivo
(defn leer-archivo [nombre]
  (with-open [archivo (io/reader nombre)]
    (read-string (slurp archivo))))

;Automata encargado de analizar el lexico
(def estado-inicial 'q0)
(def estado-aceptacion 'q1)
(def lexico '(s b i d))

(defn todos-miembros [conjunto lista]  ; verifica que todos los simbolos de la lista correspondan con el lexico
  (cond
    (empty? lista) true
    (some #(= % (first lista)) conjunto)
    (todos-miembros conjunto (rest lista))
    :else false))

(defn transicion [estado simbolos] ; si todos los simbolos corresponden a el lexico transiciona el estado
  (if (todos-miembros lexico simbolos)
    estado-aceptacion
    estado))

;funcion para contar el numero de filas de una matriz
(defn contar-filas [matriz]
  (if (empty? matriz)
    0
    (+ 1 (contar-filas (rest matriz)))))


;funcion para contar el numero de columnas de una matriz
(defn contar-columnas [matriz]
  (if (empty? matriz)
    0
    (count (first matriz))))


;funcion para contar el numero de columnas de una matriz
(defn obtener-medida [matriz]
  (str "La medida de tu carrusel es de " (contar-filas matriz) " X " (contar-columnas matriz)))

;Retorna el nombre del archivo del alamcen N
(defn archivoAlmacen [n]
  (str "almacen" n ".txt"))

;Retorna el nombre del archivo de la ventanilla N
(defn archivoVentanilla [n]
  (str "ventanilla" n ".txt"))

;Retorna el nombre del archivo de las funciones N
(defn archivoFunciones [n]
  (str "funciones" n ".txt"))




; funcion para leer el valor de los archivos de los almacenes
(defn almacen [n]
  (leer-archivo (str "almacen" n ".txt")))


; funcion para leer el valor de los archivos de las ventanilla
(defn ventanilla [n]
  (leer-archivo (str "ventanilla" n ".txt")))

; funcion para leer el valor de los archivos de las funciones 
(defn cargar-funciones [n]
  (map eval (leer-archivo (str "funciones" n ".txt"))))

;funcion para guardar el resultado en un archivo
(defn guardar-en-archivo [resultado archivo]
  (with-open [output-port (clojure.java.io/writer archivo :append false)]
    (binding [*out* output-port]
      (print resultado))))

;FUNCION PARA ENCOTRAR LOS PORDUCTOS QUE TIENEN UN INVENTARIO MENOR A N
(defn inventario-menor [matriz n]
  (filter (fn [fila] (not (empty? fila))) (map (fn [fila] (filter (fn [producto] (< (second producto) n)) fila)) matriz)))


;FUNCION PARA OBTENER EL TOTAL DEL INVEARIO DE TODO EL ALMACEN

(defn elemento-total [elemento] 
  (* (first (rest elemento)) (first (rest (rest elemento)))))

(defn sumar-lista [lista]
  (if (empty? lista)
    0
    (+ (elemento-total (first lista)) (sumar-lista (rest lista)))))

(defn total-almacen [matriz]
  (if (empty? matriz)
    0
    (+ (sumar-lista (first matriz)) (total-almacen (rest matriz)))))



;funcion para subir, mueve la primera fila al final de la lista
(defn subir [matriz] 
  (if (empty? matriz)
    '()
    (concat (rest matriz) [(first matriz)])))

;funcion para subir el carruserl y guardar
(defn subir-y-guardar [archivo]
  (let [matriz-leida (leer-archivo archivo)]
    (println "subir")
    (guardar-en-archivo (subir matriz-leida) archivo)))


; ejecuta la funcion subir-y-guardar n veces
(defn subir-y-guardar-n-veces [n archivo]
  (if (zero? n)
    nil
    (do
      (subir-y-guardar archivo)
      (subir-y-guardar-n-veces (- n 1) archivo))))



;fucnion para bajar, mueve la ultima fila al principio de la lista
(defn bajar [matriz]
  (if (empty? matriz)
    matriz
    (conj (butlast matriz) (last matriz))))


;funcion para bajar y guardar el resultado
(defn bajar-y-guardar [archivo]
  (let [matriz-leida (leer-archivo archivo)]
    (println "bajar")
    (guardar-en-archivo (bajar matriz-leida) archivo)))

; ejecuta la funcion bajar-y-guardar n veces
(defn bajar-y-guardar-n-veces [n archivo]
  (if (zero? n)
    nil
    (do
      (bajar-y-guardar archivo)
      (bajar-y-guardar-n-veces (dec n) archivo))))


;derecha, esta funcion suma 1 a la posicion de la columna de la ventanilla (no puede ser mayor al numero de columnas)
(defn derecha [ventanilla almacen archivo]
  (if (>= (second ventanilla) (- (contar-columnas almacen) 1))
    (do
      (println "Movimiento a la derecha inválido.")
      (list (first ventanilla) (second ventanilla))) 
    (do
      (guardar-en-archivo (list (first ventanilla) (+ 1 (second ventanilla))) archivo)
      (println "Movimiento a la derecha válido. Nueva posición:" (list (first ventanilla) (+ 1 (second ventanilla))))
      (list (first ventanilla) (+ 1 (second ventanilla))))))


;izquierda, esta fucnion resta 1 a la posicion de la columna de la ventanilla (no puede ser menor a 0)
(defn izquierda [ventanilla archivo]
  (if (<= (second ventanilla) 0)
    (do
      (println "Movimiento a la izquierda inválido.")
      (list (first ventanilla) (second ventanilla)))
    (do
      (guardar-en-archivo (list (first ventanilla) (- (second ventanilla) 1)) archivo)
      (println "Movimiento a la izquierda válido. Nueva posición:" (list (first ventanilla) (- (second ventanilla) 1)))
      (list (first ventanilla) (- (second ventanilla) 1)))))

;funcion para saber saber si un elemento existe en una lista, devuelve TRUE si el producto se encuentra en la lista
(defn elemento-en-lista? [elemento lista]
  (some #(= elemento (first %)) lista))



 ;funcion para saber cuantas veces se necesita usar la fucnion bajar para encontrar el producto
(defn buscar-con-bajar [matriz elemento]
  (cond
    (empty? matriz) 0
    (elemento-en-lista? elemento (first matriz)) 0
    :else (+ 1 (buscar-con-bajar (bajar matriz) elemento))))

 ;funcion para saber cuantas veces se necesita usar la funcion subir para encontrar el producto 
(defn buscar-con-subir [matriz elemento]
  (cond
    (empty? matriz) 0
    (elemento-en-lista? elemento (first matriz)) 0
    :else (+ 1 (buscar-con-subir (subir matriz) elemento))))


;FUNCION PARA SABER EL CAMINO MAS CORTO ENTRE SUBIR Y BAJAR 
(defn camino-corto [matriz nuevo-elemento archivo]
  (cond
    (empty? matriz) 0
    (< (buscar-con-bajar matriz nuevo-elemento)
       (buscar-con-subir matriz nuevo-elemento))
    (bajar-y-guardar-n-veces (buscar-con-bajar matriz nuevo-elemento)  archivo)
    :else
    (subir-y-guardar-n-veces (buscar-con-subir matriz nuevo-elemento)  archivo)))


; Función para devolver un elemento en una columna de una lista
(defn regresar-columna [lista n]
  (cond
    (= n 0) (first lista)
    :else (regresar-columna (rest lista) (- n 1))))


;; Función que devuelve la fila del índice dado
(defn obtener-fila [matriz indice]
  (if (or (< indice 0) (empty? matriz))
    '()
    (if (= indice 0)
      (first matriz)
      (obtener-fila (rest matriz) (- indice 1)))))

; Función para buscar un elemento en una matriz, introduciendo la fila y la columna
(defn buscar-elemento [matriz fila columna]
  (regresar-columna (obtener-fila matriz fila) columna))


;; Función para encontrar la columna y la fila de un producto, utilizando obtener-fila
(defn encontrar-posicion [columna elemento]
  (defn encontrar-posicion-aux [lista posicion]
    (cond
      (empty? lista) -1
      (= (first (first lista)) elemento) posicion
      :else (encontrar-posicion-aux (rest lista) (inc posicion))))
  (encontrar-posicion-aux columna 0))

; funcion para encontrar la columna y la fila de un producto, para encotrar la fila utilice la funcion buscar-con-bajar definida anteriormente
(defn posicion [matriz producto]
  (list (buscar-con-subir matriz producto) (encontrar-posicion  (obtener-fila matriz (buscar-con-subir matriz producto)) producto)))

(defn imprime-izquierda [n] ; imprime la palabra "izquierda" N veces
  (cond
    (= n 0) (println)
    :else
    (do
      (println "izquierda")
      (imprime-izquierda (dec n)))))

(defn imprime-derecha [n] ; imprime la palabra "derecha" N veces
  (cond
    (= n 0) (println)
    :else
    (do
      (println "derecha")
      (imprime-derecha (dec n)))))


;FUNCION PARA SABER LOS MOVIMIENTOS NECESARIOS EN X "DERECHA" O "IZQUIERDA"
(defn posicion-x [producto almacen ventanilla]
  (let [posicion-producto (first (rest (posicion almacen producto)))
        posicion-ventanilla (first (rest ventanilla))]
    (cond
      (< posicion-producto posicion-ventanilla) (imprime-izquierda (- posicion-ventanilla posicion-producto))
      (> posicion-producto posicion-ventanilla) (imprime-derecha (- posicion-producto posicion-ventanilla))
      (= posicion-producto posicion-ventanilla) (print ""))))



; funcion que me retorna la fila con el producto actualizado
(defn actualizar-producto [fila producto cantidad operacion] 
  (cond
    (empty? fila) '()
    (= (first (first fila)) producto)
    (if (= operacion 'agregar)
      (cons (list producto (+ cantidad (second (first fila))) (nth (first fila) 2)) ; si la funcion llamada es agregar suma la cantidad al inventario del producto dado
            (actualizar-producto (rest fila) producto cantidad operacion))
      (cons (list producto (max 0 (- (second (first fila)) cantidad)) (nth (first fila) 2)) ; si la funcion llamada es retirar resta la cantidad al inventario del producto dado
            (actualizar-producto (rest fila) producto cantidad operacion)))
    :else (cons (first fila) (actualizar-producto (rest fila) producto cantidad operacion))))

;funcion para rearmar la estructura de datos con el inventario actualizado
(defn actualizar-almacen [almacen producto cantidad operacion]
  (cond
    (empty? almacen) '()
    :else (cons (actualizar-producto (first almacen) producto cantidad operacion)
                (actualizar-almacen (rest almacen) producto cantidad operacion))))

;funcion para agregar inventario a un producto
(defn agregar [producto cantidad archivo almacen]
  (guardar-en-archivo (actualizar-almacen almacen producto cantidad 'agregar) archivo))

;fucnion para retirar inventario de un producto
(defn retirar [producto cantidad archivo almacen]
  (guardar-en-archivo (actualizar-almacen almacen producto cantidad 'retirar) archivo))



;Agregar con seis argumentos busca el producto dado dentro de la estructura de datos, lo actualiza, actualiza la ventanilla e imprime el camino mas corto para llegar a el
(defn Agregar-seis-argumentos [producto cantidad archivo almacen ventanilla archivoVentanilla]
  (do
    (println)
    (agregar producto cantidad archivo almacen)
    (println)
    (println "Se agregaron" cantidad producto)
    (camino-corto almacen producto archivo)
    (println)
    (posicion-x producto almacen ventanilla)
    (guardar-en-archivo (list 0 (second (posicion almacen producto))) archivoVentanilla)))


;Agregar con cinco argumentos actualiza el inventario de el producto que se encuentra en la ventanilla
(defn Agregar-cinco-argumento [cantidad archivo almacen ventanilla archivoVentanilla]
  (println)
  (agregar  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))) cantidad archivo almacen)
  (println)
  (println "Se agregaron" cantidad  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))))

  (guardar-en-archivo (list 0 (second (posicion almacen  (first (buscar-elemento almacen (first ventanilla) (second ventanilla)))))) archivoVentanilla))



;retiro-maximo verifica que el numero de productos a retirar no sea mayor a el numero que se encuentran en el inventario, en el caso de ser asi retira todo el inventario posible
(defn retiro-maximo [cantidad producto almacen]
  (let [elemento (buscar-elemento almacen (first (posicion almacen producto))
                                  (first (rest (posicion almacen producto))))]
    (if (< (second elemento) cantidad)
      (do
        (println "El número a retirar es mayor que el número de productos, se retirarán todos los productos posibles")
        (print "Se retiraron: ")
        (print (second elemento))
        (println ""))
      (do
        (print "Se retiraron ")
        (print cantidad)
        (print " ")
        (print producto)
        (println "")))))

;Retirar con seis argumentos busca el producto dado dentro de la estructura de datos, lo actualiza, actualiza la ventanilla e imprime el camino mas corto para llegar a el
(defn Retirar-seis-argumentos [producto cantidad archivo almacen ventanilla archivoVentanilla]
  (do
    (posicion-x producto almacen ventanilla)
    (camino-corto almacen producto archivo)
    (retirar producto cantidad archivo almacen)
    (retiro-maximo cantidad producto almacen)

    (guardar-en-archivo (list 0 (second (posicion almacen producto))) archivoVentanilla)))

;Retirar con cinco argumentos resta la cantidad a el inventario del producto que se encuentra en la ventanilla
(defn Retirar-cinco-argumento [cantidad archivo almacen ventanilla archivoVentanilla]
  (println)
  (posicion-x  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))) almacen ventanilla)
  (camino-corto almacen  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))) archivo)
  (retirar  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))) cantidad archivo almacen)
  (retiro-maximo cantidad  (first (buscar-elemento almacen (first ventanilla) (second ventanilla))) almacen)
  (guardar-en-archivo (list 0 (second (posicion almacen  (first (buscar-elemento almacen (first ventanilla) (second ventanilla)))))) archivoVentanilla))


;FUNCION PARA AGREGAR INVENTARIO
(defn Agregar [& args]
  (cond
    (= (count args) 6) (Agregar-seis-argumentos (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5))
    (= (count args) 5) (Agregar-cinco-argumento (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4))))

;FUNCION PARA RETIRAR INVENTARIO
(defn Retirar [& args] 
  (cond
    (= (count args) 6) (Retirar-seis-argumentos (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5))
    (= (count args) 5) (Retirar-cinco-argumento (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4))))


; la fucnion mover analiza la lista aceptada por el automata y realiza la accion del movimiento (subir, bajar, izquierda, derecha)
(defn mover [lista almacen ventanilla archivoVentanilla archivoProductos]
  (let [ventanilla-leida (leer-archivo archivoVentanilla)]
    (cond
      (empty? lista) (println "\n movimiento del carrusel completado con éxito.")
      (= (first lista) 'b) (do (bajar-y-guardar archivoProductos) (mover (rest lista) almacen ventanilla archivoVentanilla archivoProductos))
      (= (first lista) 's) (do (subir-y-guardar archivoProductos) (mover (rest lista) almacen ventanilla archivoVentanilla archivoProductos))
      (= (first lista) 'i) (do (izquierda ventanilla-leida archivoVentanilla) (mover (rest lista) almacen ventanilla-leida archivoVentanilla archivoProductos))
      (= (first lista) 'd) (do (derecha ventanilla-leida almacen archivoVentanilla) (mover (rest lista) almacen ventanilla-leida archivoVentanilla archivoProductos)))))

;FUNCION ENCARGADA DE ANALIZAR EL LEXICO Y EJECUTAR LA FUNCION DE EL ESTADO ACEPTOR EN EL CASO DE SER CORRECTO
(defn Mover [lista almacen ventanilla archivoVentanilla archivoProductos]
  (if (= (transicion estado-inicial lista) estado-aceptacion)
    (mover lista almacen ventanilla archivoVentanilla archivoProductos)
    "La cadena no es aceptada por el autómata."))

;funcion para ordenar una lista de mayor a menor 
(defn quick-sort [lista]
  (cond
    (empty? lista) '()
    :else
    (concat (quick-sort (filter #(>= %  (first lista)) (rest lista))) [(first lista)]
            (quick-sort (filter #(< %  (first lista)) (rest lista))))))

;Esta funcion retorna el 10% de los numeros mas alto de una lista
(defn obtener-primeros-n [lista]
  (take (Math/ceil (* 0.1 (count lista))) (quick-sort lista)))

;Funcion para obtener la posicion del 10% mas alto de una lista (INDICE)
(defn encontrar-posiciones [elemento lista]
  (cond
    (empty? lista) nil
    (= elemento (first lista)) 1
    :else (+ 1 (encontrar-posiciones elemento (rest lista)))))

;Retorna una lista con el 10% mas grande de una lista ordenado de mayor a menor
(defn list-10% [lista]
  (map #(encontrar-posiciones % lista) (obtener-primeros-n lista)))


;Funcion para imprimir el 10% de los almacenes con mas valor con el nombre del almacen correspondiente 
(defn imprimir-10% [n]
  (println (str (archivoAlmacen n) " " (total-almacen (almacen n)) (newline))))

(defn mayor-valor [lista]
  (println "10% de carruseles con mayor producto")
  (map imprimir-10% (list-10% (apply list (map #(total-almacen (almacen %)) lista)))))

;Imprime el total de un almacen junto con su nombre
(defn almacen-total-actualizado [n]
  (println (str (archivoAlmacen n) " " (total-almacen (almacen n)) (newline))))

;Imprime el total de todos los almacenes junto a su nombre
(defn imprime-almacenes-total [lista]
  (println "TOTAL DE LOS ALMACENES ACTUALIZADO")
  (map almacen-total-actualizado lista))

; define la amtriz que se guardara en todos los archivos de almacen con valores tando entre 0 y 500 para el precio y la cantidad
(defn almacen-generado []
  (list
   (list (list 'K (rand-int 500) (rand-int 500)) (list 'L (rand-int 500) (rand-int 500)) (list 'M (rand-int 500) (rand-int 500)) (list 'N (rand-int 500) (rand-int 500)) (list 'O (rand-int 500) (rand-int 500)))
   (list (list 'P (rand-int 500) (rand-int 500)) (list 'Q (rand-int 500) (rand-int 500)) (list 'R (rand-int 500) (rand-int 500)) (list 'S (rand-int 500) (rand-int 500)) (list 'T (rand-int 500) (rand-int 500)))
   (list (list 'U (rand-int 500) (rand-int 500)) (list 'V (rand-int 500) (rand-int 500)) (list 'W (rand-int 500) (rand-int 500)) (list 'X (rand-int 500) (rand-int 500)) (list 'Y (rand-int 500) (rand-int 500)))
   (list (list 'Z (rand-int 500) (rand-int 500)) (list 'A1 (rand-int 500) (rand-int 500)) (list 'B1 (rand-int 500) (rand-int 500)) (list 'C1 (rand-int 500) (rand-int 500)) (list 'D1 (rand-int 500) (rand-int 500)))
   (list (list 'E1 (rand-int 500) (rand-int 500)) (list 'F1 (rand-int 500) (rand-int 500)) (list 'G1 (rand-int 500) (rand-int 500)) (list 'H1 (rand-int 500) (rand-int 500)) (list 'I1 (rand-int 500) (rand-int 500)))
   (list (list 'J1 (rand-int 500) (rand-int 500)) (list 'K1 (rand-int 500) (rand-int 500)) (list 'L1 (rand-int 500) (rand-int 500)) (list 'M1 (rand-int 500) (rand-int 500)) (list 'N1 (rand-int 500) (rand-int 500)))
   (list (list 'O1 (rand-int 500) (rand-int 500)) (list 'P1 (rand-int 500) (rand-int 500)) (list 'Q1 (rand-int 500) (rand-int 500)) (list 'R1 (rand-int 500) (rand-int 500)) (list 'S1 (rand-int 500) (rand-int 500)))
   (list (list 'T1 (rand-int 500) (rand-int 500)) (list 'U1 (rand-int 500) (rand-int 500)) (list 'V1 (rand-int 500) (rand-int 500)) (list 'W1 (rand-int 500) (rand-int 500)) (list 'X1 (rand-int 500) (rand-int 500)))
   (list (list 'Y1 (rand-int 500) (rand-int 500)) (list 'Z1 (rand-int 500) (rand-int 500)) (list 'A2 (rand-int 500) (rand-int 500)) (list 'B2 (rand-int 500) (rand-int 500)) (list 'C2 (rand-int 500) (rand-int 500)))
   (list (list 'D2 (rand-int 500) (rand-int 500)) (list 'E2 (rand-int 500) (rand-int 500)) (list 'F2 (rand-int 500) (rand-int 500)) (list 'G2 (rand-int 500) (rand-int 500)) (list 'H2 (rand-int 500) (rand-int 500)))
   (list (list 'I2 (rand-int 500) (rand-int 500)) (list 'J2 (rand-int 500) (rand-int 500)) (list 'K2 (rand-int 500) (rand-int 500)) (list 'L2 (rand-int 500) (rand-int 500)) (list 'M2 (rand-int 500) (rand-int 500)))
   (list (list 'N2 (rand-int 500) (rand-int 500)) (list 'O2 (rand-int 500) (rand-int 500)) (list 'P2 (rand-int 500) (rand-int 500)) (list 'Q2 (rand-int 500) (rand-int 500)) (list 'R2 (rand-int 500) (rand-int 500)))
   (list (list 'S2 (rand-int 500) (rand-int 500)) (list 'T2 (rand-int 500) (rand-int 500)) (list 'U2 (rand-int 500) (rand-int 500)) (list 'V2 (rand-int 500) (rand-int 500)) (list 'W2 (rand-int 500) (rand-int 500)))
   (list (list 'X2 (rand-int 500) (rand-int 500)) (list 'Y2 (rand-int 500) (rand-int 500)) (list 'Z2 (rand-int 500) (rand-int 500)) (list 'A3 (rand-int 500) (rand-int 500)) (list 'B3 (rand-int 500) (rand-int 500)))
   (list (list 'C3 (rand-int 500) (rand-int 500)) (list 'D3 (rand-int 500) (rand-int 500)) (list 'E3 (rand-int 500) (rand-int 500)) (list 'F3 (rand-int 500) (rand-int 500)) (list 'G3 (rand-int 500) (rand-int 500)))
   (list (list 'H3 (rand-int 500) (rand-int 500)) (list 'I3 (rand-int 500) (rand-int 500)) (list 'J3 (rand-int 500) (rand-int 500)) (list 'K3 (rand-int 500) (rand-int 500)) (list 'L3 (rand-int 500) (rand-int 500)))
   (list (list 'M3 (rand-int 500) (rand-int 500)) (list 'N3 (rand-int 500) (rand-int 500)) (list 'O3 (rand-int 500) (rand-int 500)) (list 'P3 (rand-int 500) (rand-int 500)) (list 'Q3 (rand-int 500) (rand-int 500)))
   (list (list 'R3 (rand-int 500) (rand-int 500)) (list 'S3 (rand-int 500) (rand-int 500)) (list 'T3 (rand-int 500) (rand-int 500)) (list 'U3 (rand-int 500) (rand-int 500)) (list 'V3 (rand-int 500) (rand-int 500)))
   (list (list 'A (rand-int 500) (rand-int 500)) (list 'B (rand-int 500) (rand-int 500)) (list 'C (rand-int 500) (rand-int 500)) (list 'D (rand-int 500) (rand-int 500)) (list 'E (rand-int 500) (rand-int 500)))
   (list (list 'F (rand-int 500) (rand-int 500)) (list 'G (rand-int 500) (rand-int 500)) (list 'H (rand-int 500) (rand-int 500)) (list 'I (rand-int 500) (rand-int 500)) (list 'J (rand-int 500) (rand-int 500)))))

;fucnion para imprimir los productos de un almacen con un inventario menor a 10
(defn almacenes-a-rellenar [n]
  (do
    (print (newline))
    (print (archivoAlmacen n) (inventario-menor (almacen n) 10))))


; Funcion auxiliar para filtrar elementos vacios
(defn es-vacio? [elemento]
  (empty? elemento))


;Nombres de los productos que pueden tomar las fucniones de agregar y retirar
(def letras '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                A1 B1 C1 D1 E1 F1 G1 H1 I1 J1 K1 L1 M1 N1 O1 P1 Q1 R1 S1 T1 U1 V1 W1 X1 Y1 Z1
                A2 B2 C2 D2 E2 F2 G2 H2 I2 J2 K2 L2 M2 N2 O2 P2 Q2 R2 S2 T2 U2 V2 W2 X2 Y2 Z2
                A3 B3 C3 D3 E3 F3 G3 H3 I3 J3 K3 L3 M3 N3 O3 P3 Q3 R3 S3 T3 U3 V3 W3 X3 Y3 Z3))


;Funcion para generar una intruccion de Movimiento aleatoriamente
(def movimiento-rand '(s b i d))

(defn repetir-letra-aleatoria [n]
  (vec (repeatedly n #(rand-nth movimiento-rand))))


; definicion de las intrucciones que see guardaran en el archivo funciones con valores aleatorios en sus parametros
(def funcion '((ArchivoAlmacen)
         (newline)
         (Retirar (rand-nth letras) (+ 1 (rand-int 100)) (ArchivoAlmacen) (Almacen) (Ventanilla) (ArchivoVentanilla))
         (Agregar (rand-nth letras) (+ 1 (rand-int 100)) (ArchivoAlmacen) (Almacen) (Ventanilla) (ArchivoVentanilla))
               (Mover (repetir-letra-aleatoria (rand-int 10)) (Almacen) (Ventanilla) (ArchivoVentanilla) (ArchivoAlmacen)) 
               (Retirar (+ 1 (rand-int 100)) (ArchivoAlmacen) (Almacen) (Ventanilla) (ArchivoVentanilla))
               (Agregar (+ 1 (rand-int 100)) (ArchivoAlmacen) (Almacen) (Ventanilla) (ArchivoVentanilla))
         ))




;fucnion que lee y ejecuta las fucniones que se encuentren en un archivo .txt
(defn cargar-funciones [n]
  (map eval (leer-archivo (str "funciones" n ".txt"))))

;Funcion prinipal para ejecutar las fucniones con su alamcen y ventanilla corespondiente
(defn main [n]
  (defn Almacen []
    (almacen n))

  (defn Ventanilla []
    (ventanilla n))

  (defn ArchivoAlmacen []
    (archivoAlmacen n))

  (defn ArchivoVentanilla []
    (archivoVentanilla n))

  (cargar-funciones n))


;NUMERO DE ALAMCENES 
(def numeros-almacenes (range 1 11))


;fucnion para obtener el total de todas los almacenes en paralelo
(time (apply + (pmap #(total-almacen (almacen %)) numeros-almacenes)))
      
;funcion para ejecutar todos los alamcenes
;(pmap #(map main %) (partition-all 3 (range 1 30))) 



;funcion para imprimir el 10% mas grande, los productos de los alamcenenes que necesitan resurtirce y el precio todal del inventario
(defn ejecutar-y-filtrar [numeros-almacenes] 
  (filter #(not (es-vacio? %)) (doall (imprime-almacenes-total numeros-almacenes)))
  (filter #(not (es-vacio? %)) (doall (map almacenes-a-rellenar numeros-almacenes)))
  (newline)
  (filter #(not (es-vacio? %)) (doall (mayor-valor numeros-almacenes))))


;FUNCIONES PARA GENERAR LAS PRUEBAS

;funcion para generar N cantidad de alamcenenes con valores aleatorios para el precio y la cantidad
(defn almacen-generador [n]
  (guardar-en-archivo (almacen-generado) (archivoAlmacen n)))

(doall (map almacen-generador numeros-almacenes)) ; ejecutar almacen-generador

 ;funcion para generar N cantidad de archivos con funciones con valores aleatorios en sus parametros
(defn generador-funciones [n]
  (guardar-en-archivo funcion (archivoFunciones n)))

(doall (map generador-funciones numeros-almacenes)) ; ejecutar generador-funciones


;Funcion para generar N cantidad de ventanilla con valor de (0 0)
(defn ventanilla-generador [n]
  (guardar-en-archivo (list 0 0) (archivoVentanilla n)))

(doall (map ventanilla-generador numeros-almacenes)) ;ejecutar ventanilla-generador

;funcion que ejecuta las intruciones de todos los archivos con su alamcen correspondiente eh imprime los resultados
(defn programa [] 
    (doseq [resultado (doall (pmap #(map main %) (partition-all 5 numeros-almacenes)))]
      (println resultado))
  (println "Esperando 5 segundos...")
  (Thread/sleep 5000) ; Simula una espera adicional de 5 segundos
  (ejecutar-y-filtrar numeros-almacenes)); Ejecuta después de esperar 5 segundos

(defn programa2 []
(println (doall (map main numeros-almacenes)))
  (println "Esperando 5 segundos...")
  (Thread/sleep 5000) ; Simula una espera adicional de 5 segundos
  (ejecutar-y-filtrar numeros-almacenes)); Ejecuta después de esperar 5 segundos


 ;FUNCION PRINIPAL PARA EJECUTAR TODO MI CODIGO 
"Elapsed time: 5316.764235 msecs"
"Elapsed time: 5288.915195 msecs"

"Elapsed time: 5483.620518 msecs"
"Elapsed time: 5524.040633 msecs"

"Elapsed time: 5993.026686 msecs"

"Elapsed time: 6499.412065 msecs"
"Elapsed time: 6435.827305 msecs"
(time (programa))          

"Elapsed time: 5257.092968 msecs"
"Elapsed time: 5270.868228 msecs"

"Elapsed time: 5512.41238 msecs"
"Elapsed time: 5552.840777 msecs"

"Elapsed time: 5932.919972 msecs"
"Elapsed time: 5877.760162 msecs"

"Elapsed time: 6512.142218 msecs"
(time (programa2))      

;"Elapsed time: 0.20841 msecs"
(time (map main '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))


"Elapsed time: 0.349375 msecs"

  (doall (pmap #(map main %) (partition-all 4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))



(almacen 99)
(main 99)

(inventario-menor (almacen 1) 10)

(total-almacen (almacen 1))
(total-almacen (almacen 2))
(total-almacen (almacen 3))
(total-almacen (almacen 4))

(almacen 1)
(almacen 2)
(almacen 3)
(almacen 4)



(ventanilla 1)

(ventanilla 2)
(ventanilla 3)
(ventanilla 4)
 
;5 alamcenes
"Elapsed time: 1.031412 msecs"
"Elapsed time: 0.471996 msecs"

;10 almacenes
"Elapsed time: 0.603232 msecs"
"Elapsed time: 0.481639 msecs"

;30 alamcenes
"Elapsed time: 0.943946 msecs"
"Elapsed time: 0.633508 msecs"


  
(time (doall (pmap #(map main %) (partition-all 5 numeros-almacenes))))    

;5 alamcenes
"Elapsed time: 4.10729 msecs"
"Elapsed time: 1.570289 msecs"

;10 almacenes
"Elapsed time: 2.168724 msecs"
"Elapsed time: 1.481639 msecs"

;30 alamcenes
"Elapsed time: 5.943009 msecs"
"Elapsed time: 5.419132 msecs"
(time (doall (map main numeros-almacenes)))   