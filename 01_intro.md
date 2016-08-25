## ¿Qué es un programa?

Se suele enseñar que un programa es *una secuencia de instrucciones que opera sobre datos*, siendo esta una descripción operacional del asunto. Los programas están por encima de esto: son descripciones ejecutables de soluciones a problemas. Son una descripción de **qué** hacer para resolver el problema; esto es un enfoque **denotacional**, ya que no lidia con **cómo** efectuar las diferentes operaciones que resuelven el problema, sino que se limita a decir **qué** es lo que hay que hacer. Esto puede parecer un enfoque incompleto, y lo es. La programación implica tanto la parte denotacional (describir la solución al problema) como la parte operacional (poder ejecutar esa solución al problema, en nuestro caso en una computadora). La primera nos permite a las personas entender los programas, la segunda permite que las computadoras entiendan los programas. El problema viene cuando las personas quedamos reducidas a pensar a los programas desde la perspectiva de las máquinas: en vez de valernos de todas las posibilidades que nos brinda el mundo abstracto de la mente, nos limitamos a las ofrecidas por el mundo concreto de las máquinas.

* **Descripción**: involucra a personas, que son quienes expresan, piensan y transmiten las ideas reflejadas por una descripción.
	* Personas
	* Ideas ?
	* Abstracto
	* Alto nivel
	* **Denotacional**
* **Ejecutable**: involucra a máquinas, que son las que ejecutan los programas.
	* Máquinas
	* Concreto
	* Bajo nivel
	* **Operacional**

## Valores vs. Expresiones

Los valores son, en esencia, **ideas**. Cuando hablamos del **valor** *cuatro*, estamos haciendo referencia a una idea. Cuando hablamos del **número** *cuatro*, estamos haciendo referencia a un símbolo que representa al **valor** *cuatro*. Así, las expresiones son construcciones (en nuestro caso, de texto) que hacen referencia a valores. Profundizando un poco, las expresiones existen en el mundo concreto ya que les damos *forma* mediante símbolos, mientras que los valores existen en el mundo abstracto, ya que son ideas que residen en las mentes de las personas.

![expresiones y valores](./imagenes/expresiones-valores.jpg)

Así como hay expresiones que refieren a valores pertenecientes al conjunto de los números, en los programas también hay expresiones que refieren a valores en el conjunto de las **funciones**. `doble` es una expresión que refiere a una función. Más precisamente, a la función `doble x = x + x`.

En la programación funcional, la forma de definir funciones es mediante **ecuaciones orientadas**, en las que el lado izquierdo es algo **desconocido** y el lado derecho es algo **conocido**. En la definición de `doble`, tenemos que `doble x` es algo desconocido, que es precisamente lo que estamos a punto de definir, y tenemos `x + x`, que es algo conocido ya que `x` es un parámetro y `+` es una función conocida. **Tomar con pinzas las siguientes dos oraciones; son un entendimiento personal y precoz del asunto.** Luego de definir la función, lo que estamos diciendo es que en donde aparezca `doble x`, podemos (debemos?) reemplazarlo por `x + x`. Así es como se ejecuta un programa funcional: reduciendo expresiones hasta alcanzar una que no se pueda reducir y que represente un valor.

Según el esquema recién planteado, la siguiente sería una posible ejecución de un programa:

```
doble 2
2 + 2
4
```

Ahora definimos algunas otras funciones.

```
cuadruple x = 4 * x

twice f = g
	where g x = f (f x)
```

Las funciones, siendo expresiones que, a partir de un valor, retornan otro valor, son capaces de, a partir de una función, retornar una función. Este es el caso de `twice`.

```
(twice doble) 2
doble (doble 2)
```

Pará. Si `twice doble` retorna `doble (doble x)`, ¿en qué momento `2` pasa a ser `x`? O sea, me quedaría algo así como `doble (doble x) 2`...

La cosa es que `twice doble` **no** retorna `doble (doble x)`, retorna **`g`**. `g` es una expresión que denota una función definida por `g x = f (f x)`. En este caso, `f` es `doble` entonces `g x = doble (doble x)`.   <s>`g x` es una expresión que denota la aplicación de la función `g`, lo que provoca la aplicación de la expresión `doble (doble x)`</s> `g x` es la parte izquierda de la ecuación orientada que define a la función `g`. **DUDA: ¿está bien lo que digo? Siento que me enredé un poco.**

Volviendo al caso que analizábamos:

```
(twice doble) 2
g 2
doble (doble 2)
```

`g 2` es la aplicación de la función `g` sobre el argumento `2`. `g 2` es una expresión que hace referencia al valor 8.