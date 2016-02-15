El problema del "Selection bias"

Si seleccionamos genes en problemas de clasificación, la validación cruzada ha de incorporar la selección de genes.

El objetivo de esta parte es ilustrar lo que ocurre cuando hacemos las cosas bien y cuando las hacemos mal.

• Simularemos un conjunto de datos, sin señal, para un número de sujetos y un número de genes, en un problema con dos clases (ej., sanos y enfermos).

• Usaremos como algoritmo de clasificación randomForest. Pero seleccionaremos genes antes de usarlos en el clasificador. Los genes seleccionados serán aquellos con el menor p-valor en un test de la t (un procedimiento de “filtrado”).

• Evaluaremos el funcionamiento del clasificador con “cross-validation”. Sin embargo, en un caso la selección de genes se habrá hecho antes de la partición para la validación cruzada (o sea, mal) y en el otro la selección de genes se hará en cada partición. Compararemos resultados.
