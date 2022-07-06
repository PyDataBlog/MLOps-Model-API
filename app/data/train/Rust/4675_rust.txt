fn main() {  // Declaración de vectores o arrays
  
  let vector1 = vec!["primer lemento","segundo elemento","tercer elemento"] ; // Se utiliza "vec!" para declarar una apuntador de variable como vector.
    let mut vector2: Vec<&str> = Vec::new(); // Declaración de un vector vacío con el tipo de elementos que va a contener, en este caso cadenas de texto y se deja mutable para poder agregar elementos.
  
  vector2 = vec!["Primero","Segundo"]; // Se agregan elementos al vector sobreescribiendolo gracias a "mut".
  vector2.push("Tercero"); // Agrega un elemento al final del vector.
  
  println!("primer vector 1: {:?} y segundo vector: {:?}", vector1, vector2); // Forma de ver en consola un vector no olvidar el ":?" entre los corchetes para que se imprima de lo contraria genera error.
  println!("primer elemento vector 1: {} y tercer elemento vector 2: {}", vector1[0], vector2[2]); // También se puede consultar el índice de uno de los elementos del vector usando "[X]" junto al nombre del vecto, dónde X es el número de la posición del elemento.
}