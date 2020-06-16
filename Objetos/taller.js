// Ejercicio 1
function ejercicio1() {
  Olaf = {
    nombre: 'Olaf',
    altura: 1.62,
    directiva: 'abrazar'
  };
}

// Ejercicio 2
function ejercicio2() {
  nuevoMuneco = function(nombre, altura, directiva) {
    let muñecoResultante = Object.create(Olaf);
    muñecoResultante.nombre = nombre;
    muñecoResultante.altura = altura;
    muñecoResultante.directiva = directiva;
    return muñecoResultante;
  };

  Malvavisco = nuevoMuneco('Malvavisco', 5, 'cuidar a la reina');
}

// Ejercicio 3
function ejercicio3() {
  Olaf.presentarse = function() {
    return `Hola, soy ${this.nombre} y me encanta ${this.directiva}`;
  }

  Olaf.abrazar = function() {
    this.altura++;
    return 'abrazar';
  }
}

// Ejercicio 4
function ejercicio4() {
  Muneco = function(nombre, altura, directiva, acción) {
    this.nombre = nombre;
    this.altura = altura;
    this.directiva = directiva;
    this[directiva] = acción;
  }

  Muneco.prototype.presentarse = function() {
    return `Hola, soy ${this.nombre} y me encanta ${this.directiva}`;
  }

  /* ¿Quiénes pueden responder al mensaje "abrazar"?
   * Respuesta:
   * - Olaf (porque es a quien le dimos esa responsabilidad originalmente)
   * - Malvavisco (porque tiene a Olaf en su cadena de prototipos)
   * - Un objeto creado con nuevoMuneco (porque tiene a Olaf en su cadena de
   *   prototipos y Olaf puede responder abrazar)
   * - Un objeto creado con Muneco que tenga 'abrazar' como directiva
   */
}

// Ejercicio 5
function ejercicio5() {
  Liam = new Muneco(
    'Liam', NaN, 'mensajear',
    function mensajear(remitente, destinatario, mensaje) {
      if (mensaje in destinatario) {
        let respuesta = destinatario[mensaje]();
        if (respuesta in remitente) {
          return remitente[respuesta]();
        } else {
          return respuesta;
        }
      } else {
        return mensaje;
      }
    }
  );
}

// Ejercicio 6
function ejercicio6() {
  Muneco.prototype.cambiarDirectiva = function(nuevaDirectiva, nuevaAccion) {
    delete this[this.directiva];

    if (this.directiva === nuevaDirectiva) {
      this.directiva = '.';
      return;
    }

    this.directiva = nuevaDirectiva;
    this[this.directiva] = nuevaAccion;
  }

  Muneco.prototype.solicitarAyuda = function(ayudante) {
    if (this.ayudante) {
      this.ayudante.solicitarAyuda(ayudante);
      return;
    }

    this.ayudante = ayudante;
    this.ayudante.cambiarDirectiva(this.directiva, this[this.directiva]);
  }

  Olaf.solicitarAyuda = Muneco.prototype.solicitarAyuda
  Olaf.cambiarDirectiva = Muneco.prototype.cambiarDirectiva
}

// Editen esta función para que devuelva lo que quieran mostrar en la salida.
function calcularResultado() {
  let res = "";
  res += "<b>Ejercicio 1</b>\n" + crearTest(1, testEjercicio1)();
  res += "\n";
  res += "<b>Ejercicio 2</b>\n" + crearTest(2, testEjercicio2)();
  res += "\n";
  res += "<b>Ejercicio 3</b>\n" + crearTest(3, testEjercicio3)();
  res += "\n";
  res += "<b>Ejercicio 4</b>\n" + crearTest(4, testEjercicio4)();
  res += "\n";
  res += "<b>Ejercicio 5</b>\n" + crearTest(5, testEjercicio5)();
  res += "\n";
  res += "<b>Ejercicio 6</b>\n" + crearTest(6, testEjercicio6)();
  return res;
}

// Agreguen aquí los tests representados como funciones que toman un objeto res como argumento.
  // Pueden llamar a res.write para escribir en la salida
  // Pueden llamar a res.test para
    // probar que una condición se cumple (pasándole la condición como único argumento)
    // o para probar que dos valores son iguales (pasándole dos argumentos)

// Test Ejercicio 1
function testEjercicio1(res) {
  res.write("Nombre de Olaf: " + Olaf.nombre);
  res.test(Olaf.nombre, "Olaf");
  res.write("Altura de Olaf: " + Olaf.altura);
  res.test(Olaf.altura, 1.62);
}

// Test Ejercicio 2
function testEjercicio2(res) {
  res.write("Nombre de Malvavisco: " + Malvavisco.nombre);
  res.test(Malvavisco.nombre, "Malvavisco");
  res.write("Altura de Malvavisco: " + Malvavisco.altura);
  res.test(Malvavisco.altura, 5);
  res.test(!Malvavisco.isPrototypeOf(Olaf));
  res.test(Olaf.isPrototypeOf(Malvavisco));
}

// Test Ejercicio 3
function testEjercicio3(res) {
  res.write("Presentación de Olaf: " + Olaf.presentarse());
  res.test(Olaf.presentarse(), "Hola, soy Olaf y me encanta abrazar");
  res.write("Presentación de Malvavisco: " + Malvavisco.presentarse());
  res.test(Malvavisco.presentarse(), "Hola, soy Malvavisco y me encanta cuidar a la reina");

  res.write("--");
  let altura_olaf = Olaf.altura;
  res.write("Altura de Olaf antes de abrazar: " + altura_olaf);
  res.test(Olaf.abrazar(), "abrazar");
  altura_olaf ++;
  res.write("Altura de Olaf después de abrazar: " + altura_olaf);
  res.test(Olaf.altura, altura_olaf);
}

// Test Ejercicio 4
function testEjercicio4(res) {
  let C = new Muneco("C", 0.1, ".", function(){return null});
  res.test(C.presentarse(), "Hola, soy C y me encanta .");
  res.test(!Olaf.isPrototypeOf(C));
  res.test(Muneco.prototype.isPrototypeOf(C));

  let olaf_sabe_abrazar = "abrazar" in Olaf
  let malvavisco_sabe_abrazar = "abrazar" in Malvavisco
  let A = nuevoMuneco("A", 0.5, ".");
  let A_sabe_abrazar = "abrazar" in A;
  let B = new Muneco("B", 0.5, ".", function(){return null});
  let B_sabe_abrazar = "abrazar" in B;
  res.write("Olaf" + si_o_no(olaf_sabe_abrazar) + "sabe abrazar");
  res.write("Malvavisco" + si_o_no(malvavisco_sabe_abrazar) + "sabe abrazar");
  res.write("Si creo un nuevo muñeco con la función original, este" + si_o_no(A_sabe_abrazar) + "sabe abrazar");
  res.write("Si creo un nuevo muñeco con la función constructora, este" + si_o_no(B_sabe_abrazar) + "sabe abrazar");
  res.test(olaf_sabe_abrazar);
  res.test(malvavisco_sabe_abrazar);
  res.test(A_sabe_abrazar);
  res.test(!B_sabe_abrazar);
}

// Test Ejercicio 5
function testEjercicio5(res) {
  res.write("Altura de Olaf: " + Olaf.altura);
  res.write("Altura de Malvavisco: " + Malvavisco.altura);
  let altura_olaf = Olaf.altura;
  let altura_malvavisco = Malvavisco.altura;
  let resultado_1 = Liam.mensajear(Olaf,Malvavisco,"abrazar")
  res.write("Resultado Olaf -> abrazar -> Malvavisco: " + resultado_1);
  res.test(resultado_1, "abrazar");
  altura_olaf++;
  altura_malvavisco++;
  res.write("Altura de Olaf: " + Olaf.altura);
  res.write("Altura de Malvavisco: " + Malvavisco.altura);
  res.test(Olaf.altura, altura_olaf);
  res.test(Malvavisco.altura, altura_malvavisco);

  res.write("--");
  let A = new Muneco("A", 1, "fA", function(){return "fB";})
  let B = new Muneco("B", 1, "fB", function(){return "fC";})
  let resultado_2 = Liam.mensajear(A, B, "fA")
  res.write("Resultado A -> fA -> B: " + resultado_2);
  let resultado_3 = Liam.mensajear(A, B, "fB")
  res.write("Resultado A -> fB -> B: " + resultado_3);
  let resultado_4 = Liam.mensajear(B, A, "fA")
  res.write("Resultado B -> fA -> A: " + resultado_4);
  res.test(resultado_2, "fA");
  res.test(resultado_3, "fC");
  res.test(resultado_4, "fC");
}

// Test Ejercicio 6
function testEjercicio6(res) {
  let A = new Muneco("A", 0.2, "a", function(){return "a"});
  let B = new Muneco("B", 0.3, "b", function(){return "b"});
  let C = new Muneco("C", 0.3, "c", function(){return "c"});

  res.test("a" in A);
  res.write("Directiva de A antes de cambiar su directiva a \"a\": " + A.directiva);
  A.cambiarDirectiva("a");
  res.test(A.directiva, ".");
  res.test(!("a" in A));
  res.write("Directiva de A después de cambiar su directiva a \"a\": " + A.directiva);

  res.write("Directiva de A antes de ayudar a B: " + A.directiva);
  res.write("Ayudante de B antes de pedir ayuda a A: " + nombre(B.ayudante));
  res.test(!("b" in A));
  B.solicitarAyuda(A);
  res.test("b" in A);
  res.test(A.b(), "b");
  res.write("Directiva de A después de ayudar a B: " + A.directiva);
  res.write("Ayudante de B después de pedir ayuda a A: " + nombre(B.ayudante));

  res.write("--");

  res.write("Directiva de C antes de ayudar a B: " + C.directiva);
  res.write("Ayudante de B antes de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A antes de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(!("ayudante" in A));
  res.test(A.directiva, "b");
  B.solicitarAyuda(C);
  res.write("Directiva de C después de ayudar a B: " + C.directiva);
  res.write("Ayudante de B después de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A después de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(A.ayudante, C);
  res.test(!("ayudante" in C));
  res.test(A.directiva, "b");
  res.test(C.directiva, "b");

  res.write("--");

  let olaf_sabe_pedir_ayuda = "solicitarAyuda" in Olaf
  let malvavisco_sabe_abrazar = "abrazar" in Malvavisco
  let malvavisco_sabe_pedir_ayuda = "solicitarAyuda" in Malvavisco
  let malvavisco_sabe_presentarse = "presentarse" in Malvavisco
  res.write("Olaf" + si_o_no(olaf_sabe_pedir_ayuda) + "sabe pedir ayuda");
  res.write("Malvavisco" + si_o_no(malvavisco_sabe_abrazar) + "sabe abrazar");
  res.write("Malvavisco" + si_o_no(malvavisco_sabe_pedir_ayuda) + "sabe pedir ayuda");
  res.write("Malvavisco" + si_o_no(malvavisco_sabe_presentarse) + "sabe presentarse");
  res.test(olaf_sabe_pedir_ayuda);
  res.test(malvavisco_sabe_pedir_ayuda);
}

function nombre(objeto) {
  if (objeto) return objeto.nombre;
  return "Ninguno";
}

function si_o_no(bool) {
  return (bool ? " " : " <b>NO</b> ")
}

// Función auxiliar que crea un test genérico a partir de un número i y una función f
function crearTest(i, f) {
  return function() {
    eval("ejercicio"+i)();
    let res = {
      text:"",
      write: function(s) {
        this.text += s + "\n";
      },
      test: function(actual, expected) {
        if (expected) {
          if (actual !== expected) {
            fail(i, "Se esperaba " + expected + " pero se obtuvo " + actual)}
        } else {
          if (!actual) {
            fail(i, "Falló la condición del test")
          }
        }
      }
    };
    try {
      f(res);
    } catch (e) {
      fail(i, e);
    }
    return res.text;
  }
}

let Olaf = undefined
let nuevoMuneco = undefined
let Malvavisco = undefined
let Muneco = undefined
let Liam = undefined
