#include <Python.h>
#include <caml/callback.h>

static struct PyModuleDef ocaml_module =  {
    PyModuleDef_HEAD_INIT,
    "Astral_py",
};

PyObject *PyInit_astral_py()
{
    static char *argv[1] = {"python"};
    caml_startup(argv);
    PyObject *m = PyModule_Create(&ocaml_module);

    PyObject *ssl = PyImport_ImportModule("SSL");
    PyModule_AddObject(m, "SSL", ssl);

    PyObject *solver = PyImport_ImportModule("solver");
    PyModule_AddObject(m, "solver", solver);

    return m;
}
