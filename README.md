# Astral
Astral is a prototype implementation of a decision procedure for a fragment of [strong-separation logic](https://arxiv.org/abs/2001.06235) (SSL) based on a translation to SMT. Currently supported fragments are satisfiability and entailment for quantifier-free symbolic heaps with lists, and boolean combination of SL atoms (i.e., divisions QF_SHLS_SAT, QF_SHLS_ENTL and QF_BSL_SAT as defined in [SL-COMP](https://sl-comp.github.io/)).

## Strong-Separation Logic

Let $x, y$ be variables. Atomic formulae are defined as follow:

| Atomic formula     | Input syntax        | Semantics |
|---------------------|---------------------|-----------|
| $x = y$             | (= x y)             | $s(x) = s(y)$ and $h = \emptyset$ |
| $x \neq y$          | (distinct x y)      | $s(x) \neq s(y)$ and $h = \emptyset$ |
| $x \mapsto y$       | (pto x y)           | $h = \lbrace s(x) \mapsto s(y) \rbrace$ |
| $\mathsf{emp}$      | emp                 | $h = \emptyset$ |
| $\mathsf{ls}$(x,y)  | (ls x y)            | $\mathsf{dom}(h) = \emptyset \land s(x) = s(y) \text{ or there exist } n \geq 1, \ell_0, ..., \ell_n \text{ such that } \mathsf{distinct}(\ell_0, ..., \ell_n)$ $\text{ and } h = \lbrace \ell_0 \mapsto \ell_1, ..., \ell_{n-1} \mapsto \ell_n \rbrace \text{ and } s(x) = \ell_0, s(y) = \ell_n$|

Let $\phi, \psi$ be formulae.

| Connective        | Input syntax              | Semantics |
|-----------------------------|---------------------------|-----------|
| $\varphi \land \psi$                | (and $\varphi$ $\psi$)       | $(s,h) \models \varphi \text{ and } (s,h) \models \psi$       |
| $\varphi \lor \psi$                 | (or $\varphi$ $\psi$)        | $(s,h) \models \varphi \text{ or }  (s,h) \models \psi$       |
| $\varphi \land_\neg \psi$           | (and $\varphi \text{ (not } \psi$)) | $(s,h) \models \varphi \text{ and } (s,h) \not\models \psi$       |
| $\varphi \ast \psi$                 | (sep $\phi$ $\psi$)         | $\exists h_1, h_2. h = h_1 \uplus^s h_2 \text{ and } (s,h_1) \models \varphi \text{ and } (s,h_2) \models \psi$       |
| $\varphi -\kern-.46em{\circledast}$  $\psi$   | (septraction $\phi$ $\psi$) | $\exists h_1. (s,h_1) \models \varphi \text { and } h_1 \uplus^s h_2 \neq \bot \text{ and } (s,h_1 \uplus^s h_2) \models \psi$       |

### Example input
A formula $x \mapsto y -\kern-.46em{\circledast}$ $\mathsf{ls}(x,y)$ can be represented as:
```smt
(declare-sort Loc 0)      ;; Declaration of location sort. Currently fixed to this form by Astral.
(declare-heap (Loc Loc))  ;; Declaration of heap sort. Currently fixed to this form by Astral.

;; Declaration of location variables
(declare-const x Loc)
(declare-const y Loc)

;; Input formula
(assert
  (septraction
    (pto x y)
    (ls x y)
  )
)

(check-sat)
```


## Installation & Usage
After cloning the repository, run `opam install .` to install Astral. Then the solver can be run by the command `astral formula.smt2`.


## Contact
If you have any questions, do not hesitate to contact the tool/method authors:
* **Tomáš Dacík** <[xdacik00@stud.fit.vutbr.cz](mailto:xdacik00@stud.fit.vutbr.cz)>
* [**Tomáš Vojnar**](https://www.fit.vut.cz/person/vojnar/) <[vojnar@fit.vutbr.cz](mailto:vojnar@fit.vutbr.cz)>

## License
The tool is available under MIT license.
